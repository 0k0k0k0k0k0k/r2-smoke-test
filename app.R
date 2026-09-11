library(shiny)
library(DBI)
library(duckdb)

required_r2_variables <- c(
  "R2_ACCOUNT_ID",
  "R2_ACCESS_KEY_ID",
  "R2_SECRET_ACCESS_KEY",
  "R2_BUCKET",
  "R2_OBJECT"
)

redact_diagnostic_message <- function(value, sensitive_values) {
  tryCatch({
    text <- as.character(value)[[1]]
    if (is.na(text)) text <- "unavailable"

    for (sensitive_value in unique(sensitive_values[nzchar(sensitive_values)])) {
      text <- gsub(sensitive_value, "[REDACTED]", text, fixed = TRUE)
    }

    text <- gsub(
      "(?i)(https?|s3|r2)://[^[:space:]'\"`]+",
      "[REDACTED_URL]",
      text,
      perl = TRUE
    )
    text <- gsub("'([^']|'')*'", "'[REDACTED]'", text, perl = TRUE)
    text <- gsub(
      "\\b[A-Za-z0-9_+/=-]{24,}\\b",
      "[REDACTED_TOKEN]",
      text,
      perl = TRUE
    )
    text <- gsub("[[:space:]]+", " ", text)
    substr(text, 1L, 400L)
  }, error = function(error) {
    "unavailable"
  })
}

run_r2_smoke_test <- function() {
  stage <- new.env(parent = emptyenv())
  stage$label <- "reading required settings"
  redaction_values <- character()

  tryCatch({
  r2_values <- Sys.getenv(required_r2_variables, unset = "")
  names(r2_values) <- required_r2_variables
  redaction_values <- r2_values

  if (any(!nzchar(r2_values))) {
    stop("Required R2 settings are missing.")
  }

  stage$label <- "creating a temporary DuckDB connection"
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  secret_created <- FALSE
  attached <- FALSE

  on.exit({
    if (attached) try(DBI::dbExecute(con, "DETACH r2db"), silent = TRUE)
    if (secret_created) {
      try(DBI::dbExecute(con, "DROP SECRET r2_smoke_test"), silent = TRUE)
    }
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
  }, add = TRUE)

  stage$label <- "installing the remote-access extension"
  DBI::dbExecute(con, "INSTALL httpfs")
  stage$label <- "loading the remote-access extension"
  DBI::dbExecute(con, "LOAD httpfs")

  quote_sql <- function(value) as.character(DBI::dbQuoteString(con, value))
  secret_sql <- paste0(
    "CREATE SECRET r2_smoke_test (",
    "TYPE r2, ",
    "KEY_ID ", quote_sql(r2_values[["R2_ACCESS_KEY_ID"]]), ", ",
    "SECRET ", quote_sql(r2_values[["R2_SECRET_ACCESS_KEY"]]), ", ",
    "ACCOUNT_ID ", quote_sql(r2_values[["R2_ACCOUNT_ID"]]), ", ",
    "SCOPE ", quote_sql(paste0("r2://", r2_values[["R2_BUCKET"]], "/")),
    ")"
  )
  stage$label <- "creating the temporary read-only R2 connection"
  DBI::dbExecute(con, secret_sql)
  secret_created <- TRUE

  database_url <- paste0(
    "r2://", r2_values[["R2_BUCKET"]], "/", r2_values[["R2_OBJECT"]]
  )
  rm(r2_values, secret_sql)

  stage$label <- "attaching the remote database"
  attach_seconds <- unname(system.time(
    DBI::dbExecute(
      con,
      paste0("ATTACH ", quote_sql(database_url), " AS r2db (READ_ONLY)")
    )
  )[["elapsed"]])
  attached <- TRUE
  rm(database_url)

  stage$label <- "reading the database schema"
  schema_seconds <- unname(system.time(
    sighting_schema <- DBI::dbGetQuery(con, "DESCRIBE r2db.app_sightings")
  )[["elapsed"]])

  lookup_sql <- paste(
    "SELECT taxon_concept_id, max(common_name) AS common_name,",
    "max(scientific_name) AS scientific_name",
    "FROM r2db.app_sightings",
    "WHERE taxon_concept_id IS NOT NULL AND trim(taxon_concept_id) <> ''",
    "AND common_name IS NOT NULL AND trim(common_name) <> ''",
    "AND lower(category) IN ('species', 'issf', 'hybrid')",
    "GROUP BY taxon_concept_id",
    "ORDER BY common_name"
  )

  stage$label <- "startup species lookup — first run"
  first_lookup_seconds <- unname(system.time(
    first_lookup <- DBI::dbGetQuery(con, lookup_sql)
  )[["elapsed"]])

  stage$label <- "startup species lookup — repeated run"
  repeat_lookup_seconds <- unname(system.time(
    repeat_lookup <- DBI::dbGetQuery(con, lookup_sql)
  )[["elapsed"]])

  benchmark_taxon_id <- first_lookup$taxon_concept_id[[1]]
  if (is.null(benchmark_taxon_id) || !nzchar(benchmark_taxon_id)) {
    stop("No benchmark taxon is available.")
  }
  redaction_values <- c(redaction_values, benchmark_taxon_id)

  stage$label <- "finding the database date limits"
  date_limits <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      "min(CAST(observation_date AS DATE)) AS min_date,",
      "max(CAST(observation_date AS DATE)) AS max_date",
      "FROM r2db.app_checklists"
    )
  )
  benchmark_year_min <- as.integer(format(as.Date(date_limits$min_date[[1]]), "%Y"))
  benchmark_year_max <- as.integer(format(as.Date(date_limits$max_date[[1]]), "%Y"))

  stage$label <- "finding a DWR-property filter"
  benchmark_property <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT DISTINCT dwr_property_name",
      "FROM r2db.checklist_dwr_property_match",
      "WHERE dwr_property_name IS NOT NULL AND trim(dwr_property_name) <> ''",
      "ORDER BY dwr_property_name",
      "LIMIT 1"
    )
  )$dwr_property_name[[1]]
  if (is.null(benchmark_property) || !nzchar(benchmark_property)) {
    stop("No benchmark property is available.")
  }

  statewide_where <- paste(
    paste0("s.taxon_concept_id IN (", quote_sql(benchmark_taxon_id), ")"),
    paste0(
      "CAST(s.observation_date AS DATE) BETWEEN DATE '",
      benchmark_year_min,
      "-01-01' AND DATE '",
      benchmark_year_max,
      "-12-31'"
    ),
    collapse = " AND "
  )
  property_where <- paste(
    statewide_where,
    paste0(
      "EXISTS (SELECT 1 FROM r2db.checklist_dwr_property_match pm ",
      "WHERE pm.checklist_id = s.checklist_id ",
      "AND pm.dwr_property_name IN (",
      quote_sql(benchmark_property),
      "))"
    ),
    sep = " AND "
  )

  sighting_summary_sql <- function(where_sql) {
    paste(
      "SELECT",
      "count(*) AS records,",
      "count(DISTINCT s.common_name) AS species,",
      "count(DISTINCT s.checklist_id) AS checklists,",
      "count(DISTINCT s.locality_id) AS locations,",
      "sum(try_cast(nullif(s.observation_count, 'X') AS DOUBLE)) AS number_reported,",
      "min(CAST(s.observation_date AS DATE)) AS first_date,",
      "max(CAST(s.observation_date AS DATE)) AS last_date",
      "FROM r2db.app_sightings s",
      "WHERE", where_sql
    )
  }
  sighting_top_places_sql <- function(where_sql) {
    paste(
      "SELECT",
      "s.locality,",
      "s.county AS county_city,",
      "count(*) AS records",
      "FROM r2db.app_sightings s",
      "WHERE", where_sql,
      "GROUP BY s.locality_id, s.locality, s.county",
      "ORDER BY records DESC, s.locality",
      "LIMIT 10"
    )
  }
  sighting_map_sql <- function(where_sql, limit = 10000) {
    paste(
      "SELECT",
      "s.locality,",
      "try_cast(s.latitude AS DOUBLE) AS latitude,",
      "try_cast(s.longitude AS DOUBLE) AS longitude,",
      "count(*) AS records",
      "FROM r2db.app_sightings s",
      "WHERE", where_sql,
      "AND try_cast(s.latitude AS DOUBLE) IS NOT NULL",
      "AND try_cast(s.longitude AS DOUBLE) IS NOT NULL",
      "GROUP BY s.locality_id, s.locality, s.latitude, s.longitude",
      "ORDER BY records DESC",
      "LIMIT", as.integer(limit)
    )
  }
  run_timed_query <- function(sql, label) {
    first_result <- NULL
    repeat_result <- NULL
    stage$label <- paste(label, "— first run")
    first_seconds <- unname(system.time(
      first_result <- DBI::dbGetQuery(con, sql)
    )[["elapsed"]])
    stage$label <- paste(label, "— repeated run")
    repeat_seconds <- unname(system.time(
      repeat_result <- DBI::dbGetQuery(con, sql)
    )[["elapsed"]])
    result <- list(
      first_seconds = first_seconds,
      repeat_seconds = repeat_seconds,
      rows = nrow(first_result)
    )
    rm(first_result, repeat_result)
    result
  }

  benchmark_results <- list(
    explorer_summary = run_timed_query(sighting_summary_sql(statewide_where), "Explorer summary"),
    explorer_top_locations = run_timed_query(sighting_top_places_sql(statewide_where), "Explorer top locations"),
    explorer_map = run_timed_query(sighting_map_sql(statewide_where), "Explorer map"),
    explorer_dwr_property_filter = run_timed_query(sighting_summary_sql(property_where), "Explorer DWR-property filter")
  )
  rm(
    benchmark_taxon_id,
    benchmark_property,
    date_limits,
    benchmark_year_min,
    benchmark_year_max,
    statewide_where,
    property_where
  )

  list(
    ok = TRUE,
    attach_seconds = attach_seconds,
    schema_seconds = schema_seconds,
    schema_columns = nrow(sighting_schema),
    first_lookup_seconds = first_lookup_seconds,
    repeat_lookup_seconds = repeat_lookup_seconds,
    lookup_rows = nrow(first_lookup),
    benchmark_results = benchmark_results
  )
  }, error = function(error) {
    diagnostic_message <- redact_diagnostic_message(
      conditionMessage(error),
      redaction_values
    )
    message(
      "R2_SMOKE_DIAGNOSTIC ",
      "stage=", stage$label,
      " class=", paste(class(error), collapse = ","),
      " message=", diagnostic_message
    )
    list(ok = FALSE, failed_stage = stage$label)
  })
}

smoke_result <- run_r2_smoke_test()

ui <- fluidPage(
  titlePanel("Remote database connection and Explorer query test"),
  if (isTRUE(smoke_result$ok)) {
    benchmark_line <- function(label, result) {
      tags$li(sprintf(
        "%s — first: %.2f seconds; repeated: %.2f seconds; rows: %d",
        label,
        result$first_seconds,
        result$repeat_seconds,
        result$rows
      ))
    }
    tagList(
      h3("Connection succeeded"),
      p(sprintf("Attach: %.2f seconds", smoke_result$attach_seconds)),
      p(sprintf("Schema check: %.2f seconds", smoke_result$schema_seconds)),
      p(sprintf("First app-startup lookup: %.2f seconds", smoke_result$first_lookup_seconds)),
      p(sprintf("Repeated lookup: %.2f seconds", smoke_result$repeat_lookup_seconds)),
      p(sprintf("Lookup rows: %d; schema columns: %d", smoke_result$lookup_rows, smoke_result$schema_columns)),
      h4("Explorer query timings"),
      tags$ul(
        benchmark_line("Explorer summary", smoke_result$benchmark_results$explorer_summary),
        benchmark_line("Explorer top locations", smoke_result$benchmark_results$explorer_top_locations),
        benchmark_line("Explorer map", smoke_result$benchmark_results$explorer_map),
        benchmark_line("Explorer DWR-property filter", smoke_result$benchmark_results$explorer_dwr_property_filter)
      )
    )
  } else {
    failed_stage <- smoke_result$failed_stage
    if (!is.character(failed_stage) || length(failed_stage) != 1L || !nzchar(failed_stage)) {
      failed_stage <- "an unspecified step"
    }
    tagList(
      h3("Connection test did not complete"),
      p(sprintf("Test stopped during: %s.", failed_stage)),
      p("No database contents or connection details are shown on this page.")
    )
  }
)

server <- function(input, output, session) {}

shinyApp(ui, server)
