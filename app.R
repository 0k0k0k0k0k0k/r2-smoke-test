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

run_r2_smoke_test <- function() {
  r2_values <- Sys.getenv(required_r2_variables, unset = "")
  names(r2_values) <- required_r2_variables

  if (any(!nzchar(r2_values))) {
    stop("Required R2 settings are missing.")
  }

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

  DBI::dbExecute(con, "INSTALL httpfs")
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
  DBI::dbExecute(con, secret_sql)
  secret_created <- TRUE

  database_url <- paste0(
    "r2://", r2_values[["R2_BUCKET"]], "/", r2_values[["R2_OBJECT"]]
  )
  rm(r2_values, secret_sql)

  attach_seconds <- unname(system.time(
    DBI::dbExecute(
      con,
      paste0("ATTACH ", quote_sql(database_url), " AS r2db (READ_ONLY)")
    )
  )[["elapsed"]])
  attached <- TRUE
  rm(database_url)

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

  first_lookup_seconds <- unname(system.time(
    first_lookup <- DBI::dbGetQuery(con, lookup_sql)
  )[["elapsed"]])

  repeat_lookup_seconds <- unname(system.time(
    repeat_lookup <- DBI::dbGetQuery(con, lookup_sql)
  )[["elapsed"]])

  list(
    ok = TRUE,
    attach_seconds = attach_seconds,
    schema_seconds = schema_seconds,
    schema_columns = nrow(sighting_schema),
    first_lookup_seconds = first_lookup_seconds,
    repeat_lookup_seconds = repeat_lookup_seconds,
    lookup_rows = nrow(first_lookup)
  )
}

smoke_result <- tryCatch(
  run_r2_smoke_test(),
  error = function(error) list(ok = FALSE)
)

ui <- fluidPage(
  titlePanel("Remote database connection test"),
  if (isTRUE(smoke_result$ok)) {
    tagList(
      h3("Connection succeeded"),
      p(sprintf("Attach: %.2f seconds", smoke_result$attach_seconds)),
      p(sprintf("Schema check: %.2f seconds", smoke_result$schema_seconds)),
      p(sprintf("First app-startup lookup: %.2f seconds", smoke_result$first_lookup_seconds)),
      p(sprintf("Repeated lookup: %.2f seconds", smoke_result$repeat_lookup_seconds)),
      p(sprintf("Lookup rows: %d; schema columns: %d", smoke_result$lookup_rows, smoke_result$schema_columns))
    )
  } else {
    tagList(
      h3("Connection test did not complete"),
      p("No database contents or connection details are shown on this page.")
    )
  }
)

server <- function(input, output, session) {}

shinyApp(ui, server)
