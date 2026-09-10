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
  stage <- new.env(parent = emptyenv())
  stage$label <- "reading required settings"

  tryCatch({
    r2_values <- Sys.getenv(required_r2_variables, unset = "")
    names(r2_values) <- required_r2_variables

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

    stage$label <- "startup species lookup â first run"
    first_lookup_seconds <- unname(system.time(
      first_lookup <- DBI::dbGetQuery(con, lookup_sql)
    )[["elapsed"]])

    stage$label <- "startup species lookup â repeated run"
    repeat_lookup_seconds <- unname(system.time(
      repeat_lookup <- DBI::dbGetQuery(con, lookup_sql)
    )[["elapsed"]])

    benchmark_taxon_id <- first_lookup$taxon_concept_id[[1]]
    if (is.null(benchmark_taxon_id) || !nzhcar(benchmark_taxon_id)) {
      stop("No benchmark taxon is available.")
    }

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
        "-01-01' AND DATE "',
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
        "AND try_cast(s.longitude AS DOUBLE)) IS NOT NULL",
        "GROUP BY s.locality_id, s.locality, s.latitude, s.longitude",
        "ORDER BY records DESC",
        "LIMIT", as.integer(limit)
      )
    }
    run_timed_query <- function(sql, label) {
      first_result <- NULL
      repeat_result <- NULL
      stage$label <- paste(label, "â first run")
      first_seconds <- unname(system.time(
        first_result <- DBI::dbGetQuery(con, sql)
      )[["elapsed"]])
      stage$label <- paste(label, "â repeated run")
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
      repeat_lookup_seconds = repeat_lookup_secondsÛÚÝ\ÜÝÜÈHÝÊ\ÝÛÛÚÝ\
K[ÚX\×Ü\Ý[ÈH[ÚX\×Ü\Ý[Â
BK\ÜH[Ý[Û\ÜHÂ\Ý
ÚÈHSÑKZ[YÜÝYÙHHÝYÙIX[
BJBBÛ[ÚÙWÜ\Ý[H[ÜÜÛ[ÚÙWÝ\Ý

BZHHZYYÙJ]T[[
[[ÝH]X\ÙHÛÛXÝ[Û[^Ü\]Y\H\ÝKY
\ÕQJÛ[ÚÙWÜ\Ý[	ÚÊJHÂ[ÚX\×Û[HH[Ý[ÛX[\Ý[
HÂYÜÉJÜ[\È8 %\Ý	KÙXÛÛÎÈ\X]Y	KÙXÛÛÎÈÝÜÎ	YX[\Ý[	\ÝÜÙXÛÛË\Ý[	\X]ÜÙXÛÛË\Ý[	ÝÜÂ
JBBYÓ\Ý
ÊÛÛXÝ[ÛÝXØÙYYYK
Ü[]XÚ	KÙXÛÛÈÛ[ÚÙWÜ\Ý[	]XÚÜÙXÛÛÊJK
Ü[ØÚ[XHÚXÚÎ	KÙXÛÛÈÛ[ÚÙWÜ\Ý[	ØÚ[XWÜÙXÛÛÊJK
Ü[\Ý\\Ý\\ÛÚÝ\	KÙXÛÛÈÛ[ÚÙWÜ\Ý[	\ÝÛÛÚÝ\ÜÙXÛÛÊJK
Ü[\X]YÛÚÝ\	KÙXÛÛÈÛ[ÚÙWÜ\Ý[	\X]ÛÛÚÝ\ÜÙXÛÛÊJK
Ü[ÛÚÝ\ÝÜÎ	YÈØÚ[XHÛÛ[[Î	YÛ[ÚÙWÜ\Ý[	ÛÚÝ\ÜÝÜËÛ[ÚÙWÜ\Ý[	ØÚ[XWÜØÚ[XWØÛÛ[[ÊJK
^Ü\]Y\H[Z[ÜÈKYÜÉ[
[ÚX\×Û[J^Ü\Ý[[X\HÛ[ÚÙWÜ\Ý[	[ÚX\×Ü\Ý[É^Ü\ÜÝ[[X\JK[ÚX\×Û[J^Ü\ÜØØ][ÛÈÛ[ÚÙWÜ\Ý[	[ÚX\×Ü\Ý[É^Ü\ÝÜÛØØ][ÛÊK[ÚX\×Û[J^Ü\X\Û[ÚÙWÜ\Ý[	[ÚX\×Ü\Ý[É^Ü\ÛX\
K[ÚX\×Û[J^Ü\Ô\Ü\H[\Û[ÚÙWÜ\Ý[	[ÚX\×Ü\Ý[É^Ü\ÙÜÜÜ\WÙ[\B
B
BH[ÙHÂZ[YÜÝYÙHHÛ[ÚÙWÜ\Ý[	Z[YÜÝYÙBY
Z\ËÚ\XÝ\Z[YÜÝYÙJH[Ý
Z[YÜÝYÙJHOHS[Ú\Z[YÜÝYÙJJHÂZ[YÜÝYÙHH[[ÜXÚYYYÝ\BYÓ\Ý
ÊÛÛXÝ[Û\ÝYÝÛÛ\]HK
Ü[\ÝÝÜY\[Î	\ËZ[YÜÝYÙJJK
È]X\ÙHÛÛ[ÈÜÛÛXÝ[Û]Z[È\HÚÝÛÛ\ÈYÙKB
BBBÙ\\H[Ý[Û[]Ý]]Ù\ÜÚ[ÛHßBÚ[P\
ZKÙ\\B
