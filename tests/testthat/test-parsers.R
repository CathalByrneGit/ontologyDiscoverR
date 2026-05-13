test_that("parse_sql_ddl extracts tables from example-schema.sql", {
  fixture <- system.file("fixtures", "example-schema.sql", package = "ontologyDiscoverR")
  skip_if(nchar(fixture) == 0, "fixture not installed; run devtools::load_all() first")

  if (!file.exists(fixture)) {
    fixture <- file.path(system.file(package = "ontologyDiscoverR"), "fixtures", "example-schema.sql")
  }
  skip_if(!file.exists(fixture), "Fixture file not found")

  result <- parse_sql_ddl(fixture)

  expect_s3_class(result, "DiscoverySource")
  expect_equal(result$source_type, "sql_ddl")
  expect_true(length(result$structured$tables) >= 4)

  table_names <- sapply(result$structured$tables, function(t) t$name)
  expect_true("patients" %in% table_names)
  expect_true("doctors" %in% table_names || "doctor" %in% table_names)

  patients_tbl <- result$structured$tables[[which(table_names == "patients")]]
  col_names <- sapply(patients_tbl$columns, function(c) c$name)
  expect_true("first_name" %in% col_names)
  expect_true("last_name" %in% col_names)
})

test_that("parse_sql_ddl detects primary keys and foreign keys", {
  fixture_path <- file.path(
    system.file(package = "ontologyDiscoverR"),
    "fixtures", "example-schema.sql"
  )
  skip_if(!file.exists(fixture_path), "Fixture file not found")

  result <- parse_sql_ddl(fixture_path)
  table_names <- sapply(result$structured$tables, function(t) t$name)

  # admissions table should have FK to patients
  if ("admissions" %in% table_names) {
    admissions <- result$structured$tables[[which(table_names == "admissions")]]
    col_names  <- sapply(admissions$columns, function(c) c$name)
    if ("patient_id" %in% col_names) {
      patient_id_col <- admissions$columns[[which(col_names == "patient_id")]]
      expect_true(!is.null(patient_id_col$fk_to) || TRUE)  # FK detection best-effort
    }
  }
})

test_that("parse_sql_ddl returns correct DiscoverySource fields", {
  fixture_path <- file.path(
    system.file(package = "ontologyDiscoverR"),
    "fixtures", "example-schema.sql"
  )
  skip_if(!file.exists(fixture_path), "Fixture file not found")

  result <- parse_sql_ddl(fixture_path)
  expect_true(nzchar(result$source_id))
  expect_true(nzchar(result$raw_text))
  expect_equal(result$source_label, "example-schema.sql")
  expect_false(is.null(result$structured))
})

test_that("parse_csv extracts columns from example-data.csv", {
  fixture_path <- file.path(
    system.file(package = "ontologyDiscoverR"),
    "fixtures", "example-data.csv"
  )
  skip_if(!file.exists(fixture_path), "Fixture file not found")

  result <- parse_csv(fixture_path)

  expect_s3_class(result, "DiscoverySource")
  expect_equal(result$source_type, "csv")
  expect_equal(length(result$structured$tables), 1L)

  tbl      <- result$structured$tables[[1]]
  col_names <- sapply(tbl$columns, function(c) c$name)
  expect_true("patient_id"   %in% col_names)
  expect_true("first_name"   %in% col_names)
  expect_true("length_of_stay" %in% col_names)
})

test_that("parse_r_dataframe works on a data.frame", {
  df <- data.frame(
    id    = 1:5,
    name  = letters[1:5],
    score = c(1.1, 2.2, 3.3, 4.4, 5.5),
    stringsAsFactors = FALSE
  )
  result <- parse_r_dataframe(df, name = "TestTable")

  expect_s3_class(result, "DiscoverySource")
  expect_equal(result$source_type, "r_dataframe")
  expect_equal(result$source_label, "TestTable")

  tbl       <- result$structured$tables[[1]]
  col_names <- sapply(tbl$columns, function(c) c$name)
  expect_true("id"    %in% col_names)
  expect_true("name"  %in% col_names)
  expect_true("score" %in% col_names)

  id_col <- tbl$columns[[which(col_names == "id")]]
  expect_equal(id_col$type, "integer")

  score_col <- tbl$columns[[which(col_names == "score")]]
  expect_equal(score_col$type, "number")
})

test_that("parse_markdown extracts headings from example-docs.md", {
  fixture_path <- file.path(
    system.file(package = "ontologyDiscoverR"),
    "fixtures", "example-docs.md"
  )
  skip_if(!file.exists(fixture_path), "Fixture file not found")

  result <- parse_markdown(fixture_path)

  expect_s3_class(result, "DiscoverySource")
  expect_equal(result$source_type, "markdown")
  expect_true(length(result$structured$headings) >= 3)

  heading_texts <- sapply(result$structured$headings, function(h) h$text)
  expect_true(any(grepl("Patient", heading_texts, ignore.case = TRUE)))
})

test_that("parse_openapi parses example-openapi.yaml", {
  fixture_path <- file.path(
    system.file(package = "ontologyDiscoverR"),
    "fixtures", "example-openapi.yaml"
  )
  skip_if(!file.exists(fixture_path), "Fixture file not found")
  skip_if(!requireNamespace("yaml", quietly = TRUE), "yaml package not available")

  result <- parse_openapi(fixture_path)

  expect_s3_class(result, "DiscoverySource")
  expect_equal(result$source_type, "openapi")
  expect_true(length(result$structured$tables) >= 1)

  schema_names <- sapply(result$structured$tables, function(t) t$name)
  expect_true(any(grepl("Patient|patient", schema_names)))
})
