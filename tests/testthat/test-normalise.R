test_that("normalise_type maps string types correctly", {
  expect_equal(normalise_type("varchar"),          "string")
  expect_equal(normalise_type("VARCHAR(255)"),     "string")
  expect_equal(normalise_type("text"),             "string")
  expect_equal(normalise_type("TEXT"),             "string")
  expect_equal(normalise_type("char"),             "string")
  expect_equal(normalise_type("nvarchar"),         "string")
  expect_equal(normalise_type("character varying"),"string")
  expect_equal(normalise_type("uuid"),             "string")
})

test_that("normalise_type maps integer types correctly", {
  expect_equal(normalise_type("int"),     "integer")
  expect_equal(normalise_type("integer"), "integer")
  expect_equal(normalise_type("bigint"),  "integer")
  expect_equal(normalise_type("serial"),  "integer")
  expect_equal(normalise_type("INT"),     "integer")
  expect_equal(normalise_type("int4"),    "integer")
  expect_equal(normalise_type("int8"),    "integer")
})

test_that("normalise_type maps numeric types correctly", {
  expect_equal(normalise_type("float"),          "number")
  expect_equal(normalise_type("double"),         "number")
  expect_equal(normalise_type("numeric"),        "number")
  expect_equal(normalise_type("numeric(10,2)"),  "number")
  expect_equal(normalise_type("decimal"),        "number")
  expect_equal(normalise_type("real"),           "number")
})

test_that("normalise_type maps boolean types correctly", {
  expect_equal(normalise_type("bool"),    "boolean")
  expect_equal(normalise_type("boolean"), "boolean")
  expect_equal(normalise_type("BOOLEAN"), "boolean")
})

test_that("normalise_type maps date/time types correctly", {
  expect_equal(normalise_type("date"),      "date")
  expect_equal(normalise_type("DATE"),      "date")
  expect_equal(normalise_type("timestamp"), "datetime")
  expect_equal(normalise_type("datetime"),  "datetime")
  expect_equal(normalise_type("timestamptz"), "datetime")
})

test_that("normalise_type maps json types correctly", {
  expect_equal(normalise_type("json"),  "object")
  expect_equal(normalise_type("jsonb"), "object")
})

test_that("normalise_type maps array types correctly", {
  expect_equal(normalise_type("array"), "array")
  expect_equal(normalise_type("ARRAY"), "array")
})

test_that("normalise_type defaults unknown types to string with warning", {
  expect_warning(
    result <- normalise_type("myCustomType"),
    "Unknown type"
  )
  expect_equal(result, "string")
})

test_that("normalise_type handles NA and empty input", {
  expect_equal(normalise_type(NA_character_), "string")
  expect_equal(normalise_type(""), "string")
})
