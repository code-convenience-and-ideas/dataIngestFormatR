testthat::test_that("expand_dataset_path_to_file works", {
  # Test values to check
  test_1 <- expand_dataset_path_to_file("C://scripts.sql", "")
  test_2 <- expand_dataset_path_to_file("sql/script.sql", "S:/")

  testthat::expect_equal(test_1, "C://scripts.sql")
  testthat::expect_equal(test_2, "S://sql/script.sql")
})

testthat::test_that("get_extension works", {
  testthat::expect_equal(get_extension("test.xlsx"), "xlsx")
  testthat::expect_equal(get_extension("test"), character())
  testthat::expect_equal(get_extension("data.test.folder/test.xlsx"), "xlsx")
})

testthat::test_that("extension_exists works", {
  testthat::expect_true(extension_exists("test.xlsx"))
  testthat::expect_false(extension_exists("test"))
  testthat::expect_true(extension_exists("data.test.folder/test.xlsx"))
})

testthat::test_that("replace_file_extension works", {
  # Can only replace extension if it already exists, does not add in null case
  testthat::expect_equal(replace_file_extension("test", "parquet"), "test")
  # Standard checks, isolated file name and as part of path
  testthat::expect_equal(
    replace_file_extension("test.xlsx", "parquet"),
    "test.parquet"
  )

  testthat::expect_equal(
    replace_file_extension("data.test.folder/test.xlsx", "parquet"),
    "data.test.folder/test.parquet"
  )
})

testthat::test_that("get_entry_names_from_field works", {
  # extract values in simple case
  example_list <- list(
    list("name" = "a"),
    list("name" = "b")
  )

  testthat::expect_equal(get_entry_names_from_field(example_list), c("a", "b"))

  # Fails when entries don't exist
})
