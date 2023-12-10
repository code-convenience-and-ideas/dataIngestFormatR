testthat::test_that("identify_default_entries only matches defaults", {
  entry_sample <- c("default_z", "a", "b", "c_default", "a_default_b", NA)
  entry_result <- c(TRUE, FALSE, FALSE, FALSE, FALSE, NA)

  testthat::expect_equal(identify_default_entries(entry_sample), entry_result)
})

testthat::test_that("identify_path_entries only matches path entries", {
  entry_sample <- c("z_path", "a", "b", "path_c", "a_path_b", NA)
  entry_result <- c(TRUE, FALSE, FALSE, FALSE, FALSE, NA)

  testthat::expect_equal(identify_path_entries(entry_sample), entry_result)
})

testthat::test_that("extract_default_names only extracts defaults", {
  example_vector <- c(
    "default_val" = "a", "c" = "d",
    "default_test" = "e", NULL = "v"
  )
  entry_sample <- as.list(example_vector)
  entry_result <- c("default_val", "default_test")

  testthat::expect_equal(extract_default_names(entry_sample), entry_result)
})

testthat::test_that("extract_default_entries gets correct default list", {
  example_vector <- c(
    "default_val" = "a", "c" = "d",
    "default_test" = "e", NULL = "v"
  )
  entry_sample <- as.list(example_vector)
  entry_result <- list("default_val" = "a", "default_test" = "e")

  testthat::expect_equal(extract_default_entries(entry_sample), entry_result)
})

testthat::test_that("process_default_path_entry fills default path correctly", {
  test_path_1 <- ""
  test_path_2 <- NULL
  test_path_3 <- "data/test"
  default_path_1 <- "C:/"

  cur_dir <- getwd()

  # Go through examples and expected results
  testthat::expect_equal(
    process_default_path_entry(test_path_1, default_path_1),
    default_path_1
  )

  testthat::expect_equal(
    process_default_path_entry(test_path_2, default_path_1),
    default_path_1
  )

  testthat::expect_equal(
    process_default_path_entry(test_path_3, default_path_1),
    file.path(cur_dir, test_path_3)
  )
})
