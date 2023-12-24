#' @import R.utils
#' @import stringr
NULL

#' Appends default file path to entry if its not already an absolute path
#'
#' @param current_file_entry path to expected file location
#' @param default_path the default path context for the current entry
#'
#' @return an absolute path to the file as a string
#' @export
#'
#' @examples
#' expand_dataset_path_to_file("C://scripts.sql", "")
#' expand_dataset_path_to_file("sql/script.sql", "S:/")
expand_dataset_path_to_file <- function(current_file_entry, default_path) {
  # Append default path if current file-path is a relative path
  if (R.utils::isAbsolutePath(current_file_entry)) {
    return(current_file_entry)
  } else {
    (
      return(file.path(default_path, current_file_entry))
    )
  }
}


#' Gets file extension from a file path
#'
#' @param path_to_file string of a path for a file
#'
#' @return the extension ending of the file
#' @export
#'
#' @examples
#' get_extension("test.xlsx")
get_extension <- function(path_to_file) {
  return(strsplit(basename(path_to_file), ".", fixed = TRUE)[[1]][-1])
}

#' Check whether there is an extension in the file path
#'
#' @param file_path  string of a path for a file
#'
#' @return boolean on whether an extension was found
#' @export
#'
#' @examples
#' extension_exists("test.xlsx")
extension_exists <- function(file_path) {
  whether_extension_exists <- !identical(get_extension(file_path), character(0))
  return(whether_extension_exists)
}

#' Replaces extisting file extension in a path with the provided one
#'
#' @param file_path string of a path for a file
#' @param new_extension extension to replace in existing file path
#'
#' @return a matching filepath but with the file extension swapped
#' @export
#'
#' @examples
#' replace_file_extension("test.xlsx", "parquet")
replace_file_extension <- function(file_path, new_extension) {
  file_with_updated_extension <-
    stringr::str_replace(
      file_path,
      "\\.[a-zA-Z0-9]+$",
      paste0(".", new_extension)
    )

  return(file_with_updated_extension)
}


#' Loops over a list to get a vector of name entries
#'
#' @param list_to_extract_names list of entries containing a name field
#'
#' @return vector of names for each entry in the list
#' @export
#'
#' @examples
#' get_entry_names_from_field(list(
#'   list("name" = "a"),
#'   list("name" = "b")
#' ))
get_entry_names_from_field <- function(list_to_extract_names) {
  return(sapply(list_to_extract_names, function(x) x$name))
}
