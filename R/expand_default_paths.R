#' @import stringr
#' @import R.utils
NULL

#' Identifies entries in vector matching default variable format
#'
#' @param entry_names the names from entries in a yaml as a list
#'
#' @return Boolean vector indicating if entry matches default name format
#' @export
#'
#' @examples
#' identify_default_entries(c("default_z", "a", "b", "c_default"))
identify_default_entries <- function(entry_names){
    return(stringr::str_detect(entry_names, '^default_'))
}

#' Identifies entries in vector match path variable format
#'
#' @param entry_names the names from entries in a yaml as a list
#'
#' @return Boolean vector indicating if entry matches path name format
#' @export
#'
#' @examples
#' identify_default_entries(c("z_path", "a", "b", "path_c"))
identify_path_entries <- function(entry_names){
    return(stringr::str_detect(entry_names, '_path$'))
}

#' For a list, get the names matching default path
#'
#' @param yaml_as_list a yaml document that has been loaded into a list
#'
#' @return vector of names matching default format
#' @export
#'
#' @examples
#' extract_default_names(as.list(c("default_val"="a", "c"="d")))
extract_default_names <- function(yaml_as_list){
    return(names(yaml_as_list)[identify_default_entries(names(yaml_as_list))])
}

#' For a list, get entries holding default values
#'
#' @param yaml_as_list yaml_as_list a yaml document that has been loaded into a list
#'
#' @return list of entries whose name matches default format
#' @export
#'
#' @examples
#' extract_default_entries(as.list(c("default_val"="a", "c"="d")))
extract_default_entries <- function(yaml_as_list){
    return(yaml_as_list[extract_default_names(yaml_as_list)])
}

#' Fills in an empty path value with the default and expands defaults relative path
#'
#' @param path_to_check a path entry
#' @param path_default
#'
#' @return
#' @export
#'
#' @examples
#' process_default_path_entry(NULL, '.')
#' process_default_path_entry("data", 'C:/')
#' process_default_path_entry("data/test", "C:/")
process_default_path_entry <- function(path_to_check, path_default){
    path_is_null <- is.null(path_to_check)
    path_is_empty <- (path_to_check == '')
    if (path_is_null || path_is_empty){
        default_script_path <- R.utils::getAbsolutePath(path_default)
    } else {
        default_script_path <- R.utils::getAbsolutePath(path_to_check)
    }
    return(default_script_path)
}
