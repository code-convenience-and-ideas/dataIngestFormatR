#' @import assertthat
#' @import R.utils
NULL

#' Appends default file path to entry if its not already an absolute path
#'
#' @param current_file_entry a path as a string to where the file is expected to be
#' @param default_to_consider the default path context for the current entry
#'
#' @return an absolute path to the file as a string
#' @export
#'
#' @examples
#' expand_dataset_path_to_file('C://scripts.sql', '')
#' expand_dataset_path_to_file('sql/script.sql', 'S:/')
expand_dataset_path_to_file <- function(current_file_entry, default_path){
    # Process script path and use it to name datafile and get absolute path to script
    if (R.utils::isAbsolutePath(current_file_entry)){
        return(current_file_entry)
    } else (
        return(file.path(default_path, current_file_entry))
    )
}


#' Gets file extension from a file path
#'
#' @param path_to_file
#'
#' @return
#' @export
#'
#' @examples
get_extension <- function(path_to_file){
    return(strsplit(basename(path_to_file), ".", fixed=T)[[1]][-1])
}


#' Title
#'
#' @param file_path
#'
#' @return
#' @export
#'
#' @examples
extension_exists <- function(file_path){
    whether_extension_exists <- !identical(get_extension(file_path), character(0))
    return(whether_extension_exists)
}

#' Title
#'
#' @param current_file_path
#' @param new_extension
#'
#' @return
#' @export
#'
#' @examples
replace_file_extension <- function(current_file_path, new_extension){
    file_with_updated_extension <- stringr::str_replace(current_file_path, "\\.[a-zA-Z0-9]+$", paste0(".", new_extension))
    return(file_with_updated_extension)
}


#' Title
#'
#' @param yaml_data_as_list
#' @param default_path_to_consider
#'
#' @return
#' @export
#'
#' @examples
fill_list_default_paths <- function(yaml_data_as_list, default_path_to_consider){
    default_entry_names <- extract_default_names(yaml_data_as_list)
    default_path_entries <- default_entry_names[identify_path_entries(default_entry_names)]

    for (default_path_entry in default_path_entries){
        yaml_data_as_list[[default_path_entry]] <- process_default_path_entry(yaml_data_as_list[[default_path_entry]], default_path_to_consider)
    }

    return(yaml_data_as_list)
}

#' Title
#'
#' @param list_to_extract_names
#'
#' @return
#' @export
#'
#' @examples
get_entry_names_from_name_field <- function(list_to_extract_names){
    return(sapply(list_to_extract_names, function(x) x$name))
}
