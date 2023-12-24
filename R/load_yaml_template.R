#' @import stringr
#' @import R.utils
#' @import rlist
#' @import here
NULL

###################################
# Shared
###################################
#' Removed default indicator from default field name
#'
#' @param default_field_name default version of a field name
#'
#' @return string of matched non-default field name
#' @export
#'
#' @examples
#' get_non_default_entry_name(c("default_test", "default_example"))
get_non_default_entry_name <- function(default_field_name) {
  return(stringr::str_replace(default_field_name, "^default_", ""))
}

#' Adds default indicator to non default field name
#'
#' @param non_default_field_name non default field name
#'
#' @return string of matched default field name
#' @export
#'
#' @examples
#' get_default_entry_name(c("test", "example"))
get_default_entry_name <- function(non_default_field_name) {
  return(paste0("default_", non_default_field_name))
}

#' Takes a non-path entry and applies the ovverides
#'
#' @param entry_name name of the entry being parsed
#' @param default_value the default value to use if the entry is missing
#' @param dataset_block the list containing the entries to be updated
#'
#' @return a new list with updated fields
#' @export
#'
#' @examples
#' process_non_path_entry("default_engine_name", "default", list())
process_non_path_entry <- function(entry_name, default_value, dataset_block) {
  # Prepare some variables
  current_block_entries <- names(dataset_block)
  non_default_field <- get_non_default_entry_name(entry_name)

  # Flags needed to tell if an override is necessary
  entry_not_in_current_data <- !(non_default_field %in% current_block_entries)
  entry_in_current_data_is_null <- is.null(dataset_block[[non_default_field]])

  # Override values
  if (entry_not_in_current_data || entry_in_current_data_is_null) {
    dataset_block[[non_default_field]] <- default_value
  }

  # return new updated of copied data block
  return(dataset_block)
}

#' Updates default paths for the yaml data block
#'
#' @param yaml_data_as_list yaml document loaded as a nested list
#' @param default_path default path local to use to append to entries
#' @param dataset_list_updater function to update a list of datasets
#'
#' @return yaml data block with default paths filled in and appropriate names
#' @export
#'
#' @examples
#' # Simple example with a small, generic, yaml dataset
#' default_path <- file.path("C:", "test")
#' simple_data_update <- function(x) {x$test <- "value"; return(x)}
#' test_yaml <- list(
#'   "default_script_path" = NULL,
#'   "datasets_to_load" = list(
#'     list("name" = "dataset_one"),
#'     list("name" = "dataset_two")
#'   )
#'  )
#' prepare_dataset_yaml(test_yaml, default_path, simple_data_update)
prepare_dataset_yaml <- function(yaml_data_as_list,
                                 default_path,
                                 dataset_list_updater) {
  # Add default paths first
  # Pull out default paths for this block and override if value is missing
  yaml_data_as_list <- fill_list_default_paths(yaml_data_as_list, default_path)

  # Fix yaml names
  dataset_names <- get_entry_names_from_field(
    yaml_data_as_list$datasets_to_load
  )
  names(yaml_data_as_list$datasets_to_load) <- dataset_names

  # Expand needed info in the yaml dataset-blocks
  yaml_data_as_list <- dataset_list_updater(yaml_data_as_list)

  return(yaml_data_as_list)
}

#' Updates entries for every dataset in the yaml list
#'
#' @param yaml_data_as_list yaml document loaded as a nested list
#' @param individual_dataset_processor a function to update a dataset block
#'
#' @return a yaml datalist with updated measures
#' @export
#'
#' @examples
#' simple_data_update <- function(dataset_block, dataset_defaults) {
#'   dataset_block$default <- "test_value"
#'   return(dataset_block)
#' }
#' test_yaml <- list(
#'   "default_script_path" = ".",
#'   "datasets_to_load" = list(
#'     list("name" = "dataset_one"),
#'     list("name" = "dataset_two")
#'   )
#'  )
#' update_list_of_datasets(test_yaml, simple_data_update)
update_list_of_datasets <- function(yaml_data_as_list,
                                    individual_dataset_processor) {
  # unpack a few components
  dataset_names <- get_entry_names_from_field(
    yaml_data_as_list$datasets_to_load
  )
  yaml_datasets <- yaml_data_as_list$datasets_to_load
  yaml_default_entries <- extract_default_entries(yaml_data_as_list)

  # Process each sub-datasets
  for (dataset_name in dataset_names) {
    yaml_datasets[[dataset_name]] <-
      individual_dataset_processor(
        yaml_datasets[[dataset_name]],
        yaml_default_entries
      )
  }

  yaml_data_as_list$datasets_to_load <- yaml_datasets

  # Return the overral thing
  return(yaml_data_as_list)
}

#' Uses standard names to update path to a data depending on default options
#'
#' @param path_to_data path to the expected data folder
#' @param default_data_path A default file path to use for dataset location
#' @param file_name the name of the file being considered
#' @param data_file_type the default file type to use if missing an extension
#'
#' @return an absolute path to for a data file
#' @export
#'
#' @examples
#' update_data_path_with_name("raw_data", "./data", "test_data.sql", "parquet")
update_data_path_with_name <- function(path_to_data,
                                       default_data_path,
                                       file_name,
                                       data_file_type) {
  data_path_defaulted <- expand_dataset_path_to_file(path_to_data,
                                                     default_data_path)

  if (!extension_exists(data_path_defaulted)) {
    default_dataset_name <- replace_file_extension(file_name, data_file_type)
    data_path_defaulted <- file.path(data_path_defaulted, default_dataset_name)
  }

  return(data_path_defaulted)
}

#' Turns a vector of path components into a complete file path
#'
#' @param vector_of_path_components vector where each entry is part of path
#'
#' @return string of completed file path
#' @export
#'
#' @examples
#' collapsing_file_path(c("a", "b", "c"))
collapsing_file_path <- function(vector_of_path_components) {
  new_combined_file_path <- Reduce(file.path,
                                   as.list(vector_of_path_components))
  return(new_combined_file_path)
}

#' fill in all of the default path entries to fix a dataset
#'
#' @param yaml_data_as_list yaml document loaded as a nested list
#' @param default_path_to_consider the default location path to consider
#'
#' @return yaml data with paths fixed to add the default where relevant
#' @export
#'
#' @examples
#' fill_list_default_paths(list(), ".")
fill_list_default_paths <- function(
    yaml_data_as_list,
    default_path_to_consider) {
  default_entry_names <- extract_default_names(yaml_data_as_list)
  default_path_entries <- default_entry_names[
    identify_path_entries(default_entry_names)
  ]

  for (default_path_entry in default_path_entries) {
    yaml_data_as_list[[default_path_entry]] <-
      process_default_path_entry(
        yaml_data_as_list[[default_path_entry]],
        default_path_to_consider
      )
  }

  return(yaml_data_as_list)
}

############################
# Project yaml handling
############################

#' Cleans yaml names by removing redundant Folder and name portions
#'
#' @param yaml_names a vector of name entries from a flattened list
#'
#' @return Names cleaned to remove uninformative portions of flattened names
#' @export
#'
#' @examples
#' clean_yaml_names(c("Folders.other.name", "Folders.logs.name"))
clean_yaml_names <- function(yaml_names){
 cleaned_yaml_names <- stringr::str_replace_all(
   yaml_names,
   "((?:Folders.)|(?:.name$))",
   ""
 )
 return(cleaned_yaml_names)
}

#' Turn's yaml name entries into matching file paths split on '.'
#'
#' @param clean_folder_names a vector of names for the folder entries in list
#' @param default_path default path to add to relative folder path entry
#'
#' @return a vector of file paths for each folder
#' @export
#'
#' @examples
#' extract_file_paths_from_names(c("other", "logs.test", "data.raw_data"), ".")
extract_file_paths_from_names <- function(clean_folder_names, default_path){
  # split out the sub-components from the names and turn into file path
  split_entries_for_path <- stringr::str_split(clean_folder_names, "\\.")

  # Define a reduction to a full path entry
  default_path_collapser <- function(vector_of_path_parts) {
    collapsing_file_path(c(default_path, vector_of_path_parts))
  }

  # Process all of the file names
  project_folder_path_entries <- lapply(split_entries_for_path,
                                        default_path_collapser)

  return(project_folder_path_entries)
}


#' Processes a folder layout yaml to get paths to each directory
#'
#' @param yaml_data_as_list list containing data from loading yaml file
#' @param default_directory the directory to append before relative paths
#'
#' @return A list with entries being absolute paths to each folder in project
#' @export
#'
#' @examples
#' prepare_folder_structure_yaml(list("Folders" =
#' list(list("data" = list("name" = "data_dir")))), ".")
prepare_folder_structure_yaml <- function(
  yaml_data_as_list,
  default_directory
) {
  # Flatten the yaml and it also concatenates the nexsted folder names
  flattened_project_yaml <- rlist::list.flatten(yaml_data_as_list)

  # restrict the entries to just the folder entries
  flattened_project_yaml <- flattened_project_yaml[
    stringr::str_detect(names(flattened_project_yaml), "Folders")
  ]

  # Clean up the yaml files names
  names(flattened_project_yaml) <- clean_yaml_names(
    names(flattened_project_yaml)
  )

  flattened_yaml_names <- names(flattened_project_yaml)

  # Extract paths from entry names
  project_folder_path_entries <- extract_file_paths_from_names(
    flattened_yaml_names, default_directory
  )
  # Note, list entrys has the name for each folder
  names(project_folder_path_entries) <- unlist(unname(flattened_project_yaml))

  return(project_folder_path_entries)
}

###################################
# Remote execution yaml handling
###################################
#' For an remote execution yaml data block, update all of the defaults
#'
#' @param dataset_block a block of data for execution datasets
#' @param current_defaults a block of default options for entries
#'
#' @return A dataset block with execution specific defaults all applied
#' @export
#'
#' @examples
#' \dontrun{
#' update_execution_dataset_entry(list(), list())
#'
update_execution_dataset_entry <- function(dataset_block, current_defaults) {
  # Put out some useful sub-components
  non_path_default_names <- names(current_defaults)[
    !identify_path_entries(names(current_defaults))
  ]

  # File scripts path
  dataset_block[["script_path"]] <- expand_dataset_path_to_file(
    dataset_block[["script_path"]],
    current_defaults$default_script_path
  )

  script_file_basename <- basename(dataset_block[["script_path"]])

  # Data path now
  dataset_block[["data_path"]] <- update_data_path_with_name(
    dataset_block[["data_path"]],
    current_defaults$default_data_path,
    script_file_basename,
    dataset_block$data_output_format
  )

  # Employ engine and override defaults if they're missing
  for (non_path_default_name in non_path_default_names) {
    dataset_block <- process_non_path_entry(
      non_path_default_name,
      current_defaults[[non_path_default_name]],
      dataset_block
    )
  }

  return(dataset_block)
}

#' Go over the execution dataset and update all dataset blocks
#'
#' @param yaml_data_as_list yaml document loaded as a nested list
#'
#' @return An execution yaml dataset as a list with all data set entries updated
#' @export
#'
#' @examples
#' \dontrun{
#' update_exec_dataset_list(list())
#' }
update_exec_dataset_list <- function(yaml_data_as_list) {
  # process dataset to update blocks
  yaml_data_as_list <- update_list_of_datasets(yaml_data_as_list,
                                               update_execution_dataset_entry)

  # Return the overral thing
  return(yaml_data_as_list)
}

###########################
# Data from disk yaml handling
###########################
#' For a disk yaml data block, update all of the defaults
#'
#' @param dataset_block a block of data for disk datasets
#' @param current_defaults a block of default options for entries
#'
#' @return A dataset block with disk-source specific defaults all applied
#' @export
#'
#' @examples
#' \dontrun{
#' update_disk_dataset_entry(list(), list())
#' }
update_disk_dataset_entry <- function(dataset_block, current_defaults) {

  # Data path now
  dataset_block[["data_path"]] <- expand_dataset_path_to_file(
    dataset_block[["data_path"]],
    current_defaults$default_data_path
  )

  return(dataset_block)
}

#' Go over the disk datasets and update all dataset blocks
#'
#' @param yaml_data_as_list yaml document loaded as a nested list
#'
#' @return An disk yaml dataset as a list with all data set entries updated
#' @export
#'
#' @examples
#' \dontrun{
#' update_disk_list_of_datasets(list())
#' }
update_disk_list_of_datasets <- function(yaml_data_as_list) {
  # process dataset to update blocks
  yaml_data_as_list <- update_list_of_datasets(yaml_data_as_list,
                                               update_disk_dataset_entry)

  # Return the overral thing
  return(yaml_data_as_list)
}

###########################
# Data from online yaml handling
###########################
#' For an online yaml data block, update all of the defaults
#'
#' @param dataset_block a block of data for online datasets
#' @param current_defaults a block of default options for entries
#'
#' @return A dataset block with remote dataset defaults all applied
#' @export
#'
#' @examples
#' \dontrun{
#' update_online_dataset_entry(list(), list())
#' }
update_online_dataset_entry <- function(dataset_block, current_defaults) {

  # Get online file name from website link
  online_file_base_name <- basename(dataset_block[["web_link"]])
  print(dataset_block[["web_link"]])
  print(online_file_base_name)
  print(current_defaults$default_data_path)

  # Data path now
  dataset_block[["data_path"]] <- update_data_path_with_name(
    dataset_block[["data_path"]],
    current_defaults$default_data_path,
    online_file_base_name,
    dataset_block$data_output_format
  )

  return(dataset_block)
}

#' Go over the online datasets and update all dataset blocks
#'
#' @param yaml_data_as_list yaml document loaded as a nested list
#'
#' @return An online yaml dataset as a list with all data set entries updated
#' @export
#'
#' @examples
#' \dontrun{
#' update_online_list_of_datasets(list())
#' }
update_online_list_of_datasets <- function(yaml_data_as_list) {
  # process dataset to update blocks
  yaml_data_as_list <- update_list_of_datasets(yaml_data_as_list,
                                               update_online_dataset_entry)

  # Return the overral thing
  return(yaml_data_as_list)
}

##############################
# Main flow function
##############################
#' Prepares template yaml block by identifying which is relevant and processing
#'
#' @param path_to_yaml_file string with path to yaml file
#' @param default_path an overall default path to use
#'
#' @return a fulled prepared yaml dataset
#' @export
#'
#' @examples
#' local_folder_yaml <- system.file("extdata", "project_folder_structure.yaml",
#'  package = "dataIngestFormatR")
#' install_default_path <- here::here()
#' load_and_prepare_yaml_template(local_folder_yaml, install_default_path)
load_and_prepare_yaml_template <- function(path_to_yaml_file, default_path) {
  # Get the yaml data from the file itself.
  # Note, the R yaml package only download first yaml block
  yaml_data_as_list <- yaml::read_yaml(path_to_yaml_file)

  # block type is my key identifier for the yaml types
  current_yaml_block <- yaml_data_as_list$block_type

  # Switch through and pick the appropriate processor for the data
  if (current_yaml_block == "project_structure") {
    yaml_results <- prepare_folder_structure_yaml(yaml_data_as_list,
                                                  default_path)
  } else if (current_yaml_block == "remote_execution_data") {
    yaml_results <- prepare_dataset_yaml(yaml_data_as_list,
                                         default_path,
                                         update_exec_dataset_list)
  } else if (current_yaml_block == "data_from_disk") {
    yaml_results <- prepare_dataset_yaml(yaml_data_as_list,
                                         default_path,
                                         update_disk_list_of_datasets)
  } else if (current_yaml_block == "data_from_online") {
    yaml_results <- prepare_dataset_yaml(yaml_data_as_list,
                                         default_path,
                                         update_online_list_of_datasets)
  } else {
    # Raises error, only defined types covered above
  }

  return(yaml_results)
}
