#' @import stringr
#' @import R.utils
#' @import rlist
NULL

example_folder_yaml_path <- r"(C:\Users\Alex\Google Drive\projects\r_convenience_packages\dataIngestFormatR\data\project_folder_structure.yaml)"
example_execution_yaml_path <- r"(C:\Users\Alex\Google Drive\projects\r_convenience_packages\dataIngestFormatR\data\data_from_computation.yaml)"
example_disk_yaml_path <- r"(C:\Users\Alex\Google Drive\projects\r_convenience_packages\dataIngestFormatR\data\data_from_disk.yaml)"
example_online_yaml_path <- r"(C:\Users\Alex\Google Drive\projects\r_convenience_packages\dataIngestFormatR\data\data_from_online.yaml)"


###################################
# Shared
###################################
#' Title
#'
#' @param default_field_name
#'
#' @return
#' @export
#'
#' @examples
get_non_default_entry_name <- function(default_field_name){
    return(stringr::str_replace(default_field_name, "^default_", ""))
}

#' Title
#'
#' @param non_default_field_name
#'
#' @return
#' @export
#'
#' @examples
get_default_entry_name <- function(non_default_field_name){
    return(paste0("default_", non_default_field_name))
}

#' Title
#'
#' @param non_path_default_names
#' @param dataset_block
#'
#' @return
#' @export
#'
#' @examples
update_non_path_entries <- function(non_path_default_names, dataset_block){
    # NB, inefficient but data is small so shouldn't matter. No reasons to think config will get massive
    # Data gets fully copied for each non-default entry as R copies function inputs
    for (non_path_default_name in non_path_default_names){
        dataset_block <- process_non_path_entry(non_path_default_name, dataset_block[[non_path_default_name]], process_non_path_entry)
    }

    return(dataset_block)
}

#' Title
#'
#' @param entry_name
#' @param default_value
#' @param dataset_block
#'
#' @return
#' @export
#'
#' @examples
process_non_path_entry <- function(entry_name, default_value, dataset_block){
    # Prepare some variables
    current_block_entries <- names(dataset_block)
    non_default_field <- get_non_default_entry_name(entry_name)

    # Flags needed to tell if an override is necessary
    entry_not_in_current_data <- !(non_default_field %in% current_block_entries)
    entry_in_current_data_is_null <- is.null(dataset_block[[non_default_field]])

    # Override values
    if(entry_not_in_current_data || entry_in_current_data_is_null){
        dataset_block[[non_default_field]] <- default_value
    }

    # return new updated of copied data block
    return(dataset_block)
}

#' Title
#'
#' @param yaml_data_as_list
#'
#' @return
#' @export
#'
#' @examples
prepare_dataset_yaml <- function(yaml_data_as_list, default_path, dataset_list_updater){
    # Add default paths first
    # Pull out default paths for this block and override if value is missing
    yaml_data_as_list <- fill_list_default_paths(yaml_data_as_list, default_path)

    # Fix yaml names
    dataset_names <- get_entry_names_from_name_field(yaml_data_as_list$datasets_to_load)
    names(yaml_data_as_list$datasets_to_load) <- dataset_names

    # Expand needed info in the yaml dataset-blocks
    yaml_data_as_list <- dataset_list_updater(yaml_data_as_list)

    return(yaml_data_as_list)
}

#' Title
#'
#' @param yaml_data_as_list
#'
#' @return
#' @export
#'
#' @examples
update_list_of_datasets <- function(yaml_data_as_list, individual_dataset_processor){
    # unpack a few components
    dataset_names <- get_entry_names_from_name_field(yaml_data_as_list$datasets_to_load)
    yaml_datasets <- yaml_data_as_list$datasets_to_load
    yaml_default_entries <- extract_default_entries(yaml_data_as_list)
    non_path_default_names <- names(yaml_default_entries)[!identify_path_entries(names(yaml_default_entries))]

    # Process each sub-datasets
    for (dataset_name in dataset_names){
        yaml_datasets[[dataset_name]] <- individual_dataset_processor(yaml_datasets[[dataset_name]], yaml_default_entries)
    }

    yaml_data_as_list$datasets_to_load <- yaml_datasets

    # Return the overral thing
    return(yaml_data_as_list)
}

#' Title
#'
#' @param path_to_data
#' @param default_data_path
#' @param script_file_name
#' @param data_file_type
#'
#' @return
#' @export
#'
#' @examples
update_data_path_file_name_default <- function(path_to_data, default_data_path, file_name, data_file_type){
    data_path_expanded_with_default <- expand_dataset_path_to_file(path_to_data, default_data_path)

    if (!extension_exists(data_path_expanded_with_default)){
        default_dataset_name <- replace_file_extension(file_name, data_file_type)
        data_path_expanded_with_default <- file.path(data_path_expanded_with_default, default_dataset_name)
    }

    return(data_path_expanded_with_default)
}

#' Turns a vector of path components into a complete file path
#'
#' @param vector_of_path_components
#'
#' @return string of completed file path
#' @export
#'
#' @examples
#' collapsing_file_path(c("a", "b", "c"))
collapsing_file_path <- function(vector_of_path_components){
    new_combined_file_path <- Reduce(file.path, as.list(vector_of_path_components))
    return(new_combined_file_path)
}

############################
# Project yaml handling
############################
#' Processes a folder layout yaml to get paths to each directory
#'
#' @param yaml_data_as_list list containing data from loading yaml file
#' @param default_directory the directory to append before relative paths
#'
#' @return A list with entries being absolute paths to each folder in project
#' @export
#'
#' @examples
#' prepare_folder_structure_yaml(list("Folders"=list(list("data"=list("name"="data_dir")))), ".")
prepare_folder_structure_yaml <- function(yaml_data_as_list, default_directory){

    # Flatten the yaml and it also concatenates the nexsted folder names
    flattened_project_yaml <- rlist::list.flatten(yaml_data_as_list)

    # restrict the entries to just the folder-specific entries then and not the general case
    flattened_project_yaml <- flattened_project_yaml[stringr::str_detect(names(flattened_project_yaml), "Folders")]
    names(flattened_project_yaml) <- stringr::str_replace_all(names(flattened_project_yaml), "((?:Folders.)|(?:.name$))", "")

    # we can then also split out the sub-components from the names and use file.path to process them
    project_folder_path_entries <- lapply(stringr::str_split(names(flattened_project_yaml), '\\.'), function(x) collapsing_file_path(c(default_directory, x)))
    names(project_folder_path_entries) <- unlist(unname(flattened_project_yaml))  # Note, list entrys has the name for each folder

    return(project_folder_path_entries)
}

###################################
# Remote execution yaml handling
###################################
#' Title
#'
#' @param dataset_block
#' @param current_defaults
#'
#' @return
#' @export
#'
#' @examples
update_execution_dataset_entry <- function(dataset_block, current_defaults){
    # Put out some useful sub-components
    non_path_default_names <- names(current_defaults)[!identify_path_entries(names(current_defaults))]

    # File scripts path
    dataset_block[["script_path"]] <- expand_dataset_path_to_file(
        dataset_block[["script_path"]],
        current_defaults$default_script_path)

    script_file_basename <- basename(current_dataset[["script_path"]])

    # Data path now
    dataset_block[["data_path"]] <- update_data_path_file_name_default(
        dataset_block[["data_path"]],
        current_defaults$default_data_path,
        script_file_basename,
        dataset_block$data_output_format
    )

    # Employ engine and override defaults if they're missing
    for (non_path_default_name in non_path_default_names){
        dataset_block <- process_non_path_entry(
            non_path_default_name,
            yaml_default_entries[[non_path_default_name]],
            dataset_block)
    }

    return(dataset_block)
}

#' Title
#'
#' @param yaml_data_as_list
#'
#' @return
#' @export
#'
#' @examples
update_execution_list_of_datasets <- function(yaml_data_as_list){

    # process dataset to update blocks
    yaml_data_as_list <- update_list_of_datasets(yaml_data_as_list, update_execution_dataset_entry)

    # Return the overral thing
    return(yaml_data_as_list)
}

###########################
# Data from disk yaml handling
###########################
#' Title
#'
#' @param dataset_block
#' @param current_defaults
#'
#' @return
#' @export
#'
#' @examples
update_disk_dataset_entry <- function(dataset_block, current_defaults){
    # Put out some useful sub-components
    non_path_default_names <- names(current_defaults)[!identify_path_entries(names(current_defaults))]

    # Data path now
    dataset_block[["data_path"]] <- expand_dataset_path_to_file(dataset_block[["data_path"]], current_defaults$default_data_path)

    return(dataset_block)
}

#' Title
#'
#' @param yaml_data_as_list
#'
#' @return
#' @export
#'
#' @examples
update_disk_list_of_datasets <- function(yaml_data_as_list){
    # process dataset to update blocks
    yaml_data_as_list <- update_list_of_datasets(yaml_data_as_list, update_disk_dataset_entry)

    # Return the overral thing
    return(yaml_data_as_list)
}

###########################
# Data from online yaml handling
###########################
#' Title
#'
#' @param dataset_block
#' @param current_defaults
#'
#' @return
#' @export
#'
#' @examples
update_online_dataset_entry <- function(dataset_block, current_defaults){
    # Put out some useful sub-components
    non_path_default_names <- names(current_defaults)[!identify_path_entries(names(current_defaults))]

    # Get online file name from website link
    online_file_base_name <- basename(dataset_block[["web_link"]])
    print(dataset_block[["web_link"]])
    print(online_file_base_name)
    print(current_defaults$default_data_path)

    # Data path now
    dataset_block[["data_path"]] <- update_data_path_file_name_default(
        dataset_block[["data_path"]],
        current_defaults$default_data_path,
        online_file_base_name,
        dataset_block$data_output_format
    )

    return(dataset_block)
}

#' Title
#'
#' @param yaml_data_as_list
#'
#' @return
#' @export
#'
#' @examples
update_online_list_of_datasets <- function(yaml_data_as_list){
    # process dataset to update blocks
    yaml_data_as_list <- update_list_of_datasets(yaml_data_as_list, update_online_dataset_entry)

    # Return the overral thing
    return(yaml_data_as_list)
}

##############################
# Main flow function
##############################
#' Prepares template yaml block by identifying which is relevant and processing
#'
#' @param path_to_yaml_file string with path to yaml file
#'
#' @return
#' @export
#'
#' @examples
#' load_and_prepare_yaml_template("./test.yaml")
load_and_prepare_yaml_template <- function(path_to_yaml_file, default_path){
    # Get the yaml data from the file itself.
    # Note, the R yaml package only download first yaml block
    yaml_data_as_list <- yaml::read_yaml(path_to_yaml_file)

    # block type is my key identifier for the yaml types
    current_yaml_block <- yaml_folder_data$block_type

    # Switch through and pick the appropriate processor for the data
    if (current_yaml_block == "project_structure"){
        yaml_results <- prepare_folder_structure_yaml(yaml_data_as_list, default_path)

    } else if (current_yaml_block == "remote_execution_data"){
        yaml_results <- prepare_dataset_yaml(yaml_data_as_list, default_path, update_execution_list_of_datasets())

    } else if (current_yaml_block == "data_from_disk"){
        yaml_results <- prepare_dataset_yaml(yaml_data_as_list, default_path, update_disk_list_of_datasets)

    } else if (current_yaml_block == "data_from_online"){
        yaml_results <- prepare_dataset_yaml(yaml_online_data, default_path, update_online_list_of_datasets)

    } else { # Shouldn't happen, if else blocks should cover all supports block types from my coed

    }

    return(yaml_results)
}

# # Processed data equivalence checks
# processed_yaml_folder_data <- load_and_prepare_yaml_template(example_folder_yaml_path, here::here())
# processed_yaml_code_execution_data <- load_and_prepare_yaml_template(example_execution_yaml_path, here::here())
# processed_yaml_disk_data <- load_and_prepare_yaml_template(example_disk_yaml_path, here::here())
# processed_yaml_online_data <- load_and_prepare_yaml_template(example_online_yaml_path, here::here())
