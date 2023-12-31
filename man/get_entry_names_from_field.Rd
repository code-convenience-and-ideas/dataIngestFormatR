% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fill_dataset_paths.R
\name{get_entry_names_from_field}
\alias{get_entry_names_from_field}
\title{Loops over a list to get a vector of name entries}
\usage{
get_entry_names_from_field(list_to_extract_names)
}
\arguments{
\item{list_to_extract_names}{list of entries containing a name field}
}
\value{
vector of names for each entry in the list
}
\description{
Loops over a list to get a vector of name entries
}
\examples{
get_entry_names_from_field(list(
  list("name" = "a"),
  list("name" = "b")
))
}
