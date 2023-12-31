% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_yaml_template.R
\name{update_online_dataset_entry}
\alias{update_online_dataset_entry}
\title{For an online yaml data block, update all of the defaults}
\usage{
update_online_dataset_entry(dataset_block, current_defaults)
}
\arguments{
\item{dataset_block}{a block of data for online datasets}

\item{current_defaults}{a block of default options for entries}
}
\value{
A dataset block with remote dataset defaults all applied
}
\description{
For an online yaml data block, update all of the defaults
}
\examples{
\dontrun{
update_online_dataset_entry(list(), list())
}
}
