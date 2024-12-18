% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_eos_data-deprecated.R
\name{read_eos_data}
\alias{read_eos_data}
\title{read_eos_data is Deprecated}
\usage{
read_eos_data(
  eos_table,
  from_path = paste0(set_dir_NVI("EOS"), "RaData"),
  year = NULL,
  colClasses = "character",
  encoding = "UTF-8",
  ...
)
}
\arguments{
\item{eos_table}{[\code{character(1)}]\cr
The name of the table with eos raw data.}

\item{from_path}{[\code{character(1)}]\cr
Path for raw data from eos_data.}

\item{year}{[\code{character} | \code{numeric}]\cr
The years to be included in the result. Can be both numeric
or character. Defaults to \code{NULL}, i.e. no selection.}

\item{colClasses}{[\code{character}]\cr
The class of the columns, as in
\ifelse{html}{\code{\link[utils:read.table]{utils::read.table}}}{\code{utils::read.table}}.
Defaults to \code{"character"}.}

\item{encoding}{[\code{character(1)}]\cr
The encoding, one of c("UTF-8", "latin1"). Defaults to \code{"UTF-8"}.}

\item{\dots}{Other arguments to be passed to
\ifelse{html}{\code{\link[data.table:fread]{data.table::fread}}}{\code{data.table::fread}}.}
}
\description{
\code{read_eos_data} was deprecated in NVIdb v0.13.0 released
    2024-##-##. All PJS related functions have been moved to \code{NVIpjsr}.
    Use \code{NVIpjsr::read_eos_data} instead. When attaching packages,
    remember to attach \code{NVIdb} before \code{NVIpjsr}.
}
\details{
The old help pages can be found at \code{help("read_eos_data-deprecated")}.
    Information on deprecated functions can be found at \code{help("NVIdb-deprecated")}.
}
\keyword{internal}
