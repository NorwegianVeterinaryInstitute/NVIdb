% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exclude_from_PJSdata-deprecated.R
\name{exclude_from_PJSdata}
\alias{exclude_from_PJSdata}
\title{exclude_from_PJSdata is Deprecated}
\usage{
exclude_from_PJSdata(PJSdata, abroad = "exclude", quality = "exclude")
}
\arguments{
\item{PJSdata}{Data frame with data extracted from PJS.}

\item{abroad}{If equal "exclude", samples from abroad are excluded. Allowed
values are c("exclude", "include").}

\item{quality}{If equal "exclude", samples registered as quality assurance
and ring trials are excluded. Allowed values are c("exclude", "include").}
}
\description{
\code{exclude_from_PJSdata} was deprecated in NVIdb v0.13.0 released
    2024-##-##. All PJS related functions have been moved to \code{NVIpjsr}.
    Use \code{NVIpjsr::exclude_from_PJSdata} instead. When attaching packages,
    remember to attach \code{NVIdb} before \code{NVIpjsr}.
}
\details{
The old help pages can be found at \code{help("exclude_from_PJSdata-deprecated")}.
    Information on deprecated function can be found at \code{help("NVIdb-deprecated")}.
}
\keyword{internal}
