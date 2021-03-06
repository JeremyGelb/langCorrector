% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_files.R
\name{starting_server}
\alias{starting_server}
\title{Start a LanguageTools local server}
\usage{
starting_server(port = 8805)
}
\arguments{
\item{port}{The port of the server that must be used}
}
\value{
An object of class processx::process representing the process of the server
}
\description{
Start a LanguageTools local server
}
\examples{
\donttest{
server_process <- starting_server(8805)
}
}
