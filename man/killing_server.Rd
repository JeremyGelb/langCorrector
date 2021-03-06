% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_files.R
\name{killing_server}
\alias{killing_server}
\title{Stop a LanguageTools local server}
\usage{
killing_server(server_process = NULL)
}
\arguments{
\item{server_process}{An object processx::process representing the running
server. If NULL, the function will try to kill the server by looking at
running processes.}
}
\value{
Nothing
}
\description{
Stop a LanguageTools local server by killing its process
}
\examples{
\donttest{
server_process <- starting_server(8805)
killing_server(server_process)
}
}
