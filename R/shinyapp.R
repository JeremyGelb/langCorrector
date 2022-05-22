
#' @title langCorrector general environment
#'
#' @description An environment used by langCorrector to store data, functions and values
#' @export
Language_env <- new.env()



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### LAUNCHING FUNCTION for shiny apps ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# the variables used in the shiny environment must be declared as globalVariables
globalVariables(c("html_text", "parsed_rmd", "port", "rmdfile", "language"))


#' Rmarkdown corrector Shiny app
#' @description Start a Shiny app to correct and edit Rmarkdown files
#' @param rmdfile Astring, the path to the rmd file to edit
#' @param language A string, the language to use by LanguageTools
#' @param port The port to use for the LanguageTools server
#' @return Nothing
#' @export
#' @examples
#' \donttest{
#' rmd_corrector("path/to/myfile.rmd", "EN", 8805)
#' }
rmd_corrector <- function(rmdfile, language, port = 8805){

  # creating a list to store all the data to pass to the shiny app
  shiny_data <- list()

  appDir <- system.file("shiny-examples", "language_checker", package = "langCorrector")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `langCorrector`.", call. = FALSE)
  }

  # checking if the parameters are ok  ----------------------------------------
  if(file.exists(rmdfile) == FALSE){
    stop(paste0("impossible to find the file: ", rmdfile))
  }

  shiny_data$rmdfile <- rmdfile
  shiny_data$port <- port
  shiny_data$wmode <- "rendering"
  shiny_data$html_text <- ""
  shiny_data$language <- language
  ## starting the Language server
  server_proc <- starting_server(port)
  shiny_data$server_process <- server_proc
  assign('shiny_data', shiny_data, Language_env)

  ## starting the shiny app
  shiny::runApp(appDir, display.mode = "normal", port = 8100)

  ## killing the server after that (now used by onStop)
  # killing_server(server_proc)
}
