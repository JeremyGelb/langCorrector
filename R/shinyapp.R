
#' @title Language general environment
#'
#' @description An environment used by Language to store data, functions and values
#' @keywords internal
Language_env <- new.env()



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### LAUNCHING FUNCTION for shiny apps ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# the variables used in the shiny environment must be declared as globalVariables
globalVariables(c("html_text", "parsed_rmd", "port", "rmdfile", "language"))


# corrector app for rmd files
rmd_corrector <- function(rmdfile, language, port = 8805){

  # creating a list to store all the data to pass to the shiny app
  shiny_data <- list()

  appDir <- system.file("shiny-examples", "language_checker", package = "Language")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `Language`.", call. = FALSE)
  }

  # checking if the parameters are ok  ----------------------------------------
  if(file.exists(rmdfile) == FALSE){
    stop(paste0("impossible to find the file: ", rmdfile))
  }

  #TODO : find all available languages !


  shiny_data$rmdfile <- rmdfile
  shiny_data$port <- port
  shiny_data$wmode <- "rendering"
  shiny_data$html_text <- ""
  shiny_data$language <- language

  assign('shiny_data', shiny_data, Language_env)

  ## starting the Language server
  starting_server(port)

  ## starting the shiny app
  shiny::runApp(appDir, display.mode = "normal", port = 8100)

  ## killing the server after that
  killing_server()
}
