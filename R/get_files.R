#' Download the LanguageTool jar file
#' @description Download and unzip a jar file of LanguageTool (https://languagetool.org/download/LanguageTool-stable.zip)
#' @return TRUE if the installation worked, FALSE otherwise
#' @export
#' @examples
#' \donttest{
#' install_LanguageTools()
#' }
install_LanguageTools <- function(){
  url <- "https://languagetool.org/download/LanguageTool-stable.zip"
  destination <- paste0(find.package("langCorrector"),"/extdata/Language.zip")
  download.file(url, destination)
  unzip(destination, exdir = paste0(find.package("langCorrector"),"/extdata/LanguageTools"))
  return(is_LanguageTools_installed())
}

#' Find the path to the LanguageTool jar file
#' @description Find the path to the LanguageTool jar file in the local installation of the package
#' @return A string indicating the path to the jar file
get_jar_path <- function(){
  destination <- paste0(find.package("langCorrector"),"/extdata")
  all_files <- list.files(destination, recursive = TRUE, full.names = TRUE)
  file_names <- sapply(all_files, function(x){
    y <- strsplit(x, split = "/", fixed = TRUE)[[1]]
    return(y[[length(y)]])

  })
  ok_file <- all_files[file_names == "languagetool.jar"][[1]]
  return(ok_file)
}

#' Check is LanguageTools is installed
#' @description Check if LanguageTools has been installed locally
#' @return A boolean
#' @export
#' @examples
#' \donttest{
#' is_LanguageTools_installed()
#' }
is_LanguageTools_installed <- function(){
  destination <- paste0(find.package("langCorrector"),"/extdata")
  all_files <- list.files(destination, recursive = TRUE, full.names = FALSE, include.dirs = FALSE)
  file_names <- sapply(all_files, function(x){
    y <- strsplit(x, split = "/", fixed = TRUE)[[1]]
    return(y[[length(y)]])

    })
  return("languagetool.jar" %in% file_names)
}


#' Check if a current process could be the LanguageTools server
#' @description Check if a current process could be the LanguageTools server
#' @return A ps object representing the server or NULL
#' @importFrom ps ps ps_name ps_parent
running_server_id <- function(){
  all_services <- ps::ps()
  r_is_parent <- sapply(all_services$ps_handle, function(x){
    value <- tryCatch(
      {
        ps::ps_name(ps::ps_parent(x)) == "rsession.exe"

      },
      error=function(cond) {
        return(FALSE)
      }
    )
    return(value)
    })
  ok_service <- subset(all_services, grepl("java",all_services$name, fixed = TRUE) & r_is_parent)
  if(nrow(ok_service) == 0){
    warning("No services are running currently")
    return(NULL)
  }else if (nrow(ok_service) == 1){
    return(ok_service$ps_handle)
  }else{
    warning("Several Java services started from R are running...")
    return(ok_service$ps_handle)
  }

}


#' Start a LanguageTools local server
#' @description Start a LanguageTools local server
#' @param port The port of the server that must be used
#' @return An object of class processx::process representing the process of the server
#' @export
#' @examples
#' \donttest{
#' server_process <- starting_server(8805)
#' }
starting_server <- function(port = 8805){
  # look here : https://processx.r-lib.org/#running-an-external-process
  jar_path <- get_jar_path()
  p <- processx::process$new("java", c("-cp",jar_path, "org.languagetool.server.HTTPServer", "--port",port , "-- allow-origin"),  wd = NULL, echo_cmd = TRUE)
  if(p$is_alive() == FALSE){
    stop("Failed to start the server, please check if java is available in your system by running the command 'java -version'")
  }
  # if(Sys.getenv("JAVA_HOME") == ""){
  #   cmd <- paste0(
  #     "java -cp ",'"',jar_path,'"'," org.languagetool.server.HTTPServer --port ", port, " -- allow-origin"
  #   )
  # }else{
  #   cmd <- paste0(
  #     '"',java_path,'"', " -cp ",'"',jar_path,'"'," org.languagetool.server.HTTPServer --port ", port, " -- allow-origin"
  #   )
  # }
  # system(cmd, wait = FALSE)
  return(p)
}

#' Check if java is accessible
#' @description Check if java is accessible from the command line and is 64-bits
#' @return Nothing
#' @export
#' @examples
#' \donttest{
#' check_java()
#' }
check_java <- function(){

  out <- tryCatch(
    {
      # let us try to start java
      processx::run("java", args = "-version")
    }, error = function(cond){
      stop("impossible to start a java process, please check that java is accessible with the command : java -version")
    }
  )
  # testons si c'est du 64bits
  test <- grepl("64-Bit", out$stderr, fixed = TRUE)
  if(test){
    message("java seems accessible and 64-bits, perfect !")
  }else{
    warning("java is accessible, but it does not seem to be 64-bits...")
  }

}


#' Stop a LanguageTools local server
#' @description Stop a LanguageTools local server by killing its process
#' @param server_process An object processx::process representing the running
#'   server. If NULL, the function will try to kill the server by looking at
#'   running processes.
#' @return Nothing
#' @export
#' @examples
#' \donttest{
#' server_process <- starting_server(8805)
#' killing_server(server_process)
#' }
killing_server <- function(server_process = NULL){
  print("Killing the server as requested...")
  if(is.null(server_process)){
    server_process <- running_server_id()
    if(length(server_process)==1){
      ps::ps_kill(server_process[[1]])
    }else{
      stop("impossible to end the service safely, please do it manually")
    }
  }else{
    server_process$kill()
  }
}

#' Send a request to a running LanguageTools server
#' @description Send a text for correction to a running LanguageTools server
#' @param port The port of the server
#' @param text The string to check
#' @param language A string indicating which language to use
#' @seealso [install_lang_list()] and [get_lang_list()]
#' @return A list with the corrections from LanguageTools
#' @export
#' @examples
#' \donttest{
#' starting_server(8805)
#' corrections <- send_request(8805, "Hello there, it iss a obvious mistaken.", language = "EN")
#' }
send_request <- function(port = 8805, text, language){
  url <- paste0("http://localhost:",port,"/v2/check")
  resp <- httr::POST(url, body = list("text" = text,  "language" = language), encode = "form")
  js <- httr::content(resp, as = "parsed")
  return(js)
}


#' Create a local file with the available languages
#' @description Request a running server to save the available languages in a local file
#' @param port The port of the server
#' @seealso [get_lang_list()]
#' @return Nothing
#' @export
#' @examples
#' \donttest{
#' starting_server(8805)
#' install_lang_list(8805)
#' }
install_lang_list <- function(port = 8805){
  print("Starting the server...")
  server_process <- starting_server(port)
  url <- paste0("http://localhost:",port,"/v2/languages")
  resp <- httr::GET(url)
  values <- httr::content(resp)
  available_languages <- data.frame(do.call(rbind, values))
  out_file <- paste0(find.package("langCorrector"),"/extdata/available_languages.rda")
  save(available_languages, file = out_file)
  killing_server(server_process)
  print("...Done")
}

#' Provide the avaiblable languages
#' @description Provide the avaiblable languages if the local file containing them exists
#' @seealso [install_lang_list()]
#' @return A character vector
#' @export
#' @examples
#' \donttest{
#' get_lang_list()
#' }
get_lang_list <- function(){
  out_file <- paste0(find.package("langCorrector"),"/extdata/available_languages.rda")
  available_languages <- NULL
  if(file.exists(out_file)){
    load(out_file)
    return(available_languages)
  }else{
    warning("It seems that the available languages were not loaded... Have you run the function install_lang_list ?")
    return(available_languages)
  }
}


