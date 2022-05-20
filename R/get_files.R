
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_311")


# Download the jar file for language
install_Language <- function(){
  url <- "https://languagetool.org/download/LanguageTool-stable.zip"
  destination <- paste0(find.package("Language"),"/extdata/Language.zip")
  download.file(url, destination)
  unzip(destination, exdir = paste0(find.package("Language"),"/extdata/LanguageTools"))
}

# Find the file languagetool.jar
get_jar_path <- function(){
  destination <- paste0(find.package("Language"),"/extdata")
  all_files <- list.files(destination, recursive = TRUE, full.names = TRUE)
  file_names <- sapply(all_files, function(x){
    y <- strsplit(x, split = "/", fixed = TRUE)[[1]]
    return(y[[length(y)]])

  })
  ok_file <- all_files[file_names == "languagetool.jar"]
  return(ok_file)
}

# Check if languagetool.jar is available locally
is_Language_installed <- function(){
  destination <- paste0(find.package("Language"),"/extdata")
  all_files <- list.files(destination, recursive = TRUE, full.names = FALSE, include.dirs = FALSE)
  file_names <- sapply(all_files, function(x){
    y <- strsplit(x, split = "/", fixed = TRUE)[[1]]
    return(y[[length(y)]])

    })
  return("languagetool.jar" %in% file_names)
}


# Check if a server is running that could be language
running_server_id <- function(){
  library(ps)
  all_services <- ps()
  r_is_parent <- sapply(all_services$ps_handle, function(x){
    value <- tryCatch(
      {
        ps_name(ps_parent(x)) == "rsession.exe"

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
    warning("Several Java services strated from R are running...")
    return(ok_service$ps_handle)
  }

}

# Starting a Language local server
starting_server <- function(port){
  # look here : https://processx.r-lib.org/#running-an-external-process
  library(processx)
  jar_path <- get_jar_path()
  java_path <- paste0(Sys.getenv("JAVA_HOME"),"/bin/java")
  if(Sys.getenv("JAVA_HOME") == ""){
    cmd <- paste0(
      "java -cp ",'"',jar_path,'"'," org.languagetool.server.HTTPServer --port ", port, " -- allow-origin"
    )
  }else{
    cmd <- paste0(
      '"',java_path,'"', " -cp ",'"',jar_path,'"'," org.languagetool.server.HTTPServer --port ", port, " -- allow-origin"
    )
  }
  system(cmd, wait = FALSE)
}


# Killing a running language server
killing_server <- function(){
  server_process <- running_server_id()
  if(length(server_process)==1){
    ps::ps_kill(server_process[[1]])
  }else{
    stop("impossible to end the service safely, please do it manually")
  }

}

# Sending a request to the server
send_request <- function(port, text, language){
  url <- paste0("http://localhost:",port,"/v2/check")
  resp <- httr::POST(url, body = list("text" = text,  "language" = language), encode = "form")
  js <- httr::content(resp, as = "parsed")
  return(js)
}


# Listing all the langauges available on a server
install_lang_list <- function(port = 8805){
  print("Starting the server...")
  starting_server(port)
  url <- paste0("http://localhost:",port,"/v2/languages")
  resp <- httr::GET(url)
  values <- httr::content(resp)
  available_languages <- data.frame(do.call(rbind, values))
  out_file <- paste0(find.package("Language"),"/extdata/available_languages.rda")
  save(available_languages, file = out_file)
  killing_server()
  print("...Done")
}

# getting the list of all the languages available on a server
get_lang_list <- function(){
  out_file <- paste0(find.package("Language"),"/extdata/available_languages.rda")
  available_languages <- NULL
  if(file.exists(out_file)){
    load(out_file)
    return(available_languages)
  }else{
    warning("It seems that the available languages were not loaded... Have you run the function install_lang_list ?")
    return(available_languages)
  }
}


