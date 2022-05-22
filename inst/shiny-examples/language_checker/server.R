library(shiny)
library(parsermd)
library(shinyhelper)
library(shinyjs)
library(htmltools)
library(magrittr)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### HELPER FUNCTIONS FOR THE APPLICATION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# find the next bloc of text in a parsed rmd document
find_next_textbloc <- function(document, i = 1){
  found <- FALSE
  idx <- 0
  if(i == length(document)){
    return(0)
  }
  for (j in (i+1):length(document)){
    cls <- class(document[[j]])
    if(cls == "rmd_markdown"){
      found <- TRUE
      idx <- j
      break
    }
    if(j == length(document)){
      break
    }
  }
  if(found){
    return(idx)
  }else{
    return(0)
  }
}

# find the previous bloc of text in a parsed rmd document
find_previous_textbloc <- function(document, i = 1){
  found <- FALSE
  idx <- 0
  for (j in (i-1):1){
    cls <- class(document[[j]])
    if(cls == "rmd_markdown"){
      found <- TRUE
      idx <- j
      break
    }
  }
  if(found){
    return(idx)
  }else{
    return(0)
  }
}

# Save the edits done to a block of text
edit_rmd_txt <- function(new_text, rmd_obj, id){
  elements <- strsplit(new_text, "\n", fixed = TRUE)[[1]]
  for (i in 1:length(elements)){
    rmd_obj[[id]][[i]] <- elements[[i]]
  }
  rmd_obj[[id]][[(length(elements) +1)]] <- ""
  return(rmd_obj)
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### function for style correction ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# A function that prepare an error message
build_error_message <- function(error){
  h2title <- tags$h2(paste0(error$rule$id,": ",error$shortMessage))
  pbody <- tags$p(error$message)
  pbody <- tags$p("proposed corrections: ")
  listre <- tags$ul()
  for (rep in error$replacements){
    listre$children[[length(listre$children) + 1]] <- tags$li(rep$value)
  }
  main_div <- tags$span(h2title, pbody, listre, class = "tooltiptext")
  final_str <- htmltools::doRenderTags(main_div, indent = FALSE)
  return(final_str)
}

# a function that highlights the text in some html if there are errors
# the point would be to add some over pannel with javascript when there is an error
highlight_html_errors <- function(text, corrections){
  errors <- corrections$matches
  decal <- 0
  new_text <- text
  js_script <- "
  console.log('Hello World !');
  var messages = ["
  for (i in 0:(length(errors)-1)){
    el <- errors[[i+1]]
    msg <- build_error_message(el)
    js_script <- paste0(js_script,"`",msg,"`,")
    part1 <- substr(new_text, 1, el$offset+decal)
    part2 <- paste0("<span class = '",el$rule$category$id," lerror' id = 'error_",i,"'>")
    part3 <- "</span>"
    pb_text <- substr(new_text, el$offset+1+decal, el$offset + decal + el$length)
    new_text <- paste0(
      part1,
      part2,
      pb_text,
      part3,
      substr(new_text, el$offset+ 1 + decal + el$length, nchar(new_text))
    )
    decal <- decal + nchar(paste0(part2,part3))
  }
  js_script2 <- paste0(substr(js_script, 1, (nchar(js_script)-1)),"];")
  js_script3 <- '
  localStorage.setObj("messages",messages);
  document.getElementById("correction_pannel").addEventListener("click", function(e) {
    console.log(e.target);
    if(e.target.className.includes("lerror")){
      var nb = parseInt(e.target.id.split("_")[1]);
      $("#show_corrections").html(localStorage.getObj("messages")[nb]);
      console.log(localStorage.getObj("messages")[nb]);
    }
  })
  '
  all_script <- paste(js_script2, js_script3, sep = "\n")
  runjs(all_script)

  ## I want to correct for some errors in the html
  new_text <- remove_html_within(new_text, "`")
  new_text <- remove_html_within(new_text, "$")
  new_text <- remove_html_within(new_text, "$$")

  return(new_text)
}




# A simple function to get corrections and render the text
correct_and_render <- function(port, language, my_text){
  corrections <- send_request(port, my_text, language)
  # rendering
  if(length(corrections$matches) > 0){
    corrected_text <- highlight_html_errors(my_text, corrections)
  }else{
    corrected_text <- my_text
  }
  html_text <<- markdown::markdownToHTML(text = corrected_text, fragment.only = TRUE)
  return(html_text)
}


# this function will replace code elements in text with ids that will help to reinsert them latter
remove_html_within <- function(x, sep){

  regEx_expr <- paste0("\\",sep,"(.*?)\\",sep)
  locations <- stringr::str_locate_all(x, regEx_expr)[[1]]

  if(nrow(locations) > 0){
    new_strings <- sapply(1:nrow(locations), function(i){
      coords <- locations[i,]
      pb_string <- substr(x,coords[[1]], coords[[2]])
      new_string <- stringr::str_remove_all(pb_string, "\\<(.*?)\\>")
      return(new_string)
    })

    newx <- stringr::str_replace_all(x, regEx_expr, new_strings)
    newx <- gsub(paste0(sep,"</span>"), sep, newx)
  }else{
    newx <- x
  }


  return(newx)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Utilities for the UI ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Lock and unlock buttons
LockButtons <- function(actual_bloc, parsed_rmd){
    #if we are at the end, or the begining, I should block some buttons
    whatsNext <- find_next_textbloc(parsed_rmd, actual_bloc)
    whatsPrev <- find_previous_textbloc(parsed_rmd, actual_bloc)

    if(whatsNext == 0){
      shinyjs::disable("next_button")
    }else{
      shinyjs::enable("next_button")
    }

    if(whatsPrev == 0){
      shinyjs::disable("prev_button")
    }else{
      shinyjs::enable("prev_button")
    }
}

# AutoSave
AutomaticSave <- function(rmdfile, parsed_rmd){
  fname <- basename(rmdfile)
  dirName <- dirname(rmdfile)
  elements <- strsplit(fname, ".", fixed = TRUE)[[1]]
  backup_name <- paste0(dirName,"/",elements[[1]],"_AutoSave.",elements[[2]])
  writeLines(as_document(parsed_rmd), con = backup_name)
  showNotification("We just did an automatic save, we got your back!", duration = 2)
}

# # a help button wiht JS
# ## loading the helper messages
# helper_folder <- system.file("shiny-examples/language_checker/www/help_mds",
#                              package = "langCorrector", mustWork = TRUE)
# filname <- paste(helper_folder, "nextbutton", sep = "/")
# helper_string <- readChar(filname, file.info(filname)$size)
#
# show_help <- function(){
#   html_content <- paste0("<div id = 'helpBox'>", helper_string, "</div>")
# }

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### pre-work before running the server ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


## loading all the variables from shiny_app list
Language_env <- langCorrector::Language_env
shiny_data <- Language_env$shiny_data
for (name in names(shiny_data)){
  assign(name, shiny_data[[name]])
}

## parseing the rmd file
parsed_rmd <- parse_rmd(rmdfile)
to_show <- "next"



## parseing the rmd file
server <- function(input, output, session) {

  ## prepare the helpers
  observe_helpers()
  runjs("$('#helpDiv').hide();")

  ## preparing the first text
  actual_bloc <- 1
  actual_bloc <- find_next_textbloc(parsed_rmd, actual_bloc)
  shinyjs::disable("prev_button")

  string <- paste(parsed_rmd[[actual_bloc]], collapse = "\n")
  html_text <- correct_and_render(port, language, string)
  #balise <- paste0("<textarea style = 'width : 100% ; height : 15em'>",string,"</textarea>")
  shinyjs::html(id = "correction_pannel", html = html_text)
  #shinyjs::html(id = "text_pannel", html = balise)
  updateTextAreaInput(session, "text_pannel", value = string)
  runjs('$("#text_pannel").height($("#correction_pannel").height());')

  ## base file (on garde une copie du RMD si jamais !)
  base_rmd <- parsed_rmd


  ## ------------------ EVENT LISTENER0 : NEXT BLOC OF TEXT ---------------------------
  observeEvent(input$next_button, {
    # step 1 : saving the edits in the text
    #   # getting the text
    to_show <<- "next"

    # saving it in the rmd object
    actual_text <- { input$text_pannel }
    parsed_rmd <<- edit_rmd_txt(actual_text, parsed_rmd, actual_bloc)

    actual_bloc <<- find_next_textbloc(parsed_rmd, actual_bloc)
    LockButtons(actual_bloc, parsed_rmd)

    my_text <- paste(parsed_rmd[[actual_bloc]], collapse = "\n")
    html_text <- correct_and_render(port, language, my_text)
    string <- paste(parsed_rmd[[actual_bloc]], collapse = "\n")
    shinyjs::html(id = "correction_pannel", html = html_text)
    updateTextAreaInput(session, "text_pannel", value = string)
    runjs('$("#text_pannel textarea").height($("#correction_pannel").height());')
    # and the automatic save if required
    if(input$AutoSave){
      AutomaticSave(rmdfile, parsed_rmd)
    }

  })

  ## ------------------ EVENT LISTENER1 : previous BLOC OF TEXT ---------------------------
  observeEvent(input$prev_button, {
    # step 1 : saving the edits in the text
    #   # getting the text
    to_show <<- "prev"
    actual_text <- { input$text_pannel }

    # saving it in the rmd object
    parsed_rmd <<- edit_rmd_txt(actual_text, parsed_rmd, actual_bloc)

    actual_bloc <<- find_previous_textbloc(parsed_rmd, actual_bloc)
    LockButtons(actual_bloc, parsed_rmd)

    my_text <- paste(parsed_rmd[[actual_bloc]], collapse = "\n")
    html_text <- correct_and_render(port, language, my_text)
    string <- paste(parsed_rmd[[actual_bloc]], collapse = "\n")
    shinyjs::html(id = "correction_pannel", html = html_text)
    updateTextAreaInput(session, "text_pannel", value = string)

    runjs('$("#text_pannel textarea").height($("#correction_pannel").height());')
    # and the automatic save if required
    if(input$AutoSave){
      AutomaticSave(rmdfile, parsed_rmd)
    }
  })

  ## ------------------ EVENT LISTENER2 : Refresh the actual view ---------------------------
  observeEvent(input$refresh_button, {
    to_show <<- "stay"

    actual_text <- { input$text_pannel }
    # saving it in the rmd object
    parsed_rmd <<- edit_rmd_txt(actual_text, parsed_rmd, actual_bloc)


    my_text <- paste(parsed_rmd[[actual_bloc]], collapse = "\n")
    html_text <- correct_and_render(port, language, my_text)
    string <- paste(parsed_rmd[[actual_bloc]], collapse = "\n")
    shinyjs::html(id = "correction_pannel", html = html_text)
    updateTextAreaInput(session, "text_pannel", value = string)

    runjs('$("#text_pannel textarea").height($("#correction_pannel").height());')
    # and the automatic save if required
    if(input$AutoSave){
      AutomaticSave(rmdfile, parsed_rmd)
    }

  })

  ## ------------------ OnStop : what to do when the APP STOP ---------------------------
  onStop(function(){
    # print(server_process_id)
    # we kill the process !
    killing_server(server_process)
  })

  ## ------------------ EVENT LISTENER3 : follow user text ---------------------------
  # observeEvent(input$currentText, {
  #   # getting the text
  #   actual_text <- input$currentText
  #   # saving it in the rmd object
  #   parsed_rmd <<- edit_rmd_txt(actual_text, parsed_rmd, actual_bloc)
  #   # showing the next or the previous text
  #   if(to_show == "next"){
  #     print("requesting a next move")
  #     actual_bloc <<- find_next_textbloc(parsed_rmd, actual_bloc)
  #   }else if (to_show == "prev"){
  #     print("requesting a previous move")
  #     actual_bloc <<- find_previous_textbloc(parsed_rmd, actual_bloc)
  #   }else if(to_show == "stay"){
  #     print("requesting a refresh move")
  #   }else{
  #     print("to_show does not have an expected value man...")
  #   }
  #
  #   # if we are at the end, or the begining, I should block some buttons
  #   print(paste0("Actual bloc is : ", actual_bloc))
  #   whatsNext <- find_next_textbloc(parsed_rmd, actual_bloc)
  #   print(paste0("whatsNext is : ", whatsNext))
  #   whatsPrev <- find_previous_textbloc(parsed_rmd, actual_bloc)
  #
  #   if(whatsNext == 0){
  #     shinyjs::disable("next_button")
  #   }else{
  #     shinyjs::enable("next_button")
  #   }
  #
  #   if(whatsPrev == 0){
  #     shinyjs::disable("prev_button")
  #   }else{
  #     shinyjs::enable("prev_button")
  #   }
  #
  #   my_text <- paste(parsed_rmd[[actual_bloc]], collapse = "\n")
  #   print("### got my_text ###")
  #   print(my_text)
  #   html_text <- correct_and_render(port, language, my_text)
  #   print("### html_text was corrected and rendered ###")
  #
  #   string <- paste(parsed_rmd[[actual_bloc]], collapse = "\n")
  #   balise <- paste0("<textarea style = 'width : 100% ; height : 15em'>",string,"</textarea>")
  #   shinyjs::html(id = "correction_pannel", html = html_text)
  #   shinyjs::html(id = "text_pannel", html = balise)
  #
  #   runjs('$("#text_pannel textarea").height($("#correction_pannel").height());')
  #
  #   print("### end of main function ###")
  #   print(actual_bloc)
  # })

  ## ------------------ EVENT LISTENER4 : Reset the original text ---------------------------
  observeEvent(input$reset_button, {
    backed_text <- base_rmd[[actual_bloc]]
    my_text <- paste(backed_text, collapse = "\n")
    html_text <- correct_and_render(port, language, my_text)

    string <- paste(backed_text, collapse = "\n")
    balise <- paste0("<textarea style = 'width : 100% ; height : 15em'>",string,"</textarea>")
    shinyjs::html(id = "correction_pannel", html = html_text)
    shinyjs::html(id = "text_pannel", html = balise)
    runjs('$("#text_pannel textarea").height($("#correction_pannel").height());')
  })

  ## ------------------ EVENT LISTENER5 : SAVE THE MODIFICATIONS ---------------------------
  observeEvent(input$save_button, {
    fname <- basename(rmdfile)
    dirName <- dirname(rmdfile)
    elements <- strsplit(fname, ".", fixed = TRUE)[[1]]
    datetime <- gsub(":","-",as.character(Sys.time()), fixed = TRUE)
    backup_name <- paste0(dirName,"/",elements[[1]],"_",datetime,".",elements[[2]])
    if(input$KeepTrack){
      writeLines(as_document(base_rmd), con = backup_name)
      msg <- "The file has been saved with the modifications, a backup was also saved"
    }else{
      msg <- "The file has been saved with the modifications"
    }
    writeLines(as_document(parsed_rmd), con = rmdfile)
    showNotification(msg)
  })

  ## ------------------ EVENT LISTENER6 : SHOW ME HELP  ---------------------------
  observeEvent(input$help_button, {
    # showNotification(
    #   "Here are some tips to use this application:
    #   <ul>
    #
    #   <li>click on the next button to got to the next block of text of your markdown document;</li>
    #   <li>click on the previous button to got to the previous block of text of your markdown document;</li>
    #   <li>you can edit your text in the left pannel;</li>
    #   <li>when you hit Refresh, the right pannel is updated</li>
    #   <li>when you hit Reset, it will reset you text as it was in the orignal rmarkdown file</li>
    #   <li>when you hit Save, it will save your edits in a rmarkdown file</li>
    #   <li>As long as Auto Save is active, navigating to next or previous block of text will also save your edits in a temporary rmarkdown file</li>
    #   <li>As long as Keep Track is active, saving your edits will create a temporary file with the current datetime in its name. It will contain a backup of the rmarkdown before your modifications. We higlhy recommand to keep it active!</li>
    #   </ul>",
    #   closeButton = TRUE,
    #   duration = NULL
    # )
    runjs("var test = $('#helpDiv').is(':hidden');
    if(test){
      $('#helpDiv').show(400);
    }else{
      $('#helpDiv').hide(400);
    }")
  })
}
