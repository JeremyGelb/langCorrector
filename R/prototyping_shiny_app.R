library(shiny)
library(shinyjs)
library(parsermd)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### HELPER FUNCTIONS FOR THE APPLICATION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_next_textbloc <- function(document, i = 1){
  found <- FALSE
  idx <- 0
  if(i == length(document)){
    return(i)
  }
  for (j in (i+1):length(document)){
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

find_previous_textbloc <- function(document, i = 1){
  found <- FALSE
  idx <- 0
  if(i == 1){
    return(i)
  }
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


edit_rmd_txt <- function(new_text, rmd_obj, id){
  elements <- strsplit(new_text, "\n", fixed = TRUE)[[1]]
  for (i in 1:length(elements)){
    rmd_obj[[id]][[i]] <- elements[[i]]
  }
  rmd_obj[[id]][[(length(elements) +1)]] <- ""
  return(rmd_obj)
}


save_current_text <- function(){
  jscode <- "
  var text = document.getElementById('text_pannel').children[0].value;
  console.log(text);
  Shiny.setInputValue('currentText', document.getElementById('text_pannel').children[0].value);
  "
  runjs(jscode)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### PRE-APPLICATION ELEMENTS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# rmdfile <- "E:/R/Packages/Language/inst/01-priseenmainR.Rmd"
#
# ## step1 : parse the rmd file
# parsed_rmd <- parse_rmd(rmdfile)
#
#
# ## step2 : find the next bloc of text
# actual_bloc <- 1
# first_bloc <- find_next_textbloc(parsed_rmd, actual_bloc)
# html_text <- markdown::markdownToHTML(text = paste(parsed_rmd[[first_bloc]], collapse = "\n"), fragment.only = TRUE)
#
# ## global variables
# wmode <- "rendering"




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### APPLICATION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ui <- fluidPage(
#   useShinyjs(),
#   ## WE WILL HAVE ONLY ONE PANNEL FOR THIS APP
#
#   ## ----------------------- ENTETE DE l'APPLICATION ----------------------- ##
#   fluidRow(wellPanel("EN TETE DE L'APPLICATION")),
#   fluidRow(
#     column(width = 2, actionButton("prev_button", "Previous Text")),
#     column(width = 2, actionButton("next_button", "Next Text"))
#   ),
#   fluidRow(
#     ## ----------------- La première colonne : texte éditable ----------------- ##
#     column(width = 6,
#            fluidRow(
#              column(width = 3, actionButton("edit_button", "Edit text")),
#              column(width = 3, actionButton("render_button", "Render text"))
#            ),
#            fluidRow(tags$div(id="text_pannel", checked=NA, HTML(html_text))),
#            ),
#
#     ## ----------------- La deuxième colonne : affiche les corrections ----------------- ##
#     column(width = 6, wellPanel("PANNEL DE DROITE DE L'APPLICATION POUR LES CORRECTIONS")),
#   )
# )
#
#
#
# # Define server logic to plot various variables against mpg ----
# server <- function(input, output) {
#
#   ## step1 : parse the rmd file
#   parsed_rmd <- parse_rmd(rmdfile)
#
#
#   ## step2 : find the next bloc of text
#   actual_bloc <- 1
#   actual_bloc <- find_next_textbloc(parsed_rmd, actual_bloc)
#   html_text <- markdown::markdownToHTML(text = paste(parsed_rmd[[actual_bloc]], collapse = "\n"), fragment.only = TRUE)
#   shinyjs::html(id = "text_pannel", html = html_text)
#
#   ## ------------------ EVENT LISTENER0 : NEXT BLOC OF TEXT ---------------------------
#   observeEvent(input$next_button, {
#     actual_bloc <<- find_next_textbloc(parsed_rmd, actual_bloc)
#     html_text <<- markdown::markdownToHTML(text = paste(parsed_rmd[[actual_bloc]], collapse = "\n"), fragment.only = TRUE)
#     shinyjs::html(id = "text_pannel", html = html_text)
#     wmode <<- "rendering"
#   })
#
#   ## ------------------ EVENT LISTENER1 : previous BLOC OF TEXT ---------------------------
#   observeEvent(input$prev_button, {
#     actual_bloc <<- find_previous_textbloc(parsed_rmd, actual_bloc)
#     html_text <<- markdown::markdownToHTML(text = paste(parsed_rmd[[actual_bloc]], collapse = "\n"), fragment.only = TRUE)
#     shinyjs::html(id = "text_pannel", html = html_text)
#     wmode <<- "rendering"
#   })
#
#   ## ------------------ EVENT LISTENER 2 : PASS TO EDIT MODE ---------------------------
#   observeEvent(input$edit_button, {
#     if(wmode != "editing"){
#       string <- paste(parsed_rmd[[actual_bloc]], collapse = "\n")
#       balise <- paste0("<textarea>",string,"</textarea>")
#       shinyjs::html(id = "text_pannel", html = balise)
#       wmode <<- "editing"
#     }else{
#       print("we are already in the editing mode!")
#     }
#   })
#
#   ## ------------------ EVENT LISTENER 3 : PASS TO RENDERING MODE ---------------------------
#   observeEvent(input$render_button, {
#     if(wmode != "rendering"){
#       save_current_text()
#       html_text <<- markdown::markdownToHTML(text = paste(parsed_rmd[[actual_bloc]], collapse = "\n"), fragment.only = TRUE)
#       shinyjs::html(id = "text_pannel", html = html_text)
#       wmode <<- "rendering"
#     }else{
#       print("we are already in the rendering mode!")
#     }
#   })
#
#   ## ------------------ EVENT LISTENER 4 : follow user text ---------------------------
#   observeEvent(input$currentText, {
#     # getting the text
#     actual_text <- input$currentText
#     print(actual_text)
#     # saving it in the rmd object
#     parsed_rmd <<- edit_rmd_txt(actual_text, parsed_rmd, actual_bloc)
#     html_text <<- markdown::markdownToHTML(text = paste(parsed_rmd[[actual_bloc]], collapse = "\n"), fragment.only = TRUE)
#     shinyjs::html(id = "text_pannel", html = html_text)
#     wmode <<- "rendering"
#   })
# }

# shinyApp(ui, server)
