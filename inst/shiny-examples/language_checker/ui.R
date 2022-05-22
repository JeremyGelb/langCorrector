library(shiny)
library(shinyjs)
library(shinyWidgets)
library(magrittr)

## loading all the variables from shiny_app list
Language_env <- langCorrector::Language_env
shiny_data <- Language_env$shiny_data
for (name in names(shiny_data)){
  assign(name, shiny_data[[name]])
}


# a help button wiht JS
## loading the helper messages
helper_folder <- system.file("shiny-examples/language_checker/www/help_mds",
                             package = "langCorrector", mustWork = TRUE)
filname <- paste(helper_folder, "nextbutton", sep = "/")
helper_string <- readChar(filname, file.info(filname)$size)



ui <- fluidPage( theme = shinythemes::shinytheme("flatly"),
  useShinyjs(),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "error_styles.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "main_sytle.css"),
    tags$script(src = "jquery-3.6.0.min.js"),
    tags$script("
                Storage.prototype.setObj = function(key, obj) {
                  return this.setItem(key, JSON.stringify(obj))
                }
                Storage.prototype.getObj = function(key) {
                  return JSON.parse(this.getItem(key))
                }
                localStorage.setObj('messages',[]); console.log('starting the app');")
  ),

  ## WE WILL HAVE ONLY ONE PANNEL FOR THIS APP

  ## ----------------------- ENTETE DE l'APPLICATION ----------------------- ##
  fluidRow(wellPanel("THE LANGUAGE CORRECTOR FOR RMARKDOWN")),
  shiny::div(id = "helpDiv", HTML(helper_string)),
  shiny::h4("Navigation"),
  fluidRow(
    column(width = 1, actionButton("prev_button", "Previous Text")),
    column(width = 1, actionButton("next_button", "Next Text")),
    column(width = 7),
    column(width = 1, materialSwitch(inputId = "AutoSave", label = "Auto Save", status = "primary", value = TRUE)),
    column(width = 1, materialSwitch(inputId = "KeepTrack", label = "Keep Track", status = "primary", value = TRUE)),
    column(width = 1, actionButton(inputId = "help_button", "Help?"))
  ),
  shiny::br(),
  fluidRow(
    ## ----------------- La première colonne : texte éditable ----------------- ##
    column(width = 6,
           h4("Edit your text here"),
           textAreaInput("text_pannel", label = NULL) %>%
             tagAppendAttributes(class = 'main_pannels', style = 'width : 95%;')
           #tags$div(id="text_pannel", class = "main_pannels", checked=NA, HTML(html_text))
    ),

    ## ----------------- La deuxième colonne : le texte avec le rendu ----------------- ##
    column(width = 6,
           h4("Rendered"),
           tags$div(id="correction_pannel", class = "main_pannels", checked=NA, HTML(html_text))
    )
  ),
  shiny::h4("Text editing"),
  fluidRow(
    column(width = 1, actionButton("refresh_button", "Refresh")),
    column(width = 1, actionButton("reset_button", "Reset")),
    column(width = 1, actionButton("save_button", "Save"))
  ),
  fluidRow(tags$div(id="show_corrections"))

)
