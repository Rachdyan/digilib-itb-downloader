#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(glue)
library(rvest)
library(purrr)
library(shinyWidgets)
library(bslib)
library(shinyBS)
library(dplyr)
library(reactable)
library(waiter)
library(httr)
library(jsonlite)
library(shinyFeedback)
library(shinyjs)
library(stringr)

source("all_function.R")
addResourcePath("www", "www")

# Define UI 
ui <- fluidPage(
  ## Set the themes, background and plugins that the application use
  useWaiter(),
  useShinyFeedback(),
  useShinyjs(),
  tags$head(
    includeCSS("www/style.css")
  ), 
  theme = bs_theme(version = 4),
  
  setBackgroundImage("www/background1.jpg"),
  ## Title of the Page
  fluidRow(
    column(8, align="center", offset = 2, 
           h1("Digilib ITB Downloader")
    )
  ),
  fluidRow(
    column(8, align="center", offset = 2, 
           h2("Download Open for Public Thesis from ITB Digital Library")
    )
  ),
  ## Search Bar and Go Button
  fluidRow(
    column(8, align="center", offset = 2,
           bs_card(  fluidRow(id="test",
                              column(9, align="center", offset = 1, 
                                     textInput("link", "",placeholder = "Input Digilib ITB Link (e.g., https://digilib.itb.ac.id/index.php/gdl/view/6757)", width = "100%"),
                              ),
                              column(2, align = "left", offset = -2,
                                     div(id = "div_button",  
                                         shinyWidgets::actionBttn("go", "Go", style = "simple", no_outline = T,
                                                                  color = "primary", size ="sm"),
                                     )
                              )
           )
           )
    ),
    ## Create Modal Popup to Display the Result
    bsModal("result", "Result", "go", size = "large", 
            htmlOutput("summary", style = "text-align: center; margin-bottom: 1rem;"),
            reactableOutput("result_table"),
            # dataTableOutput("result")
            # tableOutput("result")
            # shinyWidgets::actionBttn("merge_download", "Merge all the files", style = "simple", no_outline = T,
            #                          color = "primary", size ="sm")
            uiOutput("merge_download"),
            textOutput("merge_success"),
            uiOutput("download_merged_file")
    ),
    actionBttn("dummy", "dummy", style = "simple", no_outline = T, color = "primary", size ="sm")
  )
)


# Define server logi

server <- function(input, output, session){
  
  ## Create the loading bar using waiter
  w <- Waiter$new(id = c("summary", "result_table", "merge_download"),
                  html = spin_throbber(),
                  color = transparent(.5))
  
  w2 <- Waiter$new(id = c("merge_success", "download_merged_file"),
                   html = spin_throbber(),
                   color = transparent(.5))
  
  ## Check if the typed input is a Digilib ITB link
  digilib <- reactive(str_detect(input$link, "digilib.itb.ac.id"))
  

  ## Create an alert and prevent the user from clicking go if the input isn't Digilib  ITB link
  observe({
    if(is.null(input$link) || input$link == ""){
      hideFeedback("link")
      disable("go")
    } else if(!digilib()){
      feedbackWarning("link", !digilib(), "Please input Digilib ITB Link")
      disable("go")
    } else{
      hideFeedback("link")
      enable("go")
    }
  })


  ## Get the Input URL
  url <- reactive({
    req(digilib())
    input$link
  })

  req(url)
  
  ## Load the Page
  page <- reactive(read_html(url()))
  
  ## Get the title, writer, and number of files of the publication with the , title, and link of each file
  judul <- reactive(get_judul(page()))
  penulis <- reactive(get_penulis(page()))
  jumlah_file <- reactive(get_jumlah_file(page()))
  link_list <- reactive(get_link_awal(page()))
  judul_bab <- reactive(get_judul_bab(page()))
  
  ## Get the specific download link for each of the file
  # print("Getting link akhir")
  link_akhir <- reactive(
    map_chr(link_list(), get_link_akhir)
  )
  
  ## Create a table that stores the file title and download link
  result_table_content <- reactive({
    w$show()
    on.exit({
      w$hide()
    })
    
    tibble(`File` = judul_bab(), `Link` = link_akhir())
  })
  
  ## Create the summary text for the publication
  summary_text <- reactive({
    w$show()
    on.exit({
      w$hide()
    })
    
    glue('You are downloading a thesis titled <b>"{judul()}"</b> written by
                                <b>{penulis()}</b> with a total of <b>{jumlah_file()}</b>')
  })

  summary_html <- reactive(HTML(summary_text()))
  
  ## Render the summary text and table
  output$summary <- renderText(summary_html())
  output$result_table <- renderReactable(reactable(result_table_content(), defaultPageSize = 5, 
                                                   columns = list(
                                                     Link = colDef(cell = function(value){
                                                       htmltools::tags$a(href = value, target = "_blank", value)
                                                     })
                                                   )))
  
  ## Create a button to merge all the file & generate a download link
  output$merge_download <- renderUI({
    req(result_table_content())
    actionBttn("merge_download", "Merge All & Generate Download Link ", style = "simple", no_outline = T,
               color = "primary", size ="sm")
  }
  )
  
  ## Merge all the file into one pdf files using Convert API and get the download link
  download_link <- eventReactive(input$merge_download, {
    w2$show()
    on.exit({
      w2$hide()
    })
    
    files_list <- make_list(link_akhir())
    
    get_download_url(files_list)
  })
  
  ## Message that shows the file is successfully merged
  success_message <- reactive({
    w2$show()
    
    on.exit({
      w2$hide()
    })
    
    req(download_link())
    "Files succesfully merged!"
  })
  
  ## Render the Message
  output$merge_success <- renderText(success_message())
  
  ## Make a button to download the merged file
  output$download_merged_file <- renderUI({
    req(success_message())
    # a(class="btn btn-default", href= download_link(), "Download File")
    a(actionBttn("download_button", "Download File", style = "simple", no_outline = T,
                 color = "primary", size ="sm"), href= download_link())
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
