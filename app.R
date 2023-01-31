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

# Define UI for application that draws a histogram
ui <- fluidPage(
  useWaiter(),
  useShinyFeedback(),
  useShinyjs(),
  tags$head(
    includeCSS("www/style.css")
  ), 
  theme = bs_theme(version = 4),
  
  setBackgroundImage("www/background1.jpg"),
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


# Define server logic required to draw a histogram

server <- function(input, output, session){
  
  
  # w <- Waiter$new(id = c("summary", "result_table", "merge_download", "merge_success", "download_merged_file"),
  #                 html = spin_throbber(),
  #                 color = transparent(.5))
  
  w <- Waiter$new(id = c("summary", "result_table", "merge_download"),
                  html = spin_throbber(),
                  color = transparent(.5))
  
  w2 <- Waiter$new(id = c("merge_success", "download_merged_file"),
                   html = spin_throbber(),
                   color = transparent(.5))
  
  
  digilib <- reactive(str_detect(input$link, "digilib.itb.ac.id"))
  
  # observe({
  #   if(is.null(input$link) || input$link == ""){
  #     disable("go")
  #   } else if(!digilib()){
  #     disable("go")
  #   } else{
  #     enable("go")
  #   }
  # })
  # 
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


  url <- reactive({
    req(digilib())
    input$link
  })

  
  # url <- eventReactive(input$go,{
  #   input$link
  # })
  
  req(url)
  
  page <- reactive(read_html(url()))
  
  judul <- reactive(get_judul(page()))
  penulis <- reactive(get_penulis(page()))
  jumlah_file <- reactive(get_jumlah_file(page()))
  
  link_list <- reactive(get_link_awal(page()))
  
  link_akhir <- reactive(
    map_chr(link_list(), get_link_akhir)
  )
  
  judul_bab <- reactive(get_judul_bab(page()))
  
  result_table_content <- reactive({
    w$show()
    
    on.exit({
      w$hide()
    })
    
    tibble(`File` = judul_bab(), `Link` = link_akhir())
  })
  
  summary_text <- reactive({
    
    w$show()
    
    on.exit({
      w$hide()
    })
    
    glue('You are downloading a thesis titled <b>"{judul()}"</b> written by
                                <b>{penulis()}</b> with a total of <b>{jumlah_file()}</b>')
  })
  
  summary_html <- reactive(HTML(summary_text()))
  
  output$summary <- renderText(summary_html())
  output$result_table <- renderReactable(reactable(result_table_content(), defaultPageSize = 5, 
                                                   columns = list(
                                                     Link = colDef(cell = function(value){
                                                       htmltools::tags$a(href = value, target = "_blank", value)
                                                     })
                                                   )))
  
  
  output$merge_download <- renderUI({
    req(result_table_content())
    actionBttn("merge_download", "Merge All & Generate Download Link ", style = "simple", no_outline = T,
               color = "primary", size ="sm")
  }
  )
  
  download_link <- eventReactive(input$merge_download, {
    w2$show()
    
    on.exit({
      w2$hide()
    })
    
    files_list <- make_list(link_akhir())
    
    get_download_url(files_list)
  })
  
  success_message <- reactive({
    w2$show()
    
    on.exit({
      w2$hide()
    })
    
    req(download_link())
    "Files succesfully merged!"
  })
  
  output$merge_success <- renderText(success_message())
  
  output$download_merged_file <- renderUI({
    req(success_message())
    # a(class="btn btn-default", href= download_link(), "Download File")
    a(actionBttn("download_button", "Download File", style = "simple", no_outline = T,
                 color = "primary", size ="sm"), href= download_link())
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
