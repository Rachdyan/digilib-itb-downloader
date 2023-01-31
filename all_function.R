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


bs_card <- function(content){
  
  shiny::HTML(glue(
    '<div class="card">
      <div class="card-body">
    {content}
    </div>
      </div>'
  )
  )
  
}

get_link_akhir <- function(link){
  link_akhir <- read_html(link) %>% html_nodes("div") %>%  html_attr("data-url")
}

get_link_awal <- function(page){
  link_awal <- page %>% html_nodes("div[class = 'container']") %>% 
    html_nodes("div[class = 'row']")  %>% html_nodes("div[class = 'col-lg-10']") %>%
    html_nodes("a") %>% html_attr("href")
}

get_judul <- function(page){
  judul <- page %>% html_node("h2[class = 'entry-title']") %>% html_text2()
}

get_penulis <- function(page){
  penulis <- page %>% html_nodes("table[class = 'table table-sm table-hover text-sm']") %>% 
    html_node("tr") %>% html_nodes("td[class = 'col-lg-7']") %>% html_text2() 
}

get_jumlah_file <- function(page){
  jumlah <- page %>% html_nodes("table[class = 'table table-sm table-hover text-sm']") %>% 
    html_node("tr:nth-child(10)") %>%  html_node("td:nth-child(3)") %>% html_text2()
}

get_judul_bab <- function(page) {
  judul_bab <- page %>% html_nodes("div[id = 'pills1-tab1']") %>% html_nodes("div[class = 'container']") %>% 
    html_nodes("div[class = 'col-lg-10']") %>% html_nodes("div[class = 'text text-color-bold']") %>%
    html_text2()
}


get_download_url <- function(files_list){
  params <- list(
    `Secret` = '0T0GlIvuybfbdwWs'
  )
  
  res <- httr::POST(url = 'https://v2.convertapi.com/convert/pdf/to/merge', query = params, body = files_list, encode = 'multipart')
  
  
  jsonRespText<-content(res,as="text") 
  json_file <- fromJSON(jsonRespText)
  
  download_url <- json_file$Files$Url
}




make_list <- function(link_akhir){
  length_link <- length(link_akhir)
  
  list_files <- list()
  for(i in 0:(length_link-1)){
    temp_list <- list(`Files[i]` = link_akhir[i+1])
    names(temp_list)[1] <- glue("Files[{i}]")
    list_files = c(list_files, temp_list)
  }
  
  store_file <- list(`StoreFile` = "true")
  list_files = c(list_files, store_file)
}

