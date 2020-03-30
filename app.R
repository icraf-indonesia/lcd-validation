###*initiate library##
library(shiny)
library(shinydashboard)
library(shinyLP)
library(shinyjs)
library(ggplot2)
library(plotly)
library(leaflet)
library(readxl)
library(magrittr)
library(rlang)
library(dplyr)
library(plyr)
library(DT)
library(devtools)
library(koboloadeR)
# library(rtf)

###*setup dashboard page####
ui <- source('interface.R')

## Shiny app untuk menampilkan data dari KoBo (melihat ID yang akan digunakan di dowloader)
kobo_apps("data_viewer")

## Mengunduh data dari Kobo
vamKoboData <- kobo_data_downloader("327419", "vamprk2019:Icraf2019!")
registKoboData <- kobo_data_downloader("327419", "vamprk2019:Icraf2019!")

saveRDS(vamKoboData, "data/vamKoboData")
saveRDS(registKoboData, "data/registKoboData")

vamData<-readRDS("data/vamKoboData")
registData <- readRDS("data/registKoboData")

###*Define server logic####
server <- function(input, output, session){
  ### MENU BERANDA ####
  
  ### MENU PETA ####
  
  ### MENU KIRIM ####
  
  ### MENU TABEL ####
  
  ### MENU TENTANG ####
}


###*run the apps#### 
shinyApp(ui = ui, server = server)