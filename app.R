###*initiate library##
library(shiny)
library(shinydashboard)
library(shinyLP)
library(shinyjs)
library(ggplot2)
library(plotly)
# library(leaflet)
library(readxl)
# library(magrittr)
# library(rlang)
# library(plyr)
# library(dplyr)
library(DT)
library(devtools)
library(koboloadeR)
# library(rtf)

###*setup dashboard page####
ui <- source('navbar-interface.R')

## Shiny app untuk menampilkan data dari KoBo (melihat ID yang akan digunakan di dowloader)
# kobo_apps("data_viewer")

## Mengunduh data dari Kobo
vamKoboData <- kobo_data_downloader("421351", "vamprk2020:Icraf2019!")
registKoboData <- kobo_data_downloader("421335", "vamprk2020:Icraf2019!")

saveRDS(vamKoboData, "data/vamKoboData")
saveRDS(registKoboData, "data/registKoboData")

vamKoboData<-readRDS("data/vamKoboData")
registKoboData <- readRDS("data/registKoboData")

###*Define server logic####
server <- function(input, output, session){
  ### MENU BERANDA ####
  
  ### MENU MASUK ####
  observeEvent(input$inputSetting, {
    showModal(ui=modalDialog("Anda berhasil masuk", easyClose = TRUE), session=session)
  })
  
  ### MENU ANALISIS ####
  output$kontribusi <- renderValueBox({
    actionTotal <- nrow(vamKoboData)
    valueBox(
      paste0(actionTotal, " Aksi Mitigasi"), "Total Aksi Mitigasi", color="red"
    )
  })
  output$validator <- renderValueBox({
    validatorTotal <- nrow(registKoboData)
    valueBox(
      paste0(validatorTotal, " Orang"), "Total Validator", color="yellow"
    )
  })
  
  ### MENU TENTANG ####
  
  
  ### Apps Menu (tidak dipakai karena sudah menggunakan Kobo) ####
      ### MENU PETA ###
      # output$distributionMap <- renderLeaflet({
      #   long_lat_data<-read_excel("data/file.xlsx")
      #   long_lat_data$`_respgeopoint_latitude` <- as.numeric(long_lat_data$`_respgeopoint_latitude`)
      #   long_lat_data$`_respgeopoint_longitude` <- as.numeric(long_lat_data$`_respgeopoint_longitude`)
      #   kobo_data <- subset(long_lat_data, select=c(`_respgeopoint_latitude`, `_respgeopoint_longitude`, aksi_mitigasi))
      #   colnames(kobo_data) = c("lat", "long", "aksi")
      #   leaflet(data = kobo_data) %>% addTiles() %>% addMarkers(
      #     clusterOptions = markerClusterOptions()
      #   )
      # 
      # })
      ### MENU KIRIM ###
      ### MENU TABEL ###
      # output$tableAksi <- renderDataTable({
      #   datatable(cars)
      # })
      # 
  
}


###*run the apps#### 
shinyApp(ui = ui, server = server)
