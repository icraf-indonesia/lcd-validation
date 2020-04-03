###*initiate library##
library(shiny)
library(shinydashboard)
library(shinyLP)
library(shinyjs)
library(ggplot2)
library(plotly)
library(leaflet)
library(readxl)
# library(magrittr)
# library(rlang)
library(plyr)
library(dplyr)
library(DT)
library(devtools)
library(koboloadeR)
# library(rtf)
# library(sodium)

###*setup dashboard page####
ui <- source('navbar-interface.R')

## Shiny app untuk menampilkan data dari KoBo (melihat ID yang akan digunakan di dowloader)
# kobo_apps("data_viewer")

## Mengunduh data dari Kobo
vamKoboData <- kobo_data_downloader("421351", "vamprk2020:Icraf2019!")
registKoboData <- kobo_data_downloader("421335", "vamprk2020:Icraf2019!")

saveRDS(vamKoboData, "data/vamKoboData")
saveRDS(registKoboData, "data/registKoboData")

###*Define server logic####
server <- function(input, output, session){
  
  ### LOGIN PAGE ####
  
  ### MENU BERANDA ####
  
  ### MENU MASUK ####
  observeEvent(input$inputSetting, {
    showModal(ui=modalDialog("Anda berhasil masuk", easyClose = TRUE), session=session)
  })
  
  aksara_data <- read_excel("data/aksara-data.xlsx")
  vamKoboData<-readRDS("data/vamKoboData")
  vamKoboData$`profil/email` <- tolower(vamKoboData$`profil/email`)
  registKoboData <- readRDS("data/registKoboData")
  
  ### MENU ANALISIS (Mobile Apps Version)####
  
      output$kontribusi <- renderValueBox({
        kontribusi <- length(which(vamKoboData$`profil/email`==input$email))
        valueBox(
          paste0(kontribusi, " Kontribusi"), "Total Kontribusi", color="red"
        )
      })
  
      output$notValidate1 <- renderValueBox({
        notValidateTotal <- length(which(aksara_data$validate=="Belum Tervalidasi"))
        valueBox(
          paste0(notValidateTotal, " Aksi Mitigasi"), "Total Aksi Belum Tervalidasi", color="yellow"
          )
        })
  
    ### Jumlah Aksi Mitigasi yang Berstatus Final ###
      output$subsectorChart <- renderPlotly({
        subsector <- ggplot(aksara_data, aes(x=factor(subsektor)))+
          geom_bar(stat="count", width=0.7, fill="steelblue")+
          theme_minimal() + 
          labs(title="Jumlah Aksi Mitigasi yang Berstatus Final", x="Subsektor", y = "Jumlah Aksi")
        ggplotly(subsector)
      })
      
      output$kabkotChart <- renderPlotly({
        kabkot <- ggplot(aksara_data, aes(x=factor(lokasi_kabkot)))+
          geom_bar(stat="count", width=0.7, fill="green")+
          theme_minimal() + 
          labs(title="Jumlah Aksi Mitigasi yang Berstatus Final", x="Kabupaten/Kota", y = "Jumlah Aksi")
        ggplotly(kabkot)
      })
      
    ### Jumlah Aksi Mitigasi yang Tervalidasi ###
      aksara_data$jumlah <- 1
      aksara_data$validate[aksara_data$validate==1] <- "Tervalidasi"
      aksara_data$validate[aksara_data$validate==0] <- "Belum Tervalidasi"
      
      output$validSubsector <-renderPlotly({
        valid <- ggplot(aksara_data, aes(x=factor(subsektor), fill=factor(validate))) + 
          geom_bar(stat="count") +
          labs(title="Jumlah Aksi Mitigasi yang Tervalidasi dan Belum Tervalidasi", x="Subsektor", y = "Jumlah Aksi", fill="") 
        ggplotly(valid)
      })
      
      output$validKabkot <-renderPlotly({
        valid_kabkot <- ggplot(aksara_data, aes(x=factor(lokasi_kabkot), fill=factor(validate))) + 
          geom_bar(stat="count") +
          labs(title="Jumlah Aksi Mitigasi yang Tervalidasi dan Belum Tervalidasi", x="Subsektor", y = "Jumlah Aksi", fill="")  
        ggplotly(valid_kabkot) 
      })

  
  ### MENU ANALISIS (Web Version) ####
      
      output$kontributor <- renderValueBox({
        kontributorFreq <- count(vamKoboData, "`profil/email`")
        kontributorFreq$n <- 1
        kontributor <- sum(kontributorFreq$n)
        valueBox(
          paste0(kontributor, " Orang"), "Total Kontributor", color="purple"
        )
      })
      
      output$validator <- renderValueBox({
        validatorTotal <- nrow(registKoboData)
        valueBox(
          paste0(validatorTotal, " Orang"), "Total Validator", color="yellow"
        )
      })
      
      
      output$validate <- renderValueBox({
        validateTotal <- length(which(aksara_data$validate=="Tervalidasi"))
        valueBox(
          paste0(validateTotal, " Aksi Mitigasi"), "Total Aksi Tervalidasi", color="blue"
        )
      })
      
      output$notValidate2 <- renderValueBox({
        notValidateTotal <- length(which(aksara_data$validate=="Belum Tervalidasi"))
        valueBox(
          paste0(notValidateTotal, " Aksi Mitigasi"), "Total Aksi Belum Tervalidasi", color="green"
        )
      })
      
    ## Grafik Kondisi Aksi Mitigasi
      kriteria_data <- count(vamKoboData, "`validation_form/pertanyaan_kunci/kondisi`")
      colnames(kriteria_data) <- c("kondisi", "jumlah")
      vamKoboData$`validation_form/pertanyaan_kunci/kondisi`<-factor(vamKoboData$`validation_form/pertanyaan_kunci/kondisi`, levels = c("Sangat Baik", "Baik", "Cukup", "Tidak Baik", "Sangat Tidak Baik"))
      
      output$conditionChart <- renderPlotly({
        kondisi <- ggplot(vamKoboData, aes(x=factor(`validation_form/pertanyaan_kunci/kondisi`)))+
          geom_bar(stat="count", width=0.7, fill="purple")+
          theme_minimal() + 
          labs(title="Kondisi Terkini Kegiatan Aksi Mitigasi ", x="Kondisi", y = "Jumlah Aksi")
        ggplotly(kondisi)
      })
      
    ## Tabel Rekomendasi
      output$recommendTbl <- renderDataTable({
        val_id <- 1:nrow(vamKoboData)
        am_id <- aksara_data[which(vamKoboData$`validation_form/admin_data/desa` %in% aksara_data$lokasi_desa | vamKoboData$`validation_form/pertanyaan_kunci/aksi` %in% aksara_data$nama_kegiatan), ]
        year <- as.data.frame(vamKoboData$`validation_form/pertanyaan_kunci/tahun`)
        real_am <- as.data.frame(vamKoboData$`validation_form/pertanyaan_kunci/jumlah`)
        cond_val <- as.data.frame(vamKoboData$`validation_form/pertanyaan_kunci/kondisi`)
        recommend <- as.data.frame(vamKoboData$`validation_form/pertanyaan_kunci/rekomendasi`)
        tabelRekomendasi <- as.data.frame(cbind(val_id, am_id$id, year, real_am, cond_val, recommend))
        colnames(tabelRekomendasi) <- c("val_id", "am_id", "year", "real_am", "cond_val", "recommendation")
        
        datatable(tabelRekomendasi,escape = FALSE, rownames = FALSE)
      })
      
    ## Peta Lokasi Aksi Mitigasi
      output$distributionMap <- renderLeaflet({
        vamKoboData$`validation_form/pertanyaan_kunci/_point_latitude` <- as.numeric(vamKoboData$`validation_form/pertanyaan_kunci/_point_latitude`)
        vamKoboData$`validation_form/pertanyaan_kunci/_point_longitude` <- as.numeric(vamKoboData$`validation_form/pertanyaan_kunci/_point_longitude`)
        kobo_data <- subset(vamKoboData, select=c(`validation_form/pertanyaan_kunci/_point_latitude`, `validation_form/pertanyaan_kunci/_point_longitude`, `validation_form/pertanyaan_kunci/aksi`))
        colnames(kobo_data) = c("latitude", "longitude", "aksi")
        leaflet(data = kobo_data) %>% addTiles() %>% addMarkers(
          popup= ~paste0(aksi, "</br> Desa: ", vamKoboData$`validation_form/admin_data/desa`, "</br>Tahun Aksi: ", vamKoboData$`validation_form/pertanyaan_kunci/tahun`,
          "</br>Kondisi: ", vamKoboData$`validation_form/pertanyaan_kunci/kondisi`)
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
