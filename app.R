###*initiate library##
library(shiny)
library(shinydashboard)
library(shinyLP)
library(shinyjs)
library(ggplot2)
library(plotly)
library(leaflet)
library(readxl)
library(plyr)
library(dplyr)
library(DT)
library(devtools)
# source("http://news.mrdwab.com/install_github.R")
# install_github("mrdwab/koboloadeR")
# library(koboloadeR)
# kobo_apps("data_viewer")
# library(rtf)
library(sodium)
library(stringr)

loginpage <- div(titlePanel ("SiVaTif-PRKI"), id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("Log In", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Email", label = tagList(icon("user"), "Email")),
                   passwordInput("passwd", placeholder="Kata Sandi", label = tagList(icon("unlock-alt"), "Kata Sandi")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "MASUK", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Email atau kata sandi Anda salah",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     tags$code("Kata Sandi: password")
                   )
                 ))

###*Download data from Kobo##
library(httr)
library(jsonlite)
library(readr)
kobo_server_url <- "https://kf.kobotoolbox.org/"
kc_server_url <- "https://kc.kobotoolbox.org/"

form_reg <- 421335 #Registrasi
form_app <- 623420 #App

header <- dashboardHeader(title = NULL, titleWidth = NULL ,uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(disable=TRUE)
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "black")

server <- function(input, output, session) {
  ### Registrasi ###
  url_reg <- paste0(kc_server_url,"api/v1/data/",form_reg,"?format=csv")
  rawdata_reg  <- GET(url_reg,authenticate("vamprk2020","Icraf2019!"),progress())
  registKoboData  <- read_csv(content(rawdata_reg,"raw",encoding = "UTF-8"))
  
  ### App ###
  url_app <- paste0(kc_server_url,"api/v1/data/",form_app,"?format=csv")
  rawdata_app  <- GET(url_app,authenticate("vamprk2020","Icraf2019!"),progress())
  vamKoboData  <- read_csv(content(rawdata_app,"raw",encoding = "UTF-8"))
  vamKoboData$`profil/email` <- tolower(vamKoboData$`profil/email`)
  
  koboData <- reactiveValues(regist = registKoboData, vam = vamKoboData)
  aksara_data <- read_excel("data/aksara-data.xlsx")
  
  ### Login Page ####
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    reg <- koboData$regist 
    credentials <- data.frame(
      username_id = reg$`profil/email`,
      passod   = sapply(c(rep("password", nrow(reg))),password_store),
      # permission  = c("basic", "advanced"),
      stringsAsFactors = F
    )
    
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Keluar", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: regular; margin:5px; padding: 10px;")
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      navbarPage("SiVaTif-PRKI", position = "static-top", collapsible = TRUE,
                 tabPanel("Beranda", icon = icon("home"),
                          jumbotron(img(src="lp.png", height="100%", width="100%"), " ", button = FALSE,
                                    tags$style("background-size: cover;"))
                 ),
                 # tabPanel("Pengguna",
                 #          tabsetPanel(
                 #            tabPanel("Lokasi Aksi Mitigasi"),
                 #            tabPanel("Dashboard")
                 #          )),
                 navbarMenu("Pengguna", icon = icon("user", lib = "font-awesome"),
                            tabPanel("Lokasi Aksi Mitigasi",
                                     h2("Pilih Lokasi Aksi Mitigasi yang Anda Tuju"),
                                     leafletOutput("actionMaps"),
                                     h3("Catatan: Jangan lupa untuk mencatat nomor ID Aksi Mitigasi Anda!"),
                                     br(),
                                     dataTableOutput("gmapstable")),
                            tabPanel("Dashboard",
                                     fluidRow(
                                       valueBoxOutput(width=6, "kontribusi"),
                                       valueBoxOutput(width=6, "notValidate1")),
                                     selectInput("finalGraph_user", "Tampilkan Grafik Berdasarkan", c("Subsektor", "Administrasi (Kabupaten/Kota)")),
                                     conditionalPanel(
                                       condition="input.finalGraph_user=='Subsektor'",
                                       plotlyOutput("subsectorChart")
                                     ),
                                     conditionalPanel(
                                       condition="input.finalGraph_user=='Administrasi (Kabupaten/Kota)'",
                                       plotlyOutput("kabkotChart")
                                     ),
                                     # plotlyOutput("subsectorChart"),
                                     br(),
                                     selectInput("validGraph_user", "Tampilkan Grafik Berdasarkan", c("Subsektor", "Administrasi (Kabupaten/Kota)")),
                                     conditionalPanel(
                                       condition="input.validGraph_user=='Subsektor'",
                                       plotlyOutput("validSubsector")
                                     ),
                                     conditionalPanel(
                                       condition="input.validGraph_user=='Administrasi (Kabupaten/Kota)'",
                                       plotlyOutput("validKabkot")
                                     ),
                                     br(),
                                     plotlyOutput("QCgraph"))),
                 tabPanel("Aksara", icon = icon("chart-bar", lib = "font-awesome"),
                          fluidRow(
                            valueBoxOutput(width=6, "kontributor"),
                            valueBoxOutput(width=6, "validator"),
                            valueBoxOutput(width=6, "validate"),
                            valueBoxOutput(width=6, "notValidate2")
                          ),
                          br(),
                          leafletOutput("distributionMap"),
                          br(),
                          # dataTableOutput("recommendTbl")
                          h2("Hasil Validasi Data AKSARA"),
                          plotlyOutput("conditionChart"),
                          br(),
                          dataTableOutput("tabelvalidasi"),
                          br(),
                          h2("Kontrol Kualitas Data SiVaTif"),
                          plotlyOutput("QCgraph2"),
                          dataTableOutput("tabelQC"),
                 ),
                 tabPanel("Cara Kontribusi", icon = icon("info-circle", lib = "font-awesome"),
                          jumbotron(img(src="kontribusi.png", height="100%", width="100%"), " ", button = FALSE)
                 )
      )
    }
    else {
      loginpage
    }
  })
  
  tables <- reactiveValues(dataQC=data.frame(), dataValid=data.frame())
  
  ### MENU ANALISIS (Mobile Apps Version)####
  
  output$kontribusi <- renderValueBox({
    kontribusi <- length(which(koboData$vam$`profil/email`==input$userName))
    valueBox(
      paste0(kontribusi, " Kontribusi"), "Total Kontribusi", color="green"
    )
  })
  
  output$notValidate1 <- renderValueBox({
    # notValidateTotal <- length(which(aksara_data$validate=="Belum Tervalidasi"))
    # valueBox(
    #   paste0(notValidateTotal, " Aksi Mitigasi"), "Total Aksi Belum Tervalidasi", color="yellow"
    # )
    data_aksara <- read_excel("data/aksara_table.xlsx")
    kontribusi <- length(which(koboData$vam$`profil/email`==input$userName))
    
    d <- tables$dataValid
    totalValidasi <- length(which(d$penilaian_validasi=="TERVALIDASI"))
    
    if (nrow(data_aksara) - kontribusi<0){
      notValidateTotal <- -1*(nrow(data_aksara) - kontribusi)
    }else {
      notValidateTotal <- nrow(data_aksara) - kontribusi
    }
    
    valueBox(
      paste0(notValidateTotal, " Aksi Mitigasi"), "Total Aksi Belum Anda Validasi", color="red"
    )
  })
  
  ### Jumlah Aksi Mitigasi yang Berstatus Final ###
  output$subsectorChart <- renderPlotly({
    subsector <- ggplot(aksara_data, aes(x=factor(subsektor)))+
      geom_bar(stat="count", width=0.7, fill="steelblue")+
      theme_minimal() + 
      labs(title="Jumlah Aksi Mitigasi yang Berstatus Final", x="Subsektor", y = "Jumlah Aksi") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 7),
            legend.text = element_text(size = 8))
    ggplotly(subsector)
  })
  
  output$kabkotChart <- renderPlotly({
    kabkot <- ggplot(aksara_data, aes(x=factor(lokasi_kabkot)))+
      geom_bar(stat="count", width=0.7, fill="green")+
      theme_minimal() + 
      labs(title="Jumlah Aksi Mitigasi yang Berstatus Final", x="Kabupaten/Kota", y = "Jumlah Aksi") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 8, hjust = 1, angle = 90, face = "plain"),
            legend.text = element_text(size = 8))
    ggplotly(kabkot)
  })
  
  ### Jumlah Aksi Mitigasi yang Tervalidasi ###
  aksara_data$jumlah <- 1
  aksara_data$validate[aksara_data$validate==1] <- "Tervalidasi"
  aksara_data$validate[aksara_data$validate==0] <- "Belum Tervalidasi"
  
  output$validSubsector <-renderPlotly({
    valid <- ggplot(aksara_data, aes(x=factor(subsektor), fill=factor(validate))) + 
      geom_bar(stat="count") +
      labs(title="Jumlah Aksi Mitigasi yang Tervalidasi dan Belum Tervalidasi", x="Subsektor", y = "Jumlah Aksi", fill="") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 7),
            legend.text = element_text(size = 8))
    ggplotly(valid) %>% layout(legend = list(orientation = "h", x = 0.1, y = -0.3))
  })
  
  output$validKabkot <-renderPlotly({
    valid_kabkot <- ggplot(aksara_data, aes(x=factor(lokasi_kabkot), fill=factor(validate))) + 
      geom_bar(stat="count") +
      labs(title="Jumlah Aksi Mitigasi yang Tervalidasi dan Belum Tervalidasi", x="Kabupaten/Kota", y = "Jumlah Aksi", fill="") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 8, hjust = 1, angle = 90, face = "plain"),
            legend.text = element_text(size = 8))
    ggplotly(valid_kabkot) #%>% layout(legend = list(orientation = "h", x = 0.1, y = 0.3))
  })
  
  ## Peta Lokasi Aksi Mitigasi yang ada
  output$actionMaps <- renderLeaflet({
    # NOTE #1: need to connect to AKSARA db ####
    data_dummy <- read_excel("data/data_lokasi_rev.xlsx")
    
    # data_aksara <- read_excel("data/aksara_table.xlsx")
    
    # data_jabar <- read_excel("data/dummy_jabar.xlsx")
    # final_jabar <- subset(data_jabar, select=c(id, nama_kegiatan, tahun_pelaporan, nama_provinsi, lat, long))
    
    ### Data Dummy AKSARA ###
    # main_data <- subset(data_aksara, select=c(id, nama_kegiatan, tahun_pelaporan, nama_provinsi))
    # temp_data <- data_dummy[!is.na(data_dummy$lat), ]
    # data_provinsi <- cbind(main_data, temp_data$lat[1:6], temp_data$long[1:6])
    # colnames(data_provinsi)[colnames(data_provinsi) == 'temp_data$lat[1:6]'] <- 'lat'
    # colnames(data_provinsi)[colnames(data_provinsi) == 'temp_data$long[1:6]'] <- 'long'
    
    final_provinsi <- as.data.frame(data_dummy)
    # final_provinsi <- rbind(data_provinsi, final_jabar)
    
    final_provinsi$lat <- as.numeric(final_provinsi$lat)
    final_provinsi$long <- as.numeric(final_provinsi$long)
    
    main_data <- subset(final_provinsi, select=c(id, lat, long, nama_provinsi, nama_kegiatan, tahun_pelaporan))
    # main_data <- na.omit(main_data)
    
    gmaps <- paste0("https://www.google.com/maps/search/?api=1&query=", main_data$lat,",", main_data$long)
    gmaps <- as.data.frame(gmaps)
    
    final_data <- cbind(main_data, gmaps)
    leaflet(data= final_data) %>% addTiles() %>% addMarkers(lng=~long,lat=~lat, popup=~paste0("ID Aksi Mitigasi: ", id ,"</br>Nama Aksi: " , nama_kegiatan, "</br>Provinsi: ", 
                                                                                              nama_provinsi, "</br>Lokasi Titik: " ,"<a href='", gmaps, "' target='_blank' >klik disini</a>"))
  })
  
  ### Grafik Kontrol Kualitas Data SiVatif ####
  output$QCgraph <- renderPlotly({
    # saveRDS(vamKoboData,"data/vamKoboData")
    # validation_table <- readRDS("data/vamKoboData")
    validation_table <- koboData$vam
    
    admin_id <- unique(validation_table$`admin_data/id_aksi`)
    validation_table$`pertanyaan_kunci/detail_aksi/q1` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q1`, "1", "Iya")
    validation_table$`pertanyaan_kunci/detail_aksi/q1` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q1`, "2", "Tidak")
    validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "1", "Iya")
    validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "2", "Tidak")
    validation_table$`pertanyaan_kunci/detail_aksi/q2`[is.na(validation_table$`pertanyaan_kunci/detail_aksi/q2`)] <- 0
    validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "0" , "Tidak")
    validation_table$`pertanyaan_kunci/detail_aksi/recom`[is.na(validation_table$`pertanyaan_kunci/detail_aksi/recom`)] <- "tidak tahu"
    
    c=NULL
    for (i in 1:length(admin_id)) {
      data <- filter(validation_table, `admin_data/id_aksi`==admin_id[i])
      
      kontributor <- length(which(validation_table$`admin_data/id_aksi`==admin_id[i]))
      
      table_q1 <- table(data$`pertanyaan_kunci/detail_aksi/q1`)
      q1 <- names(table_q1[table_q1==max(table_q1)])
      if(length(q1)==2){
        q1_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q1`=="Tidak"))
        q1 <- "Tidak"
      } else {
        q1_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q1`==q1))  
      }
      # q1_maj <- nrow(data[data$q1 == q1,])
      q1_perc <- q1_maj/nrow(data) * 100
      
      table_q1.1 <- table(data$`pertanyaan_kunci/detail_aksi/q1.1`)
      q1.1 <- names(table_q1.1[table_q1.1==max(table_q1.1)]) 
      q1.1 <- ifelse(is.null(q1.1), "Tidak tahu", q1.1)
      
      table_q1.2 <- table(data$`pertanyaan_kunci/detail_aksi/q1.2`)
      q1.2 <- names(table_q1.2[table_q1.2==max(table_q1.2)]) 
      q1.2 <- ifelse(is.null(q1.2), "0", q1.2)
      
      table_q2 <- table(data$`pertanyaan_kunci/detail_aksi/q2`)
      q2 <- names(table_q2[table_q2==max(table_q2)])
      if(length(q2)==2){
        q2_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q2`=="Tidak"))
        q2 <- "Tidak"
      } else {
        q2_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q2`==q2))  
      }
      q2_perc <- q2_maj/nrow(data) * 100
      
      if (q1=="Tidak" | q2=="Tidak"){
        q2.1 <- 0
        q2.2 <- "Tidak tahu"
        q2.3 <- 0
        q2.4 <- 0
      } else {
        table_q2.1 <- table(data$`pertanyaan_kunci/detail_aksi/q2.1`)
        q2.1 <- names(table_q2.1[table_q2.1==max(table_q2.1)])
        
        table_q2.2 <- table(data$`pertanyaan_kunci/detail_aksi/q2.2`)
        q2.2 <- names(table_q2.2[table_q2.2==max(table_q2.2)])
        
        table_q2.3 <- table(data$`pertanyaan_kunci/detail_aksi/q2.3`)
        q2.3 <- names(table_q2.3[table_q2.3==max(table_q2.3)])
        
        table_q2.4 <- table(data$`pertanyaan_kunci/detail_aksi/q2.4`)
        q2.4 <- names(table_q2.4[table_q2.4==max(table_q2.4)])
      }
      
      unique_q3 <- tolower(data$`pertanyaan_kunci/detail_aksi/recom`)
      q3 <- str_c(unique_q3, collapse = "; ")
      
      fin_valass <- "Belum ada penilaian"
      fin_valass <- ifelse(q1_perc>=80 , "Tinggi", fin_valass)
      fin_valass <- ifelse(q1_perc<80 & q1_perc>=60 , "Sedang", fin_valass)
      fin_valass <- ifelse(q1_perc<60 , "Rendah", fin_valass)
      
      am_id <- unique(admin_id[i])
      
      test <- cbind(am_id, kontributor, q1, q1_maj, q1_perc, q1.1, q1.2, q2, q2_maj, q2_perc, q2.1, q2.2, q2.3, q2.4, q3, fin_valass)
      test <- as.data.frame(test)
      
      c=rbind(c, test)
    }
    
    hasil <- as.data.frame(c)
    
    grafikQC <- ggplot(hasil, aes(x=fin_valass))+
      geom_bar(stat="count", width=0.7, fill="purple")+
      theme_minimal() + 
      labs(title="Kontrol Kualitas Data SivaTif", x="Kualitas", y = "Jumlah Aksi")
    ggplotly(grafikQC) 
  })
  
  ### Tabel Link Google Maps ####
  output$gmapstable <- renderDataTable({
    ###Kodingan Awal ####
    # data_dummy <- read_excel("data/data_dummy_lahan.xlsx")
    # data_dummy$lat <- as.numeric(data_dummy$lat)
    # data_dummy$long <- as.numeric(data_dummy$long)
    # 
    # data_aksara <- read_excel("data/aksara_table.xlsx")
    # 
    # 
    # # data_jabar <- read_excel("data/dummy_jabar.xlsx")
    # # final_jabar <- subset(data_jabar, select=c(id, nama_kegiatan, tahun_pelaporan, nama_provinsi, lat, long))
    # # main_data <- subset(data_dummy, select=c(id, lat, long, nama_kegiatan, tahun_pelaporan, nama_provinsi))
    # # data_provinsi <- filter(main_data, main_data$nama_provinsi=="SUMATERA SELATAN")
    # # data_provinsi<-data_provinsi[!is.na(data_provinsi$lat), ]
    # 
    # ### Data Dummy AKSARA ###
    # main_data <- subset(data_aksara, select=c(id, nama_kegiatan, tahun_pelaporan, nama_provinsi))
    # # data_provinsi <- filter(main_data, main_data$nama_provinsi=="SUMATERA SELATAN")
    # temp_data <- data_dummy[!is.na(data_dummy$lat), ]
    # data_provinsi <- cbind(main_data, temp_data$lat[1:6], temp_data$long[1:6])
    # colnames(data_provinsi)[colnames(data_provinsi) == 'temp_data$lat[1:6]'] <- 'lat'
    # colnames(data_provinsi)[colnames(data_provinsi) == 'temp_data$long[1:6]'] <- 'long'
    
    # NOTE #2: need to connect to AKSARA db ####
    data_aksara <- read_excel("data/data_lokasi_rev.xlsx")
    main_data <- subset(data_aksara, select=c(id, lat, long, nama_kegiatan, tahun_pelaporan, nama_provinsi))
    final_provinsi <- as.data.frame(main_data)

    
    # final_provinsi <- rbind(data_provinsi, final_jabar)
    final_provinsi$kabkot <- "Tidak ada data"
    final_provinsi$kecamatan <- "Tidak ada data"
    final_provinsi$desa <- "Tidak ada data"
    
    gmaps <- paste0("https://www.google.com/maps/search/?api=1&query=", final_provinsi$lat[1:nrow(final_provinsi)],",", final_provinsi$long[1:nrow(final_provinsi)])
    # gmaps <- paste0("https://www.google.com/maps/search/?api=1&query=", data_provinsi$lat,",", data_provinsi$long)
    
    final_provinsi$gmaps <- paste0("<a href='", gmaps, "' target='_blank'> Klik disini </a>")
    final_provinsi$lat <- NULL
    final_provinsi$long <- NULL
    
    colnamesLokasi <- read_excel("data/colnames_sivatif.xlsx", sheet = "lokasi")
    colnames(final_provinsi) <- colnamesLokasi$`Nama kolom`
    
    # datatable(data_provinsi,  escape = FALSE) 

    headerCallback <- c(
      "function(thead, data, start, end, display){",
      "  var tooltips = ['Nomor identifikasi unik aksi mitigasi di dalam AKSARA','Nama kegiatan aksi mitigasi yang dilaporkan di dalam AKSARA',
                                          'Tahun dilaksanakannya aksi mitigasi','Nama Provinsi','Nama Kabupaten/Kota','Nama Kecamatan','Nama Desa','Google Maps'];",
      "  for(var i=0; i<tooltips.length; i++){",
      "    $('th:eq('+i+')',thead).attr('title', tooltips[i]);",
      "  }",
      "}"
    )
    
    datatable(final_provinsi, rownames = FALSE, escape = FALSE,
              options = list(
                headerCallback = JS(headerCallback), scrollX = TRUE
              )
    )
  })
  
  ### MENU ANALISIS (Web Version) ####
  
  output$kontributor <- renderValueBox({
    vamKoboData<-koboData$vam
    vamKoboData$`profil/email` <- tolower(vamKoboData$`profil/email`)
    kontributorFreq <- unique(vamKoboData$`profil/email`)
    kontributor <- length(kontributorFreq)
    valueBox(
      paste0(kontributor, " Orang"), "Total Kontributor", color="purple"
    )
  })
  
  output$validator <- renderValueBox({
    validatorTotal <- nrow(koboData$regist)
    valueBox(
      paste0(validatorTotal, " Orang"), "Total Validator", color="aqua"
    )
  })
  
  
  output$validate <- renderValueBox({
    d <- tables$dataValid
    validateTotal <- length(which(d$penilaian_validasi=="TERVALIDASI"))
    valueBox(
      paste0(validateTotal, " Aksi Mitigasi"), "Total Aksi Tervalidasi", color="green"
    )
  })
  
  output$notValidate2 <- renderValueBox({
    d <- tables$dataValid
    notValidateTotal <- length(which(d$penilaian_validasi=="PERLU DIREVISI"))
    valueBox(
      paste0(notValidateTotal, " Aksi Mitigasi"), "Total Aksi Perlu Direvisi", color="maroon"
    )
  })
  
  
  output$conditionChart <- renderPlotly({
    validation_table <- koboData$vam
    admin_id <- unique(validation_table$`admin_data/id_aksi`)
    validation_table$`pertanyaan_kunci/detail_aksi/q1` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q1`, "1", "Iya")
    validation_table$`pertanyaan_kunci/detail_aksi/q1` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q1`, "2", "Tidak")
    validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "1", "Iya")
    validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "2", "Tidak")
    validation_table$`pertanyaan_kunci/detail_aksi/q2`[is.na(validation_table$`pertanyaan_kunci/detail_aksi/q2`)] <- 0
    validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "0" , "Tidak")
    validation_table$`pertanyaan_kunci/detail_aksi/recom`[is.na(validation_table$`pertanyaan_kunci/detail_aksi/recom`)] <- "tidak tahu"
    # validation_table$`pertanyaan_kunci/detail_aksi/q1.1`[is.na(validation_table$`pertanyaan_kunci/detail_aksi/q1.1`)] <- "Tidak ada"
    # # validation_table$`pertanyaan_kunci/detail_aksi/q1.1` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q1.1`, "+" , "Tidak ada")
    # validation_table$`pertanyaan_kunci/detail_aksi/q1.2`[is.na(validation_table$`pertanyaan_kunci/detail_aksi/q1.2`)] <- "Tidak ada"
    # # validation_table$`pertanyaan_kunci/detail_aksi/q1.2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q1.2`, "+" , "Tidak ada")
    # validation_table$`pertanyaan_kunci/detail_aksi/q2.1`[is.na(validation_table$`pertanyaan_kunci/detail_aksi/q2.1`)] <- "Tidak ada"
    # # validation_table$`pertanyaan_kunci/detail_aksi/q2.1` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2.1`, "+" , "Tidak ada")
    # validation_table$`pertanyaan_kunci/detail_aksi/q2.2`[is.na(validation_table$`pertanyaan_kunci/detail_aksi/q2.2`)] <- "Tidak ada"
    # # validation_table$`pertanyaan_kunci/detail_aksi/q2.2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2.2`, "+" , "Tidak ada")
    # validation_table$`pertanyaan_kunci/detail_aksi/q2.3`[is.na(validation_table$`pertanyaan_kunci/detail_aksi/q2.3`)] <- "Tidak ada"
    # # validation_table$`pertanyaan_kunci/detail_aksi/q2.3` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2.3`, "+" , "Tidak ada")
    # validation_table$`pertanyaan_kunci/detail_aksi/q2.4`[is.na(validation_table$`pertanyaan_kunci/detail_aksi/q2.4`)] <- "Tidak ada"
    # # validation_table$`pertanyaan_kunci/detail_aksi/q2.4` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2.4`, "+" , "Tidak ada")
    
    c=NULL
    for (i in 1:length(admin_id)) {
      data <- filter(validation_table, `admin_data/id_aksi`==admin_id[i])
      
      kontributor <- length(which(validation_table$`admin_data/id_aksi`==admin_id[i]))
      
      table_q1 <- table(data$`pertanyaan_kunci/detail_aksi/q1`)
      q1 <- names(table_q1[table_q1==max(table_q1)])
      if(length(q1)==2){
        q1_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q1`=="Tidak"))
        q1 <- "Tidak"
      } else {
        q1_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q1`==q1))  
      }
      # q1_maj <- nrow(data[data$q1 == q1,])
      q1_perc <- q1_maj/nrow(data) * 100
      
      table_q1.1 <- table(data$`pertanyaan_kunci/detail_aksi/q1.1`)
      q1.1 <- names(table_q1.1[table_q1.1==max(table_q1.1)])
      q1.1 <- ifelse(is.null(q1.1), "Tidak tahu", q1.1)
      
      table_q1.2 <- table(data$`pertanyaan_kunci/detail_aksi/q1.2`)
      q1.2 <- names(table_q1.2[table_q1.2==max(table_q1.2)])
      q1.2 <- ifelse(is.null(q1.2), "0", q1.2)
      
      table_q2 <- table(data$`pertanyaan_kunci/detail_aksi/q2`)
      q2 <- names(table_q2[table_q2==max(table_q2)])
      if(length(q2)==2){
        q2_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q2`=="Tidak"))
        q2 <- "Tidak"
      } else {
        q2_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q2`==q2))  
      }
      q2_perc <- q2_maj/nrow(data) * 100

      if (q1=="Tidak" | q2=="Tidak"){
        q2.1 <- 0
        q2.2 <- "Tidak tahu"
        q2.3 <- 0
        q2.4 <- 0
      } else {
        table_q2.1 <- table(data$`pertanyaan_kunci/detail_aksi/q2.1`)
        q2.1 <- names(table_q2.1[table_q2.1==max(table_q2.1)])
        
        table_q2.2 <- table(data$`pertanyaan_kunci/detail_aksi/q2.2`)
        q2.2 <- names(table_q2.2[table_q2.2==max(table_q2.2)])
        
        table_q2.3 <- table(data$`pertanyaan_kunci/detail_aksi/q2.3`)
        q2.3 <- names(table_q2.3[table_q2.3==max(table_q2.3)])
        
        table_q2.4 <- table(data$`pertanyaan_kunci/detail_aksi/q2.4`)
        q2.4 <- names(table_q2.4[table_q2.4==max(table_q2.4)])
      }
      
      unique_q3 <- tolower(data$`pertanyaan_kunci/detail_aksi/recom`)
      q3 <- str_c(unique_q3, collapse = "; ")
      
      fin_valass <- "Belum ada penilaian"
      fin_valass <- ifelse(q1_perc>=80 , "Tinggi", fin_valass)
      fin_valass <- ifelse(q1_perc<80 & q1_perc>=60 , "Sedang", fin_valass)
      fin_valass <- ifelse(q1_perc<60 , "Rendah", fin_valass)
      
      am_id <- unique(admin_id[i])
      
      test <- cbind(am_id, kontributor, q1, q1_maj, q1_perc, q1.1, q1.2, q2, q2_maj, q2_perc, q2.1, q2.2, q2.3, q2.4, q3, fin_valass)
      test <- as.data.frame(test)
      
      c=rbind(c, test)
    }
    
    hasil <- as.data.frame(c)
    
    ### TABEL AKSARA ####
    data_aksara <- read_excel("data/aksara_table.xlsx")
    
    ### TABEL HASIL VALIDASI ####
    validation_table <- koboData$vam
    admin_id <- unique(validation_table$`admin_data/id_aksi`)
    
    d=NULL
    for (i in 1:length(admin_id)) {
      tabel_aksara <- filter(data_aksara, id==admin_id[i])
      tbl <- filter(hasil, am_id==admin_id[i])
      kontributor <- length(which(validation_table$`admin_data/id_aksi`==admin_id[i]))
      
      aksara_nama <- tabel_aksara$nama_kegiatan
      sivatif_nama <- as.character(tbl$q1.1)
      kesesuaian_nama <- "Tidak Sesuai"
      kesesuaian_nama <- ifelse(sivatif_nama==aksara_nama, "Sesuai", kesesuaian_nama)
      
      aksara_tahun <- tabel_aksara$tahun_pelaporan
      sivatif_tahun <- as.numeric(as.character(tbl$q1.2))
      kesesuaian_tahun <- "Tidak Sesuai"
      kesesuaian_tahun <- ifelse(sivatif_tahun==aksara_tahun, "Sesuai", kesesuaian_tahun)
      
      aksara_realisasi <- tabel_aksara$realisasi
      sivatif_realisasi <- as.numeric(as.character(tbl$q2.1))
      kesesuaian_realisasi <- "Tidak Sesuai"
      kesesuaian_realisasi <- ifelse(sivatif_realisasi==aksara_realisasi, "Sesuai", kesesuaian_realisasi)
      
      aksara_jenis <- tabel_aksara$jenis_pohon_lain
      sivatif_jenis <- as.character(tbl$q2.2)
      kesesuaian_jenis <- "Tidak Sesuai"
      kesesuaian_jenis <- ifelse(sivatif_jenis==aksara_jenis, "Sesuai", kesesuaian_jenis)
      
      aksara_jumlah <- tabel_aksara$jumlah_pohon_tanaman_yang_masih_hidup
      sivatif_jumlah <- as.numeric(as.character(tbl$q2.4))
      persen_selisih <- (sivatif_jumlah / aksara_jumlah)
      kondisi <- "Sangat Tidak Baik"
      kondisi <- ifelse(persen_selisih>=0.8 , "Sangat Baik", kondisi)
      kondisi <- ifelse(persen_selisih<0.8 & persen_selisih>=0.6 , "Baik", kondisi)
      kondisi <- ifelse(persen_selisih<0.6 & persen_selisih>=0.4 , "Kurang Baik", kondisi)
      kondisi <- ifelse(persen_selisih<0.4 & persen_selisih>=0.2 , "Tidak Baik", kondisi)
      penilaian_validasi <- "BELUM DIVALIDASI"
      # penilaian_validasi <- ifelse(kontributor < 5, "BELUM DIVALIDASI", penilaian_validasi)
      penilaian_validasi <- ifelse(kontributor >= 5 & hasil$fin_valass=="Tinggi" & (kesesuaian_nama=="Tidak Sesuai" | kesesuaian_tahun=="Tidak Sesuai" | kesesuaian_realisasi=="Tidak Sesuai" | kesesuaian_jenis=="Tidak Sesuai" | persen_selisih < 0.6), "PERLU DIREVISI", penilaian_validasi)
      penilaian_validasi <- ifelse(kontributor >= 5 & hasil$fin_valass=="Tinggi" & kesesuaian_nama=="Sesuai" & kesesuaian_tahun=="Sesuai" & kesesuaian_realisasi=="Sesuai" & kesesuaian_jenis=="Sesuai" & persen_selisih>=0.6, "TERVALIDASI", penilaian_validasi)
      
      am_id <- unique(admin_id[i])
      
      final_tabel <-as.data.frame(cbind(am_id, aksara_nama, sivatif_nama, kesesuaian_nama, aksara_tahun, sivatif_tahun, kesesuaian_tahun, aksara_realisasi,
                                        sivatif_realisasi, kesesuaian_realisasi, aksara_jenis, sivatif_jenis, kesesuaian_jenis, aksara_jumlah, sivatif_jumlah,
                                        persen_selisih, kondisi, penilaian_validasi))
      d=rbind(d, final_tabel)
    }
    
    d <- unique(d)
    d <- data.frame(d)
    rownames(d) <- NULL
    
    kriteria_data <- count(d, "kondisi")
    colnames(kriteria_data) <- c("kondisi", "jumlah")
    d$kondisi <- factor(d$kondisi, levels = c("Sangat Baik", "Baik", "Kurang Baik", "Tidak Baik", "Sangat Tidak Baik"))
    
    kondisi <- ggplot(d, aes(x=factor(kondisi)))+
      geom_bar(stat="count", width=0.7, fill="purple")+
      theme_minimal() + 
      labs(title="Kondisi Terkini Kegiatan Aksi Mitigasi ", x="Kondisi", y = "Jumlah Aksi")
    ggplotly(kondisi) 
  })
  
  ## Peta Lokasi Aksi Mitigasi
  output$distributionMap <- renderLeaflet({
    koboData$vam$`_geolocation.0` <- as.numeric(koboData$vam$`_geolocation.0`)
    koboData$vam$`_geolocation.1` <- as.numeric(koboData$vam$`_geolocation.1`)
    # vamKoboData$aksi <- vamKoboData$`pertanyaan_kunci/detail_aksi/q1.1`
    # vamKoboData$aksi <- str_replace_all(aksi,"__", "_")
    # vamKoboData$aksi <- str_replace_all(aksi,"_", " ")
    kobo_data <- subset(koboData$vam, select=c(`admin_data/id_aksi`, `_geolocation.0`, `_geolocation.1`, `pertanyaan_kunci/detail_aksi/q1.1`))
    colnames(kobo_data) = c("id","latitude", "longitude", "aksi")
    leaflet(data = kobo_data) %>% addTiles() %>% addMarkers(
      popup= ~paste0("</br>ID Aksi Mitigasi: ", id,"</br>Nama Aksi Mitigasi: ", aksi, "</br>Provinsi: ", koboData$vam$`admin_data/provinces`, "</br>Tahun Aksi: ", koboData$vam$`pertanyaan_kunci/detail_aksi/q1.2`)
    )
  })
  
  ### Grafik Kontrol Kualitas Data SiVatif ####
  output$QCgraph2 <- renderPlotly({
    validation_table <- koboData$vam
    admin_id <- unique(validation_table$`admin_data/id_aksi`)
    validation_table$`pertanyaan_kunci/detail_aksi/q1` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q1`, "1", "Iya")
    validation_table$`pertanyaan_kunci/detail_aksi/q1` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q1`, "2", "Tidak")
    validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "1", "Iya")
    validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "2", "Tidak")
    validation_table$`pertanyaan_kunci/detail_aksi/q2`[is.na(validation_table$`pertanyaan_kunci/detail_aksi/q2`)] <- 0
    validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "0" , "Tidak")
    validation_table$`pertanyaan_kunci/detail_aksi/recom`[is.na(validation_table$`pertanyaan_kunci/detail_aksi/recom`)] <- "tidak tahu"
    
    c=NULL
    for (i in 1:length(admin_id)) {
      data <- filter(validation_table, `admin_data/id_aksi`==admin_id[i])
      
      kontributor <- length(which(validation_table$`admin_data/id_aksi`==admin_id[i]))
      
      table_q1 <- table(data$`pertanyaan_kunci/detail_aksi/q1`)
      q1 <- names(table_q1[table_q1==max(table_q1)])
      if(length(q1)==2){
        q1_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q1`=="Tidak"))
        q1 <- "Tidak"
      } else {
        q1_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q1`==q1))  
      }
      # q1_maj <- nrow(data[data$q1 == q1,])
      q1_perc <- q1_maj/nrow(data) * 100
      
      table_q1.1 <- table(data$`pertanyaan_kunci/detail_aksi/q1.1`)
      q1.1 <- names(table_q1.1[table_q1.1==max(table_q1.1)])
      q1.1 <- ifelse(is.null(q1.1), "Tidak tahu", q1.1)
      
      table_q1.2 <- table(data$`pertanyaan_kunci/detail_aksi/q1.2`)
      q1.2 <- names(table_q1.2[table_q1.2==max(table_q1.2)])
      q1.2 <- ifelse(is.null(q1.2), "0", q1.2)
      
      table_q2 <- table(data$`pertanyaan_kunci/detail_aksi/q2`)
      q2 <- names(table_q2[table_q2==max(table_q2)])
      if(length(q2)==2){
        q2_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q2`=="Tidak"))
        q2 <- "Tidak"
      } else {
        q2_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q2`==q2))  
      }
      q2_perc <- q2_maj/nrow(data) * 100
      
      if (q1=="Tidak" | q2=="Tidak"){
        q2.1 <- 0
        q2.2 <- "Tidak tahu"
        q2.3 <- 0
        q2.4 <- 0
      } else {
        table_q2.1 <- table(data$`pertanyaan_kunci/detail_aksi/q2.1`)
        q2.1 <- names(table_q2.1[table_q2.1==max(table_q2.1)])
        
        table_q2.2 <- table(data$`pertanyaan_kunci/detail_aksi/q2.2`)
        q2.2 <- names(table_q2.2[table_q2.2==max(table_q2.2)])
        
        table_q2.3 <- table(data$`pertanyaan_kunci/detail_aksi/q2.3`)
        q2.3 <- names(table_q2.3[table_q2.3==max(table_q2.3)])
        
        table_q2.4 <- table(data$`pertanyaan_kunci/detail_aksi/q2.4`)
        q2.4 <- names(table_q2.4[table_q2.4==max(table_q2.4)])
      }
      
      unique_q3 <- tolower(data$`pertanyaan_kunci/detail_aksi/recom`)
      q3 <- str_c(unique_q3, collapse = "; ")
      
      fin_valass <- "Belum ada penilaian"
      fin_valass <- ifelse(q1_perc>=80 , "Tinggi", fin_valass)
      fin_valass <- ifelse(q1_perc<80 & q1_perc>=60 , "Sedang", fin_valass)
      fin_valass <- ifelse(q1_perc<60 , "Rendah", fin_valass)
      
      am_id <- unique(admin_id[i])
      
      test <- cbind(am_id, kontributor, q1, q1_maj, q1_perc, q1.1, q1.2, q2, q2_maj, q2_perc, q2.1, q2.2, q2.3, q2.4, q3, fin_valass)
      test <- as.data.frame(test)
      
      c=rbind(c, test)
    }
    
    hasil <- as.data.frame(c)
    
    grafikQC <- ggplot(hasil, aes(x=fin_valass))+
      geom_bar(stat="count", width=0.7, fill="purple")+
      theme_minimal() + 
      labs(title="Kontrol Kualitas Data SivaTif", x="Kualitas", y = "Jumlah Aksi")
    ggplotly(grafikQC) 
  })
  
  output$tabelQC <- renderDataTable({
    validation_table <- koboData$vam
    admin_id <- unique(validation_table$`admin_data/id_aksi`)
    validation_table$`pertanyaan_kunci/detail_aksi/q1` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q1`, "1", "Iya")
    validation_table$`pertanyaan_kunci/detail_aksi/q1` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q1`, "2", "Tidak")
    validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "1", "Iya")
    validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "2", "Tidak")
    validation_table$`pertanyaan_kunci/detail_aksi/q2`[is.na(validation_table$`pertanyaan_kunci/detail_aksi/q2`)] <- 0
    validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "0" , "Tidak")
    validation_table$`pertanyaan_kunci/detail_aksi/recom`[is.na(validation_table$`pertanyaan_kunci/detail_aksi/recom`)] <- "tidak tahu"
    
    c=NULL
    for (i in 1:length(admin_id)) {
      data <- filter(validation_table, `admin_data/id_aksi`==admin_id[i])
      
      kontributor <- length(which(validation_table$`admin_data/id_aksi`==admin_id[i]))
      
      table_q1 <- table(data$`pertanyaan_kunci/detail_aksi/q1`)
      q1 <- names(table_q1[table_q1==max(table_q1)])
      if(length(q1)==2){
        q1_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q1`=="Tidak"))
        q1 <- "Tidak"
      } else {
        q1_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q1`==q1))  
      }
      # q1_maj <- nrow(data[data$q1 == q1,])
      q1_perc <- q1_maj/nrow(data) * 100
      
      table_q1.1 <- table(data$`pertanyaan_kunci/detail_aksi/q1.1`)
      q1.1 <- names(table_q1.1[table_q1.1==max(table_q1.1)])
      q1.1 <- ifelse(is.null(q1.1), "Tidak tahu", q1.1)
      
      table_q1.2 <- table(data$`pertanyaan_kunci/detail_aksi/q1.2`)
      q1.2 <- names(table_q1.2[table_q1.2==max(table_q1.2)])
      q1.2 <- ifelse(is.null(q1.2), "0", q1.2)
      
      table_q2 <- table(data$`pertanyaan_kunci/detail_aksi/q2`)
      q2 <- names(table_q2[table_q2==max(table_q2)])
      if(length(q2)==2){
        q2_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q2`=="Tidak"))
        q2 <- "Tidak"
      } else {
        q2_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q2`==q2))  
      }
      q2_perc <- q2_maj/nrow(data) * 100
      
      if (q1=="Tidak" | q2=="Tidak"){
        q2.1 <- 0
        q2.2 <- "Tidak tahu"
        q2.3 <- 0
        q2.4 <- 0
      } else {
        table_q2.1 <- table(data$`pertanyaan_kunci/detail_aksi/q2.1`)
        q2.1 <- names(table_q2.1[table_q2.1==max(table_q2.1)])
        
        table_q2.2 <- table(data$`pertanyaan_kunci/detail_aksi/q2.2`)
        q2.2 <- names(table_q2.2[table_q2.2==max(table_q2.2)])
        
        table_q2.3 <- table(data$`pertanyaan_kunci/detail_aksi/q2.3`)
        q2.3 <- names(table_q2.3[table_q2.3==max(table_q2.3)])
        
        table_q2.4 <- table(data$`pertanyaan_kunci/detail_aksi/q2.4`)
        q2.4 <- names(table_q2.4[table_q2.4==max(table_q2.4)])
      }
      
      unique_q3 <- tolower(data$`pertanyaan_kunci/detail_aksi/recom`)
      q3 <- str_c(unique_q3, collapse = "; ")
      
      fin_valass <- "Belum ada penilaian"
      fin_valass <- ifelse(q1_perc>=80 , "Tinggi", fin_valass)
      fin_valass <- ifelse(q1_perc<80 & q1_perc>=60 , "Sedang", fin_valass)
      fin_valass <- ifelse(q1_perc<60 , "Rendah", fin_valass)
      
      am_id <- unique(admin_id[i])
      
      test <- cbind(am_id, kontributor, q1, q1_maj, q1_perc, q1.1, q1.2, q2, q2_maj, q2_perc, q2.1, q2.2, q2.3, q2.4, q3, fin_valass)
      test <- as.data.frame(test)
      
      c=rbind(c, test)
    }
    
    hasil <- as.data.frame(c)
    tables$dataQC <- hasil
    
    tabelHasil <-  as.data.frame(hasil)
    tabelHasil$q1_maj <- NULL
    tabelHasil$q2_maj <- NULL
    colnamesQC <- read_excel("data/colnames_sivatif.xlsx", sheet = "QC")
    colnames(tabelHasil) <- colnamesQC$`Nama kolom`
    tabelHasil$`Tingkat Keyakinan (%) Aksi Mitigasi` <- as.numeric(as.character(tabelHasil$`Tingkat Keyakinan (%) Aksi Mitigasi`))
    tabelHasil$`Tingkat Keyakinan (%) Aksi Mitigasi` <- round(tabelHasil$`Tingkat Keyakinan (%) Aksi Mitigasi`, digits = 2)
    tabelHasil$`Tingkat Keyakinan (%) Objek` <- as.numeric(as.character(tabelHasil$`Tingkat Keyakinan (%) Objek`))
    tabelHasil$`Tingkat Keyakinan (%) Objek` <- round(tabelHasil$`Tingkat Keyakinan (%) Objek`, digits = 2)
    
    # datatable(tabelHasil, options = list(scrollX = TRUE))
    
    headerCallback <- c(
      "function(thead, data, start, end, display){",
      "  var tooltips = ['Nomor identifikasi unik aksi mitigasi di dalam AKSARA','Jumlah pengguna yang berkontribusi pada pengambilan data',
                        'Keberadaan aksi mitigasi yang dikunjungi','Tingkat keyakinan keberadaan aksi mitigasi',
                        'Nama kegiatan aksi mitigasi yang dilaporkan di dalam AKSARA','Tahun dilaksanakannya aksi mitigasi yang dilaporkan ke AKSARA',
                        'Keberadaan objek/item aksi mitigasi yang dikunjungi','Tingkat keyakinan keberadaan objek/item aksi mitigasi',
                        'Jumlah yang direalisasikan berdasarkan jawaban mayoritas kontributor', 'Jenis objek berdasarkan jawaban mayoritas kontributor',
                        'Umur objek berdasarkan jawaban mayoritas kontributor', 'Jumlah objek hidup berdasarkan jawaban mayoritas konributor',
                        'Usulan rekomendasi seluruh kontributor', 'Tingkat keyakinan kualitas data SiVaTif'];",
      "  for(var i=0; i<tooltips.length; i++){",
      "    $('th:eq('+i+')',thead).attr('title', tooltips[i]);",
      "  }",
      "}"
    )
    
    datatable(tabelHasil, rownames = FALSE, escape = FALSE,
              options = list(
                headerCallback = JS(headerCallback), scrollX = TRUE
              )
    )
  })
  
  output$tabelvalidasi <- renderDataTable({
    
    ### TABEL AKSARA ####
    data_aksara <- read_excel("data/aksara_table.xlsx")
    
    ### TABEL HASIL VALIDASI ####
    validation_table <- koboData$vam
    admin_id <- unique(validation_table$`admin_data/id_aksi`)
    
    hasil <- tables$dataQC
    
    d=NULL
    for (i in 1:length(admin_id)) {
      tabel_aksara <- filter(data_aksara, id==admin_id[i])
      tbl <- filter(hasil, am_id==admin_id[i])
      kontributor <- length(which(validation_table$`admin_data/id_aksi`==admin_id[i]))
      
      aksara_nama <- tabel_aksara$nama_kegiatan
      sivatif_nama <- as.character(tbl$q1.1)
      kesesuaian_nama <- "Tidak Sesuai"
      kesesuaian_nama <- ifelse(sivatif_nama==aksara_nama, "Sesuai", kesesuaian_nama)
      
      aksara_tahun <- tabel_aksara$tahun_pelaporan
      sivatif_tahun <- as.numeric(as.character(tbl$q1.2))
      kesesuaian_tahun <- "Tidak Sesuai"
      kesesuaian_tahun <- ifelse(sivatif_tahun==aksara_tahun, "Sesuai", kesesuaian_tahun)
      
      aksara_realisasi <- tabel_aksara$realisasi
      sivatif_realisasi <- as.numeric(as.character(tbl$q2.1))
      kesesuaian_realisasi <- "Tidak Sesuai"
      kesesuaian_realisasi <- ifelse(sivatif_realisasi==aksara_realisasi, "Sesuai", kesesuaian_realisasi)
      
      aksara_jenis <- tabel_aksara$jenis_pohon_lain
      sivatif_jenis <- as.character(tbl$q2.2)
      kesesuaian_jenis <- "Tidak Sesuai"
      kesesuaian_jenis <- ifelse(sivatif_jenis==aksara_jenis, "Sesuai", kesesuaian_jenis)
      
      aksara_jumlah <- tabel_aksara$jumlah_pohon_tanaman_yang_masih_hidup
      sivatif_jumlah <- as.numeric(as.character(tbl$q2.4))
      persen_selisih <- (sivatif_jumlah / aksara_jumlah)
      kondisi <- "Sangat Tidak Baik"
      kondisi <- ifelse(persen_selisih>=0.8 , "Sangat Baik", kondisi)
      kondisi <- ifelse(persen_selisih<0.8 & persen_selisih>=0.6 , "Baik", kondisi)
      kondisi <- ifelse(persen_selisih<0.6 & persen_selisih>=0.4 , "Kurang Baik", kondisi)
      kondisi <- ifelse(persen_selisih<0.4 & persen_selisih>=0.2 , "Tidak Baik", kondisi)
      penilaian_validasi <- "BELUM DIVALIDASI"
      # penilaian_validasi <- ifelse(kontributor < 5, "BELUM DIVALIDASI", penilaian_validasi)
      penilaian_validasi <- ifelse(kontributor >= 5 & hasil[which(hasil$am_id==admin_id[i]),]$fin_valass=="Tinggi" & (kesesuaian_nama=="Tidak Sesuai" | kesesuaian_tahun=="Tidak Sesuai" | kesesuaian_realisasi=="Tidak Sesuai" | kesesuaian_jenis=="Tidak Sesuai" | persen_selisih < 0.6), "PERLU DIREVISI", penilaian_validasi)
      penilaian_validasi <- ifelse(kontributor >= 5 & hasil[which(hasil$am_id==admin_id[i]),]$fin_valass=="Tinggi" & kesesuaian_nama=="Sesuai" & kesesuaian_tahun=="Sesuai" & kesesuaian_realisasi=="Sesuai" & kesesuaian_jenis=="Sesuai" & persen_selisih>=0.6, "TERVALIDASI", penilaian_validasi)
      
      am_id <- unique(admin_id[i])
      
      final_tabel <-as.data.frame(cbind(am_id, aksara_nama, sivatif_nama, kesesuaian_nama, aksara_tahun, sivatif_tahun, kesesuaian_tahun, aksara_realisasi, 
                                        sivatif_realisasi, kesesuaian_realisasi, aksara_jenis, sivatif_jenis, kesesuaian_jenis, aksara_jumlah, sivatif_jumlah,
                                        persen_selisih, kondisi, penilaian_validasi))
      d=rbind(d, final_tabel)
    }
    
    d <- unique(d)
    d <- data.frame(d)
    rownames(d) <- NULL
    tables$dataValid <- d
    
    tabelVal <- as.data.frame(d)
    colnamesVal <- read_excel("data/colnames_sivatif.xlsx", sheet = "validasi")
    colnames(tabelVal) <- colnamesVal$`Nama kolom`
    tabelVal$`Proporsi (%)` <- as.numeric(as.character(tabelVal$`Proporsi (%)`))
    tabelVal$`Proporsi (%)` <- round(tabelVal$`Proporsi (%)`, digits = 2)
    
    # datatable(d, escape = FALSE, options = list(searching = FALSE, info = FALSE), fillContainer = TRUE)
    # datatable(d, escape = FALSE, options = list(scrollY = 300, scrollCollapse = TRUE))
    # datatable(tabelVal, options = list(scrollX = TRUE))
    
    headerCallback <- c(
      "function(thead, data, start, end, display){",
      "  var tooltips = ['Nomor identifikasi unik aksi mitigasi di dalam AKSARA','Nama kegiatan aksi mitigasi yang dilaporkan ke AKSARA',
                        'Nama kegiatan aksi mitigasi berdasarkan validasi lapangan oleh kontributor SiVaTif','Kesesuaian nama kegiatan aksi mitigasi antara AKSARA dan SiVaTif',
                        'Tahun dilaksanakannya aksi mitigasi yang dilaporkan ke AKSARA','Tahun dilaksanakannya aksi mitigasi berdasarkan validasi lapangan oleh kontributor SiVaTif',
                        'Kesesuaian tahun dilaksanakannya aksi mitigasi antara AKSARA dan SiVaTif','Jumlah realisasi aksi mitigasi yang dilaporkan ke AKSARA',
                        'Jumlah realisasi aksi mitigasi berdasarkan validasi lapangan oleh kontributor SiVaTif', 'Kesesuaian jumlah realisasi aksi mitigasi antara AKSARA dan SiVaTif',
                        'Jenis item/objek aksi mitigasi yang dilaporkan ke AKSARA', 'Jenis item/objek aksi mitigasi berdasarkan validasi lapangan oleh kontributor SiVaTif',
                        'Jumlah item/objek aksi mitigasi yang hidup dan dilaporkan ke AKSARA', 'Jumlah item/objek aksi mitigasi yang hidup berdasarkan validasi lapangan oleh kontributor SiVaTif',
                        'Kesesuaianitem/objek aksi mitigasi yang hidup antara AKSARA dan SiVaTif', 'Proporsi perbandingan jumlah item/objek aksi mitigasi yang hidup yang dilaporkan di AKSARA dengan kondisi di lapangan',
                        'Kelas kondisi terkini objek aksi mitigasi', 'Penilaian akhir hasil validasi aksi mitigasi'];",
      "  for(var i=0; i<tooltips.length; i++){",
      "    $('th:eq('+i+')',thead).attr('title', tooltips[i]);",
      "  }",
      "}"
    )
    
    datatable(tabelVal, rownames = FALSE, escape = FALSE,
              options = list(
                headerCallback = JS(headerCallback), scrollX = TRUE
              )
    )
  })
}

# runApp(list(ui = ui, server = server), launch.browser = TRUE)
shinyApp(ui = ui, server = server)