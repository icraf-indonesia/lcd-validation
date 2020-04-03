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
library(sodium)

loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("Log In", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Email", label = tagList(icon("user"), "Email")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "MASUK", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Email atau password Anda salah!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     tags$code("Password: mypass"),
                   )
))

## Mengunduh data dari Kobo
vamKoboData <- kobo_data_downloader("421351", "vamprk2020:Icraf2019!")
registKoboData <- kobo_data_downloader("421335", "vamprk2020:Icraf2019!")

saveRDS(vamKoboData, "data/vamKoboData")
saveRDS(registKoboData, "data/registKoboData")

aksara_data <- read_excel("data/aksara-data.xlsx")
vamKoboData<-readRDS("data/vamKoboData")
vamKoboData$`profil/email` <- tolower(vamKoboData$`profil/email`)
registKoboData <- readRDS("data/registKoboData")

credentials = data.frame(
  username_id = registKoboData$`profil/email`,
  passod   = sapply(c("mypass", "mypass", "mypass", "mypass", "mypass"),password_store),
  # permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)

header <- dashboardHeader(title = NULL, titleWidth = NULL ,uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(disable=TRUE)
# body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "black")

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
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
      navbarPage("VAM PRK", position = "static-top", collapsible = TRUE,
                 tabPanel("Beranda", icon = icon("home"),
                          jumbotron(img(src="landingpage.png", width="100%"), " ", button = FALSE)
                 ),
                 # tabPanel("Masuk", icon = icon("user-cog", lib = "font-awesome"), 
                 #          selectInput("categoryProvince", label = "Pilih provinsi", 
                 #                      list(`Barat` = list("Aceh", "Bangka Belitung", "Bengkulu", "Jambi", "Kepulauan Riau",
                 #                                          "Lampung", "Riau", "Sumatera Barat", "Sumatera Selatan", "Sumatera Utara"),
                 #                           `Tengah` = list("Bali","Banten", "Jawa Barat",
                 #                                           "Jawa Tengah","Jawa Timur","Kalimantan Barat",
                 #                                           "Kalimantan Selatan","Kalimantan Tengah", "Kalimantan Timur",
                 #                                           "Nusa Tenggara Barat","Nusa Tenggara Timur","Yogyakarta"),
                 #                           `Timur` = list("Gorontalo", "Maluku", "Maluku Utara",
                 #                                          "Papua", "Papua Barat", "Sulawesi Selatan", "Sulawesi Tengah",
                 #                                          "Sulawesi Tenggara", "Sulawesi Barat", "Sulawesi Utara"))
                 #          ),
                 #          textInput("email", label = "E-mail", value = "",
                 #                    width = NULL, placeholder = "E-mail harus sama dengan saat Registrasi"),
                 #          actionButton("inputSetting", label = "Masuk")
                 # ),
                 tabPanel("Pengguna", icon = icon("user", lib = "font-awesome"),
                          fluidRow(
                            valueBoxOutput(width=6, "kontribusi"),
                            valueBoxOutput(width=6, "notValidate1")
                          ),
                          plotlyOutput("subsectorChart"),
                          plotlyOutput("validSubsector")
                 ),
                 tabPanel("Aksara", icon = icon("chart-bar", lib = "font-awesome"),
                          fluidRow(
                            valueBoxOutput(width=6, "kontributor"),
                            valueBoxOutput(width=6, "validator"),
                            valueBoxOutput(width=6, "validate"),
                            valueBoxOutput(width=6, "notValidate2")
                          ),
                          plotlyOutput("conditionChart"),
                          leafletOutput("distributionMap"),
                          dataTableOutput("recommendTbl")
                 ),
                 # tabPanel("Peta", icon = icon("map-marked-alt", lib = "font-awesome"),
                 #          actionButton("formInput", label = "Mulai isi form")
                 # ),
                 # tabPanel("Kirim", icon = icon("paper-plane", lib = "font-awesome"),
                 #          verbatimTextOutput("summary")
                 # ),
                 # tabPanel("Tabel", icon = icon("database", lib = "font-awesome"), 
                 #          dataTableOutput("tableAksi"),
                 #          actionButton("formInput", label = "Mulai isi form")
                 # ),
                 tabPanel("Tentang", icon = icon("info-circle", lib = "font-awesome"),
                          h3("LCD-Validation"),
                          p("Validasi merupakan proses menetapkan bukti yang memberikan jaminan tingkat tinggi bahwa suatu produk, layanan, atau sistem memenuhi persyaratan yang dimaksudkan. 
                      Validasi melibatkan penerimaan kesesuaian untuk tujuan dengan pengguna akhir dan pemangku kepentingan produk lainnya melalui proses eksternal.
                      Oleh karena itu, perlu adanya alat bantu untuk melakukan validasi aksi mitigasi yang dilaporkan")
                 )
      )
    }
    else {
      loginpage
    }
  })
  
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
  
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)
# shinyApp(ui = ui, server = server)