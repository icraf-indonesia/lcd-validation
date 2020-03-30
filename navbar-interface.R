library(markdown)

navbarPage("LCD Validation", position = "static-top", inverse = TRUE, collapsible = TRUE, 
           tabPanel("Beranda", icon = icon("home"),
                    jumbotron(img(src="landingpage.png", width="100%"), " ", button = FALSE)
           ),
           tabPanel("Masuk", icon = icon("user-cog", lib = "font-awesome"), 
                    selectInput("categoryProvince", label = "Pilih provinsi", 
                                list(`Barat` = list("Aceh", "Bangka Belitung", "Bengkulu", "Jambi", "Kepulauan Riau",
                                                    "Lampung", "Riau", "Sumatera Barat", "Sumatera Selatan", "Sumatera Utara"),
                                     `Tengah` = list("Bali","Banten", "Jawa Barat",
                                                     "Jawa Tengah","Jawa Timur","Kalimantan Barat",
                                                     "Kalimantan Selatan","Kalimantan Tengah", "Kalimantan Timur",
                                                     "Nusa Tenggara Barat","Nusa Tenggara Timur","Yogyakarta"),
                                     `Timur` = list("Gorontalo", "Maluku", "Maluku Utara",
                                                    "Papua", "Papua Barat", "Sulawesi Selatan", "Sulawesi Tengah",
                                                    "Sulawesi Tenggara", "Sulawesi Barat", "Sulawesi Utara"))
                    ),
                    textInput("email", label = "E-mail", value = "",
                              width = NULL, placeholder = "E-mail harus sama dengan saat Registrasi"),
                    actionButton("inputSetting", label = "Masuk")
           ),
           tabPanel("Analisis", icon = icon("chart-bar", lib = "font-awesome"),
                    fluidRow(
                      valueBoxOutput(width=6, "kontribusi"),
                      valueBoxOutput(width=6, "validator")
                    ),
                    plotlyOutput("curve1"),
                    plotlyOutput("curve2")
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