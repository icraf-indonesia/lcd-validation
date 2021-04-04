library(shiny)
library(DT)

library(readxl)
validation_table <- read_excel("data/validation_table.xlsx")

ui <- fluidPage(
  DT::dataTableOutput("DTtable")
)

server <- function(input, output) {
  output$DTtable <- DT::renderDataTable({
    data_dummy <- read_excel("data/data_dummy_lahan.xlsx")
    data_dummy$lat <- as.numeric(data_dummy$lat)
    data_dummy$long <- as.numeric(data_dummy$long)
    
    data_aksara <- read_excel("data/aksara_table.xlsx")
    
    # main_data <- subset(data_dummy, select=c(id, lat, long, nama_kegiatan, tahun_pelaporan, nama_provinsi))
    # data_provinsi <- filter(main_data, main_data$nama_provinsi=="SUMATERA SELATAN")
    # data_provinsi<-data_provinsi[!is.na(data_provinsi$lat), ]
    
    ### Data Dummy AKSARA ###
    main_data <- subset(data_aksara, select=c(id, nama_kegiatan, tahun_pelaporan, nama_provinsi))
    # data_provinsi <- filter(main_data, main_data$nama_provinsi=="SUMATERA SELATAN")
    temp_data <- data_dummy[!is.na(data_dummy$lat), ]
    data_provinsi <- cbind(main_data, temp_data$lat[1:6], temp_data$long[1:6])
    data_provinsi$kabkot <- "Tidak ada data"
    data_provinsi$kecamatan <- "Tidak ada data"
    data_provinsi$desa <- "Tidak ada data"
    
    gmaps <- paste0("https://www.google.com/maps/search/?api=1&query=", data_provinsi$`temp_data$lat[1:6]`,",", data_provinsi$`temp_data$long[1:6]`)
    # gmaps <- paste0("https://www.google.com/maps/search/?api=1&query=", data_provinsi$lat,",", data_provinsi$long)
    
    data_provinsi$gmaps <- paste0("<a href='", gmaps, "' target='_blank'> Klik disini </a>")
    # data_provinsi$lat <- NULL
    # data_provinsi$long <- NULL
    data_provinsi$`temp_data$lat[1:6]` <- NULL
    data_provinsi$`temp_data$long[1:6]` <- NULL
    
    colnamesLokasi <- read_excel("data/colnames_sivatif.xlsx", sheet = "lokasi")
    colnames(data_provinsi) <- colnamesLokasi$`Nama kolom`
    
    datatable(data_provinsi, 
              callback = JS("var tips = ['Nomor','Nomor identifikasi unik aksi mitigasi di dalam AKSARA','Nama kegiatan aksi mitigasi yang dilaporkan di dalam AKSARA',
                                          'Tahun dilaksanakannya aksi mitigasi','Nama Provinsi','Nama Kabupaten/Kota','Nama Kecamatan','Nama Desa','Google Maps'],
                            firstRow = $('#DTtable thead tr th');
                            for (var i = 0; i < tips.length; i++) {
                              $(firstRow[i]).attr('title', tips[i]);
                            }"), escape = FALSE)
  })
}

shinyApp(ui, server)

### Tabel Validasi ####
['Nomor','Nomor identifikasi unik aksi mitigasi di dalam AKSARA','Nama kegiatan aksi mitigasi yang dilaporkan ke AKSARA',
  'Nama kegiatan aksi mitigasi berdasarkan validasi lapangan oleh kontributor SiVaTif','Kesesuaian nama kegiatan aksi mitigasi antara AKSARA dan SiVaTif',
  'Tahun dilaksanakannya aksi mitigasi yang dilaporkan ke AKSARA','Tahun dilaksanakannya aksi mitigasi berdasarkan validasi lapangan oleh kontributor SiVaTif',
  'Kesesuaian tahun dilaksanakannya aksi mitigasi antara AKSARA dan SiVaTif','Jumlah realisasi aksi mitigasi yang dilaporkan ke AKSARA',
  'Jumlah realisasi aksi mitigasi berdasarkan validasi lapangan oleh kontributor SiVaTif', 'Kesesuaian jumlah realisasi aksi mitigasi antara AKSARA dan SiVaTif',
  'Jenis item/objek aksi mitigasi yang dilaporkan ke AKSARA', 'Jenis item/objek aksi mitigasi berdasarkan validasi lapangan oleh kontributor SiVaTif',
  'Jumlah item/objek aksi mitigasi yang hidup dan dilaporkan ke AKSARA', 'Jumlah item/objek aksi mitigasi yang hidup berdasarkan validasi lapangan oleh kontributor SiVaTif',
  'Kesesuaianitem/objek aksi mitigasi yang hidup antara AKSARA dan SiVaTif', 'Proporsi perbandingan jumlah item/objek aksi mitigasi yang hidup yang dilaporkan di AKSARA dengan kondisi di lapangan',
  'Kelas kondisi terkini objek aksi mitigasi', 'Penilaian akhir hasil validasi aksi mitigasi']

### Tabel QC ####
['Nomor','Nomor identifikasi unik aksi mitigasi di dalam AKSARA','Jumlah pengguna yang berkontribusi pada pengambilan data',
  'Keberadaan aksi mitigasi yang dikunjungi','Tingkat keyakinan keberadaan aksi mitigasi',
  'Nama kegiatan aksi mitigasi yang dilaporkan di dalam AKSARA','Tahun dilaksanakannya aksi mitigasi yang dilaporkan ke AKSARA',
  'Keberadaan objek/item aksi mitigasi yang dikunjungi','Tingkat keyakinan keberadaan objek/item aksi mitigasi',
  'Jumlah yang direalisasikan berdasarkan jawaban mayoritas kontributor', 'Jenis objek berdasarkan jawaban mayoritas kontributor',
  'Umur objek berdasarkan jawaban mayoritas kontributor', 'Jumlah objek hidup berdasarkan jawaban mayoritas konributor',
  'Usulan rekomendasi seluruh kontributor', 'Tingkat keyakinan kualitas data SiVaTif']
