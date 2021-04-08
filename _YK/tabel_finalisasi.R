library(plyr)
library(dplyr)
library(DT)
library(readxl)
library(stringr)

### TABEL QUALITY CONTROL (QC) VALIDASI ####

# validation_table <- read_excel("data/validation_table.xlsx")
validation_table <- readRDS("data/vamKoboData")
admin_id <- unique(validation_table$`admin_data/id_aksi`)
validation_table$`pertanyaan_kunci/detail_aksi/q1` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q1`, "1", "YES")
validation_table$`pertanyaan_kunci/detail_aksi/q1` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q1`, "2", "NO")
validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "1", "YES")
validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "2", "NO")
validation_table$`pertanyaan_kunci/detail_aksi/q2`[is.na(validation_table$`pertanyaan_kunci/detail_aksi/q2`)] <- 0
validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "0" , "Tidak ada data")

c=NULL
for (i in 1:length(admin_id)) {
  data <- filter(validation_table, `admin_data/id_aksi`==admin_id[i])
  
  kontributor <- length(which(validation_table$`admin_data/id_aksi`==admin_id[i]))
  
  table_q1 <- table(data$`pertanyaan_kunci/detail_aksi/q1`)
  q1 <- names(table_q1[table_q1==max(table_q1)])
  if(length(q1)==2){
    q1_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q1`=="NO"))
    q1 <- "NO"
  } else {
    q1_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q1`==q1))  
  }
  # q1_maj <- nrow(data[data$q1 == q1,])
  q1_perc <- q1_maj/nrow(data) * 100
  
  table_q1.1 <- table(data$`pertanyaan_kunci/detail_aksi/q1.1`)
  q1.1 <- names(table_q1.1[table_q1.1==max(table_q1.1)])
  
  table_q1.2 <- table(data$`pertanyaan_kunci/detail_aksi/q1.2`)
  q1.2 <- names(table_q1.2[table_q1.2==max(table_q1.2)])
  
  table_q2 <- table(data$`pertanyaan_kunci/detail_aksi/q2`)
  q2 <- names(table_q2[table_q2==max(table_q2)])
  if(q2=="Tidak ada data"){
    q2_maj <- "Tidak ada data"
    q2 <- "Tidak ada data"
    q2_perc <- 0
  } else {
    q2_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q2`==q2))
    q2_perc <- q2_maj/nrow(data) * 100
  }
  
  # table_q2 <- table(data$`pertanyaan_kunci/detail_aksi/q2`)
  # q2 <- names(table_q2[table_q2==max(table_q2)])
  # q2_maj <- nrow(data[data$`pertanyaan_kunci/detail_aksi/q2` == q2,])
  # # q2 <- names(table_q2[table_q2==max(table_q2)])
  # # if(length(q1)==2){
  # #   q2_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q2`=="NO"))
  # #   q2 <- "NO"
  # # } else {
  # #   q2_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q2`==q2))  
  # # }
  # q2_perc <- q2_maj/nrow(data) * 100
  
  table_q2.1 <- table(data$`pertanyaan_kunci/detail_aksi/q2.1`)
  q2.1 <- names(table_q2.1[table_q2.1==max(table_q2.1)])
  
  table_q2.2 <- table(data$`pertanyaan_kunci/detail_aksi/q2.2`)
  q2.2 <- names(table_q2.2[table_q2.2==max(table_q2.2)])
  
  table_q2.3 <- table(data$`pertanyaan_kunci/detail_aksi/q2.3`)
  q2.3 <- names(table_q2.3[table_q2.3==max(table_q2.3)])
  
  table_q2.4 <- table(data$`pertanyaan_kunci/detail_aksi/q2.4`)
  q2.4 <- names(table_q2.4[table_q2.4==max(table_q2.4)])
  
  unique_q3 <- tolower(unique(data$`pertanyaan_kunci/detail_aksi/recom`))
  q3 <- str_c(unique_q3, collapse = "; ")
  
  fin_valass <- "Belum ada penilaian"
  fin_valass <- ifelse(q1_perc>=80 , "High", fin_valass)
  fin_valass <- ifelse(q1_perc<80 & q1_perc>=60 , "Moderate", fin_valass)
  fin_valass <- ifelse(q1_perc<60 , "Low", fin_valass)
  
  am_id <- unique(admin_id[i])
  
  test <- cbind(am_id, kontributor, q1, q1_maj, q1_perc, q1.1, q1.2, q2, q2_maj, q2_perc, q2.1, q2.2, q2.3, q2.4, q3, fin_valass)
  test <- as.data.frame(test)
  
  c=rbind(c, test)
}

hasil <- as.data.frame(c)
datatable(hasil)

grafikQC <- ggplot(hasil, aes(x=fin_valass))+
  geom_bar(stat="count", width=0.7, fill="purple")+
  theme_minimal() + 
  labs(title="Kontrol Kualitas Data Sivatif", x="Kualitas", y = "Jumlah Aksi")
ggplotly(grafikQC) 

### TABEL AKSARA ####
data_aksara <- read_excel("data/aksara_table.xlsx")

### TABEL HASIL VALIDASI ####
d=NULL
for (i in 1:length(admin_id)) {
  tabel_aksara <- filter(data_aksara, id==admin_id[i])
  tbl <- filter(hasil, am_id==admin_id[i])
  
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

datatable(d)

d<- unique(d)

validation_table$`_geolocation.0` <- as.numeric(validation_table$`_geolocation.0`)
validation_table$`_geolocation.1` <- as.numeric(validation_table$`_geolocation.1`)
# vamKoboData$aksi <- vamKoboData$`pertanyaan_kunci/detail_aksi/q1.1`
# vamKoboData$aksi <- str_replace_all(aksi,"__", "_")
# vamKoboData$aksi <- str_replace_all(aksi,"_", " ")
kobo_data <- subset(validation_table, select=c(`_geolocation.0`, `_geolocation.1`, `pertanyaan_kunci/detail_aksi/q1.1`))
colnames(kobo_data) = c("latitude", "longitude", "aksi")

getColor <- function(d) {
  sapply(d$penilaian_validasi, function(penilaian_validasi) {
    if(penilaian_validasi == "TERVALIDASI") {
      "green"
    } else if(penilaian_validasi == "PERLU DIREVISI") {
      "orange"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'document-text',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(d)
)

leaflet(d) %>% addTiles() %>%
  addAwesomeMarkers(~longitude, ~latitude, icon=icons, label=~as.character(penilaian_validasi))


### Testing ####
uji <- c("YES", "YES", "NO", "NO", "YES", "NO")
test <- table(uji)
q1 <- names(test[test==max(test)])
if(length(q1)==2){
  q1_maj <- length(which(uji=="NO"))
  q1 <- "NO"
} else {
  q1_maj <- length(which(uji==a))  
}
