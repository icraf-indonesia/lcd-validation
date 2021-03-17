# source("http://news.mrdwab.com/install_github.R")
# install_github("mrdwab/koboloadeR")
# library(koboloadeR)
# kobo_apps("data_viewer")

###*Download data from Kobo##
library(httr)
library(jsonlite)
library(readr)
kobo_server_url <- "https://kf.kobotoolbox.org/"
kc_server_url <- "https://kc.kobotoolbox.org/"

form_reg <- 421335 #Registrasi
form_app <- 623420 #App

### Registrasi ###
url_reg <- paste0(kc_server_url,"api/v1/data/",form_reg,"?format=csv")
rawdata_reg  <- GET(url_reg,authenticate("vamprk2020","Icraf2019!"),progress())
registKoboData  <- read_csv(content(rawdata_reg,"raw",encoding = "UTF-8"))

### App ###
url_app <- paste0(kc_server_url,"api/v1/data/",form_app,"?format=csv")
rawdata_app  <- GET(url_app,authenticate("vamprk2020","Icraf2019!"),progress())
vamKoboData  <- read_csv(content(rawdata_app,"raw",encoding = "UTF-8"))

saveRDS(vamKoboData, "data/vamKoboData")
saveRDS(registKoboData, "data/registKoboData")

aksara_data <- read_excel("data/aksara-data.xlsx")
vamKoboData<-readRDS("data/vamKoboData")
vamKoboData$`profil/email` <- tolower(vamKoboData$`profil/email`)
registKoboData <- readRDS("data/registKoboData")

library(plyr)
library(dplyr)
library(DT)
library(readxl)

aksara_data <- read_excel("data/data_dummy_lahan.xlsx")
validation_table <- read_excel("data/validation_table.xlsx")
number <- max(validation_table$valuser_id)

### TABEL QUALITY CONTROL (QC) VALIDASI ####
validation_table <- vamKoboData
validation_table$`pertanyaan_kunci/detail_aksi/q1` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q1`, "1", "YES")
validation_table$`pertanyaan_kunci/detail_aksi/q1` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q1`, "2", "NO")
validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "1", "YES")
validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "2", "NO")
validation_table$`pertanyaan_kunci/detail_aksi/q2`[is.na(validation_table$`pertanyaan_kunci/detail_aksi/q2`)] <- 0
validation_table$`pertanyaan_kunci/detail_aksi/q2` <- str_replace_all(validation_table$`pertanyaan_kunci/detail_aksi/q2`, "0" , "Tidak ada data")
admin_id <- unique(validation_table$`admin_data/id_aksi`)

data <- filter(validation_table, `admin_data/id_aksi`==17089)

am_id <- unique(data$`admin_data/id_aksi`)

table_q1 <- table(data$`pertanyaan_kunci/detail_aksi/q1`)
q1 <- names(table_q1[table_q1==max(table_q1)])
q1_maj <- nrow(data[data$`pertanyaan_kunci/detail_aksi/q1` == q1,])
q1_perc <- q1_maj/nrow(data) * 100

table_q1.1 <- table(data$`pertanyaan_kunci/detail_aksi/q1.1`)
q1.1 <- names(table_q1.1[table_q1.1==max(table_q1.1)])

table_q1.2 <- table(data$`pertanyaan_kunci/detail_aksi/q1.2`)
q1.2 <- names(table_q1.2[table_q1.2==max(table_q1.2)])

table_q2 <- table(data$`pertanyaan_kunci/detail_aksi/q2`)
q2 <- names(table_q2[table_q2==max(table_q2)])
q2_maj <- nrow(data[data$`pertanyaan_kunci/detail_aksi/q2` == q2,])
q2_perc <- q2_maj/nrow(data) * 100

if(q2=="Tidak ada data"){
  q2_maj <- "Tidak ada data"
  q2 <- "Tidak ada data"
  q2_perc <- 0
} else {
  q2_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q2`==q2))
  q2_perc <- q2_maj/nrow(data) * 100
}

test <- as.data.frame(cbind(am_id, q1, q1_maj, q1_perc, q1.1, q1.2, q2, q2_maj, q2_perc))

if(length(q2)==2){
  q2_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q1`=="NO"))
  q1 <- "NO"
} else {
  q2_maj <- length(which(data$`pertanyaan_kunci/detail_aksi/q1`==q1))  
}

table_q2.1 <- table(data$q2.1)
q2.1 <- names(table_q2.1[table_q2.1==max(table_q2.1)])

table_q2.2 <- table(data$q2.2)
q2.2 <- names(table_q2.2[table_q2.2==max(table_q2.2)])

table_q2.3 <- table(data$q2.3)
q2.3 <- names(table_q2.3[table_q2.3==max(table_q2.3)])

table_q2.4 <- table(data$q2.4)
q2.4 <- names(table_q2.4[table_q2.4==max(table_q2.4)])

unique_q3 <- tolower(unique(data$q3))
q3 <- str_c(unique_q3, collapse = "; ")

fin_valass <- "Belum ada penilaian"
fin_valass <- ifelse(q1_perc>=80 , "High", fin_valass)
fin_valass <- ifelse(q1_perc<80 & q1_perc>=60 , "Moderate", fin_valass)
fin_valass <- ifelse(q1_perc<60 , "Low", fin_valass)

test <- as.data.frame(cbind(am_id, q1, q1_maj, q1_perc, q1.1, q1.2, q2, q2_maj, q2_perc, q2.1, q2.2, q2.3, q2.4, q3, fin_valass))

datatable(test)

### TABEL HASIL VALIDASI ####
tabel_aksara <- filter(data_aksara, id==3114)
tbl <- filter(hasil, am_id==3114)

am_id <- 3114
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
penilaian_validasi <- "PERLU DIREVISI"
penilaian_validasi <- ifelse(kesesuaian_nama=="Sesuai" & kesesuaian_tahun=="Sesuai" & kesesuaian_realisasi=="Sesuai" & kesesuaian_jenis=="Sesuai" & persen_selisih>=0.6 , "TERVALIDASI", penilaian_validasi)

am_id <- unique(admin_id[i])

final_tabel <-as.data.frame(cbind(am_id, aksara_nama, sivatif_nama, kesesuaian_nama, aksara_tahun, sivatif_tahun, kesesuaian_tahun, aksara_realisasi, 
                                  sivatif_realisasi, kesesuaian_realisasi, aksara_jenis, sivatif_jenis, kesesuaian_jenis, aksara_jumlah, sivatif_jumlah,
                                  persen_selisih, kondisi, penilaian_validasi))
View(final_tabel)
