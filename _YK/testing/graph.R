library(readxl)
library(ggplot2)
library(plotly)
library(dplyr)
library(leaflet)

aksara_data <- read_excel("data/aksara-data.xlsx")

### Jumlah Aksi Mitigasi yang Berstatus Final ####
subsector <- ggplot(aksara_data, aes(x=factor(subsektor)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + 
  labs(title="Jumlah Aksi Mitigasi yang Berstatus Final", x="Subsektor", y = "Jumlah Aksi")
ggplotly(subsector)


kabkot <- ggplot(aksara_data, aes(x=factor(lokasi_kabkot)))+
  geom_bar(stat="count", width=0.7, fill="green")+
  theme_minimal() + 
  labs(title="Jumlah Aksi Mitigasi yang Berstatus Final", x="Kabupaten/Kota", y = "Jumlah Aksi")
ggplotly(kabkot)

### Jumlah Aksi Mitigasi yang Tervalidasi ####
aksara_data$jumlah <- 1
aksara_data$validate[aksara_data$validate==1] <- "Tervalidasi"
aksara_data$validate[aksara_data$validate==0] <- "Belum Tervalidasi"
valid <- ggplot(aksara_data, aes(x=factor(subsektor), fill=factor(validate))) + 
  geom_bar(stat="count") +
  labs(title="Jumlah Aksi Mitigasi yang Tervalidasi dan Belum Tervalidasi", x="Subsektor", y = "Jumlah Aksi", fill="") 
ggplotly(valid)


valid_kabkot <- ggplot(aksara_data, aes(x=factor(lokasi_kabkot), fill=factor(validate))) + 
  geom_bar(stat="count") +
  labs(title="Jumlah Aksi Mitigasi yang Tervalidasi dan Belum Tervalidasi", x="Subsektor", y = "Jumlah Aksi", fill="")  
ggplotly(valid_kabkot)

#Jumlah Pengguna & Aksi
vamKoboData$`profil/email` <- tolower(vamKoboData$`profil/email`)
kontributorFreq <- count(vamKoboData, `profil/email`)
kontributorList <- count(kontributorFreq, `profil/email`)
kontributor <- sum(kontributorList$n)
validator <- nrow(registKoboData)
tervalidasi <- length(which(aksara_data$validate=="Tervalidasi"))
belumTervalidasi <- length(which(aksara_data$validate=="Belum Tervalidasi"))

kriteria_data <- count(vamKoboData, `validation_form/pertanyaan_kunci/kondisi`)
colnames(kriteria_data) <- c("kondisi", "jumlah")
vamKoboData$`validation_form/pertanyaan_kunci/kondisi`<-factor(vamKoboData$`validation_form/pertanyaan_kunci/kondisi`, levels = c("Sangat Baik", "Baik", "Cukup", "Tidak Baik", "Sangat Tidak Baik"))
kondisi <- ggplot(vamKoboData, aes(x=factor(`validation_form/pertanyaan_kunci/kondisi`)))+
  geom_bar(stat="count", width=0.7, fill="purple")+
  theme_minimal() + 
  labs(title="Kondisi Terkini Kegiatan Aksi Mitigasi ", x="Kondisi", y = "Jumlah Aksi")
ggplotly(kondisi)

val_id <- 1:nrow(vamKoboData)
am_id <- aksara_data[which(vamKoboData$`validation_form/admin_data/desa` %in% aksara_data$lokasi_desa | vamKoboData$`validation_form/pertanyaan_kunci/aksi` %in% aksara_data$nama_kegiatan), ]
year <- as.data.frame(vamKoboData$`validation_form/pertanyaan_kunci/tahun`)
real_am <- as.data.frame(vamKoboData$`validation_form/pertanyaan_kunci/jumlah`)
cond_val <- as.data.frame(vamKoboData$`validation_form/pertanyaan_kunci/kondisi`)
recommend <- as.data.frame(vamKoboData$`validation_form/pertanyaan_kunci/rekomendasi`)
tabelRekomendasi <- as.data.frame(cbind(val_id, am_id$id, year, real_am, cond_val, recommend))
colnames(tabelRekomendasi) <- c("val_id", "am_id", "year", "real_am", "cond_val", "recommendation")

  
vamKoboData$`validation_form/pertanyaan_kunci/_point_latitude` <- as.numeric(vamKoboData$`validation_form/pertanyaan_kunci/_point_latitude`)
vamKoboData$`validation_form/pertanyaan_kunci/_point_longitude` <- as.numeric(vamKoboData$`validation_form/pertanyaan_kunci/_point_longitude`)
kobo_data <- subset(vamKoboData, select=c(`validation_form/pertanyaan_kunci/_point_latitude`, `validation_form/pertanyaan_kunci/_point_longitude`, `validation_form/pertanyaan_kunci/aksi`))
colnames(kobo_data) = c("latitude", "longitude", "aksi")
leaflet(data = kobo_data) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions()
)
