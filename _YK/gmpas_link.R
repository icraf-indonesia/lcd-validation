library(readxl)
library(dplyr)
library(leaflet)
library(DT)

data_dummy <- read_excel("data/data_dummy_lahan.xlsx")

data_dummy$lat <- as.numeric(data_dummy$lat)
data_dummy$long <- as.numeric(data_dummy$long)

main_data <- subset(data_dummy, select=c(id, lat, long, nama_provinsi, nama_kegiatan, sumberdata_tahun))
main_data <- na.omit(main_data)

gmaps <- paste0("https://www.google.com/maps/search/?api=1&query=", main_data$lat,",", main_data$long)
gmaps <- as.data.frame(gmaps)

final_data <- cbind(main_data, gmaps)
leaflet(data= final_data) %>% addTiles() %>% addMarkers(lng=~long,lat=~lat, popup=~paste0(nama_kegiatan, "</br>Provinsi: ", 
                                                                                          nama_provinsi, "<br><a href='", gmaps, "'>Klik disini</a>"))

### Menghilangkan data yang latitudenya NA ###
# temp_data<-main_data[!is.na(main_data$lat), ]
# temp_data$sumberdata_tahun[is.na(temp_data$sumberdata_tahun)] <- "Tahun belum ditentukan"

### Menghilangkan data yang sumber tahunnya NA ###
# temp_data<-temp_data[!is.na(temp_data$sumberdata_tahun), ]

# leaflet(data= main_data) %>% addTiles() %>% addMarkers(lng=~long,lat=~lat, popup=~paste0(nama_kegiatan, "</br>Provinsi: ", 
#                                                                                          nama_provinsi, "<br><a href='", link_url, "'>", link_url, "</a>"))


# https://maps.google.com/maps?q=+4.776463+96.99245+&t=&z=15&ie=UTF8&iwloc=&output=embed
# https://maps.google.com/maps?q=+4.776463+96.99245+&t=&z=15&ie=UTF8
# https://maps.google.com/maps?q=+4.776463+96.99245
# https://www.google.com/maps/search/?api=1&query=47.5951518,-122.3316393
# https://www.google.com/maps/@?api=1&map_action=map&center=-33.712206,150.311941&zoom=12&basemap=terrain

data_dummy <- read_excel("data/data_dummy_lahan.xlsx")

data_dummy$lat <- as.numeric(data_dummy$lat)
data_dummy$long <- as.numeric(data_dummy$long)

main_data <- subset(data_dummy, select=c(id, lat, long, nama_provinsi, nama_kegiatan, tahun_pelaporan))
data_provinsi <- filter(main_data, main_data$nama_provinsi=="SUMATERA SELATAN")
data_provinsi<-data_provinsi[!is.na(data_provinsi$lat), ]
gmaps <- paste0("https://www.google.com/maps/search/?api=1&query=", data_provinsi$lat,",", data_provinsi$long)
data_provinsi$gmaps <- paste0("<a href='", gmaps, "' target='_blank'> Klik disini </a>")
data_provinsi$lat <- NULL
data_provinsi$long <- NULL
datatable(data_provinsi,  escape = FALSE)
