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
saveRDS(registKoboData, "data/registKoboData")

### App ###
url_app <- paste0(kc_server_url,"api/v1/data/",form_app,"?format=csv")
rawdata_app  <- GET(url_app,authenticate("vamprk2020","Icraf2019!"),progress())
vamKoboData  <- read_csv(content(rawdata_app,"raw",encoding = "UTF-8"))
saveRDS(vamKoboData, "data/vamKoboData")