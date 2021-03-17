library(httr)
library(jsonlite)
library(readr)
library(ggplot2)
kobo_server_url <- "https://kf.kobotoolbox.org/"
kc_server_url <- "https://kc.kobotoolbox.org/"

form_reg <- 421335 #Registrasi
form_app <- 421351 #App

### Registrasi ###
url_reg <- paste0(kc_server_url,"api/v1/data/",form_reg,".csv")
rawdata_reg  <- GET(url_reg,authenticate("vamprk2020","Icraf2019!"),progress())
registKoboData  <- read_csv(content(rawdata_reg,"raw",encoding = "UTF-8"))

### App ###
url_app <- paste0(kc_server_url,"api/v1/data/",form_app,".csv")
rawdata_app  <- GET(url_app,authenticate("vamprk2020","Icraf2019!"),progress())
vamKoboData  <- read_csv(content(rawdata_app,"raw",encoding = "UTF-8"))

