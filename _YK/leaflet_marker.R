library('leaflet')

# Fake data
df <- data.frame(lng = c(-5, -10, -15, -20, 25),
                 lat = c(8, 12, 33, 4, 18),
                 size = c(200000, 100000, 800000, 250000, 350000),
                 popup = c('A', 'B', 'C', 'D', 'E'),
                 type = c('A', 'A', 'C', 'D', 'E'),
                 stringsAsFactors = FALSE)

# If you want to set your own colors manually:
pal <- colorFactor(
  palette = c('red', 'blue', 'green', 'purple', 'orange'),
  domain = df$type
)

# If you want to use predefined palettes in the RColorBrewer package:
# Call RColorBrewer::display.brewer.all() to see all possible palettes
pal <- colorFactor(
  palette = 'Dark2',
  domain = df$type
)

leaflet(df) %>%
  addTiles() %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1, 
             radius = ~size, popup = ~popup, color = ~pal(type))

# first 20 quakes
df.20 <- quakes[1:20,]

getColor <- function(quakes) {
  sapply(quakes$mag, function(mag) {
    if(mag <= 4) {
      "green"
    } else if(mag <= 5) {
      "orange"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'document-text',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(df.20)
)

leaflet(df.20) %>% addTiles() %>%
  addAwesomeMarkers(~long, ~lat, icon=icons, label=~as.character(mag))
