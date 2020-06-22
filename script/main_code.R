


# Setup -------------------------------------------------------------------

pacman::p_load(dplyr, wrapr, readr, rvest, stringr, feather, lubridate, ggplot2,
               raster, png, sf, st, tmap)




# Download all map pics ---------------------------------------------------

# dir.create('MapTiles')

scripts <- list.files('HTMLs', full.names = T)

# tiles <- read_html('Cached/2019-11-15-09-37.html') %>% 
#   html_nodes('.basemap') %>% 
#   html_nodes('.leaflet-tile-loaded') %>% 
#   html_attr('src') %>% 
#   str_remove_all('https://hkmap.live/tiles/') %>% 
#   str_replace_all('/', '_')
# 
# 
# for (tile in tiles[!file.exists('MapTiles/' %p% tiles)] ) {
#   
#   download.file(url = 'https://hkmap.live/tiles/' %p%
#                   str_replace_all(tile, '_', '/'),
#                 destfile = 'MapTiles/' %p% tile,
#                 'auto')
# }


for (script in scripts[821:length(scripts)]) {

  lapply(
    
    script %>% 
    read_html() %>% 
    # html_nodes('.leaflet-marker-icon')
    html_nodes('.leaflet-tile') %>% 
    html_attr('src'), 
    
      function(x) 
      
      
        if ( !str_remove(x, '/tiles/') %>% 
             str_replace_all(., '/', '_') %in% list.files('MapTiles') ) {
        
          
          download.file(url = 'https://hkmap.live' %p% x, 
                        destfile = 'MapTiles/' %p% 
                          (str_remove(x, '/tiles/') %>% 
                          str_replace_all(., '/', '_')),
                        'auto')
          
          Sys.sleep(60)
          
          }
      
  )

}



# Expand coordinates ------------------------------------------------------


# "topleft": [114.1424560546875, 22.33991442556203], "bottomleft": [114.1424560546875, 22.33483345753049], 
# "topright": [114.14794921875, 22.33991442556203], "botright": [114.14794921875, 22.33483345753049]

img <- readPNG('MapTiles/land-labels-gs_16_53547_28594.png')


r <- raster('MapTiles/land-labels-gs_16_53547_28594.png',
            crs = "+proj=tmerc +lat_0=0 +lon_0=-3 +k=1 +x_0=500000 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
            nrow = 256, ncol = 256, # = pixels
            # origin = c(114.1424560546875, 22.33991442556203),
            xmx = c(114.14794921875, 22.33483345753049),
            xmn = c(114.1424560546875, 22.33483345753049),
            ymn = c(114.1424560546875, 22.33483345753049),
            ymx = c(114.1424560546875, 22.33991442556203))

a <- expand.grid(latcoords = seq(from = 22.33991442556203, to = 22.33483345753049, length.out = 256),
            lngcoords = seq(from = 114.14794921875, to = 114.1424560546875, length.out = 256)) %>% 
  st_as_sf(coords = c("lngcoords", "latcoords"),
           crs = "+proj=tmerc +lat_0=0 +lon_0=-3 +k=1 +x_0=500000 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

hk <- read_sf('/Users/mariamilosh/Downloads/Hong_Kong_18_Districts',
              crs = "+proj=tmerc +lat_0=0 +lon_0=-3 +k=1 +x_0=500000 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
hk <- st_transform(hk, crs = "+proj=tmerc +lat_0=0 +lon_0=-3 +k=1 +x_0=500000 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

tm_shape(hk) + tm_polygons() +
  tm_shape(a) + tm_dots(col = 'red') 



xm <- matrix(xFromCell(r, c(1:65536)), nrow = 256, byrow = TRUE)
x <- raster(xm, 
            
            xmx = c(114.14794921875, 22.33483345753049),
            xmn = c(114.1424560546875, 22.33483345753049),
            ymn = c(114.1424560546875, 22.33483345753049),
            ymx = c(114.1424560546875, 22.33991442556203))


data_frame(x,
           y,
           z = 1)
rasterFromXYZ

r <- raster(nrow=5, ncol=5, # = pixels
            xmn=0, xmx=5, ymn=0, ymx=5, # = span in coordinates
            crs=NA)
set.seed(1)
values(r) <- sample(1:25)
r[r < 15] <- NA
xyz <- rasterToPoints(r)

rst <- rasterFromXYZ(xyz)







projection(x)<-"+proj=tmerc +lat_0=0 +lon_0=-3 +k=1 +x_0=500000 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

;
# Get N of events ---------------------------------------------------------

# invisible(
  
  lapply(list.files('HTMLs', full.names = T), # local data
    # lapply(list.files('/Users/mariamilosh/Dropbox/HK_server', full.names = T), # server data
       function(x) {
         
  print(x)

  # for ( x in list.files('HTMLs', full.names = T)) {
    
  if ( !file.exists( 'Extracted_icons/' %p% 
                     str_remove_all(x, '/Users/mariamilosh/Dropbox/HK_server/|HTMLs/|.txt') %p%
                     '.fea') &
       file.info(x)$size > 100000 ) {
         
    
  icons <- read_html(x) %>% 
  html_nodes('.icon') %>% 
  xml_attrs()

  if ( length(icons) > 0 ) {
    
    icons %>% 
      lapply(., function(y) { data.frame(as.list(y), stringsAsFactors = F) }) %>%


      bind_rows() %>%
    
      mutate(event.type = str_extract(class, 'icon\\d{1,2}'),
             event.old = ifelse( str_detect(class, 'icon_old'), T, F),
             opacity = str_extract(style, 'opacity: [0-9.]+') %>%
               str_remove('opacity: '),
             date = str_remove_all(x, 'HTMLs/|.txt')) %>% 
    
      select(-c(style, class, tabindex)) %>%
    
    
    
      write_feather(., 'Extracted_icons/' %p%
                      str_remove_all(x, '/Users/mariamilosh/Dropbox/HK_server/|HTMLs/|.txt') %p% 
                      '.fea')
    
     # Sys.sleep(20)
  
    } } }
  ) 
# )





# Compile -----------------------------------------------------------------






compilation <- lapply(list.files('Extracted_icons', full.names = T)[1:5000],
  read_feather
) %>% 
  bind_rows() %>% 
  left_join(., read_csv('IconIDs.csv') %>% 
              select(-url)) #%>% 
  
  # filter(opacity == 1)


# got to divide by the number of files in the time frame?
# or find a way to assign an ID to each icon and use only distinct
# otherwise some of them are counted multiple times

data <- compilation %>% 
  filter(opacity == 1) %>% 
  filter(!str_detect(date, 'conflicted')) %>% 
  filter(event.descript %in% c("Police", "TearGas", "IncomingTearGas", 
                               "IncomingBullets", "Danger", "PoliceVans",
                               "Protesters")) %>%
  mutate(date = ymd_hm(date) + 14*60*60) %>% # + 14 hours to HK timezone) 
  
  # group_by(date, event.descript) %>% # by minute / 3 minutes
  group_by(morning.evening = format(date, '%Y-%m-%d') %p%
             ' ' %p% ifelse(date %>%
                              hour() < 12, '06', '18') %>% 
             ymd_h()) %>% 
  
  summarise(N = n())




ggplot() + 
  geom_bar(data = data, aes(morning.evening, N), stat = 'identity') +
  
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_datetime(date_labels = "%b %d", 
                   breaks = '1 day')

























