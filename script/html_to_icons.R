#!/usr/local/bin/Rscript


# Setup -------------------------------------------------------------------

setwd('/Users/mariamilosh/Dropbox/HK/')
pacman::p_load(tidyverse, rvest, wrapr, foreach, parallel, doParallel)

options(digits = 17)


broaden <- function(x, step) {
  
  x <- x %>% 
    append(min(x) - step) %>%
    append(max(x) + step) %>% sort()
  
  return(x)
}

append_row <- function(tiles_df) {
  
  plyr::rbind.fill(tiles_df,
                   
             # top
             expand.grid(y_tile_trans_up = broaden(tiles_df$y_tile_trans_up, 256) %>% 
                           min(),
                         x_tile_trans_left = unique(tiles_df$x_tile_trans_left) %>% 
                           broaden(., 256) ) %>% 
               mutate(y_tile_trans_down = y_tile_trans_up + 256,
                      x_tile_trans_right = x_tile_trans_left + 256) %>% 
               cbind( expand.grid(square_y = broaden(tiles_df$square_y, 1) %>% 
                                    min(),
                                  square_x = unique(tiles_df$square_x) %>% 
                                    broaden(., 1)) ),
             
             # bottom
             expand.grid(y_tile_trans_up = broaden(tiles_df$y_tile_trans_up, 256) %>%
                           max(),
                         x_tile_trans_left = unique(tiles_df$x_tile_trans_left) %>% 
                           broaden(., 256) ) %>% 
               mutate(y_tile_trans_down = y_tile_trans_up + 256,
                      x_tile_trans_right = x_tile_trans_left + 256) %>% 
               cbind( expand.grid(square_y = broaden(tiles_df$square_y, 1) %>% 
                                    max(),
                                  square_x = unique(tiles_df$square_x) %>% 
                                    broaden(., 1)) ),
             
             # left
             expand.grid(x_tile_trans_left = tiles_df$x_tile_trans_left %>%
               # x_tile_trans_left = broaden(tiles_df$x_tile_trans_left, 256) %>%
                           min(),
                         y_tile_trans_up = unique(tiles_df$y_tile_trans_up) %>% 
                           broaden(., 256)) %>% 
               mutate(y_tile_trans_down = y_tile_trans_up + 256,
                      x_tile_trans_right = x_tile_trans_left + 256) %>% 
               cbind( expand.grid(square_x = tiles_df$square_x %>%
                                  # square_x = broaden(tiles_df$square_x, 1) %>%
                                    min(),
                                  square_y = unique(tiles_df$square_y) %>% 
                                    broaden(., 1)) ),
             
             # right
             expand.grid(x_tile_trans_left = tiles_df$x_tile_trans_left %>% 
               # x_tile_trans_left = broaden(tiles_df$x_tile_trans_left, 256) %>% 
                           max(),
                         y_tile_trans_up = unique(tiles_df$y_tile_trans_up) %>% 
                           broaden(., 256)) %>% 
               mutate(y_tile_trans_down = y_tile_trans_up + 256,
                      x_tile_trans_right = x_tile_trans_left + 256) %>% 
               cbind( expand.grid(square_x = tiles_df$square_x %>% 
                                 # square_x = broaden(tiles_df$square_x, 1) %>% 
                                    max(),
                                  square_y = unique(tiles_df$square_y) %>% 
                                    broaden(., 1)) )
  ) -> tiles_df
  
  return(tiles_df)
  
}

num2deg <- function(zoom, xtile, ytile) {
  
  n <- 2^zoom
  lon_deg <- xtile / n * 360 - 180
  lat_rad <- atan(sinh(3.141592653589793 * (1 - 2 * ytile / n)) )
  lat_deg <- REdaS::rad2deg(lat_rad)
  
  return(data.frame(lat_deg = lat_deg, lon_deg = lon_deg))
}


# Loop --------------------------------------------------------------------




# cl <- makeCluster(10, 'FORK')
# registerDoParallel(cl)


for (copy in list.files('HTMLs', full.names = T)) {
# foreach (copy = list.files('HTMLs', full.names = T), .verbose = T) %dopar% {
  # copy <- list.files('HTMLs', full.names = T)[1] # [113239:129691]
  cat('\n', copy, '|', format(Sys.time(), "%b %d %X"), '\n')
  
  out_dir <- "/Users/mariamilosh/Dropbox/HK/Icons_to_tiles" %p% 
    (copy %>% str_extract('/.*\\.') %>% 
       str_extract('/\\d{4}-\\d{2}-\\d{2}'))

  if ( !dir.exists(out_dir) ) { dir.create(out_dir) }
  
  if ( file.exists(out_dir %p% '/' %p% str_remove_all(copy, 'HTMLs/|.txt') %p% '.csv') |
       file.info(copy)$size < 40000 ) { next }
  
  
  ######### READ THE TILES #########
  
  prep <- read_html(copy)
  
  icons <- prep %>% 
    html_nodes('.icon') %>% 
    html_attr('style') %>% 
    
    str_extract_all('\\d+px', simplify = T) %>% 
    as_tibble()
  
  
  if ( nrow(icons) == 0 ) { 
    write_csv(icons,
              path = out_dir %p% '/' %p% str_remove_all(copy, 'HTMLs/|.txt') %p%
                '.csv')
    write_lines(copy %p% ': 0 icons found, writing empty csv.', append = T,
                path = 'Icons_to_files_logs/process.log')
    next }
  
  
  tiles_img <- prep %>%
    html_nodes('.basemap') %>% 
    last() %>% 
    html_nodes('.leaflet-tile-loaded') %>% 
    html_attr('src') %>% 
    str_replace_all('/', '_') %>% 
    str_remove('_tiles_')
  
  if (length(tiles_img) == 0) { 
    write_lines(copy %p% ': 0 tiles found, aborted.', append = T,
                path = 'Icons_to_files_logs/process.log')
    next }
  
  container <- prep %>% 
    html_nodes('.basemap') %>% 
    html_node('.leaflet-zoom-animated') %>% 
    last() %>% 
    html_attr('style') %>% 
    str_extract_all('\\d+px', simplify = T) %>% 
    .[1:2] %>% 
    str_remove(., pattern = 'px') %>% 
    as.numeric()
  
  
  tiles_feat <- prep %>% 
    html_nodes('.basemap') %>% 
    last() %>% 
    html_nodes('.leaflet-tile-loaded') %>% 
    html_attr('style') %>% 
    
    str_extract_all('\\d+px|-\\d+px', simplify = T) %>% 
    
    as_tibble() %>% 
    select(x_tile = V3, y_tile = V4) %>% 
    mutate_all(.funs = str_remove, pattern = 'px') %>% 
    mutate_all(.funs = as.numeric) %>% 
    
    mutate(x_tile_trans_left = x_tile + container[1],
           x_tile_trans_right = x_tile_trans_left + 256,
           
           y_tile_trans_up = y_tile + container[2],
           y_tile_trans_down = y_tile_trans_up + 256,
           
           # file_path = tiles_img,
           tile_zoom = tiles_img %>%
             str_extract_all('light_\\d{2}|light-labels_\\d{2}') %>%
             str_remove_all('light_|light-labels_'),
           
           square_x = tiles_img %>%
             str_extract('_\\d{4,6}_') %>%
             str_remove_all('_'),
           
           square_y = tiles_img %>%
             str_extract('\\d{4,6}.png') %>%
             str_remove_all('.png')) %>% 
    
    mutate_at(vars(contains('tile'), contains('square')),
              .funs = as.numeric) %>% 
    filter(tile_zoom == max(tile_zoom)) %>% 
    select(-c(ends_with('_tile')))
  
  
  
  
  ######### MAP ICONS TO TILES #########
  
  
  icons <- icons %>% 
    set_names(nm = c('margin_left', 'margin_top', 'width', 'height',
                     'img_x', 'img_y', 'img_z')) %>% 
    mutate_all(.funs = str_remove, pattern = 'px') %>% 
    mutate_all(.funs = as.numeric) %>% 
    mutate(icon_type = prep %>% 
             html_nodes('.icon') %>% html_attr('class') %>% 
             str_extract('icon\\d+ ') %>% str_squish(),
           icon_opacity = prep %>% 
             html_nodes('.icon') %>% html_attr('style') %>% 
             str_extract('opacity: [0-9.]+') %>% str_remove('opacity: '),
           icon_id = 1:nrow(.) )
    
  i = 0
  
  while ( min(icons$img_x) < min(tiles_feat$x_tile_trans_left) | # icon outside l-margin
          max(icons$img_x) > max(tiles_feat$x_tile_trans_right) | # icon outside r-margin
          min(icons$img_y) < min(tiles_feat$y_tile_trans_up) | # icon above map
          max(icons$img_y) > max(tiles_feat$y_tile_trans_down) ) { # icon below map
     
    i = i + 1
    if(i %% 50 == 0) { cat(i, 'rows added |||| ') }
    # print(i)
     
     tiles_feat <- 
       tiles_feat %>%
       append_row(.) %>% # +1 square
       distinct()
    
  }
  
  icons2 <- 
    full_join(icons %>%
                mutate(j = 1),
              tiles_feat %>%
                append_row(.) %>% # +1 square
                mutate(j = 1), by = 'j') %>% 
    mutate(x_within = img_x >= x_tile_trans_left & img_x < x_tile_trans_right,
           y_within = img_y >= y_tile_trans_up & img_y < y_tile_trans_down,
           tile_zoom = replace_na(tile_zoom, tile_zoom %>% 
                                    na.omit() %>% unique()) ) %>% 
    filter(x_within == T & y_within == T) %>% 
    distinct()
  
  
  if (nrow(icons) != nrow(icons2) | 
    !identical(icons2$icon_id, seq(1:nrow(icons2))) ) { break }
  
  
  icons2 %>%
    mutate(icon_x_exact_point = ((img_x - x_tile_trans_left) / 256) +
             square_x,
           icon_y_exact_point = ((img_y - y_tile_trans_up) / 256) + 
             square_y) %>% 
    
    cbind( with(.,
                num2deg(tile_zoom,
                        icon_x_exact_point, icon_y_exact_point) %>% 
                  mutate_all(as.character))
    ) %>% 
    
    select( -c(contains('within'), j) ) %>% 
    
    write_csv(.,
              path = out_dir %p% '/' %p%
                str_remove_all(copy, 'HTMLs/|.txt') %p% '.csv')
  
  } #-> out_foreach

# stopCluster(cl)


list.files('Icons_to_tiles', full.names = T,
           recursive = T, pattern = '.csv') %>% length() ==
(file.info(list.files('HTMLs/', full.names = T))$size > 40000) %>% sum()



setdiff(list.files('HTMLs/', full.names = T)[
  file.info(list.files('HTMLs/', full.names = T))$size > 40000] %>% 
  str_remove_all(., '.txt|^.*/'),
    list.files('Icons_to_tiles',
      recursive = T, pattern = '.csv') %>% str_remove_all('.csv|^.*/'))


# lapply(list.files('Icons_to_tiles', full.names = T), R.utils::gzip )





