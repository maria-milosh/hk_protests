#!/usr/local/bin/Rscript



# Setup -------------------------------------------------------------------

pacman::p_load(tidyverse, wrapr, RSQLite, DBI, lubridate)
setwd("~")
setwd("Dropbox/HK")

options(digits = 17)

set.seed(124)


db <- DBI::dbConnect(RSQLite::SQLite(), dbname = 'Events_db/icons_to_events.db')

# load('Cached/milestone_january.Rdata')

# -------------------------------------------------------------------------


set.seed(124)

for (y in 2020) {
  for (mon in c('02') ) {
    for (d in 1:31) {
      for (hr in 0:23) {
        for (mnt in 0:59) {
          
          
          pot_path <- 'Icons_to_tiles/' %p%
            paste(y, mon, str_pad(d, 2, 'l', 0), sep = '-') %p% '/' %p%
            paste(y, mon, str_pad(d, 2, 'l', 0), str_pad(hr, 2, 'l', 0),
                  str_pad(mnt, 2, 'l', 0), sep = '-') %p% '.csv'
          
          if( pot_path %>%
              file.exists() ) {
            
            print(str_extract(pot_path, '\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}'))
            
            read_csv(pot_path, col_types = cols()) %>%
              mutate(date = str_extract(pot_path, '\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}')) -> csv
            if ( nrow(csv) == 0 ) { next }
            csv %>%
              select(icon_type, icon_opacity, date, lat_deg, lon_deg) -> csv
            
            anti_join(csv, ref_base,
                      by = c("icon_type", "lat_deg", "lon_deg")) -> new_icons
            
            new_icons %>%
              mutate(event = round(runif(n = nrow(.), 0, 2^30)) ) %>%
              select(-c(date, icon_opacity)) -> new_icons
            
            
            if ( length(unique(new_icons$event)) != nrow(new_icons) ) { break }
            
            
            ref_base %>%
              
              anti_join( # delete gone icons
                anti_join(ref_base, csv,
                          by = c("icon_type", "lat_deg", "lon_deg")),
                by = 'event'
              ) %>%
              
              rbind( # add new icons
                new_icons
              ) -> ref_base
            
            dbWriteTable(conn = db, name = 'Icons', append = T,
                         value = left_join(csv, ref_base,
                                           by = c("icon_type", "lat_deg", "lon_deg"))
            )
            
          }
        }
      }
    }
  }
}

dbDisconnect(db)



