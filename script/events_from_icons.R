#!/usr/local/bin/Rscript



# Setup -------------------------------------------------------------------

pacman::p_load(tidyverse, wrapr, RSQLite, DBI, lubridate)

setwd("/Users/mariamilosh/Dropbox/HK")

options(digits = 17)

set.seed(124)


db <- DBI::dbConnect(RSQLite::SQLite(), dbname = 'Events_db/icons_to_events.db')



# -------------------------------------------------------------------------


set.seed(124)

for (y in 2019:2020) {
  for (mon in c('11', '12', '01') ) {
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


            if ( str_extract(pot_path, '\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}') %>%
                 str_detect('2019-11-13-15-17') ) { # the 1st record

              csv %>%
                mutate(event = round(runif(n = nrow(.), 0, 2^30)) ) %>%
                select(-c(date, icon_opacity)) -> ref_base

              if ( length(unique(ref_base$event)) != nrow(ref_base) ) { break }

              dbWriteTable(conn = db, name = 'Icons', append = T,
                           value = left_join(csv, ref_base,
                                             by = c("icon_type", "lat_deg", "lon_deg")) )
              next }


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

save(ref_base, file = 'Cached/milestone_january.Rdata')



# Cached ------------------------------------------------------------------

# 
# progresslag <- 0
# 
# 
# 
# for (day in list.files('Icons_to_tiles/',  recursive = T, pattern = '.csv')) {
#   # day = list.files('Icons_to_tiles/', recursive = T, pattern = '.csv')[2]
#   
#   
#   progress <- str_extract(day, '\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}') %>% 
#     ymd_hm() %>% 
#     format('%Y-%m-%d')
#   if (progress != progresslag) { print(progress) }
# 
#   
#   
#   read_csv('Icons_to_tiles/' %p% day, col_types = cols()) %>% 
#     mutate(date = str_remove(day, '.csv')) -> csv
#   
#   if ( nrow(csv) == 0 ) { next }
#   
#   csv %>%
#     select(icon_type, icon_opacity, date, lat_deg, lon_deg) -> csv
#   
#   
#   
#   if ( str_detect(day, '2019-11-13-15-17') ) { # the 1st record
#     
#       csv %>% 
#         mutate(event = round(runif(n = nrow(.), 0, 2^30)) ) %>% 
#         select(-c(date, icon_opacity)) -> ref_base
#       
#       if ( length(unique(ref_base$event)) != nrow(ref_base) ) { break }
#       
#       dbWriteTable(conn = db, name = 'Icons', append = T,
#                    value = left_join(csv, ref_base,
#                                      by = c("icon_type", "lat_deg", "lon_deg"))
#       )
#       next }
#   
#   
#   anti_join(csv, ref_base,
#               by = c("icon_type", "lat_deg", "lon_deg")) -> new_icons
#   
#   new_icons %>% 
#     mutate(event = round(runif(n = nrow(.), 0, 2^30)) ) %>% 
#     select(-c(date, icon_opacity)) -> new_icons
#   
#   
#   if ( length(unique(new_icons$event)) != nrow(new_icons) ) { break }
#   
#   
#   ref_base %>% 
#     
#     anti_join( # delete gone icons
#       anti_join(ref_base, csv, 
#                 by = c("icon_type", "lat_deg", "lon_deg")),
#       by = 'event'
#     ) %>% 
#     
#     rbind( # add new icons
#       new_icons
#     ) -> ref_base
#   
#   rm(new_icons)
#   
#   dbWriteTable(conn = db, name = 'Icons', append = T,
#                value = left_join(csv, ref_base,
#                                  by = c("icon_type", "lat_deg", "lon_deg"))
#   )
#   
#   
#   progresslag <- progress
# 
#   }
#   


# dbListTables(db)
# dbListFields(db, "Icons")
# dbRemoveTable(db, "Icons")
# 
# res <- dbSendQuery(db, "SELECT date FROM Icons")
# cookc <- dbFetch(res)
# dbClearResult(res)

# dbGetRowCount(db)



# setdiff(list.files('Icons_to_tiles', full.names = T,
#            recursive = T, pattern = '.csv') %>%
#   str_remove('.*/'), list.files("Events_db/") )
# 
# 








