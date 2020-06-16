library(DBI)
source("read_clippings.R")
clippings_dir = "e:\\Users\\savvi\\Documents\\backup\\personal-data\\My Clippings.txt"
db_dir = "clippings.sqlite"
recreate_db = FALSE
clippings = read_kindle_clippings(clippings_dir)%>% mutate(date = as.character(date))
con <- dbConnect(RSQLite::SQLite(), db_dir)
if (recreate_db) {
  dbRemoveTable(con, "clippings")
  dbCreateTable(con, "clippings", clippings)
}
dbWriteTable(con, "clippings", clippings, overwrite=TRUE)
dbDisconnect(con)

# Query for sampling a random clipping
# dbGetQuery(con, 'SELECT * FROM clippings ORDER BY RANDOM() LIMIT 1')
