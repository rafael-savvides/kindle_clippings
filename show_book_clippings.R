# Prints highlights from books in Kindle (from "My Clippings.txt")
# Usage: Rscript show_book_clippings.R bookname file_out path_to_my_clippings
# - If bookname is empty, it prints titles of all books and number of highlights.
# - If path_to_my_clippings is empty, it defaults to a path (see FILE_CLIPPINGS below).

args = commandArgs(trailingOnly = T)
book = args[1]
FILE_OUT = args[2]
FILE_CLIPPINGS = args[3]

suppressWarnings(suppressPackageStartupMessages(source("read_clippings.R")))
if (is.na(FILE_CLIPPINGS))
  FILE_CLIPPINGS = "e:\\Users\\savvi\\Documents\\backup\\personal-data\\My Clippings.txt"
# Note: MLaPP had some weird clippings from images that made readLines stop reading, so I deleted them but they may come out again.
FILE_CLIPPINGS = normalizePath(FILE_CLIPPINGS)
clippings_df_cached_dir = "clippings_df_cached.csv"
  
if (!file.exists(clippings_df_cached_dir) | 
    file.info(FILE_CLIPPINGS)$mtime > file.info(clippings_df_cached_dir)$mtime) {
  cat("Updating clippings_df_cached.csv...\n")
  clippings = suppressWarnings(read_kindle_clippings(FILE_CLIPPINGS))
  write.csv(clippings, clippings_df_cached_dir)
} else {
  clippings = read.csv(clippings_df_cached_dir, stringsAsFactors = FALSE)
}

if (is.na(book)) {
  # Print book titles
  clippings %>% 
    group_by(title) %>% 
    summarize(Notes = n(), 
              # Latest = format(max(date), "%Y-%m-%d")) %>% 
              Latest = max(date)) %>% 
    select(Notes, Latest, Book = title) %>% 
    write.table(quote=F, row.names=F, sep="\t", fileEncoding = "UTF-8") 
} else {
  if (!is.na(FILE_OUT)) {
    print_clippings(clippings, book, file=FILE_OUT, wrap=FALSE)
  } else {
    print_clippings(clippings, book)
  }
}
