# Prints highlights from books in Kindle (from "My Clippings.txt")
# Usage: Rscript show_book_clippings.R bookname path_to_my_clippings
# - If bookname is empty, it prints titles of all books and number of highlights.
# - If path_to_my_clippings is empty, it defaults to a path (see clippings_dir below).

args = commandArgs(trailingOnly = T)
suppressWarnings(suppressPackageStartupMessages(source("read_clippings.R")))

clippings_dir = args[2]
if (is.na(clippings_dir))
  clippings_dir = "e:\\Users\\savvi\\Documents\\backup\\personal-data\\My Clippings.txt"

clippings_dir = normalizePath(clippings_dir)
clippings_df_cached_dir = "clippings_df_cached.csv"
  
if (!file.exists(clippings_df_cached_dir) | 
    file.info(clippings_dir)$mtime > file.info(clippings_df_cached_dir)$mtime) {
  cat("Updating clippings_df_cached.csv...\n")
  clippings = suppressWarnings(read_kindle_clippings(clippings_dir))
  write.csv(clippings, clippings_df_cached_dir)
} else {
  clippings = read.csv(clippings_df_cached_dir)
}

book = args[1]
# if (is.na(book)) 
#   stop("Provide book name.")

if (is.na(book)) {
  # Print book titles
  clippings %>% 
    count(title )%>%
    select(Notes = n, Book = title) %>% 
    write.table(quote=F, row.names=F, sep="\t") 
} else {
  print_clippings(clippings, book, preview="raw")
}
