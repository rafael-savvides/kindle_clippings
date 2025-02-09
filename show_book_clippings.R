# Prints highlights from books in Kindle (from "My Clippings.txt")
# Usage: Rscript show_book_clippings.R bookname file_out path_to_clippings
# - If bookname is empty, it prints titles of all books and number of highlights.
# - If path_to_clippings is empty, it defaults to a path (see path_to_clippings below).

args = commandArgs(trailingOnly = TRUE)
book = args[1]
path_to_output = args[2]
path_to_clippings = args[3]

source("read_clippings.R") |>
  suppressPackageStartupMessages() |>
  suppressWarnings()
path_to_clippings = if (is.na(path_to_clippings)) readLines("path_to_clippings.txt") else path_to_clippings
path_to_clippings = normalizePath(path_to_clippings)

path_to_cached_csv = "clippings_df_cached.csv"
if (!file.exists(path_to_cached_csv) |
  max(file.info(path_to_clippings)$mtime) > file.info(path_to_cached_csv)$mtime) {
  cat("Updating cached csv.\n")
  clippings = read_kindle_clippings(path_to_clippings) |> suppressWarnings()
  write.csv(clippings, path_to_cached_csv)
} else {
  clippings = read.csv(path_to_cached_csv, stringsAsFactors = FALSE)
}

if (is.na(book) || book == "") {
  # Print book titles
  clippings |>
    group_by(title) |>
    summarize(
      Notes = n(),
      Latest = max(date)
    ) |>
    select(Notes, Latest, Book = title) |>
    write.table(quote = FALSE, row.names = FALSE, sep = "\t", fileEncoding = "UTF-8")
} else {
  if (!is.na(path_to_output)) {
    print_clippings(clippings, book, file = path_to_output, wrap = FALSE)
  } else {
    print_clippings(clippings, book)
  }
}
