library(stringr)
library(dplyr)
library(purrr)
library(flextable)
library(officer)
library(knitr)

#' Read Kindle clippings into data frame
#' 
#' Reads Kindle clippings into tidy data frame. 
#' * Removes duplicates and almost duplicates (from re-highlighting a passage).
#' * Takes author name from content of parentheses in the title or filename. May be incorrect.
#'
#' @param fname filename. Location of "My Clippings.txt" file from Kindle
#'
#' @return data frame of title, author, body, location, date
#' @export
#' @md
#'
#' @examples
#' clippings = read_kindle_clippings("Clippings.txt")
#' print_clippings(clippings, "Thinking, Fast and Slow")
read_kindle_clippings <- function(fname) {
  clippings_raw = readLines(fname, encoding = "UTF-8")
  
  clippings = data.frame(raw = clippings_raw, stringsAsFactors = F) %>% 
    mutate(is_sep = str_detect(raw, "=========="), 
           is_title_author = lag(is_sep, default=T), 
           is_location_date = lag(is_title_author, default=F), 
           is_body = !is_sep & !is_title_author & !is_location_date, 
           id = cumsum(is_title_author)) %>% 
    mutate(title_author = map2(raw, is_title_author, function(x, y) x[y]), 
           location_date = map2(raw, is_location_date, function(x, y) x[y]), 
           body = map2(raw, is_body, function(x, y) x[y])) %>% 
    select(id, title_author, location_date, body) %>% 
    group_by(id) %>% 
    summarise(title_author = paste0(unlist(title_author), collapse=""), 
              location_date = paste0(unlist(location_date), collapse=""), 
              body = paste0(unlist(body), collapse="")) %>% 
    filter(!str_detect(location_date, "- Your Bookmark on ")) %>%
    mutate(location_date = str_remove(location_date, "- Your Highlight on "),
           location = str_remove(location_date, " \\|.*"),
           date = str_extract(location_date, "\\| Added on.*"),
           date = str_remove(date, "\\| Added on "),
           date = lubridate::parse_date_time(date, "A, b! d!, Y! I!:M!:S! p!"),
           author = str_extract(title_author, "\\(\\D*\\)"),
           author = str_remove_all(author, "\\(|\\)"),
           title = str_remove(title_author, " \\(.*")
    ) %>%
    select(title, author, body, location, date) %>% 
    filter(!duplicated(body)) %>% 
    filter(!lead(str_detect(body, fixed(lag(body))))) # Almost duplicates from when a passage is re-highlighted.
  clippings
}


#' Prints the clippings from a book in the Viewer pane
#'
#' @param clippings A data frame of Kindle clippings. See [read_kindle_clippings()].
#' @param book Name of book to display.
#' @param preview Where to display result. 
#' One of: 
#' * "html": RStudio Viewer pane
#' * "docx": MS Word
#' * "pptx": MS Powerpoint
#' * "raw": Console
#'
#' @return display data frame in Viewer pane or a MS Word document
#' @export
#'
#' @examples
#' print_clippings(clippings, "Thinking, Fast and Slow")
print_clippings <- function(clippings, book, preview = "html") {
  stopifnot(c("title", "body", "date") %in% names(clippings))
  book_clippings = clippings %>% 
    filter(str_detect(str_to_lower(title), str_to_lower(book))) %>% 
    arrange(date) %>% 
    mutate(body = str_wrap(body, 80), 
           body = paste0(body, "\n"))

  if (preview == "raw") {
    book_clippings %>% 
      select(date, location, body) %>% 
      write.table(quote=F, row.names=F, col.names=F, sep="\n")
  } else {
    book_title = str_squish(book_clippings$title[1])
    book_dates = paste0("Read from ", paste0(as.Date(range(book_clippings$date, na.rm = T)), collapse = " to "))
    table_header = paste0(book_title, "\n", book_dates)
    
    book_clippings %>% 
    select(location, body) %>%  
    flextable::flextable() %>% 
    flextable::set_header_labels(location = "", body = table_header) %>% 
    flextable::width(j=1, width=0.5) %>% 
    flextable::width(j=2, width=7) %>% 
    flextable::align_text_col("left") %>% 
    flextable::fontsize(j=1, size = 6) %>% 
    flextable::border(j=2, border.top = officer::fp_border()) %>% 
    print(preview = preview)
  }
}
