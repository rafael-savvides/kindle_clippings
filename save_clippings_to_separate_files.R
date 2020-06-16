source("read_clippings.R")
clippings_dir = "e:\\Users\\savvi\\Documents\\backup\\personal-data\\My Clippings.txt"
note_dir = "clippings/"

if (file.info(clippings_dir)$mtime > min(file.info(list.files(note_dir, full.names = TRUE))$mtime)) {
  clippings = read_kindle_clippings(clippings_dir)
  notes = clippings %>% 
    mutate(note = pmap(list(title, author, location, date, body), ~paste0(..1, " - ", ..2, " (", ..3, ")\n", ..4, "\n\n", ..5))) %>% 
    pull(note) %>% 
    unlist()
  
  if (!dir.exists(note_dir))
    dir.create(note_dir)
  note_filenames = sapply(1:length(notes), function(i) paste0(note_dir, "note", sprintf("%04d", i),".txt"))
  for (i in seq_along(notes)) 
    writeLines(notes[i], note_filenames[i])
} else {
  cat("Clipping notes are up to date.")
}
