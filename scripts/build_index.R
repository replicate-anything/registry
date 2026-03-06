library(yaml)
library(jsonlite)

papers_dir <- "papers"

folders <- list.dirs(
  papers_dir,
  recursive = FALSE,
  full.names = TRUE
)

records <- lapply(folders, function(folder){
  
  yml_file <- file.path(folder, "replication.yml")
  
  if(!file.exists(yml_file)){
    return(NULL)
  }
  
  meta <- yaml::read_yaml(yml_file)
  
  data.frame(
    doi = meta$paper$doi,
    title = meta$paper$title,
    authors = paste(meta$paper$authors, collapse = ", "),
    year = meta$paper$year,
    journal = meta$paper$journal,
    repo = "replicate-anything/registry",
    stringsAsFactors = FALSE
  )
  
})

records <- do.call(rbind, records)

write_json(
  records,
  "index.json",
  pretty = TRUE,
  auto_unbox = TRUE
)

cat("Index rebuilt successfully\n")