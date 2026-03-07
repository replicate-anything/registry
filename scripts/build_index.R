library(yaml)
library(jsonlite)

normalize_doi <- function(doi){
  doi <- tolower(doi)
  doi <- gsub("^https?://doi.org/", "", doi)
  trimws(doi)
}

papers_dir <- "papers"

folders <- list.dirs(
  papers_dir,
  recursive = FALSE,
  full.names = TRUE
)

records <- list()

for(folder in folders){
  
  yml_file <- file.path(folder, "replication.yml")
  
  if(!file.exists(yml_file)){
    message("Skipping: no replication.yml in ", folder)
    next
  }
  
  meta <- tryCatch(
    yaml::read_yaml(yml_file),
    error = function(e){
      message("Skipping invalid YAML: ", folder)
      return(NULL)
    }
  )
  
  if(is.null(meta)) next
  
  paper <- meta$paper
  
  if(is.null(paper$doi)){
    message("Skipping: DOI missing in ", folder)
    next
  }
  
  doi <- normalize_doi(paper$doi)
  
  authors <- if(!is.null(paper$authors)){
    paste(paper$authors, collapse = ", ")
  } else {
    ""
  }
  
  record <- data.frame(
    doi = doi,
    title = paper$title %||% "",
    authors = authors,
    year = paper$year %||% NA,
    journal = paper$journal %||% "",
    repo = "replicate-anything/registry",
    stringsAsFactors = FALSE
  )
  
  records[[length(records)+1]] <- record
}

if(length(records) > 0){
  index <- do.call(rbind, records)
} else {
  index <- data.frame()
}

index <- unique(index)

write_json(
  index,
  "index.json",
  pretty = TRUE,
  auto_unbox = TRUE
)

cat("Index rebuilt successfully\n")