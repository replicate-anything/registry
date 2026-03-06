library(yaml)
library(jsonlite)


get_doi_metadata <- function(doi){
  
  library(httr)
  library(jsonlite)
  
  url <- paste0("https://doi.org/", doi)
  
  res <- httr::GET(
    url,
    httr::add_headers(
      "Accept" = "application/vnd.citationstyles.csl+json"
    )
  )
  
  if (httr::status_code(res) != 200) {
    stop("DOI metadata not available")
  }
  
  txt <- httr::content(res, "text", encoding = "UTF-8")
  
  meta <- jsonlite::fromJSON(txt)
  
  authors <- paste(meta$author$given, meta$author$family)
  
  list(
    title = meta$title,
    journal = meta$`container-title`,
    year = meta$issued$`date-parts`[[1]][1],
    authors = authors
  )
}



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
  
  paper <- meta$paper
  
  # Resolve authors
  if(is.null(paper$authors)){
    
    meta_doi <- get_doi_metadata(paper$doi)
    
    authors <- paste(meta_doi$authors, collapse = ", ")
    
  } else {
    
    authors <- paste(paper$authors, collapse = ", ")
    
  }
  
  data.frame(
    doi = paper$doi,
    title = paper$title,
    authors = authors,
    year = paper$year,
    journal = paper$journal,
    repo = "replicate-anything/registry",
    stringsAsFactors = FALSE
  )
  
})

records <- Filter(Negate(is.null), records)

records <- do.call(rbind, records)

write_json(
  records,
  "index.json",
  pretty = TRUE,
  auto_unbox = TRUE
)

cat("Index rebuilt successfully\n")