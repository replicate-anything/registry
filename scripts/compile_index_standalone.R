#!/usr/bin/env Rscript
# Compile index.csv from studies/*.yml without installing replicateEverything.

args <- commandArgs(trailingOnly = TRUE)
registry_root <- if (length(args) >= 1L && nzchar(args[[1]])) {
  normalizePath(args[[1]], winslash = "/", mustWork = FALSE)
} else {
  normalizePath(file.path(".."), winslash = "/", mustWork = FALSE)
}

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Install the yaml package to compile the registry index.")
}

normalize_doi <- function(doi) {
  doi <- tolower(trimws(as.character(doi)))
  doi <- gsub("^https?://doi.org/", "", doi)
  doi <- gsub("^doi:", "", doi)
  trimws(doi)
}

doi_to_folder <- function(doi) {
  gsub("/", "", normalize_doi(doi))
}

registry_folder_from_paper <- function(paper, folder_fallback = NULL) {
  handle <- trimws(as.character(
    paper$study_handle %||% paper$study_folder %||% paper$handle %||% ""
  ))
  if (nzchar(handle[[1]] %||% handle)) {
    return(as.character(handle[[1]] %||% handle))
  }
  doi_val <- paper$doi %||% NULL
  if (!is.null(doi_val)) {
    doi_chr <- trimws(as.character(doi_val[[1]] %||% doi_val))
    if (nzchar(doi_chr)) {
      return(doi_to_folder(doi_chr))
    }
  }
  folder_fallback %||% stop("paper needs doi or study_handle")
}

row_from_meta <- function(meta) {
  paper <- meta$paper
  authors <- paper$authors %||% ""
  if (length(authors) > 1L) {
    authors <- paste(authors, collapse = ", ")
  } else {
    authors <- as.character(authors[[1]] %||% "")
  }
  folder <- registry_folder_from_paper(paper, folder_fallback = "")
  handle <- trimws(as.character(paper$handle %||% paper$study_handle %||% folder))
  if (!nzchar(handle)) {
    handle <- folder
  }
  doi_val <- paper$doi %||% NULL
  doi_out <- if (!is.null(doi_val) && nzchar(trimws(as.character(doi_val[[1]] %||% doi_val)))) {
    normalize_doi(doi_val)
  } else {
    ""
  }
  collections <- meta$collections %||% paper$collections %||% character(0)
  collections <- unique(na.omit(as.character(unlist(collections, use.names = FALSE))))
  collections <- paste(collections[nzchar(collections)], collapse = "|")
  maintainer <- meta$maintainer %||% list()
  maintainer_name <- as.character(maintainer$name %||% "")
  maintainer_email <- as.character(maintainer$email %||% "")
  languages <- meta$languages %||% paper$languages %||% character(0)
  languages <- unique(na.omit(as.character(unlist(languages, use.names = FALSE))))
  languages <- paste(languages[nzchar(languages)], collapse = ";")
  article_url <- as.character(
    paper$article_url %||% paper$landing_url %||% paper$study_url %||% ""
  )
  repo <- as.character(
    meta$repo %||% paper$study_repo %||% paper$package_repo %||% ""
  )
  data.frame(
    folder = folder,
    handle = handle,
    doi = doi_out,
    title = as.character(paper$title[[1]] %||% paper$title %||% ""),
    journal = as.character(paper$journal %||% ""),
    year = as.integer(paper$year %||% NA_integer_),
    authors = authors,
    repo = repo,
    collections = collections,
    maintainer_name = maintainer_name,
    maintainer_email = maintainer_email,
    languages = languages,
    article_url = article_url,
    stringsAsFactors = FALSE
  )
}

studies_dir <- file.path(registry_root, "studies")
yml_files <- list.files(studies_dir, pattern = "\\.yml$", full.names = TRUE)
if (length(yml_files) == 0L) {
  stop("No study stubs in ", studies_dir)
}

rows <- lapply(yml_files, function(path) {
  folder <- sub("\\.yml$", "", basename(path))
  meta <- yaml::read_yaml(path)
  row <- row_from_meta(meta)
  row$folder <- folder
  if (!nzchar(row$handle)) {
    row$handle <- folder
  }
  row
})
index <- do.call(rbind, rows)
ord <- order(index$title, index$year, index$folder)
index <- index[ord, , drop = FALSE]

index_path <- file.path(registry_root, "index.csv")
utils::write.csv(index, index_path, row.names = FALSE)
message("Wrote ", index_path, " (", nrow(index), " studies)")
