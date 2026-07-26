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
    paper$article_url %||% paper$landing_url %||% paper$publisher_url %||% ""
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
    related_upstream = "",
    related_downstream = "",
    stringsAsFactors = FALSE
  )
}

normalize_repo_slug <- function(repo) {
  repo <- trimws(as.character(repo %||% ""))
  repo <- sub("^https?://github.com/", "", repo, ignore.case = TRUE)
  sub("\\.git$", "", repo)
}

upstream_keys_from_meta <- function(meta, index) {
  refs <- list()
  paper <- meta$paper %||% list()
  extends <- paper$extends %||% meta$extends %||% NULL
  if (is.list(extends) && length(extends) > 0L) {
    refs[[length(refs) + 1L]] <- extends
  }
  related <- paper$related %||% meta$related %||% NULL
  if (is.list(related) && length(related) > 0L) {
    if (!is.null(names(related)) && any(c("doi", "repo") %in% names(related)) && !is.list(related[[1]])) {
      related <- list(related)
    }
    for (item in related) {
      if (is.list(item)) refs[[length(refs) + 1L]] <- item
    }
  }
  keys <- character(0)
  for (ref in refs) {
    doi_raw <- trimws(as.character(ref$doi[[1]] %||% ref$doi %||% ""))
    if (nzchar(doi_raw)) {
      doi_norm <- normalize_doi(doi_raw)
      index_dois <- vapply(as.character(index$doi), function(x) {
        x <- trimws(x)
        if (!nzchar(x)) NA_character_ else normalize_doi(x)
      }, character(1))
      hit <- which(!is.na(index_dois) & index_dois == doi_norm)
      if (length(hit)) {
        row <- index[hit[[1]], , drop = FALSE]
        key <- if (nzchar(trimws(as.character(row$doi[[1]])))) {
          normalize_doi(row$doi[[1]])
        } else {
          as.character(row$handle[[1]] %||% row$folder[[1]])
        }
        keys <- c(keys, key)
        next
      }
      keys <- c(keys, doi_norm)
      next
    }
    repo_raw <- normalize_repo_slug(ref$repo[[1]] %||% ref$repo %||% "")
    if (nzchar(repo_raw)) {
      repos <- vapply(as.character(index$repo), normalize_repo_slug, character(1))
      hit <- which(tolower(repos) == tolower(repo_raw))
      if (length(hit)) {
        row <- index[hit[[1]], , drop = FALSE]
        key <- if (nzchar(trimws(as.character(row$doi[[1]])))) {
          normalize_doi(row$doi[[1]])
        } else {
          as.character(row$handle[[1]] %||% row$folder[[1]])
        }
        keys <- c(keys, key)
      }
    }
  }
  unique(keys[nzchar(keys)])
}

annotate_related <- function(index, metas) {
  n <- nrow(index)
  upstream <- rep("", n)
  down_lists <- vector("list", n)
  for (i in seq_len(n)) {
    keys <- upstream_keys_from_meta(metas[[i]], index)
    upstream[i] <- paste(keys, collapse = "|")
    self_key <- if (nzchar(trimws(as.character(index$doi[[i]])))) {
      normalize_doi(index$doi[[i]])
    } else {
      as.character(index$handle[[i]] %||% index$folder[[i]])
    }
    for (uk in keys) {
      index_dois <- vapply(as.character(index$doi), function(x) {
        x <- trimws(x)
        if (!nzchar(x)) NA_character_ else normalize_doi(x)
      }, character(1))
      handles <- tolower(as.character(index$handle))
      hit <- which(
        (!is.na(index_dois) & index_dois == uk) |
          handles == tolower(uk)
      )
      if (length(hit) && hit[[1]] != i) {
        down_lists[[hit[[1]]]] <- c(down_lists[[hit[[1]]]], self_key)
      }
    }
  }
  index$related_upstream <- upstream
  index$related_downstream <- vapply(seq_len(n), function(i) {
    paste(unique(down_lists[[i]]), collapse = "|")
  }, character(1))
  index
}

studies_dir <- file.path(registry_root, "studies")
yml_files <- list.files(studies_dir, pattern = "\\.yml$", full.names = TRUE)
if (length(yml_files) == 0L) {
  stop("No study stubs in ", studies_dir)
}

metas <- lapply(yml_files, yaml::read_yaml)
rows <- lapply(seq_along(yml_files), function(i) {
  folder <- sub("\\.yml$", "", basename(yml_files[[i]]))
  row <- row_from_meta(metas[[i]])
  row$folder <- folder
  if (!nzchar(row$handle)) {
    row$handle <- folder
  }
  row
})
index <- do.call(rbind, rows)
index <- annotate_related(index, metas)
ord <- order(index$title, index$year, index$folder)
index <- index[ord, , drop = FALSE]

index_path <- file.path(registry_root, "index.csv")
utils::write.csv(index, index_path, row.names = FALSE)
message("Wrote ", index_path, " (", nrow(index), " studies)")
