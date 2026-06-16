# Helpers for self-contained replication scripts (no replicateEverything required).

REGISTRY_REPO <- "replicate-anything/registry"
REGISTRY_BRANCH <- "main"

paper_folder_name <- function(paper_path) {
  basename(normalizePath(paper_path, winslash = "/", mustWork = FALSE))
}

github_paper_url <- function(paper_path) {
  folder <- paper_folder_name(paper_path)
  paste0(
    "https://github.com/", REGISTRY_REPO, "/tree/", REGISTRY_BRANCH, "/papers/", folder
  )
}

collect_dependencies <- function(meta, rep) {
  deps <- c(meta$paper$dependencies, rep$dependencies)
  deps <- unique(as.character(unlist(deps, use.names = FALSE)))
  deps <- deps[nzchar(deps)]
  deps
}

make_header_lines <- function(meta, rep, paper_path, code_file) {
  paper <- meta$paper
  title <- paper$title %||% "Replication"
  rep_label <- rep$label %||% rep$description %||% rep$id
  script_name <- basename(code_file)
  c(
    paste0("# ", rep_label, " — ", title),
    paste0("# Paper folder: ", github_paper_url(paper_path)),
    paste0("# Run from the paper's code/ folder: Rscript ", script_name),
    ""
  )
}

make_library_block <- function(deps) {
  if (length(deps) == 0) {
    return(character(0))
  }
  c(paste0("library(", deps, ")"), "")
}

data_path <- function(path) {
  file.path("..", path)
}

read_one_expr <- function(path) {
  p <- data_path(path)
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    csv = sprintf('utils::read.csv("%s", stringsAsFactors = FALSE)', p),
    rds = sprintf('readRDS("%s")', p),
    dta = sprintf('haven::read_dta("%s")', p),
    sprintf('stop("Unsupported data format: %s")', path)
  )
}

make_data_expr <- function(data_paths) {
  if (is.null(data_paths) || length(data_paths) == 0) {
    return("NULL")
  }
  if (is.list(data_paths)) {
    data_paths <- unlist(data_paths, use.names = FALSE)
  }
  data_paths <- as.character(data_paths)
  data_paths <- data_paths[nzchar(data_paths)]
  if (length(data_paths) == 0) {
    return("NULL")
  }
  if (length(data_paths) == 1) {
    return(read_one_expr(data_paths[[1]]))
  }
  loaders <- vapply(data_paths, read_one_expr, character(1))
  names <- tools::file_path_sans_ext(basename(data_paths))
  paste0(
    "list(\n  ",
    paste(paste0(names, " = ", loaders), collapse = ",\n  "),
    "\n)"
  )
}

format_function_name <- function(rep) {
  if (is.null(rep$format) || !nzchar(as.character(rep$format[[1]]))) {
    return(NULL)
  }
  fmt <- as.character(rep$format[[1]])
  if (grepl("[/\\\\]", fmt) || grepl("\\.R$", fmt, ignore.case = TRUE)) {
    paste0("format_", gsub("[^a-zA-Z0-9_]", "_", rep$id))
  } else {
    fmt
  }
}

make_run_line <- function(meta, rep, make_name) {
  data_expr <- make_data_expr(rep$data)
  fmt_name <- format_function_name(rep)
  if (!is.null(fmt_name)) {
    return(paste0(make_name, "(", data_expr, ") |> ", fmt_name, "()"))
  }
  paste0(make_name, "(", data_expr, ")")
}

strip_existing_header <- function(lines) {
  strip_existing_preamble(lines)
}

strip_existing_preamble <- function(lines) {
  while (length(lines)) {
    line <- trimws(lines[[1]])
    if (line == "") {
      lines <- lines[-1]
      next
    }
    if (grepl("^#", line)) {
      lines <- lines[-1]
      next
    }
    if (grepl("^library\\(", line)) {
      lines <- lines[-1]
      next
    }
    if (grepl("^suppressPackageStartupMessages", line)) {
      end <- 1L
      while (end <= length(lines) && !grepl("^\\}\\)$", trimws(lines[[end]]))) {
        end <- end + 1L
      }
      lines <- lines[-seq_len(min(end, length(lines)))]
      next
    }
    break
  }
  lines
}

strip_existing_footer <- function(lines) {
  idx <- grep("^if \\(!isTRUE\\(getOption\\(", lines)
  if (length(idx)) {
    lines <- lines[seq_len(idx[[1]] - 1L)]
  }
  idx <- grep("^\\} else \\{$", lines)
  if (length(idx)) {
    lines <- lines[seq_len(idx[[1]] - 1L)]
  }
  idx <- grep("^suppressPackageStartupMessages\\(", lines)
  if (length(idx) && idx[[1]] > 1) {
    lines <- lines[seq_len(idx[[1]] - 1L)]
  }
  idx <- grep("^(make_|generate_)[a-zA-Z0-9_]+\\(", lines)
  if (length(idx)) {
    last_run <- idx[[length(idx)]]
    if (last_run > 1 && !grepl("<- function", lines[[last_run]])) {
      lines <- lines[seq_len(last_run - 1L)]
    }
  }
  idx <- grep("^generate_(table|figure) <- make_", lines)
  if (length(idx)) {
    lines <- lines[seq_len(idx[[1]] - 1L)]
  }
  while (length(lines) && trimws(lines[[length(lines)]]) == "") {
    lines <- lines[-length(lines)]
  }
  lines
}

`%||%` <- function(a, b) if (is.null(a)) b else a
