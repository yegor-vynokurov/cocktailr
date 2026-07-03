# semantic_layer_indicators.R
#
# Operation 2: semantic extraction from plant indicator-value datasets.
#
# External sources expected in:
#   data-raw/external/eive_1_0/EIVE_Paper_1.0_SM_08.xlsx
#   data-raw/external/ellenberg_tichy_2023/
#     Indicator.values-tables-2022-11-07-Zenodo.v2.xlsx
#
# Persistent local cache:
#   cache/semantic_layer/
#
# The code deliberately uses the package root, not getwd(), so running R from
# D:/documents/coctrailr does not write files outside the cocktailr project.

.require_semantic_packages <- function() {
  packages <- c(
    "dplyr",
    "tidyr",
    "purrr",
    "tibble",
    "rprojroot"
  )

  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing) > 0L) {
    stop(
      "Install the required packages first: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!requireNamespace("readxl", quietly = TRUE) &&
      !requireNamespace("xml2", quietly = TRUE)) {
    stop(
      "Semantic indicator enrichment requires either `readxl` ",
      "(preferred) or `xml2` (fallback workbook reader).",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.semantic_excel_backend <- function() {
  if (requireNamespace("readxl", quietly = TRUE)) {
    return("readxl")
  }

  if (requireNamespace("xml2", quietly = TRUE)) {
    return("xml2")
  }

  stop(
    "No Excel backend available. Install `readxl` or `xml2`.",
    call. = FALSE
  )
}


.xlsx_col_index <- function(ref) {
  letters <- gsub("[^A-Z]", "", toupper(as.character(ref)))

  if (!nzchar(letters)) {
    return(NA_integer_)
  }

  values <- utf8ToInt(letters) - utf8ToInt("A") + 1L
  out <- 0L

  for (value in values) {
    out <- out * 26L + value
  }

  out
}


.xlsx_row_index <- function(ref) {
  digits <- gsub("[^0-9]", "", as.character(ref))

  if (!nzchar(digits)) {
    return(NA_integer_)
  }

  as.integer(digits)
}


.xlsx_resolve_target_path <- function(base_dir, target) {
  target <- sub("^/+", "", as.character(target))
  normalizePath(file.path(base_dir, target), winslash = "/", mustWork = TRUE)
}


.xlsx_workbook_bundle <- function(path) {
  tmp_dir <- tempfile("cocktailr_xlsx_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(path, exdir = tmp_dir)

  main_ns <- c(
    x = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
  )
  rel_ns <- c(
    r = "http://schemas.openxmlformats.org/package/2006/relationships"
  )
  workbook_path <- file.path(tmp_dir, "xl", "workbook.xml")
  rels_path <- file.path(tmp_dir, "xl", "_rels", "workbook.xml.rels")

  workbook <- xml2::read_xml(workbook_path)
  rels <- xml2::read_xml(rels_path)

  sheet_nodes <- xml2::xml_find_all(
    workbook,
    ".//x:sheets/x:sheet",
    ns = main_ns
  )
  rel_nodes <- xml2::xml_find_all(
    rels,
    ".//r:Relationship",
    ns = rel_ns
  )

  rel_map <- stats::setNames(
    xml2::xml_attr(rel_nodes, "Target"),
    xml2::xml_attr(rel_nodes, "Id")
  )

  sheet_names <- xml2::xml_attr(sheet_nodes, "name")
  sheet_rids <- xml2::xml_attr(sheet_nodes, "id")
  sheet_targets <- unname(rel_map[sheet_rids])

  names(sheet_targets) <- sheet_names

  shared_strings_path <- file.path(tmp_dir, "xl", "sharedStrings.xml")
  shared_strings <- character(0)

  if (file.exists(shared_strings_path)) {
    shared_doc <- xml2::read_xml(shared_strings_path)
    string_nodes <- xml2::xml_find_all(shared_doc, ".//x:si", ns = main_ns)
    shared_strings <- vapply(
      string_nodes,
      function(node) {
        text_nodes <- xml2::xml_find_all(node, ".//x:t", ns = main_ns)
        paste(xml2::xml_text(text_nodes), collapse = "")
      },
      character(1)
    )
  }

  list(
    tmp_dir = tmp_dir,
    main_ns = main_ns,
    sheet_targets = sheet_targets,
    shared_strings = shared_strings
  )
}


.xlsx_cell_text <- function(cell_node, shared_strings, main_ns) {
  cell_type <- xml2::xml_attr(cell_node, "t")

  if (identical(cell_type, "inlineStr")) {
    text_nodes <- xml2::xml_find_all(cell_node, ".//x:t", ns = main_ns)
    return(paste(xml2::xml_text(text_nodes), collapse = ""))
  }

  value_node <- xml2::xml_find_first(cell_node, "./x:v", ns = main_ns)

  if (inherits(value_node, "xml_missing")) {
    return("")
  }

  value <- xml2::xml_text(value_node)

  if (identical(cell_type, "s")) {
    index <- suppressWarnings(as.integer(value))

    if (is.na(index) || (index + 1L) > length(shared_strings)) {
      return("")
    }

    return(shared_strings[[index + 1L]])
  }

  value
}


.xlsx_sheet_matrix <- function(path, sheet_name) {
  bundle <- .xlsx_workbook_bundle(path)
  on.exit(unlink(bundle$tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  if (!sheet_name %in% names(bundle$sheet_targets)) {
    stop("Sheet not found in workbook: ", sheet_name, call. = FALSE)
  }

  sheet_path <- .xlsx_resolve_target_path(
    file.path(bundle$tmp_dir, "xl"),
    bundle$sheet_targets[[sheet_name]]
  )
  sheet_doc <- xml2::read_xml(sheet_path)

  row_nodes <- xml2::xml_find_all(
    sheet_doc,
    ".//x:sheetData/x:row",
    ns = bundle$main_ns
  )

  if (!length(row_nodes)) {
    return(matrix(character(0), nrow = 0L, ncol = 0L))
  }

  row_ids <- suppressWarnings(as.integer(xml2::xml_attr(row_nodes, "r")))
  row_ids[is.na(row_ids)] <- seq_along(row_nodes)[is.na(row_ids)]

  cell_nodes <- xml2::xml_find_all(
    sheet_doc,
    ".//x:sheetData/x:row/x:c",
    ns = bundle$main_ns
  )

  if (!length(cell_nodes)) {
    return(matrix(character(0), nrow = max(row_ids), ncol = 0L))
  }

  refs <- xml2::xml_attr(cell_nodes, "r")
  col_ids <- vapply(refs, .xlsx_col_index, integer(1))
  cell_row_ids <- vapply(refs, .xlsx_row_index, integer(1))
  values <- vapply(
    cell_nodes,
    .xlsx_cell_text,
    character(1),
    shared_strings = bundle$shared_strings,
    main_ns = bundle$main_ns
  )

  n_rows <- max(cell_row_ids, na.rm = TRUE)
  n_cols <- max(col_ids, na.rm = TRUE)
  out <- matrix("", nrow = n_rows, ncol = n_cols)

  valid <- is.finite(cell_row_ids) & is.finite(col_ids)
  out[cbind(cell_row_ids[valid], col_ids[valid])] <- values[valid]
  out
}


.xml2_read_excel <- function(
    path,
    sheet,
    col_names = TRUE,
    n_max = Inf,
    skip = 0L,
    .name_repair = "minimal"
) {
  mat <- .xlsx_sheet_matrix(path, sheet_name = sheet)

  if (nrow(mat) == 0L) {
    return(data.frame())
  }

  skip <- max(0L, as.integer(skip %||% 0L))

  if (skip > 0L) {
    if (skip >= nrow(mat)) {
      return(data.frame())
    }
    mat <- mat[(skip + 1L):nrow(mat), , drop = FALSE]
  }

  if (is.finite(n_max)) {
    mat <- mat[seq_len(min(nrow(mat), as.integer(n_max))), , drop = FALSE]
  }

  if (nrow(mat) == 0L) {
    return(data.frame())
  }

  if (isTRUE(col_names)) {
    header <- as.character(mat[1L, , drop = TRUE])
    body <- if (nrow(mat) > 1L) {
      mat[-1L, , drop = FALSE]
    } else {
      matrix("", nrow = 0L, ncol = ncol(mat))
    }

    out <- as.data.frame(body, stringsAsFactors = FALSE, check.names = FALSE)
    names(out) <- header
    return(out)
  }

  as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
}


.semantic_excel_sheets <- function(path) {
  backend <- .semantic_excel_backend()

  if (identical(backend, "readxl")) {
    return(readxl::excel_sheets(path))
  }

  bundle <- .xlsx_workbook_bundle(path)
  on.exit(unlink(bundle$tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  names(bundle$sheet_targets)
}


.semantic_read_excel <- function(
    path,
    sheet,
    col_names = TRUE,
    n_max = Inf,
    skip = 0L,
    .name_repair = "minimal"
) {
  backend <- .semantic_excel_backend()

  if (identical(backend, "readxl")) {
    return(suppressMessages(
      readxl::read_excel(
        path,
        sheet = sheet,
        col_names = col_names,
        n_max = n_max,
        skip = skip,
        .name_repair = .name_repair
      )
    ))
  }

  .xml2_read_excel(
    path = path,
    sheet = sheet,
    col_names = col_names,
    n_max = n_max,
    skip = skip,
    .name_repair = .name_repair
  )
}


#' Find the cocktailr project root safely
#'
#' The function supports four situations:
#' 1. COCKTAILR_PROJECT_ROOT is set;
#' 2. the current directory is the project root;
#' 3. the current directory is the parent folder containing cocktailr/;
#' 4. the code is run from a subdirectory inside the project.
#'
#' @param start Directory from which root discovery should start.
#' @return Normalized absolute path to the cocktailr project root.
#' @export
cocktailr_project_root <- function(start = getwd()) {
  .require_semantic_packages()

  is_project_root <- function(path) {
    nzchar(path) &&
      dir.exists(path) &&
      file.exists(file.path(path, "cocktailr.Rproj")) &&
      file.exists(file.path(path, "DESCRIPTION"))
  }

  env_root <- Sys.getenv("COCKTAILR_PROJECT_ROOT", unset = "")

  direct_candidates <- unique(c(
    env_root,
    start,
    file.path(start, "cocktailr"),
    "D:/documents/coctrailr/cocktailr"
  ))

  direct_candidates <- direct_candidates[nzchar(direct_candidates)]

  for (candidate in direct_candidates) {
    if (is_project_root(candidate)) {
      return(normalizePath(
        candidate,
        winslash = "/",
        mustWork = TRUE
      ))
    }
  }

  discovered <- tryCatch(
    rprojroot::find_root(
      criterion = rprojroot::has_file("cocktailr.Rproj"),
      path = start
    ),
    error = function(e) NULL
  )

  if (!is.null(discovered) && is_project_root(discovered)) {
    return(normalizePath(
      discovered,
      winslash = "/",
      mustWork = TRUE
    ))
  }

  stop(
    paste0(
      "Could not find the cocktailr project root.\n",
      "Expected project: D:/documents/coctrailr/cocktailr\n",
      "You can also set:\n",
      "Sys.setenv(COCKTAILR_PROJECT_ROOT = ",
      "\"D:/documents/coctrailr/cocktailr\")"
    ),
    call. = FALSE
  )
}


semantic_layer_paths <- function(
    root = cocktailr_project_root()
) {
  cache_root <- file.path(root, "cache", "semantic_layer")

  paths <- list(
    root = root,

    eive_file = file.path(
      root,
      "data-raw",
      "external",
      "eive_1_0",
      "EIVE_Paper_1.0_SM_08.xlsx"
    ),

    tichy_file = file.path(
      root,
      "data-raw",
      "external",
      "ellenberg_tichy_2023",
      "Indicator.values-tables-2022-11-07-Zenodo.v2.xlsx"
    ),

    alias_file = file.path(
      root,
      "data-raw",
      "external",
      "species_aliases.csv"
    ),

    cache_root = cache_root,
    reference_dir = file.path(cache_root, "reference"),
    species_dir = file.path(cache_root, "species"),
    results_dir = file.path(cache_root, "results"),

    reference_cache = file.path(
      cache_root,
      "reference",
      "indicator_reference.rds"
    ),

    species_cache = file.path(
      cache_root,
      "species",
      "species_indicator_lookup.rds"
    )
  )

  dir.create(
    paths$reference_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  dir.create(
    paths$species_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  dir.create(
    paths$results_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  paths
}


.check_source_files <- function(paths) {
  missing <- c(
    paths$eive_file,
    paths$tichy_file
  )

  missing <- missing[!file.exists(missing)]

  if (length(missing) > 0L) {
    stop(
      "External indicator files were not found:\n",
      paste0("  - ", missing, collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.clean_names_simple <- function(x) {
  x <- as.character(x)
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  x <- trimws(x)
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)

  blank <- !nzchar(x)
  x[blank] <- paste0("x_", which(blank))

  make.unique(x, sep = "_")
}


.as_numeric_safe <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  x <- trimws(as.character(x))
  x <- gsub(",", ".", x, fixed = TRUE)

  suppressWarnings(as.numeric(x))
}


.mean_or_na <- function(x) {
  x <- .as_numeric_safe(x)

  if (all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}


.first_non_missing <- function(x) {
  x <- x[!is.na(x) & nzchar(trimws(as.character(x)))]

  if (length(x) == 0L) {
    return(NA_character_)
  }

  as.character(x[[1L]])
}


.taxon_key <- function(x) {
  x <- as.character(x)
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  x <- tolower(x)
  x <- gsub("[×x]", " ", x)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}


.binomial_key <- function(x) {
  key <- .taxon_key(x)

  vapply(
    strsplit(key, "\\s+"),
    function(parts) {
      parts <- parts[nzchar(parts)]

      if (length(parts) >= 2L) {
        paste(parts[1:2], collapse = " ")
      } else {
        paste(parts, collapse = " ")
      }
    },
    character(1)
  )
}


.pick_column <- function(
    column_names,
    patterns,
    label,
    required = TRUE
) {
  for (pattern in patterns) {
    hits <- grep(
      pattern,
      column_names,
      value = TRUE,
      perl = TRUE
    )

    if (length(hits) > 0L) {
      return(hits[[1L]])
    }
  }

  if (required) {
    stop(
      "Could not detect column for ", label, ".\n",
      "Available columns:\n",
      paste(column_names, collapse = ", "),
      call. = FALSE
    )
  }

  NA_character_
}


.header_score <- function(values, source_type, sheet_name) {
  values <- tolower(trimws(as.character(values)))
  values <- values[!is.na(values) & nzchar(values)]

  if (length(values) == 0L) {
    return(-Inf)
  }

  joined <- paste(values, collapse = " | ")

  has_taxon <- any(grepl(
    "taxon|species|accepted|euro.?med",
    values
  ))

  if (!has_taxon) {
    return(-Inf)
  }

  if (identical(source_type, "eive")) {
    axes <- c("m", "n", "r", "l", "t")

    axis_score <- sum(vapply(
      axes,
      function(axis) {
        grepl(
          paste0("eive.*[^a-z0-9]", axis, "([^a-z]|$)"),
          joined,
          perl = TRUE
        )
      },
      logical(1)
    ))

    return(4 * axis_score + 3)
  }

  if (identical(source_type, "tichy")) {
    long_axes <- c(
      "light",
      "temperature",
      "moisture",
      "reaction",
      "nutrient",
      "salinity"
    )

    short_axes <- c("l", "t", "m", "r", "n", "s")

    axis_score <- sum(vapply(
      long_axes,
      function(axis) grepl(axis, joined, fixed = TRUE),
      logical(1)
    ))

    axis_score <- axis_score + sum(values %in% short_axes)

    sheet_bonus <- if (
      grepl(
        "final|harmon|europe|summary",
        tolower(sheet_name)
      )
    ) {
      4
    } else {
      0
    }

    return(3 * axis_score + 3 + sheet_bonus)
  }

  -Inf
}


.trim_excel_table <- function(data, chosen_sheet, chosen_skip) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  names(data) <- .clean_names_simple(names(data))

  nonempty_columns <- vapply(
    data,
    function(x) any(!is.na(x) & nzchar(trimws(as.character(x)))),
    logical(1)
  )

  data <- data[, nonempty_columns, drop = FALSE]

  if (nrow(data)) {
    nonempty_rows <- apply(
      data,
      1,
      function(x) any(!is.na(x) & nzchar(trimws(as.character(x))))
    )

    data <- data[nonempty_rows, , drop = FALSE]
  }

  attr(data, "source_sheet") <- chosen_sheet
  attr(data, "source_skip") <- chosen_skip
  data
}


.read_excel_table_auto <- function(
    path,
    source_type,
    sheet = NULL,
    skip = NULL
) {
  sheets <- if (is.null(sheet)) {
    .semantic_excel_sheets(path)
  } else {
    sheet
  }

  if (!is.null(skip)) {
    chosen_sheet <- sheets[[1L]]
    chosen_skip <- skip
  } else {
    preferred_sheet <- NULL

    if (identical(source_type, "eive")) {
      preferred_hits <- sheets[grepl("^main[_ -]?table$", tolower(sheets))]
      if (length(preferred_hits)) {
        preferred_sheet <- preferred_hits[[1L]]
      }
    }

    if (!is.null(preferred_sheet)) {
      chosen_sheet <- preferred_sheet
      chosen_skip <- 0L
    } else {
    best <- list(
      score = -Inf,
      sheet = NA_character_,
      header_row = NA_integer_
    )

    for (sheet_name in sheets) {
      preview <- tryCatch(
        .semantic_read_excel(
          path,
          sheet = sheet_name,
          col_names = FALSE,
          n_max = 30,
          .name_repair = "minimal"
        ),
        error = function(e) NULL
      )

      if (is.null(preview) || nrow(preview) == 0L) {
        next
      }

      for (row_index in seq_len(nrow(preview))) {
        values <- unlist(
          preview[row_index, , drop = FALSE],
          use.names = FALSE
        )

        score <- .header_score(
          values = values,
          source_type = source_type,
          sheet_name = sheet_name
        )

        if (is.finite(score) && score > best$score) {
          best <- list(
            score = score,
            sheet = sheet_name,
            header_row = row_index
          )
        }
      }
    }

    if (!is.finite(best$score)) {
      stop(
        "Could not automatically find a data table in:\n",
        path,
        "\nSheets: ",
        paste(sheets, collapse = ", "),
        "\nRun inspect_indicator_workbooks() and pass sheet/skip manually.",
        call. = FALSE
      )
    }

      chosen_sheet <- best$sheet
      chosen_skip <- best$header_row - 1L
    }
  }

  data <- .semantic_read_excel(
    path,
    sheet = chosen_sheet,
    skip = chosen_skip,
    .name_repair = "minimal"
  )
  .trim_excel_table(data, chosen_sheet, chosen_skip)
}


#' Inspect workbook sheets and candidate column names
#'
#' Use this helper only if automatic detection fails.
#'
#' @param root cocktailr project root.
#' @return An invisible list with sheet previews.
#' @export
inspect_indicator_workbooks <- function(
    root = cocktailr_project_root()
) {
  .require_semantic_packages()

  paths <- semantic_layer_paths(root)
  .check_source_files(paths)

  files <- c(
    eive = paths$eive_file,
    tichy = paths$tichy_file
  )

  result <- lapply(names(files), function(source_name) {
    path <- files[[source_name]]
    sheets <- .semantic_excel_sheets(path)

    cat("\n", toupper(source_name), "\n", sep = "")
    cat("File: ", path, "\n", sep = "")
    cat("Sheets:\n")
    print(sheets)

    previews <- lapply(sheets, function(sheet_name) {
      data <- tryCatch(
        .semantic_read_excel(
          path,
          sheet = sheet_name,
          n_max = 5,
          .name_repair = "minimal"
        ),
        error = function(e) NULL
      )

      if (!is.null(data)) {
        cat("\nSheet: ", sheet_name, "\n", sep = "")
        print(names(data))
      }

      data
    })

    names(previews) <- sheets
    previews
  })

  names(result) <- names(files)
  invisible(result)
}


.read_eive_sm08 <- function(
    path,
    sheet = NULL,
    skip = NULL
) {
  raw <- .read_excel_table_auto(
    path = path,
    source_type = "eive",
    sheet = sheet,
    skip = skip
  )

  taxon_col <- .pick_column(
    names(raw),
    patterns = c(
      "^taxonconcept$",
      "^accepted_taxon_name$",
      "^accepted_name$",
      "^taxon_name$",
      "^taxon$",
      "^species_name$",
      "^species$",
      "accepted.*taxon",
      "accepted.*name"
    ),
    label = "EIVE accepted taxon name"
  )

  rank_col <- .pick_column(
    names(raw),
    patterns = c(
      "^taxon_rank$",
      "^rank$"
    ),
    label = "EIVE taxon rank",
    required = FALSE
  )

  source_col <- .pick_column(
    names(raw),
    patterns = c(
      "^accordingto$",
      "^source_of_taxon_concept$",
      "^taxon_concept_source$",
      "^source$"
    ),
    label = "EIVE taxon source",
    required = FALSE
  )

  out <- tibble::tibble(
    eive_name = as.character(raw[[taxon_col]]),
    taxon_key = .taxon_key(raw[[taxon_col]]),
    eive_rank = if (!is.na(rank_col)) {
      as.character(raw[[rank_col]])
    } else {
      NA_character_
    },
    eive_taxon_source = if (!is.na(source_col)) {
      as.character(raw[[source_col]])
    } else {
      NA_character_
    }
  )

  for (axis in c("m", "n", "r", "l", "t")) {
    position_col <- .pick_column(
      names(raw),
      patterns = c(
        paste0("^eiveres_", axis, "$"),
        paste0("^eive_", axis, "$")
      ),
      label = paste0("EIVE-", toupper(axis))
    )

    niche_width_col <- .pick_column(
      names(raw),
      patterns = c(
        paste0("^eiveres_", axis, "_nw3$"),
        paste0("^eive_", axis, "_nw$")
      ),
      label = paste0("EIVE-", toupper(axis), ".nw")
    )

    source_count_col <- .pick_column(
      names(raw),
      patterns = c(
        paste0("^eiveres_", axis, "_n$"),
        paste0("^eive_", axis, "_n$")
      ),
      label = paste0("EIVE-", toupper(axis), ".n")
    )

    out[[paste0("eive_", axis)]] <-
      .as_numeric_safe(raw[[position_col]])

    out[[paste0("eive_", axis, "_nw")]] <-
      .as_numeric_safe(raw[[niche_width_col]])

    out[[paste0("eive_", axis, "_n")]] <-
      .as_numeric_safe(raw[[source_count_col]])
  }

  numeric_columns <- unlist(
    lapply(
      c("m", "n", "r", "l", "t"),
      function(axis) {
        c(
          paste0("eive_", axis),
          paste0("eive_", axis, "_nw"),
          paste0("eive_", axis, "_n")
        )
      }
    ),
    use.names = FALSE
  )

  out |>
    dplyr::filter(nzchar(.data$taxon_key)) |>
    dplyr::group_by(.data$taxon_key) |>
    dplyr::summarise(
      eive_name = .first_non_missing(.data$eive_name),
      eive_rank = .first_non_missing(.data$eive_rank),
      eive_taxon_source = .first_non_missing(
        .data$eive_taxon_source
      ),
      dplyr::across(
        dplyr::all_of(numeric_columns),
        .mean_or_na
      ),
      .groups = "drop"
    )
}


.tichy_to_0_10 <- function(x, axis) {
  x <- .as_numeric_safe(x)

  scaled <- switch(
    axis,

    # Nine-degree scales: 1 ... 9
    l = (x - 1) / 8 * 10,
    r = (x - 1) / 8 * 10,
    n = (x - 1) / 8 * 10,

    # Twelve-degree scales: 1 ... 12
    t = (x - 1) / 11 * 10,
    m = (x - 1) / 11 * 10,

    # Ten-degree salinity scale: 0 ... 9
    s = x / 9 * 10,

    stop("Unknown Tichy axis: ", axis, call. = FALSE)
  )

  pmax(0, pmin(10, scaled))
}


.read_tichy_harmonized <- function(
    path,
    sheet = NULL,
    skip = NULL,
    axis_columns = NULL
) {
  if (is.null(sheet) && is.null(skip)) {
    sheets <- .semantic_excel_sheets(path)
    preferred_sheet <- sheets[
      grepl("tab[-_]?ivs[-_]?tichy", tolower(sheets))
    ]

    if (length(preferred_sheet)) {
      preview <- .semantic_read_excel(
        path,
        sheet = preferred_sheet[[1L]],
        col_names = FALSE,
        .name_repair = "minimal"
      )
      preview <- as.data.frame(preview, stringsAsFactors = FALSE, check.names = FALSE)

      if (nrow(preview) >= 3L) {
        header_top <- trimws(as.character(preview[1L, , drop = TRUE]))
        header_bottom <- trimws(as.character(preview[2L, , drop = TRUE]))
        axis_hits <- toupper(header_top) %in% c(
          "LIGHT", "TEMPERATURE", "MOISTURE", "REACTION", "NUTRIENTS", "SALINITY"
        )

        header <- ifelse(nzchar(header_bottom), header_bottom, header_top)
        header[axis_hits] <- header_top[axis_hits]

        body <- preview[-c(1L, 2L), , drop = FALSE]
        names(body) <- header
        raw <- .trim_excel_table(
          body,
          chosen_sheet = preferred_sheet[[1L]],
          chosen_skip = 0L
        )
      } else {
        raw <- .read_excel_table_auto(
          path = path,
          source_type = "tichy",
          sheet = sheet,
          skip = skip
        )
      }
    } else {
      raw <- .read_excel_table_auto(
        path = path,
        source_type = "tichy",
        sheet = sheet,
        skip = skip
      )
    }
  } else {
    raw <- .read_excel_table_auto(
      path = path,
      source_type = "tichy",
      sheet = sheet,
      skip = skip
    )
  }

  taxon_col <- .pick_column(
    names(raw),
    patterns = c(
      "^accepted_taxon_name$",
      "^accepted_name$",
      "^taxon_name$",
      "^species_name$",
      "^species$",
      "^taxon$",
      "euro.*med.*name",
      "accepted.*name"
    ),
    label = "Tichy accepted species name"
  )

  default_patterns <- list(
    l = c(
      "^l$",
      "^light$",
      "^eiv_l$",
      "^final_l$",
      "^harmonized_l$",
      "^l_mean$",
      "^mean_l$",
      "^light_mean$"
    ),
    t = c(
      "^t$",
      "^temperature$",
      "^eiv_t$",
      "^final_t$",
      "^harmonized_t$",
      "^t_mean$",
      "^mean_t$",
      "^temperature_mean$"
    ),
    m = c(
      "^m$",
      "^moisture$",
      "^eiv_m$",
      "^final_m$",
      "^harmonized_m$",
      "^m_mean$",
      "^mean_m$",
      "^moisture_mean$"
    ),
    r = c(
      "^r$",
      "^reaction$",
      "^eiv_r$",
      "^final_r$",
      "^harmonized_r$",
      "^r_mean$",
      "^mean_r$",
      "^reaction_mean$"
    ),
    n = c(
      "^n$",
      "^nutrients$",
      "^nutrient$",
      "^eiv_n$",
      "^final_n$",
      "^harmonized_n$",
      "^n_mean$",
      "^mean_n$",
      "^nutrient_mean$"
    ),
    s = c(
      "^s$",
      "^salinity$",
      "^eiv_s$",
      "^final_s$",
      "^harmonized_s$",
      "^s_mean$",
      "^mean_s$",
      "^salinity_mean$"
    )
  )

  selected_columns <- setNames(
    rep(NA_character_, 6L),
    c("l", "t", "m", "r", "n", "s")
  )

  for (axis in names(selected_columns)) {
    if (
      !is.null(axis_columns) &&
      axis %in% names(axis_columns) &&
      nzchar(axis_columns[[axis]])
    ) {
      selected_columns[[axis]] <- axis_columns[[axis]]
    } else {
      selected_columns[[axis]] <- .pick_column(
        names(raw),
        patterns = default_patterns[[axis]],
        label = paste0("Tichy ", toupper(axis)),
        required = axis != "s"
      )
    }
  }

  out <- tibble::tibble(
    tichy_name = as.character(raw[[taxon_col]]),
    taxon_key = .taxon_key(raw[[taxon_col]])
  )

  for (axis in names(selected_columns)) {
    column <- selected_columns[[axis]]

    raw_values <- if (!is.na(column)) {
      .as_numeric_safe(raw[[column]])
    } else {
      rep(NA_real_, nrow(raw))
    }

    out[[paste0("tichy_", axis, "_raw")]] <- raw_values
    out[[paste0("tichy_", axis)]] <-
      .tichy_to_0_10(raw_values, axis)
  }

  numeric_columns <- unlist(
    lapply(
      c("l", "t", "m", "r", "n", "s"),
      function(axis) {
        c(
          paste0("tichy_", axis, "_raw"),
          paste0("tichy_", axis)
        )
      }
    ),
    use.names = FALSE
  )

  out |>
    dplyr::filter(nzchar(.data$taxon_key)) |>
    dplyr::group_by(.data$taxon_key) |>
    dplyr::summarise(
      tichy_name = .first_non_missing(.data$tichy_name),
      dplyr::across(
        dplyr::all_of(numeric_columns),
        .mean_or_na
      ),
      .groups = "drop"
    )
}


.source_signature <- function(paths) {
  hashes <- tools::md5sum(c(
    paths$eive_file,
    paths$tichy_file
  ))

  paste(
    names(hashes),
    unname(hashes),
    sep = "=",
    collapse = "|"
  )
}


#' Build or load the combined EIVE/Tichy reference table
#'
#' EIVE is used as the primary source for M, N, R, L and T.
#' Tichy values are rescaled to 0-10 and used as a gap-filling source.
#' Salinity is taken from Tichy because EIVE 1.0 does not include S.
#'
#' @param root cocktailr project root.
#' @param force Re-read both Excel files even when the cache is valid.
#' @param eive_sheet,eive_skip Optional manual EIVE sheet and skipped rows.
#' @param tichy_sheet,tichy_skip Optional manual Tichy sheet and skipped rows.
#' @param tichy_axis_columns Optional named vector for manual column mapping,
#'   e.g. c(l = "L", t = "T", m = "M", r = "R", n = "N", s = "S").
#' @return List containing data, metadata and paths.
#' @export
build_indicator_reference <- function(
    root = cocktailr_project_root(),
    force = FALSE,
    eive_sheet = NULL,
    eive_skip = NULL,
    tichy_sheet = NULL,
    tichy_skip = NULL,
    tichy_axis_columns = NULL
) {
  .require_semantic_packages()

  paths <- semantic_layer_paths(root)
  .check_source_files(paths)

  signature <- .source_signature(paths)

  if (!force && file.exists(paths$reference_cache)) {
    cached <- readRDS(paths$reference_cache)

    if (
      is.list(cached) &&
      identical(cached$source_signature, signature)
    ) {
      return(cached)
    }
  }

  eive <- .read_eive_sm08(
    path = paths$eive_file,
    sheet = eive_sheet,
    skip = eive_skip
  )

  tichy <- .read_tichy_harmonized(
    path = paths$tichy_file,
    sheet = tichy_sheet,
    skip = tichy_skip,
    axis_columns = tichy_axis_columns
  )

  reference <- dplyr::full_join(
    eive,
    tichy,
    by = "taxon_key"
  ) |>
    dplyr::mutate(
      reference_name = dplyr::coalesce(
        .data$eive_name,
        .data$tichy_name
      ),
      binomial_key = .binomial_key(.data$reference_name)
    ) |>
    dplyr::arrange(.data$reference_name)

  result <- list(
    source_signature = signature,
    created_at = Sys.time(),
    data = reference,
    source_rows = list(
      eive = nrow(eive),
      tichy = nrow(tichy),
      combined = nrow(reference)
    ),
    paths = paths
  )

  saveRDS(result, paths$reference_cache)
  result
}


.default_species_aliases <- function() {
  tibble::tibble(
    input_name = "Lychnis flos-cuculi",
    accepted_name = "Silene flos-cuculi",
    note = "World Flora Online accepted-name mapping"
  )
}


.load_species_aliases <- function(paths) {
  if (!file.exists(paths$alias_file)) {
    # Missing aliases should not modify the source tree during an ordinary
    # labeling run. Use a tiny in-memory default instead.
    return(.default_species_aliases())
  }

  aliases <- utils::read.csv(
    paths$alias_file,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )

  required <- c("input_name", "accepted_name")

  if (!all(required %in% names(aliases))) {
    stop(
      "Alias file must contain columns: ",
      paste(required, collapse = ", "),
      "\nFile: ",
      paths$alias_file,
      call. = FALSE
    )
  }

  tibble::as_tibble(aliases)
}


.alias_signature <- function(aliases) {
  pairs <- paste(
    aliases$input_name,
    aliases$accepted_name,
    sep = "=>"
  )

  paste(sort(pairs), collapse = "|")
}


.match_species_to_reference <- function(
    species,
    reference,
    aliases
) {
  species <- unique(as.character(species))

  alias_map <- stats::setNames(
    aliases$accepted_name,
    aliases$input_name
  )

  lookup_name <- ifelse(
    species %in% names(alias_map),
    unname(alias_map[species]),
    species
  )

  input_key <- .taxon_key(species)
  lookup_key <- .taxon_key(lookup_name)

  exact_index <- match(
    lookup_key,
    reference$taxon_key
  )

  # Fallback to the original name when an alias did not match.
  original_index <- match(
    input_key,
    reference$taxon_key
  )

  use_original <- is.na(exact_index) & !is.na(original_index)
  exact_index[use_original] <- original_index[use_original]

  match_method <- ifelse(
    !is.na(exact_index) & lookup_name != species,
    "manual_alias",
    ifelse(!is.na(exact_index), "exact_name", "unmatched")
  )

  # Conservative binomial fallback only when the binomial is unique
  # in the reference table.
  unresolved <- which(is.na(exact_index))

  if (length(unresolved) > 0L) {
    reference_binomial_counts <- table(reference$binomial_key)
    unique_binomials <- names(
      reference_binomial_counts[
        reference_binomial_counts == 1L
      ]
    )

    input_binomial <- .binomial_key(lookup_name)
    fallback_allowed <- input_binomial %in% unique_binomials

    fallback_index <- match(
      input_binomial,
      reference$binomial_key
    )

    use_fallback <- unresolved[
      fallback_allowed[unresolved] &
        !is.na(fallback_index[unresolved])
    ]

    exact_index[use_fallback] <- fallback_index[use_fallback]
    match_method[use_fallback] <- "unique_binomial"
  }

  matched_reference <- reference[
    exact_index,
    setdiff(
      names(reference),
      c("taxon_key", "binomial_key")
    ),
    drop = FALSE
  ]

  dplyr::bind_cols(
    tibble::tibble(
      input_species = species,
      lookup_name = lookup_name,
      input_taxon_key = input_key,
      lookup_taxon_key = lookup_key,
      match_method = match_method
    ),
    tibble::as_tibble(matched_reference)
  )
}


#' Get cached indicator data for a set of species
#'
#' Only species absent from the persistent species cache are matched again.
#'
#' @param species Character vector of species names.
#' @param root cocktailr project root.
#' @param force Refresh every requested species.
#' @param force_reference Force re-reading of the two Excel files.
#' @param ... Passed to build_indicator_reference().
#' @return A tibble with source values and matching diagnostics.
#' @export
get_species_indicator_lookup <- function(
    species,
    root = cocktailr_project_root(),
    force = FALSE,
    force_reference = FALSE,
    ...
) {
  .require_semantic_packages()

  paths <- semantic_layer_paths(root)
  reference_object <- build_indicator_reference(
    root = root,
    force = force_reference,
    ...
  )

  aliases <- .load_species_aliases(paths)

  cache_signature <- paste(
    reference_object$source_signature,
    .alias_signature(aliases),
    sep = "||"
  )

  cached_data <- tibble::tibble()

  if (!force && file.exists(paths$species_cache)) {
    cached <- readRDS(paths$species_cache)

    if (
      is.list(cached) &&
      identical(cached$cache_signature, cache_signature)
    ) {
      cached_data <- cached$data
    }
  }

  requested <- unique(as.character(species))

  missing_species <- if (
    force ||
      nrow(cached_data) == 0L ||
      !"input_species" %in% names(cached_data)
  ) {
    requested
  } else {
    setdiff(requested, cached_data$input_species)
  }

  if (length(missing_species) > 0L) {
    new_matches <- .match_species_to_reference(
      species = missing_species,
      reference = reference_object$data,
      aliases = aliases
    )

    cached_data <- dplyr::bind_rows(
      cached_data,
      new_matches
    ) |>
      dplyr::distinct(
        .data$input_species,
        .keep_all = TRUE
      ) |>
      dplyr::arrange(.data$input_species)

    saveRDS(
      list(
        cache_signature = cache_signature,
        created_at = Sys.time(),
        data = cached_data
      ),
      paths$species_cache
    )
  }

  cached_data |>
    dplyr::filter(.data$input_species %in% requested) |>
    dplyr::arrange(match(.data$input_species, requested))
}


.max_or_na <- function(x) {
  x <- .as_numeric_safe(x)

  if (all(is.na(x))) {
    return(NA_real_)
  }

  max(x, na.rm = TRUE)
}


.extract_cluster_species <- function(
    x,
    clusters,
    min_phi = NULL
) {
  args <- list(
    x = x,
    labels = clusters,
    species_cluster_phi = TRUE
  )

  if (!is.null(min_phi)) {
    args$min_phi <- min_phi
  }

  cluster_list <- do.call(
    species_in_clusters,
    args
  )

  if (is.null(names(cluster_list))) {
    names(cluster_list) <- clusters
  }

  result <- purrr::imap_dfr(
    cluster_list,
    function(item, cluster_name) {
      if (is.null(item)) {
        return(tibble::tibble())
      }

      if (is.character(item)) {
        return(tibble::tibble(
          cluster = cluster_name,
          species = item,
          phi = NA_real_
        ))
      }

      item <- as.data.frame(
        item,
        stringsAsFactors = FALSE
      )
      names(item) <- .clean_names_simple(names(item))

      species_col <- .pick_column(
        names(item),
        patterns = c(
          "^species$",
          "^taxon$",
          "^name$"
        ),
        label = "cluster species"
      )

      phi_col <- .pick_column(
        names(item),
        patterns = c(
          "^phi$",
          "species.*phi",
          "phi.*species"
        ),
        label = "species-cluster phi",
        required = FALSE
      )

      tibble::tibble(
        cluster = cluster_name,
        species = as.character(item[[species_col]]),
        phi = if (!is.na(phi_col)) {
          .as_numeric_safe(item[[phi_col]])
        } else {
          NA_real_
        }
      )
    }
  )

  result |>
    dplyr::filter(
      !is.na(.data$species),
      nzchar(.data$species)
    ) |>
    dplyr::group_by(
      .data$cluster,
      .data$species
    ) |>
    dplyr::summarise(
      phi = .max_or_na(.data$phi),
      .groups = "drop"
    )
}


.weighted_median <- function(x, w) {
  valid <- is.finite(x) & is.finite(w) & w > 0
  x <- x[valid]
  w <- w[valid]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  order_index <- order(x)
  x <- x[order_index]
  w <- w[order_index]

  cumulative <- cumsum(w) / sum(w)
  x[which(cumulative >= 0.5)[1L]]
}


.axis_band <- function(axis, score) {
  if (!is.finite(score)) {
    return(NA_character_)
  }

  labels <- switch(
    axis,
    m = c(
      "very_dry",
      "dry",
      "mesic",
      "moist",
      "wet"
    ),
    n = c(
      "very_nutrient_poor",
      "nutrient_poor",
      "intermediate_nutrients",
      "nutrient_rich",
      "very_nutrient_rich"
    ),
    r = c(
      "strongly_acidic",
      "acidic",
      "subneutral",
      "base_rich",
      "strongly_base_rich"
    ),
    l = c(
      "deep_shade",
      "shade",
      "semi_open",
      "bright",
      "full_light"
    ),
    t = c(
      "very_cold",
      "cool",
      "temperate",
      "warm",
      "very_warm"
    ),
    s = c(
      "non_or_very_low_saline",
      "low_salinity",
      "moderate_salinity",
      "high_salinity",
      "very_high_salinity"
    ),
    stop("Unknown axis: ", axis, call. = FALSE)
  )

  as.character(cut(
    score,
    breaks = c(-Inf, 2, 4, 6, 8, Inf),
    labels = labels,
    right = FALSE
  ))
}


.build_species_axis_evidence <- function(
    cluster_species,
    lookup
) {
  joined <- cluster_species |>
    dplyr::left_join(
      lookup,
      by = c("species" = "input_species")
    )

  purrr::map_dfr(
    c("m", "n", "r", "l", "t", "s"),
    function(axis) {
      row_count <- nrow(joined)

      eive_value_col <- paste0("eive_", axis)
      eive_nw_col <- paste0("eive_", axis, "_nw")
      eive_n_col <- paste0("eive_", axis, "_n")
      tichy_value_col <- paste0("tichy_", axis)

      eive_value <- if (eive_value_col %in% names(joined)) {
        joined[[eive_value_col]]
      } else {
        rep(NA_real_, row_count)
      }

      eive_nw <- if (eive_nw_col %in% names(joined)) {
        joined[[eive_nw_col]]
      } else {
        rep(NA_real_, row_count)
      }

      eive_n <- if (eive_n_col %in% names(joined)) {
        joined[[eive_n_col]]
      } else {
        rep(NA_real_, row_count)
      }

      tichy_value <- if (tichy_value_col %in% names(joined)) {
        joined[[tichy_value_col]]
      } else {
        rep(NA_real_, row_count)
      }

      # EIVE is primary. Tichy is used only when EIVE is absent.
      position <- dplyr::coalesce(
        eive_value,
        tichy_value
      )

      source_layer <- dplyr::case_when(
        !is.na(eive_value) ~ "EIVE_primary",
        is.na(eive_value) & !is.na(tichy_value) ~ "Tichy_gapfill",
        TRUE ~ "missing"
      )

      # Species-cluster importance. The floor keeps constituent
      # species from receiving zero weight.
      phi_weight <- ifelse(
        is.na(joined$phi),
        1,
        0.10 + 0.90 * pmax(
          0,
          pmin(1, joined$phi)
        )
      )

      # Evidence depth, not a formal probability.
      source_support <- dplyr::case_when(
        source_layer == "EIVE_primary" & !is.na(eive_n) ~
          0.50 + 0.50 * pmin(eive_n, 8) / 8,

        source_layer == "EIVE_primary" ~ 0.60,

        source_layer == "Tichy_gapfill" ~ 0.60,

        TRUE ~ 0
      )

      # Narrow-niche species are more diagnostic. A 0.25 floor
      # prevents generalists from disappearing completely.
      niche_specificity <- ifelse(
        !is.na(eive_nw),
        0.25 + 0.75 * (
          1 - pmin(10, pmax(0, eive_nw)) / 10
        ),
        0.55
      )

      final_weight <- (
        phi_weight *
          source_support *
          niche_specificity
      )

      agreement <- ifelse(
        !is.na(eive_value) & !is.na(tichy_value),
        pmax(
          0,
          1 - abs(eive_value - tichy_value) / 10
        ),
        NA_real_
      )

      tibble::tibble(
        cluster = joined$cluster,
        species = joined$species,
        phi = joined$phi,
        match_method = joined$match_method,
        matched_reference_name = joined$reference_name,
        axis = axis,
        eive_value = eive_value,
        tichy_value_0_10 = tichy_value,
        position_0_10 = position,
        niche_width_0_10 = eive_nw,
        eive_source_systems = eive_n,
        source_layer = source_layer,
        source_agreement_0_1 = agreement,
        phi_weight = phi_weight,
        source_support_weight = source_support,
        niche_specificity_weight = niche_specificity,
        final_weight = final_weight
      )
    }
  )
}


.bootstrap_weighted_mean <- function(
    x,
    w,
    iterations
) {
  if (iterations <= 0L || length(x) == 0L) {
    return(c(
      low = NA_real_,
      high = NA_real_,
      sd = NA_real_
    ))
  }

  values <- replicate(
    iterations,
    {
      index <- sample.int(
        length(x),
        size = length(x),
        replace = TRUE
      )

      stats::weighted.mean(
        x[index],
        w[index],
        na.rm = TRUE
      )
    }
  )

  c(
    low = unname(stats::quantile(
      values,
      probs = 0.025,
      na.rm = TRUE
    )),
    high = unname(stats::quantile(
      values,
      probs = 0.975,
      na.rm = TRUE
    )),
    sd = stats::sd(values, na.rm = TRUE)
  )
}


.summarise_axis_group <- function(
    data,
    bootstrap
) {
  valid <- (
    is.finite(data$position_0_10) &
      is.finite(data$final_weight) &
      data$final_weight > 0
  )

  n_total <- nrow(data)
  n_scored <- sum(valid)

  total_phi_weight <- sum(
    data$phi_weight,
    na.rm = TRUE
  )

  coverage <- if (total_phi_weight > 0) {
    sum(
      data$phi_weight[valid],
      na.rm = TRUE
    ) / total_phi_weight
  } else {
    NA_real_
  }

  if (n_scored == 0L) {
    return(tibble::tibble(
      species_total = n_total,
      species_scored = 0L,
      coverage_weighted = coverage,
      score_mean_0_10 = NA_real_,
      score_median_0_10 = NA_real_,
      dispersion_0_10 = NA_real_,
      bootstrap_low_0_10 = NA_real_,
      bootstrap_high_0_10 = NA_real_,
      bootstrap_sd = NA_real_,
      source_agreement_mean = NA_real_,
      confidence_heuristic = 0,
      confidence_tier = "insufficient",
      band = NA_character_
    ))
  }

  x <- data$position_0_10[valid]
  w <- data$final_weight[valid]

  score_mean <- stats::weighted.mean(
    x,
    w,
    na.rm = TRUE
  )

  score_median <- .weighted_median(x, w)

  dispersion <- stats::weighted.mean(
    abs(x - score_mean),
    w,
    na.rm = TRUE
  )

  bootstrap_result <- .bootstrap_weighted_mean(
    x = x,
    w = w,
    iterations = bootstrap
  )

  agreement_valid <- is.finite(
    data$source_agreement_0_1
  )

  agreement_mean <- if (any(agreement_valid)) {
    stats::weighted.mean(
      data$source_agreement_0_1[agreement_valid],
      data$phi_weight[agreement_valid],
      na.rm = TRUE
    )
  } else {
    NA_real_
  }

  sample_factor <- min(
    1,
    sqrt(n_scored / 5)
  )

  coherence_factor <- max(
    0,
    1 - dispersion / 4
  )

  confidence <- coverage *
    sample_factor *
    coherence_factor

  confidence <- max(
    0,
    min(1, confidence)
  )

  confidence_tier <- if (
    n_scored < 3L ||
      is.na(coverage) ||
      coverage < 0.25
  ) {
    "insufficient"
  } else if (confidence >= 0.75) {
    "high"
  } else if (confidence >= 0.50) {
    "moderate"
  } else {
    "low"
  }

  tibble::tibble(
    species_total = n_total,
    species_scored = n_scored,
    coverage_weighted = coverage,
    score_mean_0_10 = score_mean,
    score_median_0_10 = score_median,
    dispersion_0_10 = dispersion,
    bootstrap_low_0_10 = bootstrap_result[["low"]],
    bootstrap_high_0_10 = bootstrap_result[["high"]],
    bootstrap_sd = bootstrap_result[["sd"]],
    source_agreement_mean = agreement_mean,
    confidence_heuristic = confidence,
    confidence_tier = confidence_tier,
    band = NA_character_
  )
}


#' Score Cocktail clusters using EIVE and Tichy indicator values
#'
#' The output is an ecological profile, not a habitat classification.
#'
#' Per-species weight:
#'   phi_weight * source_support_weight * niche_specificity_weight
#'
#' Cluster score:
#'   weighted mean of indicator positions on a common 0-10 scale
#'
#' @param x A cocktail_cluster() result.
#' @param clusters Cluster labels such as c("c_1", "c_4").
#' @param min_phi Optional species-cluster phi threshold. NULL uses the
#'   cluster-constituting species returned by species_in_clusters().
#' @param bootstrap Number of species-level bootstrap resamples.
#' @param seed Random seed for bootstrap reproducibility.
#' @param root cocktailr project root.
#' @param force_species Refresh the requested species lookup cache.
#' @param force_reference Re-read both Excel workbooks.
#' @param ... Passed to build_indicator_reference().
#' @return A list with cluster scores, per-species evidence and unmatched names.
#' @export
score_cluster_semantics <- function(
    x,
    clusters,
    min_phi = NULL,
    bootstrap = 500L,
    seed = 42L,
    root = cocktailr_project_root(),
    force_species = FALSE,
    force_reference = FALSE,
    ...
) {
  .require_semantic_packages()

  if (missing(clusters) || length(clusters) == 0L) {
    stop(
      "Provide one or more cluster labels in `clusters`.",
      call. = FALSE
    )
  }

  cluster_species <- .extract_cluster_species(
    x = x,
    clusters = clusters,
    min_phi = min_phi
  )

  lookup <- get_species_indicator_lookup(
    species = unique(cluster_species$species),
    root = root,
    force = force_species,
    force_reference = force_reference,
    ...
  )

  species_evidence <- .build_species_axis_evidence(
    cluster_species = cluster_species,
    lookup = lookup
  )

  set.seed(seed)

  grouped <- split(
    species_evidence,
    list(
      species_evidence$cluster,
      species_evidence$axis
    ),
    drop = TRUE
  )

  cluster_scores <- purrr::map_dfr(
    grouped,
    function(group_data) {
      summary <- .summarise_axis_group(
        data = group_data,
        bootstrap = as.integer(bootstrap)
      )

      summary$cluster <- group_data$cluster[[1L]]
      summary$axis <- group_data$axis[[1L]]
      summary$band <- .axis_band(
        axis = group_data$axis[[1L]],
        score = summary$score_mean_0_10[[1L]]
      )

      summary |>
        dplyr::select(
          .data$cluster,
          .data$axis,
          dplyr::everything()
        )
    }
  ) |>
    dplyr::arrange(
      .data$cluster,
      match(.data$axis, c("l", "t", "m", "r", "n", "s"))
    )

  wide_profile <- cluster_scores |>
    dplyr::select(
      .data$cluster,
      .data$axis,
      .data$score_mean_0_10,
      .data$band,
      .data$coverage_weighted,
      .data$confidence_heuristic,
      .data$confidence_tier
    ) |>
    tidyr::pivot_wider(
      names_from = .data$axis,
      values_from = c(
        .data$score_mean_0_10,
        .data$band,
        .data$coverage_weighted,
        .data$confidence_heuristic,
        .data$confidence_tier
      ),
      names_glue = "{.value}_{axis}"
    )

  unmatched_species <- lookup |>
    dplyr::filter(.data$match_method == "unmatched") |>
    dplyr::select(
      .data$input_species,
      .data$lookup_name,
      .data$match_method
    )

  result <- list(
    created_at = Sys.time(),
    clusters = clusters,
    cluster_scores = cluster_scores,
    wide_profile = wide_profile,
    species_evidence = species_evidence,
    species_lookup = lookup,
    unmatched_species = unmatched_species,
    paths = semantic_layer_paths(root),
    scoring_notes = list(
      scale = "0-10",
      primary_source = "EIVE for M/N/R/L/T",
      gapfill_source = "Tichy for missing values and salinity",
      weight_formula = paste(
        "phi_weight * source_support_weight *",
        "niche_specificity_weight"
      ),
      confidence_warning = paste(
        "confidence_heuristic is a project heuristic,",
        "not a calibrated probability or p-value"
      )
    )
  )

  paths <- result$paths

  saveRDS(
    result,
    file.path(
      paths$results_dir,
      "latest_cluster_semantic_profile.rds"
    )
  )

  utils::write.csv(
    cluster_scores,
    file.path(
      paths$results_dir,
      "latest_cluster_axis_scores.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  utils::write.csv(
    species_evidence,
    file.path(
      paths$results_dir,
      "latest_species_axis_evidence.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  utils::write.csv(
    unmatched_species,
    file.path(
      paths$results_dir,
      "latest_unmatched_species.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  result
}


#' Convert one cluster profile into compact LLM-ready evidence
#'
#' @param result Output of score_cluster_semantics().
#' @param cluster One cluster label.
#' @param as_json Return pretty JSON when jsonlite is installed.
#' @return A named list or JSON string.
#' @export
semantic_profile_for_llm <- function(
    result,
    cluster,
    as_json = FALSE
) {
  score_table <- result$cluster_scores |>
    dplyr::filter(.data$cluster == cluster)

  if (nrow(score_table) == 0L) {
    stop(
      "Cluster not found in result: ",
      cluster,
      call. = FALSE
    )
  }

  axes <- lapply(
    seq_len(nrow(score_table)),
    function(i) {
      row <- score_table[i, , drop = FALSE]

      list(
        axis = row$axis[[1L]],
        score_0_10 = row$score_mean_0_10[[1L]],
        band = row$band[[1L]],
        coverage = row$coverage_weighted[[1L]],
        confidence_tier = row$confidence_tier[[1L]],
        bootstrap_interval = c(
          row$bootstrap_low_0_10[[1L]],
          row$bootstrap_high_0_10[[1L]]
        )
      )
    }
  )

  names(axes) <- score_table$axis

  payload <- list(
    cluster = cluster,
    ecological_axes = axes,
    instructions = c(
      "Use only the supplied ecological evidence.",
      "Do not infer a formal habitat class unless evidence supports it.",
      "Mention low coverage or insufficient axes explicitly."
    )
  )

  if (!as_json) {
    return(payload)
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "Install jsonlite to produce JSON.",
      call. = FALSE
    )
  }

  jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
}


.semantic_axis_display_name <- function(axis) {
  switch(
    as.character(axis),
    l = "Light",
    t = "Temperature",
    m = "Moisture",
    r = "Reaction",
    n = "Nutrients",
    s = "Salinity",
    paste0("Axis ", axis)
  )
}


.cluster_evidence_next_semantic_id <- function(evidence) {
  ids <- names(evidence$evidence$items %||% list())

  if (!length(ids)) {
    return(1L)
  }

  numeric_ids <- suppressWarnings(
    as.integer(sub("^E", "", ids))
  )
  numeric_ids <- numeric_ids[is.finite(numeric_ids)]

  if (!length(numeric_ids)) {
    return(1L)
  }

  max(numeric_ids) + 1L
}


.semantic_cluster_unmatched_species <- function(result, cluster) {
  species_evidence <- result$species_evidence %||% NULL

  if (is.null(species_evidence) || !nrow(species_evidence)) {
    return(character(0))
  }

  unique(
    species_evidence$species[
      species_evidence$cluster == cluster &
        species_evidence$source_layer == "missing"
    ]
  )
}

.semantic_cluster_species_axis_values <- function(result, cluster) {
  species_evidence <- result$species_evidence %||% NULL
  axes <- c("l", "m", "n", "r", "t", "s")

  empty <- data.frame(
    species = character(0),
    l = numeric(0),
    m = numeric(0),
    n = numeric(0),
    r = numeric(0),
    t = numeric(0),
    s = numeric(0),
    stringsAsFactors = FALSE
  )

  if (is.null(species_evidence) || !nrow(species_evidence)) {
    return(empty)
  }

  species_order <- unique(
    as.character(species_evidence$species[species_evidence$cluster == cluster])
  )
  if (!length(species_order)) {
    return(empty)
  }

  axis_values <- species_evidence |>
    dplyr::filter(.data$cluster == cluster) |>
    dplyr::group_by(.data$species, .data$axis) |>
    dplyr::summarise(
      score_0_10 = {
        valid <- .data$position_0_10[is.finite(.data$position_0_10)]
        if (!length(valid)) {
          NA_real_
        } else {
          as.numeric(valid[[1L]])
        }
      },
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = .data$axis,
      values_from = .data$score_0_10
    )

  axis_values <- as.data.frame(axis_values, stringsAsFactors = FALSE)

  for (axis in axes) {
    if (!axis %in% names(axis_values)) {
      axis_values[[axis]] <- NA_real_
    }
  }

  axis_values <- axis_values[, c("species", axes), drop = FALSE]
  axis_values$species <- as.character(axis_values$species)
  axis_values <- axis_values[
    match(species_order, axis_values$species),
    ,
    drop = FALSE
  ]
  axis_values <- axis_values[!is.na(axis_values$species), , drop = FALSE]
  rownames(axis_values) <- NULL
  axis_values
}


.augment_cluster_evidence_with_semantic_layer <- function(
    evidence,
    semantic_result
) {
  if (!inherits(evidence, "cluster_evidence")) {
    stop("`evidence` must inherit from class `cluster_evidence`.")
  }

  cluster_id <- evidence$meta$cluster_id
  score_table <- semantic_result$cluster_scores |>
    dplyr::filter(.data$cluster == cluster_id)

  if (!nrow(score_table)) {
    stop(
      "Semantic profile does not contain cluster ",
      cluster_id,
      ".",
      call. = FALSE
    )
  }

  semantic_summary <- dplyr::transmute(
    score_table,
    axis = .data$axis,
    axis_name = vapply(.data$axis, .semantic_axis_display_name, character(1)),
    score_0_10 = .data$score_mean_0_10,
    band = .data$band,
    coverage = .data$coverage_weighted,
    confidence = .data$confidence_heuristic,
    confidence_tier = .data$confidence_tier,
    bootstrap_low_0_10 = .data$bootstrap_low_0_10,
    bootstrap_high_0_10 = .data$bootstrap_high_0_10,
    evidence_id = NA_character_
  )

  next_id <- .cluster_evidence_next_semantic_id(evidence)
  new_items <- list()
  new_ids <- character(0)

  for (i in seq_len(nrow(semantic_summary))) {
    evidence_id <- paste0("E", next_id)
    next_id <- next_id + 1L

    semantic_summary$evidence_id[[i]] <- evidence_id
    new_ids <- c(new_ids, evidence_id)
    new_items[[evidence_id]] <- list(
      id = evidence_id,
      type = "semantic_axis",
      label = paste0(
        "Semantic ecological axis ",
        semantic_summary$axis_name[[i]],
        " for ",
        cluster_id
      ),
      value = list(
        axis = semantic_summary$axis[[i]],
        axis_name = semantic_summary$axis_name[[i]],
        score_0_10 = semantic_summary$score_0_10[[i]],
        band = semantic_summary$band[[i]],
        coverage = semantic_summary$coverage[[i]],
        confidence = semantic_summary$confidence[[i]],
        confidence_tier = semantic_summary$confidence_tier[[i]],
        bootstrap_interval = c(
          semantic_summary$bootstrap_low_0_10[[i]],
          semantic_summary$bootstrap_high_0_10[[i]]
        )
      ),
      source = "score_cluster_semantics",
      support_level = "derived"
    )
  }

  evidence$evidence$items <- c(evidence$evidence$items, new_items)
  evidence$evidence$index$semantic_axes <- new_ids
  evidence$summaries$semantic_axes <- semantic_summary
  evidence$summaries$species_axis_values <- .semantic_cluster_species_axis_values(
    semantic_result,
    cluster_id
  )
  evidence$summaries$semantic_unmatched_species <- .semantic_cluster_unmatched_species(
    semantic_result,
    cluster_id
  )
  evidence$meta$source$has_semantic_layer <- TRUE
  evidence$future$semantic_layer <- list(
    source = "EIVE + Tichy indicator-value aggregation",
    instructions = semantic_profile_for_llm(
      semantic_result,
      cluster_id,
      as_json = FALSE
    )$instructions,
    reference_cache = semantic_result$paths$reference_cache %||% NULL,
    species_cache = semantic_result$paths$species_cache %||% NULL
  )

  evidence
}
