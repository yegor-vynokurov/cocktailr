#' Generate a human-readable synthetic vegetation dataset
#'
#' Creates an artificial vegetation dataset with real plant names.
#' The data are synthetic and intended for testing Cocktail clustering,
#' cluster interpretation, and LLM-assisted labeling.
#'
#' @param n_plots_per_community Number of pure plots per community.
#'   Either one number or a vector of four numbers.
#' @param n_transition_plots Number of mixed transition plots.
#' @param seed Random seed for reproducibility.
#' @param transition_mix_weight Weight applied to each parent community when
#'   generating transition plots. Higher values make transitions more blended.
#' @param diagnostic_species_scale Multiplier applied to diagnostic-species
#'   occurrence probabilities before transition mixing.
#' @param common_species_scale Multiplier applied to common/generalist-species
#'   occurrence probabilities.
#' @param noise_species_scale Multiplier applied to rare/noise-species
#'   occurrence probabilities.
#' @param use_underscores If TRUE, species names use underscores instead of spaces.
#' @param keep_absences_in_long If TRUE, the long table includes zero values.
#'
#' @return A list with:
#'   \describe{
#'     \item{wide_matrix}{Matrix: plots x species, values are synthetic cover percentages.}
#'     \item{long_table}{Data frame with plot, species, value.}
#'     \item{plot_truth}{Ground truth table for plots and communities.}
#'     \item{species_truth}{Ground truth table for species and ecological groups.}
#'     \item{community_profiles}{Human-readable description of synthetic communities.}
#'     \item{metadata}{Generation settings.}
#'   }
#'
#' @examples
#' syn <- generate_synthetic_vegetation_data(
#'   n_plots_per_community = 4,
#'   n_transition_plots = 2,
#'   seed = 42
#' )
#'
#' dim(syn$wide_matrix)
#' head(syn$plot_truth)
#' head(syn$community_profiles)
#'
#' @export
generate_synthetic_vegetation_data <- function(
  n_plots_per_community = 30,
  n_transition_plots = 16,
  seed = 42,
  transition_mix_weight = 0.55,
  diagnostic_species_scale = 1,
  common_species_scale = 1,
  noise_species_scale = 1,
  use_underscores = FALSE,
  keep_absences_in_long = FALSE
) {
  set.seed(seed)

  format_species_names <- function(x) {
    if (isTRUE(use_underscores)) {
      gsub(" ", "_", x, fixed = TRUE)
    } else {
      x
    }
  }

  communities <- list(
    dry_meadow = list(
      label = "Dry calcareous meadow",
      ecological_hint = "Open dry grassland, calcareous or relatively poor soils, many grasses and herbs.",
      species = c(
        "Festuca ovina",
        "Bromus erectus",
        "Thymus serpyllum",
        "Salvia pratensis",
        "Galium verum",
        "Achillea millefolium",
        "Lotus corniculatus",
        "Plantago lanceolata"
      )
    ),
    wet_meadow = list(
      label = "Wet meadow and marsh edge",
      ecological_hint = "Moist to wet meadow, marsh edges, periodically wet soils.",
      species = c(
        "Carex acuta",
        "Juncus effusus",
        "Caltha palustris",
        "Filipendula ulmaria",
        "Lychnis flos-cuculi",
        "Deschampsia cespitosa",
        "Mentha aquatica",
        "Iris pseudacorus"
      )
    ),
    woodland = list(
      label = "Deciduous woodland shade",
      ecological_hint = "Shaded deciduous woodland, forest floor herbs, trees and ferns.",
      species = c(
        "Fagus sylvatica",
        "Quercus robur",
        "Anemone nemorosa",
        "Galium odoratum",
        "Mercurialis perennis",
        "Hedera helix",
        "Dryopteris filix-mas",
        "Melica uniflora"
      )
    ),
    ruderal_edge = list(
      label = "Ruderal nitrophilous edge",
      ecological_hint = "Nutrient-rich disturbed edges, pathsides, settlements, tall nitrophilous herbs.",
      species = c(
        "Urtica dioica",
        "Galium aparine",
        "Sambucus nigra",
        "Artemisia vulgaris",
        "Ballota nigra",
        "Chelidonium majus",
        "Aegopodium podagraria",
        "Dactylis glomerata"
      )
    )
  )

  common_species <- c(
    "Poa pratensis",
    "Trifolium repens",
    "Taraxacum officinale",
    "Ranunculus acris",
    "Plantago major"
  )

  noise_species <- c(
    "Betula pendula",
    "Pinus sylvestris",
    "Phragmites australis",
    "Rubus caesius",
    "Vicia cracca",
    "Leucanthemum vulgare",
    "Hypericum perforatum",
    "Rumex acetosa",
    "Veronica chamaedrys",
    "Prunella vulgaris"
  )

  # Apply readable or code-safe species naming.
  for (nm in names(communities)) {
    communities[[nm]]$species <- format_species_names(communities[[nm]]$species)
  }
  common_species <- format_species_names(common_species)
  noise_species <- format_species_names(noise_species)

  community_names <- names(communities)

  if (length(n_plots_per_community) == 1) {
    n_plots_per_community <- rep(n_plots_per_community, length(community_names))
  }

  if (length(n_plots_per_community) != length(community_names)) {
    stop(
      "n_plots_per_community must be either one number or a vector of ",
      length(community_names),
      " numbers."
    )
  }

  if (any(n_plots_per_community < 1)) {
    stop("Each community must have at least one plot.")
  }

  if (n_transition_plots < 0) {
    stop("n_transition_plots must be >= 0.")
  }

  if (!is.numeric(transition_mix_weight) ||
      length(transition_mix_weight) != 1L ||
      !is.finite(transition_mix_weight) ||
      transition_mix_weight <= 0 ||
      transition_mix_weight > 1) {
    stop("transition_mix_weight must be a single number in the interval (0, 1].")
  }

  validate_scale <- function(x, name) {
    if (!is.numeric(x) ||
        length(x) != 1L ||
        !is.finite(x) ||
        x <= 0) {
      stop(name, " must be a single positive number.")
    }
  }

  validate_scale(diagnostic_species_scale, "diagnostic_species_scale")
  validate_scale(common_species_scale, "common_species_scale")
  validate_scale(noise_species_scale, "noise_species_scale")

  scale_probability <- function(x, scale, cap) {
    out <- pmin(cap, pmax(0, x * scale))
    stats::setNames(out, names(x))
  }

  diagnostic_species <- unlist(
    lapply(communities, function(x) x$species),
    use.names = FALSE
  )

  all_species <- unique(c(diagnostic_species, common_species, noise_species))

  if (anyDuplicated(all_species)) {
    stop("Duplicated species names found. Please check species lists.")
  }

  # Build species truth/probability table.
  species_truth <- data.frame(
    species = all_species,
    expected_group = NA_character_,
    ecological_role = NA_character_,
    dry_meadow = NA_real_,
    wet_meadow = NA_real_,
    woodland = NA_real_,
    ruderal_edge = NA_real_,
    stringsAsFactors = FALSE
  )

  assign_probabilities <- function(species_name, group_name) {
    # Low background probabilities outside the main group.
    probs <- c(
      dry_meadow = runif(1, 0.02, 0.12),
      wet_meadow = runif(1, 0.02, 0.12),
      woodland = runif(1, 0.02, 0.12),
      ruderal_edge = runif(1, 0.04, 0.16)
    )

    # High probability in the intended ecological group.
    probs[group_name] <- runif(1, 0.72, 0.92)

    # Mild intended overlaps to make the dataset less toy-flat.
    if (group_name == "dry_meadow") {
      probs["ruderal_edge"] <- max(probs["ruderal_edge"], runif(1, 0.10, 0.22))
    }
    if (group_name == "wet_meadow") {
      probs["ruderal_edge"] <- max(probs["ruderal_edge"], runif(1, 0.08, 0.20))
    }
    if (group_name == "woodland") {
      probs["wet_meadow"] <- max(probs["wet_meadow"], runif(1, 0.05, 0.15))
    }
    if (group_name == "ruderal_edge") {
      probs["dry_meadow"] <- max(probs["dry_meadow"], runif(1, 0.08, 0.20))
      probs["woodland"] <- max(probs["woodland"], runif(1, 0.06, 0.16))
    }

    probs
  }

  for (group_name in community_names) {
    sp <- communities[[group_name]]$species
    rows <- species_truth$species %in% sp

    species_truth$expected_group[rows] <- group_name
    species_truth$ecological_role[rows] <- "diagnostic"

    for (species_name in sp) {
      row_id <- species_truth$species == species_name
      probs <- assign_probabilities(species_name, group_name)
      probs <- scale_probability(probs, diagnostic_species_scale, cap = 0.98)
      species_truth[row_id, names(probs)] <- probs
    }
  }

  common_rows <- species_truth$species %in% common_species
  species_truth$expected_group[common_rows] <- "shared"
  species_truth$ecological_role[common_rows] <- "common/generalist"
  for (col in community_names) {
    species_truth[common_rows, col] <- scale_probability(
      runif(sum(common_rows), 0.28, 0.58),
      common_species_scale,
      cap = 0.95
    )
  }

  noise_rows <- species_truth$species %in% noise_species
  species_truth$expected_group[noise_rows] <- "noise"
  species_truth$ecological_role[noise_rows] <- "rare/noise"
  for (col in community_names) {
    species_truth[noise_rows, col] <- scale_probability(
      runif(sum(noise_rows), 0.03, 0.14),
      noise_species_scale,
      cap = 0.60
    )
  }

  # Community profile table for humans and LLM prompts.
  community_profiles <- data.frame(
    community = community_names,
    label = vapply(communities, function(x) x$label, character(1)),
    ecological_hint = vapply(communities, function(x) x$ecological_hint, character(1)),
    diagnostic_species = vapply(
      communities,
      function(x) paste(x$species, collapse = "; "),
      character(1)
    ),
    stringsAsFactors = FALSE
  )

  # Build plot truth table.
  pure_plot_rows <- list()

  for (i in seq_along(community_names)) {
    community <- community_names[i]
    n_i <- n_plots_per_community[i]

    pure_plot_rows[[community]] <- data.frame(
      plot = sprintf("%s_%03d", community, seq_len(n_i)),
      community = community,
      community_label = communities[[community]]$label,
      is_transition = FALSE,
      mix_a = community,
      mix_b = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  plot_truth <- do.call(rbind, pure_plot_rows)
  rownames(plot_truth) <- NULL

  transition_pairs <- list(
    c("dry_meadow", "wet_meadow"),
    c("wet_meadow", "woodland"),
    c("woodland", "ruderal_edge"),
    c("dry_meadow", "ruderal_edge")
  )

  if (n_transition_plots > 0) {
    base_n <- floor(n_transition_plots / length(transition_pairs))
    remainder <- n_transition_plots %% length(transition_pairs)

    transition_counts <- rep(base_n, length(transition_pairs))
    if (remainder > 0) {
      transition_counts[seq_len(remainder)] <- transition_counts[seq_len(remainder)] + 1
    }

    transition_rows <- list()

    for (i in seq_along(transition_pairs)) {
      pair <- transition_pairs[[i]]
      n_i <- transition_counts[i]

      if (n_i == 0) {
        next
      }

      label <- paste0("transition_", pair[1], "_", pair[2])

      transition_rows[[i]] <- data.frame(
        plot = sprintf("%s_%03d", label, seq_len(n_i)),
        community = label,
        community_label = paste(
          "Transition:",
          communities[[pair[1]]]$label,
          "/",
          communities[[pair[2]]]$label
        ),
        is_transition = TRUE,
        mix_a = pair[1],
        mix_b = pair[2],
        stringsAsFactors = FALSE
      )
    }

    plot_truth <- rbind(plot_truth, do.call(rbind, transition_rows))
    rownames(plot_truth) <- NULL
  }

  probability_for_plot <- function(plot_row) {
    if (!isTRUE(plot_row$is_transition)) {
      probs <- species_truth[[plot_row$mix_a]]
      names(probs) <- species_truth$species
      return(probs)
    }

    probs_a <- species_truth[[plot_row$mix_a]]
    probs_b <- species_truth[[plot_row$mix_b]]

    # Transition plots intentionally contain a mixture of both communities.
    probs <- pmin(0.95, transition_mix_weight * probs_a + transition_mix_weight * probs_b)
    names(probs) <- species_truth$species
    probs
  }

  cover_from_probability <- function(p) {
    if (p >= 0.65) {
      # Frequent diagnostic species: usually visible cover.
      return(as.integer(round(5 + 70 * stats::rbeta(1, 2.2, 3.5))))
    }

    if (p >= 0.25) {
      # Generalists and transition species.
      return(as.integer(round(1 + 35 * stats::rbeta(1, 1.6, 4.2))))
    }

    # Rare/noise/background species.
    as.integer(round(1 + 12 * stats::rbeta(1, 1.1, 5.5)))
  }

  min_species_per_plot <- 5

  wide_matrix <- matrix(
    0L,
    nrow = nrow(plot_truth),
    ncol = length(all_species),
    dimnames = list(plot_truth$plot, all_species)
  )

  for (i in seq_len(nrow(plot_truth))) {
    probs <- probability_for_plot(plot_truth[i, ])

    present <- stats::runif(length(probs)) < probs

    # Avoid almost empty plots.
    if (sum(present) < min_species_per_plot) {
      forced <- order(probs, decreasing = TRUE)[seq_len(min_species_per_plot)]
      present[forced] <- TRUE
    }

    covers <- integer(length(probs))
    covers[present] <- vapply(probs[present], cover_from_probability, integer(1))

    # Cap individual species cover at 100.
    covers <- pmin(covers, 100L)

    wide_matrix[i, ] <- covers
  }

  if (isTRUE(keep_absences_in_long)) {
    long_table <- data.frame(
      plot = rep(rownames(wide_matrix), each = ncol(wide_matrix)),
      species = rep(colnames(wide_matrix), times = nrow(wide_matrix)),
      value = as.vector(t(wide_matrix)),
      stringsAsFactors = FALSE
    )
  } else {
    present_idx <- which(wide_matrix > 0, arr.ind = TRUE)

    long_table <- data.frame(
      plot = rownames(wide_matrix)[present_idx[, 1]],
      species = colnames(wide_matrix)[present_idx[, 2]],
      value = wide_matrix[present_idx],
      stringsAsFactors = FALSE
    )

    long_table <- long_table[order(long_table$plot, long_table$species), ]
    rownames(long_table) <- NULL
  }

  metadata <- list(
    seed = seed,
    n_plots = nrow(wide_matrix),
    n_species = ncol(wide_matrix),
    n_pure_plots = sum(!plot_truth$is_transition),
    n_transition_plots = sum(plot_truth$is_transition),
    transition_mix_weight = transition_mix_weight,
    diagnostic_species_scale = diagnostic_species_scale,
    common_species_scale = common_species_scale,
    noise_species_scale = noise_species_scale,
    use_underscores = use_underscores,
    keep_absences_in_long = keep_absences_in_long,
    dataset_type = "synthetic",
    dataset_label = paste0(
      "synthetic_seed", seed,
      "_p", paste(n_plots_per_community, collapse = "x"),
      "_tr", n_transition_plots
    ),
    note = paste(
      "Synthetic dataset with real plant names.",
      "Species names are real, but co-occurrence probabilities and cover values are artificial."
    )
  )

  dataset_info <- list(
    type = metadata$dataset_type,
    label = metadata$dataset_label,
    path = NULL,
    source = "generate_synthetic_vegetation_data"
  )

  attr(wide_matrix, "cocktailr_dataset_info") <- c(
    dataset_info,
    list(representation = "wide_matrix")
  )
  attr(long_table, "cocktailr_dataset_info") <- c(
    dataset_info,
    list(representation = "long_table")
  )

  result <- list(
    wide_matrix = wide_matrix,
    long_table = long_table,
    plot_truth = plot_truth,
    species_truth = species_truth,
    community_profiles = community_profiles,
    metadata = metadata
  )

  class(result) <- c("synthetic_vegetation_data", class(result))

  result
}
