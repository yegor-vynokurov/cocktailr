test_that("generate_synthetic_vegetation_data keeps backward-compatible defaults", {
  syn <- generate_synthetic_vegetation_data(seed = 42)

  expect_s3_class(syn, "synthetic_vegetation_data")
  expect_true(is.matrix(syn$wide_matrix))
  expect_true(is.data.frame(syn$plot_truth))
  expect_true(is.data.frame(syn$species_truth))
  expect_equal(syn$metadata$transition_mix_weight, 0.55)
  expect_equal(syn$metadata$diagnostic_species_scale, 1)
  expect_equal(syn$metadata$common_species_scale, 1)
  expect_equal(syn$metadata$noise_species_scale, 1)
  expect_equal(syn$metadata$dataset_type, "synthetic")
  expect_match(syn$metadata$dataset_label, "^synthetic_seed42_", perl = TRUE)
  expect_equal(
    attr(syn$wide_matrix, "cocktailr_dataset_info")$label,
    syn$metadata$dataset_label
  )
  expect_equal(
    attr(syn$long_table, "cocktailr_dataset_info")$type,
    "synthetic"
  )
})

test_that("generate_synthetic_vegetation_data records custom difficulty controls", {
  syn <- generate_synthetic_vegetation_data(
    seed = 99,
    n_plots_per_community = 12,
    n_transition_plots = 8,
    transition_mix_weight = 0.65,
    diagnostic_species_scale = 0.9,
    common_species_scale = 1.2,
    noise_species_scale = 1.4
  )

  expect_equal(syn$metadata$n_pure_plots, 48)
  expect_equal(syn$metadata$n_transition_plots, 8)
  expect_equal(syn$metadata$transition_mix_weight, 0.65)
  expect_equal(syn$metadata$diagnostic_species_scale, 0.9)
  expect_equal(syn$metadata$common_species_scale, 1.2)
  expect_equal(syn$metadata$noise_species_scale, 1.4)
})

test_that("generate_synthetic_vegetation_data validates transition_mix_weight", {
  expect_error(
    generate_synthetic_vegetation_data(transition_mix_weight = 0),
    "transition_mix_weight"
  )
  expect_error(
    generate_synthetic_vegetation_data(noise_species_scale = -1),
    "noise_species_scale"
  )
})
