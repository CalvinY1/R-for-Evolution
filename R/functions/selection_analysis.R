selection_analysis <- function(data,
                               fitness_col,
                               trait_cols,
                               standardize = TRUE,
                               family = NULL) {
  if (length(trait_cols) < 1) {
    stop("At least 1 trait is required for selection analysis")
  }

  # Prepare data
  df <- prepare_selection_data(
    data = data,
    fitness_col = fitness_col,
    trait_cols = trait_cols,
    standardize = standardize,
    add_relative = TRUE
  )

  # Detect family and fitness type
  if (is.null(family)) {
    det <- detect_family(df[[fitness_col]])
    fam <- det$family
    fitness_type <- det$type
  } else {
    fam <- family
    fitness_type <- ifelse(fam$family == "binomial", "binary", "continuous")
  }

  # Run linear selection analysis
  lin <- analyze_linear_selection(
    data = df,
    fitness_col = fitness_col,
    trait_cols = trait_cols,
    fitness_type = fitness_type
  )

  # Run nonlinear selection analysis
  nonlin <- analyze_nonlinear_selection(
    data = df,
    fitness_col = fitness_col,
    trait_cols = trait_cols,
    fitness_type = fitness_type
  )

  # Extract coefficients
  coeff_tab <- extract_linear_coefficients(trait_cols, lin)

  if (length(trait_cols) >= 2) {
    coeff_tab <- rbind(
      coeff_tab,
      extract_quadratic_coefficients(trait_cols, nonlin),
      extract_interaction_coefficients(trait_cols, nonlin)
    )
  }

  # Return results
  list(
    coefficients_table = coeff_tab,
    prepared_data = df,
    fitness_type = fitness_type,
    analysis_date = Sys.Date(),
    models = list(linear = lin$model, nonlinear = nonlin$model)
  )
}
