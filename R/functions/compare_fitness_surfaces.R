compare_fitness_surfaces_data <- function(
  correlated_surface,
  adaptive_landscape,
  trait_cols
) {
    # 1. Prepare data
    cor_df <- correlated_surface$grid
    names(cor_df)[1:2] <- trait_cols
    cor_df$fitness <- cor_df$.fit
    cor_df$type <- "Correlated Fitness (Individual)"

    ada_df <- adaptive_landscape$grid
    names(ada_df)[1:2] <- trait_cols
    ada_df$fitness <- ada_df$.mean_fit
    ada_df$type <- "Adaptive Landscape (Population)"

    # 2. Combine data
    combined <- rbind(
        cor_df[, c(trait_cols, "fitness", "type")],
        ada_df[, c(trait_cols, "fitness", "type")]
    )

    # 3. Calculate summary statistics
    summary_stats <- data.frame(
        Surface = c("Correlated Fitness", "Adaptive Landscape"),
        Fitness_Range_Min = c(min(cor_df$fitness, na.rm = TRUE), min(ada_df$fitness, na.rm = TRUE)),
        Fitness_Range_Max = c(max(cor_df$fitness, na.rm = TRUE), max(ada_df$fitness, na.rm = TRUE)),
        Fitness_Mean = c(mean(cor_df$fitness, na.rm = TRUE), mean(ada_df$fitness, na.rm = TRUE)),
        Fitness_SD = c(sd(cor_df$fitness, na.rm = TRUE), sd(ada_df$fitness, na.rm = TRUE))
    )

    # 4. Find optimum points
    optimum_individual <- cor_df[which.max(cor_df$fitness), ]
    optimum_population <- ada_df[which.max(ada_df$fitness), ]

    cat("\n=== Optimum Phenotypes ===\n")
    cat("Individual optimum:\n")
    print(optimum_individual[, c(trait_cols, "fitness")])
    cat("\nPopulation optimum:\n")
    print(optimum_population[, c(trait_cols, "fitness")])

    cat("\n=== Summary Statistics ===\n")
    print(summary_stats)

    list(
        combined_data = combined,
        summary_stats = summary_stats,
        optimum_individual = optimum_individual,
        optimum_population = optimum_population,
        correlated_surface = correlated_surface,
        adaptive_landscape = adaptive_landscape,
        trait_cols = trait_cols
    )
}
