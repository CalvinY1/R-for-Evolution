plot_fitness_surfaces_comparison <- function(
  comparison_data,
  bins = 10,
  title = NULL
) {
    # Extract data
    combined <- comparison_data$combined_data
    trait_cols <- comparison_data$trait_cols
    optimum_individual <- comparison_data$optimum_individual
    optimum_population <- comparison_data$optimum_population

    if (is.null(title)) {
        title <- "Comparison: Correlated Fitness vs Adaptive Landscape"
    }

    # Split data
    cor_df <- combined[combined$type == "Correlated Fitness (Individual)", ]
    ada_df <- combined[combined$type == "Adaptive Landscape (Population)", ]

    # 1. Correlated Fitness plot (Blue color scheme)
    p_cor <- ggplot2::ggplot(cor_df, ggplot2::aes(
        x = .data[[trait_cols[1]]],
        y = .data[[trait_cols[2]]],
        z = fitness
    )) +
        ggplot2::geom_contour_filled(bins = bins) +
        # FIX: Use discrete color scale
        ggplot2::scale_fill_manual(
            values = colorRampPalette(c("lightblue", "steelblue", "darkblue", "navy"))(bins),
            name = "Fitness"
        ) +
        ggplot2::labs(
            x = trait_cols[1],
            y = trait_cols[2],
            title = "Correlated Fitness",
            subtitle = "Individual level"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
            aspect.ratio = 0.8,
            plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = ggplot2::element_text(hjust = 0.5)
        )

    # 2. Adaptive Landscape plot
    p_ada <- ggplot2::ggplot(ada_df, ggplot2::aes(
        x = .data[[trait_cols[1]]],
        y = .data[[trait_cols[2]]],
        z = fitness
    )) +
        ggplot2::geom_contour_filled(bins = bins) +
        ggplot2::scale_fill_manual(
            values = colorRampPalette(c("lightcoral", "coral", "darkred", "brown"))(bins),
            name = "Mean Fitness"
        ) +
        ggplot2::labs(
            x = trait_cols[1],
            y = trait_cols[2],
            title = "Adaptive Landscape"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
            aspect.ratio = 0.8,
            plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = ggplot2::element_text(hjust = 0.5)
        )

    # Combine side by side with patchwork
    p_side <- p_cor + p_ada +
        patchwork::plot_annotation(
            title = title,
            theme = ggplot2::theme(
                plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14)
            )
        )

    # 3. Overlay comparison (no color scale issue)
    p_overlay <- ggplot2::ggplot() +
        ggplot2::geom_contour(
            data = cor_df,
            ggplot2::aes(
                x = .data[[trait_cols[1]]],
                y = .data[[trait_cols[2]]],
                z = fitness,
                color = "Correlated Fitness"
            ),
            linetype = "dashed",
            bins = bins,
            linewidth = 0.9
        ) +
        ggplot2::geom_contour(
            data = ada_df,
            ggplot2::aes(
                x = .data[[trait_cols[1]]],
                y = .data[[trait_cols[2]]],
                z = fitness,
                color = "Adaptive Landscape"
            ),
            linetype = "solid",
            bins = bins,
            linewidth = 0.9
        ) +
        ggplot2::geom_point(
            data = optimum_individual,
            ggplot2::aes(
                x = .data[[trait_cols[1]]],
                y = .data[[trait_cols[2]]],
                color = "Individual Optimum"
            ),
            size = 4,
            shape = 18
        ) +
        ggplot2::geom_point(
            data = optimum_population,
            ggplot2::aes(
                x = .data[[trait_cols[1]]],
                y = .data[[trait_cols[2]]],
                color = "Population Optimum"
            ),
            size = 4,
            shape = 18
        ) +
        ggplot2::scale_color_manual(
            name = "",
            values = c(
                "Correlated Fitness" = "steelblue",
                "Adaptive Landscape" = "darkred",
                "Individual Optimum" = "darkblue",
                "Population Optimum" = "brown"
            ),
            labels = c(
                "Correlated Fitness" = "Correlated Fitness (Individual)",
                "Adaptive Landscape" = "Adaptive Landscape (Population)",
                "Individual Optimum" = "Individual Optimum",
                "Population Optimum" = "Population Optimum"
            )
        ) +
        ggplot2::labs(
            x = trait_cols[1],
            y = trait_cols[2],
            title = "Overlay Comparison",
            subtitle = "Blue dashed: Individual fitness | Red solid: Population mean fitness\nBlue star: Individual optimum | Red star: Population optimum"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
            aspect.ratio = 0.8,
            legend.position = "right",
            legend.box = "vertical",
            plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = ggplot2::element_text(hjust = 0.5)
        )

    return(list(
        side_by_side = p_side,
        overlay = p_overlay
    ))
}
