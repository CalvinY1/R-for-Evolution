plot_adaptive_landscape <- function(
  landscape,
  trait_cols,
  original_data = NULL,
  group_col = NULL,
  bins = 12,
  show_optimum = TRUE,
  show_actual_means = TRUE
) {
    # Input validation
    if (!inherits(landscape, "adaptive_landscape")) {
        warning("Object is not of class 'adaptive_landscape'")
    }

    df <- landscape$grid
    names(df)[1:2] <- trait_cols

    # Base plot
    p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
            x = .data[[trait_cols[1]]],
            y = .data[[trait_cols[2]]],
            z = .data[[".mean_fit"]]
        )
    ) +
        ggplot2::geom_contour_filled(bins = bins) +
        ggplot2::geom_contour(color = "black", alpha = 0.3) +
        ggplot2::scale_fill_viridis_d(name = "Mean Fitness") +
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

    if (show_actual_means && !is.null(landscape$actual_population_means)) {
        p <- p +
            ggplot2::geom_point(
                data = landscape$actual_population_means,
                ggplot2::aes(
                    x = .data[[trait_cols[1]]],
                    y = .data[[trait_cols[2]]]
                ),
                color = "red",
                size = 4,
                shape = 19,
                alpha = 0.8
            )

        if (!is.null(group_col) && group_col %in% names(landscape$actual_population_means)) {
            if (requireNamespace("ggrepel", quietly = TRUE)) {
                p <- p +
                    ggrepel::geom_text_repel(
                        data = landscape$actual_population_means,
                        ggplot2::aes(
                            x = .data[[trait_cols[1]]],
                            y = .data[[trait_cols[2]]],
                            label = .data[[group_col]]
                        ),
                        size = 3,
                        box.padding = 0.5,
                        point.padding = 0.3
                    )
            } else {
                p <- p +
                    ggplot2::geom_text(
                        data = landscape$actual_population_means,
                        ggplot2::aes(
                            x = .data[[trait_cols[1]]],
                            y = .data[[trait_cols[2]]],
                            label = .data[[group_col]]
                        ),
                        size = 3,
                        vjust = -1,
                        hjust = 0.5
                    )
            }
        }
    }

    if (show_optimum && !is.null(landscape$optimum)) {
        p <- p +
            ggplot2::geom_point(
                data = landscape$optimum,
                ggplot2::aes(
                    x = .data[[trait_cols[1]]],
                    y = .data[[trait_cols[2]]]
                ),
                color = "yellow",
                size = 5,
                shape = 18
            ) +
            ggplot2::annotate(
                "text",
                x = landscape$optimum[[trait_cols[1]]],
                y = landscape$optimum[[trait_cols[2]]],
                label = "Optimum",
                vjust = -1,
                size = 3,
                color = "gray30"
            )
    }

    return(p)
}


plot_adaptive_landscape_3d <- function(
  landscape,
  trait_cols,
  theta = -30,
  phi = 30,
  grid_n = 200
) {
    df <- landscape$grid

    # Extract data
    x <- df[[trait_cols[1]]]
    y <- df[[trait_cols[2]]]
    z <- df$.mean_fit

    # Add small jitter to avoid collinear warnings
    set.seed(42)
    x <- x + rnorm(length(x), 0, 1e-8)
    y <- y + rnorm(length(y), 0, 1e-8)

    # Interpolate for smooth surface
    interp <- akima::interp(
        x = x,
        y = y,
        z = z,
        xo = seq(min(x), max(x), length = grid_n),
        yo = seq(min(y), max(y), length = grid_n),
        linear = FALSE, # Spline interpolation
        extrap = TRUE,
        duplicate = "mean"
    )

    fields::drape.plot(
        x = interp$x,
        y = interp$y,
        z = interp$z,
        theta = theta,
        phi = phi,
        xlab = trait_cols[1],
        ylab = trait_cols[2],
        zlab = "Mean Fitness",
        main = "Adaptive Landscape (3D View)",
        col = viridis::plasma(100),
        border = NA,
        shade = 0.5
    )
}
