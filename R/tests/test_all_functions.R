# ======================================================
# test_all_functions.R
# Full integration test for evolutionary selection package
# ======================================================

cat("\n========================================\n")
cat("RUNNING FULL FUNCTION TEST SUITE\n")
cat("========================================\n")

cat("Working directory:", getwd(), "\n")

# ------------------------------------------------------
# 1 Initialize environment
# ------------------------------------------------------

if (file.exists("R/scripts/0.0_initialize.R")) {
    source("R/scripts/0.0_initialize.R")
}

# ======================================================
# Load scripts
# ======================================================

cat("\nLoading script files...\n")

script_files <- list.files(
    "R/scripts",
    pattern = "\\.R$",
    full.names = TRUE
)

for (f in script_files) {
    if (basename(f) != "0.0_initialize.R") {
        source(f)
        cat("Loaded script:", basename(f), "\n")
    }
}

# ======================================================
# 3 Load functions
# ======================================================

cat("\nLoading function files...\n")

fn_files <- list.files(
    "R/functions",
    pattern = "\\.R$",
    full.names = TRUE
)

for (f in fn_files) {
    source(f)
    cat("Loaded:", basename(f), "\n")
}

# ======================================================
# 4 Load plotting
# ======================================================

cat("\nLoading plotting functions...\n")

plot_files <- list.files(
    "R/plotting",
    pattern = "\\.R$",
    full.names = TRUE
)

for (f in plot_files) {
    source(f)
    cat("Loaded plot:", basename(f), "\n")
}

# ======================================================
# 2 Output directories (defined AFTER scripts)
# ======================================================

library(here)

rm(list = intersect(ls(), c("output_dir", "figure_dir", "table_dir", "model_dir")))

output_dir <- here("R", "results", "test_results")
figure_dir <- file.path(output_dir, "figures")
table_dir <- file.path(output_dir, "tables")
model_dir <- file.path(output_dir, "models")

dirs <- c(output_dir, figure_dir, table_dir, model_dir)
lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

cat("\nResults saved to:\n")
cat("  Figures:", figure_dir, "\n")
cat("  Tables: ", table_dir, "\n")
cat("  Models: ", model_dir, "\n")


# ======================================================
# 5 Create test data
# ======================================================

cat("\nCreating synthetic test data...\n")

set.seed(42)

df_binary <- data.frame(
    fitness = rbinom(80, 1, 0.6),
    size = rnorm(80, 5, 1),
    speed = rnorm(80, 10, 2),
    color = rnorm(80, 3, 0.6)
)

df_continuous <- data.frame(
    fitness = rpois(80, 3) + 1,
    size = rnorm(80, 5, 1),
    speed = rnorm(80, 10, 2),
    color = rnorm(80, 3, 0.6)
)

cat("Binary data:", dim(df_binary), "\n")
cat("Continuous data:", dim(df_continuous), "\n")

TRAITS <- c("size", "speed", "color")
FITNESS <- "fitness"

# ======================================================
# 6 selection_coefficients
# ======================================================

cat("\nTesting selection_coefficients...\n")

test_binary <- selection_coefficients(
    data = df_binary,
    fitness_col = FITNESS,
    trait_cols = c("size", "speed"),
    fitness_type = "binary",
    standardize = TRUE
)

test_continuous <- selection_coefficients(
    data = df_continuous,
    fitness_col = FITNESS,
    trait_cols = c("size", "speed"),
    fitness_type = "continuous",
    standardize = TRUE
)

write.csv(test_binary, file.path(table_dir, "selection_binary.csv"), row.names = FALSE)
write.csv(test_continuous, file.path(table_dir, "selection_continuous.csv"), row.names = FALSE)

cat("Selection coefficients Done\n")

# ======================================================
# 7 selection_differential
# ======================================================

cat("\nTesting selection_differential...\n")

sel_diff <- selection_differential(
    data = df_continuous,
    fitness_col = FITNESS,
    trait_col = "size"
)

cat("Selection differential:", round(sel_diff, 4), "\n")

# ======================================================
# 8 univariate spline + plot
# ======================================================

cat("\nTesting univariate surfaces...\n")

univariate_plots <- list()

for (trait in TRAITS) {
    spline <- univariate_spline(
        data = df_continuous,
        fitness_col = FITNESS,
        trait_col = trait,
        fitness_type = "continuous",
        k = 8
    )

    if (exists("plot_univariate_fitness")) {
        plot <- plot_univariate_fitness(
            uni = spline,
            trait_col = trait,
            title = paste("Univariate Correlated Fitness:", trait)
        )

        univariate_plots[[trait]] <- plot

        ggsave(
            file.path(figure_dir, paste0("univariate_", trait, ".png")),
            plot,
            width = 8,
            height = 6,
            dpi = 300
        )
    } else {
        cat("plot_univariate_fitness not found, using univariate_surface\n")
        if (exists("univariate_surface")) {
            plot <- univariate_surface(
                uni = spline,
                trait_col = trait
            )
            univariate_plots[[trait]] <- plot
            ggsave(
                file.path(figure_dir, paste0("univariate_", trait, ".png")),
                plot,
                width = 8,
                height = 6,
                dpi = 300
            )
        }
    }
}

cat("Univariate surfaces Done\n")

# ======================================================
# 9 correlated fitness surfaces
# ======================================================

cat("\nTesting correlated fitness surfaces...\n")

if (!exists("trait_pairs")) {
    trait_pairs <- list(c("size", "speed"), c("size", "color"), c("speed", "color"))
}

cfs_plots <- list()

for (pair in trait_pairs) {
    cfs <- correlated_fitness_surface(
        data = df_continuous,
        fitness_col = FITNESS,
        trait_cols = pair,
        grid_n = 30
    )

    plot <- plot_correlated_fitness(
        tps = cfs,
        trait_cols = pair
    )

    name <- paste(pair, collapse = "_")
    cfs_plots[[name]] <- plot

    ggsave(
        file.path(figure_dir, paste0("cfs_", name, ".png")),
        plot,
        width = 8,
        height = 6,
        dpi = 300
    )

    if (exists("plot_correlated_fitness_enhanced")) {
        plot_enhanced <- plot_correlated_fitness_enhanced(
            tps = cfs,
            original_data = df_continuous,
            fitness_col = FITNESS
        )

        ggsave(
            file.path(figure_dir, paste0("cfs_", name, "_enhanced.png")),
            plot_enhanced,
            width = 8,
            height = 6,
            dpi = 300
        )
    }
}

cat("Correlated fitness surfaces Done\n")

# ======================================================
# 10 bootstrap
# ======================================================

if (exists("bootstrap_selection")) {
    cat("\nTesting bootstrap_selection...\n")

    boot <- bootstrap_selection(
        data = df_continuous,
        fitness_col = FITNESS,
        trait_cols = c("size", "speed"),
        fitness_type = "continuous",
        B = 50,
        seed = 42
    )

    write.csv(boot$ci, file.path(table_dir, "bootstrap_ci.csv"), row.names = FALSE)

    cat("Bootstrap Done\n")
} else {
    cat("\nSkipping bootstrap_selection (function not found)\n")
}

# ======================================================
# 11 adaptive landscape
# ======================================================
if (exists("adaptive_landscape")) {
    cat("\nTesting adaptive landscape...\n")

    fitness_model <- mgcv::gam(
        fitness ~ s(size, speed),
        data = df_continuous
    )

    landscape <- adaptive_landscape(
        data = df_continuous,
        fitness_model = fitness_model,
        trait_cols = c("size", "speed"),
        simulation_n = 400,
        grid_n = 30
    )

    saveRDS(
        landscape,
        file.path(model_dir, "adaptive_landscape.rds")
    )

    # 2D adaptive landscape plot
    if (exists("plot_adaptive_landscape")) {
        p <- plot_adaptive_landscape(
            landscape = landscape,
            trait_cols = c("size", "speed"),
            original_data = df_continuous
        )

        ggsave(
            file.path(figure_dir, "adaptive_landscape.png"),
            p,
            width = 8,
            height = 6,
            dpi = 300
        )
        cat("Adaptive 2D landscape Done\n")
    }

    # 3D adaptive landscape plot
    if (exists("plot_adaptive_landscape_3d")) {
        cat("\nGenerating 3D adaptive landscape...\n")

        png(
            file.path(figure_dir, "adaptive_landscape_3d.png"),
            width = 8,
            height = 6,
            units = "in",
            res = 300
        )

        plot_adaptive_landscape_3d(
            landscape = landscape,
            trait_cols = c("size", "speed")
        )

        dev.off()

        angles <- list(
            c(-30, 30), # default
            c(0, 30), # front view
            c(-60, 20) # side view
        )

        for (i in seq_along(angles)) {
            png(
                file.path(figure_dir, paste0("adaptive_landscape_3d_angle", i, ".png")),
                width = 8,
                height = 6,
                units = "in",
                res = 300
            )

            plot_adaptive_landscape_3d(
                landscape = landscape,
                trait_cols = c("size", "speed"),
                theta = angles[[i]][1],
                phi = angles[[i]][2]
            )

            dev.off()
        }

        cat("3D adaptive landscape plots saved (", length(angles) + 1, " angles)\n", sep = "")

        if (interactive()) {
            cat("\nDisplaying interactive 3D plot (if in RStudio)...\n")
            plot_adaptive_landscape_3d(
                landscape = landscape,
                trait_cols = c("size", "speed")
            )
        }
    }
} else {
    cat("\nSkipping adaptive_landscape (function not found)\n")
}

# ======================================================
# 12 compare_fitness_surfaces - ALL PAIRS
# ======================================================

all_pairs <- list(
    c("size", "speed"),
    c("size", "color"),
    c("speed", "color")
)

comparison_results <- list()

for (pair in all_pairs) {
    pair_name <- paste(pair, collapse = "_")
    cat("\n--- Comparing", pair_name, "---\n")

    tryCatch(
        {
            cfs <- correlated_fitness_surface(
                data = df_continuous,
                fitness_col = FITNESS,
                trait_cols = pair,
                method = "tps",
                grid_n = 30,
                scale_traits = TRUE
            )

            formula_str <- paste(FITNESS, "~ s(", pair[1], ",", pair[2], ")")
            fitness_model <- mgcv::gam(
                as.formula(formula_str),
                data = df_continuous,
                family = gaussian()
            )

            alt_landscape <- adaptive_landscape(
                data = df_continuous,
                fitness_model = fitness_model,
                trait_cols = pair,
                simulation_n = 400,
                grid_n = 30
            )

            comparison_data <- compare_fitness_surfaces_data(
                correlated_surface = cfs,
                adaptive_landscape = alt_landscape,
                trait_cols = pair
            )

            plots <- plot_fitness_surfaces_comparison(
                comparison_data = comparison_data,
                bins = 10,
                title = paste("Comparison:", paste(pair, collapse = " vs "))
            )

            if (!is.null(plots$side_by_side)) {
                ggsave(
                    file.path(figure_dir, paste0("comparison_", pair_name, "_side_by_side.png")),
                    plots$side_by_side,
                    width = 12,
                    height = 5,
                    dpi = 300
                )
                cat("  Saved: side_by_side\n")
            }

            if (!is.null(plots$overlay)) {
                ggsave(
                    file.path(figure_dir, paste0("comparison_", pair_name, "_overlay.png")),
                    plots$overlay,
                    width = 8,
                    height = 6,
                    dpi = 300
                )
                cat("  Saved: overlay\n")
            }

            cor_val <- cor(comparison_data$combined_data$fitness[comparison_data$combined_data$type == "Correlated Fitness (Individual)"],
                comparison_data$combined_data$fitness[comparison_data$combined_data$type == "Adaptive Landscape (Population)"],
                use = "complete.obs"
            )

            cat("  Correlation between surfaces:", round(cor_val, 4), "\n")

            write.csv(
                data.frame(
                    trait1 = pair[1],
                    trait2 = pair[2],
                    correlation = cor_val,
                    n_points = nrow(comparison_data$combined_data) / 2
                ),
                file.path(table_dir, paste0("correlation_", pair_name, ".csv")),
                row.names = FALSE
            )

            comparison_results[[pair_name]] <- list(
                comparison_data = comparison_data,
                plots = plots,
                correlation = cor_val
            )
        },
        error = function(e) {
            cat("Failed for", pair_name, ":", e$message, "\n")
        }
    )
}

if (length(comparison_results) > 0) {
    cat("\n========================================\n")
    cat("COMPARISON SUMMARY\n")
    cat("========================================\n")

    summary_df <- data.frame()
    for (name in names(comparison_results)) {
        cor_val <- comparison_results[[name]]$correlation
        if (!is.null(cor_val)) {
            cat(name, ": correlation =", round(cor_val, 4), "\n")
            summary_df <- rbind(summary_df, data.frame(
                Trait_Pair = name,
                Correlation = round(cor_val, 4)
            ))
        }
    }

    write.csv(summary_df, file.path(table_dir, "comparison_summary.csv"), row.names = FALSE)
    cat("\nSummary saved to:", file.path(table_dir, "comparison_summary.csv"), "\n")
}

# ======================================================
# 13 Validation
# ======================================================

validate <- function(obj, type) {
    if (is.null(obj)) {
        return(FALSE)
    }

    if (type == "data.frame") {
        return(is.data.frame(obj))
    }
    if (type == "list") {
        return(is.list(obj))
    }
    if (type == "plot") {
        return(inherits(obj, "gg") || inherits(obj, "ggplot"))
    }
    return(TRUE)
}

# Check which objects exist before validating
cat("\nChecking objects for validation:\n")
cat("test_binary exists:", exists("test_binary"), "\n")
cat("test_continuous exists:", exists("test_continuous"), "\n")
cat("univariate_plots exists:", exists("univariate_plots"), "\n")
cat("cfs_plots exists:", exists("cfs_plots"), "\n")
cat("boot exists:", exists("boot"), "\n")
cat("landscape exists:", exists("landscape"), "\n")

# Build tests list safely
tests <- c()

if (exists("test_binary")) {
    tests <- c(tests, validate(test_binary, "data.frame"))
} else {
    tests <- c(tests, FALSE)
}

if (exists("test_continuous")) {
    tests <- c(tests, validate(test_continuous, "data.frame"))
} else {
    tests <- c(tests, FALSE)
}

if (exists("univariate_plots") && length(univariate_plots) > 0) {
    tests <- c(tests, validate(univariate_plots[[1]], "plot"))
} else {
    tests <- c(tests, FALSE)
}

if (exists("cfs_plots") && length(cfs_plots) > 0) {
    tests <- c(tests, validate(cfs_plots[[1]], "plot"))
} else {
    tests <- c(tests, FALSE)
}

if (exists("boot")) {
    tests <- c(tests, validate(boot, "list"))
} else {
    tests <- c(tests, FALSE)
}

if (exists("landscape")) {
    tests <- c(tests, validate(landscape, "list"))
} else {
    tests <- c(tests, FALSE)
}

cat("\nValidation results:", sum(tests), "/", length(tests), "passed\n")

# ======================================================
# 14 Final summary
# ======================================================

cat("\n========================================\n")
cat("FUNCTION TESTING COMPLETE\n")
cat("========================================\n")

cat("\nFunctions loaded:", length(fn_files), "\n")
cat("Plots created:", length(univariate_plots) + length(cfs_plots), "\n")

cat("\nFiles generated:\n")
cat("  Figures:", length(list.files(figure_dir)), "\n")
cat("  Tables:", length(list.files(table_dir)), "\n")
cat("  Models:", length(list.files(model_dir)), "\n")

cat("\nResults saved to:\n")
cat("  Figures:", figure_dir, "\n")
cat("  Tables:", table_dir, "\n")
cat("  Models:", model_dir, "\n")

cat("\n========================================\n")
