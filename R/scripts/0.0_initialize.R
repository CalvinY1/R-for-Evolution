# ======================================================
# 0.0_initialize.R
# Project initialization script
# ======================================================

cat("\n========================================\n")
cat("Initializing R Environment\n")
cat("========================================\n")

# ------------------------------------------------------
# 1. Set reproducibility seed
# ------------------------------------------------------

set.seed(42)

# ------------------------------------------------------
# 2. Load required packages
# ------------------------------------------------------

required_packages <- c(
    "mgcv", # GAM models
    "fields", # Thin plate splines (Tps)
    "MASS", # multivariate normal simulation
    "Matrix", # matrix operations
    "ggplot2", # plotting
    "dplyr", # data manipulation
    "tidyr", # data tidying
    "here", # robust path handling
    "ggrepel", # for text labels in plots
    "viridis"
)

load_or_install <- function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        library(pkg, character.only = TRUE)
    }
}

invisible(lapply(required_packages, load_or_install))
cat("Packages loaded\n")

# ------------------------------------------------------
# 5. Print environment info
# ------------------------------------------------------
cat("\nR version:\n")
print(R.version.string)
