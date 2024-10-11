# GabrielRibeiro_RM12345_fase2_cap9

data <- read.csv("data.csv")

# Selecting the continuous quantitative variable - Total Emissions
total_emissions <- data$Total_Emissions

# 1. Measures of Central Tendency
mean_value <- mean(total_emissions)
median_value <- median(total_emissions)
mode_value <- as.numeric(
  names(sort(table(total_emissions), decreasing = TRUE)[1])
)

cat("1. Measures of Central Tendency:\n")
cat(sprintf("Mean: %.2f\n", mean_value))
cat(sprintf("Median: %.2f\n", median_value))
cat(sprintf("Mode: %.2f\n\n", mode_value))

# 2. Measures of Dispersion
variance_value <- var(total_emissions)
standard_deviation <- sd(total_emissions)
range_value <- max(total_emissions) - min(total_emissions)

cat("2. Measures of Dispersion:\n")
cat(sprintf("Variance: %.2f\n", variance_value))
cat(sprintf("Standard Deviation: %.2f\n", standard_deviation))
cat(sprintf("Range: %.2f\n\n", range_value))

# 3. Measures of Separation
quartiles <- quantile(total_emissions)
percentiles <- quantile(
  total_emissions,
  probs = seq(0, 1, 0.1)
) # Percentiles from 10% to 10%

cat("3. Measures of Separation:\n")
cat("Quartiles:\n")
print(quartiles)

cat("\nPercentiles:\n")
print(percentiles)
