# Load necessary libraries
library(survival)
library(ggplot2)

# Define hazard functions for the distributions

# Log-Logistic Distribution (Using 'b' as parameter)
hazard_loglogistic <- function(t, b) {
  (b * t^(b - 1)) / (1 + t^b)
}

# Log-normal Distribution (Using 'b' for sdlog variation)
hazard_lognormal <- function(t, meanlog, b) {
  dlnorm(t, meanlog, b) / (1 - plnorm(t, meanlog, b))
}

# Gompertz Distribution (Using 'b' as the shape parameter)
hazard_gompertz <- function(t, b, rate) {
  b * exp(b * t) * exp(-rate * (exp(b * t) - 1) / b)
}

# Gompertz-Makeham Distribution (Using 'b' for shape, rate as parameter)
hazard_gompertz_makeham <- function(t, b, rate, constant) {
  hazard_gompertz(t, b, rate) + constant
}

# Time range for plotting
time <- seq(0.1, 10, by = 0.1)

# Log-Logistic: Varying 'b' parameter
loglogistic_data <- data.frame(
  Time = rep(time, 3),
  Hazard = c(
    hazard_loglogistic(time, b = 0.5),
    hazard_loglogistic(time, b = 1),
    hazard_loglogistic(time, b = 2)
  ),
  Parameters = rep(c("b=0.5", "b=1", "b=2"), each = length(time))
)

# Log-normal: Varying 'b' (sdlog) parameter
lognormal_data <- data.frame(
  Time = rep(time, 3),
  Hazard = c(
    hazard_lognormal(time, meanlog = 0, b = 0.5),
    hazard_lognormal(time, meanlog = 0, b = 1),
    hazard_lognormal(time, meanlog = 0, b = 1.5)
  ),
  Parameters = rep(c("b=0.5", "b=1", "b=1.5"), each = length(time))
)

# Gompertz: Varying 'b' (shape) parameter
gompertz_data <- data.frame(
  Time = rep(time, 3),
  Hazard = c(
    hazard_gompertz(time, b = 0.5, rate = 0.01),
    hazard_gompertz(time, b = 1, rate = 0.01),
    hazard_gompertz(time, b = 2, rate = 0.01)
  ),
  Parameters = rep(c("b=0.5", "b=1", "b=2"), each = length(time))
)

# Gompertz-Makeham: Varying 'b' (shape) and 'rate' parameter
gompertz_makeham_data <- data.frame(
  Time = rep(time, 3),
  Hazard = c(
    hazard_gompertz_makeham(time, b = 0.5, rate = 0.01, constant = 0.02),
    hazard_gompertz_makeham(time, b = 1, rate = 0.01, constant = 0.02),
    hazard_gompertz_makeham(time, b = 2, rate = 0.01, constant = 0.02)
  ),
  Parameters = rep(c("b=0.5", "b=1,", "b=2"), each = length(time))
)

# Define a function to plot variations for a distribution
plot_variations <- function(data, title) {
  ggplot(data, aes(x = Time, y = Hazard, color = Parameters)) +
    geom_line(size = 1.2) +
    theme_minimal() +
    labs(
      title = title,
      x = "Time (t)",
      y = "Hazard (h(t))",
      color = "Parameters"
    ) +
    theme(text = element_text(size = 14))
}

# Plot each distribution separately

# Log-Logistic Distribution
plot_loglogistic <- plot_variations(loglogistic_data, "Log-Logistic Distribution")
print(plot_loglogistic)

# Log-normal Distribution
plot_lognormal <- plot_variations(lognormal_data, "Log-normal Distribution")
print(plot_lognormal)

# Gompertz Distribution
plot_gompertz <- plot_variations(gompertz_data, "Gompertz Distribution ")
print(plot_gompertz)

# Gompertz-Makeham Distribution
plot_gompertz_makeham <- plot_variations(gompertz_makeham_data, "Gompertz-Makeham Distribution ")
print(plot_gompertz_makeham)


