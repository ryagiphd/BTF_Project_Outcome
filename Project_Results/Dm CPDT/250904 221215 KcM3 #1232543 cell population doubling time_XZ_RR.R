library(pzfx)
data <- read_pzfx("C:/Users/Ryohei Yagi/Documents/RY-Science/BTF_Project_Outcome/Project_Results/Dm CPDT/221215 KcM3 #1232543 cell population doubling time_XZ.pzfx")
names(data)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

library(tidyverse)

# Extract vectors directly from your original data
x <- data[["passage number"]]
y0 <- data[["0% FEx"]]
y1 <- data[["1%FEx"]]

# Fit linear models
lm_model <- lm(y0 ~ x)
lm_summary <- summary(lm_model)
r_squared <- round(lm_summary$r.squared, 3)

lm_model_1 <- lm(y1 ~ x)
lm_summary_1 <- summary(lm_model_1)
r_squared_1 <- round(lm_summary_1$r.squared, 3)

# Fit nonlinear model to 0% FEx
nls_model <- nls(y0 ~ a * exp(b * x), start = list(a = max(y0), b = -0.1))

# Generate predictions
x_seq <- seq(min(x), max(x), length.out = 100)
lm_pred <- predict(lm_model, newdata = data.frame(x = x_seq))
lm_pred_1 <- predict(lm_model_1, newdata = data.frame(x = x_seq))
nls_pred <- predict(nls_model, newdata = data.frame(x = x_seq))

# Create tibble for regression lines
regression_df <- tibble(
  x = x_seq,
  lm_fit = lm_pred,
  lm_fit_1 = lm_pred_1,
  nls_fit = nls_pred
)

# Plot both series with regression overlays and R² annotations
ggplot() +
  geom_point(aes(x = x, y = y0, color = "0% FEx"), size = 3, shape = 16) +
  geom_point(aes(x = x, y = y1, color = "1% FEx"), size = 3, shape = 15) +
  geom_line(aes(x = x, y = y0, color = "0% FEx"), linewidth = 1) +
  geom_line(aes(x = x, y = y1, color = "1% FEx"), linewidth = 1) +
  geom_line(data = regression_df, aes(x = x, y = lm_fit), color = "#1b9e77", linetype = "dashed", linewidth = 1) +
  geom_line(data = regression_df, aes(x = x, y = nls_fit), color = "#1b9e77", linetype = "dotdash", linewidth = 1) +
  geom_line(data = regression_df, aes(x = x, y = lm_fit_1), color = "#d95f02", linetype = "dashed", linewidth = 1) +
  annotate("text", x = max(x) - 2, y = max(y0) - 10, label = paste("0% FEx R² =", r_squared), color = "#1b9e77", size = 5, hjust = 1) +
  annotate("text", x = max(x) - 2, y = max(y1) - 10, label = paste("1% FEx R² =", r_squared_1), color = "#d95f02", size = 5, hjust = 1) +
  labs(
    title = "KcM3 culture in M3 w or w/o FEx",
    x = "Passage Number",
    y = "Population doubling time (hr)",
    color = "FEx Condition"
  ) +
  scale_color_manual(
    values = c("0% FEx" = "#1b9e77", "1% FEx" = "#d95f02"),
    labels = c("0% FEx" = "No FEx", "1% FEx" = "With 1% FEx")
  ) +
  xlim(0, 16) +
  ylim(0, 180) +
  theme_minimal() +
  theme(legend.position = "right")