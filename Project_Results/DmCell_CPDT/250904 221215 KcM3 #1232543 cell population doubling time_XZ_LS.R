library(pzfx)
data <- read_pzfx("C:/Users/Ryohei Yagi/Documents/RY-Science/BTF_Project_Outcome/Project_Results/DmCell_CPDT/221215 KcM3 #1232543 cell population doubling time_XZ.pzfx")
names(data)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
library(tidyverse)

# Extract vectors directly from your original data
x <- data[["passage number"]]
y0 <- data[["0% FEx"]]
y1 <- data[["1%FEx"]]

# Plot both series using ggplot with loess smoothing
ggplot() +
  geom_point(aes(x = x, y = y0, color = "0% FEx"), size = 3, shape = 16) +
  geom_point(aes(x = x, y = y1, color = "1% FEx"), size = 3, shape = 15) +
  geom_line(aes(x = x, y = y0, color = "0% FEx"), linewidth = 0.5) +
  geom_line(aes(x = x, y = y1, color = "1% FEx"), linewidth = 0.5) +
  geom_smooth(aes(x = x, y = y0, color = "0% FEx"), method = "loess", se = FALSE, linetype = "solid", linewidth = 1, span = 0.75) +
  geom_smooth(aes(x = x, y = y1, color = "1% FEx"), method = "loess", se = FALSE, linetype = "solid", linewidth = 1, span = 0.75) +
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

