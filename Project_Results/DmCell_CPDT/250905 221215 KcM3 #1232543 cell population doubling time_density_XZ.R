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

library(tidyverse)

# Build a long data frame with only the two conditions
df <- tibble(
  `0% FEx` = y0,
  `1% FEx` = y1
) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Condition",
    values_to = "DoublingTime"
  ) %>%
  filter(!is.na(DoublingTime))

# Plot density curves + raw dots
ggplot(df, aes(x = DoublingTime, color = Condition, fill = Condition)) +
  geom_density(alpha = 0.3, linewidth = 1.2) +   # smooth distribution
  geom_point(
    aes(y = 0),
    position = position_jitter(height = 0.005),
    size = 2,
    alpha = 0.8
  ) + # dots along baseline
  labs(
    title = "Distribution of cell population doubling time",
    x = "Population doubling time (hr)",
    y = "Density",
    color = "FEx Condition",
    fill = "FEx Condition"
  ) +
  scale_color_manual(values = c("0% FEx" = "#1b9e77", "1% FEx" = "#d95f02")) +
  scale_fill_manual(values = c("0% FEx" = "#1b9e77", "1% FEx" = "#d95f02")) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.line = element_line(color = "black", linewidth = 0.8),   # solid x and y axes
    panel.grid.major = element_line(color = "grey85"),            # keep grid
    panel.grid.minor = element_line(color = "grey93")             # subtle minor grid
  )
