# Required libraries
library(pzfx)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(rstatix)

# ---- User inputs ----
file_path  <- "C:/Users/Ryohei Yagi/Documents/RY-Science/BTF_Project_Outcome/Project_Results/DmCell_Rep/221012 NGDL shR CshRtR_Vd_MIJ.pzfx"
table_name <- "min nluc/fluc_MIJ_Vd"   # Table to plot
# ---------------------

# 1) Read the wide table with original Prism column names
raw_wide <- read_pzfx(file_path, table = table_name)

# 2) Keep original column names
orig_colnames <- names(raw_wide)

# 3) Convert to long format
long <- raw_wide %>%
  mutate(.row = row_number()) %>%
  pivot_longer(-.row, names_to = "variable", values_to = "value") %>%
  select(-.row) %>%
  filter(!is.na(value))

# 4) Preserve original order
ordered_levels <- intersect(orig_colnames, unique(long$variable))
long <- long %>% mutate(variable = factor(variable, levels = ordered_levels))

# 5) Optional: set custom x-axis labels for visualization
display_labels <- c(
  "ctl"                  = "Control",
  "repLZ+VdshmiR1GFP"    = "VdshmiR1Gfp>RepLZ",
  "repLZ+VdshmiR1LZ"     = "VdshmiR1Lz>RepLZ"
)
display_labels <- display_labels[names(display_labels) %in% ordered_levels]

# 6) Optional: set colors and shapes per variable
point_colors <- c(
  "ctl"                  = "black",
  "repLZ+VdshmiR1GFP"    = "blue",
  "repLZ+VdshmiR1LZ"     = "red"
)
point_shapes <- c(
  "ctl"                  = 16,  # circle
  "repLZ+VdshmiR1GFP"    = 17,  # triangle
  "repLZ+VdshmiR1LZ"     = 18   # diamond
)

# 7) Check normality per group
shapiro_results <- long %>%
  group_by(variable) %>%
  summarise(p_value = if(length(value) >= 3) shapiro_test(value)$p.value else NA)

parametric <- all(shapiro_results$p_value >= 0.05, na.rm = TRUE)

# 8) Perform overall and pairwise tests
if(parametric){
  overall_test <- anova_test(data = long, dv = value, wid = NULL, between = variable)
  pairwise_df <- long %>% tukey_hsd(value ~ variable) %>%
    select(group1, group2, p.adj) %>% rename(p = p.adj)
  overall_label <- paste0("ANOVA p = ", signif(overall_test$p,3))
} else {
  overall_test <- kruskal_test(value ~ variable, data = long)
  pairwise_df <- long %>% dunn_test(value ~ variable, p.adjust.method = "BH") %>%
    select(group1, group2, p.adj) %>% rename(p = p.adj)
  overall_label <- paste0("Kruskal-Wallis p = ", signif(overall_test$p,3))
}

# 9) Add significance labels with larger spacing near top
max_y <- max(long$value, na.rm = TRUE)
n_comparisons <- nrow(pairwise_df)
spacing <- max_y * 0.15  # increase spacing between bars

pairwise_df <- pairwise_df %>%
  add_significance("p") %>%
  mutate(
    y.position = max_y * 1.05 + (1:n_comparisons - 1) * spacing
  )


# 10) Plot with updated x-axis alignment
ggplot(long, aes(x = variable, y = value)) +
  geom_boxplot(outlier.shape = NA, color = "black") +
  geom_jitter(aes(color = variable, shape = variable), width = 0.2, alpha = 0.7, size = 2) +
  scale_x_discrete(labels = display_labels) +
  scale_color_manual(values = point_colors, labels = display_labels, name = "") +
  scale_shape_manual(values = point_shapes, labels = display_labels, name = "") +
  labs(
    title = "Reporter activity upon shmiR injection",
    subtitle = overall_label,
    x = "Vd mite injection",
    y = "RLU (target nLuc / control fLuc)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, size = 10),  # updated vjust
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.line = element_line(color = "black", size = 0.8),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    legend.position = "none"
  ) +
  stat_pvalue_manual(
    pairwise_df,
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    label = "p.signif",
    tip.length = 0.02
  )

