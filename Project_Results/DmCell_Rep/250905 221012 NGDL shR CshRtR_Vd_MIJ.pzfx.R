# Required libraries
library(pzfx)
library(janitor)
library(tidyr)
library(dplyr)
library(ggplot2)

# ---- User inputs ----
file_path  <- "C:/Users/Ryohei Yagi/Documents/RY-Science/BTF_Project_Outcome/Project_Results/DmCell_Rep/221012 NGDL shR CshRtR_Vd_MIJ.pzfx"
table_name <- "nluc/fluc_MIJ_Vd"   # <-- change to the table you want
# ---------------------

# 1) Read the wide table with original Prism column names
raw_wide <- read_pzfx(file_path, table = table_name)

# 2) Save both the original and cleaned column names
orig_colnames    <- names(raw_wide)                            # e.g. "M3 only"
cleaned_colnames <- janitor::make_clean_names(orig_colnames)   # e.g. "m3_only"

# 3) Create a cleaned wide table
cleaned_wide <- raw_wide
names(cleaned_wide) <- cleaned_colnames

# Debug: show both sets of column names
message("Original Prism column names:")
print(orig_colnames)
message("Cleaned column names used internally:")
print(cleaned_colnames)

# 4) Convert to long format
long <- cleaned_wide %>%
  mutate(.row = row_number()) %>%
  pivot_longer(-.row, names_to = "variable", values_to = "value") %>%
  select(-.row) %>%
  filter(!is.na(value))

# 5) Keep the original column order (not alphabetical)
ordered_levels <- intersect(cleaned_colnames, unique(long$variable))
if(length(ordered_levels) == 0) {
  ordered_levels <- unique(long$variable)
}
long <- long %>% mutate(variable = factor(variable, levels = ordered_levels))

# 6) Map cleaned names back to original Prism labels
display_labels <- setNames(orig_colnames, cleaned_colnames)
display_labels <- display_labels[names(display_labels) %in% ordered_levels]

# Debug: check the actual x-axis levels
message("Variable levels used for plotting:")
print(levels(long$variable))

# 7) Plot
ggplot(long, aes(x = variable, y = value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.7, color = "darkred", shape = 16, size = 2) +
  scale_x_discrete(labels = display_labels) +
  labs(
    title = "Reporter activity upon shmiR treatment w/ or w/o LA",
    x = "Vd mite injection",
    y = "RLU (target nLuc / control fLuc)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 11),
    plot.title = element_text(hjust = 0.5, size = 14)
  )

