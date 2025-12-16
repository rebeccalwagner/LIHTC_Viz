library(ggplot2)
library(tidyr)
library(plotly)

##############################

# import data
data <- read.csv("./data/LIHTCPUB.csv")

##############################

data <- data %>% 
  filter(!is.na(n_units), !is.na(li_units), n_units > li_units, n_units > 0, li_units > 0)

data_pct <- data %>%
  mutate(pct_low_income = (li_units / n_units) * 100)

data_size <- data %>%
  mutate(
    pct_low_income = (li_units / n_units) * 100,
    size_category = case_when(
      n_units < 25 ~ "Small (< 25 units)",
      n_units < 50 ~ "Medium (25-49 units)",
      n_units < 100 ~ "Large (50-99 units)",
      TRUE ~ "Very Large (100+ units)"
    ),
    size_category = factor(size_category, levels = c("Small (< 25 units)", 
                                                     "Medium (25-49 units)",
                                                     "Large (50-99 units)",
                                                     "Very Large (100+ units)"))
  )

##############################
# low income hist

p <- ggplot(data_pct, aes(x = pct_low_income)) +
  geom_histogram(binwidth = 5, fill = "#6D2E46", color = "#3D1A28", alpha = 0.8) +
  geom_vline(xintercept = 100, color = "#143642", linetype = "dashed", size = 0.8) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 100, 20)) +
  labs(
    x = "Percent of Units that are Low-Income",
    y = "Number of Projects",
    title = "Most reported LIHTC projects are 100% low-income housing",
    subtitle = "Distribution of low-income unit share across all LIHTC projects (1986-2023)",
    caption = "Dashed line represents 100% low-income units.\nSource: HUD"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#143642"),
    axis.text = element_text(color = "#143642", size = 10),
    axis.title = element_text(color = "#143642", size = 12),
    plot.title = element_text(color = "#143642", size = 16, face = "bold"),
    plot.subtitle = element_text(color = "#143642", size = 11),
    plot.caption = element_text(color = "#143642", size = 9, hjust = 0)
  )

ggsave("./docs/assets/lowincome_hist.png", plot=p,
       width = 11, 
       height = 6,  
       dpi = 300)


##############################
# violin plot

p <- ggplot(data_size, aes(x = size_category, y = pct_low_income, fill = size_category)) +
  geom_violin(color = "#143642", size = 0.5, alpha = 0.7) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "#143642") +
  scale_fill_manual(values = c("#F5E6EB", "#C59196", "#8B4F5E", "#6D2E46")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    x = "Project Size",
    y = "Percent Low-Income Units",
    title = "Larger projects tend to have more mixed-income housing",
    subtitle = "Distribution of low-income unit percentage by project size",
    caption = "Dashed line represents 100% low-income.\nSource: HUD"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#143642"),
    axis.text = element_text(color = "#143642", size = 10),
    axis.title = element_text(color = "#143642", size = 12),
    plot.title = element_text(color = "#143642", size = 16, face = "bold"),
    plot.subtitle = element_text(color = "#143642", size = 11),
    plot.caption = element_text(color = "#143642", size = 9, hjust = 0),
    legend.position = "none")

ggsave("./docs/assets/lowincome_violin.png", plot=p,
       width = 11, 
       height = 6,  
       dpi = 300)
