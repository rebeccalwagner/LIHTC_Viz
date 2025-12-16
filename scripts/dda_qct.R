library(tidyverse)
library(scales)

####################
# Load the data

lihtc <- read_csv("./data/LIHTCPUB.csv")

####################
# create area labels from dda and qct

lihtc_location <- lihtc %>%
  filter(yr_pis >= 1987 & yr_pis <= 2023) %>%
  filter(!is.na(qct)) %>%
  mutate(
    area_type = case_when(
      dda %in% c(1, 2, 3, 4) & qct == 1 ~ "Both High-Cost & Low-Income",
      dda %in% c(1, 2, 3, 4) & qct == 2 ~ "High-Cost Only",
      dda == 0 & qct == 1 ~ "Low-Income Only",
      dda == 0 & qct == 2 ~ "Neither",
      TRUE ~ "Unknown"
    ),
    area_type = factor(area_type, 
                       levels = c("Both High-Cost & Low-Income", "High-Cost Only", 
                                  "Low-Income Only", "Neither", "Unknown"))
  )

# inspect
table(lihtc_location$area_type)

####################
# data to plot

# Calculate percentages
location_pct <- lihtc_location %>%
  group_by(yr_pis, area_type) %>%
  summarise(n_projects = n(), .groups = "drop") %>%
  complete(yr_pis, area_type, fill = list(n_projects = 0)) %>%
  group_by(yr_pis) %>%
  mutate(pct = n_projects / sum(n_projects) * 100)
# make sure they sum to 100
check_pct <- location_pct %>% 
  group_by(yr_pis) %>% 
  summarise((sum(pct)))

####################
# plot

p <- ggplot(location_pct, aes(x=yr_pis, y= pct, fill=area_type)) + 
  geom_area(position="stack") +
  scale_fill_manual(values = c("Both High-Cost & Low-Income" = "#6D2E46",
                                                           "High-Cost Only" = "#C17C8F",
                                                           "Low-Income Only" = "#5A8F8A",
                                                           "Neither" = "#143642")) +
    # Add direct labels
    annotate("text", x = 2010, y = 97, label = "High-Cost & Low-Income", 
             color = "white", size = 3) +
    annotate("text", x = 2003, y = 86, label = "High-Cost Only", 
             color = "white", size = 3) +
    annotate("text", x = 2007, y = 64, label = "Low-Income Only", 
             color = "white", size = 3) +
    annotate("text", x = 2005, y = 35, label = "Neither", 
             color = "white", size = 3) +
    annotate("text", x = 2004, y = 8, label = "Unknown", 
             color = "white", size = 3) +
    # labels
    labs(
    title = "Where Are LIHTC Projects Being Built?",
    subtitle = "Comparing location in high-cost areas (DDAs) versus low-income areas (QCTs).\nProjects in both categories face the greatest challenges but serve the greatest need.",
    caption = "DDA = Difficult Development Area (high construction costs) | QCT = Qualified Census Tract (low income).\nSource: HUD LIHTC Database",
    x = "",
    y = "Percentage of LIHTC Projects",
    fill = "Area Type") +
  # theming
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#143642"),
    plot.title = element_text(color = "#143642", size = 16, face = "bold"),
    plot.subtitle = element_text(color = "#143642", size = 11),
    plot.caption = element_text(color = "#143642", size = 9, hjust = 0),
    axis.title = element_text(color = "#143642", size = 12),
    axis.text = element_text(color = "#143642"),
    legend.position = "None"
  )

p

ggsave("./docs/assets/dda_qct.png",
       plot=p,
       width = 11, 
       height = 6,  
       dpi = 300)

