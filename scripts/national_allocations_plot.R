library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny)
library(plotly)
library(htmlwidgets)
library(rsconnect)

##############################
# import data
data <- read.csv("./data/LIHTCPUB.csv")
cpi <- read.csv("./data/CPIAUCSL.csv")
population <- read.csv("./data/historical_state_population_by_year.csv", header=F, col.names = c("state","year","pop"))

##############################
# calculate real dollar values 

# aggregate cpi values by years
cpi$year <- as.integer(substr(cpi$observation_date, 1, 4)) # extract year

cpi_year_mean <- cpi %>% 
  group_by(year) %>% 
  summarise(CPIAUCSL = mean(CPIAUCSL)) 

# add cpi for the year to df
data <- data %>% 
  left_join(cpi_year_mean, by = c("yr_pis" = "year"))

# get base 2025 cpi val
cpi_base <- cpi_year_mean %>% 
  filter(year == 2025) %>% 
  pull(CPIAUCSL)

# calculate real value
data$allocamt_real <- data$allocamt * (cpi_base / data$CPIAUCSL)

##############################
# Calculate State Populations

# merge
data <- data %>%
  left_join(population, by = c("proj_st" = "state", "yr_pis" = "year"))

##############################
# Calculate total allocations per capita for each state and year

overall_allocations <- data %>% 
  filter(yr_pis!=8888, 
         yr_pis!=9999, 
         !proj_st %in% c("AS", "GU", "MP", "PR", "VI")) %>% 
  group_by(yr_pis) %>% 
  summarise(total_allocamt_real = sum(allocamt_real, na.rm=T)) %>% 
  mutate(total_allocamt_real_bil = total_allocamt_real/1000000000)

##############################
# Static, National Plot

p <- ggplot(overall_allocations, aes(x = yr_pis, y = total_allocamt_real_bil, group = 1,
                                     text = paste0(yr_pis,
                                                   "<br>",
                                                   round(total_allocamt_real_bil, 2), "B Allocated"))) +
  geom_line(color = "#143642", size = 1.2) +
  geom_point(color = "#143642", size = 2) +
  labs(
    title = "LIHTC Allocations Have Increased Over Time",
    subtitle = "Total allocations across all 50 states and DC, adjusted to 2025 dollars",
    caption = "Projects with no associated year or reported allocations are ommitted.\nSource: HUD",
    x = "",
    y = "Total Allocation (Billions of 2025 dollars)"
  ) +
  scale_y_continuous(labels = function(x) paste0("$", x, "B")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#143642"),
    plot.title = element_text(color = "#143642", size = 16, face = "bold"),
    plot.subtitle = element_text(color = "#143642", size = 12),
    axis.title = element_text(color = "#143642", size = 12),
    axis.text = element_text(color = "#143642")
  )

p

ggsave("./docs/assets/national_allocations.png", 
       plot=p,
       width = 11, 
       height = 5.625,  
       dpi = 300)

# Convert to plotly
ggplotly(p, tooltip = "text") %>%
  layout(plot_bgcolor = "white", paper_bgcolor = "white")

# Save to html for website embed
saveWidget(ggplotly(p, tooltip = "text"), "./docs/assets/national_allocations.html")


