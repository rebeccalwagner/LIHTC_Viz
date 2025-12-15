library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

##############################

# import data
data <- read.csv("./data/LIHTCPUB.csv")



##############################
# number of total projects across all years 
total_units <- data %>% 
  filter(!yr_pis %in% c("8888","9999")) %>% 
  group_by(yr_pis) %>% 
  summarize(n_ids = n_distinct(hud_id), total_units = sum(n_units, na.rm=T)) 

##############################
# plot

p <- total_units %>% 
  ggplot(aes(x = yr_pis, y = n_ids, group = 1)) +
  geom_line(color = "#143642", size = 1.2) +
  # Lines for Important Events
  geom_vline(xintercept = c(1986, 1993, 2005, 2008, 2009, 2017, 2025), 
             color = "#6D2E46", 
             alpha = 0.6) +
  # Lines for Axis Years
  geom_vline(xintercept = c(1990, 2000, 2010, 2020), 
             color = "grey", 
             linetype="dashed",
             alpha = 0.3) +
  # Text Labels for Important Events
  annotate("label", x = 1986, y = max(total_units$n_ids) * 0.95, 
           label = "1986\nThe Tax Reform Act\npasses, containing\nLIHTC as a\ntemporary program.", 
           hjust = 0, vjust=-.4, size = 3, color = "#6D2E46", 
           fill = "#F5E6EB", label.padding = unit(0.4, "lines")) +
  annotate("label", x = 1993, y = max(total_units$n_ids) * 0.85, 
           label = "1993\nLIHTC becomes a\npermanent program.", 
           hjust = 0, vjust=-.43, size = 3, color = "#6D2E46", 
           fill = "#F5E6EB", label.padding = unit(0.4, "lines")) +
  annotate("label", x = 2005, y = max(total_units$n_ids) * 0.80, 
           label = "2005\nHurricane Katrina strikes,\nand LIHTC is used to help\n rebuild in damaged states.", hjust=1, vjust=1.3, size = 3, color = "#6D2E46", 
           fill = "#F5E6EB", label.padding = unit(0.4, "lines")) +
  annotate("label", x = 2008, y = max(total_units$n_ids) * 0.75, 
           label = "2008\nThe great recession causes demand\nfor tax credits to tank. The Housing\nand Economic Recovery Act increases\nper capita tax credit allocation to states.", hjust = 1, vjust=2, size = 3, color = "#6D2E46", 
           fill = "#F5E6EB", label.padding = unit(0.4, "lines")) +
  annotate("label", x = 2009, y = max(total_units$n_ids) * 0.70, 
           label = "2009\nThe Tax Credit Assistance Program\n& Tax Credit Exchange Program\nprovide federal grants to fill\nfinancing gaps in stalled projects.", hjust = 0, vjust=2.1, size = 3, color = "#6D2E46", 
           fill = "#F5E6EB", label.padding = unit(0.4, "lines")) +
  annotate("label", x = 2017, y = max(total_units$n_ids) * 0.65, 
           label = "2017\nThe Corporate tax rate is\nlowered, reducing tax liabilities\nand corporation's demand for\ntax credits.", hjust = 0, vjust=-1.3, size = 3, color = "#6D2E46", 
           fill = "#F5E6EB", label.padding = unit(0.4, "lines")) +
  annotate("label", x = 2025, y = max(total_units$n_ids) * 0.60, 
           label = "2025\n OBBA increases total LIHTC\nallocations and reduces\nbond financing requirements.", hjust = 1, vjust=-.65, size = 3, color = "#6D2E46", 
           fill = "#F5E6EB", label.padding = unit(0.4, "lines")) +
  # Title & Labels
  labs(title = "History of the Low Income Housing Tax Credit",
       subtitle = "Takeup of the Tax Credit has historically been responsive to market conditions,\npolicy changes, and national events.",
       caption = "Projects may contain many units across one or more buildings. Projects with no associated year are ommitted.\nSource: HUD",
       x = "",
       y = "Number of LIHTC Projects") +
  # Increase y scale to have room for labels
  scale_y_continuous(limits = c(0, max(total_units$n_ids) * 1.4)) +
  # themes
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#143642"),
    plot.title = element_text(color = "#143642", size = 16, face = "bold"),
    axis.title = element_text(color = "#143642", size = 12),
    axis.text = element_text(color = "#143642")
  )

ggsave("./docs/assets/history_line_graph.png", plot=p)

