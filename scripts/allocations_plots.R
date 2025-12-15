library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny)
library(plotly)

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

state_per_capita_allocations <- data %>% 
  filter(yr_pis!=8888, 
         yr_pis!=9999, 
         !proj_st %in% c("AS", "GU", "MP", "PR", "VI")) %>% 
  group_by(proj_st, yr_pis) %>% 
  summarise(total_allocamt_real = sum(allocamt_real, na.rm = TRUE),
            pop = first(pop),
            allocation_per_capita = total_allocamt_real/pop)

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
    title = "Total Real LIHTC Allocations Across All States",
    subtitle = "Aggregate allocation amounts over time",
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

# Convert to plotly
ggplotly(p, tooltip = "text") %>%
  layout(plot_bgcolor = "white", paper_bgcolor = "white")

##############################
# Interactive, State Plot

# Get list of states 
states <- sort(unique(state_per_capita_allocations$proj_st))

state_mapping <- data.frame(
  abbr = c(state.abb, "DC"),
  name = c(state.name, "District of Columbia")
)

state_choices <- setNames(
  states,  # values (abbreviations)
  sapply(states, function(x) {
    match_name <- state_mapping$name[state_mapping$abbr == x]
    if(length(match_name) > 0) match_name else x
  })  # names (full names)
)

# set color palette
state_colors <- c("#143642", "#6D2E46", "#0F8B8D", "#A8201A", "#EC9A29", "#2A5F6C", "#8B4F5E","#1BA098","#D4571A","#F4C430") 


##############################
# Shiny app

# UI
ui <- fluidPage(
  titlePanel("LIHTC Allocations by State"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("states", 
                  "Select State(s):", 
                  choices = state_choices,
                  selected = states[1],
                  multiple = TRUE),
      
      radioButtons("metric",
                   "Select Metric:",
                   choices = c("Per Capita Allocation" = "per_capita",
                               "Total Allocation" = "total"),
                   selected = "per_capita"),
      
      helpText("Tip: Hover over points to see exact values")
    ),
    
    mainPanel(
      plotlyOutput("allocationPlot", height = "500px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$allocationPlot <- renderPlotly({
    # Require at least one state selected
    req(input$states)
    
    # Filter data for selected states
    state_data <- state_per_capita_allocations %>%
      filter(proj_st %in% input$states)
    
    # Define colors inside server - expanded palette
    plot_colors <- c("#143642", "#6D2E46", "#0F8B8D", "#A8201A", "#EC9A29",
                     "#2A5F6C", "#8B4F5E", "#1BA098", "#D4571A", "#F4C430")
    
    # Create plot based on selected metric
    if (input$metric == "per_capita") {
      p <- ggplot(state_data, aes(x = yr_pis, y = allocation_per_capita, 
                             color = proj_st, group = proj_st,
                             text = paste0("State: ", proj_st, 
                                          "<br>Year: ", yr_pis,
                                          "<br>Per Capita: $", round(allocation_per_capita, 2)))) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(
          title = "Real LIHTC Allocation Per Capita by State",
          x = "",
          y = "Per Capita Allocation (2025 dollars)",
          color = "State"
        ) +
        scale_y_continuous(labels = scales::dollar) +
        scale_color_manual(values = rep(plot_colors, length.out = length(input$states))) +
        theme_minimal(base_size = 14) +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "#143642"),
          plot.title = element_text(color = "#143642", size = 16, face = "bold"),
          axis.title = element_text(color = "#143642", size = 12),
          axis.text = element_text(color = "#143642"),
          legend.position = "right"
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(plot_bgcolor = "white", paper_bgcolor = "white")
      
    } else {
      # Convert to millions for total allocation
      state_data <- state_data %>%
        mutate(total_allocamt_millions = total_allocamt_real / 1e6)
      
      p <- ggplot(state_data, aes(x = yr_pis, y = total_allocamt_millions, 
                             color = proj_st, group = proj_st,
                             text = paste0("State: ", proj_st,
                                          "<br>Year: ", yr_pis,
                                          "<br>Total: $", round(total_allocamt_millions, 2), "M"))) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(
          title = "Total Real LIHTC Allocation by State",
          x = "",
          y = "Total Allocation (Millions of 2025 dollars)",
          color = "State"
        ) +
        scale_y_continuous(labels = function(x) paste0("$", x, "M")) +
        scale_color_manual(values = rep(plot_colors, length.out = length(input$states))) +
        theme_minimal(base_size = 14) +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "#143642"),
          plot.title = element_text(color = "#143642", size = 16, face = "bold"),
          axis.title = element_text(color = "#143642", size = 12),
          axis.text = element_text(color = "#143642"),
          legend.position = "right"
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(plot_bgcolor = "white", paper_bgcolor = "white")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
