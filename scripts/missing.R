library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(DT)

##############################
# give column names readable labels
variable_labels <- c(
  # Basic Info
  "project" = "Project Name",
  "proj_add" = "Project Address",
  "proj_cty" = "Project City",
  "proj_st" = "Project State",
  "proj_zip" = "Project ZIP Code",
  "state_id" = "State Project ID",
  
  # Geographic
  "latitude" = "Latitude",
  "longitud" = "Longitude",
  "fips1990" = "1990 Census Tract",
  "fips2000" = "2000 Census Tract",
  "fips2010" = "2010 Census Tract",
  "fips2020" = "2020 Census Tract",
  "place1990" = "1990 Place Code",
  "place2000" = "2000 Place Code",
  "place2010" = "2010 Place Code",
  "place2020" = "2020 Place Code",
  
  # Unit Counts
  "n_units" = "Total Units",
  "li_units" = "Low-Income Units",
  "n_0br" = "Efficiency Units",
  "n_1br" = "1-Bedroom Units",
  "n_2br" = "2-Bedroom Units",
  "n_3br" = "3-Bedroom Units",
  "n_4br" = "4-Bedroom Units",
  "n_unitsr" = "Total Units (Reconciled)",
  "li_unitr" = "Low-Income Units (Reconciled)",
  "ceilunit" = "Units Below Income Ceiling",
  
  # Dates
  "yr_pis" = "Year Placed in Service",
  "yr_alloc" = "Allocation Year",
  
  # Financing Sources
  "bond" = "Tax-Exempt Bond",
  "home" = "HOME Funds",
  "cdbg" = "CDBG Funds",
  "htf" = "Housing Trust Fund",
  "fha" = "FHA-Insured Loan",
  "hopevi" = "HOPE VI Funds",
  "tcap" = "TCAP Funds",
  "tcep" = "TCEP Funds",
  "rad" = "RAD Funds",
  "qozf" = "Opportunity Zone Funds",
  "mff_ra" = "HUD Multifamily Financing",
  "fmha_514" = "RHS Section 514 Loan",
  "fmha_515" = "RHS Section 515 Loan",
  "fmha_538" = "RHS Section 538 Loan",
  
  # Funding Amounts
  "allocamt" = "Tax Credit Allocation Amount",
  "home_amt" = "HOME Funding Amount",
  "cdbg_amt" = "CDBG Funding Amount",
  "htf_amt" = "Housing Trust Fund Amount",
  "hpvi_amt" = "HOPE VI Amount",
  "tcap_amt" = "TCAP Amount",
  "tcep_amt" = "TCEP Amount",
  "qozf_amt" = "Opportunity Zone Amount",
  
  # Target Population
  "trgt_pop" = "Targets Specific Population",
  "trgt_fam" = "Targets Families",
  "trgt_eld" = "Targets Elderly",
  "trgt_dis" = "Targets Disabled",
  "trgt_hml" = "Targets Homeless",
  "trgt_oth" = "Targets Other Population",
  "trgt_spc" = "Other Target Population (Specified)",
  
  # Program Details
  "type" = "Construction Type",
  "credit" = "Credit Type",
  "non_prof" = "Nonprofit Sponsor",
  "basis" = "Increase in Eligible Basis",
  "rentasst" = "Rental Assistance Contract",
  "inc_ceil" = "Income Ceiling Election",
  "low_ceil" = "Lower Income Units",
  "aff_period" = "Extended Affordability Period",
  "aff_yrs" = "Affordability Period (Years)",
  "scattered" = "Scattered Site Property",
  "resynd" = "Resyndicated Property",
  
  # Location Characteristics
  "metro" = "Metro/Non-Metro Status",
  "dda" = "Difficult Development Area",
  "qct" = "Qualified Census Tract",
  
  # Status
  "nonprog" = "No Longer Monitored",
  "nlm_reason" = "Reason Not Monitored",
  "nlm_spc" = "Not Monitored Reason (Specified)",
  "datanote" = "Data Notes",
  "record_stat" = "Record Status"
)

##############################

# Calculate missing percentages and apply labels
missing_by_category <- data %>%
  select(-nlm_reason, -nlm_spc, -datanote, -resyndication_cd) %>% 
  summarise(across(everything(), ~sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") %>%
  filter(pct_missing > 0) %>%
  mutate(
    # Apply readable labels
    variable_label = ifelse(variable %in% names(variable_labels), 
                            variable_labels[variable], 
                            variable),
    # Categorize variables
    category = case_when(
      variable %in% c("project", "proj_add", "proj_cty", "proj_st", "proj_zip", "state_id","yr_pis") ~ "Basic Info",
      variable %in% c("latitude", "longitud", "fips1990", "fips2000", "fips2010", "fips2020",
                      "place1990", "place2000", "place2010", "place2020") ~ "Geographic",
      variable %in% c("n_units", "li_units", "n_0br", "n_1br", "n_2br", "n_3br", "n_4br",
                      "n_unitsr", "li_unitr", "ceilunit") ~ "Unit Details",
      variable %in% c("bond", "home", "cdbg", "htf", "fha", "hopevi", "tcap", "tcep", "rad", 
                      "qozf", "mff_ra", "fmha_514", "fmha_515", "fmha_538") ~ "Funding Sources",
      variable %in% c("trgt_pop", "trgt_fam", "trgt_eld", "trgt_dis", "trgt_hml", "trgt_oth", "trgt_spc") ~ "Target Population",
      variable %in% c("yr_alloc", "allocamt", "home_amt", "cdbg_amt", "htf_amt", "hpvi_amt", "tcap_amt", 
                      "tcep_amt", "qozf_amt") ~ "Funding Amounts",
      variable %in% c("type", "credit", "non_prof", "basis", "rentasst", "inc_ceil", "low_ceil",
                      "aff_period", "aff_yrs", "scattered", "resynd") ~ "Project Details",
      variable %in% c("metro", "dda", "qct") ~ "Location Characteristics",
      variable %in% c("nonprog", "nlm_reason", "nlm_spc", "datanote", "record_stat") ~ "Status",
      TRUE ~ "Other"
    )
  ) %>%
  arrange(category, desc(pct_missing))

# Create the plot
ggplot(missing_by_category, aes(x = reorder(variable_label, pct_missing), 
                                y = pct_missing, fill = category)) +
  geom_col(color = "#143642", alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = c("#143642", "#6D2E46", "#0F8B8D", "#A8201A", "#EC9A29", 
                               "#2A5F6C", "#8B4F5E", "#1BA098", "#D4571A", "#7A9B76")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    x = NULL,
    y = "Percent Missing",
    fill = "Variable Type",
    title = "Missing data patterns in LIHTC database",
    subtitle = "Data on LIHTC Projects is often not reported well.",
    caption = "Only variables with missing data are shown.\nSource: HUD LIHTC Database (1987-2023)"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#143642"),
    axis.text.y = element_text(color = "#143642", size = 8),
    axis.text.x = element_text(color = "#143642", size = 10),
    axis.title = element_text(color = "#143642", size = 12),
    plot.title = element_text(color = "#143642", size = 16, face = "bold"),
    plot.subtitle = element_text(color = "#143642", size = 11),
    plot.caption = element_text(color = "#143642", size = 9, hjust = 0),
    legend.position = "bottom",
    legend.title = element_text(color = "#143642", face = "bold"),
    legend.text = element_text(color = "#143642")
  )

ggsave("./docs/assets/missing_plot.png")