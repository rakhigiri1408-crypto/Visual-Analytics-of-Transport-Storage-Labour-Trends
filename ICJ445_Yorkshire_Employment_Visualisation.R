############################################################
# ICJ445 – Data Visualization
# Transport & Storage Employment in Yorkshire (2015–2024)
############################################################
# STEP 1:  Install  required packages
############################

install.packages("tidyverse") # For data manipulation and visualization
install.packages("janitor")   # For cleaning messy data/column names
install.packages("GGally")    # Extension for ggplot2 (Parallel coordinates)
install.packages("viridis")   # Color palettes for better visibility
install.packages("stringr")   # For working with text/strings
install.packages("readr")     # For reading CSV and other data files

############################################################
# STEP 2: Load required packages
############################

library(tidyverse)   
library(janitor)  
library(GGally)      
library(viridis)     
library(stringr)     
library(readr)       

############################
# STEP 3: Import raw data set
############################

# Skip metadata rows as done in Data Science assignment
raw_data <- read_csv("Data.csv", skip = 8)

############################
# STEP 4: Initial cleaning (Data Science style)
############################

clean_data <- raw_data %>%
  clean_names() %>%
  rename(area_name = x1) %>%
  mutate(across(everything(), as.character))

###########################
# STEP 5: Convert to long format
############################

long_data <- clean_data %>%
  pivot_longer(
    cols = -area_name,
    names_to = "year_index",
    values_to = "employment_count"
  )

############################
# STEP 6: Extract year & clean numeric values
############################

final_data <- long_data %>%
  mutate(
    year_num = as.numeric(str_extract(year_index, "[0-9]+")),
    year = year_num + 2014,
    employment_count = suppressWarnings(parse_number(employment_count))
  ) %>%
  filter(
    !is.na(employment_count),
    year >= 2015,
    year <= 2024
  )

############################
# STEP 7: DISTRICTS 
############################

final_data <- final_data %>%
  filter(
    area_name %in% c(
      "Barnsley",
      "Bradford",
      "Calderdale",
      "Doncaster",
      "East Riding of Yorkshire",
      "Kingston upon Hull, City of",
      "Kirklees",
      "Leeds",
      "North East Lincolnshire",
      "North Lincolnshire",
      "Rotherham",
      "Sheffield",
      "Wakefield",
      "York"
    )
  )

############################
# STEP 8: Final summary (district × year)
############################

final_summary <- final_data %>%
  group_by(area_name, year) %>%
  summarise(
    total_employment = sum(employment_count, na.rm = TRUE),
    .groups = "drop"
  )

############################
# DATA CHECK (Should be 14 districts)
############################

print("Number of districts:")
print(length(unique(final_summary$area_name)))

print("District names:")
print(sort(unique(final_summary$area_name)))

############################
# STEP 9: PREPARE DATA FOR PLOTTING
############################

# Converting long data back to wide format to get years as columns
# This ensures that 2015, 2016... are actual column names
wide_plot_data <- final_summary %>%
  pivot_wider(names_from = year, values_from = total_employment)

# Checking if columns exist to avoid "undefined columns" error
target_years <- as.character(2015:2024)
available_cols <- colnames(wide_plot_data)
col_indices <- which(available_cols %in% target_years)

#########################################################
#  STEP 10: VISUALISATIONS 
########################################################
# FIGURE 1: PARALLEL COORDINATES PLOT
############################

ggparcoord(
  data = wide_plot_data,
  columns = col_indices,
  groupColumn = "area_name", 
  scale = "uniminmax",       # Standardising for research accuracy
  alphaLines = 0.5,          # Thin, semi-transparent lines
  showPoints = TRUE,         # Small circular nodes at each year
  splineFactor = 10          # Creates the smooth, curved trajectories
) + 
  scale_color_viridis_d(option = "plasma") + 
  theme_minimal() +
  labs(
    title = "Employment Trajectories in Yorkshire (2015-2024)",
    subtitle = "Standardised High-Dimensional Trends",
    x = "Year",
    y = "Standardised Value",
    color = "District"        # Clean legend title
  ) +
  theme(
    legend.text = element_text(size = 8),      # Smaller legend text to avoid clutter
    legend.key.size = unit(0.4, "cm"),         # Compact legend icons
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 9)
  )

############################
# FIGURE 2: Stacked Area Chart
############################
 
ggplot(
  final_summary %>%
    filter(area_name != "local authority: county / unitary (as of April 2023)"),
  aes(x = year, y = total_employment, fill = area_name)
) +
  geom_area(
    alpha = 0.7,          # Soft transparency
    colour = "white",     # Thin white border to separate 14 layers
    size = 0.3
  ) +
  ## Mix "Pastel1" and "Set3" get soft colors 
  # "If there are 14 districts, then scale_fill_manual provides the best control.
  
  scale_fill_manual(values = colorRampPalette(c("#A1C9F4", "#FFB482", "#8DE5A1", "#FF9F9B", "#D0BBFF", "#DEBB9B", "#FAB0E4", "#CFCFCF", "#FFFEA3", "#B9F2F0", "#FFD1DF", "#E0FFD1", "#D1E0FF", "#F5D1FF"))(14)) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Regional Contribution to Transport & Storage Employment",
    subtitle = "Trend Analysis across 14 Districts (Soft Palette)",
    x = "Year",
    y = "Employment Count",
    fill = "District"
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    panel.grid.major.x = element_blank(), # remove vertical lines
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", color = "#444444"),
    plot.subtitle = element_text(color = "#666666")
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

############################
# FIGURE 3: Alluvial Chart
############################

library(ggplot2)
library(ggalluvial)
library(dplyr)

ggplot(final_summary %>% 
         filter(area_name != "local authority: county / unitary (as of April 2023)"), 
       aes(x = factor(year), y = total_employment, 
           stratum = area_name, alluvium = area_name,
           fill = area_name)) +
  geom_flow(alpha = 0.6, color = "darkgray") + 
  geom_stratum(alpha = 0.8) + 
  # Aapka kal wala professional palette
  scale_fill_viridis_d(option = "viridis") + 
  theme_minimal() +
  labs(
    title = "Employment Flow Dynamics in Yorkshire (2015-2024)",
    subtitle = "Visualizing structural shifts across 14 local authorities",
    x = "Year", 
    y = "Employment Volume",
    fill = "District"
  ) +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, vjust = 0.5))

#####################################
#FIGURE 4: Radial Density Map
#####################################

ggplot(final_summary %>% 
         filter(area_name != "local authority: county / unitary (as of April 2023)"), 
       aes(x = factor(year), y = area_name, fill = total_employment)) +
  geom_tile(color = "white", size = 0.3) +  
  
  scale_fill_viridis_c(option = "magma", direction = -1) +  
  coord_polar(theta = "x") +  
  theme_minimal() +
  labs(
    title = "360-Degree Temporal Density Map",
    subtitle = "Regional Job Concentration (14 Districts)",
    fill = "Total Jobs",
    x = "", y = ""
  ) +
  theme(
    axis.text.x = element_text(size = 10, face = "bold", color = "black"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank(),
    legend.position = "right"
  ) 