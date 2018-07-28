download.file(
  "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/468966/SFR39_2015_Underlying_data.zip", 
  "UD.zip", 
  quiet = FALSE, 
  mode = "w",
  cacheOK = TRUE
)

unzip(
  "UD.zip", 
  files = "SFR39_2015_Autumn_Spring_Proposed_SFR_structure.csv",
  overwrite = TRUE
)



library(govstyle)
library(readr)
library(dplyr)
absence_data_full <- read_csv(
  file = "SFR39_2015_Autumn_Spring_Proposed_SFR_structure.csv",
  na = c( "x", ".", "")
) %>%
  mutate(
    Period = factor(Period),
    Level = factor(Level),
    Year = factor(Year),
    Country = factor(Country),
    School_type = School_type %>% tolower %>% factor
  )
# For brevity of printing, select only columns of interest.

absence_data <- absence_data_full %>%
  select(
    Period, Level, Year, Country,
    School_type, sess_possible, sess_overall
  )
absence_data
# Calculate the national OAR values.

oar_summary <- absence_data %>%
  dplyr::filter(
    Level == "NATIONAL"
  ) %>%
  mutate(
    oar = (sess_overall/sess_possible) * 100
  )

# Calculate the OAR values for Period, Level, Year, and Country combinations

oar_summary_combined <- absence_data %>%
  dplyr::filter(
    Level == "NATIONAL"
  ) %>%
  group_by(Period, Level, Year, Country) %>%
  summarise(
    sess_possible = sum(sess_possible),
    sess_overall = sum(sess_overall)
  ) %>%
  mutate(
    oar = (sess_overall/sess_possible) * 100,
    School_type = "state-funded primary and secondary"
  )

# Combine the two above dataframes

oar_summary <- bind_rows(
  oar_summary,
  oar_summary_combined
)
oar_summary
# Creating the first plot
oar_values <- oar_summary %>% 
  filter(
    Year %in% c("2006/07","2014/15")
  ) %>%
  arrange(Year)


oar_values

library(ggplot2)

p <- oar_summary %>%
  ggplot +
  aes(
    x = Year,
    y = oar,
    colour = School_type,
    fill = School_type,
    group = School_type
  ) +
  geom_path(size = 1.5) +
  xlab("Autumn and Spring term") +
  ylab("Overall absence rate (%)")
p
p1 <- p + 
  expand_limits(
    x = 0, # setting y axis at zero
    y = c(0, 8.5)
  )   +
  ggtitle(
    "Overall absence rate across state-funded\nprimary and secondary schools"
  ) 

p1

check_pal() #use theme_gov

p2 <- p1 +
  theme_gov(
    base_size = 12, 
    base_colour = "gray40") +
  scale_colour_manual(
    values =  gov_cols[c("turquoise","brown","light_blue")] %>% unname
  )

p2

p3 <- p2 +
  geom_text(
    data = oar_values,
    aes(
      label = sprintf("%.1f", oar) # Label lines 
    ),
    hjust = rep(c(1.35,-0.35), each = 3),
    fontface = "bold"
  )+
  geom_text(
    data = oar_summary %>% filter(Year == "2006/07"),
    aes(
      label = c(
        "Primary",
        "Secondary",
        "Primary and secondary"
      )
    ),
    hjust = 0,
    vjust = -1,
    fontface = "bold"
  )

p3 

p4 <- p3 + 
  theme(
    # plot.margin = grid::unit(
    #   c(0, 5, 5, 0), "mm"),
    axis.title.y = element_text(
      angle = 0 # rotate y axis
    )
  )

p4 

## Creating the second plot
### Illness absence rates

illness_summary <- absence_data_full %>%
  dplyr::filter(Level == "NATIONAL") %>%
  group_by(Year) %>%
  summarise(
    sess_overall = sum(sess_overall),
    sess_possible = sum(sess_possible),
    sess_auth_illness = sum(sess_auth_illness)
  ) %>%
  mutate(
    oar = (sess_overall / sess_possible) * 100,
    iar = (sess_auth_illness / sess_possible) * 100
  ) %>%
  tidyr::gather(key, value, oar:iar)

illness_summary

# Start with the new illness_summary object

illness_summary %>%
  
  # Set up the basics of the plot
  
  ggplot +
  aes(
    x = Year,
    y = value,
    group = key,
    colour = key
  ) +
  
  # Add the lines
  
  geom_path(size = 1.5) +
  
  # Add the values at the start and end of the lines
  
  geom_text(
    data = illness_summary %>% filter(Year %in% c("2006/07","2014/15")) %>% arrange(Year),
    
    # Force values to show one decimal place even if that is zero
    
    aes(label = sprintf("%.1f", value)),
    
    # Nudge the values away from the lines
    
    hjust = rep(c(1.25,-0.25),each = 2),
    fontface = "bold"
  ) +
  
  # Label the lines
  
  geom_text(
    data = illness_summary %>% filter(Year == "2006/07"),
    aes(label = c(
      "Overall absence rate",
      "Illness absence rate"
    )),
    
    # Left justify, and nudge the values up away from the lines
    
    hjust = 0,
    vjust = -1.2,
    size = 4,
    fontface = "bold"
  ) +
  
  # axis limits
  
  expand_limits(x = 0, y = c(0, 8)) +
  
  # Use the gov.uk colours
  
  scale_colour_manual(values = gov_cols[c("turquoise","brown")] %>% unname) +
  
  # Apply theme_gov
  
  theme_gov(
    base_size = 12, base_colour = "gray40", axes = "x"
  ) +
  
  # Label the axes
  
  xlab("Autumn and spring term") +
  ylab("Absence rate (%)") +
  
  # Add a title. Note that line breaks in the title must be specified manually
  # with "\n"
  
  ggtitle(
    "Comparison of the trend in overall and illness\n absence rates: England, autumn 2006 and\n spring 2007 to autumn 2014 and spring 2015"
  ) +
  
  # Make the y-axis title horizontal, and at the top of the axis.
  # Adjust margins to compensate for this.
  # Adjust the axis breakpoints.
  
  theme(
    axis.title.y = element_text(
      angle = 0, hjust = 20, vjust = 1.01
    ),
    plot.margin = grid::unit(c(0,5,5,0), "mm")
  ) +
  scale_y_continuous(breaks = c(0, seq(0, 8, 2)))





  