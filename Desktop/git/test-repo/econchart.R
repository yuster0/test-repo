library(tidyverse)
library(WDI)
library(hrbrthemes)
library(gganimate)
library(dplyr)
new_cache = WDIcache()
#search for the data type by regex:
WDIsearch('gdp.*constant.*2010', cache=new_cache)
dat <- WDI(indicator='NY.GDP.MKTP.KD', country=c('LR','UA', 'RW', 'ID', 'ZW', 'GR'), 
           start=1988, end=2016, cache = new_cache)
# create two vectors of country Labels and their correspoding year of economic collapse
countries <- c("Liberia", "Ukraine", "Rwanda", "Indonesia", "Zimbabwe", "Greece")
years <- c(1988, 1989, 1992, 1997, 2001, 2007)
# create seperate df for each country with appropriate time periods
# add GDP indexes and years since columns then store in a list.
dfs <- lapply(1:6, function(x) {
  dat %>% filter(country == countries[x], year >= years[x]) %>%
    arrange(year) %>% 
    rename(GDP = NY.GDP.MKTP.KD) %>% 
    mutate(index = GDP / (GDP[1] / 100),
           years_since = row_number() -1,
           country = factor(country, levels = countries))
})
# rejoin all dfs in list into one df
final.dat <- bind_rows(dfs)
head(final.dat)
glimpse(final.dat)

final.dat$years_since <- as.numeric(final.dat$years_since)



ggplot(final.dat, aes(years_since, index, group = country, colour = country)) +
  geom_hline(yintercept = 100, colour = "red", linetype = 1) +
  geom_line(size = 1) +
  geom_point(aes(x = 0, y = 100), colour = "black", size = 2) +
  scale_x_continuous(expand = c(0.02,0)) +
  scale_color_brewer(palette = "Dark2", 
                     labels = c("Liberia (1988)", "Ukraine (1989)", "Rwanda (1992)",
                                "Indonesia (1997)", "Zimbabwe (2001)", "Greece (2007)")) +
  theme_ipsum(base_family = "Iosevka", grid_col = "white") +
  labs(title = "Shock Therapy - Selected Economic Collapses Since 1988", 
       subtitle = "100 = GDP prior to collapse (in constant 2010 $)",
       y = "", x = "Years since collapse",
       caption = "Source: World Bank",
       colour = "Country") +
  theme(plot.background = element_rect(fill = "#cddee7"))

econ_chart <- final.dat %>% filter(index <= 100) %>% 
  ggplot(aes(years_since, index, group = country, colour = country)) +
  geom_hline(yintercept = 100, colour = "red", linetype = 1) +
  geom_line(size = 1, aes(frame = years_since, cumulative = TRUE)) +
  geom_point(aes(x = 0, y = 100), colour = "black", size = 2) +
  scale_x_continuous(expand = c(0.02,0)) +
  scale_color_brewer(palette = "Dark2", 
                     labels = c("Liberia (1988)", "Ukraine (1989)", "Rwanda (1992)",
                                "Indonesia (1997)", "Zimbabwe (2001)", "Greece (2007)")) +
  theme_ipsum(base_family = "Iosevka", grid_col = "white") +
  labs(title = "Shock Therapy - Selected Economic Collapses Since 1988", 
       subtitle = "100 = GDP prior to collapse (in constant 2010 $)",
       y = "", x = "Years since collapse",
       caption = "Source: World Bank",
       colour = "Country") +
  theme(plot.background = element_rect(fill = "#cddee7"))

animate(econ_chart,title_frame = FALSE, interval = .5)



animate(econ_chart,transition_length = 2,title_frame = FALSE, interval = .5)
animate(econ_chart, pause = .5, title_frame = FALSE)









