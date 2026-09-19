# Broadband Internet Visualization

source("save_visualizations.R")

# core packages

pacman::p_load(tidyverse, tidymodels, httr2, jsonlite, wbstats)

# visualization packages

pacman::p_load(patchwork, tigris, sf, rgeoboundaries, ggrepel, ggthemes, crsuggest, ggarchery)

## Visualization 1: 

# import and clean data, continue to 'tidy' data to 4 variable categories

wbindicator_options <- wbstats::wb_indicators() # download and browse through available indicators

# IT.NET.BBND.P2 for broadband
# BX.KLT.DINV.CD.WD for FDI net inflows

broadband_data <- 
  wbstats::wb_data(
  indicator = c("IT.NET.BBND.P2",
                "BX.KLT.DINV.CD.WD"), 
  country = c("Haiti", "Dominican Republic"))

broadband_data_scoped <-
  broadband_data |>
  filter(date %in% 2013:2023) |>
  select(!starts_with("iso")) |>
  rename(broadband_suscribers = IT.NET.BBND.P2,
         fdi_net_inflows = BX.KLT.DINV.CD.WD,
         collection_year = date) |>
  mutate(fdi_net_inflows = fdi_net_inflows/1e8) # reduce number by factor of 8 (100,000,000)

# just got rid of 50 lines of code by using wbstats() to condense data collection

visualization_1 <- # this is the 'base' plot
  broadband_data_scoped |>
  ggplot(aes(x = fdi_net_inflows, y = broadband_suscribers, label = collection_year, color = country)) + 
  geom_point(position = "jitter") + 
  scale_color_manual(name = "Country/Region",
                     values = c("Dominican Republic" = "#002D62", "Haiti" = "#016a16")) +
  geom_text_repel(show.legend = FALSE, max.overlaps = 20) +
  labs(
    title = str_wrap("Foreign Direct Investment Net Inflow and Broadband Access on Hispaniola"),
    subtitle = str_wrap("Despite being on the same island, the Dominican Repulic seems for posed to utilize FDI inflow to increase broadband access among its citizens, while Haiti grapples with persistentpolitical turmoil"),
    caption = str_wrap("Sources: *wbstats* | 	Programmatic Access to Data and Statistics from the World Bank API, Jesse Piburn "),
    x = "Foreign Direct Investment Net Inflows (per $100 million)",
    y = "Broadband Access (per 100 people)"
  ) +
  theme_clean()

save_my_plot(visualization_1, "visualization_1")

visualization_1
