
# Visualization 3 Script - One Rail to Rule Them All 

upt_calendaryear <- 
  read_excel("data/upt_calendaryear_082024.xlsx") |>
  clean_names() |>
  select(-ntd_id, -legacy_ntd_id) |>
  rename(transport_mode = x3_mode) |> 
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year",
    values_to = "upt"
  ) |>
  filter(mode_type_of_service_status == "Active", 
         reporter_type == "Full Reporter",
         mode %in% c("MB", "HR", "LR"),
         is.na(upt) == FALSE
  ) |> 
  mutate(upt_per_million = upt/10e6)


# fix year back to numeric 

upt_calendaryear$year <- as.numeric(gsub("x","",x = upt_calendaryear$year))

# group by top 10 agencies 

upt_calendaryear$agency <- fct(upt_calendaryear$agency)
upt_calendaryear$uza_name <- fct(upt_calendaryear$uza_name)

uptyear <- 
  tibble(upt_calendaryear$upt, upt_calendaryear$year) # for slice_max() order_by

uptyear <-
  uptyear |> 
  rename(upt = `upt_calendaryear$upt`) |>
  rename(year = `upt_calendaryear$year`)

# create rail variable only for NY/NJ

upt_year_nyrail <- 
  upt_calendaryear |>
  filter(tos == "DO", uza_name == "New York--Jersey City--Newark, NY--NJ",
         !agency %in% c("City of Long Beach",
                        "Town of Huntington",
                        "Staten Island Rapid Transit Operating Authority",
                        "Port Imperial Ferry Corporation",
                        "Bergen County",
                        "Somerset County",
                        "MTA Bus Company"),
         mode %in% c("LR","HR"),
         year %in% (2002:2023) # 2024 isn't over yet
  ) |>
  reframe(agency,upt_per_million, mode, year)



ggplot(data = upt_year_nyrail, aes(x = year,
                                   y = upt_per_million, 
                                   color = agency)) + 
  geom_line(linewidth = 1.3) + 
  geom_arrowsegment(aes(x = 2010, y = 85, xend = 2020, yend = 111.0554025), show.legend = FALSE, color = "black") + 
  labs(
    x = element_blank(),
    y = "Passengers\n(Millions)",
    title = "One Rail to Rule Them All: 20 years of MTA Subway Dominance",
    subtitle = "Over the past 20 years, the New York City subway remains the heavy favorite in commuter rail options compared to\n NJ Transit and PATH.",
    caption = str_wrap("Source: Complete Monthly Ridership - Federal Transit Administration https://www.transit.dot.gov/ntd/data-product/monthly-module-adjusted-data-release")
  ) + 
  annotate(geom = "text", x = 2009, y = 75, label = "COVID-19 Pandemic forces\nstay-at-home measures") + 
  theme_clean()

ggsave(filename = "mtaridership.png", width = 11, height = 8.5)

ggsave(filename = "mtaridership.pdf", width = 11, height = 8.5)