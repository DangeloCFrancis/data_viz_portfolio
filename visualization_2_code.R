# Rvest Pipeline and Other Tips -------------------------------------------

# HTML → candidate elements → inspect structure → test candidates → select the appropriate element → convert to a tibble.

# Find all tables
# ↓
# convert each to a tibble
# ↓
# inspect their column names
# ↓
# select the one whose structure matches what we want


# map()       # return a list
# map_chr()   # return character
# map_int()   # return integer
# map_dbl()   # return numeric
# map_lgl()   # return TRUE/FALSE

# load packages 

pacman::p_load(tidyverse, tidymodels, httr2, jsonlite, rgeoboundaries, rvest)


# 1. Get webpage ----------------------------------------------------------


page <- rvest::read_html("https://en.wikipedia.org/wiki/List_of_cities_in_Japan")

tables <- 
  page |>
  rvest::html_elements("table")


tables |>
  purrr::map_chr(rvest::html_name)


# 2. Inspect Table Data ---------------------------------------------------

table_data <- 
  tables |>
  purrr::map(rvest::html_table)

purrr::map(table_data, names) # table #3 is the one we want


page |>
  rvest::html_element("table.wikitable.static-row-numbers") |>
  length()


# 3. Select Candidate Tables ----------------------------------------------

candidate_tables <- 
  page |>
  rvest::html_elements("table.wikitable.static-row-numbers")

candidate_data <- candidate_tables |>
  purrr::map(rvest::html_table)

is_city_table <- 
  candidate_data |>
  purrr::map_lgl(~ names(.x)[1] == "City (Special Ward)") # use anon function with map_lgl() to quickly see which tables match characteristics

japan_cities <- 
  candidate_data[is_city_table][[1]]




# Old Code (To Be Replaced) -----------------------------------------------



japan <- 
  gb_adm1("japan") # get lvl1 adm map of Japan (~47 prefectures)

# load rail data in

japan_rail <- 
  read_sf("data/Japan_railroads.geojson") |> # read in railroad map
  clean_names() |>
  rename(rail_type = exs_descri) |>
  rename(func = fco_descri) |>
  filter(func != "Unknown")

# load in major city data 

japan_cities <-
  read_sf("data/cities.geojson") |>
  clean_names() |>
  rename(country = ctry) |> 
  filter(country == "Japan" & pop > 2500000)



# load 2019 GDP in, rename columns for data join and obs to match geodata 

japan_gdp2019 <- 
  read_excel("data/Japanese Prefectures by GDP - 2019.xlsx") |>
  clean_names() # using janitor package

japan_gdp2019 <-
  japan_gdp2019 |>
  rename(gdp_yen = x2019_gdp_in_millions_of_jp) |>
  rename(gdp_usd = x2019_gdp_in_millions_of_us) |>
  rename(shapeName = prefecture) |>
  rename(gdp_share = share_of_japan_gdp_percent) |>
  mutate(shapeName = case_when(
    shapeName == "Aichi" ~ "Aichi Prefecture", 
    shapeName =="Ehime" ~ "Ehime Prefecture",
    shapeName == "Fukui" ~ "Fukui Prefecture",
    shapeName == "Fukuoka" ~ "Fukuoka Prefecture",
    shapeName ==  "Gifu" ~ "Gifu Prefecture",
    shapeName ==  "Hyogo" ~ "Hyogo Prefecture",
    shapeName ==  "Ishikawa" ~ "Ishikawa Prefecture",
    shapeName ==  "Kagawa" ~ "Kagawa Prefecture",
    shapeName ==  "Kagoshima" ~ "Kagoshima Prefecture",
    shapeName ==  "Kochi" ~ "Kochi Prefecture",
    shapeName ==  "Kyoto" ~ "Kyoto Prefecture",
    shapeName ==  "Mie" ~ "Mie Prefecture",
    shapeName ==  "Miyazaki" ~ "Miyazaki Prefecture",
    shapeName ==  "Nagasaki" ~ "Nagasaki Prefecture",
    shapeName ==  "Nara" ~ "Nara Prefecture",
    shapeName ==  "Okayama" ~ "Okayama Prefecture",
    shapeName ==  "Okinawa" ~ "Okinawa Prefecture",
    shapeName ==  "Osaka" ~ "Osaka Prefecture",
    shapeName ==  "Saga" ~ "Saga Prefecture",
    shapeName ==  "Tokushima" ~ "Tokushima Prefecture",
    shapeName ==  "Tottori" ~ "Tottori Prefecture",
    shapeName ==  "Wakayama" ~ "Wakayama Prefecture",
    .default = as.character(shapeName)
  ))


# combine gdp with geodata - should have 47 obs for each data frame

japan_gdpjoin <- 
  full_join(japan_gdp2019, japan, by = join_by("shapeName"))|>
  st_as_sf()

japan_plot <- 
  filter(.data = japan_rail, rail_type == "Operational") |>
  ggplot() + 
  geom_sf(data = japan_gdpjoin, aes(fill = as.numeric(gdp_share))) +
  scale_fill_gradient(name = "Percent of GDP Share (2019)",
                      low = "#FFFFF0",
                      high = "#BC002D") + 
  geom_sf(data = japan_rail,linewidth = .1, color = "steelblue") +
  geom_sf(data = japan_cities) + 
  geom_label_repel(data = japan_cities, 
                   aes(label = name, geometry = geometry), 
                   stat = "sf_coordinates", 
                   label.size = .15, 
                   box.padding = .15, 
                   label.padding = .15, 
                   min.segment.length = 2) + # use this for sf label overlaps 
  theme(
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) + 
  labs(
    title = "The Relationship between GDP Share and Railroad Infrastructure in Japan",
    subtitle = "A Prefecture's railroad density can serve as an indicator for national GDP contributions",
    caption = str_wrap("Source: 2020 National Accounts, Economic and Social Research Institute of Japan; \n geoBoundaries R package; IMB GIS, Cities ; ArcGIS Hub, Japan Railroads"
    )
  )