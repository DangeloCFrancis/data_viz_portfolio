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

pacman::p_load(tidyverse, tidymodels, httr2, jsonlite, rgeoboundaries, rvest, sf, tmap, rnaturalearth)

source("scripts/save_visualizations.R")

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


# 4. Get Japanese Railroad, National, and Municipal Geospatial Data ----------

japan_boundaries <- 
  # gb_adm1("japan") # get lvl1 adm map of Japan (~47 prefectures) - UPDATE 10/30/2026 - rgeoboundaries throwing error - using rnaturalearth package now
  rnaturalearth::ne_states(
    country = "Japan"
  ) |>
  rename(shapeName = name)

crsuggest::suggest_crs(japan_boundaries) # get best CRS (3094)

japan_rail <- 
  sf::read_sf("data/Japan_railroads.geojson") |> # read in railroad map
  janitor::clean_names() |>
  rename(rail_type = exs_descri) |>
  rename(func = fco_descri) |>
  filter(func != "Unknown")

japan_cities <-
  read_sf("data/cities.geojson") |>
  janitor::clean_names() |>
  rename(country = ctry) |> 
  filter(country == "Japan" & pop > 2500000)

# tmap::qtm(japan_boundaries) to test boundaries

# Old Code (To Be Replaced) -----------------------------------------------


# load 2019 GDP in, rename columns for data join and obs to match geodata 

japan_gdp2019 <- 
  openxlsx2::read_xlsx("data/Japanese Prefectures by GDP - 2019.xlsx") |>
  janitor::clean_names()

japan_gdp2019 <-
  japan_gdp2019 |>
  rename(gdp_yen = x2019_gdp_in_millions_of_jp,
         gdp_usd = x2019_gdp_in_millions_of_us,
         shapeName = prefecture,
         gdp_share = share_of_japan_gdp_percent) |>
  mutate(gdp_share = as.numeric(gdp_share))


# combine gdp with geodata - should have 47 obs for each data frame

japan_gdpjoin <- 
  fuzzyjoin::stringdist_full_join(japan_gdp2019, japan_boundaries, by = join_by("shapeName"))|> # using fuzzyjoin package because boundaries data uses pronounciation symbols
  st_as_sf()

tmap_plot <-
  tmap::tm_shape(japan_gdpjoin, crs = 3094) +
  tmap::tm_polygons(fill = "gdp_share",
                    fill.scale = tm_scale_continuous(
                      values = c("#FFFFF0","#BC002D")
                    ),
                    fill.legend = tm_legend(
                      title = "Percent of GDP Share (2019)",
                      position = tm_pos_in("right", "bottom"),
                      orientation = "landscape")) +
  tmap::tm_shape(japan_cities) + # must declare a new tm_shape if using separate datasets
  tmap::tm_symbols(
    size = .5,
    fill = "black"
  ) +
  tmap::tm_labels(
    "name",
    options = opt_tm_labels(
      point.label = TRUE
    )
  ) +
  tmap::tm_shape(japan_rail) +
  tmap::tm_lines(col = "blue",
                 lwd = 0.1) +
  tm_title("Geographic Distribution of GDP and Railroad Infrastructure in Japan") +
  tm_title_in(str_wrap("Economic output is concentrated in major metropolitan prefectures served by extensive transportation networks.")) +
  tm_credits("Source: 2020 National Accounts, Economic and Social Research Institute of Japan; \n geoBoundaries R package; IMB GIS, Cities ; ArcGIS Hub, Japan Railroads")

tmap_save(
  tmap_plot,
  filename = "visualizations/japan_railroad_gdp_tmap.png",
  width = 10,
  height = 6,
  dpi = 600
)


japan_plot <- 
  filter(.data = japan_rail, rail_type == "Operational") |>
  ggplot() + 
  geom_sf(data = japan_gdpjoin, aes(fill = as.numeric(gdp_share))) +
  scale_fill_gradient(name = "Percent of GDP Share (2019)",
                      low = "#FFFFF0",
                      high = "#BC002D") + 
  geom_sf(data = filter(japan_rail, rail_type == "Operational"),linewidth = .1, color = "steelblue") +
  geom_sf(data = japan_cities) + 
  ggrepel::geom_label_repel(data = japan_cities, 
                   aes(label = name, geometry = geometry), 
                   stat = "sf_coordinates", 
                   label.size = .15, 
                   box.padding = .15, 
                   label.padding = .15, 
                   min.segment.length = 2) + # use this for sf label overlaps 
  labs(
    title = "Geographic Distribution of GDP and Railroad Infrastructure in Japan",
    subtitle = "Economic output is concentrated in major metropolitan prefectures served by extensive transportation networks.",
    caption = str_wrap("Source: 2020 National Accounts, Economic and Social Research Institute of Japan; \n geoBoundaries R package; IMB GIS, Cities ; ArcGIS Hub, Japan Railroads"
    )
  ) +
  cowplot::theme_map()

save_my_plot(japan_plot, "japan_plot_v2")
