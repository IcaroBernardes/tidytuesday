# 0. Initial setup ##########
## Loads libraries
library(dplyr)
library(ggforce)
library(ggfx)
library(ggplot2)
library(ggtext)
library(ggview)
library(glue)
library(junebug)
library(purrr)
library(readr)
library(scales)
library(stringr)
library(tidyr)

## Gets the energy data
raw_energy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv')

## Gets the data on anomalies on the
## average earth surface temperature (1966-2021, in Celsius)
## Taken from: https://www.ncdc.noaa.gov/cag/global/time-series
years_temp <- "1966-2021"
url <- glue::glue("https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series/globe/land_ocean/ann/4/{years_temp}/data.csv")
raw_temp <- readr::read_csv(url, skip = 4)

## Takes all font styles that share that exact family name and
## registers them (makes them visible to {systemfonts})
junebug::font_hoist("Font Awesome 6 Brands")

## Gets the info used to register the font families
fonts_register <- systemfonts::registry_fonts()

## Defines the font families
font_title <- "FIRES PERSONAL USE"
font_body <- "Ubuntu"
font_brands_glyphs <- "Font Awesome 6 Brands Regular"

# 1. Data handling ##########
## Selects the variables of interest and excludes lines with missing info
countries_energy <- raw_energy |> 
  dplyr::select(country, iso_code, year,
                fossil_fuel_consumption, fossil_cons_change_twh) |> 
  dplyr::rename(consumption = fossil_fuel_consumption,
                change = fossil_cons_change_twh) |> 
  na.exclude()

## Calculates yearly global consumption (fossil fuels)
global_energy <- countries_energy |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(consumption = sum(consumption)) |> 
  dplyr::ungroup()

## Renames the variables of "raw_temp"
global_temp <- raw_temp |> 
  dplyr::rename(year = Year, anom = Value)

## Joins the global tibbles
global <- dplyr::left_join(global_energy, global_temp)

## Identifies countries to highlight on the periods of '83-'88 and '17-'21
### Creates variables for the periods and stacks them
periods <- countries_energy |> 
  dplyr::mutate(perA = year %in% 1983:1988,
                perB = year %in% 2017:2021) |> 
  tidyr::pivot_longer(cols = starts_with("per")) |> 
  dplyr::filter(value)

### Calculates the average change in consumption by country on the periods 
periods <- periods |> 
  dplyr::group_by(name) |> 
  tidyr::nest() |> 
  dplyr::ungroup() |> 
  dplyr::mutate(data = purrr::map(
    data, function (df) {
      df |> 
        dplyr::group_by(country) |> 
        dplyr::summarise(change = mean(change))
    }
  ))

### Identifies the countries that had the biggest changes in consumption.
### Increases for '83-'88 and decreases for '17-'21
periods <- periods |> 
  dplyr::mutate(
    data = purrr::map2(
      data, c(1, -1), function (df, var) {
        df |> 
          dplyr::mutate(value = var*change) |> 
          dplyr::slice_max(order_by = value, n = 3) |> 
          dplyr::pull(country)
      }
    ))

## Creates combinations between years and countries
periods <- periods |> 
  dplyr::mutate(
    data = purrr::map2(
      data, list(1983:1988, 2017:2021), function (countries, years) {
        tidyr::expand_grid(
          country = countries,
          year = years
        )
      }
    )) |> 
  tidyr::unnest(cols = data) |> 
  dplyr::mutate(group = glue::glue("{name}-{country}"))

## Filter data on consumption by the selected countries
periods <- dplyr::left_join(periods, countries_energy)

## Calculates the number of countries in the database by year
lands <- countries_energy |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(n = n_distinct(country)) |> 
  dplyr::ungroup()

## Calculates how much countries add to
## the global consumption when they enter the database
append <- countries_energy |> 
  dplyr::filter(year != 1966) |> 
  dplyr::group_by(country) |> 
  dplyr::slice(1L) |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(add = sum(consumption)) |> 
  dplyr::left_join(countries_energy) |> 
  dplyr::group_by(year, add) |> 
  dplyr::summarise(total_cons = sum(consumption),
                   total_change = sum(change))

## Gets the list of countries added in 1986
append <- countries_energy |> 
  dplyr::group_by(country) |> 
  dplyr::slice(1L) |> 
  dplyr::filter(year == 1986) |> 
  dplyr::arrange(desc(consumption))

# 2. Plot production ##########
## Defines title
title <- "HOT STUFF"

## Defines subtitle
subtitle <- "The Earth is getting <span style='color:#D10F30;'>hotter</span>,
but fossil fuels still are the main source of energy in the world."

## Defines credits
social_css <- glue::glue("<span style='font-family:\"{font_brands_glyphs}\";'>")
social_text <- glue::glue("{social_css}\uf099</span> @IcaroBSC – 
                          {social_css}\uf09b</span> @IcaroBernardes – 
                          {social_css}\uf08c</span> @icarobsc")
credits <- glue::glue("Data on energy consumption: Our World in Data<br>
                      Data on Earth's surface temperature: National Centers
                      for Environmental Information<br><br>
                      Graphic by: Ícaro Bernardes<br>{social_text}")

## Defines coordinates and orientation of the informative texts 
informative_texts <- dplyr::tibble(
  x = c(1979, 1985, 2000, 2020),
  y = c(89000, 53000, 87000, 120000),
  hjust = c(1, 0, 0, 1),
  label = c(
    "The line color represents<br>
    <strong>Earth's surface temperature.</strong><br>
    It shows how much a given<br>
    year diverges from the average<br>
    temperature (in Celsius) of<br>
    the period of 1901-2000.<br>
    The scale goes from <strong style='color:#1B9BD1;'>blue</strong> (below),<br>
    to <strong>white</strong> (average) and <strong style='color:#D10F30;'>red</strong> (over).",
    
    "This sudden increase was caused<br>
    by the <strong>addition</strong> of Russia<br>
    and many Eastern European<br>
    countries to the <strong>database.</strong>",
    
    "The line y-axis position<br>
    represents the <strong>consumption</strong><br>
    of <strong>fossil fuels</strong> as primary energy.<br>
    That is, energy in its raw form,<br>
    before conversion for use.",
    
    "This minor drop<br>
    happened in most<br>
    of the world as<br>
    production shrinked.<br>
    Consequence of the<br>
    <strong>COVID-19 pandemic.</strong>"
  )
) 

## Defines coordinates of the informative lines
informative_lines <- dplyr::tibble(
  x = c(1979, 1985, 2000, 2020),
  xend = c(1979, 1985, 2000, 2020),
  y = c(88000, 54000, 87000, 120000),
  yend = c(60000, 39000, 70000, 99500)
)

## Creates the plot
p <- global |> 
  ggplot() +
  
  ### Places the colorful line
  ggforce::geom_link2(
    aes(x = year, y = consumption, color = anom),
    linewidth = 2, lineend = "round"
  ) +
  
  ### Places the title with glow effect
  ggfx::with_outer_glow(
    x = annotate("text", x = 1966, y = 140000, label = title, family = font_title,
                 color = "#D10F30", hjust = 0, vjust = 1, size = 25),
    colour = "#D10F30", sigma = 30, expand = 0.8
  ) +

  ### Places the subtitle
  annotate("TextBox", x = 1965, y = 120000, label = subtitle,
           fill = NA, box.colour = NA, color = "white", family = font_body,
           hjust = 0, vjust = 1, width = unit(0.6, "npc"), size = 7) +
  
  ### Places the credits
  annotate("RichText", x = 2021, y = 0, label = credits,
           fill = NA, label.colour = NA, color = "white",
           family = font_body, hjust = 1, vjust = 0, size = 4) +
  
  ### Places the informative texts
  ggtext::geom_richtext(
    aes(x = x, y = y, label = label, hjust = hjust),
    vjust = 1, size = 3, family = font_body,
    fill = NA, label.colour = NA, color = "white",
    label.padding = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
    data = informative_texts
  ) +
  
  ### Places the informative lines
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend),
               color = "white", lineend = "round",
               linewidth = 1, data = informative_lines) +
  
  ### Defines aesthetics rules
  scale_color_gradient2(low = "#1B9BD1", high = "#D10F30", midpoint = 0) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::label_number(scale_cut = cut_short_scale())) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10),
                     labels = ~stringr::str_replace(., "[:digit:]{2}", "'")) +
  
  ### Defines decorations 
  labs(y = "consumption<br><span style='font-size:10px;'>of fossil fuels in terawatt-hours</span>") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", colour = NA),
    plot.margin = margin(0.02, 0.05, 0.05, 0.05, "npc"),
    
    axis.title = element_text(size = 20, color = "white", family = font_body,
                              hjust = 0, margin = margin(5, 5, 0, 0)),
    axis.title.y = ggtext::element_markdown(angle = 90),
    axis.text = element_text(size = 15, color = "white", family = font_body),
    axis.text.y = element_text(hjust = 1),
    legend.position = "none"
  )

## Generates accurate preview
ggview::ggview(
  plot = p, device = "png",
  width = 3000, height = 2500, units = "px",
  dpi = 320, bg = "black"
)

## Generates the final plot
ggsave("2023/week23/week23.png", plot = p,
       width = 3000, height = 2500, units = "px",
       dpi = 320, bg = "black")
