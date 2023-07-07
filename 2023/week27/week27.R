# 0. Initial setup ##########
## Loads libraries
library(dplyr)
library(ggpath)
library(ggplot2)
library(ggtext)
library(ggview)
library(htmltools)
library(junebug)
library(readr)
library(sf)
library(stringr)
library(USAboundaries)
library(USAboundariesData)

## Loads the data
rawData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/historical_markers.csv')

## Gets the US states polygons
shp <- USAboundaries::us_states(
  resolution = "high",
  states = c(
    "Texas", "Wisconsin", "Rhode Island", "New York",
    "New Hampshire", "Minnesota", "Missouri", "North Carolina",
    "Michigan", "Louisiana", "Nebraska", "California",
    "Wyoming", "South Carolina", "Kansas", "Delaware",
    "New Jersey", "North Dakota", "Colorado", "Virginia",
    "Indiana", "Nevada", "New Mexico", "Alabama", "Tennessee",
    "Kentucky", "Oregon", "Mississippi", "Connecticut",
    "Georgia", "Utah", "Idaho", "Illinois", "Iowa", "Arizona",
    "Vermont", "Montana", "South Dakota", "Pennsylvania",
    "Oklahoma", "Maryland", "Maine", "Ohio", "West Virginia",
    "Washington", "Arkansas", "Massachusetts",
    "Florida", "District of Columbia")
)

## Takes all font styles that share that exact family name and
## registers them (makes them visible to {systemfonts})
junebug::font_hoist("Font Awesome 6 Brands")

## Gets the info used to register the font families
fonts_register <- systemfonts::registry_fonts()

## Defines the font families
brandsFont <- "Font Awesome 6 Brands Regular"

# 1. Data handling ##########
## Counts the number of years in which the US
## was at war since its independence in 1776.
## Extracted from a publication of the Department of Veterans Affairs at:
## https://www.va.gov/opa/publications/factsheets/fs_americas_wars.pdf
independence <- length(1776:2023) -1
wars <- sum(
  1776:2023 %in% c(
    1775:1783, ### American Revolution
    1812:1815, ### War of 1812
    1817:1898, ### Indian War
    1846:1848, ### Mexican War
    1861:1865, ### Civil War
    1898:1902, ### Spanish-American War
    1917:1918, ### World War I
    1941:1945, ### World War II
    1950:1953, ### Korean War
    1964:1975, ### Vietnam War
    1990:1991, ### Desert Shield/Desert Storm
    2002:2023 ### Global War on Terror
  )
)

## Keeps only markers with the word "war" on the title
warMarkers <- rawData |> 
  dplyr::mutate(title = tolower(title)) |> 
  dplyr::filter(stringr::str_detect(title, "\\bwar\\b"))

## Groups the markers in three "themes"
warMarkers <- warMarkers |> 
  dplyr::mutate(thm = case_when(
    stringr::str_detect(title, "civil war") ~ "Civil War",
    stringr::str_detect(title, "world war") ~ "World Wars",
    TRUE ~ "Other Wars"
  ))

## Gets the markers coordinates and converts the object to an sf class
locations <- warMarkers |> 
  dplyr::select(longitude_minus_w, latitude_minus_s, thm) |> 
  dplyr::rename_with(.fn = ~stringr::str_sub(., 1L, 3L)) |> 
  sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(shp))

# 2. Plot construction ##########
## Defines the title
title <- tagList(
  "MARKERS OF",
  span("WAR", style="color:red;font-size:200px;font-family:'ARMY RUST'")
)
title <- as.character(title)

## Defines the introduction
intro <- tagList(
  "Since its independence, almost", br(),
  "250 years ago, the US spent", br(),
  strong("more than a half of it at War."),
  br(),br(),
  "379 markers around the", br(),
  'country contain "war" at', br(),
  "their title. They are split", br(),
  "between those about the",
  br(),
  strong("US Civil War", style = "color:#f62458;"),
  ", ",
  strong("World Wars", style = "color:#c131ea;"),
  br(), " and ",
  strong("Other Wars", style = "color:#ef951c;")
)
intro <- as.character(intro) |> 
  stringr::str_remove_all("\n")

## Defines the commentaries
comment1 <- tagList(
  strong("Texas"), " has a", br(),
  strong("variety"),
  " of markers"
)
comment1 <- as.character(comment1) |> 
  stringr::str_remove_all("\n")
comment2 <- tagList(
  strong("Arkansas"), " and ", strong("Kentucky"),  br(),
  "have plenty of markers", br(),
  "on the ", strong("Civil War", style = "color:#f62458;")
)
comment2 <- as.character(comment2) |> 
  stringr::str_remove_all("\n")

## Creates a function that simplifies the process of placing 
## the Font Awesome glyphs on the text
faDecoder <- function(code, handle) {
  tagList(
    span(code, style = glue::glue("font-family:\"{brandsFont}\";")),
    handle
  )
}

## Defines the authorship
authorship <- tagList(
  'Markers data from: Historical Marker Database USA Index', br(),
  'US wars data from: US Department of Veterans Affairs', br(),
  'Made by Ícaro Bernardes: ',
  faDecoder('\uf099', '@IcaroBSC – '),
  faDecoder('\uf09b', '@IcaroBernardes – '),
  faDecoder('\uf08c', '@icarobsc')
)
authorship <- as.character(authorship)

## Sets the linewidth vector
linewidth <- c(1.2, rep(0.3, 25), 1.2, rep(0.3, 19), 1.2, rep(0.3, 2))

## Produces the plot
plot <- NULL |> 
  ggplot() +
  
  ### Places the US polygons and the markers points
  geom_sf(aes(linewidth = I(linewidth)), fill = NA, color = "white", data = shp) +
  geom_sf(aes(color = thm), size = 0.5, alpha = 0.8, data = locations) +
  
  ### Places the title
  annotate("RichText", x = -160, y = 50, label = title,
           hjust = 0, vjust = 0, size = 31, family = "Nunito",
           fill = NA, label.colour = NA, color = "white") +
  
  ### Places the introduction
  annotate("RichText", x = -128, y = 49, label = intro,
           hjust = 1, vjust = 1, size = 6.8, family = "Nunito",
           fill = NA, label.colour = NA, color = "white") +
  
  ### Places the commentaries
  annotate("RichText", x = -101, y = 27, label = comment1,
           hjust = 1, size = 3, family = "Nunito",
           fill = NA, label.colour = NA, color = "white") +
  annotate("RichText", x = -79, y = 31, label = comment2,
           hjust = 0, size = 3, family = "Nunito",
           fill = NA, label.colour = NA, color = "white") +
  
  ### Places the authorship
  annotate("RichText", x = -160, y = 27, label = authorship,
           hjust = 0, vjust = 1, size = 4, family = "Nunito",
           fill = NA, label.colour = NA, color = "white") +
  
  ### Defines aesthetic rules
  scale_color_discrete(
    type = c(
      `Civil War` = "#f62458",
      `World Wars` = "#c131ea", 
      `Other Wars` = "#ef951c"
    ),
    guide = "none"
  ) +
  
  ### Defines aesthetics elements
  coord_sf(ylim = c(25,58)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", colour = "black")
  )

## Generates an accurate preview of the final plot
ggview::ggview(
  plot = plot, device = "png",
  width = 4000, height = 2200, units = "px",
  dpi = 320, bg = "black"
)

## Generates the final plot
ggsave("2023/week27/week27.png", plot = plot,
       width = 4000, height = 2200, units = "px",
       dpi = 320, bg = "black")
