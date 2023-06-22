# 0. Initial setup ##########
## Loads libraries
library(dplyr)
library(forcats)
library(ggpath)
library(ggplot2)
library(ggsvg)
library(ggtext)
library(ggview)
library(glue)
library(htmltools)
library(junebug)
library(purrr)
library(readr)
library(tibble)

## Gets the raw data on sightings
ufoSightings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')

## Reads the svg image as code
svgCode <- glue::glue_collapse(readLines('2023/week25/alien-monster.svg'), sep = "\n")

## Takes all font styles that share that exact family name and
## registers them (makes them visible to {systemfonts})
junebug::font_hoist("Font Awesome 6 Brands")

## Gets the info used to register the font families
fonts_register <- systemfonts::registry_fonts()

## Defines the font families
bodyFont <- "VT323"
brandsFont <- "Font Awesome 6 Brands Regular"

# 1. Data handling ##########
## Keeps only location info.
ufoLocations <- ufoSightings |> 
  dplyr::select(city, state, country_code)

## Makes all city names uppercase
ufoLocations <- ufoLocations |> 
  dplyr::mutate(city = toupper(city))

## Calculates the percentage of sightings by country
ufoLocations |> 
  dplyr::count(country_code, sort = TRUE) |>
  dplyr::mutate(pct = round(100*n/sum(n)))

## Calculates the number of sightings by city
ufoLocations <- ufoLocations |> 
  dplyr::count(city, state, country_code, sort = TRUE)

## Keeps the top 5
ufoLocations <- ufoLocations |> 
  dplyr::slice(1:5)

## Defines colors for the rows of aliens
colors <- as.character(0:6)
names(colors) <- c(
  "#29BECC", "#294CCC", "#8829CC", "#CC29B6",
  "#CC2E29", "#CC6F28", "#CCA629"
)

## Calculates the positions of the aliens
ufoLocations <- ufoLocations |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    position = list(
      tibble::tibble(
        x = rep(1:100, 7),
        cityOrder = dplyr::cur_group_rows()
      ) |> 
        tibble::rowid_to_column() |> 
        dplyr::mutate(
          row = (rowid-1) %/% 100,
          y = row + 10*cityOrder,
          fill = factor(row),
          fill = forcats::fct_recode(fill, !!!colors)
        ) |> 
        dplyr::slice(1:n)
    )
  ) |> 
  dplyr::ungroup()

# 2. Plot production ##########
## Defines the coordinates of the city labels
ufoLocations <- ufoLocations |> 
  dplyr::mutate(
    x = 1,
    y = (1:5)*10 - 1,
    label = glue::glue("{city}: {n}")
  )

## Defines the coordinates of the city landmark images
ufoLocations <- ufoLocations |> 
  dplyr::mutate(
    ximg = 1,
    yimg = (1:5)*10 - 2.5,
    path = tolower(city),
    path = stringr::str_replace_all(path, "[:space:]", "_"),
    path = glue::glue("2023/week25/pixelated/{path}.png")
  )

## Defines the plot information text
infoText <- htmltools::tagList(
  span("91%", style = "font-size:175px;color:#16b619;"),
  br(),br(),
  "OF EVERY",
  br(),
  span("ALIEN SIGHTING", style = "color:#16b619;"),
  br(),
  "REGISTRED IN THE NATIONAL UFO REPORTING CENTER OCCURED",
  br(),
  span("IN THE US.", style = "color:#16b619;"),
  br(),br(),
  "THE CITIES ON THE RIGHT",
  br(),
  "ARE THE",
  br(),
  span("MOST VISITED", style = "color:#16b619;"),
  br(),
  span("BY ALIENS.", style = "color:#16b619;")
)
infoText <- as.character(infoText)
infoText <- stringr::str_remove_all(infoText, "\n")

## Creates a function that simplifies the process of placing 
## the Font Awesome glyphs on the text
faDecoder <- function(code, handle) {
  tagList(
    span(code, style = glue::glue("font-family:\"{brandsFont}\";")),
    handle
  )
}

## Defines the credits text
credText <- htmltools::tagList(
  'Made by Ícaro Bernardes: ',
  faDecoder('\uf099', '@IcaroBSC – '),
  faDecoder('\uf09b', '@IcaroBernardes – '),
  faDecoder('\uf08c', '@icarobsc')
)
credText <- as.character(credText)

## Creates the svg point layers
aliens <- ufoLocations$position |> 
  purrr::map(function(df) {
    ggsvg::geom_point_svg(
      aes(x = x, y = y, ggsvg::css("path", fill = fill)),
      ggsvg::css("g", stroke = "none"),
      size = 3, svg = svgCode, data = df)
  })

## Produces the plot
p <- ufoLocations |> 
  ggplot() + 
  
  ### Inserts the svg points
  aliens +
  
  ### Inserts the labels of the cities
  geom_text(aes(x = x, y = y, label = label), family = bodyFont,
            color = "#16b619", hjust = 0, vjust = 0, size = 10) +
  
  ### Inserts the city landmark images
  ggpath::geom_from_path(
    aes(x = ximg, y = yimg, path = path),
    width = 0.09, height = 0.09, hjust = 1, vjust = 1
  ) +
  
  ### Inserts the plot title
  annotate(
    "text", x = 100, y = -4,
    label = "SPACE INVADERS?",
    color = "white", size = 59,
    hjust = 1, vjust = 1, family = bodyFont
  ) +
  
  ### Inserts the info text
  annotate(
    "TextBox", x = -15, y = 7, label = infoText,
    hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = unit(0.15, "npc"),
    color = "white", fill = NA, box.colour = NA,
    size = 9, family = bodyFont
  ) +
  
  ### Inserts the credits text
  annotate(
    "RichText", x = 100, y = 60, label = credText,
    hjust = 1, vjust = 0,
    color = "white", fill = NA, label.colour = NA,
    size = 8, family = bodyFont
  ) +
  
  ### Reverses the y-axis (makes my life easier)
  scale_y_reverse(expand = expansion(mult = c(0.05, 0.1)),
                  limits = c(60, -4)) +
  
  ### Manages the x-axis
  scale_x_continuous(expand = expansion(mult = 0.05),
                     limits = c(-40, 100)) +
  
  ### Decorates the plot
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = NA)
  )

## Generates an accurate preview of the final plot
ggview::ggview(
  plot = p, device = "png",
  width = 5000, height = 5000, units = "px",
  dpi = 320, bg = "black"
)

## Generates the final plot
ggsave("2023/week25/week25.png", plot = p,
       width = 5000, height = 5000, units = "px",
       dpi = 320, bg = "black")
