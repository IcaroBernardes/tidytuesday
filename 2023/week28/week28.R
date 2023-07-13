# 0. Initial setup ##########
## Loads libraries
library(dplyr)
library(ggforce)
library(ggpath)
library(ggplot2)
library(ggtext)
library(ggview)
library(glue)
library(htmltools)
library(junebug)
library(readr)
library(stringr)

## Loads the dataset and keeps only Arctic region data
rawData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/zonann_temps.csv') |> 
  dplyr::select(Year, `64N-90N`) |> 
  dplyr::rename(year = "Year", temp = "64N-90N")

## Takes all font styles that share that exact family name and
## registers them (makes them visible to {systemfonts})
junebug::font_hoist("Font Awesome 6 Brands")

## Gets the info used to register the font families
fonts_register <- systemfonts::registry_fonts()

## Defines the font families
brandsFont <- "Font Awesome 6 Brands Regular"

# 1. Plot production ##########
## Defines the coordinates of the messages
message <- dplyr::tibble(
  xImg = rep(c(2085, 2115), 3),
  xText = xImg + rep(c(-13, 13), 3),
  y = seq(2.7, -1.3, length.out = 6),
  hjust = rep(c(1, 0), 3),
  path = c("factory", "globe", "ice",
           "sun", "temperature", "disaster"),
  title = c(
    "HUMAN<br>ACTIVITY",
    "GLOBAL<br>WARMING",
    "SEA ICE<br>MELTING",
    "REFLECTION<br> POWER DECREASE",
    "INCREASING<br>WARMING",
    "SEVERE<br>CONSEQUENCES"
  ),
  detail = c(
    "SINCE THE INDUSTRIAL REVOLUTION,<br>
    THE USE OF FOSSILE FUELS BY<br>
    HUMANS INCREASED GREATLY",
    
    "GREENHOUSE GASES INCREASE<br>
    THE HEAT RETENTION ON<br>
    THE EARTH'S SURFACE",
    
    "HIGHER TEMPERATURES<br>
    PROVOKE MELTING OF<br>
    THE ARCTIC SEA ICE",
    
    "MORE REFLECTIVE SEA ICE<br>
    GIVES WAY TO DARK OCEAN WATERS,<br>
    WHICH ABSORB MORE SOLAR RADIATION",
    
    "AS THE ARCTIC BECOMES LESS<br>
    ABLE TO REFLECT SOLAR RADIATION,<br>
    IT BECOMES MORE SUCEPTIBLE TO HEATING",
    
    "OTHER EFFECTS OF ARCTIC WARMING<br>
    ARE COASTAL EROSION, WILDLIFE LOSS,<br>
    WEATHER SYSTEMS CHANGES"
  )
) |> 
  dplyr::mutate(path = glue::glue("2023/week28/SVG/{path}.svg"))

## Defines the coordinates of the arrows
messageArrows <- message |> 
  dplyr::mutate(
    x = xImg, xend = lead(xImg),
    y = y, yend = lead(y)
  )

## Defines the plot title
plotTitle <- tagList(
  "THE EVER INCREASING",
  br(),
  span("ARCTIC WARMING", style = "color:#D10F30;")
)
plotTitle <- as.character(plotTitle) |> 
  stringr::str_remove_all("\n")

## Defines the plot subtitle
plotSubtitle <- tagList(
  "This line plot represents ",
  span("Arctic's region surface temperature", style = "color:white;"),
  " (latitudes 64N-90N).",  br(),
  "It shows how much a given year diverges from the", br(),
  "average temperature of the period of 1951-1980.", br(),
  "The color scale goes from ",
  span("blue", style = "color:#0c77dc;"),
  " (below), to ",
  span("white", style = "color:white;"),
  " (average) and ",
  span("red", style = "color:#e8192a;"),
  " (over)."
)
plotSubtitle <- as.character(plotSubtitle) |> 
  stringr::str_remove_all("\n")

## Creates a function that simplifies the process of placing 
## the Font Awesome glyphs on the text
faDecoder <- function(code, handle) {
  tagList(
    span(code, style = glue::glue("font-family:\"{brandsFont}\";")),
    handle
  )
}

## Defines the plot authorship
plotAuthorship <- tagList(
  'Data from: NASA GISS Surface', br(),
  'Temperature Analysis (GISTEMP v4)', br(),
  'Made by Ícaro Bernardes: ', br(),
  faDecoder('\uf099', ' - @IcaroBSC'), br(),
  faDecoder('\uf09b', ' - @IcaroBernardes'), br(),
  faDecoder('\uf08c', ' - @icarobsc')
)
plotAuthorship <- as.character(plotAuthorship) |> 
  stringr::str_remove_all("\n")

## Create the plot
plot <- message |> 
  ggplot() +
  
  ### Places the line plot
  ggforce::geom_link2(
    aes(x = year, y = temp, color = temp),
    linewidth = 2, lineend = "round", data = rawData
  ) +
  
  ### Places the title
  annotate(
    "RichText", x = 1880, y = 3.2, label = plotTitle,
    hjust = 0, vjust = 1, size = 18,
    fill = NA, label.colour = NA, color = "white",
    lineheight = 1, family = "Walter Turncoat"
  ) +
  
  ### Places the subtitle
  annotate(
    "RichText", x = 1880, y = 2.35, label = plotSubtitle,
    hjust = 0, vjust = 1, size = 7,
    fill = NA, label.colour = NA, color = "#a3a3a3",
    lineheight = 1.1, family = "Neucha"
  ) +
  
  ### Places the authorship
  annotate(
    "RichText", x = 1880, y = 1.5, label = plotAuthorship,
    hjust = 0, vjust = 1, size = 5,
    fill = NA, label.colour = NA, color = "#a3a3a3",
    lineheight = 1.3, family = "Sintony"
  ) +
  
  ### Places the arrows
  geom_segment(
    aes(x = x, xend = xend, y = y, yend = yend),
    arrow = arrow(angle = 5, length = unit(0.1, "npc"), type = "closed"),
    color = "white", linewidth = 2, data = messageArrows
  ) +
  
  ### Places circles around the icons
  geom_point(aes(x = xImg, y = y), size = 60) +
  
  ### Places the icons
  ggpath::geom_from_path(
    aes(x = xImg, y = y, path = path),
    width = 0.15, height = 0.15, color = "white"
  ) +
  
  ### Places the message titles
  ggtext::geom_richtext(
    aes(x = xText, y = y, label = title, hjust = hjust),
    fill = NA, label.colour = NA, color = "white",
    size = 9, vjust = 0, nudge_y = 0.01,
    lineheight = 1, family = "Neucha"
  ) +
  
  ### Places the message details
  ggtext::geom_richtext(
    aes(x = xText, y = y, label = detail, hjust = hjust),
    fill = NA, label.colour = NA, color = "white",
    size = 4, vjust = 1, nudge_y = -0.03,
    lineheight = 1.2, family = "Sintony"
  ) +
  
  ### Places the min and max values of the series
  annotate(
    "text", x = 1889, y = -1.77, label = -1.77,
    hjust = 0, family = "Neucha",
    size = 15, color = "#94a6d8"
  ) +
  annotate(
    "text", x = 2014, y = 3.24, label = 3.24,
    hjust = 1, family = "Neucha",
    size = 15, color = "#e83a38"
  ) +
  
  ### Defines aesthetic rules
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.2))) +
  scale_color_gradient2(
    low = "#0c77dc", mid = "white", high = "#e8192a",
    midpoint = 0, guide = "none", limits = c(-3.5, 3.5)
  ) +
  
  ### Defines aesthetics elements
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", colour = NA)
  )

## Generates an accurate preview of the final plot
ggview::ggview(
  plot = plot, device = "png",
  width = 8000, height = 3000, units = "px",
  dpi = 320, bg = "black"
)

## Generates the final plot
ggsave("2023/week28/week28.png", plot = plot,
       width = 8000, height = 3000, units = "px",
       dpi = 320, bg = "black")
