# 0. Library and fonts management
library(tidyverse)
library(ggplot2)
library(showtext)
library(scales)
library(glue)
library(ggtext)

## Adding Google Fonts
sysfonts::font_add_google(name = "Teko", family = "teko") ### Sans Serif
sans <- "teko"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

## Defines some layout constants
lnhgt <- 0.9 ### To set the lineheight
bgcolor <- "#d2b48c"
pal <- c("#dc143c", "#B7A184", "#ffd700",
         "#654321", "#4682b4", "#ffc0cb")
angle_cat_small <- 6.3*pi ### Final angle of the smallest value
angle_cat_big <- 2.7*pi ### Final angle of the highest value
angle_base <- 6.5*pi ### Start angle of all categories
step <- (angle_cat_big-angle_base)/5000 ### Spiral construction step size
a <- 1 ### Constant for the spirals

# 1. Data download, load and handling
## Data on inequality comes from IBGE
## https://www.ibge.gov.br/estatisticas/sociais/populacao/25844-desigualdades-sociais-por-cor-ou-raca.html?=&t=resultados
rawdata <- read.csv2("extras/dubois22/week08/data.csv")

## Gets only year and rate for blacks. Converts year to factor
df <- rawdata %>% 
  dplyr::filter(race == "black") %>% 
  dplyr::mutate(year = factor(year, levels = 2017:2012)) %>% 
  dplyr::select(year, rate = rate_per_100k, total = total_country)

## Creates a new tibble that contains the angle 
## in which the spiral of each category will end
points <- df %>% 
  dplyr::mutate(
    angle_end = scales::rescale(rate, to = c(angle_cat_small,angle_cat_big))
  )

## Calculates the points to draw the spirals
points <- points %>% 
  dplyr::group_by(year) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(
    data = purrr::map(
      data,
      ~ tibble(
        angle_end = .$angle_end,
        fi = seq(angle_base, angle_end, by = step),
        id = as.numeric(year)-1,
        r = a*fi + id,
        x = r*cos(fi),
        y = r*sin(fi)
      )
    )
  ) %>% 
  tidyr::unnest(cols = data) %>%
  dplyr::select(year, x, y)

## Adds more points to the beginning of the spirals to make all of them parallel
points <- points %>% 
  dplyr::slice(1L) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(
    data = purrr::map(
      data,
      ~ tibble(
        x = c(.$x+50*step,
              .$x+30*step),
        y = .$y
      )
    )
  ) %>% 
  tidyr::unnest(cols = data) %>%
  dplyr::select(year, x, y) %>% 
  rbind(points) %>% 
  dplyr::mutate(row = row_number()) %>% 
  dplyr::arrange(year, row) %>% 
  dplyr::select(-row)

## Defines coordinates for the labels
df <- points %>% 
  dplyr::slice(1L) %>% 
  dplyr::left_join(df) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(year)) %>% 
  dplyr::mutate(x = x-1,
                rate = format(rate, nsmall = 1),
                unit = ifelse(row_number() == 1:2, "DEATHS PER 100K PEOPLE", "''"),
                total = format(round(total/1000, digits = 1), nsmall = 1),
                label = glue::glue("{rate} <span style='font-size:20px;'>{unit} | (TOTAL: {total}K DEATHS)</span>"),
                x_order = x-10,
                label_order = year)

## Defines the titles and message between
title <- "RATE OF HOMICIDES OF BLACKS PER 100K PEOPLE."
subtitle <- "NUMBER OF BLACKS THAT WERE VICTIMS OF MURDER IN BRAZIL FOR EACH 100K BLACK HABITANTS. DATA FROM 2012 TO 2017."
message <- tibble(
  x = c(0.4,0.5,0.4,0.5),
  y = c(2.9,2.9,-1.3,-1.3),
  size = c(32,13,51,13),
  hjust = c(1,0,1,0),
  label = c(
    "65K",
    "PEOPLE WERE VICTIMS OF<br>HOMICIDE IN BRAZIL IN 2017.",
    "43%",
    "OF THEM WERE<br>BLACK MEN AGED<br>BETWEEN 15 AND 29."
  )
)

# 2. Generates the plot
## Creates the main plot
p <- ggplot(NULL) +
  
  ### Places the spirals
  geom_path(aes(x = x, y = y, group = year), color = "black", size = 13.4,
            data = points) +
  geom_path(aes(x = x, y = y, group = year, color = year), size = 12.6,
            data = points %>% dplyr::slice(-c(1L,n()))) +
  
  ### Places the lines of the labels
  geom_linerange(aes(y = y, xmin = x_order, xmax = x), data = df) + 
  
  ### Places the labels
  ggtext::geom_richtext(aes(x = x, y = y, label = label),
                        label.color = NA, fill = bgcolor, hjust = 1,
                        label.margin = unit(c(0, 0, 0, 0.5), "lines"),
                        family = sans, size = 10, data = df) +
  ggtext::geom_richtext(aes(x = x_order, y = y, label = label_order),
                        label.color = NA, fill = bgcolor, hjust = 1,
                        label.margin = unit(c(0, 0.5, 0, 0), "lines"),
                        family = sans, size = 10, data = df) +
  
  ### Places the message
  ggtext::geom_richtext(aes(x = x, y = y, label = label,
                            size = I(size), hjust = hjust), vjust = 1,
                        fill = NA, label.color = NA, family = sans,
                        lineheight = lnhgt, color = "#654321", data = message) +
  
  ### Places the titles
  labs(title = title, subtitle = subtitle) +
  
  ### Applies the Du Bois colors to the spirals
  scale_color_discrete(type = pal, guide = "none") +
  
  ### Controls the proportions
  coord_equal() +
  
  ### Eliminates most theme elements and customizes the rest
  theme_void() +
  theme(
    text = element_text(family = sans),
    plot.background = element_rect(fill = bgcolor, color = NA),
    plot.title = element_text(size = 100, hjust = 0.5, vjust = 1, lineheight = lnhgt,
                              margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
    plot.subtitle = element_text(size = 35, hjust = 0.5, vjust = 1, lineheight = 0.9,
                                 margin = margin(t = 30, r = 0, b = 80, l = 0, unit = "pt")),
    plot.margin = margin(t = 0, r = 150, b = 150, l = 150, unit = "pt")
  )

## Saves the plot
ggsave("extras/dubois22/week08/violence.png", plot = p, dpi = "retina",
       width = 22, height = 28)

