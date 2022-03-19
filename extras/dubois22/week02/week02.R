# 0. Library and fonts management
library(tidyverse)
library(ggplot2)
library(showtext)
library(glue)
library(scales)
library(ggforce)
library(ggtext)

## Adding Google Fonts
sysfonts::font_add_google(name = "Teko", family = "teko") ### Sans Serif
sans <- "teko"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

# 1. Data download, load and handling
## Data on inequality comes from IBGE
## https://www.ibge.gov.br/estatisticas/sociais/populacao/25844-desigualdades-sociais-por-cor-ou-raca.html?=&t=resultados
rawdata <- read.csv2("extras/dubois22/week02/data.csv")

## Keeps only income for black people and orders the data by it
df <- rawdata %>% 
  dplyr::filter(race == "black") %>% 
  dplyr::arrange(desc(income))

## Associates each year with a color
df <- df %>% 
  dplyr::mutate(color = c("#dc143c","#ffd700","#d2b48c",
                          "#4682b4","#654321","#d2b48c",
                          "#000000"))

## Defines size for the circles
df <- df %>% 
  dplyr::mutate(size = scales::rescale(income, to = c(200,600)))

## Defines some layout constants
lnhgt <- 0.9
bgcolor <- "#d2b48c"
thick <- 0.05
wide <- 0.15
smooth <- 3.3
radius <- c(9.55,8.5,7.4,6.17,5.37,4.9,3.05)
theta <- c(1.5,5.8,3.9,0.5,4.9,2.4)

## Defines coordinates for the titles
titles <- tibble(
  x = 0,
  y = c(14.5,11.3,-13.5),
  size = c(35,12,15),
  label = c(
    "REAL MONTHLY INCOME PER CAPITA\nOF BLACK HOUSEHOLDS IN BRAZIL.",
    "EACH CIRCLE REPRESENTS THE INCOME FOR A GIVEN YEAR. ALL VALUES ARE DEFLATED\nTO 2018 BRAZILIAN REAIS. TRANSPARENT CIRCLES INDICATE YEARS OF INCOME LOSS.",
    "INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ÍCARO BERNARDES (@IcaroBSC)"
  )
)

## Defines coordinates for the message
message <- tibble(
  x = 0,
  y = -11,
  size = 20,
  label = "IN THE SAME PERIOD, WHITE HOUSEHOLDS HAD IN THEIR LOWEST AN INCOME OF R$ 1846.31.<br>
  ALMOST THE <span style='color:white;'>DOUBLE</span> OF BLACK HOUSEHOLDS AT THEIR MAXIMUM (R$ 934.23)."
)

## Defines coordinates for the income labels
incomes <- tibble(
  x = c(8, -2.9, -4.6, 3, -4.3, 2.4, 0),
  y = c(0, 6.7, -4.25, 4.55, 1.15, -2.97, 0),
  label = glue::glue("R$ {df$income}"),
  angle = c(0, 290, 45, 60, 346, 312, 0),
  color = c("white","black","black",
            "white","white","black",
            "white")
)

## Defines coordinates for the years labels
years <- tibble(
  x = 0,
  y = -radius,
  label = df$year,
  color = c("white","black","black",
            "white","white","black",
            "white")
)

## Creates coordinates for the wedges
plt <- df %>% 
  dplyr::select(year, color) %>% 
  dplyr::rename("fill" = "color")
wedges0 <- tibble(
  r = radius[1:6]+0.2,
  start = theta,
  end = start+wide,
  year = df$year[1:6]
)  %>% 
  dplyr::left_join(plt) %>% 
  dplyr::mutate(x0 = 0, y0 = 0, r0 = 1.8)
wedges1 <- tibble(
  r = radius[2:6]+0.2,
  start = theta[1:5]-thick,
  end = start+wide+2*thick,
  year = df$year[2:6]
) %>% 
  dplyr::left_join(plt) %>% 
  dplyr::mutate(x0 = 0, y0 = 0, r0 = 1.8)
wedges2 <- tibble(
  r = radius[3:6]+0.2,
  start = theta[1:4]-2*thick,
  end = start+wide+4*thick,
  year = df$year[3:6]
) %>% 
  dplyr::left_join(plt) %>% 
  dplyr::mutate(x0 = 0, y0 = 0, r0 = 1.8)
wedges3 <- tibble(
  r = radius[4:6]+0.2,
  start = theta[1:3]-3*thick,
  end = start+wide+6*thick,
  year = df$year[4:6]
) %>% 
  dplyr::left_join(plt) %>% 
  dplyr::mutate(x0 = 0, y0 = 0, r0 = 1.8)
wedges4 <- tibble(
  r = radius[5:6]+0.2,
  start = theta[1:2]-4*thick,
  end = start+wide+8*thick,
  year = df$year[5:6]
) %>% 
  dplyr::left_join(plt) %>% 
  dplyr::mutate(x0 = 0, y0 = 0, r0 = 1.8)
wedges5 <- tibble(
  r = radius[6]+0.2,
  start = theta[1]-5*thick,
  end = start+wide+10*thick,
  year = df$year[6]
) %>% 
  dplyr::left_join(plt) %>% 
  dplyr::mutate(x0 = 0, y0 = 0, r0 = 1.8)

# 2. Generates the plot
## Creates the main plot
p <- df %>% 
  ggplot() +
  
  ### Places the "target sections"
  geom_point(aes(x = 0, y = 0, size = I(size), color = I(color))) +
  
  ### Places the wedges
  ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r = r, fill = I(fill),
                            start = start, end = end), color = NA ,
                        radius = unit(smooth, 'mm'), data = wedges5) +
  ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r = r, fill = I(fill),
                            start = start, end = end), color = NA ,
                        radius = unit(smooth, 'mm'), data = wedges4) +
  ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r = r, fill = I(fill),
                            start = start, end = end), color = NA ,
                        radius = unit(smooth, 'mm'), data = wedges3) +
  ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r = r, fill = I(fill),
                            start = start, end = end), color = NA ,
                        radius = unit(smooth, 'mm'), data = wedges2) +
  ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r = r, fill = I(fill),
                            start = start, end = end), color = NA ,
                        radius = unit(smooth, 'mm'), data = wedges1) +
  ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r = r, fill = I(fill),
                            start = start, end = end), color = NA ,
                        radius = unit(smooth, 'mm'), data = wedges0) +
  
  ### Places the titles
  geom_text(aes(x = x, y = y, label = label, size = I(size)),
            family = sans, lineheight = lnhgt, vjust = 1, data = titles) +
  
  ### Places the message and the rectangle highlight
  annotate("tile", x = -5.5, y = -12.1, width = 2.1, height = 0.8, fill = "#dc143c") +
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        family = sans, lineheight = lnhgt, vjust = 1,
                        label.color = NA, fill = NA, data = message) +
  
  ### Places the income labels
  geom_text(aes(x = x, y = y, label = label, color = I(color), angle = angle),
            size = 12, family = sans, data = incomes) +
  
  ### Places the years labels
  geom_text(aes(x = x, y = y, label = label, color = I(color)),
            size = 12, family = sans, data = years) +
  
  ### Defines a coordinates system and limits the axes
  coord_cartesian(xlim = c(-11,11), ylim = c(-14,14)) +
  
  ### Eliminates unnecessary elements and customizes the plot
  theme_void() +
  theme(
    plot.background = element_rect(fill = bgcolor, color = NA)
  )

## Saves the plot
ggsave("2022/week06/income.png", plot = p, dpi = "retina",
       width = 22, height = 28)


