# 0. Library and fonts management
library(tidyverse)
library(ggplot2)
library(showtext)
library(glue)
library(scales)
library(ggforce)

## Adding Google Fonts
sysfonts::font_add_google(name = "Teko", family = "teko") ### Sans Serif
sans <- "teko"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

## Defines some layout constants
r <- 0.05 ### To set the bars size
r1 <- 0.005 ### To set the minor vertical bars size
mult <- 0.9 ### To make the intersection between bars
Vlim <- 1 ### To define the distance to the Y-axis
Hlim <- 1.4 ### To define the distance to the X-axis
lnhgt <- 0.7 ### To set the lineheight
bgcolor <- "#d2b48c"

# 1. Data download, load and handling
## Data on inequality comes from IBGE
## https://www.ibge.gov.br/estatisticas/sociais/populacao/25844-desigualdades-sociais-por-cor-ou-raca.html?=&t=resultados
# rawdata <- read.csv2("extras/dubois22/week07/data.csv")

## Keeps only data of illiteracy for black people
df <- rawdata %>% 
  dplyr::filter(race == "black")

## Adds an estimate for 2019
df <- df %>% 
  tibble::add_row(year = 2019, race = "black", illiteracy = 9.0)

## Rescales the data to a 1-2 range
df <- df %>% 
  dplyr::mutate(x = scales::rescale(illiteracy, to = c(1,2)),
                y = scales::rescale(year, to = c(1,2)))

## Defines the coordinates for the vertical bars
Vbars <- df %>% 
  dplyr::transmute(ymax = max(y)*Hlim,
                   ymin = y*mult,
                   xmin = x*mult,
                   xmax = xmin - r)

## Defines the coordinates for the (major) horizontal bars
Hbars_mjr <- df %>% 
  dplyr::transmute(xmax = max(x)*Vlim,
                   xmin = x*mult,
                   ymin = y*mult,
                   ymax = ymin - r)

## Defines the coordinates for the (minor) horizontal bars
Hbars_mnr <- df %>% 
  dplyr::transmute(xmax = max(x)*Vlim - r1,
                   xmin = x*mult,
                   ymin = y*mult - r1,
                   ymax = ymin - r + 2*r1)

## Defines the coordinates for the circular sections
circles <- df %>% 
  dplyr::transmute(x0 = x*mult,
                   y0 = y*mult,
                   r0 = 0,
                   r = r,
                   r1 = r1,
                   r2 = r - r1)

## Defines the coordinates for the x-axis labels
Xlabels <- df %>% 
  dplyr::transmute(y = max(y)*Hlim,
                   x = x*mult - r/2,
                   label = glue::glue("{format(illiteracy, nsmall = 1)}%")) %>% 
  dplyr::mutate(label = ifelse(row_number() == n(),
                               glue::glue("({label}?)"),
                               label))

## Defines the coordinates for the x-axis labels
Ylabels <- df %>% 
  dplyr::transmute(x = max(x)*Vlim,
                   y = y*mult - r/2,
                   label = year,
                   size = 22) %>% 
  dplyr::mutate(label = ifelse(row_number() == n(),
                               glue::glue("({label}?)"),
                               label)) %>% 
  tibble::add_row(x = max(df$x)*Vlim,
                  y = max(df$y)*Hlim+0.02,
                  label = "PERCENT OF\nILLITERACY",
                  size = 12)

## Defines the coordinates for the titles
titles <- tibble(
  x = 1.45,
  y = c(0.56,0.7,2.95),
  label = c("ILLITERACY RATE OF BLACK PEOPLE IN BRAZIL.",
            "ILLITERACY RATE AMONG PEOPLE AGED 15 OR OLDER (2018).",
            "INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ÍCARO BERNARDES (@IcaroBSC)"),
  size = c(35,14,16)
)

## Defines the coordinates for the message
message <- tibble(
  x = 0.7,
  y = c(0.93,1.18),
  label = c("31.8%",
            "OF BLACKS AGED 65 OR OLDER\nWERE ILLITERATE IN 2018.\nILLITERATE WHITES IN\nTHE SAME AGE RANGE\nWERE ONLY 12.3%."),
  size = c(80,16),
  color = "#654321"
)

# 2. Generates the plot
## Creates the main plot
p <- ggplot(NULL) +
  
  ### Places the bars
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "black", data = Vbars) + #### Black vertical bars
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "black", data = Hbars_mjr) + #### Black horizontal bars
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = bgcolor, data = Hbars_mnr) + #### Beige horizontal bars
  
  ### Places the circle arcs
  ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r = r, start = pi/4, end = pi/2),
                        color = NA, fill = "black", data = circles) + #### Black lower arc
  ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r = r, start = 0, end = pi/4),
                        color = NA, fill = "black", data = circles) + #### Black upper arc
  ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r1, r = r2, start = 0, end = pi/4),
                        color = NA, fill = bgcolor, data = circles) + #### Beige upper arc
  
  ### Places the axes labels
  geom_text(aes(x = x, y = y, label = label), size = 12,
            vjust = 1, nudge_y = -0.02, family = sans, data = Xlabels) +
  geom_text(aes(x = x, y = y, label = label, size = I(size)),
            nudge_x = -0.1, family = sans, lineheight = lnhgt, data = Ylabels) +
  
  ### Places the titles
  geom_text(aes(x = x, y = y, label = label, size = I(size)),
            vjust = 1, family = sans, data = titles) +
  
  ### Places the message
  geom_text(aes(x = x, y = y, label = label, size = I(size), color = I(color)),
            hjust = 1, family = sans, lineheight = lnhgt, data = message) +
  
  ### Reverses and sets limits for the axes and eliminates axis expansion
  scale_x_reverse(limits = c(2.35,0.55), expand = expansion(0,0)) + 
  scale_y_reverse(limits = c(3.1,0.45), expand = expansion(0,0)) +
  
  ### Ensures that the proportions between axes are equal
  coord_equal(expand = FALSE) +
  
  ### Eliminates non-necessary elements and customizes the plot
  theme_void() +
  theme(
    plot.background = element_rect(fill = bgcolor, color = NA)
  )

## Saves the plot
ggsave("2022/week10/illiteracy.png", plot = p, dpi = "retina",
       width = 22, height = 28)

