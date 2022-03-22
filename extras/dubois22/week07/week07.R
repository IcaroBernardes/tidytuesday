# 0. Library and fonts management
library(tidyverse)
library(ggplot2)
library(showtext)
library(glue)
library(seriation)
library(ggtext)

## Adding Google Fonts
sysfonts::font_add_google(name = "Teko", family = "teko") ### Sans Serif
sans <- "teko"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

## Defines some layout constants
lnhgt <- 0.7 ### To set the lineheight
bgcolor <- "#d2b48c"

# 1. Data download, load and handling
## Data on inequality comes from IBGE
## https://www.ibge.gov.br/estatisticas/sociais/populacao/25844-desigualdades-sociais-por-cor-ou-raca.html?=&t=resultados
rawdata <- read.csv2("extras/dubois22/week07/data.csv")

## Converts the data into a matrix in which each row has all six values for each state
mat <- rawdata %>% 
  tidyr::pivot_wider(
    names_from = c("race", "capital"),
    values_from = "percent"
  )
states <- mat$state
mat <- as.matrix(mat[,-1])
rownames(mat) <- states

## Uses a BEA_TSP algorithm to determine the order of rows
## that minimizes difference between adjacent rows
set.seed(42)
order <- seriation::seriate(mat, method = "BEA_TSP")
order <- seriation::get_order(order, 1)
order <- states[order]

## Applies the obtained order to the data and create a numeric variable
df <- rawdata %>% 
  dplyr::mutate(state = factor(state, levels = order),
                y = as.numeric(state))

## Defines the order of the capital variable
df <- df %>% 
  dplyr::mutate(capital = factor(capital,
                                 levels = c("more than 1M", "100k to 1M", "less than 100k")))

## Inverts the values of percentage for black so the data is on the left
df <- df %>% 
  dplyr::mutate(percent = ifelse(race == "black", -percent, percent))

## Defines coordinates for the categories labels
categories <- tibble(
  race = c(rep("white",3),rep("black",3)),
  y = rep(c(6,16,26),2)
) %>% 
  dplyr::mutate(x = c(40, 80, 88, -40, -80, -88),
                angle = c(rep(-45,3),rep(45,3)),
                label = rep(unique(df$capital),2)) %>% 
  dplyr::mutate(label = toupper(label),
                label = stringr::str_wrap(label, width = 8))

## Defines the breaks for the x-axis
x_breaks <- c(
  seq(-100, -10, by = 10),
  seq(10, 100, by = 10)
)

## Defines the titles and message
title <- "CAPITAL OF CANDIDATURES FOR THE LOWER HOUSE\nOF THE BRAZILIAN CONGRESS BY RACE AND STATE."
subtitle <- "
THIS DATA IS FROM 2018 AND THE CURRENCY IS BRAZILIAN REAIS (BRL). FOR COMPARISSON, ONE US DOLLAR THIS YEAR WAS WORTH 3.65 BRL IN AVERAGE.
STATES ARE ORDERED BY SIMILARITY OF THE PERCENTAGES (USING BOND ENERGY AND TRAVELING SALESPERSON ALGORITHM).
INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ÍCARO BERNARDES (@IcaroBSC)
"
message <- "
ALL WHITE CANDIDATURES TOGETHER RECEIVED ALMOST 285M USD WHILE BLACK ONES RECEIVED 86M USD.<br>
THIS IS LESS THAN <span style = color:'white';font-size:80px;>A THIRD</span> OF WHAT WHITES OBTAINED.
"

# 2. Generates the plot
## Creates the main plot
p <- df %>% 
  ggplot() +
  
  ### Places the bars 
  geom_col(aes(x = percent, y = y, fill = capital), color = "black",
           size = 0.2, width = 1, orientation = "y") +
  
  ### Places the labels of the categories
  geom_text(aes(x = x, y = y, label = label, angle = angle),
            family = sans, size = 16, lineheight = lnhgt, data = categories) +
  
  ### Places a central line that highlights the zero
  geom_vline(xintercept = 0, size = 0.5) +
  
  ### Facets the plot between the two groups
  facet_grid(.~race, scales = "free_x", labeller = labeller(.cols = ~toupper(glue::glue("{.}S.")))) +
  
  ### Places the titles and message
  labs(title = title, subtitle = subtitle, caption = message) +
  
  ### Eliminates excess of elements
  theme_void() +
  
  ### Customizes the plot
  theme(
    text = element_text(family = sans),
    axis.line = element_blank(),
    axis.title.x = element_text(size = 28,
                                margin = margin(t = 10, r = 0, b = 0, l = 0, unit = "pt")),
    axis.text.x = element_text(size = 28,
                               margin = margin(t = 5, r = 0, b = 0, l = 0, unit = "pt")),
    axis.text.y = element_text(size = 32),
    axis.text.y.left = element_text(hjust = 1,
                                    margin = margin(t = 0, r = 20, b = 0, l = 0, unit = "pt")),
    axis.text.y.right = element_text(hjust = 0,
                                     margin = margin(t = 0, r = 0, b = 0, l = 20, unit = "pt")),
    plot.margin = margin(t = 80, r = 70, b = 20, l = 70, unit = "pt"),
    plot.background = element_rect(fill = bgcolor, color = NA),
    plot.title = element_text(size = 90, hjust = 0.5, vjust = 1, lineheight = lnhgt,
                              margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
    plot.subtitle = element_text(size = 30, hjust = 0.5, vjust = 1, lineheight = 0.9,
                                 margin = margin(t = 10, r = 0, b = 50, l = 0, unit = "pt")),
    plot.caption = ggtext::element_textbox(
      size = 45, halign = 0.5, valign = 1, lineheight = 1.1, fill = "#654321",
      color = bgcolor, box.color = NA, width = 2, hjust = 0.5,
      margin = margin(t = 30, r = 0, b = 30, l = 0, unit = "pt"),
      padding = margin(t = 30, r = 0, b = 30, l = 0, unit = "pt")
    ),
    strip.text = element_text(size = 32,
                              margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
    panel.ontop = TRUE,
    panel.spacing = unit(0, "lines"),
    panel.grid.minor.x = element_line(color = "black", size = 0.1),
    panel.grid.major.x = element_line(color = "black", size = 0.2)
  ) +
  
  ### Defines the axes scales
  scale_x_continuous(name = "PER CENTS.", breaks = x_breaks, minor_breaks = seq(-100,100,2),
                     labels = abs, expand = expansion(0,0)) +
  scale_y_continuous(name = NULL, breaks = 1:n_distinct(df$state),
                     labels = toupper(order), expand = expansion(0,0), sec.axis = dup_axis()) + 
  
  ### Defines the colors of the categories
  scale_fill_discrete(type = c("#00aa00","#dc143c","#4682b4"), guide = "none")

## Saves the plot
ggsave("extras/dubois22/week07/congress.png", plot = p, dpi = "retina",
       width = 22, height = 28)

