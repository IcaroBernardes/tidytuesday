############################ DuBoisChallenge ###################################
# 0. Library and fonts management
library(tidyverse)
library(ggplot2)
library(showtext)
library(glue)
library(sf)
library(santoku)
library(patchwork)

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
rawdata <- read.csv2("2022/week07/data.csv")

## Gets the shapes of the states of Brazil and adjusts their names
ufs <- readRDS("2022/week07/ufs.RDS") %>% 
  dplyr::mutate(name_state = stringr::str_to_title(name_state),
                name_state = stringr::str_replace(name_state,"Amazônas","Amazonas"))

## Defines limits so both variables are divided into three
## groups of even width and easily understandable boundaries
min_mobile <- 10*(min(rawdata$mobile) %/% 10)
min_internet <- 10*(min(rawdata$internet) %/% 10)
inferior <- min(min_mobile,min_internet)

max_mobile <- 10*((max(rawdata$mobile) %/% 10)+1)
max_internet <- 10*((max(rawdata$internet) %/% 10)+1)
superior <- max(max_mobile,max_internet)

if (superior > 100) {superior <- 100}
bounds <- seq(inferior, superior, length.out = 4)
bounds <- santoku::brk_manual(bounds, c(TRUE,TRUE,TRUE,FALSE))

## Creates categorical variables which represent these groups
## and a numerical one to represent their crossing
df <- rawdata %>% 
  dplyr::mutate(mobile_cat = santoku::chop(mobile, bounds, extend = FALSE),
                internet_cat = santoku::chop(internet, bounds, extend = FALSE),
                cat = glue::glue("{as.numeric(mobile_cat)}{as.numeric(internet_cat)}"),
                cat = as.numeric(cat))

## Defines and adds the colors for each crossing
cross_color <- tibble(
  cat = c(11,22,33,
          12,13,
          21,31,
          23,32),
  color = c("white","#C798B3","#A35D86",
            "#91b6d4","#4682b4",
            "#f2738c","#dc143c",
            "#a97ab2","#ba8187")
)
df <- df %>% 
  dplyr::left_join(cross_color)

## Reshapes the data so each line corresponds to a state
df <- df %>% 
  dplyr::select(-cat) %>% 
  tidyr::pivot_wider(names_from = race,
                     values_from = matches("^(mobile|internet|color)"))

## Joins data and shapes using the names of the states
df <- df %>% 
  dplyr::mutate(uf = stringr::str_to_title(uf)) %>% 
  dplyr::full_join(ufs, by = c("uf" = "name_state"))

## Creates coordinates for the titles
titles <- tibble(
  x = -53,
  y = c(14.5,8,-56.5),
  size = c(32,15,18),
  label = c(
    "ACCESS OF BLACK PEOPLE TO A PERSONAL MOBILE PHONE<br>
    AND INTERNET IN THE STATES OF BRAZIL.",
    "PROPORTION OF THE BLACK POPULATION FROM AGES 10 AND ABOVE<br>
    THAT HAVE ACCESS TO THESE ITENS (2017).",
    "INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ÍCARO BERNARDES (@IcaroBSC)"
  )
)

## Creates coordinates for the highlights
highlights <- tibble(
  x = c(-52.05,-47.75,-30.95,-27.8),
  y = rep(c(-45.9,-47),2),
  fill = c(rep("#f2738c",2),rep("#C798B3",2))
)

## Gets access data from the most unequal state
## (in terms of access) between blacks and whites
acs_int_black <- df %>% 
  dplyr::filter(uf == "Amazonas") %>% 
  dplyr::pull(internet_black)
acs_int_white <- df %>% 
  dplyr::filter(uf == "Amazonas") %>% 
  dplyr::pull(internet_white)
acs_mbl_black <- df %>% 
  dplyr::filter(uf == "Amazonas") %>% 
  dplyr::pull(mobile_black)
acs_mbl_white <- df %>% 
  dplyr::filter(uf == "Amazonas") %>% 
  dplyr::pull(mobile_white)

## Creates coordinates for the instructions
instructions <- tibble(
  x = c(-82.5,-65,-53),
  y = c(-39,-38.1,-46),
  size = c(20,12,8.5),
  label = c(
    "HOW TO READ THIS MAP:",
    "WHITE AND BLACK PEOPLES PROPORTION OF ACCESS IN THE STATES WERE GATHERED.<br>
    THE VALUES FOR THESE TWO VARIABLES WERE SEPARATED INTO THREE RANGES EACH.<br>
    THE CROSSING BETWEEN THESE RANGES WAS USED TO CREATE THE NINE GROUPS REPRESENTED IN THE LEFT.",
    glue::glue("IN THE MOST UNEQUAL STATE IN TERMS OF ACCESS TO THESE ITENS, AMAZONAS,<br>{acs_int_black}% OF THE BLACK PEOPLE HAVE ACCESS TO THE INTERNET (COMPARED TO {acs_int_white}% OF THE WHITE PEOPLE).<br>FURTHERMORE {acs_mbl_black}% OF THE BLACKS HAVE A PERSONAL MOBILE PHONE (COMPARED TO {acs_mbl_white}% THE WHITES)")
  )
)

## Creates coordinates for the tiles
cross_color <- cross_color %>% 
  dplyr::arrange(cat)
tiles <- tibble(
  x = c(rep(-77,3),rep(-74,3),rep(-71,3)),
  y = rep(c(-49,-46,-43),3),
  fill = cross_color$color
)

## Creates coordinates for the labels
labels <- tibble(
  x = c(rep(-81,3),-81,c(-77,-74,-71),-68),
  y = c(c(-49,-46,-43),-51,rep(-52,3),-52),
  size = 6,
  label = c(levels(df$internet_cat_black),"INTERNET<br>ACCESS",
            levels(df$mobile_cat_black),"MOBILE<br>ACCESS")
)

## Defines some layout constants
lnhgt <- 1.1
bgcolor <- "#d2b48c"
contourcolor <- "#bdffcc"

# 2. Generates the plot
## Creates plot for the state used as example
example <- df %>% 
  dplyr::filter(uf == "Amazonas") %>% 
  ggplot() +
  geom_sf(aes(fill = I(color_black), geometry = geom), color = contourcolor) +
  theme_void()

## Creates the main plot
p <- df %>% 
  ggplot() +
  
  ### Places the states
  geom_sf(aes(fill = I(color_black), geometry = geom), color = contourcolor) +
  
  ### Places the titles
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        fill = NA, label.color = NA, family = sans,
                        lineheight = lnhgt, data = titles) +
  
  ### Places the highlights
  geom_tile(aes(x = x, y = y, fill = I(fill)),
            width = 2, height = 1, data = highlights) +
  
  ### Places the instructions
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        fill = NA, label.color = NA, family = sans, hjust = 0,
                        lineheight = lnhgt, data = instructions) +
  
  ### Places the labels
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        fill = NA, label.color = NA, family = sans,
                        lineheight = lnhgt, data = labels) +
  
  ### Places the tiles
  geom_tile(aes(x = x, y = y, fill = I(fill)), color = contourcolor, size = 1,
            width = 3, height = 3, data = tiles) +
  
  ### Defines limits to the plot
  coord_sf(xlim = c(-85,-22), ylim = c(-60,20), expand = FALSE) +
  
  ### Eliminates unnecessary elements and customizes the plot
  theme_void() +
  theme(
    plot.background = element_rect(fill = bgcolor, color = NA)
  ) +
  
  ### Places the plot of the example state
  patchwork::inset_element(example, left = 0.32, right = 0.5,
                           bottom = 0.1, top = 0.24)

## Saves the plot
ggsave("2022/week07/access.png", plot = p, dpi = "retina",
       width = 22, height = 28)

