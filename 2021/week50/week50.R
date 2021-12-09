# 0. Library management
library(tidyverse)
library(ggplot2)
library(showtext)
library(readr)
library(scales)
library(tm)
library(stringi)
library(tidytext)
library(ggimage)
library(ggforce)

## Adding Google Fonts
font_add_google(name = "Rubik", family = "rubik") ### Sans Serif
sans <- "rubik"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_auto()

# 1. Data download, load and handling
spiders <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')

## Counts which countries are most frequently explicitly named in the distribution column
countries <- spiders %>% 
  dplyr::select(distribution) %>% 
  dplyr::mutate(distribution = tolower(distribution)) %>% 
  dplyr::mutate(distribution = tm::removePunctuation(distribution)) %>%
  dplyr::mutate(distribution = stringi::stri_trans_general(distribution, id = "Latin-ASCII")) %>% 
  dplyr::mutate(distribution = tm::removeNumbers(distribution)) %>% 
  dplyr::mutate(distribution = tm::stripWhitespace(distribution)) %>% 
  tidytext::unnest_tokens(txt, distribution) %>% 
  dplyr::count(txt) %>% 
  dplyr::arrange(desc(n))

## Filters spiders from Brazil
spiders <- spiders %>% 
  dplyr::filter(stringr::str_detect(distribution, "Brazil"))

## Gets the most frequent families that make up to 75%
fam <- spiders %>% 
  dplyr::count(family) %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::mutate(pct = 100*cumsum(n)/sum(n)) %>% 
  dplyr::filter(cumall(lag(pct, default = 0) <= 75))

## Creates variables to build the "spiders"
fam <- fam %>% 
  dplyr::mutate(szB = scales::rescale(n, to = c(10,50))) %>% 
  dplyr::mutate(szH = szB*0.75) %>% 
  dplyr::arrange(n)
fam <- rbind(
  fam,
  tibble(
    family = rep(NA,6),
    n = rep(NA,6),
    pct = rep(NA,6),
    szB = rep(NA,6),
    szH = rep(NA,6)
  ))

## Filters spiders from the most frequent families
spiders <- spiders %>% 
  dplyr::filter(family %in% fam$family)

## Creates the data for webs and spiders positions
slope <- c(1.2,0.6,-0.05,-0.4, # a1, a2, a3, a4
           -0.2,-4,1) # a12, a23, a34
dots <- tibble(base = seq(300, 1500, by = 300)) %>% 
  dplyr::mutate(
    x_1 = base,
    y_1 = x_1*slope[1],
    b_12 = x_1*(slope[1]-slope[5]),
    x_2 = b_12/(slope[2]-slope[5]),
    y_2 = x_2*slope[2],
    b_23 = x_2*(slope[2]-slope[6]),
    x_3 = b_23/(slope[3]-slope[6]),
    y_3 = x_3*slope[3],
    b_34 = x_3*(slope[3]-slope[7]),
    x_4 = b_34/(slope[4]-slope[7]),
    y_4 = x_4*slope[4],
    group = 1:n()
  )

lines <- dots %>% 
  tidyr::pivot_longer(cols = starts_with("x"), names_to = c("namex","numx"),
                      names_sep = "_", values_to = "x") %>% 
  tidyr::pivot_longer(cols = starts_with("y"), names_to = c("namey","numy"),
                      names_sep = "_", values_to = "y") %>% 
  dplyr::filter(numx == numy)

lines <- lines %>% 
  cbind(fam) %>% 
  dplyr::mutate(xH = x + 1.5*szB/sqrt(2)) %>% 
  dplyr::mutate(yH = y + 1.5*szB/sqrt(2)) %>% 
  dplyr::mutate(xL1 = x - 1.5*szB/sqrt(2)) %>% 
  dplyr::mutate(yL1 = y - 1.5*szB/sqrt(2)) %>% 
  dplyr::mutate(xL2 = x + 1.3*szB/sqrt(2)) %>% 
  dplyr::mutate(yL2 = y + 1.3*szB/sqrt(2)) %>% 
  dplyr::mutate(xP = x + 2.2*szB/sqrt(2)) %>% 
  dplyr::mutate(yP = y + 2.2*szB/sqrt(2))

## Creates data for the spiders families
families <- lines %>% 
  dplyr::slice(c(1,5,9,13,
                 2,6,10,14,
                 3,7,11,
                 4,8,12)) %>% 
  dplyr::mutate(x = x-c(15,20,25,65,
                        15,25,35,65,
                        45,50,55,
                        35,45,60)) %>% 
  dplyr::mutate(y = y-c(35,40,50,95,
                        30,35,40,60,
                        15,20,20,
                        7,4,1)) %>% 
  dplyr::mutate(angle = c(rep(47,4),
                          rep(27,4),
                          rep(-4,3),
                          rep(-21,3))) %>% 
  dplyr::mutate(label = paste0(family,": ",n)) %>% 
  na.exclude()

## Creates data for the titles
titles <- tibble(
  x = -150,
  y = c(1950,1670,1380),
  size = c(180,43,38),
  label = c("A Wrapping Land",
            "Brazil often is mentioned (3910) in the World Spider Database.\nThis snaring land of beauty and spiders has less\nspecies than China (5192) and Australia (5118),\nbut more than the US (3596)",
            "Data from: World Spider Database\nGraphic by: Ícaro Bernardes | @IcaroBSC")
)

## Creates the plot
p <- lines %>% 
  ggplot() +
  
  ### Places the web
  geom_abline(slope = slope[1:4], intercept = 0, color = "white", size = 5) +
  geom_path(aes(x = x, y = y, group = group), color = "white", size = 2) +
  
  ### Places the Brazilian flag and a border
  ggplot2::annotate("point", x = 0, y = 0, color = "white", size = 100) +
  ggimage::geom_image(aes(x = 0, y = 0, image = 'https://cdn-icons-png.flaticon.com/512/197/197386.png'), size = 0.1) +
  
  ### Places the spiders bodies and heads
  ggforce::geom_circle(aes(x0 = x, y0 = y, r = szB), fill = "black") +
  ggforce::geom_circle(aes(x0 = xH, y0 = yH, r = szH), fill = "black") +
  
  ### Places the spiders legs
  ggforce::geom_arc(aes(x0 = xL1, y0 = yL1, r = szH, start = 1.5*pi, end = 3*pi)) +
  ggforce::geom_arc(aes(x0 = xL1, y0 = yL1, r = 1.5*szH, start = 1.5*pi, end = 3*pi)) +
  ggforce::geom_arc(aes(x0 = xL2, y0 = yL2, r = 1.5*szH, start = pi/2, end = 2*pi)) +
  ggforce::geom_arc(aes(x0 = xL2, y0 = yL2, r = 2*szH, start = pi/2, end = 2*pi)) +
  
  ### Places the spider pincers
  ggforce::geom_arc(aes(x0 = xP, y0 = yP, r = 0.6*szH, start = pi/2, end = 2*pi), size = 1.3) +
  
  ### Places titles
  geom_text(aes(x = x, y = y, label = label, size = I(size)), hjust = 0, vjust = 1,
            lineheight = 0.27, family = sans, data = titles) +
  
  ### Places the families names and amounts
  geom_text(aes(x = x, y = y, label = label, angle = angle),
            size = 25, family = sans, hjust = 1, vjust = 1, data = families) +
  
  coord_cartesian(xlim = c(-100,2300), ylim = c(-800,1900)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#D4964B", color = NA)
  )

## Saves the plot
ggsave("2021/week50/webs.png", plot = p, dpi = "retina",
       width = 25, height = 25)

