# 0. Library management
library(tidyverse)
library(ggplot2)
library(showtext)
library(readr)
library(patchwork)
library(scales)
library(santoku)

## Adding Google Fonts
font_add_google(name = "Poppins", family = "poppins") ### Sans Serif
sans <- "poppins"
font_add_google(name = "Antic Slab", family = "antic") ### Serif
serif <- "antic"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_auto()

# 1. Data download, load and handling
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')

## Numbers special episodes and unite season and episode info in only one variable
episodes <- episodes %>% 
  dplyr::group_by(season_number, type) %>% 
  dplyr::mutate(episode_number = 1:n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(episode_number = ifelse(type == "episode",
                                        paste0("E",episode_number),
                                        paste0("X",episode_number)
                                        )) %>% 
  dplyr::mutate(ep = paste0("S",season_number,episode_number)) %>% 
  dplyr::filter(season_number != 13) %>% 
  dplyr::select(ep, season_number, type, episode_title, uk_viewers, rating) %>% 
  na.exclude()

## Brings the data to the same scale and flips the view variable
episodes <- episodes %>% 
  dplyr::mutate(ep = factor(ep, levels = unique(episodes$ep))) %>% 
  dplyr::mutate(ep = forcats::fct_rev(ep)) %>% 
  dplyr::mutate(rate = scales::rescale(rating, to = c(45,95))) %>% 
  dplyr::mutate(view = scales::rescale(uk_viewers, to = c(45,95))) %>% 
  dplyr::mutate(view = -view)

## Calculates how many episodes fall within equally
## size intervals of the data (views and rating)
intervals <- episodes %>% 
  dplyr::mutate(interval = santoku::chop_evenly(uk_viewers, 3)) %>% 
  dplyr::mutate(interval = forcats::fct_relabel(interval, ~stringr::str_replace(.x,", ",",\n"))) %>% 
  dplyr::count(interval) %>% 
  dplyr::mutate(x = -c(1.1,1.6,2.1), y = rep(31,3)) %>% 
  dplyr::mutate(xl = -c(1.1,1.6,2.1), yl = rep(32.1,3))
intervals <- episodes %>% 
  dplyr::mutate(interval = santoku::chop_evenly(rating, 3)) %>% 
  dplyr::mutate(interval = forcats::fct_relabel(interval, ~stringr::str_replace(.x,", ",",\n"))) %>% 
  dplyr::count(interval) %>% 
  dplyr::mutate(x = c(1.1,1.6,2.1), y = rep(31,3)) %>% 
  dplyr::mutate(xl = c(1.1,1.6,2.1), yl = rep(32.1,3)) %>% 
  rbind(intervals)

# 2. Observing the evolution of ratings and viewers over the seasons
## Defines some layout constants
{
  lnhgt <- 0.29 ### Height of lines of text
  insfnt <- 12 ### Font size of insights
  nudgey <- 0.25 ### Nudge of text in y-axis to emulate line break
  arhead <- 0.01 ### Arrow head length
}

## Creates data for the axis
{
  xinf <- -4
  xsup <- 4
  yinf <- 0
  ysup <- 50
}

## Creates coordinates for Tardis windows
xminW <- c(rep(c(1.3,1.8,2.3),2),-rep(c(1.3,1.8,2.3),2))
xmaxW <- c(rep(c(0.9,1.4,1.9),2),-rep(c(0.9,1.4,1.9),2))
yminW <- rep(c(rep(30.5,3),rep(31.6,3)),2)
ymaxW <- rep(c(rep(31.5,3),rep(32.6,3)),2)

## Creates the lollipop chart
lollipop <- episodes %>% 
  ggplot(aes(y = ep)) +
  
  geom_rect(aes(xmin = -100, xmax = -40, ymin = -Inf, ymax = Inf),
            fill = "#003561", color = "black") +
  geom_rect(aes(xmin = 100, xmax = 40, ymin = -Inf, ymax = Inf),
            fill = "#003561", color = "black") +
  
  geom_point(aes(x = view, color = type)) +
  geom_point(aes(x = rate, color = type)) +
  geom_segment(aes(x = view, xend = -40.5, yend = ep, color = type)) +
  geom_segment(aes(x = rate, xend = 40.5, yend = ep, color = type)) +
  
  geom_text(aes(x = 0, label = paste(ep, episode_title, sep=": "), color = type),
            size = 7, family = serif) +
  
  facet_grid(season_number ~ ., scales = "free_y") +
  scale_y_discrete(expand = expansion(mult = 0.05)) +
  scale_x_continuous(expand = expansion(mult = 0.05)) +
  scale_color_manual(values = c("white","#759dc9"), breaks = c("episode","special")) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    panel.spacing.y = unit(2, "lines")
  )

## Creates the final plot
p <- ggplot() +
  
  ### Creates Tardis pieces
  #### Top
  geom_ribbon(aes(y = c(37,39,40.2,40.5),
                  xmin = c(-2.6,-0.4,-0.4,0),
                  xmax = c(2.6,0.4,0.4,0)),
              fill = "#03569b") +
  geom_rect(aes(xmin = -3, xmax = 3, ymin = 36, ymax = 37), fill = "#03569b") + #### Top
  geom_rect(aes(xmin = -0.2, xmax = 0.2, ymin = 39, ymax = 40), fill = "#f5d718") + #### Lantern
  geom_rect(aes(xmin = 0.4, xmax = 0.2, ymin = 39, ymax = 40), fill = "#ed930c") + #### Lantern
  geom_rect(aes(xmin = -0.4, xmax = -0.2, ymin = 39, ymax = 40), fill = "#ed930c") + #### Lantern
  geom_rect(aes(xmin = -3.5, xmax = 3.5, ymin = 34, ymax = 36), fill = "#0068bf") + #### Lettering
  geom_rect(aes(xmin = -2.7, xmax = 2.7, ymin = 34.3, ymax = 35.7), fill = "black") + #### Lettering
  geom_rect(aes(xmin = -3, xmax = 3, ymin = 0.7, ymax = 34), fill = "#03569b") + #### Body
  geom_rect(aes(xmin = -4, xmax = 4, ymin = 0, ymax = 0.7), fill = "#0068bf") + #### Bottom
  annotate("rect", xmin = xminW, xmax = xmaxW, ymin = yminW, ymax = ymaxW, fill = "white") + #### Windows
  
  ### Creates the titles
  #### Main
  annotate("text", x = 0, y = ysup-1, label = "Is the Doctor\nrunning out time?",
           size = 85, family = sans, vjust = 1, lineheight = lnhgt) +
  #### Subtitle
  annotate("text", x = 0, y = ysup-5.5, label = "The series revived era saw its prime under\nthe stars of David Tennant (Seasons 2-4)\nand Matt Smith (Season 5-7). Since then,\nrating and viewership dropped in the UK",
           size = 35, family = sans, vjust = 1, lineheight = lnhgt) +
  #### Credits
  annotate("text", x = -2.4, y = 35, label = "DATA:",
           color = "white", size = 38, family = sans, hjust = 0) +
  annotate("text", x = -0.8, y = 35, label = "datardis\npackage", lineheight = lnhgt,
           color = "white", size = 15, family = sans) +
  annotate("text", x = -0.3, y = 35, label = "GRAPHIC:",
           color = "white", size = 38, family = sans, hjust = 0) +
  annotate("text", x = 2, y = 35, label = "Ícaro\nBernardes", lineheight = lnhgt,
           color = "white", size = 15, family = sans) +
  
  ### Creates the axis titles and labels
  annotate("text", x = -2.3, y = 33, label = "UK viewers (millions)",
           hjust = 0, size = 12, color = "white", family = sans) +
  annotate("text", x = 2.3, y = 33, label = "Audience Appreciation\nIndex (UK TV rating)",
           hjust = 1, size = 12, color = "white", family = sans, lineheight = lnhgt) +
  annotate("text", x = 0, y = 33, label = "Episodes",
           size = 12, color = "white", family = sans) +
  geom_text(aes(x = xl, y = yl, label = interval), size = 10, family = sans,
            lineheight = lnhgt, data = intervals) +
  geom_text(aes(x = x, y = y, label = n), size = 18, family = sans,
            lineheight = lnhgt, data = intervals) +
  
  theme_void() +
  
  coord_cartesian(xlim = c(xinf,xsup), ylim = c(yinf,ysup), expand = FALSE) +
  patchwork::inset_element(lollipop,
                           left = 1.5/(xsup-xinf), right = (xsup-xinf-1.5)/(xsup-xinf),
                           bottom = 1/(ysup-yinf), top = 30/(ysup-yinf))

ggsave("2020/week48/tardis.png", plot = p, width = 25, height = 80, units = "cm")

