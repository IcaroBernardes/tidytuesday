# 0. Library and fonts management
library(tidyverse)
library(ggplot2)
library(showtext)
library(glue)
library(ggtext)
library(scales)

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
rawdata <- read.csv2("2022/week09/data.csv")

## Keeps only data on percentage of managers per race
df <- rawdata %>% 
  dplyr::select(year, race, pct_bosses_total) %>% 
  dplyr::rename("pct" = "pct_bosses_total")

## Gets only percentage for blacks
blk <- df %>% 
  dplyr::filter(race == "black") %>% 
  dplyr::arrange(year) %>% 
  dplyr::mutate(pct = ifelse(row_number(pct) == 1L | row_number(pct) == n(),
                             glue::glue("{pct}%"),
                             pct)) %>% 
  dplyr::pull(pct)

## Gets only percentage and its coefficient of variation for whites
cv_wht <- rawdata %>% 
  dplyr::filter(race == "white") %>% 
  dplyr::select(year, pct_bosses_total, cv_pct_bosses)

## Calculates an area for the left of the plot using random walk (sort of)
set.seed(42)
lft_area <- tibble(
  y = seq(2012, 2018, 0.0001)
) %>% 
  dplyr::mutate(year = floor(y)) %>% 
  dplyr::left_join(cv_wht) %>% 
  dplyr::rename("sd" = "cv_pct_bosses") %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(r = rnorm(n = 1, mean = 0, sd = sd)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(z = cumsum(r),
                z = scales::rescale(z, to = c(-3,4))) %>% 
  dplyr::mutate(x = 70 + z,
                x = x +25,
                x = ifelse(x > 100, 100, x))

## Gets some numbers of interest
ocup_wht <- rawdata %>% 
  dplyr::filter(race == "white") %>% 
  dplyr::summarise(pct = mean(pct_workers_total)) %>% 
  dplyr::pull(pct) %>% 
  round(digits = 1)
ocup_blk <- rawdata %>% 
  dplyr::filter(race == "black") %>% 
  dplyr::summarise(pct = mean(pct_workers_total)) %>% 
  dplyr::pull(pct) %>% 
  round(digits = 1)
boss_wht <- rawdata %>% 
  dplyr::filter(race == "white") %>% 
  dplyr::summarise(pct = mean(pct_bosses_total)) %>% 
  dplyr::pull(pct) %>% 
  round(digits = 1)
boss_blk <- rawdata %>% 
  dplyr::filter(race == "black") %>% 
  dplyr::summarise(pct = mean(pct_bosses_total)) %>% 
  dplyr::pull(pct) %>% 
  round(digits = 1)

## Defines the title, secondary axis title and message of the plot
title <- c(
  "PARTICIPATION IN MANAGERIAL POSITIONS BY RACE IN BRAZIL.<br>
  <span style='font-size:60px;'>
  INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ÍCARO BERNARDES (@IcaroBSC)
  </span>"
)
sectitle <- "PERCENT<br>OF<br>BLACK MANAGERS"
size_msg <- "style='font-size:80px;'"
message <- glue::glue("IN THE SERIES, WHITE PEOPLE OCCUPY IN AVERAGE <span {size_msg}>{ocup_wht}%</span> OF JOB POSITIONS.
                      BLACK PEOPLE OCCUPY <span {size_msg}>{ocup_blk}%</span>.
                      <br>HOWEVER WHITES HAVE IN AVERAGE <span {size_msg}>{boss_wht}%</span> OF THE MANAGERIAL POSITIONS.
                      BLACKS HAVE ONLY <span {size_msg}>{boss_blk}%</span>.")

## Defines some layout constants
lnhgt <- 0.8
bgcolor <- "#d2b48c"

# 2. Generates the plot
## Creates the main plot
p <- df %>% 
  ggplot() +
  geom_area(aes(x = pct, y = year, fill = race), orientation = "y", size = 4,
            color = bgcolor, position = position_stack(reverse = TRUE)) +
  geom_ribbon(aes(xmin = x, xmax = 100, y = y), fill = bgcolor, data = lft_area) +
  annotate("text", x = c(63,13), y = 2012.5, label = c("WHITES","BLACKS"),
           color = "white", size = 15, family = sans) +
  labs(title = title, subtitle = sectitle, caption = message, x = NULL, y = NULL) +
  scale_x_reverse(expand = expansion(0,10), breaks = seq(25, 75, 25),
                  label = scales::label_percent(scale = 1), position = "top") +
  scale_y_reverse(expand = expansion(0,0.01), breaks = 2012:2018,
                  sec.axis = dup_axis(name = NULL, breaks = 2012:2018, labels = blk)) +
  scale_fill_manual(values = c("black","#dc143c"),
                    breaks = c("white","black"),
                    guide = "none") +
  theme(
    text = element_text(family = sans),
    plot.margin = margin(t = 60, r = 400, b = 50, l = 400, unit = "pt"),
    plot.background = element_rect(fill = bgcolor, color = NA),
    plot.title = ggtext::element_textbox_simple(
      size = 80, halign = 0.5, valign = 0.5, width = 3, lineheight = lnhgt,
      margin = ggplot2::margin(t = 0, r = 0, b = 20, l = 0, unit = "pt")
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      size = 23, halign = 0.5, valign = 0.5, width = 1.5, lineheight = lnhgt,
      padding = ggplot2::margin(t = 0, r = 0, b = -10, l = 720, unit = "pt")
    ),    
    plot.caption = ggtext::element_textbox_simple(
      size = 45, fill = "#654321", color = bgcolor,
      halign = 0.5, valign = 0.5, width = 3,
      padding = ggplot2::margin(t = 40, r = 0, b = 40, l = 0, unit = "pt"),
      margin = ggplot2::margin(t = 80, r = 0, b = 20, l = 0, unit = "pt")
    ),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = bgcolor, size = 2),
    panel.ontop = TRUE,
    axis.text.x = element_text(size = 35),
    axis.text.y = element_text(size = 50),
    axis.text.y.right = element_text(hjust = 0.5),
    axis.ticks.length.x = unit(15, "pt"),
    axis.ticks.x = element_line(size = 1),
    axis.ticks.y = element_blank()
  )

## Saves the plot
ggsave("2022/week09/managers.png", plot = p, dpi = "retina",
       width = 22, height = 28)

