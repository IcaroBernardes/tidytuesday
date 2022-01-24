# 0. Library management
library(tidyverse)
library(ggplot2)
library(showtext)
library(tidytuesdayR)
library(glue)

#http://flavorsofcacao.com/labeling_chocolate.html
#http://flavorsofcacao.com/review_guide.html
#https://www.thechocolatejournalist.com/blog/soy-lecithin-chocolate

## Adding Google Fonts
font_add_google(name = "Cormorant Garamond", family = "cormorant") ### Sans Serif
serif <- "cormorant"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_auto()

# 1. Data download, load and handling
raw <- tidytuesdayR::tt_load('2022-01-18')$chocolate


ggplot() +
  geom
  