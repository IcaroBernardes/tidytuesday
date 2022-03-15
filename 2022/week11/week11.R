# 0. Library and fonts management
library(tidyverse)
library(ggplot2)
library(showtext)
library(glue)
library(ggtext)

# 1. Data download, load and handling
bioc <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/bioc.csv')
cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')

## Gets month and year info of each row
df <- bioc %>% 
  dplyr::arrange(package, date) %>% 
  dplyr::mutate(year = as.numeric(stringr::str_sub(date, 1L, 4L)),
                month = as.numeric(stringr::str_sub(date, 6L, 7L))) %>% 
  dplyr::select(-date)

## Gets the last value of the month for each package
df <- df %>% 
  dplyr::group_by(package, year, month) %>% 
  dplyr::filter(row_number() == n()) %>% 
  dplyr::ungroup()

## Eliminates empty lines and outside the time frame of interest
df <- df %>% 
  na.exclude() %>% 
  dplyr::filter(year >= 2001)


df <- df %>% 
  dplyr::

