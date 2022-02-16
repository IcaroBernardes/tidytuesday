# 0. Library and fonts management
library(tidyverse)
library(ggplot2)
library(showtext)
library(tidytuesdayR)
library(glue)
library(ggtext)
library(ggbump)
library(tm)
library(tidytext)

## Adding FontMeme.com fonts
### Creates the "fonts" directory
dir.create("2022/week04/fonts")

### Adds the "fonts" directory to the search path
sysfonts::font_paths("2022/week04/fonts")

### Downloads and extracts the fonts (only regular styles if multiples)
ft.url <- c("https://fontmeme.com/fonts/download/243426/grandstander.zip",
            "https://fontmeme.com/fonts/download/202877/life-savers.zip")
ft.path <- glue::glue("2022/week04/fonts/{basename(ft.url)}")
purrr::map2(ft.url, ft.path,
            function(.x,.y) {
              
              download.file(.x, .y)
              
              ft.files = unzip(.y, list = TRUE)
              if (dim(ft.files)[1] > 1) {
                ft.files = ft.files %>% 
                  dplyr::filter(str_detect(tolower(Name), "regular")) %>% 
                  dplyr::slice(1L)
              }
              ft.files = ft.files %>% dplyr::pull(Name)
              
              unzip(.y, files =  ft.files, exdir = "2022/week04/fonts")
              
            })

### Gets the name of the font and adds it to the system
purrr::map(ft.path,
           function(.x) {
             
             ft.files = unzip(.x, list = TRUE)
             if (dim(ft.files)[1] > 1) {
               ft.files = ft.files %>% 
                 dplyr::filter(str_detect(tolower(Name), "regular")) %>% 
                 dplyr::slice(1L)
             }
             ft.files = ft.files %>% dplyr::pull(Name)
             
             ft.family = tolower(stringr::str_sub(ft.files, 1L, -5L))
             
             sysfonts::font_add(ft.family, glue::glue("2022/week04/fonts/{ft.files}"))
             
           })
sans <- "grandstander-regular"
serif <- "lifesavers-regular"

## Allows the use of the downloadeded fonts
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_auto()

# 1. Data download, load and handling
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

## Gets the categories of each game
category_tokens <- details %>%
  dplyr::select(yearpublished, boardgamecategory, id, description) %>%
  dplyr::mutate(boardgamecategory = stringr::str_sub(boardgamecategory, 2,-2),
                boardgamecategory = stringr::str_remove_all(boardgamecategory, "'"),
                boardgamecategory = stringr::str_remove_all(boardgamecategory, '"')) %>%
  tidytext::unnest_tokens(category, boardgamecategory, token = "regex", pattern = ",") %>%
  dplyr::mutate(category = stringr::str_trim(category, "both")) %>%
  na.exclude()

## Gets the years with most games published
top_years <- details %>% 
  dplyr::count(yearpublished) %>% 
  dplyr::mutate(pct = 100*n/sum(n)) %>% 
  dplyr::filter(pct >= 1) %>% 
  dplyr::pull(yearpublished)

## Filters only the top5 categories of each year
categories <- category_tokens %>% 
  dplyr::filter(yearpublished %in% top_years) %>% 
  dplyr::count(yearpublished, category) %>% 
  dplyr::group_by(yearpublished) %>% 
  dplyr::arrange(desc(yearpublished), desc(n)) %>% 
  dplyr::slice(1:5) %>% 
  dplyr::mutate(order = seq(14, 2, -3)) %>% 
  dplyr::ungroup()

## Gets the set of most frequent categories for the given years and associates
## some fill-colors taken from I want hue (https://medialab.github.io/iwanthue/),
## some text-colors and icons from Google (https://fonts.google.com/icons)
top_categories <- categories %>% 
  dplyr::count(category, name = "hits") %>% 
  dplyr::arrange(desc(hits)) %>% 
  dplyr::mutate(color = c("#c19e7a", "#ee7cff", "#77e641", "#53b1ff", "#fccc00",
                          "#54d4ff", "#ff9e49", "#02d6bd", "#ff99a1", "#00c553",
                          "#fff2c1", "#7fff93", "#eeffa9", "#b2ffb4")) %>% 
  dplyr::mutate(names = stringr::str_remove_all(category, "[:punct:]")) %>% 
  dplyr::mutate(icons = glue::glue("2022/week04/icons/{names}.png")) %>% 
  dplyr::select(-hits, -names)

## Makes sure each category has an "order" value
## even in years they don't make to the top5
categories <- categories %>% 
  complete(yearpublished, category, fill = list(order = -10))

## Defines colors for the categories
categories <- categories %>% 
  dplyr::full_join(top_categories)

## Creates the labels for the plot
categories <- categories %>% 
  dplyr::mutate(label = glue::glue("<strong style = 'font-size:20pt;'>{n}:</strong> {category}"))

## Cleans and breaks down games descriptions by word
stop <- tidytext::get_stopwords()$word
describe <- category_tokens %>% 
  dplyr::filter(yearpublished %in% top_years) %>% 
  dplyr::filter(category %in% top_categories$category) %>%
  dplyr::mutate(description = tolower(description),
                description = tm::removePunctuation(description),
                description = tm::removeNumbers(description),
                description = tm::stripWhitespace(description)) %>% 
  tidytext::unnest_tokens(txt, description) %>% 
  dplyr::filter(!(txt %in% stop))

## Gets the most used words in descriptions in general
top_descriptors <- describe %>% 
  dplyr::distinct(id, txt, .keep_all = TRUE) %>% 
  dplyr::count(txt) %>% 
  dplyr::arrange(desc(n))

## Elimintates the top 15 words used in general and gets
## the most used words in descriptions in each category
stop <- top_descriptors %>% 
  dplyr::slice(1L:30L) %>% 
  dplyr::pull(txt)
descriptors <- describe %>% 
  dplyr::filter(!(txt %in% stop)) %>% 
  dplyr::count(category, txt) %>%
  dplyr::group_by(category) %>% 
  dplyr::arrange(category, desc(n)) %>% 
  dplyr::slice(1L:15L) %>% 
  dplyr::mutate(rank = 1:n()) %>% 
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from = category,
                     values_from = c(txt, n))


## Gets coordinates for the icons on the timeline
## (first appearance of the category on the top 5)
first <- categories %>% 
  na.exclude() %>% 
  dplyr::group_by(category) %>% 
  dplyr::slice(1L)

## Defines coordinates for the titles
title <- tibble(
  x = c(1996),
  y = c(35,31),
  size = c(70,25),
  family = c(sans, serif),
  label = c("Always trust the heart of the cards",
            "Almost every board game in the BGG database has a category.<br>
            They mostly represent elements of game mechanic 
            <span style='color:#39c0ed'>(dices,</span> 
            <span style='color:#c19e7a'>cards)</span> or themes 
            <span style='color:#77e641'>(fantasy,</span> 
            <span style='color:#ee7cff'>war)</span>.<br>
            The timeline bellow shows (top to bottom) the Top 5 categories in regard to the number of lauched games each year.<br>
            The tags bellow present the names of the categories and the total of launched games between 1996 and 2021."),
)

## Defines coordinates for the tag categories
tags <- categories %>%
  na.exclude() %>% 
  dplyr::group_by(category) %>% 
  dplyr::summarise(total = sum(n),
                   color = unique(color),
                   icons = unique(icons)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(label = glue::glue("{category}: *{total}*")) %>% 
  dplyr::arrange(desc(total)) %>% 
  dplyr::mutate(x = rep(seq(1996.2,2018.2,length.out = 7),2),
                y = c(rep(22,7),rep(19,7)))

## Defines coordinates for the values of the x-axis
axisX <- tibble(
  x = 1996:2021,
  y = 15.7
)

## Defines coordinates for the values of the x-axis
axisY <- tibble(
  x = 1995.4,
  y = seq(14, 2, -3),
  label = glue::glue("{1:5}<sup>{c('st','nd','rd','th','th')}</sup>")
)

# 2. Plot making
## Defines some layout constants
lnhgt <- 0.37
asp_ratio <- 0.3

## Creates the plot
p <- categories %>% 
  ggplot() +
  
  ### Places points and lines to mark the rank evolution of the categories
  ggbump::geom_bump(aes(x = yearpublished, y = order, color = I(color)), size = 2) +
  geom_point(aes(x = yearpublished, y = order, color = I(color)), size = 6) +
  
  ### Places the axes labels
  ggtext::geom_richtext(aes(x = x, y = y, label = x), size = 17, color = "white",
                        label.color = NA, fill = NA, family = serif, data = axisX) +
  ggtext::geom_richtext(aes(x = x, y = y, label = label), size = 17,
                        color = "white", hjust = 1, label.color = NA,
                        fill = NA, family = serif, data = axisY) +
  
  
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size), family = family),
                        hjust = 0, vjust = 1, color = "white", label.color = NA,
                        fill = NA, lineheight = lnhgt, data = title) +
  
  ggtext::geom_textbox(aes(x = x, y = y, label = label, fill = I(color)),
                       color = "black", box.color = NA, family = serif,
                       halign = 0, hjust = 0, size = 13,
                       box.padding = unit(c(5.5, 5.5, 5.5, 20), "pt"),
                       width = unit(2.4, "inch"), data = tags) +
  geom_point(aes(x = x, y = y, color = I(color)), size = 16, data = tags) +
  ggimage::geom_image(aes(x = x, y = y, image = icons), size = 0.013,
                      by = "width", asp = 8*asp_ratio, data = tags) +
  
  geom_point(aes(x = yearpublished, y = order, color = I(color)),
             size = 11, data = first) +
  ggimage::geom_image(aes(x = yearpublished, y = order, image = icons),
                      size = 0.008, by = "width", asp = 8*asp_ratio,
                      data = first) +
  
  coord_equal(xlim = c(1994.5, 2022.5), ylim = c(0, 37), ratio = asp_ratio, expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black", color = NA)
  )

## Saves the plot
ggsave("2022/week04/categ_rank.png", plot = p, dpi = "retina",
       width = 30, height = 30*asp_ratio, limitsize  = FALSE)

# # 3. Code for (maybe) another plot
# ## Gets the categories of each game
# category <- details %>%
#   dplyr::left_join(ratings, by = "id") %>%
#   dplyr::select(id, boardgamecategory) %>%
#   dplyr::mutate(boardgamecategory = stringr::str_sub(boardgamecategory, 2,-2),
#                 boardgamecategory = stringr::str_remove_all(boardgamecategory, "'"),
#                 boardgamecategory = stringr::str_remove_all(boardgamecategory, '"')) %>%
#   tidytext::unnest_tokens(category, boardgamecategory, token = "regex", pattern = ",") %>%
#   dplyr::mutate(category = stringr::str_trim(category, "both")) %>%
#   na.exclude()
# 
# ## Gets the 23 most frequent categories
# top_categories <- category %>%
#   dplyr::count(category) %>%
#   dplyr::arrange(desc(n)) %>%
#   dplyr::mutate(pct = round(100*cumsum(n)/sum(n))) %>%
#   dplyr::slice(1:23) %P>%
#   dplyr::pull(category) %>%
#   paste0(collapse = "|")
# 
# ## Creates dummies of the most frequent categories
# dummies <- category %>%
#   dplyr::filter(stringr::str_detect(category, top_categories)) %>%
#   fastDummies::dummy_cols(select_columns = "category",
#                           remove_selected_columns = TRUE) %>%
#   dplyr::group_by(id) %>%
#   dplyr::summarise(across(.fns = sum)) %>%
#   dplyr::ungroup()
# 
# ## Keeps only games that have at least one of the top categories
# ## and selects the variables of interest
# df <- details %>%
#   dplyr::right_join(dummies) %>%
#   dplyr::left_join(ratings, by = "id") %>%
#   dplyr::select(id, description, playingtime, minage, owned, average, starts_with("category"))



# ## Creates a temporary image to mask the thumbnails and saves it
# imgdim <- 64
# png(tf <- tempfile(fileext = ".png"), 1000, 1000)
# par(mar = rep(0,4), yaxs="i", xaxs="i")
# plot(0, type = "n", ylim = c(0,1), xlim=c(0,1), axes=F, xlab=NA, ylab=NA)
# plotrix::draw.circle(.5,0.5,.5, col="black")
# dev.off()
# mask <- magick::image_read(tf)
# mask <- magick::image_scale(mask, imgdim)
# 
# ## Downloads the thumbnails, resizes them, applies the mask and saves the
# ## resulting images. Uncomment it if you need to modify the thumbnails 
# for (i in 1:dim(categories)[1]) {
#   
#   ### Gets the image with retina resolution
#   link <- categories$thumbnail[i]
#   img <- magick::image_read(link, density = 320)
#   
#   ### Gets info about dimensions of the image and scales and crops
#   ### it so the smallest size is the one defined at imgdim
#   info <- magick::image_info(img)
#   info <- min(info$width, info$height)
#   info <- imgdim/info
#   img <- magick::image_resize(img, geometry = glue::glue("{info*imgdim}x{info*imgdim}+0+0"))
#   img <- magick::image_crop(img, repage = TRUE,
#                             geometry = glue::glue("{imgdim}x{imgdim}+0+0"))
#   
#   ### Applies the mask and saves the image
#   img <- magick::image_composite(mask, img, operator = "minus")
#   magick::image_write(img, format = "png",
#                       path = glue::glue("2022/week04/thumbs/{categories$id[i]}.png"))
#   
# }