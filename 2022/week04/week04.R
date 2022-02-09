# 0. Library and fonts management
library(tidyverse)
library(ggplot2)
library(ViewPipeSteps) ## Allows the use of a printer pipe (%P>%) to show intermediate results
library(showtext)
library(tidytuesdayR)
library(glue)
library(fastDummies)
library(ggtext)
library(patchwork)
library(ggimage)
library(magick)
library(plotrix)

## Adding Google Fonts
font_add_google(name = "Lora", family = "lora") ### Sans Serif
serif <- "lora"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_auto()

# 1. Data download, load and handling
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

## Gets the categories of each game
categories <- details %>%
  dplyr::select(yearpublished, boardgamecategory) %>%
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
categories <- categories %>% 
  dplyr::filter(yearpublished %in% top_years) %>% 
  dplyr::count(yearpublished, category) %>% 
  dplyr::group_by(yearpublished) %>% 
  dplyr::arrange(desc(yearpublished), desc(n)) %>% 
  dplyr::slice(1:5) %>% 
  dplyr::mutate(order = seq(18, 2, -4)) %>% 
  dplyr::ungroup()

## Gets the set of most frequent categories for the given years and associates
## some fill-colors taken from I want hue (https://medialab.github.io/iwanthue/),
## some text-colors and icons from Google (https://fonts.google.com/icons)
top_categories <- categories %>% 
  dplyr::count(category, name = "hits") %>% 
  dplyr::arrange(desc(hits)) %>% 
  dplyr::mutate(fill = c("#c19e7a", "#ee7cff", "#77e641", "#53b1ff", "#fccc00",
                         "#45b0d4", "#ff9e49", "#02d6bd", "#ff99a1", "#00c553",
                         "#fff2c1", "#7fff93", "#eeffa9", "#b2ffb4")) %>% 
  dplyr::mutate(names = stringr::str_remove_all(category, "[:punct:]")) %>% 
  dplyr::mutate(icons = glue::glue("2022/week04/icons/{names}.png")) %>% 
  dplyr::select(-hits, -names)

## Defines colors for the categories
categories <- categories %>% 
  dplyr::full_join(top_categories)

## Creates the labels for the plot
categories <- categories %>% 
  dplyr::mutate(label = glue::glue("<strong style = 'font-size:20pt;'>{n}:</strong> {category}"))

## Obtains the link, name and average for the best rated games per year and category
categories <- details %>% 
  dplyr::left_join(ratings, by = "id") %>% 
  dplyr::filter(yearpublished %in% top_years) %>% 
  dplyr::select(yearpublished, boardgamecategory, rank, primary, average, thumbnail, id) %>% 
  dplyr::mutate(boardgamecategory = stringr::str_sub(boardgamecategory, 2,-2),
                boardgamecategory = stringr::str_remove_all(boardgamecategory, "'"),
                boardgamecategory = stringr::str_remove_all(boardgamecategory, '"')) %>%
  tidytext::unnest_tokens(categname, boardgamecategory, token = "regex", pattern = ",") %>%
  dplyr::mutate(categname = stringr::str_trim(categname, "both")) %>%
  dplyr::filter(categname %in% top_categories$category) %>% 
  dplyr::group_by(yearpublished) %>% 
  tidyr::nest() %>% 
  dplyr::full_join(categories) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    data = purrr::map2(
      data, category,
      ~.x %>% 
        dplyr::filter(categname == .y) %>% 
        dplyr::slice_min(rank)
    )) %>% 
  tidyr::unnest(cols = data)

## Creates a temporary image to mask the thumbnails and saves it
imgdim <- 64
png(tf <- tempfile(fileext = ".png"), 1000, 1000)
par(mar = rep(0,4), yaxs="i", xaxs="i")
plot(0, type = "n", ylim = c(0,1), xlim=c(0,1), axes=F, xlab=NA, ylab=NA)
plotrix::draw.circle(.5,0.5,.5, col="black")
dev.off()
mask <- magick::image_read(tf)
mask <- magick::image_scale(mask, imgdim)

## Downloads the thumbnails, resizes them, applies the mask and saves the
## resulting images. Uncomment it if you need to modify the thumbnails 
for (i in 1:dim(categories)[1]) {
  
  ### Gets the image with retina resolution
  link <- categories$thumbnail[i]
  img <- magick::image_read(link, density = 320)
  
  ### Gets info about dimensions of the image and scales and crops
  ### it so the smallest size is the one defined at imgdim
  info <- magick::image_info(img)
  info <- min(info$width, info$height)
  info <- imgdim/info
  img <- magick::image_resize(img, geometry = glue::glue("{info*imgdim}x{info*imgdim}+0+0"))
  img <- magick::image_crop(img, repage = TRUE,
                            geometry = glue::glue("{imgdim}x{imgdim}+0+0"))
  
  ### Applies the mask and saves the image
  img <- magick::image_composite(mask, img, operator = "minus")
  magick::image_write(img, format = "png",
                      path = glue::glue("2022/week04/thumbs/{categories$id[i]}.png"))
  
}

## Associates the games to the masked thumbnails
categories <- categories %>% 
  dplyr::mutate(thumbnail = glue::glue("2022/week04/thumbs/{id}.png"))

# 2. Plot making
## Defines some layout constants
asp_ratio <- 0.2

p <- categories %>% 
  ggplot() +
  geom_tile(aes(x = yearpublished, y = order, fill = I(fill)),
            width = 0.2, height = 1) +
  ggimage::geom_image(aes(x = yearpublished, y = order, image = icons),
                      size = 0.003, by = "width", asp = 1/asp_ratio) +
  ggimage::geom_image(aes(x = yearpublished+0.4, y = order, image = thumbnail),
                      size = 0.01, by = "width", asp = 1/asp_ratio) +
  scale_x_continuous(limits = c(1995.5, 2022)) +
  scale_y_continuous(limits = c(0, 27)) +
  coord_equal(ratio = asp_ratio, expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black", color = NA)
  )

## Saves the plot
ggsave("2022/week04/categ_rank.png", plot = p, dpi = "retina",
       width = 50, height = 50*asp_ratio, limitsize  = FALSE)

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