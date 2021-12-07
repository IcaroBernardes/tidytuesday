# 0. Library management
library(tidyverse)
library(ggplot2)
library(showtext)
library(readr)
library(fastDummies)
library(ComplexHeatmap)

## Adding Google Fonts
font_add_google(name = "Josefin Sans", family = "josefin") ### Sans Serif
sans <- "josefin"
font_add_google(name = "Antic Slab", family = "antic") ### Serif
serif <- "antic"
font_add_google(name = "Ubuntu Mono", family = "ubuntu") ### Monospaced
mono <- "ubuntu"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_auto()

# 1. Data download, load and handling
## Reads raw data
raw_df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/raw_anime.csv")

## Cleans up some of the data
clean_df <- raw_df %>% 
  # Producers
  mutate(producers = str_remove(producers, "\\["),
         producers = str_remove(producers, "\\]")) %>% 
  separate_rows(producers, sep = ",") %>% 
  mutate(producers = str_remove(producers, "\\'"),
         producers = str_remove(producers, "\\'"),
         producers = str_trim(producers)) %>% 
  # Genre
  mutate(genre = str_remove(genre, "\\["),
         genre = str_remove(genre, "\\]")) %>% 
  separate_rows(genre, sep = ",") %>% 
  mutate(genre = str_remove(genre, "\\'"),
         genre = str_remove(genre, "\\'"),
         genre = str_trim(genre)) %>% 
  # Studio
  mutate(studio = str_remove(studio, "\\["),
         studio = str_remove(studio, "\\]")) %>% 
  separate_rows(studio, sep = ",") %>% 
  mutate(studio = str_remove(studio, "\\'"),
         studio = str_remove(studio, "\\'"),
         studio = str_trim(studio)) %>% 
  # Aired
  mutate(aired = str_remove(aired, "\\{"),
         aired = str_remove(aired, "\\}"),
         aired = str_remove(aired, "'from': "),
         aired = str_remove(aired, "'to': "),
         aired = word(aired, start = 1, 2, sep = ",")) %>% 
  separate(aired, into = c("start_date", "end_date"), sep = ",") %>% 
  mutate(start_date = str_remove_all(start_date, "'"),
         start_date = str_sub(start_date, 1, 10),
         end_date = str_remove_all(start_date, "'"),
         end_date = str_sub(end_date, 1, 10)) %>%
  mutate(start_date = lubridate::ymd(start_date),
         end_date = lubridate::ymd(end_date)) %>% 
  # Drop unranked or unpopular series
  filter(rank != 0,
         popularity != 0)

## Creates dummy variables for the genres
df <- clean_df %>% 
  dplyr::filter(genre != "") %>% 
  dplyr::distinct(animeID, genre) %>%
  fastDummies::dummy_cols("genre", remove_selected_columns = TRUE) %>% 
  dplyr::group_by(animeID) %>% 
  dplyr::summarise(across(.fns = sum)) %>% 
  dplyr::ungroup()

## Associates Japanese characters to the genres
top_join <- tibble(
  genre = paste0("genre_",
                 c("Comedy","Action","Fantasy","Adventure","Sci-Fi",
                   "Drama","Kids","Shounen","Slice of Life","Romance")),
  color = c("#000000","#006400","#ff0000","#00ced1","#6a5acd","#00ff00","#e9967a","#0000ff","#ffff54","#ff1493"),
  katakana = c("喜劇","行動","空想","冒険","空想科学小説",
               "演劇","子供","少年","人生のひとこま","ロマン")
)

## Gets the top 10 most frequent genres
top <- df %>% 
  dplyr::select(-animeID) %>% 
  dplyr::summarise(across(.fns = sum)) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "genre", values_to = "total") %>% 
  dplyr::arrange(desc(total)) %>% 
  dplyr::slice(1:10) %>% 
  dplyr::mutate(name = paste0("var",1:10)) %>% 
  dplyr::left_join(top_join)

## Keeps only the animes and genres of the top 10
df <- df %>% 
  dplyr::select(animeID, top$genre) %>% 
  tibble::column_to_rownames("animeID") %>% 
  dplyr::filter(if_any(.fns =  ~(. == 1)))

## Subsets the data by genre and gets all sets of genres for each subset
mat <- colnames(df) %>% 
  purrr::map(list) %>% 
  purrr::map(~rlang::sym(.x[[1]])) %>% 
  purrr::map(~df %>% dplyr::filter(!!.x == 1)) %>% 
  purrr::map(ComplexHeatmap::make_comb_mat)

## Gets the top 5 sets of genres associated to each genre and their size 
size <- mat %>% 
  purrr::map(ComplexHeatmap::comb_size) %>% 
  purrr::map(sort, decreasing = TRUE) %>% 
  purrr::map(~.x[1:5])

sets <- size %>% 
  purrr::map(names) %>% 
  purrr::map(~tibble(vars = .x)) %>% 
  purrr::imap(
    ~.x %>%
      tidyr::separate(vars, into = paste0("var",1:10), sep = 1:9) %>% 
      dplyr::mutate(across(.fns = ~as.logical(as.numeric(.)))) %>% 
      dplyr::mutate(rank = seq(0, 0.4, by = 0.1)) %>% 
      tidyr::pivot_longer(cols = starts_with("var")) %>%
      dplyr::mutate(name = factor(name, levels = paste0("var",1:10))) %>% 
      dplyr::mutate(name = forcats::fct_relevel(name, paste0("var",.y))) %>% 
      dplyr::arrange(rank, name) %>% 
      dplyr::left_join(top, by = "name") %>% 
      dplyr::filter(value) %>% 
      dplyr::mutate(genre = stringr::str_remove(genre, "genre_")) %>% 
      dplyr::mutate(y = .y + rank) %>% 
      dplyr::group_by(rank) %>% 
      dplyr::mutate(x = 2 + (1:n())/10) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(color, x, y)
  ) %>% 
  purrr::map_dfr(~.x)

size <- size %>% 
  purrr::map(~tibble(n = .x)) %>% 
  purrr::imap(
    ~.x %>%
      dplyr::mutate(rank = seq(0, 0.4, by = 0.1)) %>%
      dplyr::mutate(y = .y + rank) %>% 
      dplyr::mutate(x = 2.4)
  ) %>% 
  purrr::map_dfr(~.x)

## Gives coordinates to the genre names and amount
names <- top %>% 
  dplyr::mutate(genre = stringr::str_remove(genre, "genre_")) %>% 
  dplyr::mutate(y1 = (1:10)+0.31,
                x1 = 1,
                y2 = 1:10,
                x2 = 1,
                y3 = 1:10,
                x3 = 1+0.06*stringr::str_length(genre))

p <- sets %>% 
  ggplot(aes(x = x, y = -y)) +
  
  geom_point(aes(color = color)) +
  geom_text(aes(label = n), family = mono, size = 5, data = size) +
  
  geom_text(aes(x = x1, y = -y1, label = genre, color = color),
            hjust = 0, size = 30, family = mono, data = names) +
  geom_text(aes(x = x2, y = -y2, label = katakana, color = color),
            hjust = 0, size = 10, family = "wqy-microhei", data = names) +
  geom_text(aes(x = x3, y = -y3, label = total, color = color),
            hjust = 1, size = 10, family = mono, data = names) +
  
  scale_color_identity() +
  
  # theme_void() +
  theme(
    legend.position = "none"
  )

## Saves the plot
ggsave("oldsets/19WK17/anime.png", plot = p, dpi = "retina",
       width = 5, height = 8) ## Makes the width equivalent to the aspect ratio

