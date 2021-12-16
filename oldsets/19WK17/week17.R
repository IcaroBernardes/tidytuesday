# 0. Library management
library(tidyverse)
library(ggplot2)
library(showtext)
library(readr)
library(fastDummies)
library(ComplexHeatmap)
library(tm)
library(stringi)
library(tidytext)
library(ggimage)

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
  ### Producers
  mutate(producers = str_remove(producers, "\\["),
         producers = str_remove(producers, "\\]")) %>% 
  separate_rows(producers, sep = ",") %>% 
  mutate(producers = str_remove(producers, "\\'"),
         producers = str_remove(producers, "\\'"),
         producers = str_trim(producers)) %>% 
  ### Genre
  mutate(genre = str_remove(genre, "\\["),
         genre = str_remove(genre, "\\]")) %>% 
  separate_rows(genre, sep = ",") %>% 
  mutate(genre = str_remove(genre, "\\'"),
         genre = str_remove(genre, "\\'"),
         genre = str_trim(genre)) %>% 
  ### Studio
  mutate(studio = str_remove(studio, "\\["),
         studio = str_remove(studio, "\\]")) %>% 
  separate_rows(studio, sep = ",") %>% 
  mutate(studio = str_remove(studio, "\\'"),
         studio = str_remove(studio, "\\'"),
         studio = str_trim(studio)) %>% 
  ### Aired
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
  ### Drop unranked or unpopular series
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

## Gives coordinates to the genre names and frequency
names <- top %>% 
  dplyr::mutate(genre = stringr::str_remove(genre, "genre_")) %>% 
  dplyr::mutate(y1 = (1:10)+0.31,
                x1 = 1,
                y2 = 1:10,
                x2 = 1,
                y3 = 1:10,
                x3 = 1+0.058*stringr::str_length(genre))

## Gets the most used words in the synopses of animes for each genre
### Gets some stopwords
stop <- tm::stopwords(kind = "SMART")

### Finds words that are used with similar frequency between genres
bigger_df <- df %>% 
  tibble::rownames_to_column(var = "animeID") %>% 
  dplyr::mutate(animeID = as.numeric(animeID))

generic <- clean_df %>% 
  dplyr::distinct(animeID, synopsis) %>% 
  dplyr::mutate(synopsis = tolower(synopsis)) %>% 
  dplyr::mutate(synopsis = tm::removePunctuation(synopsis)) %>%
  dplyr::mutate(synopsis = stringi::stri_trans_general(synopsis, id = "Latin-ASCII")) %>% 
  dplyr::mutate(synopsis = tm::removeNumbers(synopsis)) %>% 
  dplyr::mutate(synopsis = tm::removeWords(synopsis, stop)) %>% 
  dplyr::mutate(synopsis = tm::stripWhitespace(synopsis)) %>% 
  tidytext::unnest_tokens(txt, synopsis) %>% 
  dplyr::filter(!is.na(txt)) %>% 
  dplyr::add_count(txt) %>% 
  dplyr::left_join(bigger_df) %>% 
  na.exclude() %>% 
  dplyr::group_by(txt) %>% 
  dplyr::summarise(n = unique(n),
                   across(.cols = starts_with("genre"), .fns = ~sum(.)/n())) %>% 
  tidyr::pivot_longer(cols = starts_with("genre")) %>% 
  dplyr::group_by(txt) %>% 
  dplyr::summarise(n = unique(n),
                   n = unique(n),
                   rng = max(value)-min(value)) %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(rng <= 0.3) %>% 
  dplyr::slice(1:20) %>% 
  dplyr::pull(txt)

### Lists words to be excluded from the counting
no_meaning <- c("source","ann","mal","written","world","characters",
                "rewrite","series","anime","manga","episode","dvd","tv",
                "story","named","day","back","episodes","called","movie",
                "uta","minna","short","anidb","special","set","animation",
                "film") ### Lists words without relevance or clear meaning
stop <- c(stop,no_meaning,generic)

### Gets the top words for each genre
synop <- clean_df %>% 
  dplyr::distinct(animeID, synopsis)

words <- purrr::map(top$genre, ~.x) %>% 
  purrr::map(
    ~df %>%
      dplyr::select(.x) %>% 
      dplyr::filter(across(.fns = (~.==1))) %>% 
      rownames() %>% 
      as.numeric()
  ) %>% 
  purrr::imap(
    ~synop %>% 
      dplyr::filter(animeID %in% .x) %>% 
      dplyr::select(synopsis) %>% 
      dplyr::mutate(synopsis = tolower(synopsis)) %>% 
      dplyr::mutate(synopsis = tm::removePunctuation(synopsis)) %>%
      dplyr::mutate(synopsis = stringi::stri_trans_general(synopsis, id = "Latin-ASCII")) %>% 
      dplyr::mutate(synopsis = tm::removeNumbers(synopsis)) %>% 
      dplyr::mutate(synopsis = tm::removeWords(synopsis, stop)) %>% 
      dplyr::mutate(synopsis = tm::stripWhitespace(synopsis)) %>% 
      tidytext::unnest_tokens(txt, synopsis) %>% 
      dplyr::filter(!is.na(txt)) %>% 
      dplyr::count(txt) %>% 
      dplyr::arrange(desc(n)) %>% 
      dplyr::slice(1:25) %>% 
      dplyr::mutate(rank = 1:25) %>% 
      dplyr::mutate(var = .y)
  ) %>% 
  purrr::map_dfr(~.x)

ranked <- words %>% 
  tidyr::pivot_wider(names_from = "var", values_from = c("txt","n"), id_cols = rank)

## Finds which animes uses the top words in their synopsis the most for each genre
top_words <- words %>% 
  tidyr::pivot_wider(names_from = "rank", values_from = c("txt","n"), id_cols = var) %>% 
  dplyr::mutate(name = paste0("var",var)) %>% 
  dplyr::left_join(top) %>% 
  dplyr::select(starts_with("txt"), starts_with("n"), genre)

essence <- df %>% 
  rownames_to_column(var = "animeID") %>% 
  dplyr::mutate(animeID = as.numeric(animeID)) %>% 
  dplyr::left_join(synop) %>% 
  dplyr::mutate(synopsis = tolower(synopsis)) %>% 
  dplyr::mutate(synopsis = tm::removePunctuation(synopsis)) %>%
  dplyr::mutate(synopsis = stringi::stri_trans_general(synopsis, id = "Latin-ASCII")) %>% 
  dplyr::mutate(synopsis = tm::removeNumbers(synopsis)) %>% 
  dplyr::mutate(synopsis = tm::removeWords(synopsis, stop)) %>% 
  na.exclude()

essence <- top_words %>% 
  dplyr::group_by(genre) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(
    data = purrr::map(
      data,
      ~.x %>%
        cbind(essence) %>% 
        dplyr::select(genre, animeID, synopsis, txt_1:n_25) %>% 
        dplyr::rename_with(.cols = starts_with("genre"),
                           .fn = ~stringr::str_sub(., 1, 5)) %>% 
        dplyr::filter(genre == 1) %>% 
        dplyr::mutate(across(.cols = starts_with("txt"),
                             .fns = ~stringr::str_detect(synopsis,.))) %>% 
        dplyr::mutate(flag = rowSums(.[4:28])) %>% 
        dplyr::filter(flag >= 5) %>% 
        dplyr::mutate(total = stringr::str_count(synopsis, "\\w")) %>% 
        dplyr::select(-synopsis, -genre) %>% 
        tidyr::pivot_longer(cols = matches("^(txt|n)"), names_sep = "_", names_to = c(".value","rank")) %>% 
        dplyr::group_by(animeID) %>% 
        dplyr::mutate(wgt = n/total) %>% 
        dplyr::filter(txt) %>% 
        dplyr::summarise(wgt = sum(wgt)) %>% 
        dplyr::ungroup() %>% 
        dplyr::arrange(desc(wgt)) %>% 
        dplyr::slice(1)
    )
  ) %>% 
  tidyr::unnest(data)

## Gives coordinates to the anime posters (most typical)
typical <- essence %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(image = paste0("oldsets/19WK17/posters/",animeID,".jpg")) %>% 
  dplyr::select(genre, image) %>% 
  dplyr::mutate(x = 5,
                y = 1:10 + 0.2)

## Gives coordinates to the summaries of genres based on the top used words
sumry <- tibble(
  x = 4,
  y = 1:10,
  genre = top$genre,
  label = c("Casual life of high school students and their friends and family",
            "Young people take part in a dispute between groups",
            "In a magical and mysterious world, young people fight evil",
            "Young people fight evil in a mundane fashion",
            "Technology-driven. Robots. Space. The Future.",
            "Young people bonds at school, with friends and family. Sometimes affected by war and time",
            "Educational and music-rich. Pokemon is an important theme",
            "Young people become fast companions to fight evil",
            "High school life through the years. Clubs are an important theme",
            "High school students pursue love and friendship"
  )
) %>% 
  dplyr::mutate(label = stringr::str_wrap(label, 25)) %>% 
  dplyr::left_join(top_join)

## Gives coordinates to the horizontal stripes
Hstripes <- tibble(
  xmin = 
)


lnhgt <- 0.27
asp_ratio <- 0.68

p <- sets %>% 
  ggplot(aes(x = x, y = y)) +
  
  ### Places the points and frequency of the groups of genres
  geom_point(aes(color = color)) +
  geom_text(aes(label = n), family = mono,
            hjust = 0, size = 5, data = size) +
  
  ### Places the titles of genres and their total frequency
  geom_text(aes(x = x1, y = y1, label = genre, color = color),
            hjust = 0, size = 30, family = mono, data = names) +
  geom_text(aes(x = x2, y = y2, label = katakana, color = color),
            hjust = 0, size = 10, family = "wqy-microhei", data = names) +
  geom_text(aes(x = x3, y = y3, label = total, color = color),
            hjust = 1, size = 10, family = mono, data = names) +
  
  ### Places the summary text based on the synopses
  geom_text(aes(x = x, y = y, label = label, color = color), lineheight = lnhgt,
            hjust = 0, vjust = 1, size = 10, family = mono, data = sumry) +
  
  ### Places the posters for the typical animes
  ggimage::geom_image(aes(x = x, y = y, image = image), size = 0.05,
                      by = "width", asp = asp_ratio, data = typical) +
  
  scale_color_identity() +
  scale_y_reverse(limits = c(11,-5)) +
  
  xlim(1,7) +
  # theme_void() +
  theme(
    aspect.ratio = 1/asp_ratio,
    legend.position = "none"
  )

## Saves the plot
ggsave("oldsets/19WK17/anime.png", plot = p, dpi = "retina",
       width = 10*asp_ratio, height = 10) ## Makes the width equivalent to the aspect ratio

