# 1. Loads packages, data and custom fonts
library(dplyr)
library(ggfx)
library(ggplot2)
library(glue)
library(imgpalr)
library(junebug)
library(purrr)
library(ragg)
library(readr)
library(rvest)
library(scales)
library(stringr)
library(tidyr)

## Load the dialogue data
script <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv')

## Scrapes the IMDB rating data
ses <- rvest::session("https://www.imdb.com/title/tt4574334/episodes")
imdb <- unique(script$season) %>% 
  purrr::map_dfr(function(season){
    
    values = ses %>% 
      rvest::session_jump_to(glue::glue("?season={season}")) %>% 
      rvest::html_elements(".ipl-rating-star.small .ipl-rating-star__rating") %>% 
      rvest::html_text() %>% 
      as.numeric()
    
    tibble(
      season = season,
      episode = seq_along(values),
      rating = values
    )
    
  })

## Loads custom fonts
junebug::font_hoist("Benguiat-Bold")
font_titles <- "Benguiat-Bold Regular"
font_text <- "Bitter"

## Gets the colors to use in the punctuation count points
imgpalr::image_pal(
  "2022/week42/poster.png", n = 3, type = "qual",
  bw = c(0.1,0.9), saturation = c(0.7,1),
  brightness = c(0.7,1), seed = 13, plot = TRUE
)

# 2. Manages the data
## Counts the number of uses of some punctuations
punct <- script %>% 
  dplyr::mutate(exclaim = stringr::str_detect(dialogue, fixed("!")),
                question = stringr::str_detect(dialogue, fixed("?"))) %>% 
  dplyr::group_by(season, episode) %>% 
  dplyr::summarise(across(.cols = c(exclaim, question),
                          .fns = ~sum(., na.rm = TRUE))) %>% 
  dplyr::ungroup()

## Rearranges the punctuation data and defines colors
punct <- punct %>% 
  tidyr::pivot_longer(cols = c(exclaim, question),
                      names_to = "punct",
                      values_to = "count") %>% 
  dplyr::mutate(color = ifelse(punct == "question", "#206AB9", "#AD0F0A"))

## Rescales the count to define the lollipop height
## and inverts values for question marks
punct <- punct %>% 
  dplyr::mutate(height = scales::rescale(count, to = c(0.2, 1)),
                height = ifelse(punct == "question", -height, height))

## Places the name for the seasons
punct <- punct %>% dplyr::mutate(season = scales::label_ordinal(suffix = " Season")(season))
imdb <- imdb %>% dplyr::mutate(season = scales::label_ordinal(suffix = " Season")(season))

## Separates the data into two objects
punct_question <- punct %>% dplyr::filter(punct == "question")
punct_exclaim <- punct %>% dplyr::filter(punct == "exclaim")

## Defines the colors of the text ratings
imdb <- imdb %>% 
  dplyr::mutate(color = ifelse(rating <= 7.5, "white", "black"))

## Gets the top5 most used words (alone and pairs)
## imediately before the punctuation
topwords <- c("\\!{1}","\\?{1}") %>%
  purrr::map(function(punct){
    
    regexstr = glue::glue("\\b([:graph:]+)<punct>", .open = "<", .close = ">")
    regexstr = as.character(regexstr)
    
    set = script %>%
      dplyr::rowwise() %>%
      dplyr::mutate(word = list(str_extract_all(dialogue, regex(regexstr), simplify = TRUE))) %>%
      dplyr::pull(word) %>%
      unlist() %>%
      na.exclude() %>%
      stringr::str_remove_all("[:punct:]")%>%
      tolower() %>%
      tibble(word = .) %>%
      dplyr::count(word) %>%
      dplyr::arrange(desc(n)) %>%
      dplyr::mutate(pct = round(100*cumsum(n)/sum(n))) %>%
      dplyr::slice(1:5)
    
    pairs = set %>%
      dplyr::pull(word) %>% 
      purrr::map(function(word){
        
        regexstr = glue::glue("([:graph:]*)([:space:]|\\b)<word><punct>", .open = "<", .close = ">")
        regexstr = as.character(regexstr)
        
        script %>%
          dplyr::rowwise() %>%
          dplyr::mutate(dialogue = tolower(dialogue),
                        word = list(str_extract_all(dialogue, regex(regexstr), simplify = TRUE))) %>%
          dplyr::pull(word) %>%
          unlist() %>%
          na.exclude() %>%
          stringr::str_remove_all("[:punct:]")%>%
          stringr::str_trim() %>% 
          tibble(word = .) %>%
          dplyr::count(word) %>%
          dplyr::arrange(desc(n)) %>%
          dplyr::mutate(pct = round(100*cumsum(n)/sum(n))) %>%
          dplyr::slice(1:5)
        
      })
    
    set %>% dplyr::mutate(pairs = pairs)
    
  })

# 3. Generates plots
## Generates the main plot
main <- imdb %>% 
  ggplot(aes(x = episode)) +
  
  ### Places the lollipops (point+segment)
  ggfx::with_outer_glow(
    x = geom_segment(aes(xend = episode, y = height,
                         yend = 0, color = I(color)),
                     size = 2, data = punct_question),
    colour = "#206AB9",
    expand = 5,
    sigma = 7
  ) +
  ggfx::with_outer_glow(
    x = geom_point(aes(y = height, color = I(color)),
                   size = 4, data = punct_question),
    colour = "#206AB9",
    expand = 10,
    sigma = 15
  ) +
  ggfx::with_outer_glow(
    x = geom_segment(aes(xend = episode, y = height,
                         yend = 0, color = I(color)),
                     size = 2, data = punct_exclaim),
    colour = "#AD0F0A",
    expand = 5,
    sigma = 7
  ) +
  ggfx::with_outer_glow(
    x = geom_point(aes(y = height, color = I(color)),
                   size = 4, data = punct_exclaim),
    colour = "#AD0F0A",
    expand = 10,
    sigma = 15
  ) +
  
  ### Places the ratings
  geom_point(aes(y = 0, fill = rating), color = "black",
             shape = 21, size = 7, stroke = 0, data = imdb) +
  geom_text(aes(y = 0, label = scales::label_number(accuracy = 0.1)(rating), color = I(color)),
            size = 2.5, family = font_text, data = imdb) +
  
  ### Defines the color gradient of the ratings
  scale_fill_gradient(low = "#0D0F00", high = "#E2FF08") +
  
  ### Separates the data by season
  facet_grid(.~season, scales = "free", space = "free") +
  
  ### Eliminates and customizes theme elements
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    plot.margin = margin(20, 20, 20, 20),
    
    strip.text = element_text(color = "white", size = 20, family = font_titles),
    panel.spacing = unit(30, "pt"),
    
    legend.position = "none"
  )

## Saves the plot
ggsave("2022/week42/main.png", plot = main, dpi = 320,
       width = 17, height = 5, device = ragg::agg_png, res = 320)

## Generates the legend
legend <- NULL %>% 
  ggplot(aes(x = 1)) +
  
  ggfx::with_outer_glow(
    x = geom_segment(aes(xend = 1, y = -0.3, yend = 0),
                     color = "#206AB9", size = 2,
                     data = punct_question),
    colour = "#206AB9",
    expand = 5,
    sigma = 7
  ) +
  ggfx::with_outer_glow(
    x = geom_point(aes(y = -0.3), color = "#206AB9",
                   size = 4, data = punct_question),
    colour = "#206AB9",
    expand = 10,
    sigma = 15
  ) +
  ggfx::with_outer_glow(
    x = geom_segment(aes(xend = 1, y = 0.3, yend = 0),
                     color = "#AD0F0A", size = 2,
                     data = punct_exclaim),
    colour = "#AD0F0A",
    expand = 5,
    sigma = 7
  ) +
  ggfx::with_outer_glow(
    x = geom_point(aes(y = 0.3), color = "#AD0F0A",
                   size = 4, data = punct_exclaim),
    colour = "#AD0F0A",
    expand = 10,
    sigma = 15
  ) +
  
  geom_point(aes(y = 0), fill = "#E2FF08", color = "black",
             shape = 21, size = 7, stroke = 2) +
  geom_text(aes(y = 0, label = "X.X"), size = 2) +
  
  scale_y_continuous(expand = expansion(mult = 0.15)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = NA)
  )

## Saves the plot
ggsave("2022/week42/legend.png", plot = legend, dpi = 320,
       width = 0.5, height = 1.5, device = ragg::agg_png, res = 320)
