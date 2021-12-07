# 0. Library management
library(tidyverse)
library(ggplot2)
library(showtext)
library(readr)
library(ggimage)
library(patchwork)

## Adding Google Fonts
font_add_google(name = "Rubik", family = "rubik") ### Sans Serif
sans <- "rubik"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_auto()

# 1. Data download, load and handling
matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

## Gets the teams with most games played (at least 95%)
topteams <- matches %>% 
  tidyr::pivot_longer(cols = c("team1","team2")) %>% 
  dplyr::count(value) %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::mutate(pct = 100*cumsum(n)/sum(n)) %>% 
  dplyr::mutate(pct = floor(pct)) %>% 
  dplyr::filter(pct <= 95) %>% 
  dplyr::pull(value)

## Gets the data of matches envolving at least one of the top teams
matches <- matches %>%
  dplyr::filter(if_all(.cols = dplyr::matches("^team[[:digit:]]$"), .fns = ~ . %in% topteams))

## Gets the data of interest for the winner of each match
df <- matches %>%
  dplyr::filter(winner %in% topteams) %>% 
  dplyr::mutate(var0 = winner,
                var1 = ifelse(winner == team1, team1_away_or_home, team2_home_away),
                var2 = ifelse(time_of_day == "Day", "Day", "Day +\nnight"),
                var3 = stringr::str_remove(toss_decision, " first"),
                var4 = ifelse(winner == player_of_match_team, "yes", "no")) %>% 
  dplyr::select(dplyr::matches("var[[:digit:]]")) %>% 
  dplyr::mutate(across(.cols = num_range("var",1:4), .fns = toupper))

## Counts the results for each team
df <- df %>%
  dplyr::group_by(var0) %>% 
  tidyr::nest() %>%
  dplyr::mutate(data = map(data, ~ .x %>% tidyr::pivot_longer(cols = everything()))) %>% 
  dplyr::mutate(data = map(data, ~ .x %>% dplyr::count(name, value))) %>% 
  tidyr::unnest(data) %>% 
  dplyr::group_by(var0, name) %>% 
  dplyr::mutate(frac = round(n/sum(n), 2)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(name = factor(name),
                name = forcats::fct_rev(name))

## Makes sure each country has all categories and elements
df <- df %>%
  tidyr::complete(var0, tidyr::nesting(name, value)) %>% 
  dplyr::mutate(categy = as.numeric(name))

## Defines the category names
categ <- tibble(
  name = paste0("var",1:4),
  cat = c("Field","Time","Initial role","Earned MVP?")
)
df <- df %>%
  dplyr::left_join(categ)

## Defines the side of the card in which data will be placed and more
df <- df %>%
  dplyr::arrange(var0, name, value) %>% 
  dplyr::mutate(side = rep(c(-2,2), 40),
                categx = side*0.6,
                hjust = ifelse(side == 2, 0, 1),
                strip = ifelse(side == 2, frac, -frac)
  ) 

## Creates tibbles to hold the teams names, wins, matches and flags
## from https://www.flaticon.com/packs/countrys-flags
stats <- tibble(
  team = topteams
) %>% 
  dplyr::group_by(team) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(data = list(matches)) %>% 
  dplyr::mutate(data = data %>% purrr::map(
    ~.x %>% dplyr::summarise(
      games = sum(ifelse((team1 == team) | (team2 == team), 1, 0)),
      wins = sum(ifelse(winner == team, 1, 0))
    )
  )) %>% 
  tidyr::unnest(data) %>% 
  dplyr::rename("var0" = "team")

topteams <- tibble(
  var0 = topteams
) %>% 
  dplyr::mutate(
    flag = case_when(
      var0 == "Sri Lanka" ~ 'https://cdn-icons-png.flaticon.com/512/197/197398.png',
      var0 == "South Africa" ~ 'https://cdn-icons-png.flaticon.com/512/197/197562.png',
      var0 == "Pakistan" ~ 'https://cdn-icons-png.flaticon.com/512/197/197606.png',
      var0 == "India" ~ 'https://cdn-icons-png.flaticon.com/512/197/197419.png',
      var0 == "Australia" ~ 'https://cdn-icons-png.flaticon.com/512/197/197507.png',
      var0 == "Zimbabwe" ~'https://cdn-icons-png.flaticon.com/512/197/197394.png',
      var0 == "England" ~'https://cdn-icons-png.flaticon.com/512/197/197485.png',
      var0 == "New Zealand" ~'https://cdn-icons-png.flaticon.com/512/197/197589.png',
      var0 == "West Indies" ~'https://s.ndtvimg.com/images/entities/300/west-indies-2119.png',
      var0 == "Bangladesh" ~'https://cdn-icons-png.flaticon.com/512/197/197509.png'
    ))

## Defines some layout constants
asp_ratio <- 0.7 ### Aspect ratio
lnht <- 0.25

## Creates the points that define the "ball trajectory" to decorate the cards
waves <- tibble(
  x = seq(from = -1.45, to = 1.97, by = 0.05)
) %>% 
  dplyr::mutate(y = (3-x)*(x+2.5))

# 2. Comparing wins in different conditions
## Creates the cards
p <- df %>% 
  ggplot() +
  
  ### Creates the cards bodies
  geom_rect(aes(xmin = -2, xmax = 2, ymin = 0, ymax = 8), fill = "#317A44") +
  
  ### Creates the wins and games played highlight
  geom_rect(aes(xmin = 1, xmax = 1.99, ymin = 4.95, ymax = 6.55),
            fill = "white", alpha = 0.8, data = stats) +
  geom_text(aes(x = 1.5, y = 6.25, label = games), family = sans, size = 15, data = stats) +
  geom_text(aes(x = 1.5, y = 5.95, label = "MATCHES"), family = sans, size = 7) +
  geom_segment(aes(x = 1.2, xend = 1.8, y = 5.75, yend = 5.75), lineend = "round") +
  geom_text(aes(x = 1.5, y = 5.5, label = wins), family = sans, size = 15, data = stats) +
  geom_text(aes(x = 1.5, y = 5.2, label = "WINS"), family = sans, size = 7) +
  
  ### Creates the cards decorations
  geom_ribbon(aes(x = x, ymin = y, ymax = y+0.5), fill = "white",
              alpha = 0.1, data = waves) +
  geom_point(aes(x = x, y = y+0.25), color = "#AC1B0C", size = 6,
             data = waves[1,]) +
  annotate("line", x = waves$x[1]+c(-0.15,0.15), y = waves$y[1]+0.25,
           color = "black", size = 0.3, lineend = "round") +
  annotate("line", x = waves$x[1]+c(-0.125,0.125), y = waves$y[1]+0.33,
           color = "white", size = 0.6, lineend = "round", linetype = "dotted") +
  annotate("line", x = waves$x[1]+c(-0.125,0.125), y = waves$y[1]+0.17,
           color = "white", size = 0.6, lineend = "round", linetype = "dotted") +
  
  ### Creates the cards outlines
  geom_rect(aes(xmin = -2, xmax = 2, ymin = 0, ymax = 8),
            fill = NA, color = "#57E001", size = 3) +
  geom_rect(aes(xmin = -1.99, xmax = 1.99, ymin = 0.01, ymax = 7.99),
            fill = NA, color = "#317A44", size = 1) +
  
  ### Places the cards names and images
  geom_text(aes(x = 0, y = 7.1, label = var0), fontface = "bold",
            color = "white", family = sans, size = 25, data = topteams) +
  geom_point(aes(x = 0, y = 5.7), color = "white", size = 24) +
  geom_point(aes(x = 0, y = 5.7), color = "black", size = 20, shape = 21, stroke = 1.5) +
  #### Sets size by width and aspect ratio
  ggimage::geom_image(aes(x = 0, y = 5.7, image = flag), data = topteams,
                      size = 0.22, by = "width", asp = asp_ratio) + 
  
  ### Creates the stats bars
  geom_segment(aes(x = -1, xend = 1, y = categy, yend = categy), color = "white",
               size = 4, lineend = "round", alpha = 0.2) +
  geom_segment(aes(x = 0, xend = strip, y = categy, yend = categy), color = "white",
               size = 4, lineend = "round") +
  geom_point(aes(x = 0, y = categy), size = 3.3) +
  
  ### Places the category and element names as well the values
  geom_text(aes(x = categx, y = categy, label = value), hjust = df$hjust,
            lineheight = lnht, family = sans, size = 11, color = "white") +
  geom_text(aes(x = strip, y = categy, label = n), nudge_y = -0.31,
            family = sans, size = 7, color = "white") +
  geom_text(aes(x = 0, y = categy, label = cat), nudge_y = 0.31,
            family = sans, size = 8.5, color = "white") +
  
  xlim(-2,2) +
  facet_wrap(~ var0, ncol = 2) +
  
  theme_void() +
  theme(
    aspect.ratio = 1/asp_ratio, ### Defines aspect ratio on theme also
    legend.position = "none",
    strip.text = element_blank()
  )

## Creates text of the titles
title <- "Stick\nTo\nThe\nBest"
sub1 <- "Ten teams\nplayed 95% of\nall registered\nmacthes of\nCricket in\nthe One Day\nInternational\nformat.\n\nThe cards in\nthe right show\nhow many\nvictories each\nteam had\non different\ncoditions."
sub2 <- "Data: ESPN\nCricinfo\nby way of\nHassanasir\n\nFlag icons:\nFlaticon\n\nGraphic: Ícaro\nBernardes\n@IcaroBSC"

## Creates the table
p <- df %>% 
  ggplot() +
  
  ### Places the title
  annotate("text", x = 0, y = 6, label = title, size = 120, hjust = 0, vjust = 1,
           family = sans, color = "white", lineheight = lnht) +
  
  ### Places the subtitles
  annotate("text", x = 0, y = 4, label = sub1, size = 40, hjust = 0, vjust = 1,
           family = sans, color = "white", lineheight = lnht) +
  annotate("text", x = 6*asp_ratio, y = 6, label = sub2, size = 40, hjust = 1, vjust = 1,
           family = sans, color = "white", lineheight = lnht) +
  
  ### Creates some dices
  annotate("point", x = 0.4, y = 1, size = 30, color = "white", shape = 18) + 
  annotate("point", x = 0.4, y = 1, size = 5, color = "black") + 
  annotate("point", x = 0.8, y = 0.7, size = 25, color = "white", shape = 15) + 
  annotate("point", x = c(0.75,0.75,0.85,0.85), y = c(0.65,0.75,0.65,0.75), size = 5, color = "black") + 
  
  xlim(0,6*asp_ratio) + ylim(0,6) +
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#350C08", color = "#512C19", size = 20),
  ) +
  patchwork::inset_element(p, 0.01,0.01,0.99,0.99)

## Saves the plot
ggsave("2021/week49/cards.png", plot = p, dpi = "retina",
       width = 20*asp_ratio, height = 20) ## Makes the width equivalent to the aspect ratio
