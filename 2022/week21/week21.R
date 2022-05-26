# 0. Library and fonts management
library(tidyverse)
library(forcats)
library(glue)
library(cols4all)
library(ragg)
library(ggtext)
library(ggpattern)

## Defines some layout constants
lineheight <- 1.2
nudgex <- 50 ### Added space inf the right of the series of the countries
a <- 30 ### Half-width of the "balls"
b <- 0.4 ### Half-height of the "balls"
serif <- "Palatino Linotype"

# 1. Data download, load and handling
sevens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/sevens.csv')

## Gets the top 7 nations in number of games
nations <- sevens %>% 
  tidyr::pivot_longer(cols = starts_with("team")) %>% 
  dplyr::count(value) %>% 
  dplyr::slice_max(order_by = n, n = 7) %>% 
  dplyr::pull(value)

## Keeps only the top 7 nations
df <- sevens %>% 
  dplyr::filter(if_any(.cols = starts_with("team"), .fns = ~(. %in% nations))) %>% 
  dplyr::select(matches("(^score|team)|date")) %>% 
  dplyr::mutate(across(.cols = starts_with("score"), .fns = as.numeric)) %>% 
  stats::na.exclude()

## Calculates the margin of points for the teams in each game
df <- df %>% 
  dplyr::mutate(points_1 = score_1-score_2,
                points_2 = score_2-score_1) %>% 
  dplyr::select(-starts_with("score"))

## Arranges and filters the data
df <- df %>% 
  tidyr::pivot_longer(
    cols = matches("^points|team"),
    names_to = c(".value","side"),
    names_sep = "_"
  ) %>% 
  dplyr::filter(team %in% nations)

## Converts the country names to factor and
## creates a variable to store the number of matches
df <- df %>% 
  dplyr::mutate(team = factor(team, levels = nations),
                team = forcats::fct_rev(team),
                id = as.numeric(team)) %>% 
  dplyr::arrange(team, date) %>%
  dplyr::group_by(team) %>% 
  dplyr::mutate(matches = 1:n()) %>% 
  dplyr::select(-side, -date)

## Defines coordinates for the teams stats
styled1 <- "<span style='font-size:16px;'>"
styled2 <- "<span style='font-size:7px;'>"
info <- df %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(
    matches = glue::glue("{styled1}{n()}</span><br>{styled2}MATCHES</span>"),
    victories = glue::glue("{styled1}{round(100*sum(points > 0)/n())}%</span><br>{styled2}VICTORIES</span>"),
    pts_for = glue::glue("{styled1}{sum(points[points > 0])}</span><br>{styled2}POINTS MADE</span>"),
    pts_against = glue::glue("{styled1}{-sum(points[points < 0])}</span><br>{styled2}POINTS SUFFERED</span>"),
    team = paste0("<span style='font-size:35px;'>**", unique(team), "**</span>"),
    x = n() + nudgex
  ) %>% 
  tidyr::pivot_longer(cols = -c(id, x)) %>% 
  dplyr::ungroup()
info_coord <- tibble(
  name = c("team","matches", "victories", "pts_for", "pts_against"),
  dx = c(95,50,75,105,140),
  dy = c(0.18,rep(-0.25,4))
)
info <- info %>% 
  dplyr::left_join(info_coord) %>% 
  dplyr::mutate(x = dx + x,
                y = dy + id)

## Defines coordinates for the "ball trail"
rect <- df %>% 
  dplyr::filter(matches == 1 | matches == n()) %>% 
  dplyr::mutate(x = c("xmin","xmax")) %>%
  dplyr::ungroup() %>% 
  dplyr::select(id, matches, x) %>% 
  tidyr::pivot_wider(
    names_from = x,
    values_from = matches
  ) %>% 
  dplyr::mutate(xmax = xmax + nudgex)

## Defines coordinates for the "ball"
ball <- df %>% 
  dplyr::filter(matches == n()) %>%
  dplyr::mutate(matches = matches+nudgex) %>% 
  dplyr::select(-points) %>% 
  purrr::pmap(
    function(team, id, matches) {
      x1 = seq(matches-a, matches+a, 0.1)
      x2 = seq(matches+a, matches-a, -0.1)
      y1 = id + sqrt((b^2)*(1-(((x1-matches)^2)/(a^2))))
      y2 = id - sqrt((b^2)*(1-(((x2-matches)^2)/(a^2))))
      
      tibble(
        x = c(x1,x2),
        y = c(y1,y2),
        team = team
      )
    }
  ) %>% 
  purrr::reduce(rbind)

## Adds the path to the images for the balls
flags <- glue::glue("2022/week21/flags/{nations}.png")
names(flags) <- nations

## Defines title and subtitle
title <- "THE SEVEN NATIONS RUGBY ARMIES SHOW THEIR STRIPES"
subtitle <- paste(
  "Rugby Seven is played with teams of seven. The plot bellow shows how well",
  "fared the seven feminine national teams with most matches played.<br>",
  "Darker <span style='color:red;'>**red stripes**</span> represent",
  "<span style='color:red;'>**loses**</span> with higher point disadvantage.",
  "Darker <span style='color:blue;'>**blue stripes**</span> represent",
  "<span style='color:blue;'>**wins**</span> with higher point advantage.",
  "<span style='font-size:15px;'><br><br>Data from ScrumQueens | Graphic by Ícaro Bernardes (@IcaroBSC)</span>"
)

# 2. Generates the plot
## Creates the main plot
p <- df %>% 
  ggplot() +
  
  ### Places the "ball trail"
  geom_rect(aes(xmin = -Inf, xmax = xmax, ymin = id-0.4, ymax = id+0.4),
            fill = "#A4BD93", color = NA, alpha = 0.6, data = rect) +
  
  ### Places the tiles and applies a divergent palette
  geom_tile(aes(x = matches, y = id, fill = points), height = 0.8, alpha = 0.6) +
  cols4all::scale_fill_continuous_c4a_div(
    palette = "tableau.classic_red_blue",
    n.breaks = 7, name = "Difference in points"
  ) +
  
  ### Places the "balls" with the image of the flags as a pattern in the polygon
  ggpattern::geom_polygon_pattern(
    aes(x = x, y = y, pattern_filename = team),
    pattern = 'image', pattern_type = "none", pattern_scale = 6.1, data = ball
  ) +
  ggpattern::scale_pattern_filename_manual(values = flags, guide = "none") +
  
  ### Places the stats of the countries
  ggtext::geom_richtext(aes(x = x, y = y, label = value), family = serif,
                        fill = NA, label.color = NA, lineheight = lineheight, 
                        size = 1, data = info) +
  
  ### Places the title and subtitle
  labs(title = title, subtitle = subtitle) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    text = element_text(family = serif),
    
    legend.title = element_text(
      angle = 90, vjust = 0.9, size = 16, face = "bold"
      ),

    plot.background = element_rect(fill = "#C7E4B2", colour = NA),
    plot.title = element_text(
      size = 33.5, face = "bold", hjust = 1,
      margin = margin(0,0,20,0)
      ),
    plot.subtitle = ggtext::element_markdown(
      size = 15, halign = 0, hjust = 0.4, lineheight = lineheight
    ),
    plot.margin = margin(30,40,20,0)
    
  )

## Saves the plot
ggsave("2022/week21/sevens.png", plot = p, dpi = "retina",
       width = 16, height = 10, device = ragg::agg_png, res = 320)

