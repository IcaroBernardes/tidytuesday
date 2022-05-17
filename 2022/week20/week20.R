# 0. Library and fonts management
library(tidyverse)
library(ragg)
library(ggtext)
library(ggbump)
library(ggfx)
library(nflplotR)
library(scales)

## Defines some layout constants
lineheight <- 0.9
sans <- "Disco 1"
serif <- "Bitter"

# 1. Data download, load and handling
eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')
eurovision_votes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision-votes.csv')

## Eliminates pairing of country with itself and keeps relevant data
ranking <- eurovision_votes %>% 
  dplyr::filter(is.na(duplicate)) %>% 
  dplyr::select(year, "country" = "to_country", points)

## Sums all points gained by each country in each year
ranking <- ranking %>% 
  dplyr::group_by(year, country) %>% 
  dplyr::summarise(points = sum(points)) %>% 
  dplyr::group_by(country)

## Gets the top countries in points over all years
toplands <- ranking %>% 
  dplyr::summarise(points = sum(points)) %>% 
  dplyr::slice_max(order_by = points, n = 5) %>% 
  dplyr::pull(country)
toplands <- c(toplands, "Ireland")

## Gets the yearly evolution in the rank of total points for the top countries
ranking <- ranking %>% 
  dplyr::mutate(points = cumsum(points)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::arrange(desc(points)) %>% 
  dplyr::mutate(rank = 1:n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(country %in% toplands)

## Adds countries flags, colors and names
id <- tibble(
  country = c("Sweden", "Ukraine", "Russia", "Norway", "Greece","Ireland"),
  main_color = c("#008CFA","#F5EB01","#0116FF","#F50023","white","#16BD00"),
  second_color = c("#F5EB01","#0056F5","#F50023","white","#0056F5","#F2A502"),
  code = c("SE","UA","RU","NO","GR","IR")
) %>% 
  dplyr::mutate(image = glue::glue("2022/week20/flags/{code}.png"))

ranking <- id %>% 
  dplyr::select(country, main_color, second_color) %>% 
  dplyr::left_join(ranking)

## Defines coordinates for countries ID's
id <- id %>% 
  dplyr::mutate(
    ### ID coordinates
    x = c(2013,2005,1997,1975,1990,1978),
    ### Name boxes coordinates
    y = c(-2.5,29.5,27.5,21.5,21.5,-0.5),
    ### Triangles nudge
    n = c(+2.4,rep(-2.4, 4),+2.4),
    ### Triangles
    tri = c("▼",rep("▲", 4),"▼")
  )

## Adds years of victory
victory <- eurovision %>% 
  dplyr::filter(winner, section %in% c("final","grand-final")) %>% 
  dplyr::select(year, "country" = "artist_country", winner) %>% 
  dplyr::filter(country %in% toplands)
ranking <- ranking %>% 
  dplyr::left_join(victory) %>% 
  dplyr::mutate(winner = ifelse(is.na(winner), main_color, second_color))

## Defines coordinates for the titles
titles <- tibble(
  x = c(1976, 2003, 2003),
  y = c(-9.3, -10.1, -6.6),
  size = c(120, 8, 5),
  family = c(sans, serif, serif),
  label = c(
    "THE EURO STARS",
    
"Eurovision is one of the world's longest-running television programmes.
Since it started in 1956, it grew as more countries sent competitors.
The graphic bellow shows the Top 5 countries in terms of cumulative
points received as well as Ireland (the biggest champion). Years in which
the countries won are highlighted by points with their secundary flag color.",
    
    "Data by Eurovision by way of Tom Mock aided by Tanya Shapiro and Bob Rudis | Graphic by Ícaro Bernardes (@IcaroBSC)"
    
    )
)

# 2. Generates the plot
## Creates the main plot
p <- ranking %>% 
  ggplot() +
  
  ### Places the line of evolution of the countries
  ggbump::geom_bump(aes(x = year, y = rank, color = I(main_color), group = country),
                    size = 2) +
  
  ### Places the points that show the rank for a given year
  ### and if the country won the competition
  geom_point(aes(x = year, y = rank, fill = I(winner)), size = 5, stroke = 2,
             shape = 21, color = "black") +
  
  ### Places the text box with the names of the countries
  ggtext::geom_textbox(aes(x = x, y = y, label = country,
                           fill = I(main_color), color = I(second_color)),
                       box.colour = NA, fontface = "bold", size = 8,
                       width = unit(2.1, "inch"), height = unit(1.3, "inch"),
                       box.r = unit(20, "pt"), valign = 0, halign = 0.5,
                       family = serif, data = id) +
  
  ### Places the triangle of the tooltip
  geom_text(aes(x = x, y = y+n, label = tri, color = I(main_color)),
            size = 17, data = id) +
  
  ### Places the flag of the countries
  ggfx::with_shadow(
    nflplotR::geom_from_path(aes(x = x, y = y-0.6, path = image),
                             width = 0.025, data = id)
  ) +
  
  ### Places a tile behind the titles
  annotate("point", x = 1976, y = -10.25, color = "white", size = 114.5) +
  annotate("rect", xmin = 1976, xmax = Inf, ymin = -5.5, ymax = -15, fill = "white") +
  
  ### Places the title
  geom_text(aes(x = x, y = y, label = label, size = I(size), family = family),
            color = "black", hjust = 0, lineheight = lineheight, data = titles) +
  
  ### Defines limits and names for the axes. Reverts and defines breaks for the y-axis 
  scale_x_continuous(name = "Year") +
  scale_y_reverse(name = "Rank of cumulative points",
                  breaks = c(1,5,15,30), label = scales::ordinal) +
  coord_cartesian(xlim = c(1974, 2023), ylim = c(31,-15)) +
  
  ### Eliminates and customizes elements of the plot
  theme_void() +
  theme(
    text = element_text(color = "white", family = serif),
    
    plot.background = element_rect(fill = "black", color = NA),
    plot.margin = margin(0, 0, 40, 40),
    
    axis.text = element_text(size = 25, hjust = 1),
    axis.title = element_text(size = 40, face = "bold"),
    axis.title.x = element_text(margin = margin(30, 0, 0, 0), hjust = 0),
    axis.title.y = element_text(angle = 90, margin = margin(0, 30, 0, 0), hjust = 0)
  )

## Saves the plot
ggsave("2022/week20/eurovision.png", plot = p, dpi = "retina",
       width = 30, height = 20)

