# 0. Library and fonts management
library(tidyverse)
library(ragg)
library(scales)
library(glue)
library(ggimage)
library(ggfx)
library(ggtext)

## Defines some layout constants
width <- 45 ### Width of the plot
height <- 10 ### Height of the plot
lnhgt <- 2.8 ### Line height

# 1. Data download, load and handling
## Gets the data from the dplyr package
df <- dplyr::starwars

## Selects some characters and their heights
chars <- c("Luke Skywalker","C-3PO","R2-D2","Darth Vader",
           "Leia Organa","Han Solo","Yoda","Chewbacca")
df <- df %>% 
  dplyr::filter(name %in% chars) %>% 
  dplyr::select(name, height)

## Creates a new variable to map the heights
df <- df %>% 
  dplyr::mutate(size = scales::rescale(height, to = c(0.5, 0.9)))

## Adds the path to the images
df <- df %>% 
  dplyr::mutate(image = glue::glue("extras/starwars/cardboards/images/{tolower(name)}.png"))

## Orders the data by height and defines the coordinates in the x-axis
df <- df %>% 
  dplyr::arrange(height) %>% 
  dplyr::mutate(x = 1:n())

## Defines the coordinates in the y-axis
df <- df %>% 
  dplyr::mutate(y = -(1-size)+0.03)

## Defines coordinates for the labels
df <- df %>% 
  dplyr::mutate(
    yL = y + size - 0.03,
    xL = x + 0.25,
    label = glue::glue("<span style='font-size:50px;'>**{name}**</span><br><span style='font-size:30px;'>{height} cm</span>")
  )

## Defines coordinates for the titles
titles <- tibble(
  x = 0.7,
  y = c(0.9,0.6,0.47),
  size = c(40,11,9),
  label = c(
    "THE HEIGHT OF THE STARS",
    
    "The figures bellow have their heights scaled to the height of the characters in cm",
    
    "Graphic by: Ícaro Bernardes (@IcaroBSC)"
    )
)

# 2. Makes the plot
## Creates the plot
p <- df %>% 
  ggplot() +
  
  ### Places the names and heights
  ggtext::geom_richtext(aes(x = xL, y = yL, label = label), hjust = 0, fill = NA,
                        color = "white", label.color = NA, lineheight = lnhgt,
                        family = "TradeGothic LT CondEighteen") +
  
  ### Places lines that link the labels to the images
  geom_segment(aes(x = x+0.01, xend = xL-0.01, y = yL, yend = yL),
               color = "white", size = 2, linetype = "dotted") +
  geom_linerange(aes(x = xL-0.01, ymin = yL-0.08, ymax = yL+0.09),
                 color = "white", size = 2) +
  
  ### Places the images with an outer white border
  ggfx::with_outer_glow(
    ggimage::geom_image(aes(x = x, y = y, image = image,
                            size = I(size), group = x),
                        by = "height", asp = width/height),
    colour = "white",
    expand = 20,
    sigma = 0
  ) +
  
  ### Places the titles
  geom_text(aes(x = x, y = y, label = label, size = I(size)), color = "yellow",
            family = "TradeGothic LT CondEighteen", hjust = 0, vjust = 1,
            fontface = "bold", data = titles) +
  
  ### Customizes the axes
  scale_x_continuous(expand = expansion(add = c(0.3,0.5), 0)) +
  scale_y_continuous(limits = c(-1,1),
                     expand = expansion(0,0)) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = NA)
  )

## Saves the plot
ggsave("extras/starwars/cardboards/cardboards.png", plot = p, dpi = "retina",
       width = width, height = height, device = ragg::agg_png, res = 320)

