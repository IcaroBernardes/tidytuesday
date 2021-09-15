# 0. Library management
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(lubridate)
library(showtext)

## Add Google Font
font_add_google(name = "Oswald", family = "oswald")

# 1. Data download, join and handle
tuesdata <- tidytuesdayR::tt_load('2021-09-14')

board <- tuesdata$billboard
board <- board %>% 
  dplyr::mutate(week_id = mdy(week_id)) ## Lubridate parsing of string to date
feat <- tuesdata$audio_features

df <- full_join(board, feat)

# 2. Loudness plot
loud <- df %>%
  dplyr::select(song_id, loudness, week_id) %>% 
  ## Eliminates duplicate instances of songs, keeping all variables
  distinct(song_id, .keep_all = TRUE) %>% 
  ## Extracts year from the date
  dplyr::mutate(year = lubridate::year(week_id)) %>% 
  ## Keeps only songs with loudness and year info
  dplyr::filter(!is.na(loudness) & !is.na(year)) %>% 
  dplyr::select(loudness, year) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(loudness = median(loudness), size = n()) %>% 
  ## Creates a new variable to help place the steps
  dplyr::mutate(step = loudness+22)

## Steps to break the medians
ticks <- 0.25

## Breaks down the medians in small steps
aux2 <- tibble(year = 0, loudness = 0, size = 0, step = 0)
for (i in 1:dim(loud)[1]) {
  
  aux <- tibble(
    year = loud$year[i],
    loudness = loud$loudness[i],
    size = loud$size[i],
    step = seq(0, loud$step[i], ticks)
  )
  
  aux2 <- rbind(aux2, aux)
  
}
loud <- aux2[-1,]

## Increment/decrement to set the limits of each step  
del <- 0.05

## Establishes max and min in y for each step
aux2 <- tibble(year = 0, loudness = 0, size = 0, step = 0, y = 0)
for (i in 1:dim(loud)[1]) {
  
  aux <- tibble(
    year = loud$year[i],
    loudness = loud$loudness[i],
    size = loud$size[i],
    step = loud$step[i],
    y = c(loud$step[i]-del,loud$step[i]+del)
  )
  
  aux2 <- rbind(aux2, aux)
  
}
loud <- aux2[-1,]

## Establishes max and min in x for each step
loud <- loud %>% 
  ### Creates new variables to set the x-limits of the rounded squares
  dplyr::mutate(start = year-0.25,
                end = year+0.25) %>% 
  ### Lengthens the dataframe so these limits are allocated in each line
  pivot_longer(cols = start:end,
               values_to = "x") %>% 
  dplyr::select(-name)

## Creates an id combining step and year
loud <- loud %>% 
  dplyr::mutate(id = paste(year,step))

## Defines the plot order of the poins
loud <- loud %>% 
  dplyr::mutate(order = rep(c(1,2,4,3), dim(loud)[1] / 4)) %>% 
  dplyr::arrange(id, order)

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
{
  showtext_auto()

## Prints the plot
loud %>% 
  ggplot(aes(x = x, y = y, group = id, fill = step)) +
  geom_polygon() +
  ## Title and subtitle annotation
  annotate("text", x = 1958, y = 20, label = "Turn it up!",
           colour = "white", size = 60, hjust = 0, vjust = 1, family = "oswald") +
  annotate("text", x = 1958, y = 16.9, label = "Have the Billboard hits become louder?",
           colour = "white", size = 18, hjust = 0, vjust = 1, family = "oswald") +
  annotate("text", x = 1958, y = 15.9, label = expression("Source: Data.World "^{"[1]"}~"|"~"Graphic: Ícaro Bernardes "^{"[2]"}),
           colour = "white", size = 12, hjust = 0, vjust = 1, family = "oswald") +
  ## Text and support annotations
  annotate("text", x = 2005, y = 19, family = "oswald",
           label = expression("In the 90's the tracks grew louder.\nAs more able engineers pushed the limits of the CD format\nthe industry started to associate comercial success to loudness. "^"[3]"),
           hjust = 0, vjust = 0, colour = "white", size = 12, lineheight = 0.3) +
  annotate("tile", x = 1990, y = 13.5, fill = "white", width = 4, height = 1) +
  annotate("point", x = 1990, y = 13, fill = "white", colour = "white", size = 3, shape = 25) +
  annotate("text", x = 1990, y = 13.5, label = "-9.32db", colour = "#9c6102", size = 15, family = "oswald") +
  annotate("tile", x = 2002, y = 17.5, fill = "white", width = 4, height = 1) +
  annotate("point", x = 2002, y = 17, fill = "white", colour = "white", size = 3, shape = 25) +
  annotate("text", x = 2002, y = 17.5, label = "-5.43db", colour = "#DE0001", size = 15, family = "oswald") +
  ## Arrows annotations
  annotate("curve", x = 2004.5, y = 20, xend = 2002, yend = 18.2, arrow = arrow(length = unit(0.01, "npc")), color = "white", curvature = 0.3) +
  annotate("curve", x = 2004.5, y = 20, xend = 1990, yend = 14.2, arrow = arrow(length = unit(0.01, "npc")), color = "white", curvature = 0.4) +
  ## Axis titles and footnotes
  labs(x = "Year", y = "Median loudness of the songs (db)",
       caption = paste(
         "[1]: Data.World by Sean Miller. https://data.world/kcmillersean/billboard-hot-100-1958-2017#",
         "[2]: Tidytuesday repository. https://github.com/IcaroBernardes/tidytuesday",
         "[3]: Loudness war. https://en.wikipedia.org/wiki/Loudness_war",
         sep = "\n")
  ) +
  scale_fill_gradient2(low="#000C66", high="#DE0001", mid="#FEDA15", midpoint=median(loud$step)) +
  scale_y_continuous(limits = c(-1,20), labels = function(x) x-20) +
  scale_x_continuous(n.breaks = 7) +
  theme_ipsum() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "black"),
        axis.text.x = element_text(size = 45, colour = "white", family = "oswald"),
        axis.text.y = element_text(size = 45, colour = "white", family = "oswald"),
        axis.title.x = element_text(size = 60, colour = "white", family = "oswald"),
        axis.title.y = element_text(size = 60, colour = "white", family = "oswald"),
        ## Modifies lineheight to control the gap between lines of footnotes
        plot.caption = element_text(size = 20, colour = "white", hjust = 0, family = "oswald", lineheight = 0.4))

## Saves the plot
ggsave("week38/loudness.png", width = 40, height = 20, units = "cm")
  }

