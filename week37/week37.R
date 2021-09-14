# 0. Library management
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(ggforce)

# 1. Data download and join
tuesdata <- tidytuesdayR::tt_load('2021-09-07')

## Rename time variable so does not match variable in the pit stops dataframe
races <- tuesdata$races %>% 
  dplyr::rename(time_start = time)

pits <- tuesdata$pit_stops

teams <- tuesdata$constructors %>% 
  dplyr::select(constructorId,name) %>% 
  dplyr::rename(constructor = name)

drivers <- tuesdata$results %>% 
  dplyr::select(raceId,driverId,constructorId)

## Join the dataframes and filter the stops from 2021 season grand prix that have data
df <- full_join(races,pits) %>% 
  full_join(drivers) %>% 
  full_join(teams) %>% 
  dplyr::filter(year == 2021 & !is.na(milliseconds))

# 3. Plot production
## Constants used in the making of the graph
limit <- 3.5e+05
ticks <- limit/35
scale <- 1E+04

## Obtains the mean of pit stop times for each team in milliseconds
datum <- df %>% 
  dplyr::group_by(constructor) %>% 
  dplyr::summarise(lapse = mean(milliseconds)) %>% 
  ungroup()

## Breaks down the averages in small steps
aux2 <- tibble(constructor = "fst", lapse = 0)
for (i in 1:dim(datum)[1]) {
  
  aux <- tibble(constructor = datum$constructor[i],
                lapse = c(rep(ticks, datum$lapse[i] %/% ticks), datum$lapse[i] %% ticks))
  
  aux2 <- rbind(aux2, aux)
  
}
datum <- aux2[-1,]

## Unites the steps in a cumsum and recovers the average
datum <- datum %>% 
  dplyr::group_by(constructor) %>% 
  dplyr::mutate(sum = cumsum(lapse)) %>% 
  dplyr::mutate(maxim = max(sum)) %>% 
  dplyr::mutate(textual = paste0(round(maxim/scale,2),"s"))

datum %>% 
  ## Generates bars with the steps stacked (allowing a gradient effect along the bar)
  ggplot(aes(x = 1, y = lapse, fill = sum)) +
  geom_bar(stat = "identity", position = "stack") + 
  ## Generates the "clock hand"
  geom_rect(aes(xmin = 0.5, xmax = 1.35, ymin = maxim-3500, ymax = maxim), fill = "black") +
  geom_text(aes(x = 1, y = 100000, label = textual), size = 5) +
  ggtitle(expression(atop("Which is the fastest F1 pit stop team?",
                          atop(italic("Average pit stop times of the teams in the first thirteen Grand Prix of 2021 Season"), "")
                          ))) +
  ## Generates the "clock circular frame"
  annotate("linerange", x = 1.5, ymin = 0, ymax = limit, color = "black", size = 4) +
  ## Generates the dot at the center of the "clock"
  annotate("point", x = 0.5, y = 0, size = 4) +
  ## Generates the "times inside the clock"
  annotate("text", x = 1.3, y = 0, label = 0, size = 3) +
  annotate("text", x = 1.3, y = 70000, label = 7, size = 3) +
  annotate("text", x = 1.3, y = 140000, label = 14, size = 3) +
  annotate("text", x = 1.3, y = 210000, label = 21, size = 3) +
  annotate("text", x = 1.3, y = 280000, label = 28, size = 3) +
  ## Makes use of the cumsum steps to create a gradient along each bar
  scale_fill_gradient2(low="#FA1E4032", high="#FA1E40FA", mid="#FA1E4096", midpoint=median(datum$sum)) +
  ylim(0, limit) +
  #scale_y_continuous(labels = NULL, limits = c(0, limit), breaks = seq) +
  scale_x_continuous(breaks = 1.6, minor_breaks = NULL, limits = c(0.5,1.5)) +
  ## Brings the coordinate system from cartesian to polar, so what is vertically stacked is converted into a circle
  coord_polar(theta = 'y') +
  facet_wrap(~constructor, nrow = 2) + 
  theme_ipsum() +
  theme(axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("clocks.png", width = 30, height = 20, units = "cm")