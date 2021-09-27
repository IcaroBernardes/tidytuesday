# 0. Library management
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(showtext)
library(ggtext)
library(ggdist)

## Add Google Font
font_add_google(name = "Merriweather", family = "merri")

# 1. Data download
tuesdata <- tidytuesdayR::tt_load('2021-09-21')
emmy <- tuesdata$nominees

## Obtains which are the top 75% distributors (by nomination) from 2015 to the present
big <- emmy %>% 
  dplyr::filter(year >= 2015) %>% 
  dplyr::filter(type == "Winner") %>% 
  dplyr::select(category,title,distributor) %>% 
  distinct() %>% 
  count(distributor) %>% 
  arrange(desc(n), distributor) %>% 
  dplyr::mutate(pct = round(100*cumsum(n)/sum(n))) %>% 
  dplyr::filter(pct <= 75)

# 2. How distributed between shows are the HBO and Netflix awards?
dependency <- emmy %>% 
  dplyr::filter(year >= 2015) %>% 
  dplyr::filter(type == "Winner") %>% 
  dplyr::filter(str_detect(distributor, "HBO|Netflix")) %>% 
  dplyr::mutate(distributor = ifelse(str_detect(distributor, "HBO"), "HBO", "Netflix")) %>% 
  dplyr::select(category,year,title,distributor) %>% 
  dplyr::distinct() %>% 
  count(year,title,distributor) %>% 
  dplyr::group_by(year,distributor) %>% 
  dplyr::mutate(pct = round(100*n/sum(n))) %>% 
  ungroup()

## Calculates how many categories were disputed in each year
prizes <- emmy %>% 
  dplyr::filter(year >= 2015) %>% 
  dplyr::filter(type == "Winner") %>% 
  dplyr::select(year,category) %>% 
  dplyr::distinct() %>% 
  count(year, name = "prizes")

## Calculates the overall prizes each distributor won each year
overall <- dependency %>% 
  dplyr::group_by(year,distributor) %>%
  dplyr::summarise(total = sum(n)) %>% 
  ungroup() %>% 
  dplyr::mutate(pos = 1.5*(as.numeric(factor(distributor))-1)) %>% 
  left_join(prizes) %>% 
  dplyr::mutate(ratio = total/prizes,
                yearmax = year+ratio)

## Creates data for ticks for the y-axis
pairs <- interaction(2015:2021, c(1,5,10,15))
pairs <- levels(pairs)
ticks <- str_split_fixed(pairs, "\\.", 2)
ticks <- as.data.frame(ticks)
ticks <- ticks %>% 
  dplyr::mutate(across(.fns = as.numeric))

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext_auto()

p <- dependency %>% 
  ggplot() +
  ggdist::geom_dots(aes(x = year, y = n, fill = distributor), color = NA) +
  ## Title and subtitles
  annotate("text", x = 2014, y = 35, hjust = 0, vjust = 1, family = "merri", size = 60, fontface = "bold.italic",
           label = "Showpendency") +
  annotate("text", x = 2014, y = 31, hjust = 0, vjust = 1, family = "merri", size = 20, lineheight = 0.3, parse = TRUE,
           label = "phantom('HBO') *' and '* phantom('Netflix') *' have been the top distributors (in nominations and wins) in the Emmy Awards in the recent years.'") +
  annotate("text", x = 2014, y = 31, hjust = 0, vjust = 1, family = "merri", size = 20, lineheight = 0.3, parse = TRUE, color = "#3382c4",
           label = "'HBO'* phantom(' and ') * phantom('Netflix') * phantom('have been the top distributors (in nominations and wins) in the Emmy Awards in the recent years.')") +
  annotate("text", x = 2014, y = 31, hjust = 0, vjust = 1, family = "merri", size = 20, lineheight = 0.3, parse = TRUE, color = "#e50913",
           label = "phantom('HBO') * phantom(' and ') *'Netflix'* phantom('have been the top distributors (in nominations and wins) in the Emmy Awards in the recent years.')") +
  ggtext::geom_richtext(aes(x = 2014, y = 29.7), hjust = 0, vjust = 1, family = "merri", size = 20,
                        fill = NA, label.color = NA,  label.padding = grid::unit(rep(0, 4), "pt"),
                        label = "How the **concentration** of wins in some shows affects their perfomance?") +
  annotate("text", x = 2014, y = 28.3, hjust = 0, vjust = 1, family = "merri", size = 10, lineheight = 0.3,
           label = "Source: emmys.com | Graphic: Ícaro Bernardes (https://github.com/IcaroBernardes/tidytuesday/tree/main/week39)") +
  ## Overall performance
  geom_rect(aes(xmin = year, xmax = yearmax, ymin = 20+pos, ymax = 21+pos, fill = distributor), data = overall) +
  geom_text(aes(x = yearmax+0.1, y = 20.5+pos, label = total, color = distributor), data = overall, family = "merri", size = 15) +
  ## Total of categories for each year
  geom_rect(aes(xmin = year, xmax = year+0.98, ymin = 18.5, ymax = 19.5), fill = "gray", data = prizes) +
  geom_text(aes(x = year+0.95, y = 19, label = prizes), color = "white", data = prizes, family = "merri", size = 13, hjust = 1) +
  ## Y-axis labels and titles
  annotate("text", x = 2014.95, y = c(1,5,10,15), label = c(1,5,10,15), family = "merri", size = 18, hjust = 1) +
  annotate("text", x = 2014.95, y = 21.25, label = "Overall\nwins", hjust = 1, vjust = 0.5, family = "merri", size = 25, lineheight = 0.25) +
  annotate("text", x = 2014.95, y = 19, label = "Nº of categories\nin the year", hjust = 1, vjust = 0.5, family = "merri", size = 15, lineheight = 0.25) +
  annotate("text", x = 2014.5, y = 8, label = "Wins per program", angle = 90, family = "merri", size = 25) +
  ## X-axis labels and titles
  annotate("text", x = 2015:2021, y = -1, label = 2015:2021, family = "merri", size = 18) +
  annotate("text", x = 2021.5, y = -2, hjust = 0, vjust = 1, label = "Year", family = "merri", size = 25) +
  ## Major x-axis grid lines
  geom_segment(aes(x = year, xend = year, y = 0, yend = 23)) +
  ## Major y-axis ticks
  geom_text(aes(x = V1-0.02, y = V2, label = "◀"), size = 12, data = ticks) +
  ## Insights and arrows
  ggtext::geom_richtext(aes(x = 2015.3, y = 15, family = "merri",
                            label = "Each circle represents one program.<br>The number of wins is represented in the y-axis.<br>In 2015, Game of Thrones won in 12 categories for HBO."),
                        hjust = 0, vjust = 0.5, label.colour = NA, text.colour = "black", fill = "white", size = 12, lineheight = 0.3) +
  annotate("curve", x = 2015.3, xend = 2015.05, y = 15, yend = 12.5,
           arrow = arrow(length = unit(0.007, "npc")), curvature = 0.3) + 
  annotate("text", x = 2018, y = 25, family = "merri", size = 12, lineheight = 0.3,
           label = "Netflix sprang from a supporting role to the lead in a few years\nMeanwhile, HBO performed inconsistently.") +
  annotate("curve", x = 2016.55, xend = 2015.3, y = 25, yend = 22,
           arrow = arrow(length = unit(0.007, "npc")), curvature = 0.2) + 
  annotate("curve", x = 2019.44, xend = 2021.2, y = 25, yend = 23,
           arrow = arrow(length = unit(0.007, "npc")), curvature = -0.1) +
  geom_rect(aes(xmin = 2017.9, xmax = 2020.4, ymin = 12.9, ymax = 14.6), fill = "white") +
  geom_rect(aes(xmin = 2019.74, xmax = 2020.41, ymin = 13.7, ymax = 14.3), fill = "#3382c4") +
  geom_rect(aes(xmin = 2019.58, xmax = 2020.08, ymin = 13.1, ymax = 13.7), fill = "#e50913") +
  annotate("curve", x = 2020.45, xend = 2020.13, y = 14, yend = 10.9,
           arrow = arrow(length = unit(0.007, "npc")), curvature = -0.9) +
  annotate("curve", x = 2019.85, xend = 2019.5, y = 13, yend = 2.5,
           arrow = arrow(length = unit(0.007, "npc")), curvature = -0.1) +
  annotate("text", x = 2020.4, y = 14, family = "merri",
           hjust = 1, vjust = 0.5, size = 12, lineheight = 0.3, parse = TRUE,
           label = "'While HBO winners are a handful of ' * phantom(' heavy-hitters,')") +
  annotate("text", x = 2020.4, y = 14, family = "merri", parse = TRUE,
           hjust = 1, vjust = 0.5, size = 12, lineheight = 0.3, color = "white", fontface = "bold",
           label = "phantom('While HBO winners are a handful of ') * ' heavy-hitters,'") +
  annotate("text", x = 2020.4, y = 13.4, family = "merri",
           hjust = 1, vjust = 0.5, size = 12, lineheight = 0.3, parse = TRUE,
           label = "'Netflix has many shows that snatch ' * phantom(' one or two ') * ' prizes.'") +
  annotate("text", x = 2020.4, y = 13.4, family = "merri", color = "white", fontface = "bold",
           hjust = 1, vjust = 0.5, size = 12, lineheight = 0.3, parse = TRUE,
           label = "phantom('Netflix has many shows that snatch ') * ' one or two ' * phantom(' prizes.')") +
  scale_x_continuous(breaks = 2015:2021) +
  scale_y_continuous(limits = c(-3,35)) + 
  scale_fill_manual(values = c("#3382c4","#e50913"), breaks = c("HBO","Netflix")) +
  scale_color_manual(values = c("#3382c4","#e50913"), breaks = c("HBO","Netflix")) +
  theme_ipsum() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

## Saves the plot
ggsave("week39/showpendency.png", plot = p, width = 40, height = 25, units = "cm")

