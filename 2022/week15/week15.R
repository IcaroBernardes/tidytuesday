# 0. Library and fonts management
library(tidyverse)
library(ggtext)
library(countrycode)
library(scales)
library(ggbeeswarm)
library(ggrepel)
library(ggpubr)
library(colorspace)
library(patchwork)

## Defines some layout constants
lnhgt <- 1.2

# 1. Data download, load and handling
rawdata <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/fuel_access.csv')

## Eliminates aggregates of countries and lines with absent data
df <- rawdata %>%
  na.exclude() %>% 
  dplyr::filter(Entity != "World")

## Keeps only the first and last year
df <- df %>% 
  dplyr::filter(Year %in% range(Year))

## Changes the names of two countries
df <- df %>% 
  dplyr::mutate(Entity = case_when(Entity == "Micronesia (country)" ~ "Micronesia (Federated States of)",
                                   Entity == "Timor" ~ "East Timor",
                                   TRUE ~ Entity))

## Selects and renames the variables
df <- df %>% 
  dplyr::select(country = Entity,
                year = Year,
                pct = "Access to clean fuels and technologies for cooking  (% of population)")

## Adds the info of the 7 regions as defined
## in the World Bank Development Indicators
df <- df %>% 
  dplyr::mutate(region = countrycode::countrycode(country,
                                                  origin = "country.name",
                                                  destination = "region"))

## Converts the year to ordinals
df <- df %>% 
  dplyr::arrange(year) %>% 
  dplyr::mutate(year = factor(year),
                year = as.numeric(year))

## Places a line break in the names of the regions
df <- df %>% 
  dplyr::mutate(region = stringr::str_wrap(region, width = 15))

## Defines colors for the points and lightens most of them
## according to how well the regions perform
clr <- cols4all::c4a(palette = "misc.okabe", n = 7)
light <- df %>% 
  dplyr::filter(year == 2) %>% 
  dplyr::group_by(region) %>% 
  dplyr::summarise(pct = mean(pct)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(region) %>% 
  dplyr::mutate(pct = scales::rescale(pct, to = c(0.1,0.7))) %>% 
  dplyr::pull(pct)
clr <- colorspace::lighten(clr, amount = light)

## Gets some countries to highlight
worst_perf <- df %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(pct = mean(pct)) %>% 
  dplyr::arrange(pct) %>% 
  dplyr::slice(1L:3L) %>% 
  dplyr::pull(country)
big_inc <- df %>% 
  dplyr::group_by(country) %>% 
  dplyr::arrange(country, desc(year)) %>% 
  dplyr::summarise(pct = pct[2]-pct[1]) %>% 
  dplyr::arrange(pct) %>% 
  dplyr::slice(1L) %>% 
  dplyr::pull(country)
highlight <- c(worst_perf, big_inc, "Brazil", "United States", "Denmark", "China")

## Defines the title and subtitle
toptext <- c(
  "<span style='font-size:38px; color:red;'>Sub-Saharan countries are the most plagued by indoor air pollution</span><br><br><br>
  <span style='font-size:23px;'>Population with access to clean fuels and technologies for cooking, %</span>")

## Defines coordinates for the x-axis labels
xlb <- tibble(
  x = 1:2,
  y = -6,
  label = c(2000,2016)
)

## Defines coordinates for the y-axis labels
ylb <- tibble(
  x = 0.5,
  y = seq(0, 100, 20)
)

## Defines coordinates for the caption
caption <- tibble(
  x = c(0.5, 2.5),
  y = -10,
  hjust = c(0,1),
  label = c(
    "Source: Our World in Data",
    "Graphic by: Ícaro Bernardes (@IcaroBSC)"
  )
)

## Extracts the legend for the plot
leg <- df %>% 
  ggplot(aes(x = year, y = pct, color = region)) +
  geom_point(shape = 21, fill = "white", stroke = 1.5, size = 4) +
  scale_color_discrete(type = clr) +
  guides(color = guide_legend(nrow = 1)) +
  theme(
    legend.position = "top",
    legend.key = element_blank(),
    legend.spacing.x = unit(5, "mm"),
    legend.title = element_blank(),
    legend.text = element_text(
      size = 15, lineheight = lnhgt,
      vjust = 1, family = "Verdana",
      margin = ggplot2::margin(t = 22, r = 0, b = 0, l = 0)
    )
  )
leg <- leg %>% 
  ggpubr::get_legend() %>% 
  ggpubr::as_ggplot()

## Sets a seed for the random placement of points
seed <- 42
set.seed(seed)

## Creates the main plot
p <- df %>% 
  ggplot() +
  
  ### Places the points
  ggbeeswarm::geom_quasirandom(aes(x = year, y = pct, color = region, group = year),
                               shape = 21, stroke = 1.5, size = 4, fill = "white") +
  
  ### Places the title and red decoration on top
  annotate("rect", xmin = 0.5, xmax = 0.6, ymin = 138, ymax = 140, fill = "red") +
  annotate("linerange", xmin = 0.5, xmax = 2.5, y = 138, color = "red") +
  ggtext::geom_richtext(aes(x = 0.5, y = 135, label = toptext),
                        family = "Verdana", lineheight = lnhgt,
                        label.color = NA, fill = NA,
                        fontface = "bold", hjust = 0, vjust = 1) +
  
  ### Places the labels for the x-axis and the support line
  geom_linerange(aes(xmin = 1, xmax = 2, y = -5)) +
  geom_linerange(aes(x = 1, ymin = -5, ymax = -4)) +
  geom_linerange(aes(x = 2, ymin = -5, ymax = -4)) +
  geom_text(aes(x = x, y = y, label = label), size = 5,
            family = "Verdana", vjust = 1, data = xlb) +
  
  ### Places the labels for the y-axis
  ggtext::geom_textbox(aes(x = x, y = y, label = y), hjust = 0, halign = 1,
                       size = 5, box.color = NA, fill = "white", vjust = 0.5,
                       family = "Verdana", width = unit(0.035, "npc"),
                       box.padding = unit(c(0, 0.01, 0, 0), "npc"),
                       box.r = unit(0, "pt"), data = ylb) +
  
  ### Places the caption bellow
  ggtext::geom_richtext(aes(x = x, y = y, label = label, hjust = hjust),
                        family = "Verdana", vjust = 0, size = 3.5,
                        color = "#575757", label.color = NA, fill = NA,
                        data = caption) +
  
  ### Places the labels of highlighted countries
  ggrepel::geom_text_repel(
    aes(x = year, y = pct, label = ifelse(country %in% highlight, country, "")),
    seed = seed, position = ggbeeswarm::position_quasirandom(),min.segment.length=0) +
  
  ### Applies the color to the points
  scale_color_discrete(type = clr, guide = "none") +
  
  ### Defines limits for the scales
  scale_y_continuous(limits = c(-10,140),
                     breaks = seq(0, 100, 20),
                     expand = expansion(0,0)) +
  scale_x_continuous(limits = c(0.5, 2.5),
                     breaks = c(1,2),
                     expand = expansion(0,0)) +
  
  ### Customize some plot elements
  theme(
    text = element_text(family = "Verdana"),
    
    plot.background = element_blank(),
    plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20),
    
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "#c4c4c4"),
    
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  
  ### Places the legend
  patchwork::inset_element(leg, left = 0, right = 0.79,
                           bottom = 0.7, top = 0.95)

## Saves the plot
ggsave("2022/week15/pollution.png", plot = p, dpi = "retina",
       width = 18, height = 11)

