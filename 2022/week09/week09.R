# 0. Library and fonts management
library(tidyverse)
library(ggplot2)
library(showtext)
library(glue)
library(ggtext)
library(circlize)
library(ggplotify)
library(grid)
library(cowplot)
library(patchwork)
library(countrycode)
library(rnaturalearth)
library(sf)
library(rmapshaper)
library(colorspace)
library(ggimage)
library(ggforce)
library(scales)

## Adding Google Fonts
sysfonts::font_add_google(name = "Merriweather", family = "merriweather") ### Serif
serif <- "merriweather"
sysfonts::font_add_google(name = "Raleway", family = "raleway") ### Sans Serif
sans <- "raleway"

## Defines some layout constants
waterclr <- "#63A7D4"

# 1. Data download, load and handling
rawdata <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')

## Gets the abbreviation list and corrects some exceptions
abbreviation <- tibble(
  abbrev = unique(c(rawdata$sending_country_code, rawdata$receiving_country_code)),
  country = countrycode::countrycode(abbrev,
                                     origin = "iso2c",
                                     destination = "iso.name.en")
) %>% 
  dplyr::mutate(country = case_when(abbrev == "UK" ~ "United Kingdom",
                                    abbrev == "EL" ~ "Greece",
                                    abbrev == "XK" ~ "Kosovo",
                                    TRUE ~ country),
                name = case_when(country == "Czechia" ~ "Czech Rep.",
                                 country == "Bosnia and Herzegovina" ~ "Bosnia and Herz.",
                                 country == "Netherlands (the)" ~ "Netherlands",
                                 country == "North Macedonia" ~ "Macedonia",
                                 country == "Moldova (the Republic of)" ~ "Moldova",
                                 country == "Russian Federation (the)" ~ "Russia",
                                 country == "Palestine, State of" ~ "Palestine",
                                 TRUE ~ country)
  )

## Groups countries according to the statistical regions
## defined by the UN Statistics Division and some extras
## https://en.wikipedia.org/wiki/Eastern_Europe#/media/File:Europe_subregion_map_UN_geoscheme.svg
grouping <- tibble(
  abbrev = c("TG","PS","DZ","MA","IL",
             "EG","LB","TN","JO","AZ",
             "AM","CY",
             
             "IS","NO","SE","FI","IE",
             "DK","LV","LT","EE","UK",
             
             "ME","XK","BA","AL","RS",
             "MK","EL","MT","PT","HR",
             "SI","ES","TR","IT",
             
             "BY","RU","MD","UA","GE",
             "BG","SK","PL","HU","CZ",
             "RO",
             
             "LI","AT","NL","LU","BE",
             "DE","FR"),
  group = c(rep("Africa and Asia",12),
            rep("Northern Europe",10),
            rep("Southern Europe",14),
            rep("Eastern Europe",11),
            rep("Western Europe",7))
)

## Unites the grouping and abbreviation tibbles
sets <- dplyr::left_join(abbreviation, grouping)

## Defines the colors of the regions used to group the countries
# mycolor <- cols4all::c4a("misc.kelly", n = 9)
# mycolor <- mycolor[c(3,5,6,7,9)]
mycolor <- c("#977036", "#dc48c9", "#e55700", "#4c0518", "#ff2267")
colorgroups <- tibble(
  group = sort(unique(sets$group)),
  color = mycolor
)

## Gets the shapes of the countries
shapes <- ne_countries(scale = 50, "countries", returnclass = "sf")

## Simplifies the shapes (greatly reduces object memory size)
geom <- sf::st_geometry(shapes) %>% 
  rmapshaper::ms_simplify(keep_shapes = TRUE)
sf::st_geometry(shapes) <- geom

## Gets the shapes of the countries of interest
shapes_cnt <- shapes %>% 
  dplyr::full_join(sets, by = "name") %>% 
  dplyr::filter(name %in% sets$name) %>% 
  dplyr::select(country, group, geometry) %>% 
  dplyr::left_join(colorgroups)

## Only keeps meeting activities data and
## calculates the number of participants by country
df <- rawdata %>% 
  dplyr::filter(activity_mob != "Youth Exchanges - Programme Countries") %>% 
  dplyr::select(activity_mob, sending_country_code, receiving_country_code, participants) %>% 
  dplyr::group_by(activity_mob, sending_country_code, receiving_country_code) %>% 
  dplyr::summarise(participants = sum(participants)) %>% 
  dplyr::ungroup()

## Applies the grouping and de-abbreviation
df <- df %>% 
  dplyr::left_join(sets, by = c("receiving_country_code" = "abbrev")) %>% 
  dplyr::select(-receiving_country_code) %>% 
  dplyr::rename_with(.fn = ~glue::glue("receiving_{.}"),
                     .cols = c("country","group")) %>% 
  dplyr::left_join(sets, by = c("sending_country_code" = "abbrev")) %>% 
  dplyr::select(-sending_country_code) %>% 
  dplyr::rename_with(.fn = ~glue::glue("sending_{.}"),
                     .cols = c("country","group"))

## Calculates the number of participants in national meetings by host country
## and only keeps the top countries 
nation <- df %>% 
  dplyr::filter(activity_mob == "National youth meetings") %>% 
  dplyr::group_by(receiving_country) %>% 
  dplyr::summarise(participants = sum(participants),
                   group = unique(receiving_group)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(participants)) %>% 
  dplyr::mutate(pct = round(100*cumsum(participants)/sum(participants), 1)) %>% 
  dplyr::filter(lag(pct, default = 0) <= 75) %>% 
  dplyr::left_join(colorgroups) %>% 
  dplyr::mutate(receiving_country = factor(receiving_country),
                receiving_country = forcats::fct_inorder(receiving_country))

## Calculates the number of participants in
## international meetings by sending and receiving country
inter <- df %>% 
  dplyr::filter(activity_mob == "Transnational youth meetings") %>% 
  dplyr::group_by(sending_group, receiving_group) %>% 
  dplyr::summarise(participants = sum(participants)) %>% 
  dplyr::ungroup()

## Creates a list of the countries by region
cnt_list <- sets %>% 
  dplyr::group_by(group) %>% 
  dplyr::summarise(country = paste(sort(name), collapse = " #")) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(colorgroups) %>% 
  dplyr::mutate(group = glue::glue("<span style='color:{color};'>**{group}:**</span>")) %>% 
  dplyr::select(-color) %>% 
  tidyr::unite(label, everything(), sep = " #") %>%
  dplyr::summarise(label = paste(label, collapse = "<br><br>"))

## Defines the coordinates for the titles
titles <- tibble(
  x = c(rep(0.5,3),rep(0.05,2),rep(0.26,2),rep(0.4,2)),
  y = c(0.98,0.93,0.88,0.8,0.78,0.53,0.51,0.21,0.17),
  size = c(38,10,13,rep(c(15,8),3)),
  hjust = c(rep(0.5,3),rep(0,2),rep(0,2),rep(1,2)),
  label = c('**Hallo! Hej! Bok! Moi!**',
            'ERASMUS is an European Union programme for education, training, youth and sport.<br>
            Students that take part on it may move to institutions (mostly) across Europe.<br>
            Two of the many mobility activities are **national** and **transnational** in nature.',
            'Data from: Data.Europa | Graphic by: Ícaro Bernardes (@IcaroBSC)',
            
            '**COUNTRIES REGIONS**',
            'All countries to which the sending and receiving institutions<br>
            belong are listed bellow according to their region.',
            
            '**NATIONAL MEETINGS**',
            'Most participants in national meetings come from institutions in the hosting country.<br>
            The bar chart on the left shows the number of participants by receiving country.<br>
            These countries amount to 75% of all participants in national meetings.<br>
            The top 2, Germany and Poland, receive a staggering number of students.<br>
            It is also worth of note the number of visitors that come to<br>
            the relatively low populated Latvia and Lithuania.',
            
            '**TRANSNATIONAL<br>MEETINGS**',
            'Most students that participate<br>
            in transnational meetings stay<br>
            within the same region. Western<br>
            and Southern Europe send and<br>
            receive students the most.')
)

# 2. Generates the plot
## Defines some parameters for base plots (like circlize ones)
circos.clear()
par(
  mar = rep(0,4), ### Margin around chart
  bg = NA ### Background color
) 

## Makes the chord plot
chordDiagram(
  x = inter, 
  grid.col = mycolor,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"), 
  diffHeight  = -0.04,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE)

## Adds axis
circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    circos.axis(
      h = "top",
      labels.cex = 1.1
      )
  }
)

## Gets the plot from the device and saves 
## it in an ggplot-friendly format
circle <- recordPlot()
circle <- ggplotify::as.ggplot(cowplot::ggdraw(circle))

## Allows the use of the downloaded Google Font.
## Causes error if comes before the circular plot
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

## Creates the bar plot
bars <- nation %>% 
  ggplot() +
  
  ### Places the bars
  geom_bar(aes(x = receiving_country, y = participants, fill = I(color)),
           stat = "identity") +
  
  ### Places label for the countries and amount of visitors
  geom_text(aes(x = receiving_country, y = 0, label = receiving_country),
            color = "white", hjust = 0, angle = 90,
            family = serif, size = 8.5, nudge_y = 500, nudge_x = -0.05) +
  geom_text(aes(x = receiving_country, y = participants, color = I(color),
                label = scales::label_number_si()(participants)),
            vjust = 0, family = serif, size = 6, nudge_y = 500) +
  
  ### Eliminates unnecessary details
  theme_void()

## Creates the map
worldcolor <- "#cbd6d6"
map <- shapes %>% 
  ggplot() +
  
  ### Places countries in general
  geom_sf(fill = worldcolor, color = worldcolor) +
  
  ### Places the countries of interest
  geom_sf(aes(fill = I(color)), color = worldcolor, data = shapes_cnt) +
  
  ### Defines limits for the plot and applies the Mercator projection (default)
  coord_sf(xlim = c(-45,85), ylim = c(2,78)) +
  
  ggforce::geom_ellipse(aes(x0 = 20, y0 = 40, a = 87, b = 55, angle = 0),
                        size = 70, fill = NA,
                        color = colorspace::lighten(waterclr, 0.7)) +
  
  ### Eliminates unnecessary details and customizes the plot
  theme_void() +
  theme(
    panel.background = element_rect(fill = waterclr, color = NA)
  )

## Creates the main plot
p <- ggplot(NULL) + 
  
  ### Places the titles
  ggtext::geom_richtext(aes(x = x, y = y, label = label,
                            size = I(size), hjust = hjust),
                        vjust = 1, fill = NA, label.colour = NA,
                        family = sans, data = titles) +
  
  ### Places lines and stars to separate sections
  annotate("segment", x = 0.05, xend = 0.95, y = 0.83, yend = 0.83,
           color = "#E4BD00", size = 8, lineend = "round") +
  annotate("point", x = 0.5, y = 0.83, size = 50,
           color = colorspace::lighten(waterclr, 0.7)) +
  ggimage::geom_image(aes(x = 0.5, y = 0.83,
                          image = "https://cdn-icons-png.flaticon.com/512/197/197615.png"),
                      size = 0.05, by = "width", asp = 0.55) +
  
  ### Places the countries groups list
  ggtext::geom_textbox(aes(x = 0.05, y = 0.67, label = label), lineheight = 1.2,
                       size = 6, hjust = 0, box.colour = NA, fill = NA, 
                       width = unit(8, "inch"), family = sans, data = cnt_list) +
  
  ### Defines unitary plots limits
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
  
  ### Eliminates and customizes elements on the plot
  theme_void() +
  theme(
    panel.background = element_rect(fill = colorspace::lighten(waterclr, 0.7), color = NA)
  ) +
  
  ### Places the map, bars and chord diagram
  inset_element(map, left = 0.52, right = 0.95,
                bottom = 0.57, top = 0.77) +
  inset_element(bars, left = 0.05, right = 0.55,
                bottom = 0.3, top = 0.56) +
  inset_element(circle, left = 0.45, right = 0.95,
                bottom = 0.02, top = 0.3)

## Saves the plot
ggsave("2022/week09/erasmus.png", plot = p, dpi = "retina",
       width = 18, height = 33)

