# 0. Library management
library(tidyverse)
library(ggplot2)
library(showtext)
library(tidytuesdayR)
library(glue)

## Adding Google Fonts
font_add_google(name = "Cormorant Garamond", family = "cormorant") ### Sans Serif
serif <- "cormorant"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_auto()

# 1. Data download, load and handling
raw <- tidytuesdayR::tt_load('2022-01-11')$stressor
df <- raw %>% 
  dplyr::filter(year != "5/", state == "North Dakota") %>% 
  dplyr::mutate(months = factor(months, levels = c("January-March",
                                                   "April-June",
                                                   "July-September",
                                                   "October-December")),
                stressor = factor(stressor),
                year = as.numeric(year)) %>%
  tidyr::complete(year, months, stressor) %>% 
  dplyr::arrange(stressor, months) %>% 
  dplyr::mutate(i = glue::glue("{stressor}_{months}"),
                i = factor(i, levels = unique(i)),
                i = as.numeric(i),
                j = factor(year),
                j = as.numeric(j),
                group = 1:n()) %>% 
  dplyr::mutate(
    x1 = i*(sqrt(3)/2),
    y1 = (((i+1) %% 2)*1.5) + (j-1)*3,
    x2 = x1 - sqrt(3)/2,
    y2 = y1 + 0.5,
    x3 = x2,
    y3 = y2 + 1,
    x4 = x3 + sqrt(3)/2,
    y4 = y3 + 0.5,
    x5 = x4 + sqrt(3)/2,
    y5 = y4 - 0.5,
    x6 = x5,
    y6 = y5 - 1,
    x7 = x1,
    y7 = y1
  ) %>% 
  tidyr::pivot_longer(
    cols = c(matches("^(x|y)[[:digit:]]")),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)"
  )


aux <- df %>% 
  group_by(year, stressor) %>% 
  arrange(j,i,set) %>%
  mutate(i = ifelse(row_number() %in% 15:21, max(i)+0.5, i)) %>% 
  arrange(j,i,set) %>%
  slice(c(1:3,9:11,17:20,26:27,22:23,1)) %>% 
  mutate(group = cur_group_id()) %>% 
  ungroup()


df %>%
  ggplot(aes(x = x, y = y)) +
  geom_polygon(aes(group = group, fill = stress_pct), color = "red", size = 2) +
  geom_polygon(aes(group = group), fill = NA, color = "white", size = 2, data = aux) +
  coord_equal()

## Selects the relevant variables
df <- df %>% 
  dplyr::select(track_name, track_number, album_name, duration_ms,
                danceability, energy, speechiness, liveness, valence)

## Creates the tracks strips on the CDs
strips <- df %>% 
  dplyr::group_by(album_name) %>% 
  dplyr::mutate(angle = cumsum(duration_ms),
                angle = scales::rescale(angle, to = c(-pi/2,-3*pi/2), from = c(0,max(angle)))) %>% 
  dplyr::mutate(end = angle,
                start = dplyr::lag(end, default = -pi/2)) %>% 
  dplyr::ungroup()

## Creates the marks to identify the tracks
marks <- strips %>% 
  dplyr::transmute(album_name = album_name,
                   y = 7*cos(start),
                   x = 7*sin(start))

## Creates the labels for the tracks
labels <- strips %>% 
  dplyr::mutate(angle = units::set_units(start, rad),
                angle = units::set_units(angle, degrees)) %>% 
  dplyr::transmute(album_name = album_name,
                   track_name = stringr::str_wrap(track_name, 16),
                   track_name = stringr::str_replace_all(track_name, "\\n", "<br>"),
                   angle = -1*as.numeric(angle),
                   y = 7.5*cos(start),
                   x = 7.5*sin(start)) %>% 
  dplyr::mutate(hjust = ifelse(x <= 0, 1, 0),
                angle = ifelse(x <= 0, angle-90, angle-270))

## Creates coordinates for the CD's covers
cds <- df %>% 
  dplyr::distinct(album_name) %>% 
  dplyr::mutate(image = paste0("2021/week51/covers/",album_name,".png"))

# 2. Plot creation
lnhgt <- 0.3

## Creates the inset plot
inset <- strips %>% 
  ggplot() +
  
  ### Places the CDs titles and covers
  geom_text(aes(x = 0, y = 11, label = album_name),
            color = "white", fontface = "bold", family = serif,
            vjust = 1, size = 30, data = cds) +
  ggimage::geom_image(aes(x = 0, y = 0, image = image), size = 0.363,
                      by = "width", asp = 1, data = cds) +
  
  ### Places the tracks characteristics
  #### speechiness
  ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 1, r = 2, fill = speechiness,
                            start = start, end = end), color = NA) +
  scale_fill_gradient(low = "white", high = "#0000ff", limits = c(0,1)) +
  ggnewscale::new_scale_fill() +
  #### energy
  ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 2, r = 3, fill = energy,
                            start = start, end = end), color = NA) +
  scale_fill_gradient(low = "white", high = "#ffa500", limits = c(0,1)) +
  ggnewscale::new_scale_fill() +
  #### danceability
  ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 3, r = 4, fill = danceability,
                            start = start, end = end), color = NA) +
  scale_fill_gradient(low = "white", high = "#ff1493", limits = c(0,1)) +
  ggnewscale::new_scale_fill() +
  #### liveness
  ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 4, r = 5, fill = liveness,
                            start = start, end = end), color = NA) +
  scale_fill_gradient(low = "white", high = "#00ced1", limits = c(0,1)) +
  ggnewscale::new_scale_fill() +
  #### valence
  ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 5, r = 6, fill = valence,
                            start = start, end = end), color = NA) +
  scale_fill_gradient(low = "white", high = "#08d108", limits = c(0,1)) +
  
  ### Places the tracks marks and labels
  geom_point(aes(x = x, y = y), size = 0.5, color = "white", data = marks) +
  geom_segment(aes(x = x, y = y, xend = 0, yend = 0),
               color = "white", size = 0.1, data = marks) +
  ggtext::geom_richtext(aes(x = x, y = y, label = track_name,
                            hjust = hjust, angle = angle), color = "white",
                        family = serif, size = 9, lineheight = lnhgt,
                        fill = NA, label.colour = NA, data = labels) +
  
  ### Places the "disk hole"
  geom_point(aes(x = 0, y = 0), color = "#1f1f1f", size = 6) +
  
  ### Establishes the limits of the plot
  xlim(-15,15) + ylim(-15,15) +
  
  ### Facets the plot by column
  facet_wrap(~album_name, strip.position = "top", ncol = 1) +
  
  theme_void() +
  theme(
    aspect.ratio = 1,
    legend.position = "none",
    strip.text = element_blank(),
    panel.spacing = unit(0, "lines")
  )

## Creates the titles
titles <- tibble(
  x = 0,
  y = c(4,3.55,0.27),
  size = c(80,26,20),
  label = c(
    "**Spicy Content**",
    "The Spice Girls made three studio albuns.<br>The heatmaps on the right show five features<br>of the CD's tracks which are explained bellow<br>(from the outermost arc to the innermost).<br>Darker colors mean higher values:",
    "Data: Jacquie Tran through Spotify and Genius<br>Graphic: Ícaro Bernardes | @IcaroBSC<br>CD's covers: Virgin Records"
  )
)

## Creates the features explanations
features <- tibble(
  x = 0.1,
  y = seq(2.5,0.58,-0.48),
  size = 25,
  fill = c("#08d108","#00ced1","#ff1493","#ffa500","#0000ff"),
  label = c(
    "**Valence:** How happy, cheerful the<br>feeling conveyed by a music is;",
    "**Liveness:** How likely is that an<br>audience is in the recording;",
    "**Danceability:** How suitable<br>a track is for dancing;",
    "**Energy:** Gives an idea of<br>how intense a song is;",
    "**Speechiness:** Detects the presence<br>of spoken words in a track."
  )
)


## Creates coordinates for the icons and their surrounding points
icons <- tibble(
  y = features$y,
  image = paste0("2021/week51/icons/",
                 c("valence","liveness","danceability","energy","speechiness")
                 ,".png"),
  color = c("#08d108","#00ced1","#ff1493","#ffa500","#0000ff")
)

## Creates the main plot
p <- titles %>% 
  ggplot() +
  
  ### Places the titles
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        label.colour = NA, fill = NA, lineheight = lnhgt + 0.05,
                        hjust = 0, vjust = 1, color = "white", family = serif) +
  
  ### Places the features explanations
  ggtext::geom_textbox(aes(x = x, y = y, label = label, size = I(size), fill = I(fill)),
                       box.padding = unit(c(5.5, 5.5, 5.5, 60), "pt"),
                       width = unit(0.53,"npc"), height = unit(0.085,"npc"),
                       hjust = 0, vjust = 0.5, valign = 0, family = serif,
                       box.colour = NA, lineheight = lnhgt + 0.05,
                       text.colour = "white", data = features) +
  
  ### Places the icons and their surrounding points
  geom_point(aes(x = 0.2, y = y, color = I(color)), size = 30, data = icons) +
  ggimage::geom_image(aes(x = 0.2, y = y, image = image), size = 0.05,
                      by = "width", asp = 1, data = icons) +
  
  ### Creates a rectangle to highlight the CD's
  annotate("ribbon", x = c(2.8,Inf), ymin = -Inf, ymax = Inf,
           fill = "#1f1f1f", color = NA) +
  
  ### Establishes the limits of the plot
  xlim(0,4) + ylim(0,4) +
  
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black", color = NA)
  ) +
  
  ### Inserts the inset plot
  patchwork::inset_element(inset,
                           left = 0.345, right = 1.345,
                           bottom = 0, top = 1)

## Saves the plot
ggsave("2021/week51/spice.png", plot = p, dpi = "retina",
       width = 10, height = 10)

