# 0. Library and fonts management
library(tidyverse)
library(ragg)
library(santoku)
library(scales)
library(glue)
library(epoxy)
library(colorspace)
library(geomtextpath)
library(ggtext)
library(ggpath)
library(cowplot)

## Defines some layout constants
font_title <- "Ubuntu" ### Title text Google font
font_body <- "Teko" ### Body text Google font
lineheight <- 1.1 ### Height of text lines
angle_cat_small <- 4.3*pi ### End angle of the smallest value
angle_cat_big <- 5*pi ### End angle of the highest value
angle_base <- 4.15*pi ### Start angle of all categories
angle_body_start <- 3*pi ### Start angle of the bird's body
angle_body_end <- 5*pi ### End angle of the bird's body
size_minor <- 20 ### Minor size of the bird's body
size_major <- 70 ### Major size of the bird's body
angle_neck_start <- angle_body_start+0.2*pi ### Start angle of the bird's neck
angle_neck_end <- angle_body_start+0.5*pi ### End angle of the bird's neck
slope <- (size_major-size_minor)/(angle_neck_end-angle_neck_start) ### Slope of the neck size
intercept <- size_major - slope*angle_neck_end ### Intercept of the neck size
step <- (angle_cat_big-angle_base)/2000 ### Spiral construction step size
spi <- 1 ### Constant for the spirals

# 1. Data download, load and handling
slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
firsts_picks <- readr::read_csv2('2022/week25/first_picks.csv')

## Shows the year range of the data on arrivals of slave transport ships
range(slave_routes$year_arrival)

## Counts the number of enslaved people that arrived in the US through ships
## by period and corrects the labels so the range is shown
slavery <- slave_routes %>% 
  dplyr::mutate(period = santoku::chop(x = year_arrival,
                                       breaks = seq(1500,1900,100))) %>% 
  dplyr::group_by(period) %>% 
  dplyr::summarise(n = sum(n_slaves_arrived, na.rm = TRUE)) %>% 
  dplyr::mutate(period = forcats::fct_recode(period,
                                             `[1514, 1600)` = "[1500, 1600)",
                                             `[1800, 1866]` = "[1800, 1900)"),
                period = forcats::fct_rev(period))

## Creates a new tibble that contains the angle 
## in which the spiral of each category will end
feathers <- slavery %>% 
  dplyr::mutate(
    angle_end = scales::rescale(n, to = c(angle_cat_small,angle_cat_big))
  )

## Calculates the points to draw the spirals that will compose the feathers
feathers <- feathers %>% 
  dplyr::group_by(period) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(
    data = purrr::map(
      data,
      ~ tibble(
        angle_end = .$angle_end,
        fi = seq(angle_base, angle_end, by = step),
        id = as.numeric(period)-1,
        r = spi*fi + id,
        x = -r*cos(fi),
        y = -r*sin(fi)
      )
    )
  ) %>% 
  tidyr::unnest(cols = data) %>%
  dplyr::select(period, x, y)

## Creates the curved labels for the periods
feathers <- feathers %>% 
  dplyr::left_join(slavery) %>% 
  dplyr::mutate(period = as.character(period),
                period = glue::glue("{str_sub(period, 2L, 5L)} AND {as.numeric(str_sub(period, 8L, 11L))-1}"),
                label = glue::glue("PEOPLE BETWEEN {period}"))

## Creates the curved labels for the values
n_label <- slavery %>% 
  dplyr::group_by(period, n) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(
    data = purrr::map(
      data,
      ~ tibble(
        fi = seq(angle_body_start, angle_base-30*step, by = step),
        id = as.numeric(period)-1,
        r = spi*fi + id,
        x = -r*cos(fi),
        y = -r*sin(fi)
      )
    )
  ) %>% 
  tidyr::unnest(cols = data) %>%
  dplyr::select(period, n, x, y) %>% 
  dplyr::mutate(n = format(n, big.mark = "."))

## Defines coordinates for the message
message <- slavery %>% 
  dplyr::summarise(n = sum(n)) %>% 
  dplyr::mutate(
    n = format(n, big.mark = "."),
    x = -1,
    y = -2,
    label = glue::glue("<span style='font-size:70px;'>IT'S ESTIMATED THAT<br><span style='font-size:150px;color:red;'>**{n} PEOPLE**</span><br>WERE ENSLAVED AND BROUGHT TO THE US.</span>"))

## Calculates the points to draw the spiral that will compose the body
body <- tibble(
  fi = seq(angle_body_start, angle_body_end, by = step),
  id = mean(1:n_distinct(slavery$period))-1,
  r = spi*fi + id,
  x = -r*cos(fi),
  y = -r*sin(fi),
  size = case_when(angle_neck_start >= fi ~ size_minor,
                   ((angle_neck_start < fi) & (angle_neck_end >= fi)) ~ fi*slope + intercept,
                   TRUE ~ size_major)
) %>% 
  dplyr::select(x, y, size)

## Composes the bird's beak
beak_x <- body %>% dplyr::slice(1L) %>% dplyr::pull(x)
beak_y <- body %>% dplyr::slice(1L) %>% dplyr::pull(y)-0.1
beak_dx <- 0.55
beak_dy <- 1.6
beak <- tibble(
  xmin = beak_x + c(-1,0,-0.5,0.5)*beak_dx,
  xmax = beak_x + c(0,1,-0.5,0.5)*beak_dx,
  y = beak_y - c(0,0,1,1)*beak_dy,
  group = rep(1:2,2)
)

## Creates the bird's eye
eye <- body %>% 
  dplyr::slice(1L) %>% 
  dplyr::mutate(y = y+0.3,
                size = size/3)

## Defines coordinates for the year labels
yearly <- tibble(
  x = seq(1750, 2000, 50),
  y = 0.5
)

## Defines color for the achievements by category
firsts_cat <- tibble(
  color = c("#0053EB","#CFCB00","#EB4601",
            "#EBA000","#EB0076",
            "#00EBCB","#6E00EB","#01EB00"),
  category = c("Social & Jobs", "Religion", "Military",
               "Education & Science", "Arts & Entertainment",
               "Law", "Politics", "Sports")
)
firsts_picks <- firsts_picks %>% dplyr::left_join(firsts_cat)

## Defines the colors for the ground
ground <- firsts_picks %>% 
  dplyr::select(year, color) %>% 
  dplyr::mutate(color = colorspace::desaturate(color, 0.3)) %>% 
  tibble::add_row(year = c(1720,2020), color = "#d9d7d7") %>% 
  dplyr::arrange(year) 

## Defines coordinates for the bird's legs
legs <- tibble(
  x = c(0.43,0.465,
        0.50,0.535,
        0.454,0.44,
        0.524,0.51),
  y = c(0.18,0.08,
        0.18,0.08,
        0.11,0.08,
        0.11,0.08),
  group = c(1,1,
            2,2,
            3,3,
            4,4)
)

## Defines coordinates for the titles
titles <- tibble(
  x = c(0, rep(0.12,2), rep(0.44,2), rep(0.75,2), 0),
  y = c(1, rep(c(0.91,0.86),3), 0.78),
  size = c(50, rep(c(12,9), 3), 7),
  fontface = c("bold", rep(c("bold","plain"), 3), "bold"),
  label = c(
    
    "SANKOFA AND UBUNTU",
    
    "Seek wisdom<br>
    in the past",
    
    "Slavery is a burden shared<br>
    across the Atlantic with<br>
    long-lasting effects.",
    
    "to forge the<br>
    path ahead",
    
    "Despite the odds,<br>
    the African-Americans<br>
    accumulated achievements.",
    
    "and strengthen the<br>
    bond we share as humans",
    
    "There is no Democracy<br>
    without Equality.<br>
    This is everyone's fight!",
    
    "Data on enslaved people comes from the Slave Voyages project and data on African American Achievements comes from Wikipedia | Graphic by: Ícaro Bernardes (@IcaroBSC)"
    
  )
)

## Defines coordinates for the glyphs
glyphs <- tibble(
  x = c(0.11, 0.43, 0.74),
  y = 0.92,
  i = 1:3,
  path = glue::glue("2022/week25/images/{i}.png")
)

## Defines coordinates for the hope text
cat_vector <- firsts_cat %>%
  dplyr::mutate(category = glue::glue("<span style='color:{color};'>{toupper(category)}</span>")) %>% 
  dplyr::summarise(category = glue::glue("{category&}",
                                         .transformer = epoxy::epoxy_style_collapse(last_and = " AND "))) %>% 
  dplyr::pull(category)
hope <- tibble(
  x = 1870,
  y = 0,
  label = glue::glue("THIS GRUESOME PAST TAUGHT THE AFRICAN-AMERICAN PEOPLE THAT THEIR RIGHTS WOULD ONLY BE ATTAINED WITH PERSISTENCE. THROUGH THE YEARS, THEY CONQUERDED NEW SPACES IN AREAS SUCH AS {cat_vector}. THIS TIMELINE SHOWS SOME OF THE BARRIERS THEY BROKE.")
)

# 2. Generates the plot
## Creates the body of the bird
p1 <- ggplot(NULL) +
  
  ### Places the body
  ggforce::geom_link2(aes(x = x, y = y, size = I(size), group = 1),
                      color = "#d9d7d7", n = 1, lineend = 'round', data = body) +
  
  ### Places the feathers
  geom_path(aes(x = x, y = y, group = period), color = "black",
            lineend  = "round", size = 10, data = feathers) +
  
  ### Places the beak
  geom_ribbon(aes(xmin = xmin, xmax = xmax, y = y, group = group),
              fill = "#d9d7d7", color = NA, data = beak) +
  
  ### Places the eye
  geom_point(aes(x = x, y = y, size = I(size)), data = eye) +
  
  ### Places the curved labels for the periods
  geomtextpath::geom_textpath(aes(x = x, y = y, label = label), size = 6,
                              hjust = 0, family = font_body, text_only = TRUE,
                              color = "white", data = feathers) +
  
  ### Places the curved labels for the values
  geomtextpath::geom_textpath(aes(x = x, y = y, label = n), size = 8,
                              hjust = 1, family = font_body, text_only = TRUE,
                              color = "red", fontface = "bold", data = n_label) +
  
  ### Places the message
  ggtext::geom_textbox(aes(x = x, y = y, label = label), width = unit(6, "inch"),
                       box.color = NA, fill = NA, lineheight = lineheight,
                       color = "white", family = font_body, data = message) +
  
  ### Controls the proportions
  coord_equal(xlim = c(-15,18), ylim = c(-17,14)) +
  
  ### Eliminates theme elements
  theme_void()

## Creates the ground bellow the bird
p2 <- NULL %>% 
  ggplot() +
  
  ### Places the ground
  geom_path(aes(x = c(1720,2020), y = 0, group = 1),
            size = 40, lineend = "round", color = "#d9d7d7") +
  
  ### Places the hope text
  ggtext::geom_textbox(aes(x = x, y = y, label = label), fontface = "bold",
                       box.color = NA, fill = NA, lineheight = lineheight,
                       family = font_title, width = unit(19, "inch"),
                       size = 7, halign = 0.5, valign = 0.5, data = hope) +
  
  ### Places descriptions of the events
  ggtext::geom_textbox(aes(x = year, y = -0.4, label = pioneers, color = I(color)), 
                       width = unit(4, "inch"), size = 5,
                       hjust = 0, halign = 0, orientation = "right-rotated",
                       box.color = NA, fill = NA, lineheight = lineheight,
                       family = font_body, data = firsts_picks) +
  
  ### Places the labels of the years
  geom_text(aes(x = x, y = y, label = x), size = 10, color = "white",
            family = font_body, data = yearly) +
  
  ### Reverses the x-axis
  scale_x_reverse() +
  
  ### Controls the proportions
  coord_fixed(ratio = 25, ylim = c(-2,0.6)) +
  
  ### Eliminates theme elements
  theme_void()

## Creates the main plot
p <- NULL %>% 
  ggplot() +
  
  ### Places the titles
  ggtext::geom_richtext(aes(x = x, y = y, label = label,
                            size = I(size), fontface = I(fontface)),
                        label.color = NA, fill = NA, lineheight = lineheight,
                        hjust = 0, vjust = 1, color = "white",
                        family = font_title, data = titles) +
  
  ### Places the glyphs
  ggpath::geom_from_path(aes(x = x, y = y, path = path),
                         width = 0.1, height = 0.1,
                         hjust = 1, vjust = 1, data = glyphs) +
  
  ### Places the legs
  geom_path(aes(x = x, y = y, group = group), lineend = "round",
            color = "#d9d7d7", size = 11, data = legs) +
  
  ### Places the plots
  cowplot::draw_plot(p1,
                     x = 0.5, y = 0.75,
                     width = 1, height = 0.6,
                     hjust = 0.5, vjust = 1) +
  cowplot::draw_plot(p2,
                     x = 0.5, y = -0.18,
                     width = 1, height = 0.4,
                     hjust = 0.5, vjust = 0,
                     valign = 0) +
  
  ### Defines limits for the axes
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = NA)
  )

## Saves the plot
ggsave("2022/week25/sankofa.png", plot = p, dpi = "retina",
       width = 25, height = 32, device = ragg::agg_png, res = 320)

