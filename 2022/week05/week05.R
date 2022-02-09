# 0. Library and fonts management
library(tidyverse)
library(ggplot2)
library(showtext)
library(tidytuesdayR)
library(glue)
library(lubridate)
library(ggtext)
library(emojifont)

## Adding Google Fonts
sysfonts::font_add_google(name = "Teko", family = "teko") ### Sans Serif
sans <- "teko"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

# 1. Data download, load and handling
airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

## Selects the data
df <- airmen %>%
  dplyr::distinct(name, graduation_date, .keep_all = TRUE) %>% 
  dplyr::mutate(id = 1:n()) %>% 
  dplyr::rename("grad" = "graduation_date",
                "details" = "aerial_victory_credits",
                "victories" = "number_of_aerial_victory_credits") %>% 
  dplyr::select(id, grad, details, victories)

## Calculates the max number of different strikes a pilot downed planes
max_strikes <- df %>% 
  dplyr::transmute(strikes = str_count(details, ";")+1) %>% 
  dplyr::summarise(strikes = max(strikes, na.rm = TRUE)) %>% 
  dplyr::pull()

## Defines quantities limits
lim_vict <- 0
lim_date <- lubridate::ymd("1949-12-31")

## Extracts for each strike the humber of downed planes and date
df <- df %>% 
  dplyr::mutate(victories = round(victories)) %>% 
  dplyr::group_by(id) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(data = data %>% purrr::map(
    ~.x %>%
      tidyr::separate(col = details,
                      into = glue::glue("strike{1:max_strikes}"),
                      sep = ";") %>% 
      tidyr::pivot_longer(cols = starts_with("strike"),
                          names_to = "number",
                          values_to = "event") %>% 
      tidyr::separate(col = event,
                      into = c("downed","date"),
                      sep = "on") %>% 
      dplyr::mutate(across(.cols = c(downed, date), .fns = stringr::str_trim)) %>% 
      dplyr::mutate(downed = stringr::str_sub(downed, 8L, 8L),
                    downed = as.numeric(downed)) %>% 
      dplyr::mutate(year = stringr::str_sub(date, -4L, -1L),
                    month = str_extract(date, paste0(month.name, collapse = "|")),
                    day = str_remove(date, glue::glue(", {year}")),
                    day = str_remove(day, month),
                    month = match(month, month.name),
                    date = lubridate::ymd(glue::glue("{year}-{month}-{day}"))) %>% 
      dplyr::select(-year,-month,-day) %>% 
      dplyr::mutate(downed = if(victories[1] == 0){c(lim_vict,rep(NA,max_strikes-1))}else{downed},
                    date = if(victories[1] == 0){c(lim_date,rep(NA,max_strikes-1))}else{date}) %>% 
      na.exclude() %>% 
      dplyr::mutate(cumulative = cumsum(downed))
  )) %>% 
  tidyr::unnest(cols = data) %>% 
  dplyr::ungroup()

## Calculates the cumulative count of downed planes
strikes <- df %>% 
  dplyr::filter(downed != 0) %>% 
  dplyr::arrange(date) %>% 
  dplyr::mutate(cumulative = cumsum(downed)) %>% 
  dplyr::select(date, cumulative)

## Gets the number ot total downed planes
total_downed <- strikes %>% dplyr::slice(n()) %>% dplyr::pull(cumulative)

## Defines some layout constants
lnhgt <- 0.28
bgcolor <- "#dacbb8"
gridcolor <- "#cf7269"
top_grid <- 120
legend_pos <- seq(3,23,5)

## Defines coordinates for the start and end of the timeline
## as well as joints to cover the empty space between the geom_step
start <- tibble(
  date = lubridate::ymd("1940-1-1"),
  cumulative = 0
) %>% 
  rbind(strikes[1,])

end <- tibble(
  date = lubridate::ymd("1949-1-1"),
  cumulative = total_downed
) %>% 
  rbind(strikes %>% dplyr::slice(n()))

joints <- strikes %>% 
  dplyr::slice(1L,n())

## Defines coordinates for the plot grid
gridX <- tibble(
  y = seq(0,top_grid,5),
  xmin = lubridate::ymd("1940-1-1"),
  xmax = lubridate::ymd("1949-1-1")
) %>% 
  dplyr::mutate(color = ifelse(row_number() == 1L | row_number() == n(),
                               "black",
                               gridcolor))

gridY <- tibble(
  x = lubridate::ymd(c(glue::glue("{1940:1949}-1-1"),glue::glue("{1940:1948}-7-1"))),
  ymin = 0,
  ymax = top_grid
) %>% 
  dplyr::arrange(x) %>% 
  dplyr::mutate(color = ifelse(row_number() == 1L | row_number() == n(),
                               "black",
                               gridcolor))

## Defines coordinates for the grid on the legend on the left of the plot
legendX <- tibble(
  y = seq(0,top_grid,5),
  xmin = lubridate::ymd("1937-1-1"),
  xmax = lubridate::ymd("1939-1-1")
) %>% 
  dplyr::mutate(color = ifelse(row_number() == 1L | row_number() == n(),
                               "black",
                               gridcolor))

legendY <- tibble(
  x = lubridate::ymd(c("1937-1-1","1939-1-1")),
  ymin = 0,
  ymax = top_grid,
  color = "black"
)

## Defines coordinates for the numbers, ticks, icons
## and title on the legend on the left of the plot
legendNUM <- tibble(
  x = lubridate::ymd("1938-1-1"),
  y = legendX$y[legend_pos],
  label = legendX$y[legend_pos],
)

legendTICKS <- tibble(
  y = legendNUM$y,
  xmin = lubridate::ymd("1939-2-1"),
  xmax = lubridate::ymd("1940-1-1"),
  color = gridcolor
)

legendICON <- tibble(
  x = lubridate::ymd("1937-7-1"),
  y = legendX$y[legend_pos-1],
  label = fontawesome("fa-plane"),
)

legendTITLE <- tibble(
  x = lubridate::ymd("1938-1-1"),
  y = top_grid-0.3,
  label = "DOWNED<br>ENEMY PLANES",
)

## Defines coordinates for the numbers on the horizontal axis
axisX <- tibble(
  x = lubridate::ymd(glue::glue("{1940:1949}-1-1")),
  y = -2,
  label = 1940:1949,
)

## Defines coordinates for the insights on the timeline
insights <- tibble(
  x = lubridate::ymd(c("1941-1-1"),
                     c("1943-7-1"),
                     c("1944-9-15"),
                     c("1945-1-1"),
                     c("1947-12-1")),
  y = c(4,17,42,92,115),
  angle = c(0,90,0,0,0),
  hjust = c(0.5,0.5,0,1,0.5),
  label = c(glue::glue("TUSKGEE AIRMEN<br>GROUP IS CREATED"),
            glue::glue("FIRST MISSION IN EUROPE"),
            glue::glue("AWARDED<br>ITALIAN CAMPAIGN"),
            glue::glue("COMPLETE VICTORIES<br>AGAINST GERMAN FIGHTERS"),
            glue::glue("TUSKGEE AIRMEN<br>GROUP IS DISMISSED"))
)

## Defines coordinates for the titles
title <- tibble(
  x = lubridate::ymd("1943-1-1"),
  y = c(top_grid+20,top_grid+5),
  size = c(36,11),
  label = c(glue::glue("CUMULATIVE AERIAL VICTORIES CREDITED TO<br>PILOTS OF THE TUSKGEE AIRMEN."),
            glue::glue("INSPIRATED BY: W.E.B. DU BOIS | DATA FROM: COMMEMORATIVE AIRFORCE (CAF)
                       BY WAY OF THE VA-TUG | GRAPHIC BY: ÍCARO BERNARDES (@IcaroBSC)")),
)

## Creates the main plot
p <- strikes %>% 
  ggplot() +
  
  ### Places the plot grid
  geom_linerange(aes(y = y, xmin = xmin, xmax = xmax, color = I(color)), data = gridX) +
  geom_linerange(aes(x = x, ymin = ymin, ymax = ymax, color = I(color)), data = gridY) +
  
  ### Places the legend grid and ticks
  geom_linerange(aes(y = y, xmin = xmin, xmax = xmax, color = I(color)), data = legendX) +
  geom_linerange(aes(x = x, ymin = ymin, ymax = ymax, color = I(color)), data = legendY) +
  geom_linerange(aes(y = y, xmin = xmin, xmax = xmax, color = I(color)), data = legendTICKS) +
  
  ### Places the legend title, numbers and ticks
  ggtext::geom_richtext(aes(x = x, y = y, label = label), fill = NA,
                        label.colour = NA, size = 11, vjust = 1,
                        family = sans, lineheight = lnhgt,
                        data = legendTITLE) +
  ggtext::geom_richtext(aes(x = x, y = y, label = label), fill = bgcolor,
                        label.colour = NA, size = 27, family = sans,data = legendNUM) +
  ggtext::geom_richtext(aes(x = x, y = y, label = label), fill = bgcolor,
                        label.colour = NA, angle = -180, size = 15,
                        family = "fontawesome-webfont", data = legendICON) +
  
  ### Places the horizontal axis numbers
  ggtext::geom_richtext(aes(x = x, y = y, label = label), fill = NA,
                        label.colour = NA, size = 12, vjust = 1,
                        family = sans, data = axisX) +
  
  ### Places the timeline insights
  ggtext::geom_richtext(aes(x = x, y = y, label = label, angle = angle, hjust = hjust),
                        fill = bgcolor, label.colour = NA, size = 14,
                        lineheight = lnhgt, family = sans, data = insights) +
  
  ### Places the plot titles
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        fill = NA, label.colour = NA, vjust = 1,
                        family = sans, lineheight = lnhgt, data = title) +
  
  ### Places the joints between the geom_step
  geom_point(aes(x = date, y = cumulative), size = 7,
             fill = "black", shape = 22, data = joints) +
  
  ### Places the cumulative count of downed planes
  geom_step(aes(x = date, y = cumulative), size = 7, data = strikes) +
  
  ### Places extensions of the cumulative timeline
  ### which represent the start and end of the Tuskgee Airmen force
  geom_step(aes(x = date, y = cumulative), size = 7, data = start) +
  geom_step(aes(x = date, y = cumulative), size = 5, color = bgcolor, data = start) +
  geom_step(aes(x = date, y = cumulative), size = 7, data = end) +
  geom_step(aes(x = date, y = cumulative), size = 5, color = bgcolor, data = end) +
  
  ### Eliminates unnecesary elements and customizes the plot
  theme_void() +
  theme(
    plot.background = element_rect(fill = bgcolor, color = NA)
  )

## Saves the plot
ggsave("2022/week05/strikes.png", plot = p, dpi = "retina",
       width = 23, height = 25)

