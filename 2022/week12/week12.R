# 0. Library and fonts management
library(tidyverse)
library(ggplot2)
library(showtext)
library(glue)
library(ggtext)
library(ggforce)
library(polite)
library(rvest)
library(patchwork)

## Adding Google Fonts
sysfonts::font_add_google(name = "Grandstander", family = "Grandstander")
sans <- "Grandstander"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

## Defines some layout constants
lnhgt <- 0.9
cnvW <- 23
cnvH <- 28
clrM <- "#0184e3"
clrF <- "#a21ba7"

# 1. Data download, load and handling
rawdata <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

## Gets the top names that amount to at least 70% of names for a given year and sex
df <- rawdata %>% 
  dplyr::arrange(year, sex, desc(n), name) %>% 
  dplyr::group_by(year, sex) %>% 
  dplyr::mutate(cumsum = cumsum(prop)) %>% 
  dplyr::filter(lag(cumsum, default = 0) <= 0.7) %>% 
  dplyr::select(-cumsum) %>% 
  dplyr::ungroup()

## Gets only the neutral names (not NA for both sexes)
df <- df %>% 
  tidyr::pivot_wider(names_from = sex,
                     values_from = n:prop) %>% 
  dplyr::filter(if_all(.fns = ~!is.na(.)))

## Eliminates names that appear once
df <- df %>% 
  dplyr::group_by(name) %>% 
  dplyr::filter(n() > 1) %>% 
  dplyr::ungroup()

## Restores the data to the original format
df <- df %>% 
  tidyr::pivot_longer(cols = n_F:prop_M,
                      names_to = c(".value", "sex"),
                      names_sep = "_")

## Gets the total of births by sex and name
births <- df %>% 
  dplyr::group_by(name, sex) %>% 
  dplyr::summarise(sum_births = sum(n)) %>% 
  dplyr::group_by(name) %>% 
  dplyr::mutate(pct_births = round(100*sum_births/sum(sum_births))) %>% 
  dplyr::ungroup()

## Gets the year of peak popularity of
## the names in terms or proportion of births
peak <- df %>% 
  dplyr::group_by(year, name) %>% 
  dplyr::summarise(prop = sum(prop)) %>% 
  dplyr::group_by(name) %>% 
  dplyr::summarise(topyear = year[which.max(prop)])

## Gets the spearman correlation between
## males and females births in the time series
spear <- df %>% 
  dplyr::group_by(name, sex) %>% 
  dplyr::summarise(n = list(cur_data()$n)) %>% 
  dplyr::group_by(name) %>% 
  dplyr::summarise(cor = cor(unlist(cur_data()$n[1]),
                             unlist(cur_data()$n[2]),
                             method = "spearman")) %>% 
  dplyr::mutate(cor = round(cor, 2))

## Gets the year range of popularity of the names
range <- df %>% 
  dplyr::group_by(name) %>% 
  dplyr::summarise(period = glue::glue("{min(year)}-{max(year)}"))

## Scraps the meaning and origin of the names as stated by nameberry.com
### Establishes connection with the host and announces our presence
session <- polite::bow("https://nameberry.com/search")
names <- sort(unique(df$name))

### Sends queries (asks info. about the names) to the page
### defined in the session and gets the response for each query
responses <- purrr::map(names,
                        ~ polite::scrape(session, query = list(q = tolower(.x))))

### Extracts the data of interest (origin and meaning) from the html of the pages
results <- purrr::map(responses,
                      ~ .x %>% 
                        rvest::html_element(".Name-meaning-origin") %>%
                        rvest::html_text())

### Separates the characters (origin and meaning)
results <- purrr::map(results,
                      ~ .x %>% 
                        stringr::str_split_fixed(pattern = '"', 2) %>% 
                        stringr::str_remove_all(pattern = '"'))

## Converts the results to a tibble origin
etimol <- purrr::map(results,
                     ~tibble(origin = .x[1], meaning = .x[2])
) %>%  purrr::reduce(rbind)

## Adds the names to the tibble
etimol <- etimol %>% 
  dplyr::mutate(name = names)

## Combines the origin and meaning, since they appear in most, but not in
## every name. Places breaks in the names according to character width
etimol <- etimol %>% 
  dplyr::mutate(detail = glue::glue("{origin}. {meaning}"),
                detail = stringr::str_wrap(detail, width = 25))

## Corrects one of the descriptions
etimol <- etimol %>% 
  dplyr::mutate(detail = ifelse(name == "Armani",
                                "Italian surname, after\nGermanic Herman,\nmeaning warrior.",
                                detail))

## Joins all names info
info <- dplyr::left_join(peak, range) %>% 
  dplyr::left_join(etimol) %>% 
  dplyr::left_join(spear)

## Defines coordinates for the titles
titles <- tibble(
  x = 0.03,
  y = c(0.97,0.92,0.83),
  label = c(
    "What is in a name?",
    
    glue::glue(
      "Of the myriad of names given to babies in the US between 1880 and 2017
      <br>many are gender neutral. The donuts bellow show info about gender
      <br>neutral names that were in the top 70% for both sexes for at least two
      <br>years. The sections represent the percentage of all
      <span style='color:{clrM}'>males</span> and
      <span style='color:{clrF}'>females</span>
      <br>babies that were given a certain name during its years of popularity."
    ),
    
    "Data from babynames R package from Hadley Wickham | Graphic: Ícaro Bernardes (@IcaroBSC) "
  ),
  size = c(40,10,8)
)

# 2. Generates the plot
## Creates the facet of donuts
donuts <- ggplot(NULL) +
  ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 1, r = 1.3, amount = pct_births,
                            fill = sex), color = NA, stat = "pie", data = births) +
  geom_text(aes(x = 0, y = 0.46, label = name), family = sans,
            color = "white", size = 6, fontface = "bold", data = info) +
  geom_text(aes(x = 0, y = -0.55, label = topyear), family = sans,
            color = "white", size = 5, data = info) +
  geom_text(aes(x = 0, y = -0.77, label = period), family = sans,
            color = "white", size = 4, data = info) +
  geom_text(aes(x = 0, y = -0.06, label = detail), family = sans,
            color = "white", size = 3, lineheight = lnhgt, data = etimol) +
  scale_fill_discrete(type = c(clrF,clrM), guide = "none") +
  coord_fixed() +
  facet_wrap(~name) +
  theme_void() +
  theme(
    strip.text = element_blank()
  )

## Creates the example donut
example <- ggplot(NULL) +
  ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 1, r = 1.3, amount = c(0.2,0.8),
                            fill = c("F","M")), color = NA, stat = "pie") +
  annotate("text", x = 0, y = 0.6, label = "Name", color = "white", 
           family = sans, size = 8, fontface = "bold") +
  annotate("text", x = 0, y = -0.35, label = "Peak year of\nprop. of births",
           color = "white", family = sans, size = 4, lineheight = lnhgt) +
  annotate("text", x = 0, y = -0.75, label = "First and last year\nof popularity",
           color = "white", family = sans, size = 3, lineheight = lnhgt) +
  annotate("text", x = 0, y = 0.15, label = "Origin and meaning\nof the name",
           color = "white", family = sans, size = 4, lineheight = lnhgt) +
  scale_fill_discrete(type = c(clrF,clrM), guide = "none") +
  coord_fixed() +
  theme_void()

## Creates the main plot
p <- titles %>% 
  ggplot() +
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        color = "white", label.color = NA, fill = NA,
                        hjust = 0, vjust = 1, family = sans, lineheight = 1.2) +
  annotate("text", x = 0.945, y = 0.96, label = "HOW TO READ THIS CHART:", color = "white",
           family = sans, size = 9, fontface = "bold", hjust = 1, vjust = 1) +
  coord_fixed(xlim = c(0,1), ylim = c(0,1), ratio = cnvH/cnvW, expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black")
  ) +
  patchwork::inset_element(donuts,
                           top = 0.8, bottom = 0.02,
                           left = 0.02, right = 0.98) +
  patchwork::inset_element(example,
                           top = 0.945, bottom = 0.815,
                           left = 0.77, right = 0.94)

## Saves the plot
ggsave("2022/week12/names.png", plot = p, dpi = "retina",
       width = cnvW, height = cnvH)

