# 0. Library and fonts management
library(tidyverse)
library(ggplot2)
library(showtext)
library(glue)
library(ggtext)
library(patchwork)
library(santoku)
library(scales)
library(ggimage)
library(ggtext)
library(colorspace)

## Adding Google Fonts
sysfonts::font_add_google(name = "Montagu Slab", family = "Montagu Slab")
serif <- "Montagu Slab"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

## Defines some layout constants
clrM <- "#0184e3"
clrF <- "#a21ba7"
lnght <- 0.9
width <- 35
height <- 40
cards_clr <- "#95E63B"
line_clr <- colorspace::darken(cards_clr, 0.25)
border_clr <- colorspace::darken(cards_clr, 0.35)

# 1. Data download, load and handling
rawdata <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

## Gets the relative difference between the amount of students by sex
## and separates these values in bins. Keeps the institutions with the
## smallest differences (at least 75% of all institutions)
inst <- rawdata %>% 
  dplyr::distinct(unitid, ef_male_count, ef_female_count, ef_total_count) %>% 
  dplyr::mutate(dif = 100*abs(ef_male_count-ef_female_count)/ef_total_count,
                bin = santoku::chop_evenly(dif, 10)) %>% 
  na.exclude() %>% 
  dplyr::arrange(dif)

bins <- inst %>% 
  dplyr::count(bin) %>% 
  dplyr::mutate(pct = 100*cumsum(n)/sum(n)) %>% 
  dplyr::filter(lag(pct, default = 0) <= 75) %>% 
  dplyr::pull(bin)

inst <- inst %>% 
  dplyr::filter(bin %in% bins) %>% 
  dplyr::pull(unitid)

## Keeps only the institutions and variables of interest. Excludes lines
## with NA and keeps only the 2018-2019 season (last season pre-pandemic)
df <- rawdata %>% 
  dplyr::filter(year == 2018) %>% 
  dplyr::select(unitid, sports,
                sum_partic_men, sum_partic_women,
                rev_men, rev_women, exp_men, exp_women) %>% 
  dplyr::filter(unitid %in% inst) %>% 
  na.exclude()

## Renames counts for men and women
df <- df %>% 
  dplyr::rename(
    partic_men = sum_partic_men,
    partic_women = sum_partic_women,
    revenue_men = rev_men,
    revenue_women = rev_women,
    expend_men = exp_men,
    expend_women = exp_women
  )

## Rearranges the data so pair of variables of men and women are placed together
df <- df %>% 
  tidyr::pivot_longer(cols = partic_men:expend_women,
                      names_to = c(".value","sex"),
                      names_sep = "_")

## Converts the 'All Track Combined' to 'Athletics'
df <- df %>% 
  dplyr::mutate(sports = ifelse(sports == "All Track Combined", "Athletics", sports))

## Keeps only some Olympic sports
olymp_sports <- c("Athletics","Archery", "Basketball",
                  "Beach Volleyball", "Diving", "Fencing",
                  "Golf", "Gymnastics", "Ice Hockey",
                  "Rifle", "Rowing", "Sailing",
                  "Soccer", "Swimming", "Table Tennis",
                  "Tennis", "Volleyball", "Water Polo",
                  "Weight Lifting", "Wrestling")
df <- df %>% dplyr::filter(sports %in% olymp_sports)

## Calculates the mean ratio between sexes for the numeric variables
kpi <- df %>% 
  dplyr::group_by(unitid, sports) %>% 
  dplyr::mutate(across(.cols = where(is.numeric), .fns = ~./sum(.))) %>% 
  dplyr::group_by(sports, sex) %>% 
  dplyr::select(-unitid) %>% 
  dplyr::summarise(across(.cols = where(is.numeric),
                          .fns = ~round(100*mean(.), 1))) %>% 
  dplyr::group_by(sports) %>% 
  dplyr::summarise(across(.cols = where(is.numeric),
                          .fns = list(sex = ~sex[which.max(.)],
                                      value = max),
                          .names = "{.col}_{.fn}")) %>% 
  tidyr::pivot_longer(cols = matches("(value|sex)$"),
                      names_to = c("var",".value"),
                      names_sep = "_") %>% 
  dplyr::mutate(color = ifelse(sex == "men", clrM, clrF))

joiner <- tribble(
  ~var,      ~x,   ~y,   ~verb,  ~noun,
  "partic",  -0.6, -0.75, "make", "participants",
  "expend",  0,    -0.75, "take", "expenses",
  "revenue", 0.6,  -0.75, "give", "revenues"
)

kpi <- kpi %>% 
  dplyr::left_join(joiner) %>%
  dplyr::mutate(across(.cols = c(sex, verb, noun),
                       .fns = toupper)) %>% 
  dplyr::mutate(label = glue::glue(
    "{sex} {verb} MOST<br>OF THE {noun}<br><br>
    <span style='font-size:30px;color:{color}'>**{value}%**</span>"
  ))

## Calculates the summary statistics for each sport and sex
stats_sum <- df %>% 
  dplyr::select(-unitid) %>% 
  dplyr::group_by(sports, sex) %>% 
  dplyr::summarise(across(.fns = sum)) %>% 
  dplyr::ungroup()

min_coin <- stats_sum %>% 
  dplyr::summarise(value = min(revenue, expend)) %>% 
  dplyr::mutate(value = log10(value)) %>% 
  dplyr::pull(value)
max_coin <- stats_sum %>% 
  dplyr::summarise(value = max(revenue, expend)) %>% 
  dplyr::mutate(value = log10(value)) %>% 
  dplyr::pull(value)

stats_sum <- stats_sum %>% 
  dplyr::mutate(across(.cols = partic,
                       .fns = ~scales::rescale(log10(.), to = c(0.25,0.9)),
                       .names = "scaled_{.col}")) %>% 
  dplyr::mutate(across(.cols = c(revenue, expend),
                       .fns = ~scales::rescale(log10(.),
                                               to = c(0.25,0.9),
                                               from = c(min_coin, max_coin)),
                       .names = "scaled_{.col}")) %>% 
  dplyr::mutate(across(.cols = where(is.numeric),
                       .fns = ~ifelse(sex == "men", -., .))) %>% 
  dplyr::mutate(xmin = ifelse(sex == "men", -0.2, 0.2),
                xmax = ifelse(sex == "men", -0.9, 0.9),
                hjust = ifelse(sex == "men", 1, 0)) %>% 
  dplyr::rename_with(.cols = matches("^(partic|revenue|expend)"),
                     .fn = ~glue::glue("value_{.}")) %>% 
  tidyr::pivot_longer(cols = matches("^(value|scaled)"),
                      names_to = c(".value","var"),
                      names_sep = "_") %>% 
  dplyr::mutate(y = case_when(var == "partic" ~ 0.15,
                              var == "revenue" ~ 0,
                              var == "expend" ~ -0.15,
                              TRUE ~ 0),
                label = abs(value),
                label = ifelse(var == "partic",
                               scales::comma_format(accuracy = 1, suffix = " PEOPLE")(label),
                               scales::dollar_format()(label)
                               ))

## Defines the coordinates for the pictograms
pictos <- df %>% 
  dplyr::distinct(sports) %>% 
  dplyr::mutate(image = glue::glue("2022/week13/icons/{sports}.png"),
                x = 0,
                y = 0.6)

## Defines the coordinatesfor the medal icon
medal <- tibble(
  x = 0.9,
  y = 0.95,
  image = "2022/week13/icons/medal-solid.png"
)

## Defines coordinates for the summary labels
labels <- tibble(
  x = 0,
  y = c(0.15, 0, -0.15),
  label = c("PARTICIPANTS",
            "EXPENDITURE",
            "REVENUE")
)

## Defines coordinates for the titles
titles <- tibble(
  x = 0.03,
  y = c(0.97,0.92,0.87),
  label = c(
    "Olympic effort",
    
    glue::glue(
      "
      Between the many sports played in US colleges, there are 20 that can be associated to Olympic competitions.<br>
      Participation, expenditure and revenues are somewhat similar between the sexes for most of them.<br>
      The cards bellow show these informations for colleges that have similiar quantities of women and men.<br>
      The bars show sums over all colleges. The numbers at the bottom show the mean of the ratios.
      "
    ),
    
    "Data from Equity in Athletics Data Analysis | Pictograms: © 2013 Copyright by the Rio de Janeiro Olympic Organizing Committee | Graphic: Ícaro Bernardes (@IcaroBSC)"
  ),
  size = c(50,12,8)
)

# 2. Generates the plot
## Creates the cards using facet
cards <- stats_sum %>% 
  ggplot() +
  
  ### Places the Olympic pictograms
  ggimage::geom_image(aes(x = x, y = y, image = image), size = 0.42,
                      by = 'width', asp = width/height, data = pictos) +
  
  ### Places the labels of the stats
  geom_text(aes(x = x, y = y, label = label), size = 3,
            fontface = "bold", family = serif, data = labels) +
  
  ### Places the 'background' of the lines of the stats
  geom_linerange(aes(xmin = xmin, xmax = xmax, y = y),
                 color = line_clr, size = 7) +
  
  ### Places the lines of the stats
  geom_linerange(aes(xmin = xmin, xmax = scaled, y = y, color = sex), size = 7) +
  
  ### Places the values of the stats
  ggtext::geom_richtext(aes(x = xmin*1.01, y = y, hjust = hjust, label = label),
                        color = "white", size = 3, family = serif,
                        label.color = NA, fill = NA, nudge_y = -0.005) +
  
  ### Places the mean ratios and the title of this section
  annotate("text", x = 0, y = -0.45, size = 6,
           fontface = "bold", family = serif, lineheight = lnght,
           label = "TYPICAL SPLIT BETWEEN SEXES IN\nTHE INSTITUTIONS (MEAN RATIO)") +
  ggtext::geom_richtext(aes(x = x, y = y, label = label), size = 3,
                        family = serif, label.color = NA, fill = NA, data = kpi) +
  
  ### Applies colors for the sexes
  scale_color_discrete(type = c(clrM, clrF), guide = "none") +
  
  ### Facets the plots
  facet_wrap(~ sports, nrow = 4, ncol = 5) +
  
  ### Defines unitary plots limits
  coord_cartesian(xlim = c(-1,1), ylim = c(-1,1), expand = FALSE) +
  
  ### Eliminates unnecessary elements and customizes others
  theme_void() +
  theme(
    strip.text = element_text(family = serif, size = 35, face = "bold", color = "white",
                              margin = margin(t = 30, r = 0, b = 30, l = 0, unit = "pt")),
    strip.background = element_rect(fill = border_clr, color = NA),
    panel.background = element_rect(fill = cards_clr, color = border_clr,
                                    size = 7),
    panel.spacing = unit(50, "pt")
  )

## Creates the main plot
p <- ggplot(NULL) +
  
  ### Places the titles
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        hjust = 0, vjust = 1, family = serif, label.color = NA,
                        fill = NA, lineheight = lnght, data = titles) +
  
  ### Places the medal icon
  ggimage::geom_image(aes(x = x, y = y, image = image), size = 0.17,
                      by = 'width', asp = width/height, data = medal) +
  
  ### Defines unitary plots limits
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
  
  ### Eliminates elements on the plot
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#f7f7f7", color = NA)
  ) +
  
  ### Places the cards in the main plot
  patchwork::inset_element(cards,
                           left = 0.03,
                           right = 0.97,
                           bottom = 0.03,
                           top = 0.85)

## Saves the plot
ggsave("2022/week13/sports.png", plot = p, dpi = "retina",
       width = width, height = height)

