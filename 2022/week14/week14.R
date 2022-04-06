# 0. Library and fonts management
library(tidyverse)
library(showtext)
library(glue)
library(patchwork)
library(fastDummies)
library(UpSetR)
library(ggbeeswarm)
library(ggbump)
library(tidytext)
library(cols4all)

## Adding Google Fonts
sysfonts::font_add_google(name = "Grape Nuts", family = "Grape Nuts", db_cache = FALSE)
hand <- "Grape Nuts"
sysfonts::font_add_google(name = "Cormorant Garamond", family = "Cormorant Garamond")
sans <- "Cormorant Garamond"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

## Defines some layout constants
lnhgt <- 0.9
c_trunk <- "#664014"

# 1. Data download, load and handling
rawdata <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')

## Keeps only data on the budgets and coverage
df <- rawdata %>% 
  dplyr::select(starts_with("budget"), coverage_topics) %>% 
  na.exclude()

## Renames and lumps together some budget percentages 
df <- df %>% 
  dplyr::mutate(across(
    .cols = starts_with("budget"),
    .fns = ~factor(., levels = c("0-10", "11-20", "21-30", "31-40", "41-50",
                                 "51-60", "61-70", "71-80", "81-90", "91-100"))
  )) %>% 
  dplyr::mutate(across(
    .cols = starts_with("budget"),
    .fns = ~forcats::fct_collapse(.,
                                  `0-20` = c("0-10", "11-20"),
                                  `21-40` = c("21-30", "31-40"),
                                  `41-60` = c("41-50", "51-60"),
                                  `61-80` = c("61-70", "71-80"),
                                  `81-100` = c("81-90", "91-100"),
    ))) %>% 
  dplyr::rename_with(.fn = ~str_remove_all(., "(budget|percent|\\_)"))

## Gets dummies of the percentages
sets <- df %>% 
  dplyr::select(-coveragetopics) %>% 
  fastDummies::dummy_cols(remove_selected_columns = TRUE) %>%
  as.data.frame()

## Creates an upset plot to see the most frequent arrangements
UpSetR::upset(sets, order.by = "freq", nintersects = 10, nsets = 11)

## Lists the top arrangements
top_arranges <- tibble::tribble(
  ~group, ~editorial, ~revenuegeneration, ~producttechnology, ~administration,
  1,     "61-80",    "0-20",             "0-20",             "0-20",
  2,     "41-60",    "0-20",             "0-20",             "0-20",
  3,     "41-60",    "21-40",            "0-20",             "0-20",
  4,     "81-100",   "0-20",             "0-20",             "0-20",
  5,     "21-40",    "21-40",            "0-20",             "0-20",
  6,     "41-60",    "0-20",             "0-20",             "21-40",
  7,     "0-20",     "0-20",             "0-20",             "0-20",
  8,     "21-40",    "21-40",            "21-40",            "21-40",
  9,     "21-40",    "21-40",            "0-20",             "21-40",
  10,    "61-80",    "21-40",            "0-20",             "0-20"
)

## Filters only organizations that have one of the top arrangements
df <- dplyr::right_join(df, top_arranges)

## Gets the counts of the arrangements
top_arranges <- df %>%
  dplyr::count(group) %>% 
  dplyr::left_join(top_arranges)

## Gets the topics covered by the organizations
df <- df %>% 
  tidytext::unnest_tokens(topic, coveragetopics,
                          token = "regex", pattern = "\\,") %>% 
  dplyr::mutate(topic = stringr::str_trim(topic)) %>% 
  dplyr::select(group, topic)

## Gets the count of the topics 
topics <- df %>% 
  dplyr::count(topic) %>% 
  dplyr::arrange(desc(n))

## Lumps together topics
df <- df %>% 
  dplyr::mutate(topic = factor(topic),
                topic = forcats::fct_collapse(
                  topic,
                  `global crisis` = c("military", "immigration", "environment"),
                  `government & politics` = c("government", "politics"),
                  `citizens concerns` = c("education & schools", "public policy", "energy", "housing"),
                  `productive forces` = c("business", "economics", "technology & innovation", "planning & development", "transportation"),
                  `culture & entertainment` = c("events in coverage area", "entertainment & arts", "religion", "culture", "sports & recreation", "travel", "local profiles"),
                  `crime` = c("crime & justice", "corruption"),
                  `society changes` = c("social justice & inequality", "gender identity", "race & ethnicity", "cannabis reform"),
                  `healthy life` = c("health & medicine", "food", "lifestyle"),
                  `news` = c("local news", "breaking news")
                ))

## Defines coordinates of the "fruits"
fruits_clr <- tibble(
  topic = unique(df$topic),
  # color = c("#954cd0", "#77bd45", "#51317b", "#ad8e3f", "#738ed3", "#d55a3e", "#539f72", "#d462a1", "#86333f")
  color = cols4all::c4a("misc.kelly", n = 9)
)
set.seed(42)
fruits <- df %>% 
  dplyr::group_by(group) %>% 
  dplyr::mutate(x = 0.5,
                y = runif(n(), min = 0.67, max = 0.98)) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(fruits_clr)

## Defines coordinates of the "roots" and "trunk"
root_clr <- cols4all::c4a("viridis.mako", n = 5, contrast = c(0, 0.3))
trunk <- top_arranges %>% 
  tidyr::pivot_longer(cols = editorial:administration) %>% 
  dplyr::mutate(x = 0.5,
                y = 0.6,
                xend = case_when(name == "editorial" ~ 0.1,
                                 name == "revenuegeneration" ~ 0.36,
                                 name == "producttechnology" ~ 0.67,
                                 name == "administration" ~ 0.9,
                                 TRUE ~ 0),
                yend = 0.1) %>% 
  dplyr::mutate(cat = case_when(name == "editorial" ~ "EDITORIAL",
                                name == "revenuegeneration" ~ "REVENUE\nGENERATION",
                                name == "producttechnology" ~ "PRODUCT\nTECHNOLOGY",
                                name == "administration" ~ "ADMIN.",
                                TRUE ~ "")) %>% 
  dplyr::mutate(fill = case_when(value == "0-20" ~ root_clr[1],
                                 value == "21-40" ~ root_clr[2],
                                 value == "41-60" ~ root_clr[3],
                                 value == "61-80" ~ root_clr[4],
                                 value == "81-100" ~ root_clr[5],
                                 TRUE ~ "red"))

## Defines coordinates for the titles
vec_topics <- glue::glue("<span style='color:{fruits_clr$color}'>{fruits_clr$topic}</span>") %>% 
  paste0(collapse = ", ")
titles <- tibble(
  x = 0.03,
  y = c(0.97, 0.875, 0.735),
  size = c(35, 7.5, 7),
  label = c(
    "Locally sourced news organizations",
    
    glue::glue(
      "The trees bellow show some locally focused digital news publishers from the US and Canada mapped by the Project Oasis.<br>
      The roots represent the how the budget is divided between four categories of spending: Editorial, Revenue Generation, Product Technology and Administration.<br>
      The original data had ten ranges. In this image, they were lumped together into five: 0-20, 21-40, 41-60, 61-80, 81-100.<br>
      In the top of the tree trunk it's indicated the number of organizations that belong to the same group that has the same combination of investments indicated in the roots.<br>
      Between the mass of tree leaves there are fruits. These fruits represent the topics covered by organizations in the same budget breakdown group. The topics are:<br>
      {vec_topics}."
    ),
    
    "Data from Project Oasis by way of Data is Plural | Graphic: Ícaro Bernardes (@IcaroBSC) "
    
  )
)

## Defines coordinates for the background of the percentage labels in the title
pct_labels <- tibble(
  x = c(0.526,0.567,0.61,0.653,0.699),
  y = 0.82,
  width = c(0.037, rep(0.041, 3), 0.043),
  fill = root_clr
)

# 2. Generates the plot
## Makes the "trees"
tree <- ggplot(NULL) +
  
  ### Places the "soil"
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 0.4,
           fill = "#bfa47a", color = NA) +
  
  ### Places the "sky"
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.4, ymax = 1,
           fill = "#d3f5f4", color = NA) +
  
  ### Places the "leaves"
  annotate("point", x = 0.5, y = 0.92, color = "#bbf2c0", size = 140) +
  
  ### Places the "trunk and roots"
  ggbump::geom_sigmoid(aes(x = x, y = y, xend = xend, yend = yend, group = name),
                       direction = "y", color = c_trunk, size = 14, smooth = 12,
                       data = trunk) +
  
  ### Places the "fruits"
  ggbeeswarm::geom_quasirandom(aes(x = x, y = y, color = I(color)),
                               method = "tukey", width = 0.42, data = fruits) +
  
  ### Places labels on the "trunk and roots" (budgets and number of orgs.)
  ggtext::geom_textbox(aes(x = xend, y = yend, label = value, fill = I(fill)),
                       box.color = NA, size = 3.5, width = unit(0.11, "npc"),
                       halign = 0.5, valign = 0.5, family = sans, data = trunk) +
  ggtext::geom_textbox(aes(x = x, y = y, label = n), fill = c_trunk,
                       box.color = NA, size = 6, width = unit(0.11, "npc"),
                       halign = 0.5, valign = 0.5, family = sans,
                       color = "white", data = trunk) +
  
  ### Places titles for the "roots"
  geom_text(aes(x = xend, y = yend, label = cat), nudge_y = 0.04, size = 4,
            hjust = 0, angle = 90, family = sans, lineheight = lnhgt,
            color = "white", data = trunk) +
  
  ### Facets the plots
  facet_wrap(~group, nrow = 2) +
  
  ### Gives unitary limits to the axes
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
  
  ### Eliminates unnecessary theme elements and customize others
  theme_void() +
  theme(
    strip.text = element_blank(),
    panel.spacing = unit(20, "pt")
  )

## Makes the main plot
p <- ggplot(NULL) +
  
  ### Places the labels for the percentages in the titles
  geom_tile(aes(x = x, y = y, fill = I(fill), width = width), height = 0.016,
            data = pct_labels) +
  
  ### Places the titles
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        fill = NA, label.color = NA, hjust = 0, vjust = 1,
                        family = hand, data = titles) +
  
  ### Gives unitary limits to the axes
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
  
  ### Eliminates unnecessary theme elements
  theme_void() +
  
  ### Places the "trees"
  patchwork::inset_element(tree, left = 0, right = 1,
                           bottom = 0, top = 0.7, on_top = FALSE)

## Saves the plot
ggsave("2022/week14/tree.png", plot = p, dpi = "retina",
       width = 20, height = 16)

