# 0. Initial setup ##########
## Loads libraries
library(boot)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggtext)
library(ggview)
library(glue)
library(htmltools)
library(junebug)
library(readr)
library(stringr)
library(tidyr)

## Gets the data
rawData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-15/spam.csv')

## Takes all font styles that share that exact family name and
## registers them (makes them visible to {systemfonts})
junebug::font_hoist("Font Awesome 6 Brands")

## Gets the info used to register the font families
fonts_register <- systemfonts::registry_fonts()

## Defines the font families
brandsFont <- "Font Awesome 6 Brands Regular"

## Creates a function that simplifies the process of placing 
## the Font Awesome glyphs on the text
faDecoder <- function(code, handle) {
  tagList(
    span(code, style = glue::glue("font-family:\"{brandsFont}\";")),
    handle
  )
}

# 1. Data handling ##########
## Keeps only proportion features
bootData <- rawData |> 
  dplyr::select(-crl.tot)

## Brings the values to the 0-1 range
bootData <- bootData |> 
  dplyr::mutate(across(
    .cols = -yesno,
    .fns = ~./100
  ))

## Generates bootstrap samples
### Defines the number of bootstrap samples
nSamples <- 5000

### Defines a function for the statistics (mean)
funMean <- function(data, indices) {
  mean(data[indices])
}

### Generates the samples
bootData <- bootData |> 
  dplyr::group_by(yesno) |> 
  dplyr::summarise(across(
    .cols = everything(),
    .fns = ~list(boot::boot(., statistic = funMean, R = nSamples))
  )) |> 
  dplyr::ungroup()

## Calculates the 95% CI by taking the percentiles of the estimates
### Creates a function that takes the list with
### the pair of samples and gives back the CI
funCI <- function(x) {
  result = boot::boot.ci(x, conf = 0.95, type = "perc")
  glue::glue_collapse(
    c(result$percent[4:5], result$t0),
    sep = "#"
  )
}

### Applies the function
CIData <- bootData |> 
  dplyr::mutate(across(
    .cols = -yesno,
    .fns = ~purrr::map(., funCI)
  ))

### Stacks the proportions into one column
CIData <- CIData |> 
  tidyr::pivot_longer(
    cols = -yesno,
    names_to = "feature"
  )

### Separates the values
CIData <- CIData |>
  tidyr::separate_wider_delim(
    cols = value,
    delim = "#",
    names = c("inf", "sup", "cnt")
  )

### Converts all values to numeric
CIData <- CIData |>
  dplyr::mutate(across(
    .cols = inf:cnt,
    .fns = as.numeric
  ))

## Orders the features
CIData <- CIData |>
  dplyr::mutate(feature = factor(
    feature,
    levels = c("make", "dollar", "money", "n000", "bang")
  ))

## Renames the features
newNames <- c(
  'word\n"make"' = "make",
  'dollar\nsign' = "dollar",
  'word\n"money"' = "money", 
  'string\n"000"' = "n000",
  'exclam.\nmark' = "bang"
)
CIData <- CIData |>
  dplyr::mutate(feature = forcats::fct_recode(feature, !!!newNames))

# 2. Plot production ##########
## Starts the plot creation
CIplot <- CIData |> 
  ggplot() +
  
  ### Places the range of the confidence interval
  geom_linerange(aes(ymin = inf, ymax = sup, x = yesno, color = yesno),
                 linewidth = 5, lineend = "round") +
  
  ### Places the central value of the confidence interval
  geom_point(aes(x = yesno, y = cnt, color = yesno), size = 8) +
  
  ### Places the plot title
  labs(title = "FREQUENCY OF CHARACTERS\nAS PERCENT OF THE TOTAL") +
  
  ### Defines aesthetic rules
  scale_color_discrete(type = c("y" = "#f7ea00", "n" = "#f7a9c3")) +
  scale_y_continuous(labels = scales::label_percent()) +
  
  ### Separates the plots by feature
  facet_grid(.~feature) +
  
  ### Defines aesthetics elements
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = "black"),
    plot.title = element_text(
      family = "Athiti", size = 30, color = "white",
      lineheight = 0.8, hjust = 0.5, face = "bold",
      margin = margin(0, 0, 10, 0, "pt")
    ),
    axis.text.y = element_text(family = "Athiti", size = 40, color = "white"),
    strip.text = element_text(family = "Athiti", size = 17,
                              color = "white", face = "bold",
                              margin = margin(10, 0, 10, 0, "pt")),
    panel.spacing = unit(0.1, "npc"),
    legend.position = "none"
  )

## Defines the title
plotTitle <- tagList(
  "NOW'S YOUR", br(),
  "CHANCE TO BE", br(),
  span("A BIG SHOT!!!", style = "color:#f7ea00;")
)

## Defines the subtitle
plotSubtitle <- "Spam emails tend to be<br>attention-grabbing<br>and full of money-traps."

## Defines the details
plotDetails <- tagList(
  "Spam emails range from nuisance to menace on the", br(),
  "internet. The dataset used on this graphic is", br(),
  "comprised of 4601 emails collected at HP Labs.", br(), br(),
  
  strong(
    "The lines on the right show the", br(),
    "95% confidence interval of ", br(),
    span("spam", style = "color:#f7ea00;"), " and ",
    span("normal", style = "color:#f7a9c3;"), " emails features.",
  ), br(), br(),
  
  "These features refer to the average frequency", br(),
  "of some characters on the emails.", br(),
  "These statistics were calculated", br(),
  "through Bootstrap resampling."
)
plotDetails <- as.character(plotDetails) |> 
  stringr::str_remove_all("\n")

## Defines the plot authorship
plotAuthorship <- tagList(
  'Data from: Rdatasets R package', br(),
  'Spamton sprite by: Temmie Chang', br(),
  'Made by Ícaro Bernardes: ', br(),
  faDecoder('\uf099', ' - @IcaroBSC'), br(),
  faDecoder('\uf09b', ' - @IcaroBernardes'), br(),
  faDecoder('\uf08c', ' - @icarobsc')
)
plotAuthorship <- as.character(plotAuthorship) |> 
  stringr::str_remove_all("\n")

## Creates the final plot
plot <- ggplot() +
  
  ### Defines aesthetic rules
  scale_x_continuous(limits = c(0,1), expand = expansion(add = 0)) +
  scale_y_continuous(limits = c(0,1), expand = expansion(add = 0)) +
  
  ### Places the Spamton sprite
  ggpath::geom_from_path(
    aes(x = 0, y = 1, width = 0.22, height = 0.22,
        path = '2023/week33/spamton.png'),
    hjust = 0.25, vjust = 1, angle = -5
  ) +
  
  ### Places the title
  annotate(
    "RichText", x = 0.12, y = 1, label = plotTitle,
    hjust = 0, vjust = 1, size = 15, angle = -5,
    fill = NA, label.colour = NA, color = "white",
    lineheight = 1, family = "Press Start 2P"
  ) +
  
  ### Places the subtitle
  annotate(
    "RichText", x = 0.63, y = 0.6, label = plotSubtitle,
    hjust = 0, vjust = 1, size = 10, angle = 9,
    fill = NA, label.colour = NA, color = "white",
    lineheight = 1.2, family = "Luckiest Guy"
  ) +
  
  ### Places the details
  annotate(
    "RichText", x = 0.02, y = 0.7, label = plotDetails,
    hjust = 0, vjust = 1, size = 8,
    fill = NA, label.colour = NA, color = "white",
    lineheight = 1.2, family = "Athiti"
  ) +
  
  ### Places the authorship
  annotate(
    "RichText", x = 0.02, y = 0, label = plotAuthorship,
    hjust = 0, vjust = 0, size = 7,
    fill = NA, label.colour = NA, color = "white",
    lineheight = 1.2, family = "Athiti"
  ) +
  
  ### Defines aesthetics elements
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = "black"),
    plot.margin = margin(0.1, 0.03, 0.1, 0.03, "npc")
  ) +
  
  ### Places the plot with the confidence intervals
  patchwork::inset_element(
    p = CIplot, on_top = FALSE,
    left = 0.5, bottom = 0,
    right = 1, top = 1,
  )

## Generates an accurate preview of the final plot
ggview::ggview(
  plot = plot, device = "png",
  width = 6000, height = 4000, units = "px",
  dpi = 320, bg = "black"
)

## Generates the final plot
ggsave("2023/week33/week33.png", plot = plot,
       width = 6000, height = 4000, units = "px",
       dpi = 320, bg = "black")
