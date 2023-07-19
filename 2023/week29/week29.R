# 0. Initial setup ##########
## Loads libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggnewscale)
library(ggtext)
library(ggview)
library(glue)
library(htmltools)
library(junebug)
library(readr)
library(santoku)
library(stringr)
library(tidyr)

## Gets the data
rawData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv')

## Takes all font styles that share that exact family name and
## registers them (makes them visible to {systemfonts})
junebug::font_hoist("Font Awesome 6 Brands")
junebug::font_hoist("Geo")

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
## Keeps only the essays for TOEFL (human and AI-made)
data <- rawData |> 
  dplyr::filter(stringr::str_detect(name, "TOEFL"))

## Renames the prediction variable
data <- data |> 
  dplyr::rename("pred" = ".pred_AI")

## Cuts the prediction values into bins
data <- data |> 
  dplyr::mutate(bins = santoku::chop_width(pred, width = 0.1, start = 0))

## Counts the number of essays per bin, detector and writer
data <- data |> 
  dplyr::count(kind, detector, bins)

## Orders the "detector" column
levelsDetector <- c(
  "ZeroGPT", "GPTZero", "Crossplag", "HFOpenAI",
  "Quil", "OriginalityAI", "Sapling"
)
data <- data |> 
  dplyr::mutate(detector = factor(detector, levels = levelsDetector))

## Fills the empty categories with NA
data <- data |> 
  tidyr::complete(kind, detector, bins)

## Rearranges the order of the observations
data <- data |> 
  dplyr::arrange(kind, detector, bins)

# 2. Plot production ##########
## Defines the height of the tiles
tileHeight <- 0.4

## Defines y-axis coordinates for the tiles
tileY <- dplyr::tibble(
  y = rep(1:7, 2) + c(rep(tileHeight/2, 7), rep(-tileHeight/2, 7)),
  kind = c(rep("AI", 7), rep("Human", 7)),
  detector = rep(levelsDetector, 2)
)

## Adds the coordinates to the data
data <- data |> 
  dplyr::left_join(tileY)

## Defines coordinates for the detector labels
detectorLabel <- tileY |> 
  dplyr::group_by(detector) |> 
  dplyr::summarise(y = mean(y),
                   x = 0.2) |> 
  dplyr::ungroup()

## Defines the title
plotTitle <- "PROMPT ENGINEER IT<br>TILL YOU MAKE IT"

## Defines the subtitle
plotSubtitle <- "The GPT Arms Race may deepen inequalities among<br>
native and non-native English speakers."

## Defines the details
plotDetails <- tagList(
  "The", strong(" GPT detectors ", style = "color:#6a0475;"),
  "used on this analysis were provided with texts ", br(),
  "made by", strong(" Humans ", style = "color:#3065f4;"),
  "taking on TOEFL exams (English fluency tests) and by an", br(),
  strong(" AI ", style = "color:#d41727;"),
  "(GPT4 model) prompted to enhance texts like a native on a fake TOEFL exam.", br(),br(),
  "The heatmap on the right shows the distribution of these texts across", br(),
  "a scale that represents the probability of being made by an AI.", br(),
  "These probabilities were estimated by the GPT detectors.", br(),br(),
  "These detectors gave the GPT texts a very low likelihood of being AI-generated.", br(),
  "On the other hand, texts from non-native English speakers were", br(),
  "frequently flagged as AI-made. Indiscriminate use of these tools", br(),
  "may not accomplish their purpose while targeting vulnerable groups."
)
plotDetails <- as.character(plotDetails) |> 
  stringr::str_remove_all("\n")

## Defines the plot authorship
plotAuthorship <- tagList(
  'Data from: GPT detectors R package', br(),
  'Made by Ícaro Bernardes: ', br(),
  faDecoder('\uf099', ' - @IcaroBSC'), br(),
  faDecoder('\uf09b', ' - @IcaroBernardes'), br(),
  faDecoder('\uf08c', ' - @icarobsc')
)
plotAuthorship <- as.character(plotAuthorship) |> 
  stringr::str_remove_all("\n")

## Start the plot creation
plot <- NULL |> 
  ggplot() +
  
  ### Places the tiles on AI essays
  geom_tile(aes(x = bins, y = y, fill = n), color = "white", linewidth = 1,
            height = tileHeight, data = data |> dplyr::filter(kind == "AI")) +
  scale_fill_steps(low = "#c88c8d", high = "#d41727",
                   na.value = "#d7d7d7", name = NULL) +
  
  ### Allows a new fill scale to be defined
  ggnewscale::new_scale_fill() +
  
  ### Places the tiles on human essays
  geom_tile(aes(x = bins, y = y, fill = n), color = "white", linewidth = 1,
            height = tileHeight, data = data |> dplyr::filter(kind == "Human")) +
  scale_fill_steps(low = "#9899c7", high = "#3065f4",
                   na.value = "#d7d7d7", name = NULL) +
  
  ### Places some of the quantities in the bins
  geom_text(aes(x = bins, y = y, label = n), family = "Palanquin",
            data = data |> dplyr::filter(kind == "AI" & bins == "[0, 0.1)")) +
  geom_text(aes(x = bins, y = y, label = n), family = "Palanquin",
            data = data |> dplyr::filter(kind == "Human" & bins == "[0.9, 1]")) +
  
  ### Places the detector labels
  geom_text(aes(x = x, y = y, label = detector), family = "Jura",
            hjust = 1, size = 5, color = "#6a0475", data = detectorLabel) +

  ### Places the title
  annotate(
    "RichText", x = -15, y = 7.5, label = plotTitle,
    hjust = 0, vjust = 1, size = 28,
    fill = NA, label.colour = NA,
    lineheight = 0.7, family = "Geo Regular"
  ) +
  
  ### Places the subtitle
  annotate(
    "RichText", x = -15, y = 5.5, label = plotSubtitle,
    hjust = 0, vjust = 1, size = 8.5,
    fill = NA, label.colour = NA,
    lineheight = 1, family = "Jura"
  ) +
  
  ### Places the details
  annotate(
    "RichText", x = -15, y = 4.5, label = plotDetails,
    hjust = 0, vjust = 1, size = 5,
    fill = NA, label.colour = NA,
    lineheight = 1.2, family = "Palanquin"
  ) +
  
  ### Places the authorship
  annotate(
    "RichText", x = -15, y = 1.5, label = plotAuthorship,
    hjust = 0, vjust = 1, size = 3,
    fill = NA, label.colour = NA,
    lineheight = 1.2, family = "Palanquin"
  ) +
  
  ### Defines aesthetic rules
  scale_x_discrete(position = "top") +
  scale_y_continuous(expand = expansion(mult = c(0,0)))

## Extracts the plot legend
plotLegend <- plot + 
  theme(legend.position = "bottom") 
plotLegend <- ggpubr::get_legend(plotLegend)

## Continues plot creations
plot <- plot +
  
  ### Places the legend in a custom location on the plot
  annotation_custom(
    plotLegend,
    xmin = -7, xmax = -4,
    ymin = 0, ymax = 2
  ) +
  
  ### Places the legend title
  annotate(
    "text", x = -8, y = 1.1, label = "NUMBER OF\nTEXTS",
    hjust = 1, size = 6, fontface = "bold",
    lineheight = 1, family = "Palanquin"
  ) +
  
  ### Places the legend groups
  annotate(
    "text", x = c(-6.6, -4.25), y = 1.3, label = c("HUMAN", "AI"),
    vjust = 0, size = 4, fontface = "bold",
    family = "Palanquin"
  ) +
  
  ### Defines aesthetics elements
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(0.1, 0.05, 0.1, 0.05, "npc"),
    axis.text.x = element_text(size = 11, family = "Jura")
  )

## Generates an accurate preview of the final plot
ggview::ggview(
  plot = plot, device = "png",
  width = 6000, height = 3000, units = "px",
  dpi = 320, bg = "white"
)

## Generates the final plot
ggsave("2023/week29/week29.png", plot = plot,
       width = 6000, height = 3000, units = "px",
       dpi = 320, bg = "white")
