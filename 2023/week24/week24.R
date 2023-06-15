# 0. Initial setup ##########
## Loads libraries
library(dplyr)
library(gghighlight)
library(ggplot2)
library(ggtext)
library(ggview)
library(htmltools)
library(junebug)
library(patchwork)
library(readr)
library(tidyr)

## Gets the raw data on the villages
rawData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-13/safi_data.csv')

## Gets climate data on Tanzania
tzData <- readr::read_csv("2023/week24/tanzania.csv")

## Gets climate data on Mozambique
mbData <- readr::read_csv("2023/week24/mozambique.csv")

## Takes all font styles that share that exact family name and
## registers them (makes them visible to {systemfonts})
junebug::font_hoist("Font Awesome 6 Brands")

## Gets the info used to register the font families
fonts_register <- systemfonts::registry_fonts()

## Defines the font families
bodyFont <- "Ubuntu"
brandsFont <- "Font Awesome 6 Brands Regular"

# 1. Data handling ##########
## Separates the "months_lack_food" column, standardize month abbreviations
## and calculates how many househoulds had hunger in each month
hunger <- rawData |> 
  dplyr::select(months_lack_food, no_membrs) |> 
  tidyr::separate_rows(months_lack_food, sep = ";") |> 
  dplyr::filter(months_lack_food != "none") |> 
  dplyr::mutate(
    months_lack_food = factor(months_lack_food),
    months_lack_food = forcats::fct_recode(
      months_lack_food,
      Sep = "Sept",
      Jun = "June",
      Jul = "July"
    ),
    months_lack_food = forcats::fct_relevel(months_lack_food, month.abb)
  ) |> 
  dplyr::count(months_lack_food)

## Calculates how many households
## suffered from hunger for at least one month
hungerHouses <- rawData |> 
  dplyr::filter(months_lack_food != "none") |> 
  nrow()

## Orders the months in the precipitation tibbles
tzData <- tzData |> 
  dplyr::mutate(Category = factor(Category, month.abb))
mbData <- mbData |> 
  dplyr::mutate(Category = factor(Category, month.abb))

# 2. Poster production ##########
## Calculates mean number of households 
## suffering from hunger among the months
meanHunger <- mean(hunger$n)

## Creates the plot on hunger and
## highlights months above average
plotHunger <- hunger |> 
  ggplot() +
  
  ### Places the bars
  geom_col(aes(x = months_lack_food, y = n), fill = "#D1352A") +
  
  ### Highlights some bars according to the rule
  gghighlight::gghighlight(
    n > meanHunger,
    unhighlighted_params = list(fill = "#8C5650")
  ) +
  
  ### Places a guideline at zero
  geom_hline(yintercept = 0) +
  
  ### Places a text on hunger among households
  annotate("richText", x = 3.8, y = 48, hjust = 1, 
           size = 22, color = "#D1352A", label.colour = NA,
           label.padding = unit(c(0.75, 0.5, 0.5, 0.5), "lines"),
           fontface = "bold", family = bodyFont, label.r = unit(0, "lines"),
           label = hungerHouses) +
  annotate("richText", x = 3.8, y = 48, hjust = 0,
           size = 6.8, color = "#D1352A", label.colour = NA,
           label.padding = unit(c(0.75, 0.5, 0.5, 0.5), "lines"),
           lineheight = 1, family = bodyFont, label.r = unit(0, "lines"),
           label = "households declared<br>to have lacked enough<br>food at least once") +
  
  ### Reverses the y-axis
  scale_y_reverse() +
  
  ### Defines aesthetics elements
  labs(x = "Month", y = "Households lacking food") +
  theme_minimal() +
  theme(
    text = element_text(family = bodyFont),
    plot.title = element_text(size = 30, face = "bold"),
    plot.margin = margin(15, 30, 30, 30)
  )

## Creates a function that shows typical monthly precipitation
## and highlights months bellow average
rainBringer <- function(data) {
  
  ### Calculates mean precipitation among the months
  meanRain = mean(data$Precipitation)
  
  ### Defines title of the plot as function of the object name
  title = as.character(substitute(data))
  title = case_when(title == "tzData" ~ "Tanzania",
                    title == "mbData" ~  "Mozambique",
                    TRUE ~ "")
  
  ### Creates the plot
  data |> 
    ggplot(aes(x = Category, y = Precipitation)) +
    geom_col(fill = "#2766CC") +
    gghighlight::gghighlight(
      Precipitation < meanRain,
      unhighlighted_params = list(fill = "#4D6B8C")
    ) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(0, 250)) +
    labs(x = "Month", y = "Precipitation [mm]",
         title = title) +
    theme_minimal() +
    theme(
      text = element_text(family = bodyFont),
      plot.title = element_text(size = 30, face = "bold"),
      plot.margin = margin(30, 30, 0, 30)
    )
  
}

## Applies the rainBringer() function
tzRains <- rainBringer(tzData)
mbRains <- rainBringer(mbData)

## Creates a function that simplifies the process of placing 
## the Font Awesome glyphs on the text
faDecoder <- function(code, handle) {
  tagList(
    span(code, style = glue::glue("font-family:\"{brandsFont}\";")),
    handle
  )
}

## Creates the title and information text of the piece
infotext <- tagList(
  '131 househoulds on the villages of Chirodzo, God and Ruaca in Mozambique
  and Tanzania were interviewed for the Studying African Farmer-Led
  Irrigation (SAFI) survey. Among many questions on living conditions,
  one focused on food scarcity. People were asked to indicate which months
  in the 12 months prior the interview they did ',
  strong('not have enough food', style = "color:#8C5650;"),
  ' to feed their household. Declared ',
  strong('food shortage peaks', style = "color:#D1352A;"),
  'from',
  strong('September to January.', style = "color:#D1352A;"),
  'This partially aligns with ',
  strong('precipitation', style = "color:#4D6B8C;"),
  ' data of the countries. From ',
  strong('May to October', style = "color:#2766CC;"),
  ' both lands suffer from ',
  strong('shortage of rains.', style = "color:#2766CC;"), br(), br(),
  'Food shortage data from: Studying African Farmer-Led Irrigation (SAFI) survey', br(),
  'Precipitation data from: the Climatic Research Unit of University of East Anglia', br(),
  'Made by Ícaro Bernardes: ',
  faDecoder('\uf099', '@IcaroBSC – '),
  faDecoder('\uf09b', '@IcaroBernardes – '),
  faDecoder('\uf08c', '@icarobsc')
)
infotext <- as.character(infotext)

## Creates a plot for the title and information text of the piece
headPlot <- ggplot(NULL) +
  annotate("text", x = 0, y = 1, hjust = 0, vjust = 0,
           family = bodyFont, size = 13, fontface = "bold",
           label = "DRY MONTHS, FAMINE AHEAD") +
  annotate("TextBox", x = 0, y = 1, hjust = 0, vjust = 1.1,
           family = bodyFont, size = 4, box.colour = NA,
           width = unit(0.95, "npc"), label = infotext) +
  scale_x_continuous(expand = expansion(mult = c(0, 1))) +
  scale_y_continuous(expand = expansion(mult = c(2, 0.7))) +
  theme_void()

## Assembles the poster
plot <- headPlot / (tzRains + mbRains) / plotHunger + 
  patchwork::plot_layout(heights = c(2,1,1.4))

## Generates an accurate preview of the final plot
ggview::ggview(
  plot = plot, device = "png",
  width = 3000, height = 3000, units = "px",
  dpi = 320, bg = "white"
)

## Generates the final plot
ggsave("2023/week24/week24.png", plot = plot,
       width = 3000, height = 3000, units = "px",
       dpi = 320, bg = "white")
