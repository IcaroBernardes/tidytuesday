# 0. Library management
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(showtext)
library(readr)
library(shades)
library(scales)
library(patchwork)

## Adding Google Fonts
font_add_google(name = "Josefin Sans", family = "josefin") ### Sans Serif
sans <- "josefin"
font_add_google(name = "Antic Slab", family = "antic") ### Serif
serif <- "antic"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_auto()

# 1. Data download, load and handling
captured_vs_farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv')
consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-and-seafood-consumption-per-capita.csv')
stock <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-stocks-within-sustainable-levels.csv')
fishery <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/global-fishery-catch-by-sector.csv')
production <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')

## Data from World Bank (population and metadata on coutries)
pop <- readr::read_csv("week42/pop.csv", skip = 3)
metadata <- readr::read_csv("week42/metadata.csv")

# 2. How many of the highest per capita seafood consumers between
# low and lower middle income countries can be combined and still
# consume less seafood than the catches discarded

## Preparing the data
### Estimated mass of seafood consumed
consume <- metadata %>% 
  dplyr::filter(IncomeGroup %in% c("Low income","Lower middle income","Upper middle income","High income")) %>% 
  dplyr::left_join(pop) %>%
  dplyr::select(`Country Code`,`Country Name`,IncomeGroup,`1960`:`2020`) %>% 
  dplyr::rename(Code = "Country Code", Entity = "Country Name") %>% 
  tidyr::pivot_longer(cols = `1960`:`2020`, names_to = "Year", values_to = "pop") %>%
  dplyr::mutate(Year = as.numeric(Year)) %>%
  dplyr::mutate(IncomeGroup = factor(IncomeGroup, levels = c("Low income","Lower middle income","Upper middle income","High income"))) %>% 
  dplyr::left_join(consumption) %>% 
  dplyr::rename(percap = "Fish, Seafood- Food supply quantity (kg/capita/yr) (FAO, 2020)") %>%
  dplyr::left_join(captured_vs_farmed) %>% 
  dplyr::rename(consume = "Capture fisheries production (metric tons)") %>% 
  dplyr::select(-`Aquaculture production (metric tons)`) %>% 
  dplyr::mutate(percap = ifelse(is.na(percap), round(consume*1000/pop,2), percap))

### Comparison between consumption and discard
bycatch <- fishery %>% 
  dplyr::select(Year, Discards) %>%
  dplyr::full_join(consume) %>% 
  na.exclude() %>% 
  dplyr::group_by(Year, IncomeGroup) %>%
  dplyr::arrange(desc(percap), .by_group = TRUE) %>%
  tidyr::nest() %>% 
  #### Calculates how many countries are in each year and income category
  dplyr::mutate(qnt = map(data, . %>% dplyr::summarise(n()) %>% dplyr::pull())) %>%
  #### Creates a gradient of colors for each group.
  #### Uses different palettes for each income category
  dplyr::mutate(colors = case_when(
    IncomeGroup == "Low income" ~ map(qnt, ~shades::gradient(c("#bd0808","#cf9b9b"), ., space="Lab")),
    IncomeGroup == "Lower middle income" ~ map(qnt, ~shades::gradient(c("#0077b0","#82a1b0"), ., space="Lab")),
    IncomeGroup == "Upper middle income" ~ map(qnt, ~shades::gradient(c("#1111d9","#8888d1"), ., space="Lab")),
    TRUE ~ map(qnt, ~shades::gradient(c("#00ad11","#72a177"), ., space="Lab"))
  )) %>%
  #### Converts the shade char vector into a data.frame like object
  dplyr::mutate(colors = map(colors, tibble)) %>%
  dplyr::mutate(data = map2(data, colors, cbind)) %>%
  dplyr::select(-qnt, -colors) %>%
  tidyr::unnest(cols = data) %>% 
  dplyr::rename(contrast = "<shade>") %>%
  dplyr::ungroup() %>% 
  dplyr::group_by(Year) %>% 
  dplyr::mutate(cover = cumsum(consume),
                covered = cumsum(consume) <= Discards) %>% 
  dplyr::mutate(order = n():1) %>% 
  dplyr::ungroup()

### Nº of countries covered by discard
discard <- bycatch %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(covered = sum(covered),
                   discard = unique(Discards))

### Pct of waste
waste <- fishery %>% 
  dplyr::mutate(total = rowSums(.[4:8])) %>% 
  dplyr::mutate(waste = round(100*Discards/total,1)) %>% 
  dplyr::select(Year, waste) %>% 
  dplyr::filter(Year %in% unique(bycatch$Year)) %>% 
  #### Associates each value with a color
  dplyr::mutate(color = col_numeric(colorRamp(c("#d4cdba","#d9a000"), space = "Lab", interpolate = "linear"),c(8,24))(waste))

### Compares pct of waste from different data sources
compare <- bycatch %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(discard = unique(Discards),
                   consume = sum(consume),
                   .groups = "keep") %>% 
  dplyr::transmute(pct = round(100*discard/consume,1)) %>% 
  dplyr::ungroup() %>% 
  dplyr::full_join(waste)

### Calculates the rate of catching
bycatch %>% 
  dplyr::group_by(Year, IncomeGroup) %>% 
  dplyr::summarise(consume = sum(consume)) %>% 
  dplyr::group_by(IncomeGroup) %>% 
  dplyr::mutate(rate = round(100*(lead(consume)-consume)/consume,1)) %>% 
  ggplot(aes(x=Year, y=rate, color = IncomeGroup)) +
  geom_point() +
  geom_line() +
  theme_ipsum()

## Axis texts and titles
{
  xinf <- 1950
  xsup <- 2043
  yinf <- -7
  ysup <- 101
  xtit <- "Year"
  ytit <- "Mass of captured seafood\n(million tonnes)"
  lattit <- "Pct. of discard\nof caught fishes"
  xbrk <- seq(1960,2010,by=10)
  ybrk <- seq(0,100,by=10)
  }

## Defines some layout constants
{
  lnhgt <- 0.29 ### Height of lines of text
  insfnt <- 10 ### Font size of insights
  intfnt <- 14 ### Font size of instructions
  nudgey <- 1.5 ### Nudge of text in y-axis to emulate line break
}

## Titles and subtitles
{
  title <- "No Fish to Waste!"
  
  subtitle1 <- 
    paste(
      "Discard adds pressure to the seafood stocks without attending human demand.",
      "The good news: data shows that waste is going down in absolute and relative to the overall catch.",
      "The concerning facts: catching rates are not diminishing very much.",
      sep = "\n"
    )
  
  subtitle2 <- 
    paste("Capture fisheries production: Food and Agriculture Organization of the United Nations",
          "Estimated population of countries: World Bank",
          "Fishery catch by category: Pauly and Zeller (2016)",
          "Graphic: Ícaro Bernardes (https://github.com/IcaroBernardes/tidytuesday/tree/main/week42)",
          sep = "\n")
  }

## Instructions
int <- c(
  ### Instruction 01
  "The bars show the production of capture fisheries of each country.",
  "'They are colored (hue) by their income group: '*phantom('High')*', '*phantom('Upper middle')*', '*phantom('Lower middle')*' and '*phantom('Low.')",
  "phantom('They are colored (hue) by their income group: ')*'High'*phantom(', ')*phantom('Upper middle')*phantom(', ')*phantom('Lower middle')*phantom(' and ')*phantom('Low.')",
  "phantom('They are colored (hue) by their income group: ')*phantom('High')*phantom(', ')*'Upper middle'*phantom(', ')*phantom('Lower middle')*phantom(' and ')*phantom('Low.')",
  "phantom('They are colored (hue) by their income group: ')*phantom('High')*phantom(', ')*phantom('Upper middle')*phantom(', ')*'Lower middle'*phantom(' and ')*phantom('Low.')",
  "phantom('They are colored (hue) by their income group: ')*phantom('High')*phantom(', ')*phantom('Upper middle')*phantom(', ')*phantom('Lower middle')*phantom(' and ')*'Low.'",
  paste("The color lightness is defined by the rank of percapita seafood consumption withing the group.",
        "Darker groups are dominated by relatively high percapita consumers.",
        sep = "\n"),
  
  ### Instruction 02
  "The point-and-line series shows",
  "'the mass of '*phantom('discards')*' of caught seafood'",
  "phantom('the mass of ')*'discards'*phantom(' of caught seafood')"
  
)

## Creates plots to emulate a "zoom" effect
lilcatch1 <- bycatch %>% 
  dplyr::filter(Year >= 2009) %>% 
  dplyr::filter(between(cover/1e+06,45,65))

lilcatch2 <- bycatch %>% 
  dplyr::filter(Year >= 2009) %>% 
  dplyr::filter(between(cover/1e+06,0,20))

lildiscard <- discard %>% 
  dplyr::filter(Year >= 2009)

p1 <- lilcatch1 %>% 
  ggplot(aes(x = Year, y = consume/1e+06)) +
  geom_bar(aes(group = order), stat = "identity", fill = lilcatch1$contrast, color = "white", size = 0.1) +
  annotate("point", x = 2010, y = 10, color = "white", fill = NA, shape = 21, size = 23, stroke = 70) +
  annotate("point", x = 2010, y = 10, color = "black", fill = NA, shape = 21, size = 23, stroke = 3) +
  coord_fixed(xlim = c(2008.5,2010.5), ylim = c(0,20), ratio = 1/5, expand = FALSE, clip = "off") +
  theme_void()

p2 <- lilcatch2 %>% 
  ggplot(aes(x = Year, y = consume/1e+06)) +
  geom_bar(aes(group = order), stat = "identity", fill = lilcatch2$contrast) +
  geom_point(aes(y = discard/1e+06), color = "#c99400", data = lildiscard, size = 4) +
  geom_line(aes(y = discard/1e+06), color = "#c99400", data = lildiscard, size = 2) +
  annotate("point", x = 2010, y = 10, color = "white", fill = NA, shape = 21, size = 23, stroke = 70) +
  annotate("point", x = 2010, y = 10, color = "black", fill = NA, shape = 21, size = 23, stroke = 3) +
  coord_fixed(xlim = c(2008.5,2010.5), ylim = c(0,20), ratio = 1/5, expand = FALSE, clip = "off") +
  theme_void()

## Creates the graph
p <- bycatch %>% 
  ggplot(aes(x = Year, y = consume/1e+06)) +
  ### Caught seafood
  geom_bar(aes(group = order), stat = "identity", fill = bycatch$contrast) +
  ### Absolute amount of discard
  geom_point(aes(y = discard/1e+06), color = "#c99400", data = discard) +
  geom_line(aes(y = discard/1e+06), color = "#c99400", data = discard) +
  ### Pct. of discard
  geom_tile(aes(y = -1), height = 1.5, width = 0.9, fill = waste$color, data = waste) +
  geom_text(aes(y = -1, label = waste), family = serif, size = 9, data = waste) +
  ### Titles and subtitles
  annotate("text", x = xinf+3, y = ysup, label = title, hjust = 0, vjust = 1, family = sans, size = 90, fontface = "bold") +
  annotate("text", x = xinf+3.3, y = ysup-8.6, label = subtitle1, hjust = 0, vjust = 1, family = sans, size = 22, lineheight = lnhgt) +
  annotate("text", x = xinf+3.3, y = ysup-16.5, label = subtitle2, hjust = 0, vjust = 1, family = sans, size = 17, lineheight = lnhgt) +
  ### Axes
  annotate("text", x = xbrk, y = yinf+3.5, label = xbrk, family = sans, size = 20) +
  annotate("text", x = xinf+1.5, y = ybrk, label = ybrk, family = sans, size = 20) +
  annotate("text", x = 2011, y = yinf+1, label = xtit, hjust = 1, vjust = 1, family = sans, size = 30) +
  annotate("text", x = xinf, y = 100, label = ytit, hjust = 1, vjust = 0,
           angle = 90, family = sans, size = 30, lineheight = lnhgt) +
  annotate("text", x = 1958.5, y = -1, label = lattit, hjust = 1, vjust = 0.5,
           family = serif, size = 16, lineheight = lnhgt) +
  ### Instructions
  #### Instruction 01
  annotate("text", x = 2015.5, y = 66.2, hjust = 0, vjust = 1, label = int[1], family = serif, size = intfnt, lineheight = lnhgt) +
  annotate("text", x = 2015.5, y = 66.2-nudgey, hjust = 0, vjust = 1, label = int[2],
           family = serif, size = intfnt, lineheight = lnhgt, parse = TRUE, color = "black") +
  annotate("text", x = 2015.5, y = 66.2-nudgey, hjust = 0, vjust = 1, label = int[3],
           family = serif, size = intfnt, lineheight = lnhgt, parse = TRUE, color = "#00ad11") +
  annotate("text", x = 2015.5, y = 66.2-nudgey, hjust = 0, vjust = 1, label = int[4],
           family = serif, size = intfnt, lineheight = lnhgt, parse = TRUE, color = "#1111d9") +
  annotate("text", x = 2015.5, y = 66.2-nudgey, hjust = 0, vjust = 1, label = int[5],
           family = serif, size = intfnt, lineheight = lnhgt, parse = TRUE, color = "#0077b0") +
  annotate("text", x = 2015.5, y = 66.2-nudgey, hjust = 0, vjust = 1, label = int[6],
           family = serif, size = intfnt, lineheight = lnhgt, parse = TRUE, color = "#bd0808") +
  annotate("text", x = 2015.5, y = 66.2-2*nudgey-lnhgt, hjust = 0, vjust = 1, label = int[7], family = serif, size = intfnt, lineheight = lnhgt) +
  annotate("path", x = c(2010.7,2013.4,2013.4), y = c(56,56,60.3), size = 2) +
  annotate("point", x = 2010, y = 56, color = "black", fill = NA, shape = 21, size = 11, stroke = 1.6) +
  #### Instruction 02
  annotate("text", x = 2015.5, y = 18, hjust = 0, vjust = 1, label = int[8], family = serif, size = intfnt, lineheight = lnhgt) +
  annotate("text", x = 2015.5, y = 18-nudgey, hjust = 0, vjust = 1, label = int[9],
           family = serif, size = intfnt, lineheight = lnhgt, parse = TRUE, color = "black") +
  annotate("text", x = 2015.5, y = 18-nudgey, hjust = 0, vjust = 1, label = int[10],
           family = serif, size = intfnt, lineheight = lnhgt, parse = TRUE, color = "#c99400") +
  annotate("path", x = c(2010.7,2013.4,2013.4), y = c(9.7,9.7,14), size = 2) +
  annotate("point", x = 2010, y = 9.7, color = "black", fill = NA, shape = 21, size = 11, stroke = 1.6) +
  ylim(yinf,ysup) + xlim(xinf,xsup) +
  theme_ipsum() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

## Inserts the plots in the bigger plot
p <- p +
  patchwork::inset_element(p2, 0.58, 0.17, 0.73, 0.32, on_top = FALSE) +
  patchwork::inset_element(p1, 0.58, 0.56, 0.73, 0.71, on_top = FALSE)

## Saves the plot
ggsave("2020/week42/waste.png", plot = p, width = 70, height = 40, units = "cm")

