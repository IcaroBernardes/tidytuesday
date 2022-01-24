# 0. Library management
library(tidyverse)
library(ggplot2)
library(showtext)
library(geobr)
library(rmapshaper)
library(foreign)
library(tm)
library(tidytext)
library(stringi)
library(scales)
library(ggtext)
library(ggforce)
library(patchwork)
library(emojifont)

## Adding Google Fonts
font_add_google(name = "Lato", family = "lato")
sans <- "lato"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_auto()

# 1. Data download, load and handling
## Loads data on area (in km²) of all municipalities of the country (2020).
## Data from https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2020/Brasil/BR/
area <- foreign::read.dbf("extras/baresbr/data/BR_Municipios_2020.dbf") %>% 
  dplyr::mutate(CD_MUN = as.character(CD_MUN),
                CD_MUN = as.numeric(CD_MUN)) %>% 
  dplyr::rename("code_muni" = "CD_MUN",
                "area" = "AREA_KM2") %>% 
  dplyr::select(code_muni, area)

## Loads data on estimated population of all municipalities of the country (2020).
## Data from https://www.ibge.gov.br/estatisticas/sociais/populacao/9103-estimativas-de-populacao.html
population <- readRDS("extras/baresbr/data/estimativa_dou_2020.RDS")

## Loads the data of registered business that are named "bars"
baresbr <- readRDS("extras/baresbr/data/baresbr.RDS") %>% 
  dplyr::rename("code_muni" = "id_municipio")
numbars <- baresbr %>% 
  dplyr::count(code_muni, name = "bars")

## Loads the outline geometries of the states and federal district
## with eliminated small islands and complexity. Alternative code
## to download from IPEA and simplify geometry is commented bellow
# ufs <- geobr::read_state(code_state = "all", year = 2020) %>%
#   dplyr::mutate(geom = rmapshaper::ms_simplify(geom)) %>%
#   saveRDS("extras/baresbr/data/ufs.RDS")
ufs <- readRDS("extras/baresbr/data/ufs.RDS")

## Loads the outline geometry of the country with eliminated small
## islands and complexity. Alternative code to download from IPEA
## and simplify geometry is commented bellow
# br <- geobr::read_country(year = 2020) %>%
#   dplyr::mutate(geom = rmapshaper::ms_simplify(geom)) %>% 
#   saveRDS("extras/baresbr/data/br.RDS")
br <- readRDS("extras/baresbr/data/br.RDS")

## Loads the outline geometry of the municipalities with eliminated small
## islands and complexity. Alternative code to download from IPEA
## and simplify geometry is commented bellow
# mun <- geobr::read_municipality(code_muni = "all", year = 2020) %>%
#   dplyr::mutate(geom = rmapshaper::ms_simplify(geom, keep = 0.12)) %>% 
#   saveRDS("extras/baresbr/data/mun.RDS")
mun <- readRDS("extras/baresbr/data/mun.RDS")

## Joins all data (by city code)
mun <- mun %>%
  dplyr::left_join(numbars) %>%
  dplyr::left_join(area) %>%
  dplyr::left_join(population)

## Creates a function that takes a value between 0 and 1 and gives RGB numeric
## values resulting from the interpolation of two colors
colormaker1 <- grDevices::colorRamp(c('#f7fcf5','#e5f5e0','#c7e9c0','#a1d99b','#74c476'))
colormaker2 <- grDevices::colorRamp(c('#238b45','#006d2c','#00441b'))

## Calculates the bar density in the municipalities (by area and by population),
## also creates a color scale based in the amount of bars
mun <- mun %>%
  dplyr::mutate(bars = ifelse(is.na(bars), 0, bars),
                dens = bars/area,
                abund = bars/pop) %>%
  dplyr::mutate(fill1 = ifelse(bars <= 200, bars, NA),
                fill1 = scales::rescale(fill1, to = c(0,1)),
                fill2 = ifelse(bars > 200, bars, NA),
                fill2 = scales::rescale(fill2, to = c(0,1))) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(fill = ifelse(bars <= 200,
                              grDevices::rgb(colormaker1(fill1), maxColorValue = 255),
                              grDevices::rgb(colormaker2(fill2), maxColorValue = 255)
  )) %>%
  dplyr::ungroup()

## Calculates the correlation of spearman (as it is less afected by
## outliers than Pearson) between number of bars and area size and population
cor <- mun %>% 
  as_tibble() %>% 
  dplyr::select(bars, area, pop)
cor_area <- cor(cor$bars, cor$area, method = "spearman") %>% round(digits = 2)
cor_pop <- cor(cor$bars, cor$pop, method = "spearman") %>% round(digits = 2)

## Gets the most/least bar-dense cities in terms of area/population
filled <- mun %>% 
  as_tibble() %>% 
  dplyr::filter(bars != 0) %>% 
  dplyr::mutate(city = glue::glue("{name_muni} ({abbrev_state})")) %>% 
  dplyr::select(city, dens, abund)

dens_area_low_city <- filled %>%
  dplyr::slice_min(dens) %>% 
  dplyr::pull(city)
dens_area_low_value <- filled %>%
  dplyr::mutate(dens = 1/dens) %>% 
  dplyr::slice_max(dens) %>% 
  dplyr::pull(dens) %>% 
  round(digits = 0)

dens_pop_low_city <- filled %>%
  dplyr::slice_min(abund) %>% 
  dplyr::pull(city)
dens_pop_low_value <- filled %>%
  dplyr::mutate(abund = 1/abund) %>% 
  dplyr::slice_max(abund) %>% 
  dplyr::pull(abund) %>% 
  round(digits = 0)

dens_area_high_city <- filled %>%
  dplyr::slice_max(dens) %>% 
  dplyr::pull(city)
dens_area_high_value <- filled %>%
  dplyr::slice_max(dens) %>% 
  dplyr::pull(dens) %>% 
  round(digits = 0)

dens_pop_high_city <- filled %>%
  dplyr::slice_max(abund) %>% 
  dplyr::pull(city)
dens_pop_high_value <- filled %>%
  dplyr::mutate(abund = 1/abund) %>% 
  dplyr::slice_min(abund) %>% 
  dplyr::pull(abund) %>% 
  round(digits = 0)

## Gets two cities to illustrate how to read the plot
city_high <- mun %>% 
  dplyr::filter(bars == max(bars))

city_low <- mun %>% 
  dplyr::filter(bars <= 10) %>% 
  dplyr::filter(area == max(area))

## Gets the 1/3 top cities in amount of bars
high <- mun %>%
  dplyr::arrange(desc(bars)) %>%
  dplyr::mutate(pct = cumsum(bars)/sum(bars)) %>%
  dplyr::filter(lag(pct) <= 1/3) %>%
  as.data.frame()

## Gets the most used words in bars names
nomes <- baresbr %>% 
  dplyr::select(nome_fantasia) %>% 
  dplyr::rename("name" = "nome_fantasia") %>% 
  dplyr::mutate(name = tolower(name)) %>% 
  dplyr::mutate(name = tm::removePunctuation(name)) %>%
  dplyr::mutate(name = stringi::stri_trans_general(name, id = "Latin-ASCII")) %>% 
  dplyr::mutate(name = tm::removeNumbers(name)) %>% 
  dplyr::mutate(name = tm::stripWhitespace(name)) %>% 
  tidytext::unnest_tokens(txt, name) %>% 
  dplyr::filter(!is.na(txt)) %>% 
  dplyr::count(txt) %>% 
  dplyr::arrange(desc(n))

# 2. Plot making
## Creates coordinates for titles and subtitles
titles <- tibble(
  x = c(-80,-79.3,-80,-80),
  y = c(40,30.5,-40,-44.5),
  label = c(
    "BARS ALL AROUND",
    "Bars are a gathering place for many Brazilians and are intimately\nlinked to cultural symbols like Samba, Football and Caipirinha.\nHowever they are not solely Carnival and Leisure,\nbars also mirror curious aspects of Brazilian society and economy.",
    "WHAT'S IN A NAME?",
    "Bars names usually follow at least one between four main themes:"
  ),
  size = c(93,16,40,25),
  color = c(rep("black",2),rep("white",2)),
  fontface = rep(c("bold","plain"),2)
)

## Creates coordinates for acknowledgments
acknowledgments <- tibble(
  x = 18,
  y = 27,
  label = 'Data from Base dos Dados (https://basedosdados.org) and\nqueried by Fernando Barbalho (@barbalhofernand).\nThe data contains only registered business\nthat have the word "bar" in the name.\nGraphic by Ícaro Bernardes (@IcaroBSC)'
)

## Creates coordinates for instructions
instructions <- tibble(
  x = c(-79.2,-79.2),
  y = c(17,13.5),
  label = c(
    "HOW TO READ THIS MAP:",
    "The map shows the amount of registered bars in each municipality.\nDarker colors mean higher values.\nCities that have at most 200 bars follow a light green to green scale\nCities over 200 bars folllow a dark green scale."
  ),
  size = c(24,14),
  fontface = c("bold","plain")
)

## Creates coordinates for the examples lines
line_instr <- tibble(
  x = c(-12.3,-12.3,-16,
        5.4,5.4,9),
  xend = c(-12.3,-16,-19,
           5.4,9,12),
  y = c(14,15,15,
        10,11,11),
  yend = c(16,15,12,
           12,11,14)
)

## Creates coordinates for highlights
highlights <- tibble(
  x = rep(c(rep(-22.5,3),
            rep(8.5,3)),2),
  y = c(rep(c(0,-4,-9),2),
        rep(c(-19,-23,-28),2)),
  size = rep(c(17,12,8),4),
  fontface = rep(c("bold","plain","plain"),4),
  label = c(
    "EVERYWHERE?",
    "Out of 5570 Brazilian\ncities, only 222 don't\nhave any registered bars",
    "The distribution of registered bars is inequal.\nThe northern half of the country has\nway less bars than the south",
    "CAPITAL IS KING",
    glue::glue("80 cities concentrate\na third of all {round(dim(baresbr)[1]/1000)}K\nbars in the country"),
    "Most of them are capitals\nand neighboring cities or\nare located in rich states",
    "ROOM FOR ALL",
    glue::glue("City area and number of\nbars have too small of a\nmonotonic correlation (r = {cor_area})"),
    glue::glue("The most bar-packed city (in terms of area)\nis {dens_area_high_city} with {dens_area_high_value} bars per sq. km.\nOn the other hand, the least packed city is\n{dens_area_low_city} with one bar in {dens_area_low_value} sq. km"),
    "WHERE THE PEOPLE ARE",
    glue::glue("City population and number of\nbars have some monotonic\ncorrelation (r = {cor_pop})"),
    glue::glue("The most bar-packed city (in terms of population) is\n{dens_pop_high_city} with one bar for every {dens_pop_high_value} people.\nOn the other hand, the least packed city is\n{dens_pop_low_city} with one bar for every {dens_pop_low_value} people")
  )
)

## Creates legend info
legend <- tibble(
  x = -63,
  y = c(-30:-28,-26:-24)+0.7,
  ticks = c(0,100,200,5000,14000,28000),
) %>% 
  dplyr::mutate(fill1 = ifelse(ticks <= 200, ticks, NA),
                fill1 = scales::rescale(fill1, to = c(0,1)),
                fill2 = ifelse(ticks > 200, ticks, NA),
                fill2 = scales::rescale(fill2, to = c(0,1))) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(fill = ifelse(ticks <= 200,
                              grDevices::rgb(colormaker1(fill1), maxColorValue = 255),
                              grDevices::rgb(colormaker2(fill2), maxColorValue = 255))) %>% 
  dplyr::ungroup()

## Creates coordinates for themes of bar names
icons <- tibble(
  x = 0,
  y = 0,
  theme = 1:4,
  label = c("fork_and_knife","church","beers","key"),
  color = c("#1F5593","#B80C3D","#F59113","#E0BB01"),
  fill = c(rep("white",2),rep("black",2)),
  size = 100
)

motifs <- tibble(
  x = 0,
  y = rep(c(7.5,-7.5),4),
  theme = c(rep(1,2),
            rep(2,2),
            rep(3,2),
            rep(4,2)),
  label = c(
    "Business",
    "bars that do more<br>than only sell drinks",
    "Religious",
    "bars that draw protection<br>from Catholic saints",
    "Socializing",
    "bars that honor the idea<br>of getting together",
    "Ownership",
    "bars named after<br>the owner or founder"
  ),
  color = c(rep("#1F5593",2),
            rep("#B80C3D",2),
            rep("#F59113",2),
            rep("#E0BB01",2)),
  fill = c(rep("white",4),rep("black",4)),
  fontface = rep(c("bold","plain"),4),
  size = rep(c(19,9),4)
)

## Defines some layout constants
lnhgt <- 0.28
W <- 40
H <- 7
d <- W/2.5
A <- 5*pi/3
a <- tan(A)

## Creates coordinates for the bars "banners"
barribbons <- tibble(
  x = 0,
  y = 0,
  width = W,
  height = H,
  fill = c(rep("#1F5593",3),
           rep("#B80C3D",3),
           rep("#F59113",3),
           rep("#E0BB01",3)),
  color = c(rep("white",6),
            rep("black",6)),
  type = c(rep(1,3),
           rep(2,3),
           rep(3,3),
           rep(4,3)),
  order = rep(1:3,4),
  name = c(
    "Bar e Mercearia","Bar e Restaurante","Bar e Lanchonete",
    "Bar de Nossa Senhora Aparecida","Bar São José","Bar Santo Antônio",
    "Bar dos Amigos","Bar Ponto de Encontro","Bar Recanto",
    "Bar do Zé","Bar do João","Bar do Alemão"
  ),
  quant = c(
    "~120 000 bars","~40 000 bars","~40 000 bars",
    "~8 000 bars","~6 000 bars","~6 000 bars",
    "~10 000 bars","~4 000 bars","~3 000 bars",
    "~2 000 bars","~1 000 bars","~1 000 bars"
  )
) %>% 
  dplyr::mutate(name = toupper(name),
                size = stringr::str_length(name),
                size = scales::rescale(1/size, to = c(7,13))) %>% 
  dplyr::mutate(x0 = x-d,
                y0 = y,
                a = 2, 
                b = 1.5, 
                angle = A)

## Calculates the points for the "bottle" in the "banners"
x0 <- -d
y0 <- 0

y1 <- -H/2
x1 <- ((y1-y0) + a*x0)/a

x2 <- 1.2*x1
y2 <- -H/2

x5 <- 0.8*x1
y5 <- -H/2

y3 <- H/2
x3 <- ((y3-y2)+a*x2)/a

y4 <- H/2
x4 <- ((y4-y5)+a*x5)/a

barbottles <- tibble(
  x = c(x1,x2,x3,x4,x5,x1),
  y = c(y1,y2,y3,y4,y5,y1)
)

bottledrops <- tibble(
  x = c(-W/3,-W/2.2),
  y = c(-H/3.5,H/3.3),
  label = emojifont::fontawesome("fa-tint"),
  size = c(7,5)
)

## Creates the bars "banners"
barnames <- barribbons %>% 
  ggplot() +
  
  ### Places the "banner body"
  geom_tile(aes(x = x, y = y, width = width, height = height, fill = I(fill))) +
  
  ### Places the bar name
  geom_text(aes(x = x-(width/4), y = y, label = name, size = I(size), color = I(color)),
            family = sans, fontface = "bold", hjust = 0) +
  
  ### Places the approximated number of bars that have similar name
  geom_text(aes(x = x+(width/2.1), y = y-(height/2.8), label = quant,
                color = I(color)), size = 6, family = sans, hjust = 1, vjust = 0) +
  
  ### Places the "bottle"
  geom_polygon(aes(x = x, y = y), fill = "#904800", data = barbottles) +
  ggforce::geom_ellipse(aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle),
                        fill = "white") +
  
  ### Places "water drops in the bottle"
  geom_text(aes(x = x, y = y, label = label, size = I(size)), color = "#BD5E00C8",
            family = 'fontawesome-webfont', data = bottledrops) +
  
  ### Facets the plot
  facet_grid(order ~ type) +
  
  ### Guarantees proportion of the plot and eliminates
  ### extra space in the axes beyond the limits
  coord_equal(xlim = c(-W/2,W/2), ylim = c(-H/2,H/2), expand = FALSE) +
  
  ### Hides unnecessary plot elements including facet labels
  theme_void() +
  theme(
    strip.text = element_blank()
  )

## Creates the bar "coasters"
coasters <- icons %>% 
  ggplot() +
  
  ### Places the main part of the coasters
  ggforce::geom_regon(aes(x0 = x, y0 = y, sides = 4, r = 13,
                          angle = 0, fill = I(fill)),
                      radius = unit(1, 'cm')) +
  
  ### Places the border detail inside the coasters
  ggforce::geom_regon(aes(x0 = x, y0 = y, sides = 4, r = 11,
                          angle = 0, color = I(color)), fill = NA, size = 1,
                      radius = unit(1, 'cm')) +
  
  ### Places the emojis in the center
  geom_text(aes(x = x, y = y+19, label = emojifont::emoji(label), size = I(size),
                color = I(color)), family = 'EmojiOne') +
  
  ### Places the text inside the coasters
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size),
                            color = I(color), fill = I(fill),
                            fontface = I(fontface)), label.colour = NA,
                        family = sans, lineheight = lnhgt, data = motifs) +
  
  ### Facets the plot
  facet_grid(.~theme) +
  
  ### Sets the axes limits
  coord_equal(xlim = c(-10,10), ylim = c(-10,10), expand = FALSE) +
  
  ### Hides unnecessary plot elements including facet labels
  theme_void() +
  theme(
    strip.text = element_blank()
  )

## Creates the illustrative city plot (higher scale)
howto_high <- city_high %>% 
  ggplot() +
  geom_sf(aes(fill = I(fill)), color = "white", size = 0.05) +
  theme_void()

## Creates the illustrative city plot (lower scale)
howto_low <- city_low %>% 
  ggplot() +
  geom_sf(aes(fill = I(fill)), color = "white", size = 0.05) +
  theme_void()

## Generates the main plot
p <- mun %>% 
  ggplot() +
  
  ### Creates a line to divide main titles and instructions
  ggplot2::annotate("segment", x = -80, xend = 20, y = 20, yend = 20,
                    color = "#bf780d", size = 3, lineend = "round") +
  ggplot2::annotate("point", x = -30, y = 20, color = "#edb055", size = 20) +
  ggplot2::annotate("text", x = -30, y = 20.3, color = "#bf780d", size = 25,
                    label = fontawesome("fa-beer"), family = "fontawesome-webfont") +
  
  ### Creates a visual separation between parts of the main plot
  ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -37.35,
                    fill = "#216325", color = NA) +
  ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -37.75,
                    fill = "#409c46", color = NA) +
  ggplot2::annotate("point", x = seq(-85.6, 30.6, by = 3.2), y = -36.6,
                    shape = 17, size = 7, color = "#216325") +
  ggplot2::annotate("point", x = seq(-85, 30, by = 3.2), y = -37,
                    shape = 17, size = 7, color = "#409c46") +
  
  ### Creates a highlighted region on the upper part of the plot
  ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 2, ymax = -11.5,
                    fill = "white", color = NA) +
  ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = -17, ymax = -31,
                    fill = "white", color = NA) +
  
  ### Places the country outline
  geom_sf(fill = NA, color = "#edb055", size = 6, data = br) +
  
  ### Places the municipalities
  geom_sf(aes(fill = I(fill)), color = "white", size = 0.05) +
  
  ### Places the states and federal district outlines
  geom_sf(fill = NA, color = "#662506", size = 0.05, data = ufs) +
  
  ### Places titles and subtitles
  geom_text(aes(x = x, y = y, label = label, size = I(size),
                color = I(color), fontface = I(fontface)),
            family = sans, hjust = 0, vjust = 1,
            lineheight = lnhgt, data = titles) +
  
  ### Places acknowledgments
  geom_text(aes(x = x, y = y, label = label), size = 12, hjust = 1,
            family = sans, lineheight = lnhgt, data = acknowledgments) +
  
  ### Places instructions
  geom_text(aes(x = x, y = y, label = label, size = I(size), fontface = I(fontface)),
            family = sans, hjust = 0, vjust = 1,
            lineheight = lnhgt, data = instructions) +
  
  ### Places city examples
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend), data = line_instr) +
  ggplot2::annotate("text", x = -12, y = 15, size = 10, family = sans,
                    lineheight = lnhgt, hjust = 0,
                    label = glue::glue("{city_low$name_muni} ({city_low$abbrev_state}) has\n{city_low$bars} registered bars")) +
  ggplot2::annotate("text", x = 5, y = 11, size = 10, family = sans,
                    lineheight = lnhgt, hjust = 1,
                    label = glue::glue("{city_high$name_muni} ({city_high$abbrev_state}) has\n{round(city_high$bars/1000)}K registered bars")) +
  
  ### Places the highlights
  geom_text(aes(x = x, y = y, label = label, size = I(size), fontface = I(fontface)),
            family = sans, lineheight = lnhgt, data = highlights) +
  
  ### Places the legend
  ggplot2::annotate("text", x = -62.5, y = -20, label = "Number of registered\nbars in a city",
                    family = sans, lineheight = lnhgt, hjust = 1, size = 12, fontface = "bold") +
  geom_tile(aes(x = x, y = y, fill = I(fill)), color = "black",
            width = 1, height = 0.5, data = legend) +
  geom_text(aes(x = x-1, y = y, label = ticks), size = 8,
            family = sans, hjust = 1, data = legend) +
  
  ### Sets the axes limits
  coord_sf(xlim = c(-80,20), ylim = c(-82,38)) +
  
  ### Hides grids, axes ticks and so on
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#edb055", color = NA)
  ) +
  
  ### Places the shapes of the illustrative cities
  patchwork::inset_element(howto_high, left = 0.85, right = 0.95,
                           bottom = 0.71, top = 0.81, on_top = FALSE) +
  patchwork::inset_element(howto_low, left = 0.55, right = 0.65,
                           bottom = 0.71, top = 0.81, on_top = FALSE) +
  
  ### Places info about the the themes of bar names
  patchwork::inset_element(coasters, left = 0.03, right = 0.97,
                           bottom = 0, top = 0.42) +
  ### Places examples of bar names for each theme
  patchwork::inset_element(barnames, left = 0.03, right = 0.97,
                           bottom = 0.001, top = 0.12)

## Saves the plot
ggsave("extras/baresbr/baresbr.png", plot = p, dpi = "retina",
       width = 12, height = 14.4)

