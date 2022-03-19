# 0. Library and fonts management
library(tidyverse)
library(ggplot2)
library(showtext)
library(glue)
library(ggtext)
library(rgdal)
library(rgeos)
library(scales)
library(colorspace)
library(patchwork)

## Adding Google Fonts
sysfonts::font_add_google(name = "Merriweather", family = "merriweather") ### Serif
serif <- "merriweather"
sysfonts::font_add_google(name = "Raleway", family = "raleway") ### Sans Serif
sans <- "raleway"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

# 1. Data download, load and handling
rawdata <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

## Counts the number of available stations by state and fuel type and
## guarantees that every pairing of state and fuel type have a line
df <- rawdata %>% 
  dplyr::filter(STATUS_CODE == "E") %>% 
  dplyr::count(FUEL_TYPE_CODE, STATE, name = "stations") %>% 
  tidyr::complete(FUEL_TYPE_CODE, STATE)

## Gets the data on the number of cars (electric or combustion engine) by state
cars <- readr::read_delim("2022/week08/car_us.csv")

## Downloads the hexbin map data
download.file("https://raw.githubusercontent.com/holtzy/D3-graph-gallery/master/DATA/us_states_hexgrid.geojson.json", "2022/week08/USHEXGRID.json")

## Reads the map data
us <- rgdal::readOGR("2022/week08/USHEXGRID.json")

## Gets the centroids of each hex
centers <- data.frame(
  rgeos::gCentroid(us, byid = TRUE),
  id = us@data$iso3166_2
)

## Converts the map to a dataframe
us <- us %>% 
  ggplot2::fortify(region = "iso3166_2")

## Joins all data objects
df <- dplyr::right_join(df, us, by = c("STATE" = "id")) %>% 
  dplyr::left_join(cars)

## Calculates station availability (for each 100,000 cars)
df <- df %>% 
  dplyr::mutate(avail = round(100000*stations/CARS, 2))

## Creates labels for the countries
centers <- df %>%
  dplyr::distinct(FUEL_TYPE_CODE, STATE, avail) %>% 
  dplyr::full_join(centers, by = c("STATE" = "id")) %>% 
  dplyr::mutate(avail = ifelse(is.na(avail), 0.00, avail)) %>% 
  dplyr::mutate(label = glue::glue("{STATE}<br>{format(avail, nsmall = 2)}"))

## Defines the color palettes for each fuel type
palette <- tibble(
  FUEL_TYPE_CODE = c("BD", "CNG", "E85", "ELEC", "HY", "LNG", "LPG"),
  COLOR_FUN = c(
    grDevices::colorRamp(c(colorspace::lighten("#639046", 0.7, "relative"),"#639046")),
    grDevices::colorRamp(c(colorspace::lighten("#4a7aff", 0.7, "relative"),"#4a7aff")),
    grDevices::colorRamp(c(colorspace::lighten("#ae45bb", 0.7, "relative"),"#ae45bb")),
    grDevices::colorRamp(c(colorspace::lighten("#c04758", 0.7, "relative"),"#c04758")),
    grDevices::colorRamp(c(colorspace::lighten("#7e5890", 0.7, "relative"),"#7e5890")),
    grDevices::colorRamp(c(colorspace::lighten("#006a85", 0.7, "relative"),"#006a85")),
    grDevices::colorRamp(c(colorspace::lighten("#b06f38", 0.7, "relative"),"#b06f38"))
  )
)

## Joins the functions to the data
df <- df %>% 
  dplyr::left_join(palette)

## Creates a variable to scale the availability of stations
## between 0 and 1 inside each group of fuel type
df <- df %>% 
  dplyr::group_by(FUEL_TYPE_CODE) %>% 
  dplyr::mutate(scaled = scales::rescale(avail)) %>% 
  dplyr::ungroup() 

## Defines the fill colors for each hex.
## In the abscence of stations defines the color as grey
df <- df %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    color = ifelse(is.na(scaled),
                   "grey",
                   grDevices::rgb(do.call(COLOR_FUN, list(scaled)), maxColorValue = 255))
  ) %>% 
  dplyr::ungroup()

## Defines color for the labels 
centers <- df %>%
  dplyr::distinct(FUEL_TYPE_CODE, STATE, scaled) %>% 
  dplyr::mutate(color = ifelse((scaled < 0.65) | is.na(scaled), "black", "white")) %>% 
  dplyr::right_join(centers)

## Defines the coordinates for the background for prices and fuel names
bgprices <- tibble(
  xmin = -65, xmax = Inf,
  ymin = 32, ymax = 52,
  fill = c("#639046","#4a7aff","#ae45bb","#c04758","#7e5890","#006a85","#b06f38"),
  FUEL_TYPE_CODE = unique(df$FUEL_TYPE_CODE)
)

## Defines the coordinates for the prices and fuel names
prices <- tibble(
  x = -62,
  y = 42,
  FUEL_TYPE_CODE = unique(df$FUEL_TYPE_CODE),
  price = c(3.35,2.33,3.55,1.35,NA,2.45,4.34),
  label = c(
    "Biodiesel:<br>B20 and above",
    "Compressed<br>Natural Gas",
    "Ethanol: E85",
    "Electric",
    "Hydrogen",
    "Liquefied<br>Natural Gas",
    "Propane: LPG"
  )
) %>% 
  dplyr::mutate(
    price = ifelse(is.na(price), "NOT AVAILABLE", glue::glue("{price} US$ per GGE")),
    label = glue::glue("<span style='font-size:50px;'>{label}</span><br><br><span style='font-size:35px;'>{price}</span>")
  )

## Defines the coordinates for the titles
titles <- tibble(
  x = 0.025,
  y = c(0.96,0.82,0.11),
  size = c(25,10,12),
  label = c(
    "**Alternatives<br>for a seeable<br>Future**",
    
    'IPCC`s 2022 report alerts:<br>
    "Any further delay [...] will miss<br>
    a brief and rapidly closing<br>
    window of opportunity to<br>
    secure a liveable and<br>
    sustainable future for all"<br><br>
    The hexbin plots (on the right)<br>
    show the number of alternative<br>
    fuel stations currently available<br>
    in the US per 100,000 cars<br>
    by state and fuel type. Darker<br>
    colors mean higher availabilty.<br>
    Some fuels have little to no<br>
    presence in many states.<br><br>
    Cost is also a significative<br>
    issue. Some fuels are not as<br>
    cost-effective as gasoline. The<br>
    retail price of a gasoline gallon<br>
    was 3.25 US$ in 1-Oct-2021.<br>
    Prices shown in the plot take as<br>
    basis the energy of a galon of<br>
    gasoline. Then, for each fuel the<br>
    needed volume to produce the<br>
    same energy is calculated.<br>
    Therefore its price is<br>
    defined as dollars per<br>
    gasoline gallon equivalent.<br><br>
    Carbon footprint of each fuel in<br>
    this plot is lower than fossil fuels.<br>
    However, its important to keep in<br>
    mind the whole supply chain. An<br>
    electric car may be as harmful as<br>
    a gasoline-fueled if the eletricity<br>
    is produced from coal.<br><br>
    The important thing is to not give<br>
    up hope. If we want our descendants<br>
    to see a sustainable future we need<br>
    to work in the foreseeable future<br>
    to reduce our emissions.',
    
    "Data from: US DOT<br>
    Graphic by: Ícaro Bernardes<br>
    @IcaroBSC"
  )
)

## Defines some layout constants
lnhgt <- 1.2

# 2. Generates the plot
## Creates the facet of hexbins
hexbins <- df %>% 
  ggplot() +
  
  ### Places the hexagons
  geom_polygon(aes(x = long, y = lat, group = group, fill = I(color)),
               colour = "#F8F5E6", size = 2) +
  
  ### Places the labels
  ggtext::geom_richtext(aes(x = x, y = y, label = label, color = I(color)),
                        size = 3.5, fill = NA, label.color = NA, family = serif,
                        lineheight = lnhgt, data = centers) +
  
  ### Places a background for prices and fuel names
  geom_rect(aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax, fill = I(fill)), data = bgprices) +
  
  ### Places the prices and fuel names
  ggtext::geom_richtext(aes(x = x, y = y, label = label), fill = NA,
                        color = "white", label.color = NA, hjust = 0,
                        family = serif, lineheight = lnhgt, data = prices) +
  
  ### Facets the plot
  facet_grid(FUEL_TYPE_CODE ~ .) +
  
  ### Defines limits for the plot and applies the Mercator projection (default)
  coord_map(xlim = c(-138,-23)) +
  
  ### Eliminates unnecessary elements and customizes the plot
  theme_void() +
  theme(
    strip.text = element_blank(),
    plot.background = element_rect(fill = "#edebe4", color = NA) 
  )

p <- titles %>% 
  ggplot() +
  
  ### Places the titles
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)), fill = NA,
                        color = "black", label.color = NA, hjust = 0, vjust = 1,
                        family = sans, lineheight = lnhgt, data = titles) +
  
  ### Defines limits for the plot
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
  
  ### Eliminates unnecessary elements and customizes the plot
  theme_void() +
  theme(
    strip.text = element_blank(),
    plot.background = element_rect(fill = "#f7f7f5", color = NA) 
  ) +
  
  ### Places the hexbins plots in the main plot
  patchwork::inset_element(hexbins,
                           left = 0.385, right = 1,
                           bottom = 0, top = 1)

## Saves the plot
ggsave("2022/week08/fuels.png", plot = p, dpi = "retina",
       width = 20, height = 30)

