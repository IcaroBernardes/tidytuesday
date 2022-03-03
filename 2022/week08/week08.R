############################ DuBoisChallenge ####################################
# 0. Library and fonts management
library(tidyverse)
library(ggplot2)
library(showtext)
library(glue)
library(ggtext)
library(emojifont)

## Adding Google Fonts
sysfonts::font_add_google(name = "Teko", family = "teko") ### Sans Serif
sans <- "teko"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

## Defines some layout constants
lnhgt <- 0.9
bgcolor <- "#dacbb8"
gridcolor <- "#cf7269"
bottom_grid <- 1000
top_grid <- 1099
x_pos <- seq(2012,2018,0.25)
y_pos <- seq(bottom_grid,top_grid,3)
legend_pos <- sort(seq(length(y_pos)-8,10,-8))

# 1. Data download, load and handling
## Data on inequality comes from IBGE
## https://www.ibge.gov.br/estatisticas/sociais/populacao/25844-desigualdades-sociais-por-cor-ou-raca.html?=&t=resultados
rawdata <- read.csv2("2022/week08/data.csv") %>% 
  dplyr::arrange(year)

## Filters only data of the income of black people in informal jobs
df <- rawdata %>% 
  dplyr::filter(race == "black") %>% 
  dplyr::select(informal, year)

## Adds a variable to indicate the turning point of Brazil's economy
df <- df %>% 
  dplyr::mutate(era = ifelse(year <= 2014, bgcolor, "black"))

## Creates two extra rows that repeat the first and last data in order to
## recreate the original visual. These rows follow the adjacent lines
n_years <- dim(df)[1]

### Defines coordinates of the start
a_start <- (df$informal[2]-df$informal[1])/(df$year[2]-df$year[1])
b_start <- df$informal[1]-a_start*df$year[1]
x_start <- df$year[1]-0.1
y_start <- a_start*x_start + b_start

### Defines coordinates of the end
a_end <- (df$informal[n_years]-df$informal[n_years-1])/(df$year[n_years]-df$year[n_years-1])
b_end <- df$informal[n_years]-a_end*df$year[n_years]
x_end <- df$year[n_years]+0.1
y_end <- a_end*x_end + b_end

ends <- tibble(
  informal = c(y_start,y_end),
  year = c(x_start,x_end),
  era = c(bgcolor, "black")
)
df <- rbind(ends[1,],df,ends[2,])

## Defines coordinates to smooth the
## appearance of the join between the geom_line
joints <- df %>% 
  dplyr::filter(year == 2014)

## Defines coordinates for the plot grid
gridX <- tibble(
  y = y_pos,
  xmin = 2012,
  xmax = 2018
) %>% 
  dplyr::mutate(color = ifelse(row_number() == 1L | row_number() == n(),
                               "black",
                               gridcolor))

gridY <- tibble(
  x = x_pos,
  ymin = bottom_grid,
  ymax = top_grid
) %>% 
  dplyr::arrange(x) %>% 
  dplyr::mutate(color = ifelse(row_number() == 1L | row_number() == n(),
                               "black",
                               gridcolor))

## Defines coordinates for the grid on the legend on the left of the plot
legendX <- tibble(
  y = y_pos,
  xmin = 2010,
  xmax = 2011
) %>% 
  dplyr::mutate(color = ifelse(row_number() == 1L | row_number() == n(),
                               "black",
                               gridcolor))

legendY <- tibble(
  x = c(2010,2011),
  ymin = bottom_grid,
  ymax = top_grid,
  color = "black"
)

## Defines coordinates for the numbers, ticks, icons
## and title on the legend on the left of the plot
legendNUM <- tibble(
  x = 2010.5,
  y = legendX$y[legend_pos],
  label = glue("{y}.00"),
)

legendTICKS <- tibble(
  y = legendNUM$y,
  xmin = 2011.1,
  xmax = 2012,
  color = gridcolor
)

legendICON <- tibble(
  x = 2010.2,
  y = c(legendX$y[legend_pos[length(legend_pos)]+3],
        legendX$y[legend_pos[length(legend_pos)]+5],
        legendX$y[legend_pos-3],
        legendX$y[legend_pos-5]),
  label = fontawesome("fa-dollar"),
)

legendTITLE <- tibble(
  x = 2010.5,
  y = top_grid-0.5,
  label = "INCOME (BRL)",
)

## Defines coordinates for the numbers on the horizontal axis
axisX <- tibble(
  x = 2012:2018,
  y = bottom_grid-1,
  label = 2012:2018,
)

## Creates a tibble for the average exchange rates from
## Brazilian Real (BRL) to US Dollar (USD) by https://www.exchangerates.org.uk
exchange_rates <- tibble(
  year = 2012:2018,
  rate = c(0.5137,0.4651,0.4263,
           0.305,0.2885,0.3134,0.2755)
)

## Gets some values of interest
max_blk_inf <- df %>% 
  dplyr::filter(informal == max(informal)) %>% 
  dplyr::pull(informal)
year_blk_inf <- df %>% 
  dplyr::filter(informal == max(informal)) %>% 
  dplyr::pull(year)
rate_blk_inf <- exchange_rates %>% 
  dplyr::filter(year == year_blk_inf) %>% 
  dplyr::pull(rate)
pct_wht_inf <- rawdata %>% 
  dplyr::filter(race == "white" & year == year_blk_inf) %>% 
  dplyr::pull(informal)
pct_wht_inf <- round(100*(pct_wht_inf-max_blk_inf)/max_blk_inf)
pct_blk_frm <- rawdata %>% 
  dplyr::filter(race == "black" & year == year_blk_inf) %>% 
  dplyr::pull(formal)
pct_blk_frm <- round(100*(pct_blk_frm-max_blk_inf)/max_blk_inf)
pct_wht_frm <- rawdata %>% 
  dplyr::filter(race == "white" & year == year_blk_inf) %>% 
  dplyr::pull(formal)
pct_wht_frm <- round(100*(pct_wht_frm-max_blk_inf)/max_blk_inf)

## Defines coordinates for the titles
titles <- tibble(
  x = 2014.2,
  y = c(top_grid+30,top_grid+10,bottom_grid-8,bottom_grid-38),
  size = c(40,15,20,16),
  label = c(glue::glue("REAL INCOME OF BLACK PEOPLE WHOSE<br>MAIN WORK IS INFORMAL."),
            glue::glue("AVERAGE USUAL INCOME IS CALCULATED TAKING INTO ACCOUNT PEOPLE OVER 14 THAT
                       <br>WERE WORKING IN THE WEEK THAT PRECEDED THE INTERVIEW."),
            glue::glue("IN {year_blk_inf} BLACK PEOPLE WITH INFORMAL JOBS HAD THEIR BEST YEAR IN THE SERIES.
                       <br>THEY EARNED IN AVERAGE {max_blk_inf}.00 BRL ({round(max_blk_inf*rate_blk_inf, 2)} USD) PER MONTH."),
            glue::glue("INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ÍCARO BERNARDES (@IcaroBSC)")),
)

## Defines coordinates for the highlights
highlights <- tibble(
  x = c(2010.9,2011.9,2013.7,2014.7,2016.5,2017.5),
  y = bottom_grid-27,
  size = 15,
  label = c(glue::glue("WHITE PEOPLE<br>IN INFORMAL<br>JOBS EARNED"),
            glue::glue("<span style='font-size:110px;'>{pct_wht_inf}%</span><br><span style='font-size:90px;'>MORE</span>"),
            glue::glue("BLACK PEOPLE<br>IN FORMAL<br>JOBS EARNED"),
            glue::glue("<span style='font-size:110px;'>{pct_blk_frm}%</span><br><span style='font-size:90px;'>MORE</span>"),
            glue::glue("WHITE PEOPLE<br>IN FORMAL<br>JOBS EARNED"),
            glue::glue("<span style='font-size:110px;'>{pct_wht_frm}%</span><br><span style='font-size:90px;'>MORE</span>"))
)

## Defines coordinates for the insights on the timeline
insights <- tibble(
  x = c(2012.4,2014.3,2016.7),
  y = c(1055,1095,1057),
  angle = c(90,0,0),
  hjust = c(0,0,0.5),
  label = c(glue::glue("YEARS OF CONTINUOUS<br><span syle='margin-left:5px;'>DEVELOPMENT OF BRAZIL.</span>"),
            glue::glue("COMMODITIES PRICES DROP."),
            glue::glue("ECONOMIC INCENTIVES FOR CONSUME<br>AND RETURN OF FOREIGN INVESTORS."))
)

## Creates the main plot
p <- df %>% 
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
                        label.colour = NA, size = 9, vjust = 1,
                        family = sans, lineheight = lnhgt,
                        data = legendTITLE) +
  ggtext::geom_richtext(aes(x = x, y = y, label = label), fill = bgcolor,
                        label.colour = NA, size = 14, family = sans,data = legendNUM) +
  ggtext::geom_richtext(aes(x = x, y = y, label = label), fill = bgcolor,
                        label.colour = NA, angle = -180, size = 11,
                        family = "fontawesome-webfont", data = legendICON) +
  
  ### Places the horizontal axis numbers
  ggtext::geom_richtext(aes(x = x, y = y, label = label), fill = NA,
                        label.colour = NA, size = 12, vjust = 1,
                        family = sans, data = axisX) +
  
  ### Places the plot titles
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        fill = NA, label.colour = NA, vjust = 1,
                        family = sans, lineheight = lnhgt, data = titles) +
  
  ### Places a rectangle behind the highlights
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = bottom_grid-35, ymax = bottom_grid-19, fill = "#654321") +
  
  ### Places the highlights
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        fill = NA, label.colour = NA, family = sans, color = bgcolor,
                        lineheight = lnhgt, data = highlights) +
  
  ### Places the timeline insights
  ggtext::geom_richtext(aes(x = x, y = y, label = label, angle = angle, hjust = hjust),
                        fill = bgcolor, label.colour = NA, size = 14,
                        lineheight = lnhgt, family = sans, data = insights) +
  
  ### Places the lines
  geom_line(aes(x = year, y = informal), color = "black", size = 7) +
  geom_line(aes(x = year, y = informal, color = I(era)), size = 5) +
  
  ### Places the joints between the geom_line
  geom_point(aes(x = year, y = informal), size = 4,
             color = "black", data = joints) +
  
  ### Places rectangles to hide the excess of the lines
  geom_tile(aes(x = year, y = informal), fill = bgcolor, color = NA,
            width = 0.193, height = 15, data = ends) +
  
  ### Adds back the leftmost grid limit
  annotate("linerange", ymin = gridY$ymin[1], ymax = gridY$ymax[1],
           x = gridY$x[1], color = "black") +
  
  ### Eliminates unnecessary elements and customizes the plot
  theme_void() +
  theme(
    plot.background = element_rect(fill = bgcolor, color = NA),
    legend.position = "none"
  )

## Saves the plot
ggsave("2022/week08/incomes.png", plot = p, dpi = "retina",
       width = 22, height = 28)

############################### TidyTuesday ####################################
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

