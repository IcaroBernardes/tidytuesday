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
rawdata <- read.csv2("extras/dubois22/week04/data.csv") %>% 
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
