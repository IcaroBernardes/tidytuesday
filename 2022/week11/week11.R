# 0. Library and fonts management
library(tidyverse)
library(ggplot2)
library(showtext)
library(glue)
library(ggtext)
library(lubridate)
library(circlize)
library(ComplexHeatmap)
library(cols4all)
library(patchwork)
library(cowplot)
library(ggplotify)

## Adding Google Fonts
sysfonts::font_add_google(name = "League Spartan", family = "lgspartan") ### Serif
sans <- "lgspartan"

# 1. Data download, load and handling
bioc <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/bioc.csv')

## Simplifies dates by keeping only month and year info
df <- bioc %>% 
  dplyr::select(-rmd) %>% 
  dplyr::arrange(package, date) %>% 
  dplyr::mutate(date = stringr::str_sub(date, 1L, 7L),
                date = lubridate::ym(date))

## Gets only changes in value for each package
df <- df %>% 
  dplyr::group_by(package, rnw) %>% 
  dplyr::filter(row_number() == n()) %>% 
  dplyr::ungroup()

## Eliminates empty lines and those outside the time frame of interest
df <- df %>% 
  na.exclude() %>% 
  dplyr::filter(lubridate::year(date) >= 2001,
                lubridate::year(date) <= 2020)

## Converts each observation from being a count of the number of vignettes
## to the variation in the number of vignettes
df <- df %>% 
  dplyr::group_by(package) %>% 
  dplyr::mutate(rnw = rnw - lag(rnw, default = 0)) %>% 
  dplyr::ungroup()

## Sums the data by date
df <- df %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise(across(.cols = where(is.numeric), .fns = sum)) %>% 
  dplyr::ungroup()

## Gets periods limits
start_year <- df %>% 
  dplyr::slice(1L) %>% 
  dplyr::pull(date) %>% 
  lubridate::year()
end_year <- df %>% 
  dplyr::slice(n()) %>% 
  dplyr::pull(date) %>% 
  lubridate::year()
periods <- ((end_year-start_year+1)*12)-1

## Adds the missing dates on the data
dates <- tibble(
  date = lubridate::ymd(glue::glue("{start_year}-1-1")) + months(0:periods)
)
df <- df %>% 
  dplyr::full_join(dates) %>% 
  dplyr::mutate(across(.cols = where(is.numeric),
                       .fns = ~ifelse(is.na(.), 0, .)))

## Calculates cumulative sums
df <- df %>% 
  dplyr::arrange(date) %>% 
  dplyr::mutate(across(.cols = where(is.numeric),
                       .fns = cumsum,
                       .names = "{.col}_cumsum"))


## Defines coordinates for the years labels
years <- tibble(
  x = 13,
  y = seq(1, 12*(end_year-start_year)+1, by = 12)+100,
  label = start_year:end_year
)

## Creates the matrix for the rnw method
mat1 <- df %>% 
  dplyr::mutate(year = year(date),
                month = month(date)) %>% 
  dplyr::select(year, month, rnw) %>% 
  tidyr::pivot_wider(names_from = year,
                     values_from = rnw) %>% 
  dplyr::arrange(month) %>% 
  dplyr::select(-month)
mat1 <- as.matrix(mat1)
rownames(mat1) <- month.name

## Makes the color function to fill the rnw heatmap
lim1 <- abs(mat1) %>% max() %>% signif(digits = 1)
col_vec1 <- cols4all::c4a(palette = "brewer.prgn", n = 11, contrast = c(0.2,1))
col_fun1 <- circlize::colorRamp2(seq(-lim1, lim1, length.out = 11), col_vec1)

## Defines coordinates for the titles
titles <- tibble(
  x = 0.5,
  y = c(0.7,0.62,0.49,0.23),
  size = c(25,rep(7,3)),
  label = c(
    "Spooky Vignettes",
    
    "Bioconductor is an R project that provides tools for biological studies.<br>
    This circular heatmap shows the change in the number of vignettes.<br>
    These are tutorials made in Sweave (LaTeX + R) for the packages.<br>
    Darker colors mean more variation.",
    
    "Productivy is relatively low throught the year while in <span style='color:red;'>**October**</span> reaches its peak.",
    
    "Data from: Robert Flight (@rmflight)<br>
    Graphic by: Ícaro Bernardes (@IcaroBSC)")
)

# 2. Generates the plot
## Resets and sets parameters for the plot
circlize::circos.clear()
par(
  mar = rep(0,4), ### Margin around chart
  bg = NA ### Background color
) 
circlize::circos.par(
  start.degree = 86, ### Rotates the chart
  gap.after = 5, ### Distance after each sector
  circle.margin = 0.1 ### Margins around the circular chart
)

## Creates the circular heatmap
circlize::circos.heatmap(
  mat1,
  col = col_fun1,
  track.height = 0.2
)

## Places the years labels
circlize::circos.track(
  track.index = get.current.track.index(),
  bg.border = NA, 
  panel.fun = function(x, y) {
    if(CELL_META$sector.numeric.index == 1) { 
      cn = colnames(mat1)
      n = length(cn)
      circos.text(
        x = rep(CELL_META$cell.xlim[2], n) + convert_x(0.5, "mm"), 
        y = n:1 - 0.5,
        labels = cn, 
        cex = 0.75,
        family = sans,
        adj = c(0, 0.5),
        facing = "inside",
        niceFacing = TRUE
      )
    }
  }
)

## Places the month names labels
circlize::circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    ro = CELL_META$row_order
    rn = rownames(mat1)[ro]
    n = length(rn)
    circos.text(
      x = c(1:12) - 0.5, 
      y = rep(13.5, n),
      labels = rn, 
      cex = 2.2,
      family = sans,
      facing = "outside",
      niceFacing = TRUE,
      adj = c(0.5,1)
    )
  }
)

## Places the month numbers labels
circlize::circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {
    ro = CELL_META$row_order
    rn = glue::glue("{scales::ordinal(ro)} month")
    n = length(rn)
    circos.text(
      x = c(1:12) - 0.5, 
      y = rep(11.5, n),
      labels = rn,
      cex = 1.3,
      family = sans,
      facing = "outside",
      niceFacing = TRUE,
      adj = c(0.5,1)
    )
  }
)

## Gets the plot from the device and saves 
## it in an ggplot-friendly format
hmap1 <- recordPlot()
hmap1 <- ggplotify::as.ggplot(cowplot::ggdraw(hmap1))

## Makes a legend guide and converts it to a ggplot-friendly format
lgd <- ComplexHeatmap::Legend(title = "Change in the total number of vignettes",
                              title_gp = gpar(fontfamily = sans, fontsize = 16),
                              legend_gp = gpar(fontfamily = sans, fontsize = 12),
                              col_fun = col_fun1,
                              direction = "horizontal",
                              title_position = "topcenter",
                              legend_width = unit(70, "mm"))
lgd <- ggplotify::as.ggplot(cowplot::ggdraw(lgd@grob))

## Allows the use of the downloaded Google Font.
## Causes error if comes before the circular plot
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

## Makes the time series plot
timeseries <- df %>% 
  dplyr::mutate(color = ifelse(lubridate::month(date) == 9, "red", "black")) %>% 
  ggplot(aes(x = date, y = rnw_cumsum, color = I(color), group = 1)) +
  geom_line(size = 2, lineend = "round") +
  ggtext::geom_richtext(aes(x = lubridate::ymd("2011-03-01"),
                            y = max(df$rnw_cumsum),
                            label = "**CUMULATIVE COUNT<br>OF SWEAVE<br>VIGNETTES**"),
                        hjust = 0, vjust = 1, size = 8,
                        label.color = NA, fill = NA, family = sans) +
  coord_fixed(ratio = 2.1) +
  theme_void() +
  theme(
    axis.text = element_text(family = sans, size = 20)
  )

## Makes the main plot
p <- ggplot(NULL) + 
  
  annotate("point", x = 0.5, y = 0.5, size = 421,
           color = colorspace::lighten("#3D8EF5", 0.6)) +
  
  ### Places the titles
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        label.color = NA, fill = NA, family = sans, data = titles) +
  
  ### Defines unitary plots limits
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
  
  ### Eliminates and customizes elements on the plot
  theme_void() +
  theme(
    panel.background = element_rect(fill = colorspace::lighten("#3D8EF5", 0.75), color = NA)
  ) +
  
  ### Places the circular heatmap, time series and the legend
  patchwork::inset_element(hmap1, left = 0, right = 1,
                           bottom = 0, top = 1) +
  patchwork::inset_element(timeseries, left = 0.25, right = 0.75,
                           bottom = 0.28, top = 0.46) +
  patchwork::inset_element(lgd, left = 0.4, right = 0.6,
                           bottom = 0.49, top = 0.59)

## Saves the plot
ggsave("2022/week11/vignettes.png", plot = p, dpi = "retina",
       width = 15, height = 15)

