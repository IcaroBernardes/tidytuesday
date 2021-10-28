# 0. Library management
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(showtext)
library(readr)
library(patchwork)
library(biscale)
library(glue)
# library(hrbrthemes)

## Adding Google Fonts
font_add_google(name = "Josefin Sans", family = "josefin") ### Sans Serif
sans <- "josefin"
font_add_google(name = "Antic Slab", family = "antic") ### Serif
serif <- "antic"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_auto()

# 1. Data download, load and handling
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

# 2. Comparing estimated and actual time of the group of runners
pace <- race %>% 
  dplyr::mutate(htime = distance/4,
                vtime = (elevation_gain/300)+(-elevation_loss/500)) %>%
  dplyr::rowwise() %>% 
  dplyr::mutate(DINtime = max(htime,vtime) + (min(htime,vtime))/2) %>% 
  dplyr::filter(DINtime != 0 & !is.na(DINtime)) %>% 
  full_join(ultra_rankings) %>% 
  dplyr::filter(!is.na(time_in_seconds)) %>% 
  dplyr::filter(runner != "NO Participants") %>% 
  dplyr::mutate(time = time_in_seconds/3600) %>% 
  dplyr::group_by(race_year_id) %>% 
  dplyr::summarise(time = median(time),
                   DINtime = unique(DINtime),
                   htime = unique(htime),
                   vtime = unique(vtime)) %>% 
  na.exclude()

## Creates a variable dividing the points in categories
## according to two variables quantiles (vtime and htime)
pace <- biscale::bi_class(pace, x = htime, y = vtime, style = "quantile", dim = 3)

## Creates data for the bicolor palette
colors <- biscale::bi_pal("DkBlue", 3, FALSE)
bicolor <- tibble(
  x = c(rep(140,3),rep(130,3),rep(120,3)),
  y = rep(c(90,80,70),3),
  c = colors,
  label = paste0(c(rep(75,3),rep(50,3),rep(25,3)),"% (H)\n",
                 rep(c(75,50,25),3),"% (V)")
)

## Makes a linear model with DIN time as predictor and time as predicted
model <-  lm(time ~ DINtime, data = pace)

## Gets the model coefficients
intercept <- coefficients(model)[1]
slope <- coefficients(model)[2]

## Plots histogram for the horizontal and vertical times of each race
bins <- ceiling(1 + log2(dim(pace)[1]))

hist1 <- pace %>% 
  ggplot(aes(x = htime)) +
  geom_histogram(aes(fill = ..x..), bins = bins) +
  scale_fill_gradient(low = colors[9], high = colors[3]) +
  labs(x = "Horizontal time (h)", y = "Count") +
  theme_ipsum() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 50, family = sans),
        axis.title.y = element_text(size = 50, family = sans),
        axis.text.x = element_text(size = 30, family = sans),
        axis.text.y = element_text(size = 30, family = sans),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

hist2 <- pace %>% 
  ggplot(aes(x = vtime)) +
  geom_histogram(aes(fill = ..x..), bins = bins) +
  scale_fill_gradient(low = colors[9], high = colors[7]) +
  labs(x = "Vertical time (h)", y = "Count") +
  theme_ipsum() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 50, family = sans),
        axis.title.y = element_text(size = 50, family = sans),
        axis.text.x = element_text(size = 30, family = sans),
        axis.text.y = element_text(size = 30, family = sans),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Creates data for the axis
{
  xinf <- -10
  xsup <- 340
  yinf <- -10
  ysup <- 140
  xtit <- "Estimated time of the race (h)"
  ytit <- "Median time of the race (h)"
  xbrk <- seq(10, 100, by = 10)
  ybrk <- seq(10, 100, by = 10)
  }

## Defines some layout constants
{
  lnhgt <- 0.29 ### Height of lines of text
  insfnt <- 12 ### Font size of insights
  nudgey <- 0.25 ### Nudge of text in y-axis to emulate line break
  arhead <- 0.01 ### Arrow head length
  a1 <- (1-0)/(xsup-xinf)
  b1 <- 1-a1*xsup
  a2 <- (1-0)/(ysup-yinf)
  b2 <- 1-a2*ysup
  clpnt <- "#FFBF19" ### Color of the highlight background
  cltxt <- "#0140FF" ### Color of the highlight background
}

## Creates data for titles
titles <- c(
  "Through the rough patch",
  "Can we predict race time through distance and elevation?",
  paste(
    "Race and racers data: Benjamin Nowak by way of International Trail Running Association (ITRA)",
    "Graphic: Ícaro Bernardes (https://github.com/IcaroBernardes/tidytuesday/tree/main/week44)",
    sep = "\n"
  )
)

## Creates data for the instructions
instruc <- c(
  paste("How to estimate hiking time",
        "using the DIN standard",
        sep = "\n"),
  
  "About: https://www.wanderndeluxe.de/en/calculate-hiking-time-distance-altitude/",
  
  "300m",
  paste("takes",
        "in ascent",
        sep = "\n"),
  "1h",
  
  "500m",
  paste("in descent",
        "takes",
        sep = "\n"),
  "1h",
  
  "4km",
  paste("horizontally",
        "translates to",
        sep = "\n"),
  "1h",
  
  "summed make the vertical time (V)",
  "is the horizontal time (H)",
  
  "DIN[time] == MAX(V,H) + frac(MIN(V,H),2)"
  
)

## Creates data for insights
inst <- c(
  
  paste("Each point represents a race for which",
        "a median time of the runners is calculated",
        "and a typical run time is estimated from",
        "horizontal and vertical distances.",
        sep = "\n"),
  
  paste("For each race the vertical and horizontal times",
        "are estimated and divided into three quantiles.",
        "The points are colored accordingly to which",
        "pair of groups they belong.",
        sep = "\n"),
  
  paste(glue::glue("The linear model fitted to the data has a slope of {round(slope, 2)} and intercept of {round(intercept, 2)}."),
        "That means that the 50% slower runners at each race walk at a rhythm",
        "approximately two times faster than the pace assumed by the DIN standard",
        sep = "\n"),
  
  paste("Some race times diverge",
        "a lot from the trend",
        "'La Mission' race in Argentina is",
        "the most emblematic case.",
        "These events are more akin to",
        "a survival test than a race.",
        sep = "\n")
  
  
)
quiver <- tibble(
  x = c(102,152,45,50),
  y = c(38,92,8,75),
  xend = c(97,147,34,57),
  yend = c(45,90,18,66),
  curv = c(-0.3,0.1,-0.3,-0.1),
  hjust = c(0,0,0,1),
  label = inst
)
quiver <- quiver %>% 
  dplyr::mutate(nudgex = ifelse(hjust == 0, 2, -2),
                .after = hjust)

p <- pace %>% 
  ggplot(aes(x = DINtime, y = time)) +
  
  ### Defines the limits of the plot
  coord_cartesian(xlim = c(xinf,xsup), ylim = c(yinf,ysup), expand = FALSE) +
  
  ### Inserts points colored by vertical and horizontal time
  geom_point(aes(color = bi_class)) +
  biscale::bi_scale_color(pal = "DkBlue", dim = 3) +
  
  ### Creates the color legend
  geom_tile(aes(x = x, y = y), fill = bicolor$c, width = 10, height = 10, data = bicolor) +
  geom_text(aes(x = 115, y = 63, label = "Horizontal time (H) -->"), hjust = 0, vjust = 1,
            family = sans, size = 15) +
  geom_text(aes(x = 110, y = 66, label = "Vertical time (V) -->"), hjust = 0, vjust = 1,
            family = sans, size = 15, angle = 90) +
  geom_text(aes(x = x, y = y, label = label), family = sans, size = 10,
            color = clpnt, lineheight = lnhgt, data = bicolor) +
  
  ### Inserts the linear model line
  annotate("segment", x = 0, xend = 100, y = slope*0+intercept, yend = slope*100+intercept) +
  
  ### Defines axes
  annotate("text", x = xbrk, y = 0, label = xbrk, family = sans, size = 12, vjust = 1) +
  annotate("text", x = 0, y = ybrk, label = ybrk, family = sans, size = 12, hjust = 1) +
  annotate("text", x = 100, y = yinf+1, label = xtit, family = sans, size = 22, vjust = 0, hjust = 1) +
  annotate("text", x = xinf, y = 100, label = ytit, family = sans, size = 22, vjust = 1, hjust = 1, angle = 90) +
  annotate("text", x = 0, y = 0, label = 0, family = sans, size = 12, hjust = 1, vjust = 1) +
  
  ### Defines "clouds"
  annotate("point", color = "#f2f2f2",
           x = c(0,16,18,50,70,90,110,130,150,170),
           y = c(120,115,130,120,115,122,118,115,120,125),
           size = c(60,85,80,110,80,85,60,70,100,70)) +
  annotate("rect", xmin = xinf, xmax = 170, ymin = 110, ymax = ysup, fill = "#f2f2f2") +
  
  ### Defines titles
  annotate("text", x = 0, y = c(ysup-3,ysup-19,ysup-27), label = titles,
           family = sans, size = c(80,30,20), hjust = 0, vjust = 1, lineheight = lnhgt) +
  
  ### Defines instructions and "clouds"
  annotate("point", x = xsup-48, y = ysup, color = clpnt, size = 330) +
  annotate("text", x = xsup-3, y = ysup-5, label = instruc[1], hjust = 1, vjust = 1,
           family = serif, size = 30, lineheight = lnhgt, color = cltxt) + 
  annotate("text", x = xsup-3, y = ysup-20, label = instruc[2], hjust = 1, vjust = 1,
           family = serif, size = 10, lineheight = lnhgt, color = cltxt) + 
  
  annotate("point", color = "#f7f7f7",
           x = c(230,238,240,245,247,253),
           y = c(107,109,105,110,106,107),
           size = c(30,35,25,20,30,27)) +
  annotate("point", color = "#f7f7f7",
           x = c(268,273,275,280,290,295),
           y = c(107,102,109,106,107,107),
           size = c(29,10,25,30,25,15)) +
  annotate("point", color = "#f7f7f7",
           x = c(307,312,315,319,325,335),
           y = c(107,105,110,109,107,108),
           size = c(28,22,20,27,30,28)) +
  
  annotate("text", x = 239, y = 107, label = instruc[3], hjust = 1,
           family = serif, size = 25, lineheight = lnhgt) + 
  annotate("text", x = 246, y = 107, label = instruc[4],
           family = serif, size = 12, lineheight = lnhgt) + 
  annotate("text", x = 253, y = 107, label = instruc[5], hjust = 0,
           family = serif, size = 25, lineheight = lnhgt) +
  
  annotate("text", x = 276, y = 107, label = instruc[6], hjust = 1,
           family = serif, size = 25, lineheight = lnhgt) + 
  annotate("text", x = 283, y = 107, label = instruc[7],
           family = serif, size = 12, lineheight = lnhgt) + 
  annotate("text", x = 290, y = 107, label = instruc[8], hjust = 0,
           family = serif, size = 25, lineheight = lnhgt) +
  
  annotate("text", x = 312, y = 107, label = instruc[9], hjust = 1,
           family = serif, size = 25, lineheight = lnhgt) + 
  annotate("text", x = 321, y = 107, label = instruc[10],
           family = serif, size = 12, lineheight = lnhgt) + 
  annotate("text", x = 330, y = 107, label = instruc[11], hjust = 0,
           family = serif, size = 25, lineheight = lnhgt) +
  
  annotate("text", x = 263, y = 96, label = instruc[12],
           family = serif, size = 15, lineheight = lnhgt, color = cltxt) +
  annotate("text", x = 319, y = 96, label = instruc[13],
           family = serif, size = 15, lineheight = lnhgt, color = cltxt) +
  
  annotate("text", x = 296, y = 80, label = instruc[14], color = cltxt,
           family = serif, size = 25, lineheight = lnhgt, parse = TRUE) +
  
  ## Defines insights
  geom_text(aes(x = x+nudgex, y = y, label = label), family = serif, size = insfnt,
            hjust = quiver$hjust, lineheight = lnhgt, data = quiver) +
  annotate("curve", x = quiver$x[1], xend = quiver$xend[1], y = quiver$y[1], yend = quiver$yend[1],
           curvature = quiver$curv[1], arrow = arrow(length = unit(arhead, "npc"))) +
  annotate("curve", x = quiver$x[2], xend = quiver$xend[2], y = quiver$y[2], yend = quiver$yend[2],
           curvature = quiver$curv[2], arrow = arrow(length = unit(arhead, "npc"))) +
  annotate("curve", x = quiver$x[3], xend = quiver$xend[3], y = quiver$y[3], yend = quiver$yend[3],
           curvature = quiver$curv[4], arrow = arrow(length = unit(arhead, "npc"))) +
  annotate("curve", x = quiver$x[4], xend = quiver$xend[4], y = quiver$y[4], yend = quiver$yend[4],
           curvature = quiver$curv[4], arrow = arrow(length = unit(arhead, "npc"))) +
  
  theme_void() +
  # theme_ipsum() +
  theme(legend.position = "none") +
  
  ### Inserts the mini-violin for illustration purposes
  patchwork::inset_element(hist1,
                           left = a1*260+b1, right = a1*340+b1,
                           bottom = a2*-10+b2, top = a2*60+b2) +
  patchwork::inset_element(hist2,
                           left = a1*170+b1, right = a1*250+b1,
                           bottom = a2*-10+b2, top = a2*60+b2)
## Saves the plot
ggsave("week44/pace.png", plot = p, width = 58, height = 25, units = "cm")

