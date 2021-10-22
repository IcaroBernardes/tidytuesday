# 0. Library management
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(showtext)
library(readr)
library(rcompanion)
library(ggforce)
library(gginnards)
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
pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')
pumpkins <- pumpkins %>% 
  dplyr::filter(stringr::str_detect(place, "Entries", negate = TRUE)) %>% 
  tidyr::separate(id, into = c("year","type"), sep = "-")

# 2. Which pollination technique (sib, self or open) yields the best results?
## Converts weights to numerical, filters and handles the labels of relevant techniques 
pump <- pumpkins %>% 
  dplyr::select(type, weight_lbs, country, pollinator_father) %>% 
  dplyr::mutate(weight_lbs = str_remove_all(weight_lbs,",")) %>% 
  dplyr::mutate(weight_lbs = as.numeric(weight_lbs)) %>%
  dplyr::mutate(pollinator_father = tolower(pollinator_father)) %>% 
  dplyr::filter(stringr::str_detect(pollinator_father,"sib|self|open")) %>% 
  dplyr::mutate(pollinator_father =
                  case_when(stringr::str_detect(pollinator_father,"self") ~ "self",
                            stringr::str_detect(pollinator_father,"sib") ~ "sib",
                            stringr::str_detect(pollinator_father,"open") ~ "open",
                            TRUE ~ "")) %>% 
  dplyr::mutate(pollinator_father = factor(pollinator_father, levels = c("self","sib","open")))

## Estimates medians and their confidence intervals of each group
set.seed(989)
med <- groupwiseMedian(weight_lbs ~ pollinator_father + type, data = pump,
                       conf = 0.95, R = 3000, boot = TRUE, bca = TRUE, digits = 3)

## Defines some layout constants
{
  rng <- 0.8 ### Width of the violins
  lnhgt <- 0.29 ### Height of lines of text
  insfnt <- 12 ### Font size of insights
  intfnt <- 14 ### Font size of instructions
  nudgey <- 0.25 ### Nudge of text in y-axis to emulate line break
}

## Plots first part of the results
main <- pump %>% 
  ggplot() +
  geom_violin(aes(x = pollinator_father, y = weight_lbs, fill = type),
              scale = "width", width = rng) +
  ggforce::geom_sina(aes(x = pollinator_father, y = weight_lbs),
                     scale = "width", maxwidth = rng, size = 0.1, seed = 989) +
  facet_grid(type~., scales = "free") +
  scale_fill_manual(values = c("#855503","#8ae079","#de8410","#b0d1a1","#bf0e08","#04751c"),
                    breaks = c("F","L","P","S","T","W"))

## Gets data from the violin to make the "ribbons"
lvl1 <- tibble(type = factor(unique(med$type)))
lvl1 <- lvl1 %>% dplyr::mutate(PANEL = as.numeric(type))
lvl2 <- tibble(pollinator_father = factor(unique(med$pollinator_father)))
lvl2 <- lvl2 %>% dplyr::mutate(x = as.numeric(pollinator_father))

violins <- ggplot2::layer_data(main, i = 1L)
violins <- violins %>% 
  dplyr::mutate(x = as.numeric(x)) %>% 
  dplyr::mutate(PANEL = as.numeric(PANEL)) %>% 
  dplyr::left_join(lvl1) %>% 
  dplyr::left_join(lvl2)

## Creates "ribbon" data
ribbon <- med %>% 
  dplyr::select(-n) %>% 
  dplyr::mutate(type = factor(type)) %>% 
  dplyr::full_join(violins) %>% 
  dplyr::rowwise() %>% 
  dplyr::filter(between(y, Bca.lower, Bca.upper)) %>% 
  dplyr::mutate(xmin = x-(rng/2)*scaled,
                xmax = x+(rng/2)*scaled) %>% 
  dplyr::select(-x) %>% 
  tidyr::pivot_longer(cols = c(xmin,xmax), values_to = "x") %>% 
  dplyr::mutate(y = ifelse(name == "xmin", -y, y)) %>%
  dplyr::arrange(pollinator_father, type, name, y) %>% 
  dplyr::mutate(y = abs(y)) %>% 
  dplyr::select(pollinator_father, type, y, x)
extra <- med %>% 
  dplyr::mutate(xmin = 0, xmax = 4) %>% 
  tidyr::pivot_longer(cols = c(xmin,xmax), values_to = "x")

## Plots the rest of the results
main <- main +
  geom_polygon(aes(x = x, y = y, group = pollinator_father),
               fill = "#4617EB", alpha = 0.6, data = ribbon) +
  geom_ribbon(aes(x = x, ymin = Bca.lower, ymax = Bca.upper, group = pollinator_father),
              fill = "#4617EB", alpha = 0.2, data = extra) 

## Moves the "extra ribbon" to the back of all layers.
## Creates an "extra" violin (because of ggplot limitations)
main <- gginnards::move_layers(main, idx = 4L, position = "bottom")
main <- gginnards::append_layers(main, gginnards::extract_layers(main, "GeomViolin"), position = "bottom")
main <- main +
  theme_void() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.y = element_blank(),
        panel.spacing = unit(0, "lines"))

## Creates an "extra" version of the main plot losely based on one violin
mainextra <- list(
  ext1 = ggplot2::layer_data(main, 1L),
  ext4 = ggplot2::layer_data(main, 4L)
)

mainextra <- mainextra %>% 
  map(. %>% dplyr::mutate(x = as.numeric(x)) %>% 
        dplyr::mutate(PANEL = as.numeric(PANEL)) %>% 
        dplyr::left_join(lvl1) %>% 
        dplyr::left_join(lvl2) %>% 
        dplyr::filter(group != 2) %>% 
        dplyr::filter(type == "P") %>% 
        dplyr::filter(pollinator_father == "self" | is.na(pollinator_father)))

mainextra$ext4 <- dplyr::slice(mainextra$ext4, sample.int(dim(mainextra$ext4)[1],100))

mainextra <- ggplot() +
  geom_ribbon(aes(xmin = 1-scaled*width/2, xmax = 1+scaled*width/2, y = y),
              fill = mainextra$ext1$fill, data = mainextra$ext1) +
  annotate("ribbon", x = c(0,2), ymin = c(1300,1300), ymax = c(1500,1500),
           fill = "#4617EB", alpha = 0.2) +
  annotate("ribbon", x = c(0,2), ymin = c(1300,1300), ymax = c(1800,1800),
           fill = "#4617EB", alpha = 0.2) +
  annotate("ribbon", x = c(0,2), ymin = c(400,400), ymax = c(1100,1100),
           fill = "#4617EB", alpha = 0.2) +
  geom_ribbon(aes(xmin = 1-scaled*width/2, xmax = 1+scaled*width/2, y = y),
              fill = mainextra$ext1$fill, data = mainextra$ext1) +
  geom_point(aes(x = x, y = y), size = mainextra$ext4$size,
             data = mainextra$ext4) +
  annotate("ribbon", x = c(0.65,1.35), ymin = c(400,400), ymax = c(1100,1100),
           fill = "#4617EB", alpha = 0.6) +
  annotate("text", x = 0.6, y = c(2000,100), label = c(2000,100), size = 14, family = sans, hjust = 1) +
  annotate("point", x = 1, y = 1000, stroke = 15, size = 30, shape = 1, color = "white") +
  annotate("point", x = 1, y = 1000, stroke = 5, size = 30, shape = 1, color = "red") +
  coord_equal(xlim = c(-0.7,2.7), ylim = c(-700,2700), ratio = 1/1000) +
  theme_void()

## Creates data for titles
titles <- c(
  "Gorgeous Gourds",
  "How the pollen source affects the weight of gourds and tomatos?",
  paste(
    "Fruits data: Great Pumpkin Commonwealth's (BigPumpkins.com)",
    "Graphic: Ícaro Bernardes (https://github.com/IcaroBernardes/tidytuesday/tree/main/week43)",
    sep = "\n"
  )
)

## Creates data for the instructions
instruc <- c(
  paste("Pollen comes from","the same plant",sep = "\n"),
  paste("Pollen comes from","a sibling plant",sep = "\n"),
  paste("Pollen comes from","the ambient",sep = "\n")
)
instmain <- paste("The fruits are grouped by pollen source and their weights are used to generate a density plot (violins).",
                  "Inside the violins, there are jittery points giving an idea of sample size.",
                  "The confidence interval of the median of each group is plotted as a blue ribbon.",
                  "In the left, are presented the max and min values of the trimmed violins.",
                  sep = "\n")

## Creates data for the axis
{
  xinf <- 0
  xsup <- 7
  yinf <- 0
  ysup <- 10
  xtit <- "Pollen source"
  ytit <- "Weight of the fruit (lbs)"
  xbrk <- c("Self","Sibling","Open")
  ybrk <- c("Watermelon","Tomato","Squash","Pumpkins","Long Gourds","Field Pumpkins")
  a1 <- 0.13
  b1 <- 0.045
  a2 <- 0.111
  b2 <- -0.1005
}

## Defines y-axis limits texts
ylims <- violins %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(min = min(y), max = max(y)) %>% 
  dplyr::mutate(med = (min+max)/2, rng = max-min) %>% 
  tidyr::pivot_longer(cols = c(min,max), values_to = "lims", names_to = "val") %>%
  dplyr::mutate(pos = ifelse(val == "min", med-0.25*rng, med+0.25*rng)) %>% 
  dplyr::left_join(lvl1) %>% 
  dplyr::mutate(pos = ifelse(val == "min", PANEL-0.3, PANEL+0.3))

## Creates data for insights
inst <- c(
  
  "'Watermelons'*phantom(' are secretly gourds.')",
  "phantom('Watermelons')*' are secretly gourds.'",
  paste("'Data shown is bimodal in the more controlled genetic pool.",
        "Maybe there are two lineages in here?'",
        sep = "\n"),
  
  "'Tomatos'*phantom(' are guests in this community (they are berries).')",
  "phantom('Tomatos')*' are guests in this community (they are berries).'",
  paste("'Different from the gourds, data suggests that open pollination",
        "yields slightly more weight than other techniques.'",
        sep = "\n"),
  
  "phantom('The word for ')*'Squash'*phantom(' means a green thing eaten raw.')",
  "'The word for '*phantom('Squash')*' means a green thing eaten raw.'",
  paste("'There are green and yellow variants of the fruit.",
        "Although rarer, sibling-pollinated fruits seem to fare better.'",
        sep = "\n"),
  
  "phantom('Halloween stars, ')*'Pumpkins'*phantom(' also name the GPC community.')",
  "'Halloween stars, '*phantom('Pumpkins')*' also name the GPC community.'",
  paste("'Alongside squashes, they are the heaviest produce in this set.",
        "Sibling-pollinated fruits are aparently heavier.'",
        sep = "\n"),
  
  "'Long Gourds'*phantom(' come in many shapes and have many utilities.')",
  "phantom('Long Gourds')*' come in many shapes and have many utilities.'",
  paste("'Their weight distribution has a short tail",
        "in the higher end compared to other gourds.'",
        sep = "\n"),
  
  "'Field Pumpkins'*phantom(' have a genetical heritage to protect.')",
  "phantom('Field Pumpkins')*' have a genetical heritage to protect.'",
  paste("'GPC alerts to a risk to the lineages under the distribution of",
        "open-pollinated seeds, however many fruits come from them.'",
        sep = "\n")
  
)
quiver <- tibble(
  x = rep(5.1,18),
  y = c(1.15,1.15,1.15-nudgey,
        2.15,2.15,2.15-nudgey,
        3.15,3.15,3.15-nudgey,
        4.15,4.15,4.15-nudgey,
        5.15,5.15,5.15-nudgey,
        6.15,6.15,6.15-nudgey),
  color = c("#04751c",rep("black",2),
            "#bf0e08",rep("black",2),
            "#b0d1a1",rep("black",2),
            "#de8410",rep("black",2),
            "#8ae079",rep("black",2),
            "#855503",rep("black",2)),
  inst = inst
)

## Creates a new plot for all text and decorations around the main plot
p <- ggplot() +
  coord_cartesian(xlim = c(xinf,xsup), ylim = c(yinf,ysup)) +
  
  ### Defines titles
  annotate("text", x = 0, y = c(ysup,ysup-0.75,ysup-1.1), label = titles,
           family = sans, size = c(80,35,22), hjust = 0, vjust = 1, lineheight = lnhgt) +
  
  ## Defines instructions
  annotate("text", x = 2:4, y = 6.9, label = instruc, family = sans,
           size = 16, vjust = 0, hjust = 0.5, lineheight = lnhgt) +
  annotate("text", x = 0.7, y = 7.85, label = instmain, family = sans,
           size = 20, hjust = 0, vjust = 0.5, lineheight = lnhgt) +
  geom_text(aes(x = x, y = y, label = inst), color = quiver$color, family = serif, size = insfnt,
            parse = TRUE, hjust = 0, vjust = 1, lineheight = lnhgt, data = quiver) +
  annotate("segment", x = xinf, xend = xsup, y = 8.35, yend = 8.35, lineend = "round", size = 0.9) +
  annotate("segment", x = xinf, xend = xsup, y = 7.35, yend = 7.35, lineend = "round", size = 0.9) +
  
  
  ### Defines axes
  annotate("text", x = 2:4, y = 0.3, label = xbrk, family = sans, size = 12, vjust = 1) +
  annotate("text", x = 2:4, y = 6.7, label = xbrk, family = sans, size = 12, vjust = 1) +
  annotate("text", x = 0.9, y = 1:6, label = ybrk, family = sans, size = 12, hjust = 1) +
  annotate("text", x = 5, y = 0, label = xtit, family = sans, size = 22, vjust = 1, hjust = 1) +
  annotate("text", x = 0.1, y = 6.5, label = ytit, family = sans, size = 22, vjust = 1, hjust = 1, angle = 90) +
  geom_text(aes(x = 1.4, y = pos, label = lims), hjust = 1, family = sans, size = 12,
            data = ylims) +
  
  theme_void() +
  
  ### Inserts the mini-violin for illustration purposes
  patchwork::inset_element(mainextra,
                           left = a1*-0.05+b1, right = a1*0.65+b1,
                           bottom = a2*7.4+b2, top = a2*8.1+b2) +
  
  ### Inserts the main plot
  patchwork::inset_element(main,
                           left = 0.175, right = 0.695,
                           bottom = 0.091, top = 0.636)

## Saves the plot
ggsave("week43/pollinator.png", plot = p, width = 40, height = 40, units = "cm")

