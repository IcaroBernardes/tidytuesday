# 0. Library management
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(showtext)
library(readxl)
library(readr)
library(urbnmapr)
library(patchwork)

## Add Google Font
font_add_google(name = "Ubuntu", family = "ubuntu")
font_add_google(name = "Noto Serif", family = "noto")

# 1. Data download, load and handling
tuesdata <- tidytuesdayR::tt_load('2021-10-05')
nurses <- tuesdata$nurses

## US Census population estimation by state (from 2010 to 2019)
## Guam and the Virgin Islands are missing in the data
POP <- read_excel("week41/pop.xlsx")
POP <- POP %>% 
  pivot_longer(-State, names_to = "Year", values_to = "pop") %>% 
  dplyr::mutate(Year = as.numeric(Year))

## US Regional Price Parities by state (from 2008 to 2019)
## Guam, Puerto Rico and the Virgin Islands are missing in the data
RPP <- read_csv("week41/RPP.csv", skip = 4)
RPP <- RPP %>% 
  dplyr::select(-GeoFips) %>% 
  dplyr::slice(-c(1,53:54)) %>% 
  dplyr::rename(State = "GeoName") %>% 
  pivot_longer(-State, names_to = "Year", values_to = "rpp") %>% 
  dplyr::mutate(Year = as.numeric(Year))

## US National mean annual wage for Registered Nurses (29-1141)
natwage <- tibble(
  Year = 2010:2019,
  natwage = c(67720,69110,67930,68910,
              69790,71000,72180,73550,
              75510,77460)
)

## United States Census Bureau statistical regions
region <- tibble(
  region = c(rep("Northeast",9),rep("Midwest",12),rep("South",17),rep("West",13)),
  State = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont",
            "New Jersey", "New York", "Pennsylvania",
            "Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin",
            "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota",
            "Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "District of Columbia", "West Virginia",
            "Alabama", "Kentucky", "Mississippi", "Tennessee",
            "Arkansas", "Louisiana", "Oklahoma", "Texas",
            "Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming",
            "Alaska", "California", "Hawaii", "Oregon", "Washington")
)

## Joining all data
df <- nurses %>% 
  full_join(POP) %>% 
  full_join(RPP) %>% 
  full_join(natwage) %>% 
  full_join(region) %>% 
  filter(between(Year, 2010, 2019))

## Gets US states coordinates
states_map <- urbnmapr::states %>% 
  dplyr::rename(State = state_name)

# 2. Is a higher salary enough to solve the nurses shortage?
## Data handling
datum <- df %>% 
  dplyr::mutate(nurse = round(`Total Employed RN`/(pop/1000),2),
                salary = round(100*`Annual Salary Avg`/natwage,1)) %>% 
  dplyr::select(region, State, Year, nurse, salary, rpp) %>% 
  dplyr::mutate(ratio = salary/rpp) %>% 
  na.exclude()

## Most frequent states in the Top 3 of RN per 100 hab.
datum %>% 
  group_by(Year) %>% 
  mutate(rank = min_rank(desc(nurse))) %>% 
  filter(rank <= 3) %>% 
  ungroup() %>% 
  count(State) %>% 
  arrange(desc(n))

## Most frequent states in the Last 3 of RN per 100 hab.
datum %>% 
  group_by(Year) %>% 
  mutate(rank = min_rank(nurse)) %>% 
  filter(rank <= 3) %>% 
  ungroup() %>% 
  count(State) %>% 
  arrange(desc(n))

## Texts for really big subtitles
subtitle1 <- 
  paste(
    "Data from 2010 to 2019 suggests that higher salaries won't solve the US shortage of nurses.",
    "Supply of new professionals is not keeping up with the demand for health services.",
    sep = "\n"
  )

subtitle2 <- 
  paste("Nurses salaries: US Bureau of Labor Statistics through Data.World",
        "Population estimates: United States Census Bureau (https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html)",
        "Regional Price Parities: US Bureau of Economic Analysis (https://www.bea.gov/data/prices-inflation/regional-price-parities-state-and-metro-area)",
        "Insights on nursing shortage: Rebecca Grant - The Atlantic (https://www.theatlantic.com/health/archive/2016/02/nursing-shortage/459741/),",
        "Insights on nursing shortage: American Association of Colleges of Nursing (https://www.aacnnursing.org/News-Information/Fact-Sheets/Nursing-Shortage)",
        "Insights on nursing shortage: Ana Ibarra - KHN (https://khn.org/news/californias-rn-wages-now-highest-in-the-nation-federal-data-show/)",
        "Graphic: Ícaro Bernardes (https://github.com/IcaroBernardes/tidytuesday/tree/main/week41)",
        sep = "\n")

## Insights and instructions
msg <- 
  c(
    ### Instruction 1
    paste("This ratio is comprised of two relative states metrics against the national average:",
          "on top is the relative average salary of nurses and",
          "at the bottom is the relative living cost (price levels).",
          sep = "\n"),
    ### Insight 1.1
    "'Most western states (like '*phantom('California')*' and '*phantom('Nevada')*') have'",
    "phantom('Most western states (like ')*'California'*phantom(' and ')*phantom('Nevada')*phantom(') have')",
    "phantom('Most western states (like ')*phantom('California')*phantom(' and ')*'Nevada'*phantom(') have')",
    ### Insight 1.2
    paste("an advantageous salary/cost of living ratio however",
          "they are in the most critical shortage situation.",
          sep = "\n"),
    ### Insight 1.3
    "'Despite the '*phantom('Californian')*' union fight for better salaries and work conditions,'",
    "phantom('Despite the ')*'Californian'*phantom(' union fight for better salaries and work conditions,')",
    ### Insight 1.4
    paste("the supply of nursing services is surpassed by",
          "the demands of a growing and aging population.",
          sep = "\n"),
    ### Insight 2.1
    "phantom('DC')*' and '*phantom('South Dakota')*' have the'",
    "'DC'*phantom(' and ')*phantom('South Dakota')*phantom(' have the')",
    "phantom('DC')*phantom(' and ')*'South Dakota'*phantom(' have the')",
    ### Insight 2.2
    paste(
      "best coverage of nurses per habitant.",
      "Curiously both regions have",
      "small populations (under 900K)",
      sep ="\n"),
    ### Insight 3.1
    "While an advantageous salary may explain the",
    ### Insight 3.2
    "'differences between '*phantom('Utah')*' and '*phantom('Massachusetts')",
    "phantom('differences between ')*'Utah'*phantom(' and ')*phantom('Massachusetts')",
    "phantom('differences between ')*phantom('Utah')*phantom(' and ')*'Massachusetts'",
    ### Insight 3.3
    paste(
      "the biggest factors that contribute to the nursing shortage are:",
      "lack of faculty and resources in nursing schools,",
      "aging and growing population,",
      "workforce is retiring or abandoning the profession",
      sep ="\n"),
    ### Instruction 2
    paste(
      "Each point represents the statistics of a state in a given year.",
      "The symbols are associated with the statistical regions defined by the US Census:",
      "Northeast (\U25B2), South (\U25BC), West (\U25A0) and Midwest (\U25CF)",
      sep ="\n")
  )

## Defines some layout constants
arhead <- 0.01 ### Arrow head length
lnhgt <- 0.28 ### Height of lines of text
typogr <- 9 ### Font size of insights and instructions
fam <- "noto" ### Font for insights and instructions
nudgey <- 0.022 ### Nudge of text in y-axis to emulate line break

## Stores data of arrows for insights and instructions
quiver <- tibble(
  hjust = c(0,0,0,0,0.5,0.5,0),
  x = c(4.75,7.9,15.5,15.5,9.3,9.3,14.9),
  xend = c(4.33,7.19,15.05,14.8,6.25,12.5,13.4),
  y = c(1.35,1.30,0.88,0.88,0.82,0.82,1.23),
  yend = c(1.31,1.22,0.95,0.87,0.89,1.12,1.17),
  curvature = c(0.5,0.4,-0.3,0.1,-0.3,0.3,0.2)
)
quiver <- quiver %>% 
  dplyr::mutate(nudgex = ifelse(hjust == 0, 0.05, -0.05),
                .after = hjust)

## Creates the US maps
maps <- df %>%
  dplyr::filter(Year == 2019) %>% 
  na.exclude() %>% 
  dplyr::mutate(rn = round(100*`Total Employed RN`/mean(`Total Employed RN`),1),
                pop = round(100*pop/mean(pop),1),
                salary = round(100*`Annual Salary Avg`/natwage,1)) %>% 
  dplyr::select(State, rn, pop, salary, rpp) %>% 
  full_join(states_map)

p1 <- maps %>% 
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = rn), color = "black", size = 0.01) +
  scale_fill_gradientn(colors = c('#f7fbff','#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#08519c','#08306b')) +
  theme_void() +
  theme(legend.position = "none")

p2 <- maps %>% 
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = pop), color = "black", size = 0.01) +
  scale_fill_gradientn(colors = c('#fcfbfd','#efedf5','#dadaeb','#bcbddc','#9e9ac8','#807dba','#6a51a3','#54278f','#3f007d')) +
  theme_void() +
  theme(legend.position = "none")

p3 <- maps %>% 
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = salary), color = "black", size = 0.01) +
  scale_fill_gradientn(colors = c('#f7fcf5','#e5f5e0','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#006d2c','#00441b')) +
  theme_void() +
  theme(legend.position = "none")

p4 <- maps %>% 
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = rpp), color = "black", size = 0.01) +
  scale_fill_gradientn(colors = c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')) +
  theme_void() +
  theme(legend.position = "none")

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext_auto()

p <- datum %>%
  ggplot(aes(x = nurse, y = ratio)) +
  geom_point(aes(shape = region), size = 2, alpha = 0.3, color = "black", fill = "gray") +
  
  ## Most frequent states in the Top 3 of RN per 100 hab.
  geom_point(aes(shape = region), fill = "#e41a1c", size = 2, alpha = 0.7,
             data = datum %>% filter(State == "District of Columbia")) +
  geom_point(aes(shape = region), fill = "#ff7f00", size = 2, alpha = 0.7,
             data = datum %>% filter(State == "South Dakota")) +
  geom_point(aes(shape = region), fill = "#f781bf", size = 2, alpha = 0.7,
             data = datum %>% filter(State == "Massachusetts")) +
  ## Most frequent states in the Last 3 of RN per 100 hab.
  geom_point(aes(shape = region), fill = "#377eb8", size = 2, alpha = 0.7,
             data = datum %>% filter(State == "Nevada")) +
  geom_point(aes(shape = region), fill = "#984ea3", size = 2, alpha = 0.7,
             data = datum %>% filter(State == "Utah")) +
  geom_point(aes(shape = region), fill = "#4daf4a", size = 2, alpha = 0.7,
             data = datum %>% filter(State == "California")) +
  
  ## X-axis labels and title
  annotate("text", x = seq(6,18, by = 3), y = 0.7, family = "ubuntu",
           label = seq(6,18, by = 3), size = 15) +
  annotate("text", x = 18, y = 0.65, family = "ubuntu", hjust = 1, vjust = 1,
           label = "Nº of registered nurses per 1.000 of habitants", size = 18) +
  
  ## Y-axis labels and title
  annotate("text", y = seq(0.8,1.3, by = 0.1), x = 5, family = "ubuntu",
           label = seq(0.8,1.3, by = 0.1), size = 15) +
  annotate("text", x = 4, y = 1.3, family = "ubuntu", hjust = 1, vjust = 1, angle = 90,
           label = "Relative salary vs\nrelative cost of living Ratio", size = 18, lineheight = lnhgt) +
  
  ## Title and subtitles
  annotate("text", x = 4, y = 1.7, family = "ubuntu", hjust = 0, vjust = 1, size = 50,
           label = "Nursing the US back to health") +
  annotate("text", x = 4, y = 1.61, family = "ubuntu", hjust = 0, vjust = 1, lineheight = lnhgt, size = 23,
           label = subtitle1) +
  annotate("text", x = 4, y = 1.525, family = "ubuntu", hjust = 0, vjust = 1, lineheight = lnhgt, size = 9,
           label = subtitle2) +
  
  ## Insights and instructions
  ### Instruction 1
  annotate("text", x = quiver$x[1]+quiver$nudgex[1], y = quiver$y[1], hjust = quiver$hjust[1], label = msg[1],
           size = typogr, family = fam, lineheight = lnhgt) +
  annotate("curve", x = quiver$x[1], xend = quiver$xend[1], y = quiver$y[1], yend = quiver$yend[1],
           curvature = quiver$curvature[1], arrow = arrow(length = unit(arhead, "npc"))) +
  ### Insight 1
  annotate("text", x = quiver$x[2]+quiver$nudgex[2], y = quiver$y[2], hjust = quiver$hjust[2], label = msg[2],
           size = typogr, family = fam, lineheight = lnhgt, parse = TRUE) +
  annotate("text", x = quiver$x[2]+quiver$nudgex[2], y = quiver$y[2], hjust = quiver$hjust[2], label = msg[3],
           size = typogr, family = fam, lineheight = lnhgt, parse = TRUE, color = "#4daf4a") +
  annotate("text", x = quiver$x[2]+quiver$nudgex[2], y = quiver$y[2], hjust = quiver$hjust[2], label = msg[4],
           size = typogr, family = fam, lineheight = lnhgt, parse = TRUE, color = "#377eb8") +
  annotate("text", x = quiver$x[2]+quiver$nudgex[2], y = quiver$y[2]-nudgey, hjust = quiver$hjust[2], label = msg[5],
           size = typogr, family = fam, lineheight = lnhgt) +
  annotate("text", x = quiver$x[2]+quiver$nudgex[2], y = quiver$y[2]-2.1*nudgey, hjust = quiver$hjust[2], label = msg[6],
           size = typogr, family = fam, lineheight = lnhgt, parse = TRUE) +
  annotate("text", x = quiver$x[2]+quiver$nudgex[2], y = quiver$y[2]-2.1*nudgey, hjust = quiver$hjust[2], label = msg[7],
           size = typogr, family = fam, lineheight = lnhgt, parse = TRUE, color = "#4daf4a") +
  annotate("text", x = quiver$x[2]+quiver$nudgex[2], y = quiver$y[2]-3.1*nudgey, hjust = quiver$hjust[2], label = msg[8],
           size = typogr, family = fam, lineheight = lnhgt) +
  annotate("curve", x = quiver$x[2], xend = quiver$xend[2], y = quiver$y[2]-1.4*nudgey, yend = quiver$yend[2],
           curvature = quiver$curvature[2], arrow = arrow(length = unit(arhead, "npc"))) +
  ### Insight 2
  annotate("text", x = quiver$x[3]+quiver$nudgex[3], y = quiver$y[3], hjust = quiver$hjust[3], label = msg[9],
           size = typogr, family = fam, lineheight = lnhgt, parse = TRUE) +
  annotate("text", x = quiver$x[3]+quiver$nudgex[3], y = quiver$y[3], hjust = quiver$hjust[3], label = msg[10],
           size = typogr, family = fam, lineheight = lnhgt, parse = TRUE, color = "#e41a1c") +
  annotate("text", x = quiver$x[3]+quiver$nudgex[3], y = quiver$y[3], hjust = quiver$hjust[3], label = msg[11],
           size = typogr, family = fam, lineheight = lnhgt, parse = TRUE, color = "#ff7f00") +
  annotate("text", x = quiver$x[3]+quiver$nudgex[3], y = quiver$y[3]-1.5*nudgey, hjust = quiver$hjust[3], label = msg[12],
           size = typogr, family = fam, lineheight = lnhgt) +
  annotate("curve", x = quiver$x[3], xend = quiver$xend[3], y = quiver$y[3]-1.4*nudgey, yend = quiver$yend[3],
           curvature = quiver$curvature[3], arrow = arrow(length = unit(arhead, "npc"))) +
  annotate("curve", x = quiver$x[4], xend = quiver$xend[4], y = quiver$y[4]-1.4*nudgey, yend = quiver$yend[4],
           curvature = quiver$curvature[4], arrow = arrow(length = unit(arhead, "npc"))) +
  ### Insight 3
  annotate("text", x = quiver$x[5]+quiver$nudgex[5], y = quiver$y[5], hjust = quiver$hjust[5], label = msg[13],
           size = typogr, family = fam, lineheight = lnhgt) +
  annotate("text", x = quiver$x[5]+quiver$nudgex[5], y = quiver$y[5]-0.8*nudgey, hjust = quiver$hjust[5], label = msg[14],
           size = typogr, family = fam, lineheight = lnhgt, parse = TRUE) +
  annotate("text", x = quiver$x[5]+quiver$nudgex[5], y = quiver$y[5]-0.8*nudgey, hjust = quiver$hjust[5], label = msg[15],
           size = typogr, family = fam, lineheight = lnhgt, parse = TRUE, color = "#984ea3") +
  annotate("text", x = quiver$x[5]+quiver$nudgex[5], y = quiver$y[5]-0.8*nudgey, hjust = quiver$hjust[5], label = msg[16],
           size = typogr, family = fam, lineheight = lnhgt, parse = TRUE, color = "#f781bf") +
  annotate("text", x = quiver$x[5]+quiver$nudgex[5], y = quiver$y[5]-2.7*nudgey, hjust = quiver$hjust[5], label = msg[17],
           size = typogr, family = fam, lineheight = lnhgt) +
  annotate("curve", x = quiver$x[5]-2.55, xend = quiver$xend[5], y = quiver$y[5]-2.7*nudgey, yend = quiver$yend[5],
           curvature = quiver$curvature[5], arrow = arrow(length = unit(arhead, "npc"))) +
  annotate("curve", x = quiver$x[6]+2.55, xend = quiver$xend[6], y = quiver$y[6]-2.7*nudgey, yend = quiver$yend[6],
           curvature = quiver$curvature[6], arrow = arrow(length = unit(arhead, "npc"))) +
  ### Instructions 2
  annotate("text", x = quiver$x[7]+quiver$nudgex[7], y = quiver$y[7], hjust = quiver$hjust[7], label = msg[18],
           size = typogr, family = fam, lineheight = lnhgt) +
  annotate("curve", x = quiver$x[7], xend = quiver$xend[7], y = quiver$y[7], yend = quiver$yend[7],
           curvature = quiver$curvature[7], arrow = arrow(length = unit(arhead, "npc"))) +
  
  ## Maps titles
  annotate("rect", xmin = 21.8, xmax = 26, ymin = -Inf, ymax = Inf, fill = "#f0f0f0") +
  annotate("text", x = 25.8, y = 1.7, hjust = 1, vjust = 1, size = 16, family = "ubuntu", fontface ="bold", lineheight = lnhgt, 
           label = "States statistics relative to\n the US average in 2019 (%)") +
  annotate("text", x = 25.8, y = 1.64, hjust = 1, vjust = 1, size = 10, family = "ubuntu",
           label = "Darker colors indicate higher values") +
  annotate("text", x = 25.8, y = 1.61, hjust = 1, vjust = 1, size = 14, family = "ubuntu",
           label = "Registered nurses (RN)") +
  annotate("text", x = 25.8, y = 1.38, hjust = 1, vjust = 1, size = 14, family = "ubuntu",
           label = "Estimated population") +
  annotate("text", x = 25.8, y = 1.15, hjust = 1, vjust = 1, size = 14, family = "ubuntu",
           label = "Average RN salary") +
  annotate("text", x = 25.8, y = 0.92, hjust = 1, vjust = 1, size = 14, family = "ubuntu",
           label = "Regional Price Parities") +
  
  scale_shape_manual(values = c(21,22,24,25), breaks = c("Midwest", "West", "Northeast", "South")) +
  ylim(0.6,1.7) + xlim(4,26) +
  theme_ipsum() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "#fafafa", color = NA))

p <- p +
  inset_element(p1, left = 0.795, bottom = 0.70, right = 0.955, top = 0.86) +
  inset_element(p2, left = 0.795, bottom = 0.51, right = 0.955, top = 0.67) + 
  inset_element(p3, left = 0.795, bottom = 0.32, right = 0.955, top = 0.48) + 
  inset_element(p4, left = 0.795, bottom = 0.13, right = 0.955, top = 0.29)

## Saves the plot
ggsave("week41/nurse.png", plot = p, width = 45, height = 25, units = "cm")