# 0. Library management
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(lubridate)
library(showtext)
library(ggimage)
library(scales)

## Add Google Font
font_add_google(name = "Oswald", family = "oswald")
font_add_google(name = "Cookie", family = "cookie")

# 1. Data download, join and handle
tuesdata <- tidytuesdayR::tt_load('2021-09-14')

board <- tuesdata$billboard
board <- board %>% 
  dplyr::mutate(week_id = mdy(week_id)) ## Lubridate parsing of string to date
feat <- tuesdata$audio_features

df <- full_join(board, feat) %>%
  distinct() ## Eliminates entirely duplicated lines

# 2. Loudness plot
loud <- df %>%
  dplyr::select(song_id, loudness, week_id) %>% 
  ## Eliminates duplicate instances of songs, keeping all variables
  distinct(song_id, .keep_all = TRUE) %>% 
  ## Extracts year from the date
  dplyr::mutate(year = lubridate::year(week_id)) %>% 
  ## Keeps only songs with loudness and year info
  dplyr::filter(!is.na(loudness) & !is.na(year)) %>% 
  dplyr::select(loudness, year) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(loudness = median(loudness), size = n()) %>% 
  ## Creates a new variable to help place the steps
  dplyr::mutate(step = loudness+22)

## Steps to break the medians
ticks <- 0.25

## Breaks down the medians in small steps
aux2 <- tibble(year = 0, loudness = 0, size = 0, step = 0)
for (i in 1:dim(loud)[1]) {
  
  aux <- tibble(
    year = loud$year[i],
    loudness = loud$loudness[i],
    size = loud$size[i],
    step = seq(0, loud$step[i], ticks)
  )
  
  aux2 <- rbind(aux2, aux)
  
}
loud <- aux2[-1,]

## Increment/decrement to set the limits of each step  
del <- 0.05

## Establishes max and min in y for each step
aux2 <- tibble(year = 0, loudness = 0, size = 0, step = 0, y = 0)
for (i in 1:dim(loud)[1]) {
  
  aux <- tibble(
    year = loud$year[i],
    loudness = loud$loudness[i],
    size = loud$size[i],
    step = loud$step[i],
    y = c(loud$step[i]-del,loud$step[i]+del)
  )
  
  aux2 <- rbind(aux2, aux)
  
}
loud <- aux2[-1,]

## Establishes max and min in x for each step
loud <- loud %>% 
  ### Creates new variables to set the x-limits of the rounded squares
  dplyr::mutate(start = year-0.25,
                end = year+0.25) %>% 
  ### Lengthens the dataframe so these limits are allocated in each line
  pivot_longer(cols = start:end,
               values_to = "x") %>% 
  dplyr::select(-name)

## Creates an id combining step and year
loud <- loud %>% 
  dplyr::mutate(id = paste(year,step))

## Defines the plot order of the poins
loud <- loud %>% 
  dplyr::mutate(order = rep(c(1,2,4,3), dim(loud)[1] / 4)) %>% 
  dplyr::arrange(id, order)

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext_auto()

## Prints the plot
loud %>% 
  ggplot(aes(x = x, y = y, group = id, fill = step)) +
  geom_polygon() +
  ## Title and subtitle annotation
  annotate("text", x = 1958, y = 20, label = "Turn it up!",
           colour = "white", size = 60, hjust = 0, vjust = 1, family = "oswald") +
  annotate("text", x = 1958, y = 16.9, label = "Have the Billboard hits become louder?",
           colour = "white", size = 18, hjust = 0, vjust = 1, family = "oswald") +
  annotate("text", x = 1958, y = 15.9, label = expression("Source: Data.World "^{"[1]"}~"|"~"Graphic: Ícaro Bernardes "^{"[2]"}),
           colour = "white", size = 12, hjust = 0, vjust = 1, family = "oswald") +
  ## Text and support annotations
  annotate("text", x = 2005, y = 19, family = "oswald",
           label = expression("In the 90's the tracks grew louder.\nAs more able engineers pushed the limits of the CD format\nthe industry started to associate comercial success to loudness. "^"[3]"),
           hjust = 0, vjust = 0, colour = "white", size = 12, lineheight = 0.3) +
  annotate("tile", x = 1990, y = 13.5, fill = "white", width = 4, height = 1) +
  annotate("point", x = 1990, y = 13, fill = "white", colour = "white", size = 3, shape = 25) +
  annotate("text", x = 1990, y = 13.5, label = "-9.32db", colour = "#9c6102", size = 15, family = "oswald") +
  annotate("tile", x = 2002, y = 17.5, fill = "white", width = 4, height = 1) +
  annotate("point", x = 2002, y = 17, fill = "white", colour = "white", size = 3, shape = 25) +
  annotate("text", x = 2002, y = 17.5, label = "-5.43db", colour = "#DE0001", size = 15, family = "oswald") +
  ## Arrows annotations
  annotate("curve", x = 2004.5, y = 20, xend = 2002, yend = 18.2, arrow = arrow(length = unit(0.01, "npc")), color = "white", curvature = 0.3) +
  annotate("curve", x = 2004.5, y = 20, xend = 1990, yend = 14.2, arrow = arrow(length = unit(0.01, "npc")), color = "white", curvature = 0.4) +
  ## Axis titles and footnotes
  labs(x = "Year", y = "Median loudness of the songs (db)",
       caption = paste(
         "[1]: Data.World by Sean Miller. https://data.world/kcmillersean/billboard-hot-100-1958-2017#",
         "[2]: Tidytuesday repository. https://github.com/IcaroBernardes/tidytuesday",
         "[3]: Loudness war. https://en.wikipedia.org/wiki/Loudness_war",
         sep = "\n")
  ) +
  scale_fill_gradient2(low="#000C66", high="#DE0001", mid="#FEDA15", midpoint=median(loud$step)) +
  scale_y_continuous(limits = c(-1,20), labels = function(x) x-20) +
  scale_x_continuous(n.breaks = 7) +
  theme_ipsum() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "black"),
        axis.text.x = element_text(size = 45, colour = "white", family = "oswald"),
        axis.text.y = element_text(size = 45, colour = "white", family = "oswald"),
        axis.title.x = element_text(size = 60, colour = "white", family = "oswald"),
        axis.title.y = element_text(size = 60, colour = "white", family = "oswald"),
        ## Modifies lineheight to control the gap between lines of footnotes
        plot.caption = element_text(size = 20, colour = "white", hjust = 0, family = "oswald", lineheight = 0.4))

## Saves the plot
ggsave("week38/loudness.png", width = 40, height = 20, units = "cm")

# 3. The Christmas Market plot
## List of songs of interest
interest <- c("Rockin' Around The Christmas TreeBrenda Lee",
              "All I Want For Christmas Is YouMariah Carey",
              "The Christmas Song (Merry Christmas To You)Nat King Cole",
              "Jingle Bell RockBobby Helms",
              "White ChristmasBing Crosby")

xmas <- df %>% 
  dplyr::filter(song_id %in% interest) %>% 
  dplyr::select(week_id:song_id, instance) %>% 
  distinct() %>% 
  ## Extracts year and month from the date
  dplyr::mutate(year = lubridate::year(week_id)) %>% 
  dplyr::mutate(month = lubridate::month(week_id)) %>% 
  ## Separates the tracks performance in two eras
  dplyr::mutate(era = case_when(year <= 1970 ~ "60s",
                                year >= 2010 ~ "XXI",
                                TRUE ~ "-")) %>% 
  ## Eliminates instances too isolated in time
  dplyr::filter(era != "-") %>% 
  ## Converts dates to strings so only those dates are ploted
  dplyr::arrange(week_id) %>% 
  dplyr::mutate(week_id = as.character(week_id))


## Visualizing the data
xmas %>% 
  ggplot(aes(x = week_id, y = week_position, color = song)) +
  geom_line(aes(group = paste0(song,instance))) +
  geom_line(aes(group = song), linetype = "dashed") +
  geom_point() +
  geom_label(aes(label = week_position), size = 3, nudge_x = 0.3) +
  theme_ipsum() +
  facet_grid(.~era, scales = "free_x", space = "free_x") +
  scale_y_reverse(breaks = c(c(1,3,5),seq(10,100,by=10))) +
  ### Takes the parts of the date in string format to compose the labels
  scale_x_discrete(labels = function(x) paste(str_sub(x,1L,4L), str_sub(x,6L,7L), str_sub(x,9L,10L), sep = "\n")) +
  coord_cartesian(ylim = c(100,1)) +
  theme(legend.position = "bottom")

## Creates new data summing observations
yoyo <- tibble(
  period = c(rep("1958\nand\n1959",2),
             rep("1960",4),
             rep("1961",3),
             rep("1962",4),
             "Silence 1",
             "Silence 2",
             "Silence 3",
             "2012",
             rep("2013",3),
             "2014",
             rep("2015",4),
             rep("2016",4),
             rep("2017",3),
             rep("2018",5),
             rep("2019",5),
             rep("2020",5)
  ),
  position = c(35,59,
               80,36,18,26,
               50,41,12,
               56,59,65,38,
               NA,NA,NA,
               21,
               26,45,50,
               35,
               11,30,38,47,
               16,27,29,47,
               9,34,38,
               3,8,9,11,34,
               1,2,3,16,42,
               1,2,3,11,20
  ),
  song_id = c("Jingle Bell RockBobby Helms","White ChristmasBing Crosby",
              "The Christmas Song (Merry Christmas To You)Nat King Cole","Jingle Bell RockBobby Helms","Rockin' Around The Christmas TreeBrenda Lee","White ChristmasBing Crosby",
              "Rockin' Around The Christmas TreeBrenda Lee","Jingle Bell RockBobby Helms","White ChristmasBing Crosby",
              "Jingle Bell RockBobby Helms","Rockin' Around The Christmas TreeBrenda Lee","The Christmas Song (Merry Christmas To You)Nat King Cole","White ChristmasBing Crosby",
              NA,NA,NA,
              "All I Want For Christmas Is YouMariah Carey",
              "All I Want For Christmas Is YouMariah Carey","The Christmas Song (Merry Christmas To You)Nat King Cole","Rockin' Around The Christmas TreeBrenda Lee",
              "All I Want For Christmas Is YouMariah Carey",
              "All I Want For Christmas Is YouMariah Carey","Rockin' Around The Christmas TreeBrenda Lee","The Christmas Song (Merry Christmas To You)Nat King Cole","Jingle Bell RockBobby Helms",
              "All I Want For Christmas Is YouMariah Carey","Rockin' Around The Christmas TreeBrenda Lee","Jingle Bell RockBobby Helms","The Christmas Song (Merry Christmas To You)Nat King Cole",
              "All I Want For Christmas Is YouMariah Carey","Rockin' Around The Christmas TreeBrenda Lee","The Christmas Song (Merry Christmas To You)Nat King Cole",
              "All I Want For Christmas Is YouMariah Carey","Jingle Bell RockBobby Helms","Rockin' Around The Christmas TreeBrenda Lee","The Christmas Song (Merry Christmas To You)Nat King Cole","White ChristmasBing Crosby",
              "All I Want For Christmas Is YouMariah Carey","Rockin' Around The Christmas TreeBrenda Lee","Jingle Bell RockBobby Helms","The Christmas Song (Merry Christmas To You)Nat King Cole","White ChristmasBing Crosby",
              "All I Want For Christmas Is YouMariah Carey","Rockin' Around The Christmas TreeBrenda Lee","Jingle Bell RockBobby Helms","The Christmas Song (Merry Christmas To You)Nat King Cole","White ChristmasBing Crosby"
  )
)

## Aids the creation of a new variable to make the background tiles alternating in color
## and another to identify the two sections separated by the Intermezzo
bkg <- tibble(
  period = unique(yoyo$period),
  # bg = rep(c("a","b"),7),
  sep = c(rep("a",4),rep("b",3),rep("c",9))
)
yoyo <- yoyo %>% dplyr::left_join(bkg)

## Converts the period to factor to keep the order
yoyo <- yoyo %>% 
  dplyr::mutate(period = factor(period, levels = unique(yoyo$period)))

## Plot axis limits
inf <- 120
sup <- -20
lft <- -1
rgt <- 17

## Calculates how many Christmas songs make to the list in the Christmas season
mount <- df %>% 
  ### Obtains month info. and filters data from November, December and January
  dplyr::mutate(month = lubridate::month(week_id)) %>% 
  dplyr::filter(month %in% c(11,12,1)) %>% 
  dplyr::select(week_id:song_id, spotify_track_album, month) %>% 
  distinct() %>% 
  ### Replaces not available data in album names by an hyphen
  dplyr::mutate(spotify_track_album = ifelse(is.na(spotify_track_album), "-", spotify_track_album)) %>% 
  ### Search for strings that contain the characters "christ" or "xmas"
  ### in the song or album name while ignoring case sensitivity
  dplyr::mutate(xmas = str_detect(song, regex("christ|xmas", ignore_case  = TRUE)) |
                  str_detect(spotify_track_album, regex("christ|xmas", ignore_case  = TRUE))) %>% 
  dplyr::filter(xmas) %>% 
  ### Obtains year info. and separates the periods by subtracting the year of the january months
  dplyr::mutate(year = lubridate::year(week_id), .before = month) %>% 
  dplyr::mutate(year = ifelse(month == 1, year-1, year)) %>% 
  ### Counts tracks per year
  dplyr::count(year) %>% 
  ### Separates the years by period and takes the mean frequency
  dplyr::mutate(period = case_when(year %in% 1958:1959 ~ "1958\nand\n1959",
                                   year %in% c(1960:1962,2012:2020) ~ as.character(year),
                                   TRUE ~"Silence 2"
  )) %>% 
  dplyr::group_by(period) %>% 
  dplyr::summarise(n = mean(n)) %>% 
  ### Rescale inverse values so they are plotted at the bottom of the graph
  dplyr::mutate(pos = scales::rescale(1/n, to = c(85,inf-10))) %>% 
  dplyr::select(-n) %>% 
  ### Reorder the data to correspond to the periods
  dplyr::mutate(period = factor(period, levels = levels(yoyo$period))) %>% 
  dplyr::arrange(period) %>% 
  dplyr::mutate(period = as.numeric(period)) %>% 
  ### Creates id variable
  dplyr::mutate(id = period)

## Creates random values to emulate a mountainous landscape
mount <- rbind(c(lft-1,inf+10,NA),mount,c(rgt+1,inf+10,NA))
aux2 <- mount
step <- 0.1
for (i in mount$period[-1]) {
  
  j <- which(mount$period == i)
  per <- seq(mount$period[j-1]+step, mount$period[j]-step, by = step)
  a <- (mount$pos[j]-mount$pos[j-1])/(mount$period[j]-mount$period[j-1])
  b <- mount$pos[j]-a*mount$period[j]
  
  sd1 <- seq(0, 1, length.out = length(per) %/% 2)
  sd2 <- seq(1, 0, length.out = length(per) %/% 2)
  sd <- c(sd1,1,sd2)
  
  id1 <- rep(mount$id[j-1], 2)
  id2 <- rep(NA, length(per)-4)
  id3 <- rep(mount$id[j], 2)
  id <- c(id1,id2,id3)
  
  aux <- tibble(
    period = per,
    pos = a*per + b + rnorm(length(per), sd = sd),
    id = id
  )
  
  aux2 <- rbind(aux2,aux)
  
}

mount <- aux2 %>% 
  dplyr::arrange(period)

## Creates the "snowy peak"
peak <- mount %>% 
  dplyr::filter(!is.na(id))
peak2 <- peak %>% 
  dplyr::group_by(id) %>% 
  dplyr::slice(-c(1,n())) %>% 
  dplyr::ungroup()
peak2 <- peak2 %>% 
  dplyr::mutate(pos = pos + ((inf-pos)/20) + rnorm(dim(peak2)[1], sd = 0.5), .after = pos) %>% 
  dplyr::arrange(id, desc(period))
peak <- rbind(peak, peak2)
peak <- peak %>% 
  dplyr::arrange(id)

## Creates "snow flakes"
flakes <- tibble(
  x = sample(x = seq(lft, rgt, length.out = 300), size = 150),
  y = sample(x = seq(sup, inf, length.out = 300), size = 150),
  size = rnorm(150, mean = 0.5, sd = 0.5)
)

## Defines lineheight and gap between text and photos
lnt <- 0.25
gap <- 0.6

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext_auto()

yoyo %>% 
  ggplot(aes(x = as.numeric(period), y = position)) +
  
  ## "snow flakes" (makes the saving of the graph take somewhat longer)
  geom_point(aes(x = x, y = y, size = size), data = flakes, shape = 8, color = "white", alpha = 0.3) +
  
  ## Segment tying Mariah Carey's song performance between eras
  annotate("segment", x = 6, xend = 8, y = 83, yend = 21, size = 1.5, color = "#e41a1c", linetype = "dashed") +
  
  ## Lines and points showing songs peaks and evolution
  geom_line(aes(color = song_id, group = paste0(sep, song_id)), size = 2) +
  geom_point(aes(color = song_id), size = 4, stroke = 3, shape = 21, fill = "white") +
  
  ## "snowy mountains" plot
  geom_polygon(aes(x = period, y = pos), data = mount, fill = "#A8A8C0") +
  geom_polygon(aes(x = period, y = pos, group = id), data = peak, fill = "white") +
  
  ## Title and subtitles
  annotate("text", x = -1, y = sup-4, hjust = 0, vjust = 1, size = 70,
           family = "cookie", color = "white", label = "The Christmas Yo-Yos") +
  annotate("text", x = -1, y = sup+7, hjust = 0, vjust = 1, size = 30, family = "cookie", lineheight = lnt, color = "white",
           label = "The most recurring songs in the Billboard chart are Christmas songs.\nThey peak in Christmas season and hibernate the rest of the year.") +
  annotate("text", x = -1, y = sup+16, hjust = 0, vjust = 1, size = 17, family = "cookie", colour = "white",
           label = "Source: Data.World by Sean Miller (Billboard and Spotify data) | Graphic: Ícaro Bernardes | Mariah Carey's photo: https://www.youtube.com/watch?v=k-ME-4gcB40") +
  
  ## Explanatory texts and arrows
  annotate("text", x = 0, y = 1, hjust = 0, vjust = 1, size = 22, family = "cookie", color = "white", parse = TRUE,
           label = '"Around the early 60s," * phantom(" four ") * "songs were able to"') +
  annotate("text", x = 0, y = 1, hjust = 0, vjust = 1, size = 22, family = "cookie", color = "#FFA824", parse = TRUE,
           label = 'phantom("Around the early 60s,") * " four " * phantom("songs were able to")') +
  annotate("text", x = 0, y = 4, hjust = 0, vjust = 1, size = 22, family = "cookie", color = "white",
           label = "make this back and forth movement...") +
  annotate("curve", x = -0.1, xend = -0.5, y = 4.5, yend = 8, arrow = arrow(length = unit(0.005, "npc")), color = "white", curvature = 0.3) +
  annotate("text", x = 5.6, y = 95, hjust = 1, vjust = 1, size = 22, family = "cookie", color = "white", parse = TRUE,
           label = '"For almost" * phantom(" 50 years ") * "Christmas songs"') +
  annotate("text", x = 5.6, y = 95, hjust = 1, vjust = 1, size = 22, family = "cookie", color = "#F0D55E", parse = TRUE,
           label = 'phantom("For almost") * " 50 years " * phantom("Christmas songs")') +
  annotate("text", x = 5.6, y = 98, hjust = 1, vjust = 1, size = 22, family = "cookie", color = "white", lineheight = lnt,
           label = "weren`t able to stick to the season and\ntheir presence in the list dwindled") +
  annotate("curve", x = 5.8, xend = 6.5, y = 100, yend = 105, arrow = arrow(length = unit(0.005, "npc")), color = "white", curvature = -0.1) +
  annotate("text", x = 7.2, y = 70, hjust = 0, vjust = 0.5, family = "cookie", size = 22,
           lineheight = lnt, color = "white",
           label = "...however, years later, the popularity of Carey`s song soared.\nSince then, it has consistenly appeared in the list") +
  annotate("curve", x = 7.1, xend = 6.8, y = 70, yend = 68, arrow = arrow(length = unit(0.005, "npc")), color = "white", curvature = -0.1) +
  annotate("text", x = 16.5, y = 54, hjust = 1, vjust = 1, family = "cookie", size = 22, parse = TRUE, color = "white",
           label = '"...Carey`s hit reached the" * phantom(" seasonal top ") * "in the last years"') +
  annotate("text", x = 16.5, y = 54, hjust = 1, vjust = 1, family = "cookie", size = 22, parse = TRUE, color = "red",
           label = 'phantom("...Carey`s hit reached the") * " seasonal top " * phantom("in the last years")') +
  annotate("text", x = 16.5, y = 57, hjust = 1, vjust = 1, family = "cookie", size = 22, lineheight = lnt, color = "white",
           label = 'alognside Brenda Lee`s and Bobby Helm`s songs.\nAdditionally, more Christmas songs are making to the top 100.') +
  annotate("curve", x = 16.7, xend = 16.3, y = 58, yend = 1.5, arrow = arrow(length = unit(0.005, "npc")), color = "white", curvature = 0.35) +
  
  ## Labels for the periods
  annotate("text", x = c(1:4,8:16), y = inf, family = "cookie", size = 22,
           lineheight = lnt, color = "white", label = levels(yoyo$period)[c(1:4,8:16)]) +
  
  ## Intermezzo label and background
  annotate("text", x = 6, y = inf, family = "cookie", size = 28,
           lineheight = lnt, color = "white", label = "The silent\nIntermezzo") +
  
  ## Presenting the songs
  ### helms
  annotate("point", x = 1, y = 35, size = 15, stroke = 3,
           shape = 21, fill = "white", color = "#ffff33") +
  geom_image(aes(x = 1, y = 35, image = "week38/helms.png"), size = 0.035, by = "height") +
  annotate("text", x = 1-gap, y = 35, hjust = 1, vjust = 0.5, family = "cookie", size = 20,
           lineheight = lnt, color = "white", label = "...Jingle Bell Rock\nby Bobby Helms") +
  annotate("point", x = 1-0.3, y = 35+3, size = 8, stroke = 3,
           shape = 21, fill = "white", color = "#ffff33") +
  annotate("text", x = 1-0.3, y = 35+3, family = "cookie", size = 15,
           label = "35") +
  ### lee
  annotate("point", x = 2, y = 18, size = 15, stroke = 3,
           shape = 21, fill = "white", color = "#4daf4a") +
  geom_image(aes(x = 2, y = 18, image = "week38/lee.png"), size = 0.035, by = "height") +
  annotate("text", x = 2-gap, y = 18, hjust = 1, vjust = 0.5, family = "cookie", size = 20,
           lineheight = lnt, color = "white", label = "...Rockin' Around\nThe Christmas Tree\nby Brenda Lee") +
  annotate("point", x = 2-0.3, y = 18+3, size = 8, stroke = 3,
           shape = 21, fill = "white", color = "#4daf4a") +
  annotate("text", x = 2-0.3, y = 18+3, family = "cookie", size = 15,
           label = "18") +
  ### crosby
  annotate("point", x = 3, y = 12, size = 15, stroke = 3,
           shape = 21, fill = "white", color = "#ff7f00") +
  geom_image(aes(x = 3, y = 12, image = "week38/crosby.png"), size = 0.035, by = "height") +
  annotate("text", x = 3+gap, y = 12, hjust = 0, vjust = 0.5, family = "cookie", size = 20,
           lineheight = lnt, color = "white", label = "...White Christmas\nby Bing Crosby") +
  annotate("point", x = 3-0.3, y = 12+3, size = 8, stroke = 3,
           shape = 21, fill = "white", color = "#ff7f00") +
  annotate("text", x = 3-0.3, y = 12+3, family = "cookie", size = 15,
           label = "12") +
  ### cole
  annotate("point", x = 4, y = 65, size = 15, stroke = 3,
           shape = 21, fill = "white", color = "#984ea3") +
  geom_image(aes(x = 4, y = 65, image = "week38/cole.png"), size = 0.035, by = "height") +
  annotate("text", x = 4-gap, y = 65, hjust = 1, vjust = 0.5, family = "cookie", size = 20,
           lineheight = lnt, color = "white", label = "...The Christmas Song\n(Merry Christmas To You)\nby Nat King Cole") +
  annotate("point", x = 4-0.3, y = 65+3, size = 8, stroke = 3,
           shape = 21, fill = "white", color = "#984ea3") +
  annotate("text", x = 4-0.3, y = 65+3, family = "cookie", size = 15,
           label = "65") +
  ### carey
  annotate("point", x = 6, y = 83, size = 15, stroke = 3,
           shape = 21, fill = "white", color = "#e41a1c") +
  geom_image(aes(x = 6, y = 83, image = "week38/carey.png"), size = 0.035, by = "height") +
  annotate("text", x = 6+gap, y = 83, hjust = 0, vjust = 0.5, family = "cookie", size = 22,
           lineheight = lnt, color = "white",
           label = 'The big hit, "All I Want For Christmas Is You" by Mariah Carey,\nmodestly debuted in the list in the year 2000...') +
  annotate("point", x = 6-0.3, y = 83+3, size = 8, stroke = 3,
           shape = 21, fill = "white", color = "#e41a1c") +
  annotate("text", x = 6-0.3, y = 83+3, family = "cookie", size = 15,
           label = "83") +
  
  coord_cartesian(ylim = c(inf,sup), xlim = c(lft,rgt)) +
  scale_y_reverse() +
  scale_fill_manual(values = c("#00441b","#006d2c")) +
  scale_color_manual(values = c('#e41a1c','#ffff33','#4daf4a','#984ea3','#ff7f00')) +
  theme_ipsum() +
  theme(panel.background = element_rect(fill = "#415EEF", color = NA),
        plot.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
  )

## Saves the plot
ggsave("week38/christmas.png", width = 35, height = 35, units = "cm")

