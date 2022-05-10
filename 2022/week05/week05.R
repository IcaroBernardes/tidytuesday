# 0. Library and fonts management
library(tidyverse)
library(ragg)
library(glue)
library(FactoMineR)
library(factoextra)
library(ggtext)
library(ggfx)
library(ggimage)
library(magick)

## Defines some layout constants
lnhgt <- 1.2 ### To set the lineheight
width <- 26 ### Width of the plot
height <- 26 ### Height of the plot
bgcolor <- "#0F9EB3" ### Color of the background

## Defines the fonts. Downloaded from https://fontmeme.com and https://fonts.google.com
font_dog <- "Ennobled Pet"
font_title <- "Ubuntu"
font_body <- "Bitter"
font_icons <- "FontAwesome"
font_photos <- "Permanent Marker"

# 1. Data download, load and handling
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')

## Makes the breeds as the rownames
df <- df %>% tibble::column_to_rownames(var = "Breed")

## Takes the "Plott Hounds" breed out of the data
## as their values are out of scale (equal to zero)
df <- df %>% dplyr::filter(if_all(.fns = ~(. != 0)))

## Eliminates the "Coat Type" variable (too many categories)
df <- df %>% dplyr::select(-"Coat Type")

## Converts all variables to characters
df <- df %>% dplyr::mutate(across(.fns = as.character))

## Gets the breakdown of the categories for each variable
df %>% 
  tidyr::pivot_longer(cols = everything(),
                      names_to = "category") %>% 
  dplyr::count(category, value) %>% 
  ggplot(aes(x = n, y = value)) +
  geom_col() +
  facet_wrap(~category, scales = "free_y")

## Shows the relative amounts of each category compared to the biggest one
df %>% 
  tidyr::pivot_longer(cols = everything(),
                      names_to = "category") %>% 
  dplyr::count(category, value) %>% 
  dplyr::group_by(category) %>% 
  dplyr::mutate(rel = n/max(n)) %>% 
  ggplot(aes(x = rel, y = value)) +
  geom_col() +
  geom_vline(xintercept = 0.1, color = "red") +
  facet_wrap(~category, scales = "free_y")

## Reduces the scale of the ordinal traits
## and creates a common label for all traits
df <- df %>% 
  dplyr::mutate(across(
    .fns = ~case_when(. %in% c("1", "2", "Short") ~ "Bellow mid-scale",
                      . %in% c("4", "5", "Long") ~ "Above mid-scale",
                      . %in% c("3", "Medium") ~ "Mid-scale",
                      TRUE ~ "?")
  ))

## Makes the Multiple Correspondence Analysis for the data
res.mca <- FactoMineR::MCA(df, graph = FALSE)

## Shows the amount of variability that the first dimensions hold
factoextra::fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 12), ncp = 5)

## Gets the set of categories and dimensions that have a
## cos2 equal or superior to 0.3 so they are somewhat well represented
var.mca <- factoextra::get_mca_var(res.mca)
var.cos2 <- var.mca$cos2 %>% 
  dplyr::as_tibble()
var.cos2 <- var.cos2 %>% 
  dplyr::mutate(categ = rownames(var.mca$cos2)) %>%
  dplyr::select(categ, where(~ max(.x) >= 0.3)) %>% 
  dplyr::filter(if_any(.cols = where(is.numeric),
                       .fns = ~(. >= 0.3))) %>% 
  tidyr::pivot_longer(cols = starts_with("Dim"),
                      names_to = "Dim",
                      values_to = "cos2") %>% 
  dplyr::group_by(categ) %>% 
  dplyr::filter(cos2 == max(cos2)) %>% 
  dplyr::ungroup()
pcs <- unique(var.cos2$Dim)

## Gets the contribution and coordinate for these categories
## in the component in which they are best represented
var.contrib <- var.mca$contrib %>% 
  dplyr::as_tibble()
var.contrib <- var.contrib %>% 
  dplyr::mutate(categ = rownames(var.mca$contrib)) %>% 
  tidyr::pivot_longer(cols = starts_with("Dim"),
                      names_to = "Dim",
                      values_to = "contrib")
var.coord <- var.mca$coord %>% 
  dplyr::as_tibble()
var.coord <- var.coord %>% 
  dplyr::mutate(categ = rownames(var.mca$coord)) %>% 
  tidyr::pivot_longer(cols = starts_with("Dim"),
                      names_to = "Dim",
                      values_to = "coord")
var.info <- var.cos2 %>% 
  dplyr::left_join(var.contrib) %>% 
  dplyr::left_join(var.coord)

## Gets the breeds that are somewhat well represented by the chosen dimensions
ind.mca <- factoextra::get_mca_ind(res.mca)
ind.cos2 <- ind.mca$cos2 %>% 
  dplyr::as_tibble()
ind.cos2 <- ind.cos2 %>% 
  dplyr::mutate(Breed = rownames(ind.mca$cos2)) %>%
  dplyr::select(Breed, all_of(pcs)) %>% 
  dplyr::filter(if_any(.cols = where(is.numeric),
                       .fns = ~(. >= 0.3))) %>% 
  tidyr::pivot_longer(cols = starts_with("Dim"),
                      names_to = "Dim",
                      values_to = "cos2") %>% 
  dplyr::group_by(Breed) %>% 
  dplyr::filter(cos2 >= 0.3) %>% 
  dplyr::ungroup()

## Gets the contribution and coordinate for these breeds
## in the component in which they are represented well
ind.contrib <- ind.mca$contrib %>% 
  dplyr::as_tibble()
ind.contrib <- ind.contrib %>% 
  dplyr::mutate(Breed = rownames(ind.mca$contrib)) %>% 
  tidyr::pivot_longer(cols = starts_with("Dim"),
                      names_to = "Dim",
                      values_to = "contrib")
ind.coord <- ind.mca$coord %>% 
  dplyr::as_tibble()
ind.coord <- ind.coord %>% 
  dplyr::mutate(Breed = rownames(ind.mca$coord)) %>% 
  tidyr::pivot_longer(cols = starts_with("Dim"),
                      names_to = "Dim",
                      values_to = "coord")
ind.info <- ind.cos2 %>% 
  dplyr::left_join(ind.contrib) %>% 
  dplyr::left_join(ind.coord)

## Creates a simplified version of the name of the breeds
breed_rank_all <- breed_rank_all %>%
  dplyr::mutate(joiner = str_remove_all(Breed, "[:punct:]|[:space:]"),
                joiner = tolower(joiner)) %>%
  dplyr::select(joiner, `2020 Rank`, Image)

## Keeps only the selected breeds
ind.info <- ind.info %>%
  dplyr::mutate(joiner = str_remove_all(Breed, "[:punct:]|[:space:]"),
                joiner = tolower(joiner))
breed_rank_all <- breed_rank_all %>%
  dplyr::filter(joiner %in% ind.info$joiner)

## Gets the images of the dogs and creates a white frame around them
func <- function(name, rank, url) {
  img = magick::image_read(url)
  img = image_modulate(img, brightness = 80)
  img = magick::image_scale(img, "450")
  img = magick::image_border(img, color = "white", geometry = "50x50")
  img = magick::image_extent(img, color = "white", geometry = "x650", gravity = "North")
  magick::image_write(img, glue::glue("2022/week05/images/{name}.png"))
}
purrr::pwalk(breed_rank_all, ~func(..1, ..2, ..3))

## Keeps the path to the local images
breed_rank_all <- breed_rank_all %>%
  dplyr::mutate(image = glue::glue("2022/week05/images/{joiner}.png")) %>%
  dplyr::select(joiner, `2020 Rank`, image)

## Inserts the 2020 popularity rank info and images for the breeds
ind.info <- ind.info %>%
  dplyr::left_join(breed_rank_all) %>%
  dplyr::select(-joiner)

## Defines the text of the footnote
footnote <- "Data from the American Kennel Club, courtesy of KKakey | Graphic by Ícaro Bernardes (@IcaroBSC)"

## Gives coordinates for the details
details <- tibble(
  x = 0.05,
  y = 0.83,
  size = 7,
  label = c(
    'The American Kennel Club has a trait table for almost two hundred Dog Breeds.<br>
    Most of them are ordinal traits scaled from 1 to 5 while others are categorical or text-coded ordinal.<br>
    For this analysis, the ordinal variables were brought to a smaller scale as the extreme categories were lumped together:<br>
    1 and 2 became "Bellow mid-scale", 3 became "Mid-scale", 4 and 5 became "Above mid-scale".<br>
    Then, using Multiple Correspondence Analysis, new variables (dimensions) were created as combination of correlated features.<br>
    These dimensions have no correlation with each other and encompass most of the diversity in the data.<br>
    Three dimensions (PC 1, 2 and 4) had at least one feature somewhat well represented (with a squared cosine over 0.3).<br><br>
    <span style="font-size:50px;">**They estimate how much a dog is:**</span>'
  )
)

## Gives coordinates for the icons and labels of the dimensions
dims <- tibble(
  x = 0.11,
  y = c(0.64,0.43,0.22),
  icon = c("\uf004","\uf0e7","\uf132"),
  color = c("#FFB0D9","#F4FFA5","#FFC5A0"),
  label = c(
    "<span style='font-size:120px;color:#FFB0D9;'>**Giving**</span><br><br>
    Whether a dog shows mid-scale or above mid-scale
    **openness to strangers**, **playfulness level**,
    **adaptability level** and **affection with the family**.</span>",
    
    "<span style='font-size:120px;color:#F4FFA5;'>**Energetic**</span><br><br>
    Whether a dog shows bellow mid-scale or not
    **playfulness level** and **energy level**.",
    
    "<span style='font-size:120px;color:#FFC5A0;'>**Protective**</span><br><br>
    Whether a dog shows mid-scale or above mid-scale
    **watchdog/protective nature**."
  )
)

## Defines coordinates for the photos and their labels
set.seed(42)
photos <- ind.info %>%
  dplyr::mutate(coord = ifelse(Dim == "Dim 1", -coord, coord)) %>% 
  dplyr::mutate(y = case_when(Dim == "Dim 1" ~ 0.54,
                              Dim == "Dim 2" ~ 0.33,
                              Dim == "Dim 4" ~ 0.12,
                              TRUE ~ 0),
                y = y + sample(seq(-0.02, 0.02, 0.0001), n())) %>% 
  dplyr::group_by(Dim) %>% 
  dplyr::mutate(x = seq(0.08, 0.08+0.05*n(), length.out = n())) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    atrib = case_when(Dim == "Dim 1" ~ "giving",
                      Dim == "Dim 2" ~ "energetic",
                      Dim == "Dim 4" ~ "protective",
                      TRUE ~ ""),
    direc = ifelse(coord > 0, "more", "less"),
    label = glue::glue("<span style='font-size:6px;'>{Breed}</span><br>
                                   <span style='font-size:4px;'>{direc} {atrib}</span>"))

## Defines coordinates for the background for the dimensions
bgdims <- tibble(
  x = 0.5,
  y = c(0.59, 0.38, 0.17),
  fill = c("#a10053", "#b0c702", "#a84200")
)

# 2. Generates the plot
## Makes the plot
p <- ggplot() +
  
  ### Places the title and its background
  annotate("tile", x = 0.5, y = 0.92, width = 1, height = 0.12,
           fill = "#f2fcfc", color = NA) +
  annotate("text", x = 0.54, y = 0.92, label = "What makes your\nunique?",
           fontface = "bold", color = bgcolor, size = 28, hjust = 1,
           lineheight = 0.9, family = font_title) +
  annotate("text", x = 0.59, y = 0.925, label = "DOG", color = bgcolor,
           size = 53, hjust = 0, family = font_dog) +
  
  ### Places the footnote and its background
  annotate("tile", x = 0.5, y = 0.03, width = 1, height = 0.03,
           fill = "#f2fcfc", color = NA) +
  annotate("text", x = 0.5, y = 0.03, label = footnote, fontface = "bold",
           color = bgcolor, size = 8, family = font_title) +
  
  ### Places the details
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        hjust = 0,
                        vjust = 1, fill = NA, label.color = NA, color = "white",
                        family = font_body, lineheight = lnhgt,
                        data = details) +
  
  ### Places the background for the dimensions
  geom_tile(aes(x = x, y = y, fill = I(fill)), color = NA,
            width = 1, height = 0.2, data = bgdims) +
  
  ### Places the icons, labels and descriptions of the dimensions
  geom_text(aes(x = x, y = y, label = icon, color = I(color)),
            family = font_icons, size = 57, data = dims) +
  ggtext::geom_richtext(aes(x = x, y = y, label = label), size = 8,
                        nudge_x = 0.07, nudge_y = -0.007, hjust = 0,
                        fill = NA, label.color = NA, color = "white", 
                        lineheight = lnhgt, family = font_body, data = dims) +
  
  ### Places the photos
  ggfx::with_shadow(
    ggimage::geom_image(aes(x = x, y = y, image = image, size = 0.05, group = Breed),
                        by = "height", asp = width/height, data = photos)
  ) +
  
  ### Places the label of the photos
  ggtext::geom_richtext(aes(x = x, y = y, label = label), nudge_y = -0.019,
                        fill = NA, label.color = NA, size = 1,
                        family = font_photos, data = photos) +
  
  ### Defines limits for the axes
  coord_cartesian(ylim = c(0,1), xlim = c(0,1), expand = FALSE) +
  
  ### Eliminates theme elements
  theme_void()

## Saves the plot
ggsave("2022/week05/traits.png", plot = p, dpi = "retina", bg = bgcolor,
       width = width, height = height, device = ragg::agg_png, res = 320)


