# 0. Library and fonts management
library(tidyverse)
library(rlang)
library(glue)
library(reactable)
library(reactablefmtr)
library(htmlwidgets)
library(webshot2)

# 1. Data download, load and handling
## Gets the data and eliminates duplicates
chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv') %>%
  dplyr::arrange(desc(review_date)) %>% 
  dplyr::distinct(ref, .keep_all = TRUE)

## Gets the ingredients list for the chocolates
df <- chocolate %>% 
  dplyr::select(ingredients, rating) %>% 
  dplyr::mutate(ingredients = str_remove_all(ingredients,"-|[:digit:]|[:space:]")) %>% 
  dplyr::filter(if_all(.fns = ~!is.na(.)))

## Creates a function that detects the codes for the ingredients and
## creates dummy columns for their presence
dummycoder <- function(code, varname) {
  
  varname = rlang::sym(varname)
  
  df %>% 
    dplyr::select(-rating) %>% 
    purrr::map_dfr(~str_detect(., code)) %>% 
    dplyr::rename(!!varname := "ingredients")
  
}

## Applies the function in the set of ingredients
coded <- tibble(
  string = c("B", "S(?!\\*)", "S\\*",
             "V", "C", "L",
             "Sa"),
  varname = c("Cocoa Beans", "Sugar", "Other Sweeteners",
              "Vanilla", "Cocoa Butter", "Lecithin",
              "Salt")
)
df <- coded %>% 
  purrr::pmap(~dummycoder(..1, ..2)) %>% 
  purrr::reduce(cbind) %>% 
  cbind(df) %>% 
  dplyr::select(-ingredients)

## Creates a function that estimates the effect of each ingredient
## by comparing the mean rating of chocolates with/without them
## and associated with other ingredients
calceffect <- function(string) {
  
  varname = rlang::sym(string)
  
  groups = df %>% 
    dplyr::rename(varname := !!varname) %>% 
    dplyr::group_by(across(.cols = -c(varname, rating))) %>% 
    dplyr::filter(n() > 1) %>% 
    dplyr::mutate(id = glue::glue("{string}#{cur_group_id()}"))
  
  ratings = groups %>% 
    dplyr::group_by(id) %>% 
    dplyr::filter(n_distinct(varname) == 2) %>% 
    dplyr::group_by(id, varname) %>% 
    dplyr::summarise(rating = round(mean(rating), 2))
  
  effect = ratings %>% 
    dplyr::mutate(varname = ifelse(varname, 1, -1),
                  rating = varname*rating) %>% 
    dplyr::group_by(id) %>% 
    dplyr::summarise(Effect = round(sum(rating), 2)) %>% 
    dplyr::ungroup()
  
  groups = groups %>% 
    dplyr::select(-varname, -rating) %>% 
    dplyr::distinct()
  
  ratings = ratings %>% 
    dplyr::mutate(varname = ifelse(varname, "With", "Without")) %>% 
    tidyr::pivot_wider(names_from = varname,
                       values_from = rating)
  
  effect %>% 
    dplyr::left_join(groups) %>% 
    dplyr::left_join(ratings) %>% 
    dplyr::select(-id) %>% 
    dplyr::mutate(across(.cols = where(is.logical), .fns = ~(as.numeric(.)*4)-2),
                  `ingred` = string)
  
}

## Applies the function to all ingredients but
## "Cocoa Beans" as they are in all chocolates
data <- coded$varname[-1] %>% 
  purrr::map_dfr(calceffect)

## Adds description of the ingredients to the data
descripto <- tibble(
  ingred = c("Sugar", "Other Sweeteners", "Vanilla",
             "Cocoa Butter", "Lecithin", "Salt"),
  url = c("https://upload.wikimedia.org/wikipedia/commons/thumb/a/ac/Würfelzucker_--_2018_--_3582.jpg/1200px-Würfelzucker_--_2018_--_3582.jpg",
          "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3c/Assugrin_f3453504.jpg/1280px-Assugrin_f3453504.jpg",
          "https://upload.wikimedia.org/wikipedia/commons/4/40/Vanilla_planifolia_1.jpg",
          "https://upload.wikimedia.org/wikipedia/commons/thumb/d/df/Cocoa_butter_p1410148.JPG/220px-Cocoa_butter_p1410148.JPG",
          "https://upload.wikimedia.org/wikipedia/commons/thumb/0/07/Lecitina_de_soja_en_Montevideo.jpg/1024px-Lecitina_de_soja_en_Montevideo.jpg",
          "https://upload.wikimedia.org/wikipedia/commons/thumb/0/07/Comparison_of_Table_Salt_with_Kitchen_Salt.png/1280px-Comparison_of_Table_Salt_with_Kitchen_Salt.png"),
  desc = c("Dilutes the intense bitter flavor of pure<br>cocoa beans, making it more palatable. <sup>[1]</sup>",
           "Some chocolates use other sweetener<br>than white cane or beet sugar.",
           "Moderates the sweet and<br>bitter in chocolate. <sup>[2]</sup>",
           "Makes the chocolate easier to work with<br>by diminishing its viscosity. <sup>[3]</sup>",
           "Has similiar uses to Cocoa Butter,<br>but it's cheaper. <sup>[4]</sup>",
           "Accentuates sweetness by<br>creating contrast. <sup>[5]</sup>"
  )
) %>% 
  dplyr::mutate(`Ingredient under analysis` = glue::glue("<span style='float:left;width:250px;'><div style='font-size:26px;'><strong>{ingred}</strong></div><span style='font-size:12px;'>{desc}</span></span><img src='{url}' style='float:right;height:70px;'>"))
data <- data %>% dplyr::left_join(descripto)

## Defines the order of the columns
data <- data %>% 
  dplyr::select("Ingredient under analysis", "Effect", "Cocoa Beans",
                "Sugar", "Other Sweeteners", "Vanilla", "Cocoa Butter",
                "Lecithin", "Salt", "With", "Without")

# 2. Generates the plot
## Creates the table
table <- data %>% 
  reactable(
    .,
    
    theme = fivethirtyeight(centered = TRUE, header_font_size = 16),
    pagination = FALSE,
    
    columns = list(
      `Ingredient under analysis` = colDef(style = group_merge_sort("Ingredient under analysis"), html = TRUE, width = 400),

      `Effect` = colDef(style = color_scales(., colors = c("red","green"), bold_text = TRUE, text_size = 40), width = 150),
      
      `Cocoa Beans` = colDef(style = color_scales(., colors = c("red","green"), show_text = FALSE, span = TRUE),
                              headerStyle = list(writingMode = "vertical-rl"), width = 30),
      `Sugar` = colDef(style = color_scales(., colors = c("red","green"), show_text = FALSE, span = TRUE),
                       headerStyle = list(writingMode = "vertical-rl"), width = 30),
      `Other Sweeteners` = colDef(style = color_scales(., colors = c("red","green"), show_text = FALSE, span = TRUE),
                                  headerStyle = list(writingMode = "vertical-rl"), width = 30),
      `Cocoa Butter` = colDef(style = color_scales(., colors = c("red","green"), show_text = FALSE, span = TRUE),
                              headerStyle = list(writingMode = "vertical-rl"), width = 30),
      `Vanilla` = colDef(style = color_scales(., colors = c("red","green"), show_text = FALSE, span = TRUE),
                         headerStyle = list(writingMode = "vertical-rl"), width = 30),
      `Lecithin` = colDef(style = color_scales(., colors = c("red","green"), show_text = FALSE, span = TRUE),
                          headerStyle = list(writingMode = "vertical-rl"), width = 30),
      `Salt` = colDef(style = color_scales(., colors = c("red","green"), show_text = FALSE, span = TRUE),
                      headerStyle = list(writingMode = "vertical-rl"), width = 30),
      
      `With` = colDef(cell = icon_assign(., icon = "star", fill_color = "gold", show_values = "right"), width = 120),
      `Without` = colDef(cell = icon_assign(., icon = "star", fill_color = "gold", show_values = "right"), width = 120)
      
    ),
    
    columnGroups = list(
      colGroup(name = "Mean rating _____ the ingredient under analysis", columns = c("With", "Without")),
      colGroup(name = "Presence of other ingredients", columns = c("Cocoa Beans", "Sugar",
                                                                   "Other Sweeteners", "Vanilla",
                                                                   "Cocoa Butter", "Lecithin", "Salt"))
    )
    
  ) %>% 
  add_title("What makes a good chocolate?",
            
            font_size = 58,
            text_transform = "uppercase") %>% 
  add_subtitle('Georgios and Kelsey conducted the dataviz community to the flavorful database
               from Flavors of Cacao. It holds data on reviews of commercial chocolate bars.
               The table bellow shows for each "ingredient under analysis" the "effect" it has
               in the quality of the bars. That is, the difference in the mean rating of sets
               of bars with the same ingredients. These bars only differ in the presence or not
               of the "ingredient under analysis". For exemple, the second row of the table shows
               that bars made with only Cocoa Beans and Cocoa Butter have a mean rating of 3.0,
               while bars that have Cocoa Beans, Cocoa Butter and Sugar have a mean rating of 3.2.
               An effect of +0.2 in the rating. This estimate is not rigorous, since there are many
               factors that affect quality which are not taken into account in this analysis. This
               table was made by Ícaro Bernardes (@IcaroBSC). Images extracted from Wikipedia.',
               
               font_size = 16,
               font_color = "gray50",
               font_weight = "normal",
               margin = margin(15,20,30,0)) %>% 
  add_source("Sources of descriptions: [1] https://beantobarworld.com/myths-faq/does-sugar-bring-out-the-flavour-of-chocolate -
             [2] http://artchocolat.com/portfolio/vanilla-in-chocolate/ -
             [3] https://www.aalstchocolate.com/post/2016/11/21/chocolate-lesson-101-cocoa-butter -
             [4] https://www.hotelchocolat.com/uk/blog/chocolateknowledge/what-is-lecithin-and-why-is-it-in-chocolate.html -
             [5] https://www.thechocolatejournalist.com/blog/salt-chocolate",
             
             font_size = 12,
             font_color = "gray") %>% 
  google_font("Bitter")

## Saves a hmtl version of the table
htmlwidgets::saveWidget(widget = table, "2022/week03/chocolate.html")

## Saves a png version of the table
webshot2::webshot("2022/week03/chocolate.html", "2022/week03/chocolate.png", vwidth = 1100)

