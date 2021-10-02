# 0. Library management
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(showtext)
library(ggtext)
library(textclean)
library(tidytext)
library(tm)
library(fastDummies)
library(seriation)
library(BBmisc)
library(emojifont)

## Add Google Font
font_add_google(name = "Roboto Mono", family = "roboto")

# 1. Data download and handling
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

# 2. Buzzwords plots
docs <- papers %>% 
  dplyr::filter(year >= 1980) %>% 
  count(year, name = "total")

## Gets dummy variables indicating the programs association with the papers
progs <- paper_programs %>% 
  fastDummies::dummy_cols(select_columns = "program", remove_selected_columns = TRUE) %>% 
  dplyr::group_by(paper) %>% 
  dplyr::summarise(across(starts_with("program_"), sum))

## Obtains stopwords to be eliminated (e.g.: and, I, for, ...)
stop <- tm::stopwords("en")

## Cleans the text and lengthens the data so each row is a word
clean <- papers %>% 
  ### Eliminates old data (not many articles) and 2021 (incomplete data)
  dplyr::filter(between(year, 1980, 2020)) %>% 
  dplyr::mutate(title = textclean::strip(title, char.keep = NULL, lower.case = FALSE)) %>% 
  unnest_tokens(words, title, "words", to_lower = FALSE) %>% 
  ### Modifies the US (country) name to USA
  dplyr::mutate(words = ifelse(words == "US", "USA", words)) %>% 
  dplyr::mutate(words = tolower(words)) %>% 
  ### Unifies some singular/plural nouns
  dplyr::mutate(words = textclean::mgsub(words,
                                         c("effects","markets","models","rates"),
                                         c("effect","market","model","rate"))) %>%
  filter(!words %in% stop) %>% 
  ### Obtains the dummy association with the programs
  left_join(progs) %>% 
  dplyr::filter(across(starts_with("program_"), ~!is.na(.)))

## Counts the top 5 terms used in each year
## Breaks ties by count over all years 
themes <- clean %>% 
  dplyr::mutate(n = 1) %>% 
  dplyr::group_by(year, words) %>%
  dplyr::summarise(across(c(starts_with("program_"),n), sum)) %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::group_by(words) %>% 
  dplyr::mutate(total = sum(n)) %>% 
  dplyr::arrange(desc(n), desc(total)) %>% 
  dplyr::select(-total) %>% 
  dplyr::group_by(year) %>% 
  dplyr::slice_head(n = 5) %>% 
  dplyr::mutate(ord = row_number(desc(n))) %>% 
  ungroup()

## Shows the set of terms for all years
top <- unique(themes$words)
top

## Associates an FontAwesome icon to each buzzword
awesome <- tibble(
  awe = c("fa-search","fa-shopping-cart","fa-sign-out","fa-gavel",
          "fa-flag", "fa-briefcase","fa-bar-chart","fa-percent",
          "fa-dollar","fa-plus","fa-cogs","fa-shopping-basket",
          "fa-university","fa-arrow-circle-up","fa-balance-scale","fa-diamond",
          "fa-exchange","fa-arrow-circle-down","fa-tags","fa-line-chart",
          "fa-bed"),
  id = 1:21
)
awesome <- awesome %>% 
  dplyr::mutate(awe = emojifont::fontawesome(awe))

## Creates a tibble with the overall counts for the buzzwords
cnt <- clean %>% 
  dplyr::filter(words %in% top) %>% 
  dplyr::mutate(n = 1) %>% 
  dplyr::group_by(words) %>%
  dplyr::summarise(total = sum(n)) %>% 
  ungroup() %>% 
  dplyr::arrange(desc(total))

top <- unique(cnt$words)

cnt <- cnt %>% 
  dplyr::mutate(id = factor(words, levels = top)) %>% 
  dplyr::mutate(id = as.numeric(id)) %>% 
  dplyr::arrange(id) %>% 
  dplyr::mutate(pos = 1*str_length(words)+5.7) %>% 
  dplyr::mutate(pos = lag(cumsum(pos), default = 1)+3) %>% 
  dplyr::mutate(counts = paste(words, total, sep = "\n")) %>% 
  left_join(awesome)

## Calculates the best order of the programs by similarity
mat <- themes %>% 
  dplyr::select(starts_with("program_")) %>% 
  as.matrix()
colnames(mat) <- str_remove(colnames(mat),"program_")
set.seed(42)
o <- seriation::seriate(mat, method = "BEA_TSP")
col <- colnames(mat)[get_order(o, 2)]

## Normalizes data for each category in each year
themes <- themes %>% 
  dplyr::group_by(year) %>% 
  nest() %>% 
  dplyr::mutate(categ = map(data, ~.x %>% dplyr::select(-words,-n,-ord)),
                data = map(data, ~.x %>% dplyr::select(words,n,ord))) %>% 
  dplyr::mutate(intense = map(categ, ~BBmisc::normalize(.x, method = "standardize", margin = 2L))) %>% 
  unnest(cols = c(data, categ, intense), names_sep = "#")

## Creates levels for the x-axis by getting all combinations between two factors
poslevel <- expand.grid(sort(unique(themes$year)), 1:6)
poslevel <- poslevel %>% 
  dplyr::transmute(var = paste(Var1, Var2, sep=".")) %>% 
  dplyr::arrange(var) %>% 
  dplyr::pull()

## Rearranges the data and handles variable names
themes <- themes %>% 
  pivot_longer(cols = contains("program_"),
               names_to = c(".value", "set"),
               names_sep = "#") %>% 
  dplyr::mutate(set = str_remove(set,"program_")) %>% 
  dplyr::mutate(set = factor(set, levels = col)) %>% 
  dplyr::rename_with(.cols = starts_with("data#"), .fn = ~gsub("data#","",.))

## Makes the x-axis and y-data as factors and then, as numerics
themes <- themes %>% 
  dplyr::mutate(xpos = paste(year, ord, sep=".")) %>% 
  dplyr::mutate(xpos = factor(xpos, levels = poslevel)) %>% 
  dplyr::mutate(xpos = as.numeric(xpos)) %>% 
  dplyr::mutate(ypos = as.numeric(set))

## Creates a tibble to id the buzzwords
buzz <- themes %>% 
  dplyr::select(year, words, xpos) %>% 
  distinct() %>% 
  dplyr::mutate(id = factor(words, levels = top)) %>% 
  dplyr::mutate(id = as.numeric(id)) %>% 
  left_join(awesome)

## Optimally distinct colors obtained from https://mokole.com/palette.html
optcol <- c("#000000","#696969","#556b2f","#228b22","#8b0000",
            "#483d8b","#008b8b","#000080","#d2691e","#daa520",
            "#8fbc8f","#8b008b","#b03060","#0000ff","#8a2be2",
            "#e9967a","#00bfff","#ff0000","#ff00ff","#1e90ff",
            "#ff1493")

## Creates labels for the x-axis
xtext <- buzz %>% 
  dplyr::select(year, xpos) %>% 
  dplyr::summarise(across(.fns = mean))

## Creates labels for the y-axis
ytext <- programs %>% 
  dplyr::mutate(program = factor(program, levels = col)) %>% 
  dplyr::mutate(program = as.numeric(program)) %>% 
  dplyr::mutate(color = case_when(program_category == "Finance" ~ "#b03060",
                                  program_category == "Macro/International" ~ "#483d8b",
                                  program_category == "Micro" ~ "#d2691e",
                                  TRUE ~ "#000000")) %>% 
  ### Substitutes some long words in the description of the programs
  dplyr::mutate(program_desc = mgsub(program_desc,
                                     c("Development","conomics","conomic","Entrepreneurship","International","Innovation"),
                                     c("Devlp.","conom.","conom.","Entrep.","Internat.","Innov.")))

## Obtains the articles that use the most the buzzwords
essence <- clean %>% 
  dplyr::filter(words %in% top) %>% 
  distinct(paper, words) %>% 
  dplyr::count(paper)

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext_auto()

## Generates plot
p <- themes %>% 
  ggplot(aes(x = xpos, y = ypos)) +
  geom_tile(aes(fill = intense), color = "#000000C8") + 
  ## Title and subtitles
  ggplot2::annotate("text", x = 1, y = 30.3, size = 35, family = "roboto", hjust = 0, vjust = 1, label = "Buzzwords!") +
  ggplot2::annotate("text", x = 1, y = 27.6, size = 15, family = "roboto", hjust = 0, vjust = 1,
                    label = "What are the most frequent words in titles of NBER articles?") +
  ggplot2::annotate("text", x = 1, y = 26, size = 8, family = "roboto", hjust = 0, vjust = 1,
                    label = "Source: NBER (nberwp package) | Graphic: Ícaro Bernardes (https://github.com/IcaroBernardes/tidytuesday/tree/main/week40)") +
  ## Labels representing the buzzwords in the x-axis
  geom_text(aes(x = xpos, y = 22, color = as.character(id), label = awe),
            data = buzz, size = 5, family = 'fontawesome-webfont') +
  ## Labels with the overall count of the buzzwords (icons)
  geom_text(aes(x = pos, y = 23, label = awe, color = as.character(id)),
            data = cnt, size = 15, hjust = 1, vjust = 0, family = 'fontawesome-webfont', nudge_x = -1) +
  ## Labels with the overall count of the buzzwords (words and quantity)
  geom_text(aes(x = pos, y = 23, label = counts, color = as.character(id)),
            data = cnt, size = 10, hjust = 0, vjust = 0, lineheight = 0.3, family = "roboto") +
  ## Labels with the overall count of the buzzwords (title)
  ggplot2::annotate("text", x = -2, y = 23, hjust = 1, vjust = 0, family = "roboto",
                    size = 12, lineheight = 0.3, label = "Overall freq.\nof the words") +
  ## X-axis labels and title
  geom_text(aes(x = xpos, y = 0, label = year), size = 8, family = "roboto", data = xtext) +
  ggplot2::annotate("text", x = 246, y = -1, hjust = 1, vjust = 1,
                    size = 12, family = "roboto", label = "Year of publication") +
  ## Y-axis labels and title
  geom_text(aes(x = 0, y = program, label = program_desc),
            data = ytext, size = 8, family = "roboto", color = ytext$color, hjust = 1) +
  geom_text(aes(x = 246, y = program, label = program_desc),
            data = ytext, size = 8, family = "roboto", color = ytext$color, hjust = 0) +
  ggplot2::annotate("text", x = -30, y = 21, hjust = 1, vjust = 0, angle = 90, lineheight = 0.3,
                    size = 12, family = "roboto", label = "Program responsible for the paper") +
  ## Insights
  ### Insight 01
  ggplot2::annotate("text", x = 2, y = -1, hjust = 0, vjust = 1, family = "roboto", size = 6, lineheight = 0.3,
                    label = "The programs are ordered by similarity. The most similar are closer.\nThe colors indicate to which category the programs belong:") +
  ggplot2::annotate("text", x = 2, y = -2.3, hjust = 0, vjust = 1, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE, color = "#b03060",
                    label = "'Finance, '*phantom('Macro/International, ')*phantom('Micro')*phantom(' or non-categorized.')") +
  ggplot2::annotate("text", x = 2, y = -2.3, hjust = 0, vjust = 1, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE, color = "#483d8b",
                    label = "phantom('Finance, ')*'Macro/International, '*phantom('Micro')*phantom(' or non-categorized.')") +
  ggplot2::annotate("text", x = 2, y = -2.3, hjust = 0, vjust = 1, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE, color = "#d2691e",
                    label = "phantom('Finance, ')*phantom('Macro/International, ')*'Micro'*phantom(' or non-categorized.')") +
  ggplot2::annotate("text", x = 2, y = -2.3, hjust = 0, vjust = 1, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE, color = "#000000",
                    label = "phantom('Finance, ')*phantom('Macro/International, ')*phantom('Micro')*' or non-categorized.'") +
  ggplot2::annotate("text", x = 2, y = -3, hjust = 0, vjust = 1, family = "roboto", size = 6, lineheight = 0.3,
                    label = "The clustering of the groups is clear, however the Microeconomics category seems to be divided\nbetween two groups: one focused on individuals (top) and other focused on business (bottom).") +
  ggplot2::annotate("curve", x = 1.8, xend = -2, y = -2.5, yend = 0, arrow = arrow(length = unit(0.01, "npc")), curvature = -0.5, color = "black") +
  ### Insight 02
  ggplot2::annotate("text", x = 90, y = 30, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3,
                    label = 'The most "buzzwordy" article title has nine occurrences of frequent words:') +
  ggplot2::annotate("text", x = 90, y = 29.3, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE,
                    label = "'Short-Run Independence of '*phantom('Monetary ')*phantom('Policy ')*'Under'") +
  ggplot2::annotate("text", x = 90, y = 29.3, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE, color = "#e9967a",
                    label = "phantom('Short-Run Independence of ')*'Monetary '*phantom('Policy ')*phantom('Under')") +
  ggplot2::annotate("text", x = 90, y = 29.3, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE, color = "#228b22",
                    label = "phantom('Short-Run Independence of ')*phantom('Monetary ')*'Policy '*phantom('Under')") +
  ggplot2::annotate("text", x = 90, y = 28.6, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE, color = "#00bfff",
                    label = "phantom('Pegged ')*'Exchange '*phantom('Rates ')*phantom('and ')*phantom('Effects ')*phantom('of Money on')") +
  ggplot2::annotate("text", x = 90, y = 28.6, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE, color = "#000080",
                    label = "phantom('Pegged ')*phantom('Exchange ')*'Rates '*phantom('and ')*phantom('Effects ')*phantom('of Money on')") +
  ggplot2::annotate("text", x = 90, y = 28.6, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE,
                    label = "'Pegged '*phantom('Exchange ')*phantom('Rates ')*'and '*phantom('Effects ')*'of Money on'") +
  ggplot2::annotate("text", x = 90, y = 28.6, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE, color = "#556b2f",
                    label = "phantom('Pegged ')*phantom('Exchange ')*phantom('Rates ')*phantom('and ')*'Effects '*phantom('of Money on')") +
  ggplot2::annotate("text", x = 90, y = 27.9, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE, color = "#00bfff",
                    label = "'Exchange '*phantom('Rates ')*phantom('and ')*phantom('Interest ')*phantom('Rates')") +
  ggplot2::annotate("text", x = 90, y = 27.9, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE, color = "#000080",
                    label = "phantom('Exchange ')*'Rates '*phantom('and ')*phantom('Interest ')*'Rates'") +
  ggplot2::annotate("text", x = 90, y = 27.9, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE,
                    label = "phantom('Exchange ')*phantom('Rates ')*'and '*phantom('Interest ')*phantom('Rates')") +
  ggplot2::annotate("text", x = 90, y = 27.9, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE, color = "#1e90ff",
                    label = "phantom('Exchange ')*phantom('Rates ')*phantom('and ')*'Interest '*phantom('Rates')") +
  ggplot2::annotate("curve", x = 89.5, xend = 84, y = 29.3, yend = 27.7, arrow = arrow(length = unit(0.01, "npc")), curvature = 0.4, color = "black") +
  ### Insight 03
  ggplot2::annotate("text", x = 155, y = 25.5, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3,
                    label = "The graph shows in each tile the frequency of use of the terms by year and program.\nThe data was normalized by lines (programs). Darker tiles indicate which words a program used more frequently.\nThe icons above the tiles indicate which word the column refers to.") +
  ggplot2::annotate("curve", x = 154.5, xend = 151, y = 26.3, yend = 22.3, arrow = arrow(length = unit(0.01, "npc")), curvature = 0.35, color = "black") +
  ### Insight 04
  ggplot2::annotate("text", x = 80, y = -2, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE, color = "#000080",
                    label = "'Rate '*phantom('was a very popular term until 1992.')") +
  ggplot2::annotate("text", x = 80, y = -2, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE,
                    label = "phantom('Rate ')*'was a very popular term until 1992.'") +
  ggplot2::annotate("text", x = 80, y = -2.6, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3,
                    label = "It gave way to a triad of terms that persists") +
  ggplot2::annotate("text", x = 80, y = -3.4, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE,
                    label = "'to this day: '*phantom('evidence, ')*phantom('market ')*'and '*phantom('effect.')") +
  ggplot2::annotate("text", x = 80, y = -3.4, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE, color = "#000000",
                    label = "phantom('to this day: ')*'evidence, '*phantom('market ')*phantom('and ')*phantom('effect.')") +
  ggplot2::annotate("text", x = 80, y = -3.4, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE, color = "#696969",
                    label = "phantom('to this day: ')*phantom('evidence, ')*'market '*phantom('and ')*phantom('effect.')") +
  ggplot2::annotate("text", x = 80, y = -3.4, hjust = 0, vjust = 0, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE, color = "#556b2f",
                    label = "phantom('to this day: ')*phantom('evidence, ')*phantom('market ')*phantom('and ')*'effect.'") +
  ggplot2::annotate("curve", x = 79.5, xend = 75, y = -2.3, yend = -0.5, arrow = arrow(length = unit(0.01, "npc")), curvature = -0.3, color = "black") +
  ### Insight 05
  ggplot2::annotate("text", x = 247, y = 28, hjust = 0, vjust = 1, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE,
                    label = "'In 2020, '*phantom('COVID ')*'took the scientific'") +
  ggplot2::annotate("text", x = 247, y = 28, hjust = 0, vjust = 1, family = "roboto", size = 6, lineheight = 0.3, parse = TRUE, color = "#ff1493",
                    label = "phantom('In 2020, ')*'COVID '*phantom('took the scientific')") +
  ggplot2::annotate("text", x = 247, y = 27.3, hjust = 0, vjust = 1, family = "roboto", size = 6, lineheight = 0.3,
                    label = "world by storm. NBER wasn't immune to\nthis movement as the term was the 2nd\nmost popular this year and was\nthe most used by many programs") +
  ggplot2::annotate("curve", x = 246.5, xend = 243, y = 27, yend = 25, arrow = arrow(length = unit(0.01, "npc")), curvature = 0.3, color = "black") +
  ylim(-4,31) +
  xlim(-30,260) + 
  scale_fill_gradient(low = "white", high = "blue") +
  scale_color_manual(values = optcol, breaks = 1:length(top)) +
  theme_ipsum() +                                     
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

## Saves the plot
ggsave("week40/buzzwords.png", plot = p, width = 60, height = 15, units = "cm")

