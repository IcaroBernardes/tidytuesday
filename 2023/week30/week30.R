# 0. Initial setup ##########
## Loads libraries
library(dplyr)
library(ggpath)
library(ggplot2)
library(ggtext)
library(ggview)
library(glue)
library(gt)
library(htmltools)
library(junebug)
library(readr)
library(stringr)
library(webshot2)

## Gets the data
rawData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-25/scurvy.csv')

## Takes all font styles that share that exact family name and
## registers them (makes them visible to {systemfonts})
junebug::font_hoist("Font Awesome 6 Brands")
junebug::font_hoist("Font Awesome 6 Free")

## Gets the info used to register the font families
fonts_register <- systemfonts::registry_fonts()

## Defines the font families
brandsFont <- "Font Awesome 6 Brands Regular"
solidFont <- "Font Awesome 6 Free Solid"

## Creates a function that simplifies the process of placing 
## the Font Awesome glyphs on the text
faDecoder <- function(code, handle) {
  tagList(
    span(code, style = glue::glue("font-family:\"{brandsFont}\";")),
    handle
  )
}

# 1. Data handling ##########
## Keeps only the treatments and symptoms
data <- rawData |> 
  dplyr::select(treatment, dosing_regimen_for_scurvy, gum_rot_d6:lassitude_d6)

## Converts the labels to numeric
data <- data |> 
  dplyr::mutate(across(
    .cols = ends_with("d6"),
    .fns = ~as.numeric(stringr::str_extract(., "[:digit:]"))
  ))

## Reorders the observations
treatOrder <- c("citrus", "cider", "dilute_sulfuric_acid",
                "vinegar", "purgative_mixture", "sea_water")
data <- data |> 
  dplyr::mutate(treatment = factor(treatment, levels = treatOrder)) |> 
  dplyr::arrange(treatment)

## Renames the treatments and creates a label for them
## that includes dosing (with HTML)
data <- data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    treatment = stringr::str_replace_all(treatment, "_", " "),
    treatment = stringi::stri_trans_totitle(treatment),
    treatment = list(tagList(
      strong(treatment, style = "font-size:22px;display:block;margin-bottom:5px;"),
      div(dosing_regimen_for_scurvy, style = "font-size:12px;color:gray;display:block;margin-right:10px;")
    )),
    treatment = as.character(treatment)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::select(-dosing_regimen_for_scurvy)

## Eliminates the duplicate second instance of each treatment
data <- data |> 
  dplyr::group_by(treatment) |> 
  dplyr::mutate(treatment = c(treatment[1], "")) |> 
  dplyr::ungroup()

## Defines the color palette for the symptoms
symPal <- c("#a6a91c", "#b2712f", "#b7315f")

## Adds Font Awesome icons to the symptoms columns
data <- data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(across(
    .cols = ends_with("d6"),
    .fns = ~as.character(
      div(
        span(glue::glue_collapse(rep("\uf165", .), sep = " "),
             style = paste0("color:",symPal[.],";")),
        span(glue::glue_collapse(rep("\uf165", 3-.), sep = " "),
             style = "color:#b0b0b0;"),
        style = glue::glue("font-family:\"{solidFont}\";font-size:25px;text-align:center;")
      )
    )
  )) |> 
  dplyr::ungroup()

# 2. Table creation ##########
## Starts the table
gtTable <- data |> 
  gt::gt(id = "one")

## Defines the widths of the columns
gtTable <- gtTable |> 
  gt::cols_width(
    treatment ~ gt::pct(31),
    ends_with("d6") ~ gt::pct(23)
  )

## Allows HTML use in all columns
gtTable <- gtTable |> 
  gt::fmt_markdown(columns = everything())

## Defines labels for the columns
treatLabel <- as.character(tagList(
  strong("TREATMENT", style = "display:block;margin-bottom:5px;"),
  div("DOSING", style = "font-size:15px;color:gray;")
))
gtTable <- gtTable |> 
  gt::cols_label(
    treatment = HTML(treatLabel),
    gum_rot_d6 = "GUM ROT",
    skin_sores_d6 = "SKIN SORES",
    weakness_of_the_knees_d6 = "WEAK KNEES",
    lassitude_d6 = "LASSITUDE"
  )

## Creates a spanner for the symptoms columns
spannerHTML <- tagList(
  strong("SYMPTOMS GRAVITY", style = "font-size:50px;"),
  div("AFTER THE 6-DAY TREATMENT", style = "font-size:20px;color:gray;")
)
gtTable <- gtTable |> 
  gt::tab_spanner(
    label = HTML(as.character(spannerHTML)),
    columns = ends_with("d6")
  )

## Defines the font family of the table
gtTable <- gtTable |> 
  opt_css(
    css =
      "#one table {
        font-family:'Merriweather Sans';
      }
      #one .gt_col_headings {
        font-size:25px;
      }
      #one .gt_col_headings th {
        text-align:center;
      }
      #one .gt_col_headings.gt_spanner_row th.gt_col_heading {
        text-align:left;
      }
      #one .gt_table_body tr:nth-child(odd) > * {
        border-top: solid 3px #D3D3D3;
      }")

## Saves the table as image
gt::gtsave(gtTable, "2023/week30/gtTable.png")

# 3. Plot creation ##########
## Defines the title
plotTitle <- "Scurrying salves for scurvy"

## Defines the subtitle
plotSubtitle <- "Scurvy was a common concern among<br>
seamen abroad with no clear cause or cure.<br>
This issue led Ship Surgeon James Lind in 1757 to<br>
promote one of the first controlled clinical trials ever."

## Defines the details
plotDetails <- tagList(
  "James Lind enlisted 12 seamen with equally symptomatic scurvy in his study.", br(),
  "Each pair underwent a treatment that included ship rations and a supposedly", br(),
  "curative substance. After 6 days of therapy, the progression of their symptoms", br(),
  "was evaluated. The table below shows the results of this experiment.", br(),
  "Each symptom is described in terms of gravity:",
  strong(" severe, ", style = "color:#b7315f;"),
  strong(" moderate, ", style = "color:#b2712f;"),
  strong(" mild, ", style = "color:#a6a91c;"),
  strong(" or absent.", style = "color:#b0b0b0;")
)
plotDetails <- as.character(plotDetails) |> 
  stringr::str_remove_all("\n")

## Defines the plot authorship
plotAuthorship <- tagList(
  'Data from: medicaldata R package', br(),
  'Made by Ícaro Bernardes: ', br(),
  faDecoder('\uf099', ' - @IcaroBSC | '),
  faDecoder('\uf09b', ' - @IcaroBernardes | '),
  faDecoder('\uf08c', ' - @icarobsc')
)
plotAuthorship <- as.character(plotAuthorship) |> 
  stringr::str_remove_all("\n")

## Creates the plot
plot <- NULL |> 
  ggplot() +
  
  ### Places the title
  annotate(
    "RichText", x = 0, y = 1, label = plotTitle,
    hjust = 0, vjust = 1, size = 27,
    fill = NA, label.colour = NA,
    lineheight = 0.7, family = "Seaweed Script"
  ) +
  
  ### Places the subtitle
  annotate(
    "RichText", x = 0, y = 0.9, label = plotSubtitle,
    hjust = 0, vjust = 1, size = 10,
    fill = NA, label.colour = NA,
    lineheight = 1, family = "Work Sans"
  ) +
  
  ### Places the details
  annotate(
    "RichText", x = 0, y = 0.78, label = plotDetails,
    hjust = 0, vjust = 1, size = 6.5,
    fill = NA, label.colour = NA,
    lineheight = 1.3, family = "Merriweather Sans"
  ) +
  
  ### Places the table
  ggpath::geom_from_path(
    aes(x = 0.5, y = 0.08, path = "2023/week30/gtTable.png"),
    width = 1, height = 0.52, vjust = 0
  ) +
  
  ### Places the authorship
  annotate(
    "RichText", x = 0, y = 0, label = plotAuthorship,
    hjust = 0, vjust = 0, size = 6,
    fill = NA, label.colour = NA,
    lineheight = 1.3, family = "Merriweather Sans"
  ) +
  
  ### Defines aesthetic rules
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) +
  
  ### Defines aesthetics elements
  theme_void()

## Generates an accurate preview of the final plot
ggview::ggview(
  plot = plot, device = "png",
  width = 4000, height = 6000, units = "px",
  dpi = 320, bg = "white"
)

## Generates the final plot
ggsave("2023/week30/week30.png", plot = plot,
       width = 4000, height = 6000, units = "px",
       dpi = 320, bg = "white")
