# 0. Gestão de pacotes e fontes
library(tidyverse)
library(ggplot2)
library(showtext)
library(glue)
library(ggtext)
library(lubridate)
library(circlize)
library(ComplexHeatmap)
library(colorspace)
library(cols4all)
library(patchwork)
library(cowplot)
library(ggplotify)
library(ggimage)
library(rlang)

## Adicionando fontes do Google Fonts
sysfonts::font_add_google(name = "Outfit", family = "Outfit")
sans <- "Outfit"

# 1. Download, carregamento e manejo dos dados
rawdata <- readr::read_csv('https://raw.githubusercontent.com/dssgPT/Plotting-Good-DSSG/main/desafios/001_Seca_Em_Portugal_SNIRH/snirh_clean.csv')

## Elimina a variável que contém o número de linha,
## elimina linhas sem informação e mantém apenas 
## porcentagens de volume armazenado das bacias inteiras
df <- rawdata %>% 
  dplyr::filter(resumo_infraestrutura != "n/d",
                !is.na(resumo_infraestrutura),
                tipo_de_infraestrutura == "bacia",
                medida == "percentagem") %>% 
  dplyr::select(nome = nome_infraestrutura,
                pct = resumo_infraestrutura,
                data)

## Converte valores de porcentagem de caractere a número
df <- df %>% 
  dplyr::mutate(pct = as.numeric(pct))

## Extrai mês e ano da data
df <- df %>% 
  dplyr::mutate(data = lubridate::ymd(data),
                year = lubridate::year(data),
                month = lubridate::month(data)) %>% 
  dplyr::select(-data)

## Gera um vetor com os nomes das bacias e converte em factor
bacias <- sort(unique(df$nome))
bacias <- factor(bacias)

## Gera um tibble com o caminho e coordenadas para as imagens de mapas
mapas <- tibble(
  nome = bacias
) %>%
  dplyr::mutate(image = str_remove_all(nome, "[:space:]|[:punct:]"),
                image = glue::glue("extras/plottinggood/week01/mapas/{image}.png"),
                x = rep(seq(0.10, 0.9, by = 0.16), 2),
                y = c(rep(0.52,6), rep(0.19,6)))

### Define a escala de cores dos mapas de calor
col_vec1 <- c(cols4all::c4a(palette = "tableau.red_gold", n = 5, contrast = c(0.1,1), reverse = TRUE),
              cols4all::c4a(palette = "hcl.greens3", n = 5, contrast = c(0.1,1)))
col_fun1 <- circlize::colorRamp2(seq(0, 100, length.out = 10), col_vec1)

## Gera os mapas de calor circulares para cada bacia
bacias %>% 
  purrr::walk(
    function(x){
      
      ### Reorganiza os dados de sorte que as colunas são anos,
      ### as linhas são meses e as células guardam as porcentagens
      mat1 <- df %>% 
        dplyr::filter(nome == x) %>% 
        dplyr::arrange(year, month) %>% 
        tidyr::pivot_wider(names_from = year,
                           values_from = pct) %>% 
        dplyr::mutate(month = factor(month, 1:12)) %>% 
        dplyr::arrange(month) %>% 
        dplyr::select(-nome, -month)
      
      ### Converte a tibble em matriz e insere os nomes dos meses
      mat1 <- as.matrix(mat1)
      rownames(mat1) <- c("Janeiro","Fevereiro","Março","Abril","Maio","Junho",
                          "Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")
      
      ### Reinicia e define os parâmetros de plotagem dos gráficos
      circlize::circos.clear()
      par(
        mar = rep(0,4), ### Margem ao redor do gráfico
        bg = NA ### Cor de fundo
      ) 
      circlize::circos.par(
        start.degree = 80, ### Rotaciona o gráfico
        gap.after = 10, ### Distância após cada setor
        circle.margin = 0.1 ### Margens ao redor do gráfico
      )
      
      ### Cria os mapas de calor
      circlize::circos.heatmap(
        mat1,
        col = col_fun1,
        track.height = 0.5,
        cell.border	= "gray50",
        cell.lwd = 0.5,
        ignore.white = FALSE,
        cluster = FALSE
      )
      
      ### Insere os anos no mapas de calor
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
              cex = 0.25,
              family = sans,
              adj = c(0, 0.5),
              facing = "inside",
              niceFacing = TRUE
            )
          }
        }
      )
      
      ### Insere os nomes dos meses no mapas de calor
      circlize::circos.trackPlotRegion(
        track.index = 1, 
        bg.border = NA, 
        panel.fun = function(x, y) {
          ro = CELL_META$row_order
          rn = rownames(mat1)[ro]
          n = length(rn)
          circos.text(
            x = c(1:12) - 0.5, 
            y = rep(26, n),
            labels = rn, 
            cex = 1,
            family = sans,
            facing = "outside",
            niceFacing = TRUE,
            adj = c(0.5,1)
          )
        }
      )
      
      ### Converte os fatores em valores numéricos
      i <- as.numeric(x)
      
      ### Obtém os gráficos do device e os salva em formato ggplot
      hmap <- recordPlot()
      hmap <- ggplotify::as.ggplot(cowplot::ggdraw(hmap))
      assign(glue::glue("hmap{i}"), hmap, envir = globalenv())
      
    }
  )



# 2. Generates the plot


## Makes a legend guide and converts it to a ggplot-friendly format
lgd <- ComplexHeatmap::Legend(title = "Disponibilidade hídrica da bacia",
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



## Makes the main plot
p <- ggplot(NULL) + 
  
  
  ggimage::geom_image(aes(x = x, y = y, image = image), size = 0.025,
                      by = 'width', asp = 2, data = mapas) +
  
  
  ### Defines unitary plots limits
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
  
  ### Eliminates and customizes elements on the plot
  theme_void() 


tibble(
  hmap = glue::glue("hmap{1:12}"),
  left = rep(seq(0.03, 0.83, by = 0.16), 2),
  right = rep(seq(0.17, 0.97, by = 0.16), 2),
  bottom = c(rep(0.38, 6), rep(0.05, 6)),
  top = c(rep(0.66, 6), rep(0.33, 6))
) %>% 
  tibble::add_row(.before = 1L) %>%
  t() %>%
  as.data.frame() %>% 
  dplyr::summarise(across(.fns = ~paste0(.,collapse = " "))) %>% 
  purrr::reduce(
    function(cumulative, add) {
      
      args <- str_split(add, " ", simplify = TRUE) 
      pname <- args[1] %>% 
        rlang::parse_expr()
      
      print(args[1])
      p <- p + patchwork::inset_element(rlang::eval_bare(pname),
                                        left = args[2],
                                        right = args[3],
                                        bottom = args[4],
                                        top = args[5])
      assign("p", p, envir = globalenv())
      
    }
  )


## Saves the plot
ggsave("extras/plottinggood/week01/bacias.png", plot = p, dpi = "retina",
       width = 30, height = 15)

