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
library(scales)
library(geomtextpath)

## Adiciona fontes do Google Fonts
sysfonts::font_add_google(name = "Outfit", family = "Outfit")
sans <- "Outfit"

## Define algumas constantes
width <- 20
height <- 30
lnght <- 0.9
clr_fundo <- "#5C2D24"
clr_ondas <- c("#15638c","#062666")

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

## Extrai mês e ano da data e mantém apenas dados 
## entre od anos de 2000 e 2021
df <- df %>% 
  dplyr::mutate(data = lubridate::ymd(data),
                year = lubridate::year(data),
                month = lubridate::month(data)) %>% 
  dplyr::select(-data) %>% 
  dplyr::filter(between(year, 2000, 2021))

## Obtém a média mensal das pct. de cada bacia e o meses que
## aparecem mais frequentemente entres as maiores e menores médias
mensal <- df %>% 
  dplyr::group_by(nome, month) %>% 
  dplyr::summarise(pct = mean(pct, na.rm = TRUE))
bottom_mensal <- mensal %>% 
  dplyr::arrange(nome, pct) %>% 
  dplyr::group_by(nome) %>% 
  dplyr::slice(1:3) %>% 
  dplyr::ungroup() %>% 
  dplyr::count(month)
top_mensal <- mensal %>% 
  dplyr::arrange(nome, desc(pct)) %>% 
  dplyr::group_by(nome) %>% 
  dplyr::slice(1:3) %>% 
  dplyr::ungroup() %>% 
  dplyr::count(month)

## Gera um vetor com os nomes das bacias e converte em factor
bacias <- unique(df$nome)
bacias <- factor(bacias,
                 levels = c("LIMA","CÁVADO/RIBEIRAS COSTEIRAS","AVE",
                            "RIBEIRAS DO OESTE","SADO","RIBEIRAS DO ALGARVE",
                            "DOURO","MONDEGO","TEJO",
                            "MIRA","GUADIANA","ARADE"))
bacias <- sort(bacias)

## Gera um tibble com o caminho e coordenadas para as imagens de mapas
mapas <- tibble(
  nome = bacias
) %>%
  dplyr::arrange(desc(nome)) %>% 
  dplyr::mutate(image = str_remove_all(nome, "[:space:]|[:punct:]"),
                image = glue::glue("extras/plottinggood/week01/mapas/{image}.png"),
                y = rep(seq(0.10, 0.9, by = 0.16), 2),
                x = c(rep(0.41,6), rep(0.15,6)))

## Gera as coordenadas para os nomes das bacias 
nomes_curvos <- tibble(
  nome = mapas$nome,
  size = scales::rescale(stringr::str_length(nome), to = c(10,5)),
  a = 0.115,
  b = 0.075,
  h = mapas$x,
  k = mapas$y,
  x1 = h + a
) %>% 
  dplyr::group_by(nome, size) %>% 
  tidyr::nest() %>%
  dplyr::mutate(
    data = purrr::map(
      data,
      function(vetor) {
        a = vetor$a
        b = vetor$b
        h = vetor$h
        k = vetor$k
        x1 = vetor$x1
        
        tibble(
          x = seq(h, x1, length.out = 300),
          y = sqrt(b^2 * (1 - (((x-h)^2)/(a^2)))) + k
        )
      }
    )
  ) %>% 
  tidyr::unnest(cols = data) %>% 
  dplyr::filter(if_all(.fns = ~!is.nan(.)))

### Define a escala de cores dos mapas de calor
col_vec <- c(cols4all::c4a(palette = "tableau.red_gold", n = 5, contrast = c(0.1,1), reverse = TRUE),
             "white",
             cols4all::c4a(palette = "hcl.greens3", n = 5, contrast = c(0.1,1)))
col_fun <- circlize::colorRamp2(seq(0, 100, 10), col_vec)

## Cria uma legenda para a escala de cores e converte a um formato amigável ao ggplot
lgd <- ComplexHeatmap::Legend(at = seq(0, 100, 10),
                              title = "Disponibilidade hídrica da bacia",
                              labels = glue::glue("{seq(0,100,10)}%"),
                              title_gp = gpar(fontfamily = sans, fontsize = 20, col = "white"),
                              labels_gp = gpar(fontfamily = sans, fontsize = 16, col = "white"),
                              legend_gp = gpar(col = "white"),
                              col_fun = col_fun,
                              direction = "horizontal",
                              title_position = "topcenter",
                              legend_width = unit(180, "mm"),
                              title_gap = unit(5, "mm"))
lgd <- ggplotify::as.ggplot(cowplot::ggdraw(lgd@grob))

## Gera os mapas de calor circulares para cada bacia
bacias %>% 
  purrr::walk(
    function(x){
      
      ### Reorganiza os dados de sorte que as colunas são anos,
      ### as linhas são meses e as células guardam as porcentagens
      mat <- df %>% 
        dplyr::filter(nome == x) %>% 
        dplyr::arrange(desc(year), month) %>% 
        tidyr::pivot_wider(names_from = year,
                           values_from = pct) %>% 
        dplyr::mutate(month = factor(month, 1:12)) %>% 
        dplyr::arrange(month) %>% 
        dplyr::select(-nome, -month)
      
      ### Converte a tibble em matriz e insere os nomes dos meses
      mat <- as.matrix(mat)
      rownames(mat) <- c("Janeiro","Fevereiro","Março","Abril","Maio","Junho",
                         "Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")
      
      ### Reinicia e define os parâmetros de plotagem dos gráficos
      circlize::circos.clear()
      par(
        mar = rep(0,4), ### Margem ao redor do gráfico
        bg = NA ### Cor de fundo
      ) 
      circlize::circos.par(
        start.degree = 80, ### Rotaciona o gráfico
        gap.after = 12, ### Distância após cada setor
        circle.margin = 0.1 ### Margens ao redor do gráfico
      )
      
      ### Cria os mapas de calor
      circlize::circos.heatmap(
        mat,
        col = col_fun,
        track.height = 0.6,
        cell.border	= "gray50",
        cell.lwd = 0.3,
        ignore.white = FALSE,
        cluster = FALSE
      )
      
      ### Insere os anos no mapas de calor
      circlize::circos.track(
        track.index = get.current.track.index(),
        bg.border = NA, 
        panel.fun = function(x, y) {
          if(CELL_META$sector.numeric.index == 1) { 
            cn = colnames(mat)
            n = length(cn)
            circos.text(
              x = rep(CELL_META$cell.xlim[2], n) + convert_x(0.5, "mm"), 
              y = n:1 - 0.5,
              labels = cn,
              col = "white",
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
          rn = rownames(mat)[ro]
          n = length(rn)
          circos.text(
            x = c(1:12) - 0.5, 
            y = rep(24, n),
            labels = rn,
            col = "white",
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

## Obtém o volume máximo (100%) de armazenamento
## de cada bacia na data mais recente
max_volume <- rawdata %>% 
  dplyr::filter(resumo_infraestrutura != "n/d",
                !is.na(resumo_infraestrutura),
                tipo_de_infraestrutura == "bacia") %>% 
  dplyr::select(-...1, -tipo_de_infraestrutura) %>%
  dplyr::rename(nome = nome_infraestrutura) %>% 
  tidyr::pivot_wider(names_from = medida,
                     values_from = resumo_infraestrutura) %>% 
  dplyr::group_by(nome) %>% 
  dplyr::arrange(nome, desc(data)) %>% 
  dplyr::slice(1L) %>% 
  dplyr::mutate(across(.cols = c(metro_cubico, percentagem),
                       .fns = as.numeric)) %>% 
  dplyr::transmute(capacidade = round(100*metro_cubico/percentagem, 2)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(capacidade))

## Obtém cores a serem usadas para colorir o mapa do país
## de acordo com o log da capacidade total de cada bacia
col_vec_mapa <- cols4all::c4a(palette = "kovesi.linear_green_5_95_c69",
                              n = 9,
                              contrast = c(0.1,0.9))
col_fun_mapa <- circlize::colorRamp2(seq(0, 1, length.out = 9),
                                     col_vec_mapa)
max_volume <- max_volume %>% 
  dplyr::mutate(scl_cap = scales::rescale(log10(capacidade))) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(hex = col_fun_mapa(scl_cap),
                rgb = paste0(colorspace::hex2RGB(hex)@coords, collapse = " ")) %>% 
  dplyr::ungroup() %>% 
  tidyr::separate(col = rgb,
                  into = c("R","G","B"),
                  sep = " ",
                  convert = TRUE) %>% 
  dplyr::mutate(across(.cols = c("R","G","B"),
                       .fns = ~.*255))

## Cria uma legenda para a escala de cores e converte a um formato amigável ao ggplot
lbls <- c(10,100,1000)
at <- scales::rescale(log10(lbls), from = range(log10(max_volume$capacidade)))
lgd_mapa <- ComplexHeatmap::Legend(at = at,
                                   title = "Volume total\nda bacia\n(milhões de m³)",
                                   labels = lbls,
                                   title_gp = gpar(fontfamily = sans, fontsize = 20, col = "white"),
                                   labels_gp = gpar(fontfamily = sans, fontsize = 16, col = "white"),
                                   legend_gp = gpar(col = "white"),
                                   col_fun = col_fun_mapa,
                                   direction = "vertical",
                                   title_position = "leftcenter-rot",
                                   legend_height = unit(90, "mm"),
                                   title_gap = unit(5, "mm"))
lgd_mapa <- ggplotify::as.ggplot(cowplot::ggdraw(lgd_mapa@grob))

## Permite o uso da fonte do Google Fonts.
## Causa erro se chamado antes dos gráficos do circlize
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

## Gera coordenadas do plano de fundo da direita
fundo <- tibble(
  y = seq(-0.1, 1.1, 0.001),
  x = 0.545-(sin(60*y)/70)
)

## Gera coordenadas dos títulos
titulos <- tibble(
  x = 0.96,
  y = c(0.96,0.86,0.22,0.145,0.05),
  size = c(35,rep(6.7,4)),
  fontface = c("bold", rep("plain",3), "bold"),
  label = c(
    "ÁGUAS DE\nPORTUGAL",
    
    "Os mapas de calor circulares (à esq.) exibem a porcentagem de
    volume armazenado em relação à capacidade máxima do
    conjunto de albufeiras das bacias de Portugal.
    Dados têm frequência mensal e são agrupados anualmente
    (de 2000 no círculo mais interno até 2021 no mais externo).
    As porcentagens são codificadas pela guia abaixo.",
    
    "As bacias de maior capacidade total em volume são as dos rios
    Guadiana e Tejo. Elas têm se mantido acima dos 40%. O mapa
    acima exibe a capacidade em milhões de metro cúbicos. A guia
    também acima apresenta tais volumes numa escala logarítmica.",
    
    "Março, Abril (de águas mil) e Maio são os meses nos
    quais as bacias estão usualmente mais cheias.
    Já Outubro e Novembro são meses de maior baixa.
    As baixas têm sido especialmente severas no sul do país.
    Mira e Ribeiras do Algarve têm operado constantemente
    em 50% ou menos do volume total nos últimos anos.",
    
    "Dados do SNIRH | Gráfico por Ícaro Bernardes (@IcaroBSC)"
    
  )
)

## Define as coordenadas das cidades em destaque
cidades <- tibble(
  x = c(0.620,0.684,0.698,0.693),
  y = c(0.425,0.638,0.675,0.554),
  label = c("Lisboa","Porto","Braga","Coimbra")
)

## Define as coordenadas das linhas que indicam as cidades destacadas
linhas <- tibble(
  x = c(0.610,0.56,
        0.630,
        0.690, 0.620,
        0.690, 0.590),
  y = c(0.415,0.415,
        0.638,
        0.670, 0.670,
        0.550,0.550),
  label = c("Lisboa","Lisboa",
            "Porto",
            "Braga","Braga",
            "Coimbra","Coimbra")
)
linhas <- rbind(cidades, linhas)

## Define as coordenadas dos nomes das cidades destacadas
nomes <- linhas %>% 
  dplyr::group_by(label) %>% 
  dplyr::slice(n())

## Define as coordenadas dos gráficos circulares a inserir no gráfico principal
insets <- tibble(
  hmap = glue::glue("hmap{1:12}"),
  left = c(rep(0.03, 6), rep(0.29, 6)),
  right = c(rep(0.27, 6), rep(0.53, 6)),
  bottom = rep(seq(0.83, 0.03, by = -0.16), 2),
  top = rep(seq(0.97, 0.17, by = -0.16), 2)
) %>% 
  tibble::add_row(.before = 1L) %>%
  t() %>%
  as.data.frame() %>% 
  dplyr::summarise(across(.fns = ~paste0(.,collapse = " "))) 

## Gera o gráfico principal
p <- ggplot(NULL) + 
  
  ### Insere o plano de fundo da direita
  geom_ribbon(aes(xmin = 1, xmax = x-0.008, y = y-0.006),
              fill = clr_ondas[1], color = NA, data = fundo) +
  geom_ribbon(aes(xmin = 1, xmax = x, y = y),
              fill = clr_ondas[2], color = NA, data = fundo) +
  
  ### Insere as imagens do mapa de Portugal com destaque a certas bacias
  ggimage::geom_image(aes(x = x, y = y, image = image), size = 0.033,
                      by = 'width', asp = width/height, data = mapas) +
  
  ### Insere a imagem maior do mapa de Portugal com todas
  ### as bacias coloridas por capacidade total em volume
  ggimage::geom_image(aes(x = 0.76, y = 0.50,
                          image = "extras/plottinggood/week01/mapas/PORTUGAL.png"), size = 0.35,
                      by = 'width', asp = width/height) +
  
  ### Insere linhas e nomes para destacar algumas cidades
  geom_path(aes(x = x, y = y, group = label), color = "white", data = linhas) +
  geom_text(aes(x = x, y = y, label = label), hjust = 0, vjust = 0, size = 7,
            family = sans, color = "white",  nudge_y = 0.002, data = nomes) +
  
  ### Insere pontos nos locais das cidades de destaque
  geom_point(aes(x = x, y = y), size = 3, color = "red", data = cidades) +
  
  ### Insere os nomes das bacias ao longo de uma curva
  geomtextpath::geom_textpath(aes(x = x, y = y, label = nome, size = I(size)),
                              family = sans, text_only = TRUE, hjust = 0,
                              color = "white", data = nomes_curvos) +
  
  ### Insere os títulos
  geom_text(aes(x = x, y = y, label = label, size = I(size), fontface = fontface),
            family = sans, hjust = 1, vjust = 1, lineheight = lnght,
            color = "white", data = titulos) +
  
  ### Define limites unitários ao gráfico
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
  
  ### Elimina e customiza elementos do gráfico
  theme_void() +
  theme(
    plot.background = element_rect(color = NA, fill = clr_fundo)
  ) +
  
  ### Insere a legenda dos gráficos circulares
  patchwork::inset_element(lgd, left = 0.585, right = 0.96,
                           bottom = 0.745, top = 0.795) +
  
  ### Insere a legenda do mapa de capacidade total
  patchwork::inset_element(lgd_mapa, left = 0.865, right = 0.96,
                           bottom = 0.27, top = 0.4)

## Insere os gráficos circulares no gráfico principal
insets %>% 
  purrr::reduce(
    function(cumulative, add) {
      
      args <- str_split(add, " ", simplify = TRUE) 
      pname <- args[1] %>% 
        rlang::parse_expr()
      
      p <- p + patchwork::inset_element(rlang::eval_bare(pname),
                                        left = args[2],
                                        right = args[3],
                                        bottom = args[4],
                                        top = args[5])
      assign("p", p, envir = globalenv())
      
    }
  )

## Salva o gráfico
ggsave("extras/plottinggood/week01/bacias.png", plot = p, dpi = "retina",
       width = width, height = height)

