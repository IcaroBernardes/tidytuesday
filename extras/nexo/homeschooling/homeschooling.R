# 0. Gestão das fontes e pacotes
library(tidyverse)
library(readxl)
library(ragg)
library(ggparliament)
library(geomtextpath)
library(ggtext)

## Definição de constantes de layout
lineheight <- 1.2

## Define fontes
font_sans <- "Ubuntu"
font_serif <- "Bitter"

# 1. Carregamento e manejo dos dados
dadosbrutos <- read_excel("extras/nexo/homeschooling/dados.xlsx")

## Torna os valores vazios em zeros e
## calcula quantos pct. de cada partido votou "SIM"
dados <- dadosbrutos %>% 
  dplyr::mutate(across(.fns = ~ifelse(is.na(.), 0, .))) %>% 
  dplyr::mutate(sim_ratio = Sim/n_dept)

## Verifica os partidos que compõem a maioria do congresso (75% da casa)
## e pega os partidos menores e divide em dois grupos (minorias)
dados <- dados %>% 
  dplyr::arrange(desc(n_dept)) %>% 
  dplyr::mutate(pct = round(100*cumsum(n_dept)/sum(n_dept)),
                main = (lag(pct, default = 0) <= 75)) %>% 
  dplyr::mutate(grupo = case_when(main ~ Partido,
                                  sim_ratio >= 0.5 ~ "Minoria a favor",
                                  sim_ratio < 0.5 ~ "Minoria contra",
                                  TRUE ~ ""))

## Agrupa os dados por grupo/partido e recalcula a pct. de "SIM"
dados <- dados %>% 
  dplyr::group_by(grupo) %>% 
  dplyr::summarise(
    Partido = paste0(Partido, collapse = "\n"),
    across(.cols = c("n_dept","Sim","Não","Abs/Art.17","Ausente"), .fns = sum)
  ) %>%
  dplyr::mutate(sim_ratio = Sim/n_dept) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(sim_ratio)) %>% 
  dplyr::select(-sim_ratio, -n_dept) %>% 
  dplyr::mutate(order = 1:n())

## Guarda os nomes dos partidos em minoria
min_favor <- dados %>%
  dplyr::filter(grupo == "Minoria a favor") %>%
  dplyr::pull(Partido)
min_contra <- dados %>%
  dplyr::filter(grupo == "Minoria contra") %>%
  dplyr::pull(Partido)

## Adiciona cores para os grupos/partidos
clr_partidos <- tibble(
  colour = c("#CCBC3F","#0E3FED","#606FDB","#EB4700","#2B8F3D",
             "#F50201","#B0AC58","#1A708F","#4EBF58","#BA5E54"),
  grupo = c("MDB","PL","PP","PSB","PSD",
            "PT","Republicanos","União Brasil","Minoria a favor","Minoria contra")
)
dados <- dados %>% dplyr::left_join(clr_partidos)

## Reorganiza os dados (agrupa as posições tomadas
## pelos deputados em uma única coluna)
dados <- dados %>% 
  tidyr::pivot_longer(
    cols = -c(grupo, Partido, colour, order),
    names_to = "Posição",
    values_to = "Qtd"
  )

## Converte os dados da votação em formato adequado para o ggparliament
parlamento <- ggparliament::parliament_data(election_data = dados, 
                                            parl_rows = 10,
                                            party_seats = dados$Qtd,
                                            type = "semicircle",
                                            plot_order = dados$order)

## Define coordenadas para os nomes dos grupos/partidos em curva
nomes_partidos <- parlamento %>% 
  dplyr::filter(row == 10) %>% 
  dplyr::arrange(desc(theta)) %>% 
  dplyr::select(x, y, grupo, colour)

## Define as coordenadas para os títulos
titulos <- tibble(
  x = -3,
  y = c(4.2,3.6,-0.2),
  size = c(16,8,5),
  colour = c("black","gray50","black"),
  label = c(
    '**Como votaram os partidos no projeto de "Homeschooling"?**',
    
    'Os círculos abaixo representam a participação dos deputados na votação do projeto.<br>
    Da esquerda para a direita os partidos são ordenados conforme a fração de votos **SIM** ao projeto.<br>
    Tais deputados são destacados em círculos de tamanho maior e borda preta.<br>
    Os outros deputados votaram **NÃO** ou estavam **ausentes** ou se **abstiveram**.',
    
    'Dados compilados por: Gabriel Zanlorenssi | Gráfico por: Ícaro Bernardes (@IcaroBSC)'
  )
)

## Define as coordenadas para a mensagem
mensagem <- tibble(
  x = 2.4,
  y = 1,
  colour = "black",
  label = c(
    '<span style="font-size:70px;">**498**</span><span style="font-size:30px;"> deputados pertencem à câmara federal</span><br>
    <span style="font-size:70px;color:#06D157;">**264**</span><span style="font-size:30px;"> deles votaram <span style="color:#06D157;">SIM</span> ao projeto</span>'
  )
)

# 2. Gera o gráfico
## Cria o gráfico
p <- parlamento %>% 
  ggplot(aes(x, y, colour = I(colour))) +
  
  ### Insere os pontos que representam os deputados
  geom_parliament_seats() +
  
  ### Destaca aqueles que responderam "SIM"
  geom_highlight_government(Posição == "Sim", size = 3.5) +
  
  ### Insere os nomes dos grupos/partidos em curva
  geomtextpath::geom_textpath(aes(label = grupo), size = 5, vjust = -1,
                              fontface = "bold", text_only = TRUE,
                              family = font_serif,
                              data = nomes_partidos) +
  
  ### Insere os títulos
  ggtext::geom_richtext(aes(label = label, size = I(size)),
                        fill = NA, label.colour = NA,
                        hjust = 0, vjust = 1, family = font_sans,
                        lineheight = lineheight, data = titulos) +
  
  ### Insere a mensagem
  ggtext::geom_richtext(aes(label = label),
                        fill = NA, label.colour = NA,
                        hjust = 0, family = font_serif,
                        lineheight = lineheight, data = mensagem) +
  
  ### Insere a lista de partidos em minoria
  annotate("path", x = c(-1.7,-1.7,-2.3), y = c(1.4,1.8,1.8),
           size = 1.5, color = "#BA5E54", lineend = "round") +
  annotate("text", x = -2.35, y = 1.8, label = min_contra, fontface = "bold",
           color = "#BA5E54", hjust = 1, family = font_serif) +
  annotate("path", x = c(1.5,1.5,2), y = c(1.6,2,2),
           size = 1.5, color = "#4EBF58", lineend = "round") +
  annotate("text", x = 2.05, y = 2, label = min_favor, fontface = "bold",
           color = "#4EBF58", hjust = 0, family = font_serif) +
  
  ### Define limites para os eixos e os torna proporcionais
  coord_equal(xlim = c(-3, 6), ylim = c(-0.3,4.2)) +
  
  ### Aplica o tema e o customiza
  ggparliament::theme_ggparliament() +
  theme(
    plot.background = element_rect(fill = "#eeeeee", color = NA)
  )

## Salva o gráfico
ggsave("extras/nexo/homeschooling/homeschooling.png", plot = p, dpi = "retina",
       width = 20, height = 10, device = ragg::agg_png, res = 320)


