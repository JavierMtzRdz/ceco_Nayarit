# En este script genero una gráfica con el Índice de complejidad potencial y el Índice de complejidad económica

library(pacman)
p_load(tidyverse, haven, ggrepel, ggthemes, readxl, stringr, scales)

tema_propio <- theme_few() +
  theme(axis.line = element_line(size = 0.3),
        plot.title = element_text(hjust=0.5, 
                                  size = 14),
        plot.subtitle = element_text(hjust=0.5,
                                     size = 12,
                                     color = "gray50"),
        plot.caption =  element_text(color = "gray50",
                                     size=10, 
                                     hjust=0),
        strip.background = element_rect(fill="gray99", 
                                        linetype="blank"),
        panel.border = element_rect(color = "gray98",
                                    fill=NA),
        rect = element_rect(fill = "transparent")) 

#Atlas de Complejidad Económica----
base_atlas <- read_dta("Complejidad económica/base vizuet Atlas económico.dta")
code_edo <- read_dta("Complejidad económica/code_edo.dta")
base_atlas <- base_atlas %>% 
  left_join(code_edo, by = "location_code")

scatter <- base_atlas %>% 
  group_by(nom_abr, location_name) %>% 
  summarise(coi = mean(coi),
            eci = mean(eci)) %>% 
  ungroup() %>% 
  mutate(coi_n = scale(coi)) 
#Tasa de crecimiento promedio anual ----

#Principales clústers de nayarit según población ocupada
p_cluster_Nayarit <- read_excel("Complejidad económica/Principales cluster Nayarit.xlsx")

(p_cluster <- p_cluster_Nayarit %>% 
  mutate(cluster = str_wrap(cluster,
                            width = 26)) %>% 
  ggplot(aes(x = reorder(cluster, est), y = est, fill = factor(year))) +
  geom_col(position = "dodge") +
  coord_flip(ylim=c(0,21000)) +
  labs(title = NULL,
       x = "",
       y = "Personal ocupado",
       fill = "",
       caption = NULL
       ) +
    scale_y_continuous(label=comma) +
  scale_fill_manual(values = c("#aba09b", "#ba3d05"),
                    breaks = c("2009", "2014")) +
  tema_propio + theme(axis.text.x = element_text(face = "bold", size = 20),
                      axis.text.y = element_text(face = "bold", size = 15),
                      legend.text = element_text(face = "bold", size = 20),
                      axis.title.x = element_text(face = "bold", size = 20),
                      legend.position="top")
  )
ggsave("Gráficas/p_cluster.png",
       plot = p_cluster,
       bg = "transparent",
       width = 270,                  # Ancho de la gráfica
       height =170,
       units = "mm")
ggsave("Gráficas/p_cluster.jpeg",
       plot = p_cluster,
       width = 270,                  # Ancho de la gráfica
       height =170,
       units = "mm")


#Ranking de complejidad económica ----
(g0 <- scatter %>% 
  mutate(cols = ifelse(nom_abr == "Nay.", 1, 0),
         mas = ifelse(eci >= 0, eci, NA),
         menos = ifelse(eci < 0, eci, NA)) %>% 
  ggplot(aes(y = eci, x = reorder(location_name, eci) ,  fill = factor(cols))) +
  geom_col() +
    geom_text(aes(label = round(mas, 2)), hjust = -0.2) +
    geom_text(aes(label = round(menos, 2)),hjust = 1.2) +
    labs(title = "Ranking de complejidad económica por \nentidad federativa, 2014",
         x = "",
         y = "Índice de Complejidad Económica",
         caption = "\nAtlas de Complejidad Económica")  +
    coord_flip(ylim=c(-1.3,1.6)) +
    scale_fill_manual(guide = F, values = c("#aba09b", "#ba3d05"),
                       breaks = c("0", "1")) +
    tema_propio
)
ggsave("Gráficas/ranking_eci.png",
       plot = g0,
       bg = "transparent",
       width = 200,                  # Ancho de la gráfica
       height =140,
       units = "mm")
ggsave("Gráficas/ranking_eci.jpeg",
       plot = g0,
       width = 200,                  # Ancho de la gráfica
       height =140,
       units = "mm")

#Coi vs sci----
(g1 <- scatter %>% 
  mutate(cols = ifelse(nom_abr == "Nay.", 1, 0)) %>% 
  ggplot(aes(x = eci, y = coi_n , label = nom_abr, color = factor(cols))) +
  geom_smooth(method = "loess",
              span = 1.5,
              se = F,
              color = "red") +
  geom_point() +
  geom_text_repel() +
  geom_hline(yintercept = 0, color = "firebrick2", linetype = "dashed")  +
  geom_vline(xintercept = 0, color = "firebrick2", linetype = "dashed") +
  labs(title = "Perfil de las Exportaciones\n por Entidad Federativa (2014)",
       x = "Índice de Complejidad Económica",
       y = "Índice de Complejidad Potencial",
       caption = "\nDr. Gonzalo Castañeda, CIDE.") +
    scale_color_manual(values = c("deepskyblue4", "red"),
                       breaks = c("0", "1")) +
  tema_propio) 

ggsave("Gráficas/coi_eci.png",
       plot = g1,
       bg = "transparent",
       width = 200,                  # Ancho de la gráfica
       height =150,
       units = "mm")
ggsave("Gráficas/coi_eci.jpeg",
       plot = g1,
       width = 200,                  # Ancho de la gráfica
       height =150,
       units = "mm")


#Mapas de coeficientes de localización----
p_load(sp, rgeos, geojsonio, broom, viridis, cowplot, patchwork)

clusters_ent <- read_excel("Complejidad económica/Comparación clusters en entidades por coeficiente de localización.xlsx") %>% 
  janitor::clean_names() 

estados <- geojson_read("https://gist.githubusercontent.com/ponentesincausa/46d1d9a94ca04a56f93d/raw/a05f4e2b42cf981e31ef9f6f9ee151a060a38c25/mexico.json", 
                       what = "sp")

# simplify the polgons a tad (tweak 0.00001 to your liking)
estados_simp <- gSimplify(estados, tol = 0.00001)
#Conservo el nombre
estados_nom = SpatialPolygonsDataFrame(estados_simp, data=estados@data)

estados_fortified <- tidy(estados_nom, region = "name")

#Ahora mezclo las bases de datos
estados_forti_merg <- estados_fortified %>% 
  left_join(clusters_ent, by = c("id"="ent"))
  
names(clusters_ent)

#1.Cluster Hotelería y turismo----
(bar_hyt <- clusters_ent %>% 
  mutate(cols = ifelse(ent == "Nayarit", 1, 0)) %>% 
  top_n(5, hoteleria_y_turismo) %>% 
  ggplot(aes(x = reorder(ent, hoteleria_y_turismo), 
             y = hoteleria_y_turismo,
             label = round(hoteleria_y_turismo, 1),
             fill = factor(cols))) +
  geom_col() +
  geom_text(hjust = -0.2) +
  labs(title = "Top 5",
       x ="",
       y = "") +
  coord_flip(ylim=c(0,10)) +
    scale_fill_manual(values = c("dimgray", "chocolate1"),
                       breaks = c("0", "1")) +
  tema_propio 
  )

(bar_hyt_2 <- clusters_ent %>% 
    ggplot(aes(x = reorder(ent, hoteleria_y_turismo), 
               y = hoteleria_y_turismo,
               label = round(hoteleria_y_turismo, 1),
               fill = hoteleria_y_turismo)) +
    geom_col() +
    geom_text(hjust = -0.2, size = 3) +
    labs(title = "",
         x ="",
         y = "") +
    coord_flip(ylim=c(0,10)) +
    scale_fill_viridis(option = "magma",
                       direction = -1,
                       name = "Coeficiente de localización",
                       #Esto lo hago para mantener la escala continua
                       guide = guide_colorbar(direction = "horizontal",
                                              barheight = unit(2, units = "mm"),
                                              barwidth = unit(50, units = "mm"),
                                              draw.ulim = F,
                                              title.position = 'top',
                                              # some shifting around
                                              title.hjust = 0.5,
                                              label.hjust = 0.5)) +
    tema_propio 
)


(map_hyt <- estados_forti_merg %>% 
  ggplot() +
  geom_polygon(aes( x = long, 
                    y = lat, 
                    group = group, 
                    fill = hoteleria_y_turismo),
               color = "grey60") +
  labs(title = "Coeficiente de localización del cluster \n\"Hotelería y turismo\", 2014",
       caption = "Elaboración propia con datos de INEGI") +
  scale_fill_viridis(option = "magma",
                     direction = -1,
                     name = "Coeficiente de localización",
                     #Esto lo hago para mantener la escala continua
                     guide = guide_colorbar(direction = "horizontal",
                                            barheight = unit(2, units = "mm"),
                                            barwidth = unit(50, units = "mm"),
                                            draw.ulim = F,
                                            title.position = 'top',
                                            # some shifting around
                                            title.hjust = 0.5,
                                            label.hjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5, 
                                  size = 14),
        plot.subtitle = element_text(hjust=0.5,
                                     size = 12,
                                     color = "gray50"),
        plot.caption =  element_text(color = "gray50",
                                     size=10, 
                                     hjust=0),
        strip.background = element_rect(fill="gray99", 
                                        linetype="blank"),
        panel.border = element_rect(color = "gray98",
                                    fill=NA),
        legend.position = c(0.2, 0.1)) +
  coord_map() 
  )


c_hyt <- ggdraw() +
  draw_plot(map_hyt) +
  draw_plot(bar_hyt,
            x = 0.55, 
            y = 0.5, 
            width = 0.45, # Ancho de la gráfica
            height = 0.4, 
            scale = 0.8)
c_hyt


ggsave("Gráficas/c_hyt.jpeg",
       c_hyt,
       width = 200,                  # Ancho de la gráfica
       height =130,
       units = "mm")

c_hyt_2 <- map_hyt + bar_hyt_2 + plot_layout(ncol = 2, widths = c(5, 1))
c_hyt_2

ggsave("Gráficas/c_hyt.png",
       c_hyt_2,
       bg = "transparent",
       width = 200,                  # Ancho de la gráfica
       height =130,
       units = "mm")

#2.Cluster Alimentos----
cluster_Nay <- clusters_ent %>% 
  filter(ent == "Nayarit")

(bar_al <- clusters_ent %>% 
   top_n(5, elaboracion_y_procesamiento_de_alimentos) %>% 
    bind_rows(cluster_Nay) %>% 
   mutate(cols = ifelse(ent == "Nayarit", 1, 0)) %>% 
   ggplot(aes(x = reorder(ent, elaboracion_y_procesamiento_de_alimentos), 
              y = elaboracion_y_procesamiento_de_alimentos,
              label = round(elaboracion_y_procesamiento_de_alimentos, 1),
              fill = factor(cols))) +
   geom_col() +
   geom_text(hjust = -0.2) +
   labs(title = "Top 5 y Nayarit",
        x ="",
        y = "") +
   coord_flip(ylim=c(0,2)) +
   scale_fill_manual(values = c("dimgray", "chocolate1"),
                     breaks = c("0", "1")) +
   tema_propio 
)

(bar_al_2 <- clusters_ent %>% 
    ggplot(aes(x = reorder(ent, elaboracion_y_procesamiento_de_alimentos), 
               y = elaboracion_y_procesamiento_de_alimentos,
               label = round(elaboracion_y_procesamiento_de_alimentos, 1),
               fill = elaboracion_y_procesamiento_de_alimentos)) +
    geom_col() +
    geom_text(hjust = -0.2, size = 3) +
    labs(title = "",
         x ="",
         y = "") +
    coord_flip(ylim=c(0,2)) +
    scale_fill_viridis(option = "magma",
                       direction = -1,
                       name = "Coeficiente de localización",
                       #Esto lo hago para mantener la escala continua
                       guide = guide_colorbar(direction = "horizontal",
                                              barheight = unit(2, units = "mm"),
                                              barwidth = unit(50, units = "mm"),
                                              draw.ulim = F,
                                              title.position = 'top',
                                              # some shifting around
                                              title.hjust = 0.5,
                                              label.hjust = 0.5)) +
    tema_propio 
)


(map_al <- estados_forti_merg %>% 
    ggplot() +
    geom_polygon(aes( x = long, 
                      y = lat, 
                      group = group, 
                      fill = elaboracion_y_procesamiento_de_alimentos),
                 color = "grey60") +
    labs(title = "Coeficiente de localización del cluster \n\"Elaboración y procesamiento de alimentos\", 2014",
         caption = "\nElaboración propia con datos de INEGI") +
    scale_fill_viridis(option = "magma",
                       direction = -1,
                       name = "Coeficiente de localización",
                       #Esto lo hago para mantener la escala continua
                       guide = guide_colorbar(direction = "horizontal",
                                              barheight = unit(2, units = "mm"),
                                              barwidth = unit(50, units = "mm"),
                                              draw.ulim = F,
                                              title.position = 'top',
                                              # some shifting around
                                              title.hjust = 0.5,
                                              label.hjust = 0.5)) +
    theme_void() +
    theme(plot.title = element_text(hjust=0.5, 
                                    size = 14),
          plot.subtitle = element_text(hjust=0.5,
                                       size = 12,
                                       color = "gray50"),
          plot.caption =  element_text(color = "gray50",
                                       size=10, 
                                       hjust=0),
          strip.background = element_rect(fill="gray99", 
                                          linetype="blank"),
          panel.border = element_rect(color = "gray98",
                                      fill=NA),
          legend.position = c(0.2, 0.1)) +
    coord_map() 
)


c_al <- ggdraw() +
  draw_plot(map_al) +
  draw_plot(bar_al,
            x = 0.55, 
            y = 0.5, 
            width = 0.45, # Ancho de la gráfica
            height = 0.4, 
            scale = 0.8)
c_al

ggsave("Gráficas/c_al.jpeg",
       c_al,
       width = 200,                  # Ancho de la gráfica
       height =130,
       units = "mm")

c_al_2 <- map_al + bar_al_2 + plot_layout(ncol = 2, widths = c(5, 1))
c_al_2

ggsave("Gráficas/c_al.png",
       c_al_2,
       bg = "transparent",
       width = 200,                  # Ancho de la gráfica
       height =130,
       units = "mm")

#3.Pesca----

(bar_pes <- clusters_ent %>% 
    top_n(5, pesca) %>% 
    bind_rows(cluster_Nay) %>% 
    mutate(cols = ifelse(ent == "Nayarit", 1, 0)) %>% 
    ggplot(aes(x = reorder(ent, pesca), 
               y = pesca,
               label = round(pesca, 1),
               fill = factor(cols))) +
    geom_col() +
    geom_text(hjust = -0.2) +
    labs(title = "Top 5 y Nayarit",
         x ="",
         y = "") +
    coord_flip(ylim=c(0, 7.5)) +
    scale_fill_manual(values = c("dimgray", "chocolate1"),
                      breaks = c("0", "1")) +
    tema_propio 
)

(bar_pes_2 <- clusters_ent %>% 
    filter(!is.na(pesca)) %>% 
    ggplot(aes(x = reorder(ent, pesca), 
               y = pesca,
               label = round(pesca, 2),
               fill = pesca)) +
    geom_col() +
    geom_text(hjust = -0.2) +
    labs(title = "",
         x ="",
         y = "") +
    coord_flip(ylim=c(0, 7.5)) +
    scale_fill_manual(values = c("dimgray", "chocolate1"),
                      breaks = c("0", "1")) +
    tema_propio +
    scale_fill_viridis(option = "magma",
                       direction = -1,
                       name = "Coeficiente de localización",
                       #Esto lo hago para mantener la escala continua
                       guide = guide_colorbar(direction = "horizontal",
                                              barheight = unit(2, units = "mm"),
                                              barwidth = unit(50, units = "mm"),
                                              draw.ulim = F,
                                              title.position = 'top',
                                              # some shifting around
                                              title.hjust = 0.5,
                                              label.hjust = 0.5))
)

(map_pes <- estados_forti_merg %>% 
    filter(!(is.na(pesca))) %>% 
    ggplot() +
    geom_polygon(aes( x = long, 
                      y = lat, 
                      group = group, 
                      fill = as.numeric(pesca)),
                 color = "grey60") +
    labs(title = "Coeficiente de localización del cluster \n\"Pesca\", 2014",
         caption = "\nElaboración propia con datos de INEGI") +
    scale_fill_viridis(option = "magma",
                       direction = -1,
                       name = "Coeficiente de localización",
                       #Esto lo hago para mantener la escala continua
                       guide = guide_colorbar(direction = "horizontal",
                                              barheight = unit(2, units = "mm"),
                                              barwidth = unit(50, units = "mm"),
                                              draw.ulim = F,
                                              title.position = 'top',
                                              # some shifting around
                                              title.hjust = 0.5,
                                              label.hjust = 0.5)) +
    theme_void() +
    theme(plot.title = element_text(hjust=0.5, 
                                    size = 14),
          plot.subtitle = element_text(hjust=0.5,
                                       size = 12,
                                       color = "gray50"),
          plot.caption =  element_text(color = "gray50",
                                       size=10, 
                                       hjust=0),
          strip.background = element_rect(fill="gray99", 
                                          linetype="blank"),
          panel.border = element_rect(color = "gray98",
                                      fill=NA),
          legend.position = c(0.2, 0.1)) +
    coord_map() 
)


c_pes <- ggdraw() +
  draw_plot(map_pes) +
  draw_plot(bar_pes,
            x = 0.5, 
            y = 0.5, 
            width = 0.53, # Ancho de la gráfica
            height = 0.4, 
            scale = 0.8)
c_pes

ggsave("Gráficas/c_pes.jpeg",
       c_pes,
       width = 200,                  # Ancho de la gráfica
       height =130,
       units = "mm")

c_pes_2 <- map_pes + bar_pes_2 + plot_layout(ncol = 2, widths = c(5, 1))
c_pes_2

ggsave("Gráficas/c_pes.png",
       c_al_2,
       bg = "transparent",
       width = 200,                  # Ancho de la gráfica
       height =130,
       units = "mm")
#4.Cluster comercio electrónico----

(bar_dce <- clusters_ent %>% 
    top_n(5, distribucion_y_comercio_electronico) %>% 
    bind_rows(cluster_Nay) %>% 
    mutate(cols = ifelse(ent == "Nayarit", 1, 0)) %>% 
    ggplot(aes(x = reorder(ent, distribucion_y_comercio_electronico), 
               y = distribucion_y_comercio_electronico,
               label = round(distribucion_y_comercio_electronico, 1),
               fill = factor(cols))) +
    geom_col() +
    geom_text(hjust = -0.2) +
    labs(title = "Top 5 y Nayarit",
         x ="",
         y = "") +
    coord_flip(ylim=c(0, 1.7)) +
    scale_fill_manual(values = c("dimgray", "chocolate1"),
                      breaks = c("0", "1")) +
    tema_propio 
)

(bar_dce_2 <- clusters_ent %>% 
    ggplot(aes(x = reorder(ent, distribucion_y_comercio_electronico), 
               y = distribucion_y_comercio_electronico,
               label = round(distribucion_y_comercio_electronico, 1),
               fill = distribucion_y_comercio_electronico)) +
    geom_col() +
    geom_text(hjust = -0.2, size = 3) +
    labs(title = "",
         x ="",
         y = "") +
    coord_flip(ylim=c(0, 1.7)) +
    tema_propio +
  scale_fill_viridis(option = "magma",
                     direction = -1,
                     name = "Coeficiente de localización",
                     #Esto lo hago para mantener la escala continua
                     guide = guide_colorbar(direction = "horizontal",
                                            barheight = unit(2, units = "mm"),
                                            barwidth = unit(50, units = "mm"),
                                            draw.ulim = F,
                                            title.position = 'top',
                                            # some shifting around
                                            title.hjust = 0.5,
                                            label.hjust = 0.5))
)


(map_dce <- estados_forti_merg %>% 
    ggplot() +
    geom_polygon(aes( x = long, 
                      y = lat, 
                      group = group, 
                      fill = distribucion_y_comercio_electronico),
                 color = "grey60") +
    labs(title = "Coeficiente de localización del cluster \n\"Distribución y comercio electrónico\", 2014",
         caption = "Elaboración propia con datos de INEGI") +
    scale_fill_viridis(option = "magma",
                       direction = -1,
                       name = "Coeficiente de localización",
                       #Esto lo hago para mantener la escala continua
                       guide = guide_colorbar(direction = "horizontal",
                                              barheight = unit(2, units = "mm"),
                                              barwidth = unit(50, units = "mm"),
                                              draw.ulim = F,
                                              title.position = 'top',
                                              # some shifting around
                                              title.hjust = 0.5,
                                              label.hjust = 0.5)) +
    theme_void() +
    theme(plot.title = element_text(hjust=0.5, 
                                    size = 14),
          plot.subtitle = element_text(hjust=0.5,
                                       size = 12,
                                       color = "gray50"),
          plot.caption =  element_text(color = "gray50",
                                       size=10, 
                                       hjust=0),
          strip.background = element_rect(fill="gray99", 
                                          linetype="blank"),
          panel.border = element_rect(color = "gray98",
                                      fill=NA),
          legend.position = c(0.2, 0.1)) +
    coord_map() 
)


c_dce <- ggdraw() +
  draw_plot(map_dce) +
  draw_plot(bar_dce,
            x = 0.5, 
            y = 0.5, 
            width = 0.5, # Ancho de la gráfica
            height = 0.4, 
            scale = 0.8)
c_dce

ggsave("Gráficas/c_dce.jpeg",
       c_dce,
       width = 200,                  # Ancho de la gráfica
       height =130,
       units = "mm")

c_dce_2 <- map_dce + bar_dce_2 + plot_layout(ncol = 2, widths = c(5, 1))
c_dce_2

ggsave("Gráficas/c_dce.png",
       c_dce_2,
       bg = "transparent",
       width = 200,                  # Ancho de la gráfica
       height =130,
       units = "mm")

#5.Cluster edu----

(bar_edu <- clusters_ent %>% 
   top_n(5, educacion) %>% 
   bind_rows(cluster_Nay) %>% 
   mutate(cols = ifelse(ent == "Nayarit", 1, 0)) %>% 
   ggplot(aes(x = reorder(ent, educacion), 
              y = educacion,
              label = round(educacion, 1),
              fill = factor(cols))) +
   geom_col() +
   geom_text(hjust = -0.2) +
   labs(title = "Top 5 y Nayarit",
        x ="",
        y = "") +
   coord_flip(ylim=c(0, 1.7)) +
   scale_fill_manual(values = c("dimgray", "chocolate1"),
                     breaks = c("0", "1")) +
   tema_propio 
)

(bar_edu_2 <- clusters_ent %>% 
    ggplot(aes(x = reorder(ent, educacion), 
               y = educacion,
               label = round(educacion, 1),
               fill = educacion)) +
    geom_col() +
    geom_text(hjust = -0.2, size = 3) +
    labs(title = "",
         x ="",
         y = "") +
  coord_flip(ylim=c(0, 1.8)) +
    scale_fill_viridis(option = "magma",
                       direction = -1,
                       name = "Coeficiente de localización",
                       #Esto lo hago para mantener la escala continua
                       guide = guide_colorbar(direction = "horizontal",
                                              barheight = unit(2, units = "mm"),
                                              barwidth = unit(50, units = "mm"),
                                              draw.ulim = F,
                                              title.position = 'top',
                                              # some shifting around
                                              title.hjust = 0.5,
                                              label.hjust = 0.5)) +
    tema_propio 
)


(map_edu <- estados_forti_merg %>% 
    ggplot() +
    geom_polygon(aes( x = long, 
                      y = lat, 
                      group = group, 
                      fill = educacion),
                 color = "grey60") +
    labs(title = "Coeficiente de localización del cluster \n\"Educación y desarrollo de talento\", 2014",
         caption = "\nElaboración propia con datos de INEGI") +
    scale_fill_viridis(option = "magma",
                       direction = -1,
                       name = "Coeficiente de localización",
                       #Esto lo hago para mantener la escala continua
                       guide = guide_colorbar(direction = "horizontal",
                                              barheight = unit(2, units = "mm"),
                                              barwidth = unit(50, units = "mm"),
                                              draw.ulim = F,
                                              title.position = 'top',
                                              # some shifting around
                                              title.hjust = 0.5,
                                              label.hjust = 0.5)) +
    theme_void() +
    theme(plot.title = element_text(hjust=0.5, 
                                    size = 14),
          plot.subtitle = element_text(hjust=0.5,
                                       size = 12,
                                       color = "gray50"),
          plot.caption =  element_text(color = "gray50",
                                       size=10, 
                                       hjust=0),
          strip.background = element_rect(fill="gray99", 
                                          linetype="blank"),
          panel.border = element_rect(color = "gray98",
                                      fill=NA),
          legend.position = c(0.2, 0.1)) +
    coord_map() 
)


c_edu <- ggdraw() +
  draw_plot(map_edu) +
  draw_plot(bar_edu,
            x = 0.5, 
            y = 0.5, 
            width = 0.5, # Ancho de la gráfica
            height = 0.4, 
            scale = 0.8)
c_edu

ggsave("Gráficas/c_edu.jpeg",
       c_edu,
       width = 200,                  # Ancho de la gráfica
       height =130,
       units = "mm")

c_edu_2 <- map_edu + bar_edu_2 + plot_layout(ncol = 2, widths = c(5, 1))
c_edu_2

ggsave("Gráficas/c_edu.png",
       c_edu_2,
       bg = "transparent",
       width = 200,                  # Ancho de la gráfica
       height =130,
       units = "mm")
