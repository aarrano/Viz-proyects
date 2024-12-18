#install.packages("ggalluvial")

pacman::p_load(tidyverse,data.table,ggalluvial,dplyr,tidyr)

asis_23 <- readRDS("data/Asistencia_2023_Mar_Dic (1).rds")

asis_23 <- asis_23 %>%
  mutate(across(c(n_asis_3:n_asis_12, n_trab_3:n_trab_12), ~ replace_na(., 0))) %>%
  mutate(
    asis_abril = round((n_asis_3 + n_asis_4) * 100 / (n_trab_3 + n_trab_4), 2),
    asis_julio = round(rowSums(across(n_asis_3:n_asis_7)) * 100 / rowSums(across(n_trab_3:n_trab_7)), 2),
    asis_dic = round(rowSums(across(n_asis_3:n_asis_12)) * 100 / rowSums(across(n_trab_3:n_trab_12)), 2)
  )

asis_23 <- asis_23 %>%
  mutate(
    tipo_asis_abril = case_when(
      asis_abril >= 97 ~ "Destacada",
      asis_abril < 97 & asis_abril >= 90 ~ "Normal",
      asis_abril < 90  & asis_abril >= 85 ~ "Inasis Reiterada",
      asis_abril < 85   ~ "Inasis grave",
      TRUE ~ "Sin asistencia"
    )
  )

asis_23 <- asis_23 %>%
  mutate(
    tipo_asis_julio = case_when(
      asis_julio >= 97 ~ "Destacada",
      asis_julio < 97 & asis_julio >= 90 ~ "Normal",
      asis_julio < 90  & asis_julio >= 85 ~ "Inasis Reiterada",
      asis_julio < 85   ~ "Inasis grave",
      TRUE ~ "Sin asistencia"
    )
  )

asis_23 <- asis_23 %>%
  mutate(
    tipo_asis_dic = case_when(
      asis_dic >= 97 ~ "Destacada",
      asis_dic < 97 & asis_dic >= 90 ~ "Normal",
      asis_dic < 90  & asis_dic >= 85 ~ "Inasis Reiterada",
      asis_dic < 85   ~ "Inasis grave",
      TRUE ~ "Sin asistencia"
    )
  )

# Generamos tablas con frecuencias de casos
contingency_table <- as.data.frame(table(asis_23$tipo_asis_abril, asis_23$tipo_asis_julio,asis_23$tipo_asis_dic))

colnames(contingency_table) <- c("tipo_asis_abril", "tipo_asis_julio","tipo_asis_diciembre", "count")

#Eliminamos combinaciones irrelevantes
contingency_table <- contingency_table %>% 
  filter(tipo_asis_abril != "Sin asistencia") %>% 
  filter(tipo_asis_julio != "Sin asistencia") %>%
  filter(tipo_asis_diciembre != "Sin asistencia") %>% 
  filter( count > 0)

## GGaluvial

contingency_table <- contingency_table %>%
  mutate(across(c(tipo_asis_abril, tipo_asis_julio, tipo_asis_diciembre), 
                ~ factor(., levels = c("Destacada", "Normal", "Inasis Reiterada", "Inasis grave"))))

ggplot(
  data = contingency_table,
  aes(
    axis1 = tipo_asis_abril,
    axis2 = tipo_asis_julio,
    axis3 = tipo_asis_diciembre,
    y = count
  ) ) +
  geom_alluvium(aes(fill = tipo_asis_diciembre), ,
                curve_type = "cubic") +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Abril", "Julio","Diciembre"),
                    expand = c(.2, .01)) +
  xlab("Tipo de asistencia por mes")+
  theme_minimal() +
  labs(fill = "Tipo Asistencia", title = "¿Quiénes tienen inasistencia grave en diciembre? Asistencia 2023 en Chile",
       caption = "Autor: Alonso Arraño Portuguez \n Fuente: Elaboración propia en base a datos CEM (2023)") +
  scale_fill_manual(
    values = c(
      "Destacada" = "#88878a", #1F77B4
      "Normal" = "#88878a", #AEC7E8
      "Inasis Reiterada" = "#FF9896", #FF9896
      "Inasis grave" = "#88878a" #"#D62728"
    )
  )+
  guides(fill = "none")+
  theme_void() +
  theme(
    axis.title.x = element_text(size = 12),  # Título del eje X visible
    axis.text.x = element_text(size = 10)  # Etiquetas del eje X visibles
  ) + theme(
    plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")  # Ajusta los márgenes (superior, derecho, inferior, izquierdo)
  )

ggsave(
  "grafico_alta_calidad_dic.png",  # Nombre del archivo de salida
  width = 15,                  # Ancho en pulgadas
  height = 10,                 # Altura en pulgadas
  dpi = 300,                   # Resolución (300 ppi para alta calidad)
  units = "in",                # Unidades de tamaño (pulgadas)
  plot = last_plot() ,          # Utiliza el último gráfico generado
  bg = "white")

contingency_table <- contingency_table %>% 
  mutate(total = sum(count), .by = tipo_asis_abril) %>% 
  mutate(prop = round(count*100/total,2)) %>% 
  group_by(tipo_asis_abril) %>% 
  arrange(desc(prop),.by_group = TRUE) %>% 
  ungroup()

# writexl::write_xlsx(contingency_table,"tabla_casos.xlsx",col_names = TRUE)


##  Educacion Pública  ----

asis_23_ep <- asis_23 %>% filter(cod_depe2_m == 5)


contingency_table_ep <- as.data.frame(table(asis_23_ep$tipo_asis_abril, asis_23_ep$tipo_asis_julio,asis_23_ep$tipo_asis_dic))

colnames(contingency_table_ep) <- c("tipo_asis_abril", "tipo_asis_julio","tipo_asis_diciembre", "count")

contingency_table_ep <- contingency_table_ep %>% 
  filter(tipo_asis_abril != "Sin asistencia") %>% 
  filter(tipo_asis_julio != "Sin asistencia") %>%
  filter(tipo_asis_diciembre != "Sin asistencia") %>% 
  filter( count > 0)

## GGaluvial

contingency_table_ep$tipo_asis_abril <- factor(contingency_table_ep$tipo_asis_abril, 
                                            levels = c("Destacada", "Normal", "Inasis Reiterada", "Inasis grave"))

contingency_table_ep$tipo_asis_julio <- factor(contingency_table_ep$tipo_asis_julio, 
                                            levels = c("Destacada", "Normal", "Inasis Reiterada", "Inasis grave"))

contingency_table_ep$tipo_asis_diciembre <- factor(contingency_table_ep$tipo_asis_diciembre, 
                                                levels = c("Destacada", "Normal", "Inasis Reiterada", "Inasis grave"))

ggplot(
  data = contingency_table_ep,
  aes(
    axis1 = tipo_asis_abril,
    axis2 = tipo_asis_julio,
    axis3 = tipo_asis_diciembre,
    y = count
  ) ) +
  geom_alluvium(aes(fill = tipo_asis_diciembre), ,
                curve_type = "cubic") +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Abril", "Julio","Diciembre"),
                   expand = c(.2, .01)) +
  xlab("Tipo de asistencia por mes")+
  theme_minimal() +
  labs(fill = "Tipo Asistencia", title = "Flujo del tipo de asistencia (abril, julio, diciembre) 2023") +
  scale_fill_manual(
    values = c(
      "Destacada" = "#1F77B4", #1F77B4
      "Normal" = "#88878a", #AEC7E8
      "Inasis Reiterada" = "#88878a", #FF9896
      "Inasis grave" = "#88878a" #"#D62728"
    )
  )+
  guides(fill = "none")+
  theme_void() +
  theme(
    axis.title.x = element_text(size = 12),  # Título del eje X visible
    axis.text.x = element_text(size = 10)  # Etiquetas del eje X visibles
  ) + theme(
    plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")  # Ajusta los márgenes (superior, derecho, inferior, izquierdo)
  )

contingency_table_h <- contingency_table_h %>% 
  mutate(total = sum(count), .by = tipo_asis_abril) %>% 
  mutate(prop = round(count*100/total,2)) %>% 
  group_by(tipo_asis_abril) %>% 
  arrange(desc(prop),.by_group = TRUE) %>% 
  ungroup()