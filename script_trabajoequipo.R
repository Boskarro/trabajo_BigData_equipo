#SCRIPT DEL TRABAJO SOBRE ALQUILERES -- MANIPULANDO LOS DATOS


#Primero vamos a importar los datos:

datos <- "./datos/DATOS_VIVIENDA.xlsx"
info <- rio::import(datos)

#Están un poco asalvajados, tenemos que ordenarlos y apañarlos un poco
library(plotly)

library(zoo)

library(gganimate)

library(lubridate)

library(tidyverse)
df2 <- info %>%
  pivot_wider(names_from = TAMAÑO,
              values_from = ÍNDICE)
df <- info %>%
  pivot_wider(names_from = AÑO,
              values_from = ÍNDICE)

#PLOT EVOLUCION INDICE GENERAL POR TAMAÑO

############

p_plotly <- pp %>%
  layout(
    legend = list(
      title = "Tamaño",
      font = list(
        family = "Arial", 
        size = 12
      ),
      bgcolor = "white",
      bordercolor = "black",
      borderwidth = 1,
      itemsizing = "constant"  
    )
  )

p_plotly

p_plotly <- p_plotly %>%
  layout(
    annotations = list(
      list(
        text = "Fuente: INE",  # Agregar la fuente de información
        font = list(
          family = "Arial",  # Establecer la fuente del texto de la fuente de información
          size = 10
        ),
        x = 1,  # Ajustar la posición horizontal del texto
        y = -0.15,  # Ajustar la posición vertical del texto
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        xanchor = "right",
        yanchor = "auto"
      )
    )
  )


p_plotly

###########


df3 <- info %>%
  filter(COMUNIDAD == "Total nacional")


p<- ggplot(df3,
       aes(
  AÑO, 
  ÍNDICE,
  color = TAMAÑO)) +
  geom_line(size = 0.95) +
  ggthemes::theme_stata() + 
  labs(
  title = "Evolución del Índice general de precios de alquiler por tamaño de vivienda (2015 = 100).",
  subtitle = "serie 2011 - 2021")

pp <- ggplotly(p)

p <- pp %>%
  layout(
    legend = list(
      title = "Tamaño",
      font = "Arial black",
      size = 12
    ),
    annotations = list(
      text = "Fuente: INE",
      font = list(
        family = "Arial",
        size = 9.5)
      ),
    title = list(
      text = "Evolución del Índice general de precios\nde alquiler por tamaño de vivienda (2015 = 100)."),
    font = list(
      size = 14.5,
      family = "Arial black"),
    x = 0.02,
    xaxis = list(
      title = "AÑO",
      tickfont = list(
        family = "Arial Black",
        size = 10
      )),
    yaxis = list(
      title = "ÍNDICE",
      tickfont = list(
        family = "Arial Black",
        size = 12)))

pp

p_plotly <- p_plotly %>%
  layout(
    annotations = list(
      list(
        text = "Fuente: TuFuenteDeInformacion.com",  # Agregar la fuente de información
        font = list(
          family = "Arial",  # Establecer la fuente del texto de la fuente de información
          size = 10
        ),
        x = 1,  # Ajustar la posición horizontal del texto
        y = -0.15,  # Ajustar la posición vertical del texto
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        xanchor = "right",
        yanchor = "auto"
      )
    )
  )


pp

#POR CCAA

library(patchwork)

coloresg1 <- c("#66CDAA","#A52A2A","#53868B","#458B00")
coloresg2 <- c("darkgoldenrod3","#9A32CD", "deeppink3", "#EEEE00")
coloresg3 <- c("#8B7E66", "#00FF7F", "#7FFF00","#00BFFF","#FF3030", "darkslategray")
coloresg4 <- c("#EEC900","darkseagreen4", "#CD5555", "#EEA2AD")


df_cca1 <- info %>%
  pivot_wider(names_from = TAMAÑO, values_from = ÍNDICE) %>%
  filter(COMUNIDAD %in% c("06 Cantabria", "02 Aragón", "03 Principado de Asturias", "12 Galicia"))

df_cca2 <- info %>%
  pivot_wider(names_from = TAMAÑO, values_from = ÍNDICE) %>%
  filter(COMUNIDAD %in% c("07 Castilla y León", "08 Castilla La Mancha", "15 La Rioja", "11 Extremadura"))

df_cca3 <- info %>%
  pivot_wider(names_from = TAMAÑO, values_from = ÍNDICE) %>%
  filter(COMUNIDAD %in% c("10 Comunitat Valenciana", "01 Andalucía", "13 Comunidad de Madrid", "14 Región de Murcia", "09 Cataluña"))

df_cca4 <- info %>%
  pivot_wider(names_from = TAMAÑO, values_from = ÍNDICE) %>%
  filter(COMUNIDAD %in% c("16 Ceuta", "17 Melilla", "04 Illes Balears", "05 Canarias"))


CCAAg1<- ggplot(df_cca1,
              aes(
                AÑO,
                General,
                color = COMUNIDAD)) +
  geom_line(size = 0.95) +
  scale_color_manual(values = coloresg1) + 
  labs(
    y = "IPC") +
  ggthemes::theme_stata()


CCAAg2<- ggplot(df_cca2,
                aes(
                  AÑO,
                  General,
                  color = COMUNIDAD)) +
  geom_line(size = 0.95) +
  scale_color_manual(values = coloresg2) + 
  labs(
    color = NULL,
    y = "IPC") +
  ggthemes::theme_stata()

CCAAg3<- ggplot(df_cca3,
                aes(
                  AÑO,
                  General,
                  color = COMUNIDAD)) +
  geom_line(size = 0.95) +
  scale_color_manual(values = coloresg3) + 
  labs(
  color = NULL,
  y = "IPC") +
  ggthemes::theme_stata() 

CCAAg4<- ggplot(df_cca4,
                aes(
                  AÑO,
                  General,
                  color = COMUNIDAD)) +
  geom_line(size = 0.95) +
  scale_color_manual(values = coloresg4) + 
  labs(
  color = NULL,
  y = "IPC") +
  ggthemes::theme_stata() 


PLOT_CONJUNTO <- ((CCAAg1 + CCAAg2) / (CCAAg3 + CCAAg4)) +
  plot_layout(guides = "collect") +
  theme(
    plot.title.position = "collect")  +
  plot_annotation(
    title = "Evolución del ÍPC general de \n precios de alquiler por CCAA agrupadas (2015 = 100)",
    caption = "Fuente: INE") & theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.text = element_text(size = 5) ,
      legend.key.size = unit(0.5,"cm"),
      legend.title = element_text(size = 5)
    )

PLOT_CONJUNTO <- ((CCAAg1 + CCAAg2) / (CCAAg3 + CCAAg4)) +
  plot_layout(guides = "collect") +
  theme(
    plot.title.position = "collect")  +
  plot_annotation(
    title = "Evolución del ÍPC general de \n precios de alquiler por CCAA agrupadas (2015 = 100)",
    caption = "Fuente: INE") & ggthemes::theme_stata() & theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.text = element_text(size = 5) ,
      legend.key.size = unit(0.5,"cm"),
      legend.title = element_text(size = 5),
      plot.title = element_text(size = 12, hjust = 0.05))
    
library(gganimate)

PLOT_CONJUNTO + transition_reveal(AÑO)

animate(PLOT_CONJUNTO, nframes = 144, fps = 10)

PLOT_CONJUNTO


ggdraw(mi_leyenda)
PLOT_CONJUNTOv2 + mi_leyenda

##PEDAZO GRAFICA Q HAY Q METER SI O SI#####################################
library(tidyverse)
CCAA_MEJOR <- ggplot(df2, aes(AÑO, General, color = COMUNIDAD)) +
  geom_line(size = 1) +

    labs(
    title = "Evolución del Índice general de precios de alquiler por CCAA (2015 = 100).",
    subtitle = "Serie 2011 - 2021",
    caption = "Fuente: INE"
  ) +
  ggthemes::theme_stata() + 
  facet_wrap(
  vars(COMUNIDAD),
  nrow = 9,
  ncol = 9)

CCAA_MEJOR
#####################################################################

library(tidyverse)
library(gganimate)
CCAA_MEJOR <- ggplot(info, aes(Año, General, color = COMUNIDAD)) +
  geom_line(size = 1) +
  labs(
    title = "Evolución del Índice general de precios de alquiler por CCAA (2015 = 100).",
    subtitle = "Serie 2011 - 2021",
    caption = "Fuente: INE"
  ) +
  ggthemes::theme_stata() + 
  facet_wrap(
    vars(COMUNIDAD),
    nrow = 9,
    ncol = 9)

CCAA_MEJOR


#EVOLUCION PARA COMUNIDADES MAS GRANDES

df4 <- info %>%
  filter()


CCAA_GRANDES<- ggplot(df2,
                      aes(
                        AÑO,
                        General,
                        color = COMUNIDAD)) +
  geom_line()

  
####OTRA MAS QUE HAY QUE METER###################################################

SALARIOS <- rio::import("./datos/datos_salariomedio.csv") %>%
  select(Año, Salariomedio) %>%
  drop_na() %>%
  mutate(Variacion =((Salariomedio - lag(Salariomedio))/lag(Salariomedio))*100)


Evo_salarios <- ggplot(SALARIOS, aes(Año, Salariomedio)) + geom_line(aes(group = 1), color = "aquamarine4", size = 0.95) + ggthemes:: theme_stata()

  
Evo_salarios + labs(
  title = "Evolución del salario medio anual.",
  subtitle = "Serie 2006-2020.",
  y = "Salario medio (miles de euros)",
  caption = "Fuente: Epdata")

Var_Salarios <- ggplot(SALARIOS, aes(Año, Variacion)) + geom_line(aes(group = 1), color = "#53868B", size = 0.95) + ggthemes:: theme_stata()

Var_Salarios + labs(
  title = "Crecimiento del salario medio.",
  subtitle = "Serie 2006-2020.",
  y = "Variación en porcentaje",
  caption = "Fuente: Epdata")

#MÁS DATOS
library(gganimate)
library(tidyverse)
IPC<- rio::import("./datos/ipc_bueno.csv")

df_IPC <- IPC %>%
  rename(IPC = indice_precios_consumo_tasas_variacion) %>%
  select(indicadores,grupos_coicop_nombre,provincias_nombre,periodos_id,IPC) %>%
  filter(periodos_id %in% c("2002M01","2003M01","2004M01","2005M01","2006M01","2007M01","2008M01","2009M01","2010M01","2011M01","2012M01","2013M01","2014M01","2015M01","2016M01"))


df_IPC_valores <- df_IPC %>%
  filter(indicadores == "Índices") %>%
  filter(provincias_nombre == "España") %>%
  mutate(periodos_id = as.Date(paste0(substr(periodos_id, 1, 4), substr(periodos_id, 6, 7), "-01"), format = "%Y%m-%d"))

df_IPC_tasas <- df_IPC %>%
  filter(indicadores == "Tasas de variación anual") %>%
  filter(provincias_nombre == "España") %>%
  mutate(periodos_id = as.Date(paste0(substr(periodos_id, 1, 4), substr(periodos_id, 6, 7), "-01"), format = "%Y%m-%d"))


plot_IPC_tasas <- ggplot(df_IPC_tasas, aes(periodos_id, IPC, group = grupos_coicop_nombre, color = grupos_coicop_nombre)) + 
  geom_line(size = 0.9) +
  ggthemes::theme_stata() +
  theme(
    axis.text.x = element_text(
      angle = 75, hjust = 1, vjust = 1, size = 5, margin = margin(
        r = 5, b = 5, l = 5)),
    axis.text.y = element_text(size = 5),
    panel.spacing = unit(0.75, "lines"),
    legend.title = element_text(size = 8),
    legend.key.size = unit(1, "cm"),
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(0.1,"cm"),
    legend.direction = "vertical",
    legend.position = "right") +
  labs(
    title = "Evolución IPC en tasas por grupos COICOP",
    subtitle = "Serie 2002 - 2016",
    x = "Periodo",
    y = "Var. IPC",
    caption = "Fuente: datos.gob.es (Gobierno de España)",
    color = "Grupos COICOP"
  ) +
  facet_wrap(
    vars(grupos_coicop_nombre),
    nrow = 4,
    ncol = 4,
    labeller = labeller(grupos_coicop_nombre = function(label) str_wrap(label, width = 20))) +
  theme(strip.text = element_text(size = 8))

plot_IPC_tasas + transition_reveal(periodos_id) + view_follow(fixed_y = TRUE)

gganimate::animate(plot_IPC_tasas, nframes = 195, fps = 20)

plot_IPC_valores <- ggplot(df_IPC_valores, aes(periodos_id, IPC, group = grupos_coicop_nombre, color = grupos_coicop_nombre)) + 
  geom_line(size = 0.9) +
  ggthemes::theme_stata() +
  theme(
    axis.text.x = element_text(
      angle = 75, hjust = 1, vjust = 1, size = 5, margin = margin(
        r = 5, b = 5, l = 5)),
    axis.text.y = element_text(size = 5),
    panel.spacing = unit(0.75, "lines"),
    legend.title = element_text(size = 8),
    legend.key.size = unit(1, "cm"),
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(0.1,"cm"),
    legend.direction = "vertical",
    legend.position = "right") +
  labs(
    title = "Evolución IPC por grupos COICOP",
    subtitle = "Serie 2002 - 2016",
    x = "Periodo",
    y = "IPC",
    caption = "Fuente: datos.gob.es (Gobierno de España)",
    color = "Grupos COICOP"
  ) +
  facet_wrap(
    vars(grupos_coicop_nombre),
    nrow = 4,
    ncol = 4,
    labeller = labeller(grupos_coicop_nombre = function(label) str_wrap(label, width = 20))) +
  theme(strip.text = element_text(size = 8))

plot_animado <- plot_IPC_valores + transition_reveal(periodos_id) + view_follow(fixed_y = TRUE)
plot_animado + theme(
 legend.text = element_text(size = 5) 
  
)

animacion <- animate(plot_animado, nframes = 195, fps = 20)

ajustes_leyenda <- list(theme(legend.text = element_text(size = 5)))

animacion + theme(legend.text = element_text(size = 5))

plot_IPC_valores + theme(axis.text.y = element_text(size = 4),
    legend.text = element_text(size = 6,
        family = "Japan1"), legend.title = element_text(size = 5,
        family = "Bookman"))



colores_IPC <- c("#FF4040", "#8B7355", "chartreuse3", "#0000CD","#68228B","darkseagreen3","#EEC900","#FFB6C1","#7FFFD4","#CAFF70","#8B0000","#FFFF00", "wheat")


CCAA_MEJOR <- ggplot(df2, aes(AÑO, General, color = COMUNIDAD)) +
  geom_line(size = 1) +
  labs(
    y = "IPC General",
    title = "Evolución del Índice general de precios de alquiler por CCAA (2015 = 100).",
    subtitle = "Serie 2011 - 2021",
    caption = "Fuente: INE"
  ) +
  ggthemes::theme_stata() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1, size = 6, margin = margin(r = 5, b = 5, l = 5))) +
  facet_wrap(
    vars(COMUNIDAD),
    nrow = 9,
    ncol = 9,
    labeller = labeller(COMUNIDAD = function(label) str_wrap(label, width = 15))) +
  theme(strip.text = element_text(size = 8))

CCAA_MEJOR


library(sf)
library(tidyverse)
library(pjpv.curso.R.2022)

###cargamos los datos 
datos_c<- "./datos/prueba.csv"
datos_c<- rio::import(datos_c)

### En la columna Total queremos poner un punto en vez de una coma 

datos_c <- datos_c%>% mutate(Total = stringr::str_replace(Total, "," , "." ))

#- Cambiamos el nombre de las columnas 

df_c<- datos_c %>% rename(Indice_precio = Total) %>% rename(CCAA = Comunidades_Autonomas)

###Separamos la columna de CCAA en dos Columnas, una que muestre el código y otra el nombre de la Comunidad Autonoma  

df_c <- df_c %>%
  tidyr::separate(CCAA, sep = " ",
                  into = c("ine_ccaa", "ine_ccaa.n"), extra = "merge")
df_c<-  drop_na(df_c) #quitamos el total nacional, ya que no se van a utilizar

df_c<- df_c %>% filter(General == "General") %>% filter( Índices_tasas =="Media anual") %>% filter(Periodo == "2022") ##cogemos las viviendas generales, la media anual y elegimos el año más reciente que es 2022


###Cargamos las geometrías y elegimos las variables que nos interesan
df_geo_prov <- pjpv.curso.R.2022::LAU2_prov_2020_canarias
plot(df_geo_prov, max.plot = 1)
df_geo_prov <- df_geo_prov %>% select(ine_prov, ine_prov.n, ine_ccaa, ine_ccaa.n)

##Agregamos geometrías de las Comunidades Autonomas 

df_geo_ccaa <- df_geo_prov %>% group_by(ine_ccaa, ine_ccaa.n) %>% summarize() %>% ungroup()
plot(df_geo_ccaa, max.plot = 1)

df_ok <- left_join(df_geo_ccaa, df_c, by = c("ine_ccaa" = "ine_ccaa"))
names(df_ok)

##Calculamos los Centroides
library(sf)
df_geo_ccaa <- cbind(df_geo_ccaa, st_coordinates(st_centroid(df_geo_ccaa$geometry)))

##Juntamos los datos de los precios de los alquileres con las geometrías 
df_ok <- left_join(df_geo_ccaa, df_c, by = c("ine_ccaa" = "ine_ccaa"))





library(tidyverse)
library(viridis)
library(hrbrthemes)
library(mapdata)
library(sf)
library(sp)

# Load dataset from github

datos_alquiler<- "./datos/59057.csv"
datos_alquiler<- rio::import(datos_alquiler)

datos_alquiler<- datos_alquiler%>% mutate(Total = stringr::str_replace(Total, "," , "." ))

datos_alquiler<- drop_na(datos_alquiler)

##Separamos la columna de CCAA en dos Columnas, una que muestre el código y otra el nombre de la Comunidad Autonoma  

datos_alquiler <- datos_alquiler %>%
  tidyr::separate(CCAA, sep = " ",
                  into = c("ine_ccaa", "ine_ccaa.n"), extra = "merge") 


##Quitamos Total Nacional (que no lo necesitamos)

datos_alquiler<- drop_na(datos_alquiler)

##Cogemos las variables que nos interesan, filtramos por Indice y filtramos por el año 2021(es el más reciente)

datos_alquiler<- datos_alquiler %>%
  filter(Tamaño == "Total") %>%
  filter(Dato =="Índice") %>% 
  filter(Periodo == 2021) %>%
  select(ine_ccaa, ine_ccaa.n, Total)


df_geo_ccaa <- df_geo_prov %>% group_by(ine_ccaa, ine_ccaa.n) %>% summarize() 
df_ok <- left_join(df_geo_ccaa, datos_alquiler, by = c("ine_ccaa" = "ine_ccaa"))
names(df_ok)

ibrary(sf)
df_geo_ccaa <- cbind(df_geo_ccaa, st_coordinates(st_centroid(df_geo_ccaa$geometry)))

df_ok <- left_join(df_geo_ccaa, datos_alquiler, by = c("ine_ccaa" = "ine_ccaa"))


library(tmap)



df_ok$Indice_precio <- as.numeric(as.character(df_ok$Indice_precio))
df_ok %>%
  as.numeric(df_ok$Indice_precio)

str(df_ok$Indice_precio)

plot1 <- ggplot(df_ok) +
  geom_sf(aes(fill = Indice_precio), alpha = 0.9, color = NA) +
  scale_fill_gradientn(
    colors = hcl.colors(100, "Inferno"),
    n.breaks = 10,
    labels = scales::label_comma(),
    guide = guide_legend()
  ) +
  labs(
    fill = "IPC General",
    title = NULL,
    subtitle = "Datos INE (2019)"
  ) +
  theme_void() 

plot1

# plot
df_ok %>%
  ggplot( aes(x=X, y=Y)) + 
  geom_hex(bins= 25) +
  ggplot2::annotate("text", x = -27, y = 72, label="ATULAKAKATUAD", colour = "black", size=5, alpha=1, hjust=)
                      
                      
                      
                      
############TASA EMANCIPACION##############
library(tidyverse)
Df_eman <- rio::import("./datos/TASA DE EMANCIPACION.csv")

Df_good <- Df_eman %>%
  mutate(Year = as.factor(Year)) %>%
  select(Year, Tasa)

plot_eman <-ggplot(Df_good, aes(Year, Tasa)) + geom_line(group = 1) +
  labs(
    title = "Tasa de emancipación para jóvenes de 18 -34 años.",
    x = "Año",
    y = "Tasa de emancipación",
    caption = "Fuente: datos.gob.es (Gobierno de España)."
  ) +
  ggthemes::theme_stata()

plot_eman
