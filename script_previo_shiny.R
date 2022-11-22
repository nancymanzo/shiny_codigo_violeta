# - - - - - - - - - - - - - SCRPIT PREVIO A CORRER SHINY - - - - - - - - - - - #

library(googledrive)
library(googlesheets4)
library(shiny)
library(readxl)
library(readr)
library(viridis)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyverse)
library(DT)
library(plotly)
library(lubridate)
library(viridis)
library(RColorBrewer) 
library(janitor)
library(mxmaps)
library(stringr)
library(wordcloud2)
library(RColorBrewer)
library(shinydashboard)
library(wordcloud)
library(shinydashboard)
library(shiny)
library(plotly)
library(dashboardthemes)
library(shinythemes)
library(shinybusy)
library(extrafont)
library(showtext)
library(jsonlite)
library(data.table)
library(shinyjs)
library(leaflet)
library(mxmaps)



nb.cols <-13
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)


nb.cols.129 <-129
mycolors129 <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols.129)


nb.cols_2 <- 20
bupu <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols_2)


nb.cols_2 <- 20
bupu_2 <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols_2)


drive_find(n_max = 6)
1

drive_download("codigo_violeta_bases.xlsx",overwrite = TRUE)
drive_download("2.reportes_llamadas_2021_2022.xlsx",overwrite = TRUE)
drive_download("1.reportes_llamadas_2019_2020.xlsx",overwrite = TRUE)
drive_download("medidas_ordenes",overwrite = TRUE)
drive_download("atenciones.xlsx",overwrite = TRUE)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

reportes_llamadas_2021_2022 <- read_excel("2.reportes_llamadas_2021_2022.xlsx")
reportes_llamadas_2019_2020 <- read_excel("1.reportes_llamadas_2019_2020.xlsx")
reportes_llamadas<-rbind(reportes_llamadas_2019_2020, reportes_llamadas_2021_2022)

setnames(reportes_llamadas, tolower(names(reportes_llamadas)))


reportes_llamadas$fecha <-substr(reportes_llamadas$fecha, start = 1, stop = 10)
reportes_llamadas$fecha   <-as.POSIXct(reportes_llamadas$fecha)
reportes_llamadas$fecha   <-as.Date(reportes_llamadas$fecha,format="%Y-%m-%d")
reportes_llamadas$mes     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y-%m")
reportes_llamadas$año     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y")
reportes_llamadas$month     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%B")

reportes_llamadas <- reportes_llamadas %>%
mutate(clasificación=case_when(
  clasificación=="Violencia contra la mujer"~ "Violencia contra las mujeres",
  clasificación=="Violencia de pareja"~ "Violencia de pareja",
  clasificación=="Violencia familiar"~ "Violencia familiar")) %>%
  mutate(
    month=case_when(
      month=="enero" ~ "Enero",
      month=="febrero" ~ "Febrero",
      month=="marzo" ~ "Marzo",
      month=="abril" ~ "Abril",
      month=="mayo" ~ "Mayo",
      month=="junio" ~ "Junio",
      month=="julio" ~ "Julio",
      month=="agosto" ~ "Agosto",
      month=="septiembre" ~ "Septiembre",
      month=="octubre" ~ "Octubre",
      month=="noviembre" ~ "Noviembre",
      month=="diciembre" ~ "Diciembre"),
    month=factor(
      month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                     "Septiembre", "Octubre","Noviembre", "Diciembre"))) %>% 
  select(año, mes, month, municipio, clasificación) 



write.csv(reportes_llamadas, "reportes_llamadas_2.csv")
reportes_llamadas<- read.csv("reportes_llamadas_2.csv") %>% 
  mutate(
    month=case_when(
      month=="enero" ~ "Enero",
      month=="febrero" ~ "Febrero",
      month=="marzo" ~ "Marzo",
      month=="abril" ~ "Abril",
      month=="mayo" ~ "Mayo",
      month=="junio" ~ "Junio",
      month=="julio" ~ "Julio",
      month=="agosto" ~ "Agosto",
      month=="septiembre" ~ "Septiembre",
      month=="octubre" ~ "Octubre",
      month=="noviembre" ~ "Noviembre",
      month=="diciembre" ~ "Diciembre"),
    month=factor(
      month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                     "Septiembre", "Octubre","Noviembre", "Diciembre")))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - #

violencia_familiar_diario <- read_excel("codigo_violeta_bases.xlsx",sheet = "violencia_familiar_diario") %>%
  gather(key = "sexo", value = registro, 4:5) %>%
  mutate(sexo=case_when(
    sexo=="hombre"~"Hombres",
      sexo=="mujer"~"Mujeres"),
    zona=case_when(
      zona=="Distrito 1"~"AVGM",
      zona=="Distrito 8"~"Puerto Vallarta",
      zona=="Estado"~"Estado de Jalisco")) %>%
  filter(registro >=  0)

violencia_familiar_diario$fecha   <-as.POSIXct(violencia_familiar_diario$fecha)
violencia_familiar_diario$fecha   <-as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d")
violencia_familiar_diario$mes     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y-%m")
violencia_familiar_diario$año     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y")
violencia_familiar_diario$month   <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%B")

violencia_familiar_diario<-violencia_familiar_diario %>%
  mutate(
    month=case_when(
      month=="enero" ~ "Enero",
      month=="febrero" ~ "Febrero",
      month=="marzo" ~ "Marzo",
      month=="abril" ~ "Abril",
      month=="mayo" ~ "Mayo",
      month=="junio" ~ "Junio",
      month=="julio" ~ "Julio",
      month=="agosto" ~ "Agosto",
      month=="septiembre" ~ "Septiembre",
      month=="octubre" ~ "Octubre",
      month=="noviembre" ~ "Noviembre",
      month=="diciembre" ~ "Diciembre"),
    month=factor(month,
                 levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio",
                          "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))

 write.csv(violencia_familiar_diario, "violencia_familiar_diario.csv")


reportes_llamadas_2021_2022 <- read_excel("2.reportes_llamadas_2021_2022.xlsx")
reportes_llamadas_2019_2020 <- read_excel("1.reportes_llamadas_2019_2020.xlsx")
reportes_llamadas<-rbind(reportes_llamadas_2019_2020, reportes_llamadas_2021_2022)

setnames(reportes_llamadas, tolower(names(reportes_llamadas)))


reportes_llamadas$fecha <-substr(reportes_llamadas$fecha, start = 1, stop = 10)
reportes_llamadas$fecha   <-as.POSIXct(reportes_llamadas$fecha)
reportes_llamadas$fecha   <-as.Date(reportes_llamadas$fecha,format="%Y-%m-%d")
reportes_llamadas$mes     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y-%m")
reportes_llamadas$año     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y")
reportes_llamadas$month     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%B")

reportes_llamadas <- reportes_llamadas %>%
mutate(clasificación=case_when(
  clasificación=="Violencia contra la mujer"~ "Violencia contra las mujeres",
  clasificación=="Violencia de pareja"~ "Violencia de pareja",
  clasificación=="Violencia familiar"~ "Violencia familiar")) %>%
  mutate(
    month=case_when(
      month=="enero" ~ "Enero",
      month=="febrero" ~ "Febrero",
      month=="marzo" ~ "Marzo",
      month=="abril" ~ "Abril",
      month=="mayo" ~ "Mayo",
      month=="junio" ~ "Junio",
      month=="julio" ~ "Julio",
      month=="agosto" ~ "Agosto",
      month=="septiembre" ~ "Septiembre",
      month=="octubre" ~ "Octubre",
      month=="noviembre" ~ "Noviembre",
      month=="diciembre" ~ "Diciembre"),
    month=factor(
      month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                     "Septiembre", "Octubre","Noviembre", "Diciembre")))



write.csv(reportes_llamadas, "reportes_llamadas_2.csv")
reportes_llamadas<- read.csv("reportes_llamadas_2.csv")



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - #

violencia_familiar_diario <- read_excel("codigo_violeta_bases.xlsx",sheet = "violencia_familiar_diario") %>%
  gather(key = "sexo", value = registro, 4:5) %>%
  mutate(sexo=case_when(
    sexo=="hombre"~"Hombres",
      sexo=="mujer"~"Mujeres"),
    zona=case_when(
      zona=="Distrito 1"~"AVGM",
      zona=="Distrito 8"~"Puerto Vallarta",
      zona=="Estado"~"Estado de Jalisco")) %>%
  filter(registro >=  0)

violencia_familiar_diario$fecha   <-as.POSIXct(violencia_familiar_diario$fecha)
violencia_familiar_diario$fecha   <-as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d")
violencia_familiar_diario$mes     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y-%m")
violencia_familiar_diario$año     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y")
violencia_familiar_diario$month   <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%B")

violencia_familiar_diario<-violencia_familiar_diario %>%
  mutate(
    month=case_when(
      month=="enero" ~ "Enero",
      month=="febrero" ~ "Febrero",
      month=="marzo" ~ "Marzo",
      month=="abril" ~ "Abril",
      month=="mayo" ~ "Mayo",
      month=="junio" ~ "Junio",
      month=="julio" ~ "Julio",
      month=="agosto" ~ "Agosto",
      month=="septiembre" ~ "Septiembre",
      month=="octubre" ~ "Octubre",
      month=="noviembre" ~ "Noviembre",
      month=="diciembre" ~ "Diciembre"),
    month=factor(month,
                 levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio",
                          "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))

 write.csv(violencia_familiar_diario, "violencia_familiar_diario.csv")


 
reportes_llamadas_2021_2022 <- read_excel("2.reportes_llamadas_2021_2022.xlsx")
reportes_llamadas_2019_2020 <- read_excel("1.reportes_llamadas_2019_2020.xlsx")
reportes_llamadas<-rbind(reportes_llamadas_2019_2020, reportes_llamadas_2021_2022)

setnames(reportes_llamadas, tolower(names(reportes_llamadas)))


reportes_llamadas$fecha <-substr(reportes_llamadas$fecha, start = 1, stop = 10)
reportes_llamadas$fecha   <-as.POSIXct(reportes_llamadas$fecha)
reportes_llamadas$fecha   <-as.Date(reportes_llamadas$fecha,format="%Y-%m-%d")
reportes_llamadas$mes     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y-%m")
reportes_llamadas$año     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y")
reportes_llamadas$month     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%B")

reportes_llamadas <- reportes_llamadas %>%
mutate(clasificación=case_when(
  clasificación=="Violencia contra la mujer"~ "Violencia contra las mujeres",
  clasificación=="Violencia de pareja"~ "Violencia de pareja",
  clasificación=="Violencia familiar"~ "Violencia familiar")) %>%
  mutate(
    month=case_when(
      month=="enero" ~ "Enero",
      month=="febrero" ~ "Febrero",
      month=="marzo" ~ "Marzo",
      month=="abril" ~ "Abril",
      month=="mayo" ~ "Mayo",
      month=="junio" ~ "Junio",
      month=="julio" ~ "Julio",
      month=="agosto" ~ "Agosto",
      month=="septiembre" ~ "Septiembre",
      month=="octubre" ~ "Octubre",
      month=="noviembre" ~ "Noviembre",
      month=="diciembre" ~ "Diciembre"),
    month=factor(
      month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                     "Septiembre", "Octubre","Noviembre", "Diciembre")))



write.csv(reportes_llamadas, "reportes_llamadas_2.csv")
reportes_llamadas<- read.csv("reportes_llamadas_2.csv")



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - #

violencia_familiar_diario <- read_excel("codigo_violeta_bases.xlsx",sheet = "violencia_familiar_diario") %>%
  gather(key = "sexo", value = registro, 4:5) %>%
  mutate(sexo=case_when(
    sexo=="hombre"~"Hombres",
      sexo=="mujer"~"Mujeres"),
    zona=case_when(
      zona=="Distrito 1"~"AVGM",
      zona=="Distrito 8"~"Puerto Vallarta",
      zona=="Estado"~"Estado de Jalisco")) %>%
  filter(registro >=  0)

violencia_familiar_diario$fecha   <-as.POSIXct(violencia_familiar_diario$fecha)
violencia_familiar_diario$fecha   <-as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d")
violencia_familiar_diario$mes     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y-%m")
violencia_familiar_diario$año     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y")
violencia_familiar_diario$month   <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%B")

violencia_familiar_diario<-violencia_familiar_diario %>%
  mutate(
    month=case_when(
      month=="enero" ~ "Enero",
      month=="febrero" ~ "Febrero",
      month=="marzo" ~ "Marzo",
      month=="abril" ~ "Abril",
      month=="mayo" ~ "Mayo",
      month=="junio" ~ "Junio",
      month=="julio" ~ "Julio",
      month=="agosto" ~ "Agosto",
      month=="septiembre" ~ "Septiembre",
      month=="octubre" ~ "Octubre",
      month=="noviembre" ~ "Noviembre",
      month=="diciembre" ~ "Diciembre"),
    month=factor(month,
                 levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio",
                          "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))

 write.csv(violencia_familiar_diario, "violencia_familiar_diario.csv")

reportes_llamadas_2021_2022 <- read_excel("2.reportes_llamadas_2021_2022.xlsx")
reportes_llamadas_2019_2020 <- read_excel("1.reportes_llamadas_2019_2020.xlsx")
reportes_llamadas<-rbind(reportes_llamadas_2019_2020, reportes_llamadas_2021_2022)

setnames(reportes_llamadas, tolower(names(reportes_llamadas)))


reportes_llamadas$fecha <-substr(reportes_llamadas$fecha, start = 1, stop = 10)
reportes_llamadas$fecha   <-as.POSIXct(reportes_llamadas$fecha)
reportes_llamadas$fecha   <-as.Date(reportes_llamadas$fecha,format="%Y-%m-%d")
reportes_llamadas$mes     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y-%m")
reportes_llamadas$año     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y")
reportes_llamadas$month     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%B")

reportes_llamadas <- reportes_llamadas %>%
mutate(clasificación=case_when(
  clasificación=="Violencia contra la mujer"~ "Violencia contra las mujeres",
  clasificación=="Violencia de pareja"~ "Violencia de pareja",
  clasificación=="Violencia familiar"~ "Violencia familiar")) %>%
  mutate(
    month=case_when(
      month=="enero" ~ "Enero",
      month=="febrero" ~ "Febrero",
      month=="marzo" ~ "Marzo",
      month=="abril" ~ "Abril",
      month=="mayo" ~ "Mayo",
      month=="junio" ~ "Junio",
      month=="julio" ~ "Julio",
      month=="agosto" ~ "Agosto",
      month=="septiembre" ~ "Septiembre",
      month=="octubre" ~ "Octubre",
      month=="noviembre" ~ "Noviembre",
      month=="diciembre" ~ "Diciembre"),
    month=factor(
      month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                     "Septiembre", "Octubre","Noviembre", "Diciembre")))



write.csv(reportes_llamadas, "reportes_llamadas_2.csv")
reportes_llamadas<- read.csv("reportes_llamadas_2.csv")



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - #

violencia_familiar_diario <- read_excel("codigo_violeta_bases.xlsx",sheet = "violencia_familiar_diario") %>%
  gather(key = "sexo", value = registro, 4:5) %>%
  mutate(sexo=case_when(
    sexo=="hombre"~"Hombres",
      sexo=="mujer"~"Mujeres"),
    zona=case_when(
      zona=="Distrito 1"~"AVGM",
      zona=="Distrito 8"~"Puerto Vallarta",
      zona=="Estado"~"Estado de Jalisco")) %>%
  filter(registro >=  0)

violencia_familiar_diario$fecha   <-as.POSIXct(violencia_familiar_diario$fecha)
violencia_familiar_diario$fecha   <-as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d")
violencia_familiar_diario$mes     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y-%m")
violencia_familiar_diario$año     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y")
violencia_familiar_diario$month   <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%B")

violencia_familiar_diario<-violencia_familiar_diario %>%
  mutate(
    month=case_when(
      month=="enero" ~ "Enero",
      month=="febrero" ~ "Febrero",
      month=="marzo" ~ "Marzo",
      month=="abril" ~ "Abril",
      month=="mayo" ~ "Mayo",
      month=="junio" ~ "Junio",
      month=="julio" ~ "Julio",
      month=="agosto" ~ "Agosto",
      month=="septiembre" ~ "Septiembre",
      month=="octubre" ~ "Octubre",
      month=="noviembre" ~ "Noviembre",
      month=="diciembre" ~ "Diciembre"),
    month=factor(month,
                 levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio",
                          "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))

 write.csv(violencia_familiar_diario, "violencia_familiar_diario.csv")




violencia_familiar_diario<- read.csv("violencia_familiar_diario.csv") %>% 
  mutate(
    month=factor(
      month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                     "Septiembre", "Octubre","Noviembre", "Diciembre")))




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Ordenes y medidas

library(readxl)

medidas_ordenes_estatal <- read_excel("medidas_ordenes.xlsx", sheet = "estatal")
medidas_ordenes_municipal <- read_excel("medidas_ordenes.xlsx", sheet = "municipal") %>% 
  mutate(
    # mes=case_when(
    #   mes=="enero" ~ "Enero",
    #   mes=="febrero" ~ "Febrero",
    #   mes=="marzo" ~ "Marzo",
    #   mes=="abril" ~ "Abril",
    #   mes=="mayo" ~ "Mayo",
    #   mes=="junio" ~ "Junio",
    #   mes=="julio" ~ "Julio",
    #   mes=="agosto" ~ "Agosto",
    #   mes=="septiembre" ~ "septiembre",
    #   mes=="octubre" ~ "Octubre",
    #   mes=="noviembre" ~ "Noviembre",
    #   mes=="diciembre" ~ "Diciembre"),
    mes=factor(mes,
               levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio",
                        "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))



# mapa
#df_mxmunicipio_2020<-data("df_mxmunicipio_2020") 

#data("df_mxmunicipio_2020")

medidas_ordenes_municipal %>% 
  #filter(año==2019) %>%
  group_by(año, municipio) %>% 
  summarise(medidas=sum(medidas_aceptadas + medidas_rechazadas),
            ordenes=sum(ordenes_aceptadas + ordenes_rechazadas)) %>% 
  pivot_longer(cols=c("ordenes","medidas"),
               names_to = "tipo",
               values_to = "total") ->medidas_y_ordenes


medidas_y_ordenes %>%  filter(tipo=="medidas", año==2019)->medidas_2019
medidas_y_ordenes %>%  filter(tipo=="medidas", año==2020)->medidas_2020
medidas_y_ordenes %>%  filter(tipo=="medidas", año==2021)->medidas_2021
medidas_y_ordenes %>%  filter(tipo=="medidas", año==2022)->medidas_2022



# df_mxmunicipio_2020<-df_mxmunicipio_2020%>% filter(state_name=="Jalisco")

medidas_2019 <-merge(df_mxmunicipio_2020, medidas_2019, by.x="municipio_name", by.y="municipio")
medidas_2020 <-merge(df_mxmunicipio_2020, medidas_2020, by.x="municipio_name", by.y="municipio")
medidas_2021 <-merge(df_mxmunicipio_2020, medidas_2021, by.x="municipio_name", by.y="municipio")
medidas_2022 <-merge(df_mxmunicipio_2020, medidas_2022, by.x="municipio_name", by.y="municipio")

medidas_2019$value<- medidas_2019$total
medidas_2020$value<- medidas_2020$total
medidas_2021$value<- medidas_2021$total
medidas_2022$value<- medidas_2022$total

medidas_2019<- medidas_2019 %>% filter(state_name=="Jalisco")
medidas_2020<- medidas_2020 %>% filter(state_name=="Jalisco")
medidas_2021<- medidas_2021 %>% filter(state_name=="Jalisco")
medidas_2022<- medidas_2022 %>% filter(state_name=="Jalisco")

#_______________________________________________________________________________#

medidas_y_ordenes %>%  filter(tipo=="ordenes", año==2019)->ordenes_2019
medidas_y_ordenes %>%  filter(tipo=="ordenes", año==2020)->ordenes_2020
medidas_y_ordenes %>%  filter(tipo=="ordenes", año==2021)->ordenes_2021
medidas_y_ordenes %>%  filter(tipo=="ordenes", año==2022)->ordenes_2022

# df_mxmunicipio_2020<-df_mxmunicipio_2020%>% filter(state_name=="Jalisco")

ordenes_2019 <-merge(df_mxmunicipio_2020, ordenes_2019, by.x="municipio_name", by.y="municipio")
ordenes_2020 <-merge(df_mxmunicipio_2020, ordenes_2020, by.x="municipio_name", by.y="municipio")
ordenes_2021 <-merge(df_mxmunicipio_2020, ordenes_2021, by.x="municipio_name", by.y="municipio")
ordenes_2022 <-merge(df_mxmunicipio_2020, ordenes_2022, by.x="municipio_name", by.y="municipio")

ordenes_2019$value<- ordenes_2019$total
ordenes_2020$value<- ordenes_2020$total
ordenes_2021$value<- ordenes_2021$total
ordenes_2022$value<- ordenes_2022$total

ordenes_2019<- ordenes_2019 %>% filter(state_name=="Jalisco")
ordenes_2020<- ordenes_2020 %>% filter(state_name=="Jalisco")
ordenes_2021<- ordenes_2021 %>% filter(state_name=="Jalisco")
ordenes_2022<- ordenes_2022 %>% filter(state_name=="Jalisco")









# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

`Región Norte`<- c("Bolaños", "Chimaltitán", "Colotlán", "Huejúcar", "Huejuquilla el Alto",
                   "Mezquitic", "San Martín de Bolaños", "Santa María de los Ángeles", "Totatiche", "Villa Guerrero")

`Región Altos Norte`<- c("Encarnación de Díaz", "Lagos de Moreno", "Ojuelos de Jalisco",
                         "San Diego de Alejandría", "San Juan de los Lagos", "Teocaltiche",
                         "Unión de San Antonio", "Villa Hidalgo")

`Región Altos Sur`<- c("Acatic", "Arandas", "Cañadas de Obregón", "Jalostotitlán", "Jesús María",
                       "Mexticacán", "San Ignacio Cerro Gordo", "San Julián", "San Miguel el Alto",
                       "Tepatitlán de Morelos", "Valle de Guadalupe", "Yahualica de González Gallo")

`Región Ciénega`<- c("Atotonilco el Alto", "Ayotlán", "Degollado", "Jamay", "La Barca", "Ocotlán",
                     "Poncitlán", "Tototlán", "Zapotlán del Rey")


`Región Sureste`<- c("Chapala", "Concepción de Buenos Aires", "Jocotepec", "La Manzanilla de la Paz",
                     "Mazamitla", "Quitupan", "Santa María del Oro", "Tizapán el Alto",
                     "Tuxcueca","Valle de Juárez")


`Región Sur`<- c("Gómez Farías", "Jilotlán de los Dolores", "Pihuamo", "San Gabriel",
                 "Tamazula de Gordiano", "Tecalitlán", "Tolimán", "Tonila", "Tuxpan",
                 "Zapotiltic", "Zapotitlán de Vadillo", "Zapotlán el Grande")

`Región Sierra de Amula`<- c("Atengo", "Autlán de Navarro", "Ayutla", "Chiquilistlán",
                             "Cuautla", "Ejutla", "El Grullo", "El Limón", "Juchitlán",
                             "Tecolotlán", "Tenamaxtlán", "Tonaya", "Tuxcacuesco", "Unión de Tula")


`Región Costa Sur`<- c("Casimiro Castillo", "Cihuatlán", "Cuautitlán de García Barragán",
                       "La Huerta", "Tomatlán", "Villa Purificación")



`Región Costa-Sierra Occidental`<- c("Atenguillo", "Cabo Corrientes", "Guachinango",
                                     "Mascota", "Mixtlán", "Puerto Vallarta",
                                     "San Sebastián del Oeste", "Talpa de Allende")



`Región Valles`<- c("Ahualulco de Mercado", "Amatitán", "Ameca", "El Arenal",
                    "Etzatlán", "Hostotipaquillo", "Magdalena",
                    "San Juanito de Escobedo", "San Marcos", "Tala",
                    "Tequila", "Teuchitlán")


`Región Lagunas`<- c("Acatlán de Juárez", "Amacueca", "Atemajac de Brizuela",
                     "Atoyac", "Cocula", "San Martín Hidalgo", "Sayula",
                     "Tapalpa", "Techaluta de Montenegro", "Teocuitatlán de Corona",
                     "Villa Corona", "Zacoalco de Torres")


`Región Centro`<- c("Cuquío", "El Salto", "Guadalajara", "Ixtlahuacán de los Membrillos",
                    "Ixtlahuacán del Río", "Juanacatlán", "San Cristóbal de la Barranca",
                    "San Pedro Tlaquepaque", "Tlajomulco de Zúñiga", "Tonalá", "Zapopan",
                    "Zapotlanejo")




#Carpetas
Regiones<-read.csv("IDM_NM_oct22.csv", encoding="latin1", check.names = T) %>%
  filter(Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual", "Hostigamiento sexual",
                                  "Violación simple", "Violación equiparada", "Feminicidio",
                                  "Violencia familiar",
                                  "Violencia de género en todas sus modalidades distinta a la violencia familiar"),
         Entidad=="Jalisco") %>%
  mutate(Región = case_when(
    Municipio %in% `Región Norte` ~ "Región Norte",
    Municipio %in% `Región Altos Norte` ~ "Región Altos Norte",
    Municipio %in% `Región Altos Sur` ~ "Región Altos Sur",
    Municipio %in% `Región Ciénega` ~ "Región Ciénega",
    Municipio %in% `Región Sureste` ~ "Región Sureste",
    Municipio %in% `Región Sur` ~ "Región Sur",
    Municipio %in% `Región Sierra de Amula` ~ "Región Sierra de Amula",
    Municipio %in% `Región Costa Sur` ~ "Región Costa Sur",
    Municipio %in% `Región Costa-Sierra Occidental` ~ "Región Costa-Sierra Occidental",
    Municipio %in% `Región Valles` ~ "Región Valles",
    Municipio %in% `Región Lagunas` ~ "Región Lagunas",
    Municipio %in% `Región Centro` ~ "Región Centro",
    TRUE~ "No especificado")) %>%
  group_by(Año, Región, Subtipo.de.delito) %>%
  mutate(Subtipo.de.delito=case_when(
    Subtipo.de.delito=="Acoso sexual"~"Acoso sexual",
    Subtipo.de.delito=="Abuso sexual"~"Abuso sexual",
    Subtipo.de.delito=="Violación equiparada"~"Violación",
    Subtipo.de.delito=="Violación simple"~ "Violación",
    Subtipo.de.delito=="Feminicidio"~"Feminicidio",
    Subtipo.de.delito=="Violencia familiar" ~"Violencia familiar",
    Subtipo.de.delito=="Hostigamiento sexual" ~"Hostigamiento sexual")) %>% 
  filter(Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual","Feminicidio", 
                                  "Hostigamiento sexual",
                                  "Violación", "Violencia familiar")) %>% 
  summarise(ene=sum(Enero, na.rm = T),
            feb=sum(Febrero, na.rm = T),
            mar=sum(Marzo, na.rm = T),
            abr=sum(Abril, na.rm = T),
            may=sum(Mayo, na.rm = T),
            jun=sum(Junio, na.rm = T),
            jul=sum(Julio, na.rm = T),
            ago=sum(Agosto, na.rm = T),
            sep=sum(Septiembre, na.rm = T),
            oct=sum(Octubre, na.rm = T),
            nov=sum(Noviembre, na.rm = T),
            dic=sum(Diciembre, na.rm = T),
            Total=sum(ene+ feb+ mar+ abr+
                        may+ jun + jul+ ago+
                        sep+ oct+ nov+ dic))

write.csv(Regiones, "Regiones.csv")

Regiones<-read.csv("Regiones.csv", encoding="latin-1", check.names = T) 


# - - - -  - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - -

municipios_2<-read.csv("IDM_NM_oct22.csv",check.names = T, encoding = "latin1") %>%
  filter(Entidad=="Jalisco",
         Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual", "Hostigamiento sexual",
                                  "Violación simple", "Violación equiparada", "Feminicidio",
                                  "Violencia familiar"))

municipios_2<-municipios_2 %>%
  group_by(Año, Subtipo.de.delito, Municipio) %>%
  summarise(ene=sum(Enero, na.rm = T),
            feb=sum(Febrero, na.rm = T),
            mar=sum(Marzo, na.rm = T),
            abr=sum(Abril, na.rm = T),
            may=sum(Mayo, na.rm = T),
            jun=sum(Junio, na.rm = T),
            jul=sum(Julio, na.rm = T),
            ago=sum(Agosto, na.rm = T),
            sep=sum(Septiembre, na.rm = T),
            oct=sum(Octubre, na.rm = T),
            nov=sum(Noviembre, na.rm = T),
            dic=sum(Diciembre, na.rm = T),
            Total=sum(ene+ feb+ mar+ abr+
                        may+ jun + jul+ ago+
                        sep+ oct+ nov+ dic)) %>% 
  mutate(Subtipo.de.delito=case_when(
    Subtipo.de.delito=="Acoso sexual"~"Acoso sexual",
    Subtipo.de.delito=="Abuso sexual"~"Abuso sexual",
    Subtipo.de.delito=="Violación equiparada"~"Violación",
    Subtipo.de.delito=="Violación simple"~ "Violación",
    Subtipo.de.delito=="Feminicidio"~"Feminicidio",
    Subtipo.de.delito=="Violencia familiar" ~"Violencia familiar",
    Subtipo.de.delito=="Hostigamiento sexual" ~"Hostigamiento sexual")) %>% 
  filter(Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual","Feminicidio", 
                                  "Hostigamiento sexual",
                                  "Violación", "Violencia familiar"))




  Estatal_total<-read.csv("IDM_NM_oct22.csv",check.names = T, encoding = "latin1") %>%   
    mutate(Subtipo.de.delito=case_when(
      Subtipo.de.delito=="Acoso sexual"~"Acoso sexual",
      Subtipo.de.delito=="Abuso sexual"~"Abuso sexual",
      Subtipo.de.delito=="Violación equiparada"~"Violación",
      Subtipo.de.delito=="Violación simple"~ "Violación",
      Subtipo.de.delito=="Feminicidio"~"Feminicidio",
      Subtipo.de.delito=="Violencia familiar" ~"Violencia familiar",
      Subtipo.de.delito=="Hostigamiento sexual" ~"Hostigamiento sexual")) %>% 
  filter(Entidad=="Jalisco",
         Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual","Feminicidio", 
                                  "Hostigamiento sexual","Violación", "Violencia familiar")) %>%

  group_by(Año, Subtipo.de.delito) %>% 
  summarise(ene=sum(Enero, na.rm = T),
   feb=sum(Febrero, na.rm = T),
   mar=sum(Marzo, na.rm = T),
   abr=sum(Abril, na.rm = T),
   may=sum(Mayo, na.rm = T),
   jun=sum(Junio, na.rm = T),
   jul=sum(Julio, na.rm = T),
   ago=sum(Agosto, na.rm = T),
   sep=sum(Septiembre, na.rm = T),
   oct=sum(Octubre, na.rm = T),
   nov=sum(Noviembre, na.rm = T),
   dic=sum(Diciembre, na.rm = T),
   Total=sum(ene+ feb+ mar+ abr+
               may+ jun + jul+ ago+
               sep+ oct+ nov+ dic))

entidad<- c("Estado de Jalisco")
cbind(entidad, Estatal_total)->Estatal_total
names(Estatal_total)[names(Estatal_total) == "...1"] <- "Municipio"
rbind(Estatal_total, municipios_2 )->municipios
write.csv(municipios, "municipios.csv")

municipios<-read.csv("municipios.csv",check.names = T, encoding = "latin-1")



################################################################################
################################################################################

victimas <- read.csv("IDVFC_NM_oct22.csv", encoding="latin1", check.names = T) %>%
  filter(Subtipo.de.delito %in% c("Homicidio doloso", "Feminicidio"),
         Sexo=="Mujer", Entidad=="Jalisco")
write.csv(victimas, "victimas.csv")


victimas<-read.csv("victimas.csv",check.names = T, encoding = "latin-1")

################################################################################

atenciones <- read_excel("atenciones.xlsx"
                         #, 
                         # col_types = c("text", "text", "text", 
                         #               "date", "numeric", "numeric", 
                         #               "numeric", "numeric", "numeric", 
                         #               "numeric", "numeric", "numeric", 
                         #               "numeric", "numeric", "numeric", 
                         #               "numeric", "numeric", "numeric", "numeric", 
                         #               "numeric", "numeric", "numeric", 
                         #               "numeric", "numeric")
) %>% 
  pivot_longer(
    cols=Psicológica:Digital,
    names_to = "tipo", 
    values_to = "total") %>% 
  suppressWarnings()

atenciones$fecha   <-as.POSIXct(atenciones$Fecha) 
atenciones$fecha   <-as.Date(atenciones$Fecha,format="%Y-%m-%d")
atenciones$mes     <-format(as.Date(atenciones$Fecha,format="%Y-%m-%d"), "%Y-%m")
atenciones$año     <-format(as.Date(atenciones$Fecha,format="%Y-%m-%d"), "%Y")
atenciones$month   <-format(as.Date(atenciones$Fecha,format="%Y-%m-%d"), "%B")

atenciones <- atenciones %>% 
  filter(año >= 2022, 
         mes != "2022-11") %>%
  mutate(
    month=case_when(
      month=="enero" ~ "Enero",
      month=="febrero" ~ "Febrero",
      month=="marzo" ~ "Marzo",
      month=="abril" ~ "Abril",
      month=="mayo" ~ "Mayo",
      month=="junio" ~ "Junio",
      month=="julio" ~ "Julio",
      month=="agosto" ~ "Agosto",
      month=="septiembre" ~ "Septiembre",
      month=="octubre" ~ "Octubre",
      month=="noviembre" ~ "Noviembre",
      month=="diciembre" ~ "Diciembre"),
    month=factor(
      month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                     "Septiembre", "Octubre","Noviembre", "Diciembre")))



