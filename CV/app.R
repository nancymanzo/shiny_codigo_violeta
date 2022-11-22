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
library(shinyWidgets)



nb.cols <-13
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)


nb.cols.129 <-129
mycolors129 <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols.129)


nb.cols_2 <- 20
bupu <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols_2)


nb.cols_2 <- 20
bupu_2 <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols_2)


# drive_find(n_max = 6)
# 1
# 
# drive_download("codigo_violeta_bases.xlsx",overwrite = TRUE)
# drive_download("2.reportes_llamadas_2021_2022.xlsx",overwrite = TRUE)
# drive_download("1.reportes_llamadas_2019_2020.xlsx",overwrite = TRUE)
# drive_download("medidas_ordenes",overwrite = TRUE)
# drive_download("atenciones.xlsx",overwrite = TRUE)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# reportes_llamadas_2021_2022 <- read_excel("2.reportes_llamadas_2021_2022.xlsx")
# reportes_llamadas_2019_2020 <- read_excel("1.reportes_llamadas_2019_2020.xlsx")
# reportes_llamadas<-rbind(reportes_llamadas_2019_2020, reportes_llamadas_2021_2022)
# 
# setnames(reportes_llamadas, tolower(names(reportes_llamadas)))
# 
# 
# reportes_llamadas$fecha <-substr(reportes_llamadas$fecha, start = 1, stop = 10)
# reportes_llamadas$fecha   <-as.POSIXct(reportes_llamadas$fecha)
# reportes_llamadas$fecha   <-as.Date(reportes_llamadas$fecha,format="%Y-%m-%d")
# reportes_llamadas$mes     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y-%m")
# reportes_llamadas$año     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y")
# reportes_llamadas$month     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%B")
# 
# reportes_llamadas <- reportes_llamadas %>%
# mutate(clasificación=case_when(
#   clasificación=="Violencia contra la mujer"~ "Violencia contra las mujeres",
#   clasificación=="Violencia de pareja"~ "Violencia de pareja",
#   clasificación=="Violencia familiar"~ "Violencia familiar")) %>%
#   mutate(
#     month=case_when(
#       month=="enero" ~ "Enero",
#       month=="febrero" ~ "Febrero",
#       month=="marzo" ~ "Marzo",
#       month=="abril" ~ "Abril",
#       month=="mayo" ~ "Mayo",
#       month=="junio" ~ "Junio",
#       month=="julio" ~ "Julio",
#       month=="agosto" ~ "Agosto",
#       month=="septiembre" ~ "Septiembre",
#       month=="octubre" ~ "Octubre",
#       month=="noviembre" ~ "Noviembre",
#       month=="diciembre" ~ "Diciembre"),
#     month=factor(
#       month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                      "Septiembre", "Octubre","Noviembre", "Diciembre")))
# 
# 
# 
# write.csv(reportes_llamadas, "reportes_llamadas_2.csv")


reportes_llamadas<- read.csv("reportes_llamadas_2.csv")



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - #

# violencia_familiar_diario <- read_excel("codigo_violeta_bases.xlsx",sheet = "violencia_familiar_diario") %>%
#   gather(key = "sexo", value = registro, 4:5) %>%
#   mutate(sexo=case_when(
#     sexo=="hombre"~"Hombres",
#       sexo=="mujer"~"Mujeres"),
#     zona=case_when(
#       zona=="Distrito 1"~"AVGM",
#       zona=="Distrito 8"~"Puerto Vallarta",
#       zona=="Estado"~"Estado de Jalisco")) %>%
#   filter(registro >=  0)
# 
# violencia_familiar_diario$fecha   <-as.POSIXct(violencia_familiar_diario$fecha)
# violencia_familiar_diario$fecha   <-as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d")
# violencia_familiar_diario$mes     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y-%m")
# violencia_familiar_diario$año     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y")
# violencia_familiar_diario$month   <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%B")
# 
# violencia_familiar_diario<-violencia_familiar_diario %>%
#   mutate(
#     month=case_when(
#       month=="enero" ~ "Enero",
#       month=="febrero" ~ "Febrero",
#       month=="marzo" ~ "Marzo",
#       month=="abril" ~ "Abril",
#       month=="mayo" ~ "Mayo",
#       month=="junio" ~ "Junio",
#       month=="julio" ~ "Julio",
#       month=="agosto" ~ "Agosto",
#       month=="septiembre" ~ "Septiembre",
#       month=="octubre" ~ "Octubre",
#       month=="noviembre" ~ "Noviembre",
#       month=="diciembre" ~ "Diciembre"),
#     month=factor(month,
#                  levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio",
#                           "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))
# 
#  write.csv(violencia_familiar_diario, "violencia_familiar_diario.csv")


# reportes_llamadas_2021_2022 <- read_excel("2.reportes_llamadas_2021_2022.xlsx")
# reportes_llamadas_2019_2020 <- read_excel("1.reportes_llamadas_2019_2020.xlsx")
# reportes_llamadas<-rbind(reportes_llamadas_2019_2020, reportes_llamadas_2021_2022)
# 
# setnames(reportes_llamadas, tolower(names(reportes_llamadas)))
# 
# 
# reportes_llamadas$fecha <-substr(reportes_llamadas$fecha, start = 1, stop = 10)
# reportes_llamadas$fecha   <-as.POSIXct(reportes_llamadas$fecha)
# reportes_llamadas$fecha   <-as.Date(reportes_llamadas$fecha,format="%Y-%m-%d")
# reportes_llamadas$mes     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y-%m")
# reportes_llamadas$año     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y")
# reportes_llamadas$month     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%B")
# 
# reportes_llamadas <- reportes_llamadas %>%
# mutate(clasificación=case_when(
#   clasificación=="Violencia contra la mujer"~ "Violencia contra las mujeres",
#   clasificación=="Violencia de pareja"~ "Violencia de pareja",
#   clasificación=="Violencia familiar"~ "Violencia familiar")) %>%
#   mutate(
#     month=case_when(
#       month=="enero" ~ "Enero",
#       month=="febrero" ~ "Febrero",
#       month=="marzo" ~ "Marzo",
#       month=="abril" ~ "Abril",
#       month=="mayo" ~ "Mayo",
#       month=="junio" ~ "Junio",
#       month=="julio" ~ "Julio",
#       month=="agosto" ~ "Agosto",
#       month=="septiembre" ~ "Septiembre",
#       month=="octubre" ~ "Octubre",
#       month=="noviembre" ~ "Noviembre",
#       month=="diciembre" ~ "Diciembre"),
#     month=factor(
#       month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                      "Septiembre", "Octubre","Noviembre", "Diciembre")))
# 
# 
# 
# write.csv(reportes_llamadas, "reportes_llamadas_2.csv")


reportes_llamadas<- read.csv("reportes_llamadas_2.csv") %>% 
  select(año, mes, municipio, clasificación)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - #

# violencia_familiar_diario <- read_excel("codigo_violeta_bases.xlsx",sheet = "violencia_familiar_diario") %>%
#   gather(key = "sexo", value = registro, 4:5) %>%
#   mutate(sexo=case_when(
#     sexo=="hombre"~"Hombres",
#       sexo=="mujer"~"Mujeres"),
#     zona=case_when(
#       zona=="Distrito 1"~"AVGM",
#       zona=="Distrito 8"~"Puerto Vallarta",
#       zona=="Estado"~"Estado de Jalisco")) %>%
#   filter(registro >=  0)
# 
# violencia_familiar_diario$fecha   <-as.POSIXct(violencia_familiar_diario$fecha)
# violencia_familiar_diario$fecha   <-as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d")
# violencia_familiar_diario$mes     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y-%m")
# violencia_familiar_diario$año     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y")
# violencia_familiar_diario$month   <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%B")
# 
# violencia_familiar_diario<-violencia_familiar_diario %>%
#   mutate(
#     month=case_when(
#       month=="enero" ~ "Enero",
#       month=="febrero" ~ "Febrero",
#       month=="marzo" ~ "Marzo",
#       month=="abril" ~ "Abril",
#       month=="mayo" ~ "Mayo",
#       month=="junio" ~ "Junio",
#       month=="julio" ~ "Julio",
#       month=="agosto" ~ "Agosto",
#       month=="septiembre" ~ "Septiembre",
#       month=="octubre" ~ "Octubre",
#       month=="noviembre" ~ "Noviembre",
#       month=="diciembre" ~ "Diciembre"),
#     month=factor(month,
#                  levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio",
#                           "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))
# 
#  write.csv(violencia_familiar_diario, "violencia_familiar_diario.csv")

# reportes_llamadas_2021_2022 <- read_excel("2.reportes_llamadas_2021_2022.xlsx")
# reportes_llamadas_2019_2020 <- read_excel("1.reportes_llamadas_2019_2020.xlsx")
# reportes_llamadas<-rbind(reportes_llamadas_2019_2020, reportes_llamadas_2021_2022)
# 
# setnames(reportes_llamadas, tolower(names(reportes_llamadas)))
# 
# 
# reportes_llamadas$fecha <-substr(reportes_llamadas$fecha, start = 1, stop = 10)
# reportes_llamadas$fecha   <-as.POSIXct(reportes_llamadas$fecha)
# reportes_llamadas$fecha   <-as.Date(reportes_llamadas$fecha,format="%Y-%m-%d")
# reportes_llamadas$mes     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y-%m")
# reportes_llamadas$año     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y")
# reportes_llamadas$month     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%B")
# 
# reportes_llamadas <- reportes_llamadas %>%
# mutate(clasificación=case_when(
#   clasificación=="Violencia contra la mujer"~ "Violencia contra las mujeres",
#   clasificación=="Violencia de pareja"~ "Violencia de pareja",
#   clasificación=="Violencia familiar"~ "Violencia familiar")) %>%
#   mutate(
#     month=case_when(
#       month=="enero" ~ "Enero",
#       month=="febrero" ~ "Febrero",
#       month=="marzo" ~ "Marzo",
#       month=="abril" ~ "Abril",
#       month=="mayo" ~ "Mayo",
#       month=="junio" ~ "Junio",
#       month=="julio" ~ "Julio",
#       month=="agosto" ~ "Agosto",
#       month=="septiembre" ~ "Septiembre",
#       month=="octubre" ~ "Octubre",
#       month=="noviembre" ~ "Noviembre",
#       month=="diciembre" ~ "Diciembre"),
#     month=factor(
#       month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                      "Septiembre", "Octubre","Noviembre", "Diciembre")))
# 
# 
# 
# write.csv(reportes_llamadas, "reportes_llamadas_2.csv")


#reportes_llamadas<- read.csv("reportes_llamadas_2.csv")



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - #

# violencia_familiar_diario <- read_excel("codigo_violeta_bases.xlsx",sheet = "violencia_familiar_diario") %>%
#   gather(key = "sexo", value = registro, 4:5) %>%
#   mutate(sexo=case_when(
#     sexo=="hombre"~"Hombres",
#       sexo=="mujer"~"Mujeres"),
#     zona=case_when(
#       zona=="Distrito 1"~"AVGM",
#       zona=="Distrito 8"~"Puerto Vallarta",
#       zona=="Estado"~"Estado de Jalisco")) %>%
#   filter(registro >=  0)
# 
# violencia_familiar_diario$fecha   <-as.POSIXct(violencia_familiar_diario$fecha)
# violencia_familiar_diario$fecha   <-as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d")
# violencia_familiar_diario$mes     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y-%m")
# violencia_familiar_diario$año     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y")
# violencia_familiar_diario$month   <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%B")
# 
# violencia_familiar_diario<-violencia_familiar_diario %>%
#   mutate(
#     month=case_when(
#       month=="enero" ~ "Enero",
#       month=="febrero" ~ "Febrero",
#       month=="marzo" ~ "Marzo",
#       month=="abril" ~ "Abril",
#       month=="mayo" ~ "Mayo",
#       month=="junio" ~ "Junio",
#       month=="julio" ~ "Julio",
#       month=="agosto" ~ "Agosto",
#       month=="septiembre" ~ "Septiembre",
#       month=="octubre" ~ "Octubre",
#       month=="noviembre" ~ "Noviembre",
#       month=="diciembre" ~ "Diciembre"),
#     month=factor(month,
#                  levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio",
#                           "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))
# 
#  write.csv(violencia_familiar_diario, "violencia_familiar_diario.csv")

# reportes_llamadas_2021_2022 <- read_excel("2.reportes_llamadas_2021_2022.xlsx")
# reportes_llamadas_2019_2020 <- read_excel("1.reportes_llamadas_2019_2020.xlsx")
# reportes_llamadas<-rbind(reportes_llamadas_2019_2020, reportes_llamadas_2021_2022)
# 
# setnames(reportes_llamadas, tolower(names(reportes_llamadas)))
# 
# 
# reportes_llamadas$fecha <-substr(reportes_llamadas$fecha, start = 1, stop = 10)
# reportes_llamadas$fecha   <-as.POSIXct(reportes_llamadas$fecha)
# reportes_llamadas$fecha   <-as.Date(reportes_llamadas$fecha,format="%Y-%m-%d")
# reportes_llamadas$mes     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y-%m")
# reportes_llamadas$año     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%Y")
# reportes_llamadas$month     <-format(as.Date(reportes_llamadas$fecha,format="%Y-%m-%d"), "%B")
# 
# reportes_llamadas <- reportes_llamadas %>%
# mutate(clasificación=case_when(
#   clasificación=="Violencia contra la mujer"~ "Violencia contra las mujeres",
#   clasificación=="Violencia de pareja"~ "Violencia de pareja",
#   clasificación=="Violencia familiar"~ "Violencia familiar")) %>%
#   mutate(
#     month=case_when(
#       month=="enero" ~ "Enero",
#       month=="febrero" ~ "Febrero",
#       month=="marzo" ~ "Marzo",
#       month=="abril" ~ "Abril",
#       month=="mayo" ~ "Mayo",
#       month=="junio" ~ "Junio",
#       month=="julio" ~ "Julio",
#       month=="agosto" ~ "Agosto",
#       month=="septiembre" ~ "Septiembre",
#       month=="octubre" ~ "Octubre",
#       month=="noviembre" ~ "Noviembre",
#       month=="diciembre" ~ "Diciembre"),
#     month=factor(
#       month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
#                      "Septiembre", "Octubre","Noviembre", "Diciembre")))
# 
# 
# 
# write.csv(reportes_llamadas, "reportes_llamadas_2.csv")


reportes_llamadas<- read.csv("reportes_llamadas_2.csv") %>% 
  mutate(
    month=factor(
      month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                     "Septiembre", "Octubre","Noviembre", "Diciembre")))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - #

# violencia_familiar_diario <- read_excel("codigo_violeta_bases.xlsx",sheet = "violencia_familiar_diario") %>%
#   gather(key = "sexo", value = registro, 4:5) %>%
#   mutate(sexo=case_when(
#     sexo=="hombre"~"Hombres",
#       sexo=="mujer"~"Mujeres"),
#     zona=case_when(
#       zona=="Distrito 1"~"AVGM",
#       zona=="Distrito 8"~"Puerto Vallarta",
#       zona=="Estado"~"Estado de Jalisco")) %>%
#   filter(registro >=  0)
# 
# violencia_familiar_diario$fecha   <-as.POSIXct(violencia_familiar_diario$fecha)
# violencia_familiar_diario$fecha   <-as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d")
# violencia_familiar_diario$mes     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y-%m")
# violencia_familiar_diario$año     <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%Y")
# violencia_familiar_diario$month   <-format(as.Date(violencia_familiar_diario$fecha,format="%Y-%m-%d"), "%B")
# 
# violencia_familiar_diario<-violencia_familiar_diario %>%
#   mutate(
#     month=case_when(
#       month=="enero" ~ "Enero",
#       month=="febrero" ~ "Febrero",
#       month=="marzo" ~ "Marzo",
#       month=="abril" ~ "Abril",
#       month=="mayo" ~ "Mayo",
#       month=="junio" ~ "Junio",
#       month=="julio" ~ "Julio",
#       month=="agosto" ~ "Agosto",
#       month=="septiembre" ~ "Septiembre",
#       month=="octubre" ~ "Octubre",
#       month=="noviembre" ~ "Noviembre",
#       month=="diciembre" ~ "Diciembre"),
#     month=factor(month,
#                  levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio",
#                           "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))
# 
#  write.csv(violencia_familiar_diario, "violencia_familiar_diario.csv")




violencia_familiar_diario<- read.csv("violencia_familiar_diario.csv") %>% 
  mutate(
    month=factor(
      month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                     "Septiembre", "Octubre","Noviembre", "Diciembre"))) %>% 
  select(año, mes, zona, sexo, registro, month)




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

data("df_mxmunicipio_2020")

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

# `Región Norte`<- c("Bolaños", "Chimaltitán", "Colotlán", "Huejúcar", "Huejuquilla el Alto",
#                    "Mezquitic", "San Martín de Bolaños", "Santa María de los Ángeles", "Totatiche", "Villa Guerrero")
# 
# `Región Altos Norte`<- c("Encarnación de Díaz", "Lagos de Moreno", "Ojuelos de Jalisco",
#                          "San Diego de Alejandría", "San Juan de los Lagos", "Teocaltiche",
#                          "Unión de San Antonio", "Villa Hidalgo")
# 
# `Región Altos Sur`<- c("Acatic", "Arandas", "Cañadas de Obregón", "Jalostotitlán", "Jesús María",
#                        "Mexticacán", "San Ignacio Cerro Gordo", "San Julián", "San Miguel el Alto",
#                        "Tepatitlán de Morelos", "Valle de Guadalupe", "Yahualica de González Gallo")
# 
# `Región Ciénega`<- c("Atotonilco el Alto", "Ayotlán", "Degollado", "Jamay", "La Barca", "Ocotlán",
#                      "Poncitlán", "Tototlán", "Zapotlán del Rey")
# 
# 
# `Región Sureste`<- c("Chapala", "Concepción de Buenos Aires", "Jocotepec", "La Manzanilla de la Paz",
#                      "Mazamitla", "Quitupan", "Santa María del Oro", "Tizapán el Alto",
#                      "Tuxcueca","Valle de Juárez")
# 
# 
# `Región Sur`<- c("Gómez Farías", "Jilotlán de los Dolores", "Pihuamo", "San Gabriel",
#                  "Tamazula de Gordiano", "Tecalitlán", "Tolimán", "Tonila", "Tuxpan",
#                  "Zapotiltic", "Zapotitlán de Vadillo", "Zapotlán el Grande")
# 
# `Región Sierra de Amula`<- c("Atengo", "Autlán de Navarro", "Ayutla", "Chiquilistlán",
#                              "Cuautla", "Ejutla", "El Grullo", "El Limón", "Juchitlán",
#                              "Tecolotlán", "Tenamaxtlán", "Tonaya", "Tuxcacuesco", "Unión de Tula")
# 
# 
# `Región Costa Sur`<- c("Casimiro Castillo", "Cihuatlán", "Cuautitlán de García Barragán",
#                        "La Huerta", "Tomatlán", "Villa Purificación")
# 
# 
# 
# `Región Costa-Sierra Occidental`<- c("Atenguillo", "Cabo Corrientes", "Guachinango",
#                                      "Mascota", "Mixtlán", "Puerto Vallarta",
#                                      "San Sebastián del Oeste", "Talpa de Allende")
# 
# 
# 
# `Región Valles`<- c("Ahualulco de Mercado", "Amatitán", "Ameca", "El Arenal",
#                     "Etzatlán", "Hostotipaquillo", "Magdalena",
#                     "San Juanito de Escobedo", "San Marcos", "Tala",
#                     "Tequila", "Teuchitlán")
# 
# 
# `Región Lagunas`<- c("Acatlán de Juárez", "Amacueca", "Atemajac de Brizuela",
#                      "Atoyac", "Cocula", "San Martín Hidalgo", "Sayula",
#                      "Tapalpa", "Techaluta de Montenegro", "Teocuitatlán de Corona",
#                      "Villa Corona", "Zacoalco de Torres")
# 
# 
# `Región Centro`<- c("Cuquío", "El Salto", "Guadalajara", "Ixtlahuacán de los Membrillos",
#                     "Ixtlahuacán del Río", "Juanacatlán", "San Cristóbal de la Barranca",
#                     "San Pedro Tlaquepaque", "Tlajomulco de Zúñiga", "Tonalá", "Zapopan",
#                     "Zapotlanejo")
# 
# 
# 
# 
# #Carpetas
# Regiones<-read.csv("IDM_NM_oct22.csv", encoding="latin1", check.names = T) %>%
#   filter(Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual", "Hostigamiento sexual",
#                                   "Violación simple", "Violación equiparada", "Feminicidio",
#                                   "Violencia familiar",
#                                   "Violencia de género en todas sus modalidades distinta a la violencia familiar"),
#          Entidad=="Jalisco") %>%
#   mutate(Región = case_when(
#     Municipio %in% `Región Norte` ~ "Región Norte",
#     Municipio %in% `Región Altos Norte` ~ "Región Altos Norte",
#     Municipio %in% `Región Altos Sur` ~ "Región Altos Sur",
#     Municipio %in% `Región Ciénega` ~ "Región Ciénega",
#     Municipio %in% `Región Sureste` ~ "Región Sureste",
#     Municipio %in% `Región Sur` ~ "Región Sur",
#     Municipio %in% `Región Sierra de Amula` ~ "Región Sierra de Amula",
#     Municipio %in% `Región Costa Sur` ~ "Región Costa Sur",
#     Municipio %in% `Región Costa-Sierra Occidental` ~ "Región Costa-Sierra Occidental",
#     Municipio %in% `Región Valles` ~ "Región Valles",
#     Municipio %in% `Región Lagunas` ~ "Región Lagunas",
#     Municipio %in% `Región Centro` ~ "Región Centro")) %>%
#   group_by(Año, Región, Subtipo.de.delito) %>%
#   summarise(ene=sum(Enero, na.rm = T),
#             feb=sum(Febrero, na.rm = T),
#             mar=sum(Marzo, na.rm = T),
#             abr=sum(Abril, na.rm = T),
#             may=sum(Mayo, na.rm = T),
#             jun=sum(Junio, na.rm = T),
#             jul=sum(Julio, na.rm = T),
#             ago=sum(Agosto, na.rm = T),
#             sep=sum(Septiembre, na.rm = T),
#             oct=sum(Octubre, na.rm = T),
#             nov=sum(Noviembre, na.rm = T),
#             dic=sum(Diciembre, na.rm = T),
#             Total=sum(ene+ feb+ mar+ abr+
#                         may+ jun + jul+ ago+
#                         sep+ oct+ nov+ dic))
# 
# write.csv(Regiones, "Regiones.csv")

Regiones<-read.csv("Regiones.csv", encoding="latin-1", check.names = T)

Regiones %>% 
  pivot_longer(cols = ene:dic,
               names_to = "mes",
               values_to = "total") %>% 
  mutate(mes=case_when(
    mes=="ene"~1,
    mes=="feb"~2,
    mes=="mar"~3,
    mes=="abr"~4,
    mes=="may"~5,
    mes=="jun"~6,
    mes=="jul"~7,
    mes=="ago"~8,
    mes=="sep"~9,
    mes=="oct"~10,
    mes=="nov"~11,
    mes=="dic"~12)
    # ,
    # mes=factor(mes,
    #            levels=c("Enero", "Febrero", "Marzo",
    #                     "Abril", "Mayo", "Junio",
    #                     "Julio", "Agosto", "Septiembre",
    #                     "Octubre", "Noviembre", "Diciembre"))
  ) %>%
  mutate(Periodo = ymd(paste0(Año, "-", mes, "-01"))) %>% 
  mutate(text = paste("Año: ", Año,
                      "\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
                      "\nTotal de carpetas: ", scales::comma(total), sep="")) %>% 
  filter(Periodo <= "2022-10-01")->Regiones

#Regiones$Periodo<-substr(Regiones$Periodo, start = 1, stop = 7)


# - - - -  - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - -

municipios_2<-read.csv("municipios.csv") 

# municipios_2<-read.csv("IDM_NM_oct22.csv",check.names = T, encoding = "latin1") %>%
#   mutate(Subtipo.de.delito=case_when(
#     Subtipo.de.delito=="Acoso sexual"~"Acoso sexual",
#     Subtipo.de.delito=="Abuso sexual"~"Abuso sexual",
#     Subtipo.de.delito=="Violación equiparada"~"Violación",
#     Subtipo.de.delito=="Violación simple"~ "Violación",
#     Subtipo.de.delito=="Feminicidio"~"Feminicidio",
#     Subtipo.de.delito=="Violencia familiar" ~"Violencia familiar",
#     Subtipo.de.delito=="Hostigamiento sexual" ~"Hostigamiento sexual")) %>% 
#   filter(Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual","Feminicidio", 
#                                   "Hostigamiento sexual",
#                                   "Violación", "Violencia familiar"))

municipios_2<-municipios_2 %>%
  group_by(Año, Subtipo.de.delito, Municipio) %>%
  summarise(ene=sum(ene, na.rm = T),
            feb=sum(feb, na.rm = T),
            mar=sum(mar, na.rm = T),
            abr=sum(abr, na.rm = T),
            may=sum(may, na.rm = T),
            jun=sum(jun, na.rm = T),
            jul=sum(jul, na.rm = T),
            ago=sum(ago, na.rm = T),
            sep=sum(sep, na.rm = T),
            oct=sum(oct, na.rm = T),
            nov=sum(nov, na.rm = T),
            dic=sum(dic, na.rm = T),
            Total=sum(ene+ feb+ mar+ abr+
                        may+ jun + jul+ ago+
                        sep+ oct+ nov+ dic))




# read.csv("IDM_NM_oct22.csv",check.names = T, encoding = "latin1") %>%
#   filter(Entidad=="Jalisco",
#          Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual", "Hostigamiento sexual",
#                                   "Violación simple", "Violación equiparada", "Feminicidio",
#                                   "Violencia familiar",
#                                   "Violencia de género en todas sus modalidades distinta a la violencia familiar")) %>%
#   group_by(Año, Subtipo.de.delito) %>% summarise(ene=sum(Enero, na.rm = T),
#                                                  feb=sum(Febrero, na.rm = T),
#                                                  mar=sum(Marzo, na.rm = T),
#                                                  abr=sum(Abril, na.rm = T),
#                                                  may=sum(Mayo, na.rm = T),
#                                                  jun=sum(Junio, na.rm = T),
#                                                  jul=sum(Julio, na.rm = T),
#                                                  ago=sum(Agosto, na.rm = T),
#                                                  sep=sum(Septiembre, na.rm = T),
#                                                  oct=sum(Octubre, na.rm = T),
#                                                  nov=sum(Noviembre, na.rm = T),
#                                                  dic=sum(Diciembre, na.rm = T),
#                                                  Total=sum(ene+ feb+ mar+ abr+
#                                                              may+ jun + jul+ ago+
#                                                              sep+ oct+ nov+ dic))-> Estatal_total
# entidad<- c("Estado de Jalisco")
# cbind(entidad, Estatal_total)->Estatal_total
# names(Estatal_total)[names(Estatal_total) == "...1"] <- "Municipio"
# rbind(Estatal_total, municipios_2 )->municipios
# write.csv(municipios, "municipios.csv")

municipios<-read.csv("municipios.csv",check.names = T, encoding = "latin-1") %>% 
  # mutate(Subtipo.de.delito=case_when(
  #   Subtipo.de.delito=="Acoso sexual"~"Acoso sexual",
  #   Subtipo.de.delito=="Abuso sexual"~"Abuso sexual",
  #   Subtipo.de.delito=="Violación equiparada"~"Violación",
  #   Subtipo.de.delito=="Violación simple"~ "Violación",
  #   Subtipo.de.delito=="Feminicidio"~"Feminicidio",
  #   Subtipo.de.delito=="Violencia familiar" ~"Violencia familiar",
  #   Subtipo.de.delito=="Hostigamiento sexual" ~"Hostigamiento sexual")) %>% 
  # filter(Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual","Feminicidio", 
  #                                 "Hostigamiento sexual",
  #                                 "Violación", "Violencia familiar")) %>% 
  pivot_longer(cols = ene:dic,
               names_to = "mes",
               values_to = "total") %>% 
  mutate(mes=case_when(
    mes=="ene"~1,
    mes=="feb"~2,
    mes=="mar"~3,
    mes=="abr"~4,
    mes=="may"~5,
    mes=="jun"~6,
    mes=="jul"~7,
    mes=="ago"~8,
    mes=="sep"~9,
    mes=="oct"~10,
    mes=="nov"~11,
    mes=="dic"~12)
    # ,
    # mes=factor(mes,
    #            levels=c("Enero", "Febrero", "Marzo",
    #                     "Abril", "Mayo", "Junio",
    #                     "Julio", "Agosto", "Septiembre",
    #                     "Octubre", "Noviembre", "Diciembre"))
  ) %>%
  mutate(Periodo = ymd(paste0(Año, "-", mes, "-01"))) %>% 
  mutate(text = paste("Año: ", Año,
                      "\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
                      "\nTotal de carpetas: ", scales::comma(total), sep="")) %>% 
  filter(Periodo <= "2022-10-01")



################################################################################
################################################################################

# victimas <- read.csv("IDVFC_NM_oct22.csv", encoding="latin1", check.names = T) %>%
#   filter(Subtipo.de.delito %in% c("Homicidio doloso", "Feminicidio"),
#          Sexo=="Mujer", Entidad=="Jalisco")
# write.csv(victimas, "victimas.csv")


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
         mes <="2022-11-01") %>%
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





################################################################################


ui <- shinyUI(
  tagList(
    includeCSS("./www/style.css"),
    fluidPage(shinythemes::themeSelector(),

      add_busy_spinner(onstart = F, spin = "fading-circle", color = "#E34F70"),
      
      navbarPage(title = "DATOS ABIERTOS", 
                 header=
                   busy_start_up(
                     loader = spin_epic("flower", color = "#7e3794"),
                     text = "Cargando",
                     timeout = 1500,
                     color = "#7e3794",
                     background = " white"
                   ),
                 useShinydashboard(),
                 navbarMenu(title = "CÓDIGO VIOLETA", #icon = icon("dot-circle"),
                            tabPanel(title = "Reportes 911",
                                     #tabsetPanel(
                                        box(
                                          width=12,  
                                          valueBox("En promedio", "se atienden 6,675 llamadas al mes relacionadas a violencias por razón de género y 222 al día.",icon=icon("chart-area"),color="fuchsia"),
                                          valueBox("Máximo histórico", "Marzo es el mes con mayor registro de llamadas: 8,986 y 8,714 en 2021 y 2022 respectivamente.", icon=icon("equals"), color="purple"),
                                          valueBox("90,435", "Es el record de llamadas anuales para 2021. De enero a septiembre 2022 se contabilizan 65,478.", icon=icon("wave-square"), color="maroon"),
                                          
                                       sidebarLayout(
                                         sidebarPanel("Seleccione algunas características",
                                                     selectInput(
                                                        inputId = "llamadas_año",
                                                        label = "Año",
                                                        choices = unique(sort(reportes_llamadas$año)),
                                                        multiple = T
                                                      ),
                                                      selectInput(
                                                        inputId = "llamadas_month",
                                                        label = "Mes",
                                                        choices = unique(sort(reportes_llamadas$month)),
                                                        multiple = TRUE
                                                      ),
                                                      selectInput(
                                                        inputId = "llamadas_clasificacion",
                                                        label = "Tipo de violencia",
                                                        choices = unique(sort(reportes_llamadas$clasificación)),
                                                        multiple = TRUE
                                                      ),
                                                      selectInput(
                                                        inputId = "llamadas_municipio",
                                                        label = "Municipio",
                                                        choices = unique(sort(reportes_llamadas$municipio)),
                                                        multiple = T,
                                                      ),  
                                                      downloadButton("downloadData_llamadas", "Descarga (.csv)")
                                                     ),
                                         mainPanel(#h3(align="center", "Llamadas al 911 por violencia contra las mujeres",
                                                      plotlyOutput("grafico_llamadas",  height = "auto", width = "auto"),
                                                      h6("Fuente: Datos proporcionados por Escudo Urbano C5."),
                                                      h6("Los registro del ´Reporte al 911´son las llamadas que se contabilizan por motivo de violencia contra las mujeres.
                                                      Se clasifican las violencias en tres: 1) violencia contra las mujeres, 2) violencia de pareja y 3) violencia familiar."))
                                         ))#)
                                     
                            ),
                            tabPanel(title = "Violencia familiar",
                                     #tabsetPanel(
                                     box(
                                       width=12,  
                                       valueBox("En promedio", "se atienden 1,048 denuncias por violencia familiar, 17 al día.",icon=icon("chart-area"),color="fuchsia"),
                                       valueBox("9 de cada 10", "denuncias por violencia familiar fueron realizadas por mujeres.", icon=icon("equals"), color="purple"),
                                       valueBox("De enero a noviembre 2022", "se han contabilizado 11,820 denuncias.", icon=icon("wave-square"), color="maroon"), #En 2021 el total fue de 12,783 y en 2020 de 10,003
                                       
                                       sidebarLayout(
                                         sidebarPanel("Seleccione algunas características",
                                                      selectInput(
                                                        inputId = "violencia_familiar_año",
                                                        label = "Año",
                                                        choices = unique(sort(violencia_familiar_diario$año)),
                                                        multiple = T
                                                      ),
                                                      selectInput(
                                                        inputId = "violencia_familiar_month",
                                                        label = "Mes",
                                                        choices = unique(sort(violencia_familiar_diario$month)),
                                                        multiple = TRUE
                                                      ),
                                                      selectInput(
                                                        inputId = "violencia_familiar_sexo",
                                                        label = "Sexo",
                                                        choices = unique(sort(violencia_familiar_diario$sexo)),
                                                        multiple = TRUE
                                                      ),
                                                      selectInput(
                                                        inputId = "violencia_familiar_zona",
                                                        label = "Zona",
                                                        choices = unique(sort(violencia_familiar_diario$zona)),
                                                        multiple = TRUE,
                                                        selected = "Estado de Jalisco"
                                                      ),
                                                      downloadButton("download_violencia_familiar", "Descarga (.csv)")
                                                      ),
                                         mainPanel(plotlyOutput("grafico_violencia_familiar",  height = "auto", #width = "auto"
                                         ),
                                         h6("Fuente: Elaborado con datos de la Fiscalía Estatal."))
                                       ))),
                            
                            # - - - - - - - - -- - - - - - - - - - - - - - - - - - 
                            tabPanel(title = "Medidas de protección",
                                     fluidRow(width=12,  
                                              #h3(align="center", "Medidas de protección trabajadas"),
                                              #box(width=12,
                                                  tabsetPanel(
                                                    box(
                                                      width=12,  
                                                      valueBox("2022 (ene-sep)", "Se otorgaron 18,522 medidas de protección",icon=icon("chart-area"),color="fuchsia", width = 3),
                                                      valueBox("2021", "Se otorgaron 21,644 medidas de protección", icon=icon("equals"), color="purple", width = 3),
                                                      valueBox("2020", "Se otorgaron 17,473 medidas de protección.", icon=icon("wave-square"), color="maroon", width = 3),
                                                      valueBox("2019", "Se otorgaron 18,278 medidas de protección.", icon=icon("signal"), color="light-blue", width = 3)),
                                                      
                                              tabsetPanel(
                                                tabPanel("Total de medidas",  
                                                        #h2(align="center", "Medidas de protección trabajadas")
                                                             
                                                             sidebarPanel("Seleccione algunas características",
                                                                          selectInput(
                                                                            inputId = "medidas_año",
                                                                            label = "Año",
                                                                            choices = unique(sort(medidas_ordenes_municipal$año)),
                                                                            multiple = TRUE                                                               ),                                                               
                                                                          selectInput(
                                                                            inputId = "medidas_mes",
                                                                            label = "Mes",
                                                                            choices = unique(sort(medidas_ordenes_municipal$mes)),
                                                                            multiple = TRUE
                                                                          ),
                                                                          selectInput(
                                                                            inputId = "medidas_municipio",
                                                                            label = "Municipio",
                                                                            choices = unique(sort(medidas_ordenes_municipal$municipio)),
                                                                            multiple = TRUE
                                                                          ),
                                                                          downloadButton("download_medidas", "Descarga (.csv)")
                                                                          ),
                                                             mainPanel(plotlyOutput("grafico_medidas", height = "auto", width = "auto"),
                                                                       h6("Fuente: Elaborado con datos de la Fiscalía Estatal."))),
                                                tabPanel("Mapa de medidas de protección",
                                                         column(12, align="center",
                                                                h2(""),
                                                                
                                                                #h2("Total de medidas trabajadas en el estado de Jalisco, 2019 a 2022"),
                                                                #h6("Datos de la Fiscalía del Estado"),
                                                                selectInput("mapa_medidas", "Seleccione el año" ,
                                                                            choices = c("Año 2019", "Año 2020", 
                                                                                        "Año 2021", "año 2022 (septiembre)"),
                                                                            selected = "año 2022 (septiembre)",  multiple = FALSE, 
                                                                            selectize = TRUE),
                                                                # h3(text=paste0("Total de medidas de protección otorgadas: ", input$mapa_medidas ,
                                                                #           '<br>','<sup>',
                                                                #           'Datos de la Fiscalía del Estado de Jalisco ', align = "left")),
                                                                plotlyOutput("mapa_1",height = "auto", width = "auto"),
                                                                h5("Datos de la Fiscalía del Estado de Jalisco", align="left"),
                                                                h5("*La etiqueta del mapa 'value' hace referencia al valor de la tasa de homicidios dolosos de mujeres.", 
                                                                   align="left", face="italic")))
                                                )
                                              ))),
                            
                            #--------------------------------------------
                            
                            tabPanel(title = "Ordenes de protección",
                                     fluidRow(width=12,  
                                              #h3(align="center", "Ordenes de protección emitidas"),
                                              #box(width=12,
                                                  tabsetPanel(
                                                    box(
                                                      width=12,  
                                                      valueBox("2022 (ene-sep)", "Se otorgaron 183 ordenes de protección, un aumento del 40% con respecto al mismo periodo del año anterior",icon=icon("chart-area"),color="fuchsia", width = 3),
                                                      valueBox("2021", "Se otorgaron 359 ordenes de protección, un aumento del 81% con respecto al año anterior", icon=icon("equals"), color="purple", width = 3),
                                                      valueBox("2020", "Se otorgaron 198 ordenes de protección, un aumento del 607% con respecto al año anterior.", icon=icon("wave-square"), color="maroon", width = 3),
                                                      valueBox("2019", "Se otorgaron 28 ordenes de protección, resalta diciembre que representa el 40% del total de ordenes emitidas.", icon=icon("signal"), color="light-blue", width = 3)),
                                                    
                                              tabsetPanel(
                                                tabPanel("Total de Ordenes",  
                                                         box(width=12,
                                                             tabsetPanel(
                                                               #h2(align="center", "ordenes de protección trabajadas")
                                                             ),
                                                             sidebarPanel("Seleccione algunas características",
                                                                          selectInput(
                                                                            inputId = "ordenes_año",
                                                                            label = "Año",
                                                                            choices = unique(sort(medidas_ordenes_estatal$año)),
                                                                            multiple = TRUE                                                               ),                                                               
                                                                          selectInput(
                                                                            inputId = "ordenes_mes",
                                                                            label = "Mes",
                                                                            choices = unique(sort(medidas_ordenes_estatal$mes)),
                                                                            multiple = TRUE
                                                                          ),
                                                                          selectInput(
                                                                            inputId = "ordenes_municipio",
                                                                            label = "Municipio",
                                                                            choices = unique(sort(medidas_ordenes_estatal$municipio)),
                                                                            multiple = TRUE
                                                                          ),
                                                                          downloadButton("downloadData_ordenes", "Descarga (.csv)")),
                                                             mainPanel(plotlyOutput("grafico_ordenes", height = "auto", width = "auto"),
                                                                       h6("Fuente: Elaborado con datos de la Fiscalía Estatal.")))),
                                                tabPanel("Mapa de ordenes de protección",
                                                         #tags$br(),
                                                         column(12, align="center",
                                                                h2(""),
                                                                
                                                                #h2("Total de ordenes trabajadas en el estado de Jalisco, 2019 a 2022"),
                                                                #h6("Datos de la Fiscalía del Estado"),
                                                                selectInput("mapa_ordenes", "Seleccione el año" ,
                                                                            choices = c("Año 2019", "Año 2020", 
                                                                                        "Año 2021", "año 2022 (septiembre)"),
                                                                            selected = "año 2022 (septiembre)",  multiple = FALSE, 
                                                                            selectize = TRUE),
                                                                # h3(text=paste0("Total de ordenes de protección otorgadas: ", input$mapa_ordenes ,
                                                                #           '<br>','<sup>',
                                                                #           'Datos de la Fiscalía del Estado de Jalisco ', align = "left")),
                                                                plotlyOutput("mapa_2",height = "auto", width = "auto"),
                                                                h5("Datos de la Fiscalía del Estado de Jalisco", align="left"),
                                                                h5("*La etiqueta del mapa 'value' hace referencia al valor de la tasa de homicidios dolosos de mujeres.", 
                                                                   align="left", face="italic")))
                                              )))),
                            
                            tabPanel(title = "Muertes violentas",
                                     column(12, align="center",
                                            h2("Muertes violentas de mujeres"),
                                            box(width = 12,
                                                tabsetPanel(
                                                  box(
                                                    width=12,  
                                                    valueBox("2022 (ene-sep)", "Se contabilizan 152 muertes violentas de mujeres, 130 homicidios dolosos y 22 feminicidios.",icon=icon("chart-area"),color="fuchsia", width = 4),
                                                    valueBox("Máximo histórico", "2019 es el año con mayor número de muertes violentas con 285: homicidios dolosos 218 y feminicidios 67.", icon=icon("equals"), color="purple", width = 4),
                                                    valueBox("Crecimiento anual", "La mayor variación anual se presenta en 2018 al aumentar en 98% con respecto a 2017.", icon=icon("wave-square"), color="maroon", width = 4)),
                                                  
                                                  sidebarLayout(
                                                    sidebarPanel("Seleccione algunas características",
                                                                 
                                                                 
                                                                 selectInput(
                                                                   inputId = "victimas_año",
                                                                   label = "Año",
                                                                   choices = unique(sort(victimas$Año)),
                                                                   multiple = T
                                                                 ),
                                                                 selectInput(
                                                                   inputId = "victimas_edad",
                                                                   label = "Modalidad",
                                                                   choices = unique(sort(victimas$Rango.de.edad)),
                                                                   multiple = TRUE
                                                                 ),
                                                                 selectInput(
                                                                   inputId = "victimas_delito",
                                                                   label = "Delito",
                                                                   choices = unique(sort(victimas$Subtipo.de.delito)),
                                                                   multiple = TRUE
                                                                 ),  
                                                                 downloadButton("downloadData_victimas", "Descarga (.csv)")),
                                                    mainPanel(#h3(align="center", "Total de muertes violentas de mujeres",
                                                                 
                                                                 plotlyOutput("grafico_victimas",  height = "auto", width = "auto"),
                                                                 dataTableOutput("table_muertes"),
                                                                 h6("Datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, SESNSP"))))))
                                            
                                            # box(width=12,
                                            #     mainPanel(dataTableOutput("table_muertes"))
                                            # )
                                     )),
                 
                 
                 #############################################################################################
                 #############################################################################################
                 
                 navbarMenu(
                   title = "Incidencia delictiva",
                   #icon = icon("dot-circle"),
                   tabPanel(title = "Municipal",
                            
                            tabPanel("Total por municipio",
                                     box(
                                       width=12,  
                                       valueBox("2022 (ene-oct)", "se registran 14,7751 carpetas iniciadas 
                                                por incidencia delicitva por razón de género.",icon=icon("chart-area"),color="fuchsia"),
                                       valueBox("De 2018 a 2019", "se presenta la variación anual más grande del histórico con 22%, al pasar de 11,780 a 14,355.", icon=icon("equals"), color="purple"),
                                       valueBox("Violencia familiar", "es el delito con mayor número de carpetas al concentrar el 78%, 
                                                seguido de abuso sexual (16%) y violación con 2%.", icon=icon("wave-square"), color="maroon")),
                                     
                                     sidebarLayout(
                                       sidebarPanel("\nSeleccione algunas características",
                                                    selectInput(
                                                      inputId = "municipal_año",
                                                      label = "Año",
                                                      choices = sort(unique(municipios$Año)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "municipal_mes",
                                                      label = "Mes",
                                                      choices = sort(unique(municipios$mes)),
                                                      multiple = T,
                                                      selected = "Violencia familiar"
                                                    ),
                                                    selectInput(
                                                      inputId = "municipal_delito",
                                                      label = "Delito",
                                                      choices = sort(unique(municipios$Subtipo.de.delito)),
                                                      multiple = F,
                                                      selected = "Violencia familiar"
                                                    ),
                                                    selectInput(
                                                      inputId = "municipal_municipio",
                                                      label = "Municipio",
                                                      choices = sort(unique(municipios$Municipio)),
                                                      multiple = T,
                                                      selected = "Estado de Jalisco"
                                                      # options = list(
                                                      #   `actions-box` = TRUE,
                                                      #   `deselect-all-text` = "Sin selección filtro",
                                                      #   `select-all-text` = "Seleccionar todos",
                                                      #   `none-selected-text` = "Sólo los de la Región")
                                                    ),
                                       
                                       downloadButton("downloadData_municipal", "\nDescarga (.csv)")
                                       ),
                                       mainPanel(plotlyOutput("grafico_municipal_periodo"),
                                                 h6("Fuente: Datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)."),
                                                 h6("Datos a octubre de 2022"),
                                                 h6("En este apartado sólo se muestran los delitos que se consideran estar relacionados a una razón de género: acoso sexual,
                                                    abuso sexual, feminicidio, hostigamiento sexual, violación y violencia familiar."),
                                                 
                                                 plotlyOutput("grafico_municipal"),
                                                 h6("Fuente: Datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)."),
                                                 h6("Datos a octubre de 2022"),
                                                 h6("En este apartado sólo se muestran los delitos que se consideran estar relacionados a una razón de género: acoso sexual,
                                                    abuso sexual, feminicidio, hostigamiento sexual, violación y violencia familiar."))
                                     ))),
                   
                   
                   #############################################################################################
                   
                   tabPanel(title = "Comparativa Regional",
                            tabPanel("Incidencia delictiva por Regiones",
                                     box(
                                       width=12,  
                                       valueBox("2022 (ene-oct)", "se registran 14,775 carpetas iniciadas 
                                                por incidencia delicitva por razón de género.",icon=icon("chart-area"),color="fuchsia"),
                                       valueBox("De 2018 a 2019", "se presenta la variación anual más grande del histórico con 22%, al pasar de 11,780 a 14,355.", icon=icon("equals"), color="purple"),
                                       valueBox("Violencia familiar", "es el delito con mayor número de carpetas al concentrar el 78%, 
                                                seguido de abuso sexual (16%) y violación con 2%.", icon=icon("wave-square"), color="maroon")),
                                     
                                     sidebarLayout(
                                       sidebarPanel("Seleccione algunas características",
                                                    selectInput(
                                                      inputId = "Regional_Año",
                                                      multiple = T,
                                                      label = "Año",
                                                      choices = sort(unique(Regiones$Año))
                                                    ),
                                                    selectInput(
                                                      inputId = "Regional_Mes",
                                                      label = "Mes",
                                                      multiple = T,
                                                      choices = sort(unique(Regiones$mes))
                                                      ),
                                                    selectInput(
                                                      inputId = "Regional_delito",
                                                      label = "Delito",
                                                      choices = sort(unique(Regiones$Subtipo.de.delito)),
                                                      multiple = F,
                                                      selected = "Violencia familiar"
                                                    ),
                                                    selectInput(
                                                      inputId = "Regional_Región",
                                                      label = "Región",
                                                      choices = sort(unique(Regiones$Región)),
                                                      multiple = T,
                                                      selected = c("Región Altos Norte")
                                                    ),
                                       downloadButton("downloadData_regional", "\nDescarga (.csv)")
                                       ),
                                       
                                       
                                       mainPanel(#h3(align="center","Total de carpetas por el delito de \n"),
                                                 plotlyOutput("grafico_Regional"),
                                                 h6("Fuente: Datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)."),
                                                 h6("Datos a octubre de 2022"),
                                                 #h3(align="center","Total de carpetas por el delito de \n"),
                                                 plotlyOutput("regional_anual",height = "auto", width = "auto"),
                                                 h6("Fuente: Datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)."),
                                                 h6("Datos a octubre de 2022"),
                                                 h6("En este apartado sólo se muestran los delitos que se consideran estar relacionados a una razón de género: acoso sexual,
                                                    abuso sexual, feminicidio, hostigamiento sexual, violación y violencia familiar."))
                                     )))
                 ),
      
                 
                 navbarMenu(
                   title = "Unidades de atención",
                   #icon = icon("dot-circle"),
                   tabPanel(title = "Total de atenciones",
                            tabPanel("Total por atenciones brindadas en el año 2022.",
                                     box(
                                       width=12, 
                                       valueBox("2022", "de enero a septiembre se han atendido a 19,494 usuarias.", icon=icon("wave-square"), color="maroon", width = 4),
                                       valueBox("1,949", "atenciones promedios al mes, durante 2022.",icon=icon("chart-area"),color="fuchsia", width = 4),
                                       valueBox("52%", "de las atenciones se realizaron en la UMEA", icon=icon("equals"), color="purple", width = 4)),
                                     
                                     #h3(align="center", "Total de atenciones realizadas en las módulos de atención."),
                                     #h5(align="center", "Los datos son proporcionados mensualmente por el registro interno a cada usuaria que solicita atención en cualquiera de las unidades de atención."),
                                     sidebarLayout(
                                       sidebarPanel("Seleccione algunas características",
                                                    # selectInput(
                                                    #   inputId = "atencion_año",
                                                    #   label = "Delito",
                                                    #   choices = sort(unique(atenciones$año)),
                                                    #   multiple = T
                                                    # ),
                                                    selectInput(
                                                      inputId = "atencion_mes",
                                                      label = "Mes",
                                                      choices = sort(unique(atenciones$month)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "atencion_tipo",
                                                      label = "Tipo de violencia por la que se atiende",
                                                      choices = sort(unique(atenciones$tipo)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "atencion_unidad",
                                                      label = "Unidad",
                                                      choices = sort(unique(atenciones$Unidad)),
                                                      multiple = T
                                                    ),
                                       
                                       downloadButton("downloadData_violencia", "Descarga (.csv)")),
                                       mainPanel(plotlyOutput("total_atenciones_grafico"),
                                                 h6("Fuente: Datos proporcionados por las Unidades Especializadas de Atención, de la
                                                    Secretaría de Igualdad Sustantiva Entre Mujeres y Hombres (SISEMH)."),
                                                 h6("Los datos son proporcionados mensualmente por el registro interno a cada usuaria 
                                                    que solicita atención en cualquiera de las unidades de atención."))))),
                   
                   tabPanel(title = "Comportamiento anual por tipo de violencia",
                            tabPanel("Total por atenciones brindadas en el año 2022.",
                                     box(
                                       width=12,  
                                       valueBox("Violencia psicológica", "es el tipo de violencia que más se atiende en las Unidades Especializadas.",icon=icon("chart-area"),color="fuchsia", width = 6),
                                       valueBox("La UMEA", "La Unidad Metropolitana de Atención Integral a Mujeres y. Niñez (UMEA) atiende a más de la mitad de usuarias.", icon=icon("equals"), color="purple", width = 6)),
                                     sidebarLayout(
                                       sidebarPanel("Seleccione algunas características",
                                                    # selectInput(
                                                    #   inputId = "atencion_año",
                                                    #   label = "Delito",
                                                    #   choices = sort(unique(atenciones$año)),
                                                    #   multiple = T
                                                    # ),
                                                    selectInput(
                                                      inputId = "atencion_mes2",
                                                      label = "Mes",
                                                      choices = sort(unique(atenciones$Mes)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "atencion_tipo2",
                                                      label = "Tipo de violencia",
                                                      choices = sort(unique(atenciones$tipo)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "atencion_unidad2",
                                                      label = "Unidad",
                                                      choices = sort(unique(atenciones$Unidad)),
                                                      multiple = T
                                                    ),
                                       
                                       downloadButton("downloadData_anual_violencia", "\nDescarga (.csv)")),
                                       mainPanel(plotlyOutput("atenciones_mensuales"),
                                                 h6("Fuente: Datos proporcionados por las Unidades Especializadas de Atención, de la
                                                    Secretaría de Igualdad Sustantiva Entre Mujeres y Hombres (SISEMH)."),
                                                 h6("Los datos son proporcionados mensualmente por el registro interno a cada usuaria 
                                                    que solicita atención en cualquiera de las unidades de atención."))))),
#                 )))),
                   
                   tabPanel(title = "Atenciones por tipo de violencia",
                            tabPanel("Total de atenciones brindadas en el año 2022 por tipo de violencia.",
                                     box(
                                       width=12, 
                                       valueBox("2022", "de enero a septiembre se han atendido a 19,494 usuarias.", icon=icon("wave-square"), color="maroon", width = 4),
                                       valueBox("Violencia psicológica", "económica y física son los tipos de violencia que más atienden.", icon=icon("wave-square"), color="fuchsia", width = 4),
                                       valueBox("A partir de junio", "las atenciones por violencia sexual han aumentado, sube un puesto en el ranking.",icon=icon("chart-area"),color="purple", width = 4)),
                                     
                                     sidebarLayout(
                                       sidebarPanel("Seleccione algunas características",
                                                    #              selectInput(
                                                    #                inputId = "atencion_año",
                                                    #                label = "Delito",
                                                    #                choices = sort(unique(atenciones$año)),
                                                    #                multiple = T
                                                    #              ),
                                                    selectInput(
                                                      inputId = "atencion_mes3",
                                                      label = "Mes",
                                                      choices = sort(unique(atenciones$Mes)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "atencion_tipo3",
                                                      label = "Tipo de violencia",
                                                      choices = sort(unique(atenciones$tipo)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "atencion_unidad3",
                                                      label = "Unidad",
                                                      choices = sort(unique(atenciones$Unidad)),
                                                      multiple = T
                                       ),
                                       
                                       downloadButton("downloadData_atenciones_violencia", "\nDescarga (.csv)")),
                                       mainPanel(plotlyOutput("atenciones_tipo")))))
                 ),
                 navbarMenu(
                   title = "Documentación",
                   icon = icon("file-export"),
                   tabPanel(title = "Reportes estadísticos"),
                   tabPanel(title = "Descarga masiva")
                 )
      )
      
    )))

server <- function(input, output) {
  
  
  output$llamadas_año <- renderUI({
    selectInput("llamadas_año",
                label =  "Seleccione el año",
                choices = sort(unique(reportes_llamadas$año)),
                multiple = T)
  })
  
  output$llamadas_month<- renderUI({
    selectInput("llamadas_month",
                label =  "Seleccione el mes",
                choices = sort(unique(reportes_llamadas$month)),
                multiple = T)
  })
  
  
  output$llamadas_clasificacion <- renderUI({
    selectInput("llamadas_clasificacion",
                label =  "Selecciona el tipo de violencia",
                choices = sort(unique(reportes_llamadas$clasificación)),
                multiple = T)
  })
  
  
  output$llamadas_municipio <- renderUI({
    selectInput("llamadas_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(reportes_llamadas$municipio)),
                multiple = T)
  })
  
  
  #base reactiva para slide 3
  reportes_llamadas_reactive <- reactive({
    
    reportes_llamadas %>%
      filter(
        if(!is.null(input$llamadas_año))                       año %in% input$llamadas_año             else año != "",
        if(!is.null(input$slider))                             año %in% input$slider                   else año != "",
        if(!is.null(input$slider2))                             año %in% input$slider2                 else año != "",
        if(!is.null(input$llamadas_month))                       month %in% input$llamadas_month       else month != "",
        if(!is.null(input$llamadas_clasificacion))   clasificación %in% input$llamadas_clasificacion   else clasificación != "",
        if(!is.null(input$llamadas_municipio))           municipio %in% input$llamadas_municipio       else municipio != ""
      )
    
  })
  
  # output$vbox <- renderValueBox({
  #   valueBox(
  #     paste('Promedio', input$llamadas_año),
  #     count(reportes_llamadas[reportes_llamadas$llamadas_año %in% input$llamadas_año])
  #   )
  # })
  
  
  output$downloadData_llamadas <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reportes_llamadas_reactive(), file, row.names = FALSE)
    })
  
  

  output$grafico_llamadas <- renderPlotly ({
    
    reportes_llamadas_reactive() %>% 
      group_by(año, mes, clasificación) %>% 
      summarise(total=n(),.groups = "drop") %>% 
      mutate(text = paste("Total de llamadas: ", scales::comma(total), 
                          "\nPeríodo: ", mes,
                          "\nTipo de violencia: ", clasificación, sep="")) %>% 
      ggplot() +
      aes(x = as.factor(mes), y = total, text=text,
          group = 1, fill = clasificación, color = clasificación) +
      geom_point(size=1.7)+
      geom_line(size=1) +
      scale_color_manual(
        values = c(
          `Violencia contra las mujeres` = "#D98CBC",
          `Violencia de pareja` = "#C91682",
          `Violencia familiar` = "#7E3794"))+      
      
      scale_fill_manual(
        values = c(
          `Violencia contra las mujeres` = "#D98CBC",
          `Violencia de pareja` = "#C91682",
          `Violencia familiar` = "#7E3794"))+      
      scale_y_continuous(labels = scales::comma) +
      labs(title= "",
           #paste0("Municipio ",reportes_llamadas_reactive()$municipio[1]),
           x="", y="Total de llamadas", fill="", color="")+
      scale_x_discrete(breaks = c("2017-12","2017-03","2017-06","2017-09",
                                  "2018-12","2018-03","2018-06","2018-09",
                                  "2019-12","2019-03","2019-06","2019-09",
                                  "2020-12","2020-03","2020-06","2020-09",
                                  "2021-12","2021-03","2021-06","2021-09",
                                  "2022-03","2022-06", "2022-09"))+ 
      theme_minimal()+
      theme(text=element_text(size=11,  family="Century Gothic"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10))->grafico_llamadas
    
    
    ggplotly(grafico_llamadas, tooltip = "text") %>%
      layout(title = list(text = paste0("Total de denuncias por violencia familiar \n", 
                                        #reportes_llamadas_reactive()$municipio,
                                        '<br>',
                                        '<sup>')),
             margin = list(b=0,t=30), 
             xaxis = list(side = "bottom"),
             legend = list(orientation = "h", x = 0.1, y = -0.3,
                           side="bottom"))
    
    # layout(legend = list(orientation = "h", x = 0.1, y = -0.3),
    #        margin = list(b=0,t=30)) Este es el bueno
    
    #   layout(title = list(text = paste0("Total de denuncias por violencia familiar\n",
    #                                     reportes_llamadas_reactive()$municipio,
    #                                     '</sup>',
    #                                     '<br>')),
    #          margin = list(b=-5,t=40), 
    #          xaxis = list(side = "bottom"),legend = list(side="bottom"))
    # 
    
    
  })  
  
  
  
  
  
  
  # ---------------------------------------------------------------------------- #  

  
  output$violencia_familiar_año <- renderUI({
    selectInput("violencia_familiar_año",
                label =  "Seleccione el año",
                choices = sort(unique(violencia_familiar_diario$año)),
                multiple = T)
  })
  
  
  output$violencia_familiar_month <- renderUI({
    selectInput("violencia_familiar_month",
                label =  "Seleccione el mes",
                choices = sort(unique(violencia_familiar_diario$month)),
                multiple = T)
  })
  
  output$violencia_familiar_sexo <- renderUI({
    selectInput("violencia_familiar_sexo",
                label =  "Selecciona el sexo",
                choices = sort(unique(violencia_familiar_diario$sexo)),
                multiple = T)
  })
  
  
  output$violencia_familiar_zona <- renderUI({
    selectInput("violencia_familiar_zona",
                label =  "Selecciona la zona",
                choices = sort(unique(violencia_familiar_diario$zona)),
                multiple = T)
  })
  
  
  #base reactiva para slide 3
  violencia_familiar_diario_reactive <- reactive({
    
    violencia_familiar_diario %>%
      filter(
        if(!is.null(input$violencia_familiar_año))             año %in% input$violencia_familiar_año          else año != "",
        if(!is.null(input$violencia_familiar_month))         month %in% input$violencia_familiar_month        else month != "",
        if(!is.null(input$violencia_familiar_zona))           zona %in% input$violencia_familiar_zona         else zona != "",
        if(!is.null(input$violencia_familiar_sexo))           sexo %in% input$violencia_familiar_sexo         else sexo != ""
      ) 
    
    
  })
  
  output$download_violencia_familiar <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(violencia_familiar_diario_reactive(), file, row.names = F)
    
      })
  
  
  output$grafico_violencia_familiar <- renderPlotly ({
    
    violencia_familiar_diario_reactive() %>% 
      mutate(
        month=factor(
          month,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                         "Septiembre", "Octubre","Noviembre", "Diciembre"))) %>%
      group_by(mes,sexo) %>% 
      summarise(registro=sum(registro),.groups = "drop") %>% 
      mutate(text = paste("Total de denuncias: ", scales::comma(registro), 
                          "\nPeríodo: ", mes,
                          "\nSexo del denunciante: ", sexo, sep="")) %>% 
      ggplot()+
      aes(x = as.factor(mes), y = registro, group=sexo, text=text) +
      geom_point(aes(fill=sexo, color = sexo), size=3)+
      geom_line (aes(fill=sexo, color = sexo), size=1) +
      scale_color_manual(values = c(
        Mujeres = "#C91682",
        Hombres = "#7E3794"))+
      scale_fill_manual(values = c(
        Mujeres = "#C91682",
        Hombres = "#7E3794"))+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(breaks = c("2017-12","2017-03","2017-06","2017-09",
                                  "2018-12","2018-03","2018-06","2018-09",
                                  "2019-12","2019-03","2019-06","2019-09",
                                  "2020-12","2020-03","2020-06","2020-09",
                                  "2021-12","2021-03","2021-06","2021-09",
                                  "2022-03","2022-06", "2022-09"))+
      labs(x="", y="Total de denuncias", fill="Sexo", color="Sexo")+
      theme_minimal()+
      theme(text=element_text(size=11, family="Century Gothic"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5, family="Century Gothic"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=11, family="Century Gothic"))->grafico_violencia_familiar_1
    
    
    # #grafico_violencia_familiar
    ggplotly(grafico_violencia_familiar_1, tooltip = "text") %>%
      layout(title = list(text = paste0("Total de denuncias por violencia familiar \n",
                                        violencia_familiar_diario_reactive()$zona,
                                        '<br>',
                                        '<sup>')),
             margin = list(b=0,t=50),
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
    
    
    # layout(#legend = list(orientation = "h", x = 0.1, y = -0.3),
    #        margin = list(b=0,t=150),
    #                      title = list(paste0("Total de denuncias por violencia familiar \n",
    #                                     violencia_familiar_diario_reactive()$zona,
    #                                    '</sup>',
    #                                    '<br>')))
    
    
  })  
  
  
  
  
  
  output$downloadData_total <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(medidas_reactive(), file, row.names = FALSE)
    }
  )
  
  
  output$medidasr_año <- renderUI({
    selectInput("medidas_año",
                label =  "Seleccione el año",
                choices = sort(unique(medidas_ordenes_municipal$año)),
                multiple = T)
  })
  
  output$medidasr_mes <- renderUI({
    selectInput("medidasr_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(medidas_ordenes_municipal$mes)),
                multiple = T)
  })
  
  
  output$medidas_municipio <- renderUI({
    selectInput("medidas_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(medidas_ordenes_municipal$municipio)),
                multiple = T)
  })
  
  
  
  medidas_reactive <- reactive({
    
    medidas_ordenes_municipal %>%
      filter(
        if(!is.null(input$medidas_año))             año %in% input$medidas_año               else año != "",
        if(!is.null(input$medidas_mes))             mes %in% input$medidas_mes               else mes != "",
        if(!is.null(input$medidas_municipio))       municipio %in% input$medidas_municipio   else municipio != ""
      )
    
  })
  
  
  output$grafico_medidas <- renderPlotly ({
    
    
    medidas_reactive() %>% 
      #medidas_ordenes_municipal %>% 
      group_by(año, mes) %>% 
      summarise(medidas=sum(medidas_aceptadas + medidas_rechazadas),
                ordenes=sum(ordenes_aceptadas + ordenes_rechazadas),.groups = "drop") %>% 
      pivot_longer(cols=c("ordenes","medidas"),
                   names_to = "tipo",
                   values_to = "total")%>% 
      mutate(fecha=case_when(
        mes=="Enero"~ "01", 
        mes=="Febrero"~"02", 
        mes=="Marzo"~"03", 
        mes=="Abril"~"04", 
        mes=="Mayo"~"05", 
        mes=="Junio"~"06",
        mes=="Julio"~"07", 
        mes=="Agosto"~"08", 
        mes=="Septiembre"~"09", 
        mes=="Octubre"~"10", 
        mes=="Noviembre"~"11", 
        mes=="Diciembre"~"12"),
        fecha=paste0(año,"-", fecha)) %>%  
      filter(tipo=="medidas") %>% 
      mutate(text = paste("Total de medidas: ", scales::comma(total), 
                          "\nPeríodo: ", fecha, sep="")) %>% 
      ggplot() +
      aes(x =fecha, y = total, color="#de1065", text=text) +
      #geom_col()+
      geom_point(color="#C91682", size=4, alpha=0.7) + 
      geom_segment(aes(x=fecha, xend=fecha, y=0, yend=total))+
      #geom_line(size=1)+
      # scale_fill_manual(
      #   values = c(#ordenes = "#C91682"#,
      #     medidas ="#C91682" #"#7E3794"
      #   ))+
      #   scale_color_manual(
      #     values = c(#ordenes = "#C91682"#,
      #       medidas ="#C91682" #"#7E3794"
      #     ))+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(breaks = c("2017-12","2017-03","2017-06","2017-09",
                                  "2018-12","2018-03","2018-06","2018-09",
                                  "2019-12","2019-03","2019-06","2019-09",
                                  "2020-12","2020-03","2020-06","2020-09",
                                  "2021-12","2021-03","2021-06","2021-09",
                                  "2022-03","2022-06", "2022-09"))+
      #scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      labs(title = paste("Total de medidas trabajadas"),
           #violencia_familiar_diario_reactive()$municipio[1]),
           x="", y="Total", fill="Tipo", color="Tipo")+
      #facet_grid(.~año, space = "free_x", scales = "free_x", switch="x") +
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(size=11,  family="Century Gothic"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9))->gr_medidas
    
    ggplotly(gr_medidas, tooltip = "text") %>%
      layout(#legend = list(orientation = "h", x = 0.1, y = -0.3),
        margin = list(b=0,t=50),
        title = paste("Total de medidas de protección trabajadas \n",
                      #medidas_reactive()$municipio,
                      '</sup>',
                      '<br>'))
  })  
  
  
  
  
  output$mapa_1 <- renderPlotly ({
    
    
    if (input$mapa_medidas == "Año 2019") {
      
      
      mxmunicipio_choropleth(medidas_2019, num_colors = 1,
                             zoom = subset(medidas_2019, state_name %in% 
                                             c("Jalisco"))$region,
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x="", y="") +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#C91682",
          guide = "colourbar",
          label=comma)+
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(.8, 'cm'),
              legend.key.width= unit(.8, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Century Gothic"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Century Gothic"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_1
    }
    
    if (input$mapa_medidas == "Año 2020") {
      
      mxmunicipio_choropleth(medidas_2020, num_colors = 1,
                             zoom = subset(medidas_2020, state_name %in% 
                                             c("Jalisco"))$region ,
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x="", y="") +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#C91682",
          guide = "colourbar",
          label=comma)+        
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(.8, 'cm'),
              legend.key.width= unit(.8, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Century Gothic"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Century Gothic"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_1
    }
    
    if (input$mapa_medidas == "Año 2021") {
      
      mxmunicipio_choropleth(medidas_2021, num_colors = 1,
                             zoom = subset(medidas_2021, state_name %in% 
                                             c("Jalisco"))$region,
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x="", y="") +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#C91682",
          guide = "colourbar",
          label=comma)+
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(.8, 'cm'),
              legend.key.width= unit(.8, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Century Gothic"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Century Gothic"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_1
    }
    
    if (input$mapa_medidas == "año 2022 (septiembre)") {
      
      mxmunicipio_choropleth(medidas_2022, num_colors = 1,
                             zoom = subset(medidas_2022, state_name %in% c("Jalisco"))$region, 
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x=" ", y=" ", title = paste0("Total de medidas de protección otorgadas: \n",'<br>','<sup>', input$mapa_medidas)) +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#C91682",
          guide = "colourbar",
          label=comma)+
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(.8, 'cm'),
              legend.key.width= unit(.8, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Century Gothic"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Century Gothic"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_1
    }  
    
    ggplotly(mapa_1) %>%
      layout(title = list(text = paste0("Total de medidas de protección otorgadas: \n", 
                                        input$mapa_medidas,
                                        '<br>',
                                        '<sup>')),
             
             margin = list(b=0, t=30), annotations =
               list(x =.67, y = -.27,
                    text = "",
                    #text = "   Datos de cerodesabasto.org",
                    showarrow = F, xref='paper', yref='paper',
                    xanchor='right', yanchor='auto', xshift=0, yshift=4,
                    font=list(size=10,  color="#9443FF"))
      )
    # layout(
    #    title = list(text = paste0("Total de medidas de protección otorgadas: ", input$mapa_medidas 
    #                               )),
    #   margin = list(b=0,t=55))
    
    # add_annotations(
    #   yref="paper", 
    #   xref="paper", 
    #   y=0, 
    #   x=0, 
    #   text=paste0("Total de medidas de protección otorgadas: ", '</sup>',
    #               input$mapa_medidas),#"My Title", 
    #   showarrow=F, 
    #   font=list(size=13)
    # ) %>% 
    #   layout(title=FALSE)
    
  })  
  
  
  
  
  
  
  
  ###############################################################################
  
  output$ordenesr_año <- renderUI({
    selectInput("ordenes_año",
                label =  "Seleccione el año",
                choices = sort(unique(medidas_ordenes_municipal$año)),
                multiple = T)
  })
  
  output$ordenesr_mes <- renderUI({
    selectInput("ordenesr_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(medidas_ordenes_municipal$mes)),
                multiple = T)
  })
  
  
  output$ordenes_municipio <- renderUI({
    selectInput("ordenes_municipio",
                label =  "Selecciona el municipio",
                choices = sort(unique(medidas_ordenes_municipal$municipio)),
                multiple = T)
  })
  
  
  
  ordenes_reactive <- reactive({
    
    medidas_ordenes_municipal %>%
      filter(
        if(!is.null(input$ordenes_año))             año %in% input$ordenes_año               else año != "",
        if(!is.null(input$ordenes_mes))             mes %in% input$ordenes_mes               else mes != "",
        if(!is.null(input$ordenes_municipio))       municipio %in% input$ordenes_municipio   else municipio != ""
      )
    
  })
  
  
  output$downloadData_ordenes <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ordenes_reactive(), file, row.names = FALSE)
    }
  )
  
  
  output$grafico_ordenes <- renderPlotly ({
    
    
    ordenes_reactive() %>% 
      #medidas_ordenes_municipal %>% 
      group_by(año, mes) %>% 
      summarise(medidas=sum(medidas_aceptadas + medidas_rechazadas),
                ordenes=sum(ordenes_aceptadas + ordenes_rechazadas),.groups = "drop") %>% 
      pivot_longer(cols=c("ordenes","medidas"),
                   names_to = "tipo",
                   values_to = "total")%>% 
      mutate(fecha=case_when(
        mes=="Enero"~ "01", 
        mes=="Febrero"~"02", 
        mes=="Marzo"~"03", 
        mes=="Abril"~"04", 
        mes=="Mayo"~"05", 
        mes=="Junio"~"06",
        mes=="Julio"~"07", 
        mes=="Agosto"~"08", 
        mes=="Septiembre"~"09", 
        mes=="Octubre"~"10", 
        mes=="Noviembre"~"11", 
        mes=="Diciembre"~"12"),
        fecha=paste0(año,"-", fecha)) %>%  
      filter(tipo=="ordenes") %>% 
      mutate(text = paste("Total de ordenes: ", scales::comma(total), 
                          "\nPeríodo: ", fecha, sep="")) %>% 
      ggplot() +
      aes(x =fecha, y = total, color="7E3794", text=text) +
      #geom_col()+
      geom_point(color="#7E3794", size=3, alpha=0.6) + 
      geom_segment(aes(x=fecha, xend=fecha, y=0, yend=total))+#, color="#b24dd1")+
      #geom_line(size=1)+
      # scale_fill_manual(
      #   values = c(#ordenes = "#C91682"#,
      #     medidas ="#C91682" #"#7E3794"
      #   ))+
      #   scale_color_manual(
      #     values = c(#ordenes = "#C91682"#,
      #       medidas ="#C91682" #"#7E3794"
      #     ))+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(breaks = c("2017-12","2017-03","2017-06","2017-09",
                                  "2018-12","2018-03","2018-06","2018-09",
                                  "2019-12","2019-03","2019-06","2019-09",
                                  "2020-12","2020-03","2020-06","2020-09",
                                  "2021-12","2021-03","2021-06","2021-09",
                                            "2022-03","2022-06","2022-09"))+  
      #scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      labs(title = paste("Total de ordenes emitidas"),
           #violencia_familiar_diario_reactive()$municipio[1]),
           x="", y="Total", fill="Tipo", color="Tipo")+
      #facet_grid(.~año, space = "free_x", scales = "free_x", switch="x") +
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(size=11,  family="Century Gothic"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9))->gr_ordenes
    
    ggplotly(gr_ordenes, tooltip = "text") %>%
      layout(#legend = list(orientation = "h", x = 0.1, y = -0.3),
        margin = list(b=0,t=50),
        title = paste("Total de ordenes de protección emitidas \n",
                      #medidas_reactive()$municipio,
                      '</sup>',
                      '<br>'))
  })  
  
  
  
  
  output$mapa_2 <- renderPlotly ({
    
    
    if (input$mapa_ordenes == "Año 2019") {
      
      mxmunicipio_choropleth(ordenes_2019, num_colors = 1,
                             zoom = subset(ordenes_2019, state_name %in% c("Jalisco"))$region, 
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x="", y="") +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#7E3794",
          guide = "colourbar",
          label=comma)+
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(.8, 'cm'),
              legend.key.width= unit(.8, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Century Gothic"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Century Gothic"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_2
    }
    
    if (input$mapa_ordenes == "Año 2020") {
      
      mxmunicipio_choropleth(ordenes_2020, num_colors = 1,
                             zoom = subset(ordenes_2020, state_name %in% c("Jalisco"))$region, 
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x="", y="") +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#7E3794",
          guide = "colourbar",
          label=comma)+        
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(.8, 'cm'),
              legend.key.width= unit(.8, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Century Gothic"),
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_2
    }
    
    if (input$mapa_ordenes == "Año 2021") {
      
      mxmunicipio_choropleth(ordenes_2021, num_colors = 1,
                             zoom = subset(ordenes_2021, state_name %in% c("Jalisco"))$region, 
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x="", y="") +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#7E3794",
          guide = "colourbar",
          label=comma)+
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(.8, 'cm'),
              legend.key.width= unit(.8, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Century Gothic"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Century Gothic"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_2
    }
    
    if (input$mapa_ordenes == "año 2022 (septiembre)") {
      
      mxmunicipio_choropleth(ordenes_2022, num_colors = 1,
                             zoom = subset(ordenes_2022, state_name %in% c("Jalisco"))$region, 
                             show_states = FALSE, legend = "%")+ 
        labs(caption="", fill="Total", x=" ", y=" ", 
             # title = paste0("Total de medidas de protección otorgadas: \n",'<br>','<sup>', 
             #                                                        input$mapa_medidas)
        ) +
        scale_fill_gradient(
          low = "#e9e8eb", 
          high = "#7E3794",
          guide = "colourbar",
          label=comma)+
        theme_minimal()+
        theme(#legend.position = "bottom",
              legend.key.height= unit(.8, 'cm'),
              legend.key.width= unit(.8, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Century Gothic"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Century Gothic"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_2
    }  
    
    ggplotly(mapa_2) %>%
      layout(title = list(text = paste0("Total de ordenes de protección otorgadas: \n", 
                                        input$mapa_medidas,
                                        '<br>',
                                        '<sup>')),
             
             margin = list(b=0, t=30), annotations =
               list(x =.67, y = -.27,
                    text = "",
                    #text = "   Datos de cerodesabasto.org",
                    showarrow = F, xref='paper', yref='paper',
                    xanchor='right', yanchor='auto', xshift=0, yshift=4,
                    font=list(size=10,  color="#9443FF"))
      )
    # layout(
    #    title = list(text = paste0("Total de medidas de protección otorgadas: ", input$mapa_medidas 
    #                               )),
    #   margin = list(b=0,t=55))
    
    # add_annotations(
    #   yref="paper", 
    #   xref="paper", 
    #   y=0, 
    #   x=0, 
    #   text=paste0("Total de medidas de protección otorgadas: ", '</sup>',
    #               input$mapa_medidas),#"My Title", 
    #   showarrow=F, 
    #   font=list(size=13)
    # ) %>% 
    #   layout(title=FALSE)
    
  })  
  
  
  # ---------------------------------------------------------------------------
  
  output$table_muertes <- renderDataTable ({
    
    
    victimas_reactive() %>% 
      #victimas %>% 
      group_by(Año, Subtipo.de.delito, Sexo) %>% 
      filter(Subtipo.de.delito %in% c("Homicidio doloso", "Feminicidio"),
             Sexo=="Mujer", Entidad=="Jalisco") %>% 
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
                value=sum(ene+feb+mar+abr+ 
                            may+jun+jul+ago+
                            sep+oct+nov+dic)) %>% 
      select(Año, Subtipo.de.delito, value) %>% 
      pivot_wider(names_from = "Subtipo.de.delito",
                  values_from = "value") %>% 
      summarise(
        feminicidio=Feminicidio,
        homicidio=`Homicidio doloso`,
        total=sum(Feminicidio + `Homicidio doloso`), .groups = "drop") %>% 
      mutate(variación = total - lag(total)) %>% 
      #select(Año, total, variación) %>% 
      arrange(-Año) %>% 
      datatable(
        
        filter = 'top',
        colnames = c('Año', 
                     'Feminicidios','Homicidios', 
                     'Total','Variación'), 
        
        extensions = 'Buttons',
        options = list(
          
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#7E3794', 'color': '#fff', align:'center'});","}"),
          
          dom = "tip",#'Blfrtip',
          buttons = c('copy', 'excel', 'print'),
          lengthMenu = list(c(8,1,4,8, "All"),
                            c(8,1,4,8, "All")),
          columnDefs = list(list(className = 'dt-center', targets = 1:5)))) %>% 
      formatCurrency('total',currency = "", interval = 5, mark = ",", digits = 0) %>% 
      
      #formatStyle('entidad', target = "row", fontWeight = "bold") %>% 
      #formatStyle("subtipo.de.delito", color = styleEqual(c("Homicidio doloso"), c("#5F55A0"))) %>% 
      formatStyle(
        columns = c(1:5),
        fontFamily = "Century Gothic",
        #fontSize = "13px",
        #color = '#008080',
        fontWeight = 'plain',
        #paddingRight = "0.5em",
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        #borderCollapse = "collapse",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word"#,
        #backgroundColor = '#e6e6e5'
      )
    
    
  })
  
  
  
  
  # ---------------------------------------------------------------------------- #  
  
  output$victimas_año <- renderUI({
    selectInput("victimas_año",
                label =  "Seleccione el año",
                choices = sort(unique(victimas$Año)),
                multiple = T)
  })
  
  
  output$victimas_edad <- renderUI({
    selectInput("victimas_edad",
                label =  "Selecciona el municipio",
                choices = sort(unique(victimas$Rango.de.edad)),
                multiple = T)
  })
  
  
  output$victimas_delito <- renderUI({
    selectInput("victimas_delito",
                label =  "Selecciona el delito",
                choices = sort(unique(victimas$Subtipo.de.delito)),
                multiple = T)
  })
  
  #base reactiva para slide 3
  victimas_reactive <- reactive({
    
    victimas %>%
      filter(
        if(!is.null(input$victimas_año))                    Año %in% input$victimas_año          else Año != "",
        if(!is.null(input$victimas_edad))        Rango.de.edad %in% input$victimas_edad   else Rango.de.edad != "",
        if(!is.null(input$victimas_delito))   Subtipo.de.delito %in% input$victimas_delito       else Subtipo.de.delito != ""
        
        
      )
    
  })
  
  output$downloadData_victimas <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(victimas_reactive(), file, row.names = FALSE)
    }
  )
  
  output$grafico_victimas <- renderPlotly ({
    
    victimas_reactive() %>% 
      group_by(Año, Subtipo.de.delito, Sexo) %>% 
      filter(Subtipo.de.delito %in% c("Homicidio doloso", "Feminicidio"),
             Sexo=="Mujer", Entidad=="Jalisco") %>% 
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
                value=sum(ene+feb+mar+abr+ 
                            may+jun+jul+ago+
                            sep+oct+nov+dic),.groups = "drop") %>% 
      select(Año, Subtipo.de.delito, value) %>% 
      mutate(text = paste("Total de carpetas del delito: ", scales::comma(value), 
                          "\nAño: ", Año,
                          "\nDelito: ", Subtipo.de.delito, sep="")) %>% 
      ggplot(aes(x=as.factor(Año), y=value, fill=Subtipo.de.delito, text=text))+
      geom_col(position="dodge2") +
      scale_fill_manual(values =
                          c(Feminicidio = "#C91682",
                            `Homicidio doloso` = "#7E3794"))+
      scale_y_continuous(labels = scales::comma) +
      labs(x="", y="", fill="Delito", color="Delito")+
      theme_minimal()+
      theme(text=element_text(size=13,  family="Century Gothic"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11))->grafico_victimas
    
    ggplotly(grafico_victimas, tooltip = "text") %>%
      layout(
      #legend = list(orientation = "v", x = 0.1, y = -0.3),
      #        margin = list(b=0,t=30),
             title = paste0("Total de muertes violentas de mujeres \n", victimas_reactive()$Rango.de.edad, 
                            '</sup>',
                            '<br>'))
    
    
  })  
  
  
  
  
  ######################################################################3
  
  output$Regional_Periodo <- renderUI({
    selectInput("Periodo",
                label =  "Seleccione tipo de Periodo",
                choices = sort(unique(Regiones$Periodo)))
  })
  
  
  output$Regional_delito <- renderUI({
    selectInput("Delito",
                label =  "Seleccione tipo de delito",
                choices = sort(unique(Regiones$Subtipo.de.delito)))
  })
  
  output$Regional_Región <- renderUI({
    selectInput("Región",
                label =  "Seleccione alguna Región",
                choices = sort(unique(Regiones$Región)))
  })
  
  
  output$Regional_Año<- renderUI({
    selectInput("Año",
                label =  "Seleccione año",
                choices = sort(unique(Regiones$Año)))
  })
  
  output$Regional_Mes <- renderUI({
    selectInput("Región",
                label =  "Seleccione algún mes",
                choices = sort(unique(Regiones$mes)))
  })
  
  Regiones_data <- reactive({
    
    Regiones %>%
      filter(if(!is.null(input$Regional_Mes))                  mes %in% input$Regional_Mes     else mes != "",
             if(!is.null(input$Regional_Año))                  Año %in% input$Regional_Año     else Año != "",
             if(!is.null(input$Regional_delito)) Subtipo.de.delito %in% input$Regional_delito  else Subtipo.de.delito != "",
             if(!is.null(input$Regional_Región))            Región %in% input$Regional_Región  else Región != "")  
  })
  
  
  
  output$downloadData_regional <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(Regiones_data(), file, row.names = FALSE)
    })
  
  
  
  output$grafico_Regional<- renderPlotly({
    
    Regiones_data() %>% 
      ggplot()+ 
      aes(x=Periodo, y=total,
          fill=Región, colour = Región, group = Región,
          text=text)+
      geom_line(aes(x=Periodo, y=total),size=1) +
      geom_point(aes(x=Periodo, y=total), size=3)+
      labs(x="", y="Total"
           #title = paste0("Total de carpetas por el delito de \n"#, Regiones_data()$Subtipo.de.delito[1])
      ) +
      scale_fill_manual(values = mycolors) +
      scale_color_manual(values = mycolors)+
      scale_y_continuous(labels = scales::comma) + 
      # scale_x_discrete(breaks = c("2015-12","2015-03","2015-06","2015-09",
      #                             "2016-12","2016-03","2016-06","2016-09",
      #                             "2017-12","2017-03","2017-06","2017-09",
      #                             "2018-12","2018-03","2018-06","2018-09",
      #                             "2019-12","2019-03","2019-06","2019-09",
      #                             "2020-12","2020-03","2020-06","2020-09",
      #                             "2021-12","2021-03","2021-06","2021-09",
      #                             "2022-03","2022-06", "2022-09"))+
      labs(x="", y="Total de carpetas iniciadas")+
      theme_minimal()+
      theme(text=element_text(size=11,  family="Century Gothic"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11))->Regional
    
    ggplotly(Regional, tooltip="text") %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.8),
             margin = list(b=0,t=30), 
             title = paste0("Total de carpetas por el delito de \n" , Regiones_data()$Subtipo.de.delito[1])
             # title = paste0(Regiones_data()$Subtipo.de.delito[1])
      )
    
    
    
  })
  
  # output$test <- renderText({
  #   head(Regiones_data()$Periodo)
  # })
  # 
  
  output$regional_anual<- renderPlotly({
    
    Regiones_data() %>% 
      #Regiones %>%   
      group_by(Año, Región, Subtipo.de.delito) %>% 
      summarise(Total=sum(total, na.rm = T)) %>% 
      mutate(text = paste("Total de carpetas del delito: ", scales::comma(Total), 
                          "\nAño: ", Año,
                          "\nDelito: ", Subtipo.de.delito,
                          "\nRegión: ", Región, sep="")) %>% 
      ggplot() +
      aes(x =as.factor(Año), y = Total, fill= Región, text=text) +
      geom_col(position = "dodge")+
      labs(x="", y="Total de carpetas",
           #title = paste0("Total de carpetas por el delito de \n"#, Regiones_data()$Subtipo.de.delito[1])
      )+
      scale_fill_manual(values = mycolors) +
      scale_y_continuous(labels = scales::comma) + 
      theme_minimal()+
      theme(text=element_text(size=11,  family="Century Gothic"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11))->Regional_anual
    
    ggplotly(Regional_anual, tooltip="text") %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.4),
             margin = list(b=0,t=30),
             title = paste0("Total de carpetas por el delito de \n" , Regiones_data()$Subtipo.de.delito[1])
             # title = paste0(Regiones_data()$Subtipo.de.delito[1])
      )
    
    
    
  })
  
  
  ############################################################
  #                     MUNICIPAL
  ############################################################
  output$municipal_año <- renderUI({
    selectInput("Año",
                label =  "Seleccione uno o varios años",
                choices = sort(unique(municipios$Año)))
    
  })
  output$municipal_mes <- renderUI({
    selectInput("Mes",
                label =  "Seleccione uno o varios mes",
                choices = sort(unique(municipios$mes)))
    
  })
  output$municipal_delito <- renderUI({
    selectInput("municipal_delito",
                label =  "Seleccione tipo de delito",
                choices = sort(unique(municipios$Tipo.de.delito)))
  })
  output$municipal_municipio <- renderUI({
    selectInput("Municipio",
                label =  "Seleccione uno o varios municipios",
                choices = sort(unique(municipios$Municipio)))
    
  })
  
  #######################################################################33
  
  
  
  municipios_data <- reactive({
    
    municipios %>%
      filter(if(!is.null(input$municipal_año))                    Año %in% input$municipal_año        else Año != "",
             if(!is.null(input$municipal_mes))                    mes %in% input$municipal_mes        else mes != "",
             if(!is.null(input$municipal_delito))   Subtipo.de.delito %in% input$municipal_delito     else Subtipo.de.delito != "",
             if(!is.null(input$municipal_municipio))         Municipio %in% input$municipal_municipio  else Municipio != "")
      
  })
  
  
  output$downloadData_municipal <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(municipios_data(), file, row.names = FALSE)
    })
  
  output$grafico_municipal<- renderPlotly({
    
    municipios_data() %>% 
      mutate(text = paste("Total de carpetas del delito: ", scales::comma(Total), 
                          "\nAño: ", Año,
                          "\nMunicipio: ", Municipio,
                          "\nDelito: ", Subtipo.de.delito, sep="")) %>% 
      ggplot() +
      aes(x =as.factor(Año), y = Total, fill= Municipio, text=text) +
      geom_col(position = "dodge")+
      # geom_line(aes(x=Año, y=Total, colour=Municipio),size=1) +
      # geom_point(aes(x=Año, y=Total, colour=Municipio), size=3)+
      labs(x="Año", y="Total de carpetas")+
      #scale_color_brewer(palette = "mycolors")+
      #scale_fill_brewer(palette = "mycolors")+      
      scale_fill_manual(values = mycolors129) +
      #scale_color_manual(values = mycolors129)+
      scale_y_continuous(labels = scales::comma) +
 
      theme_minimal()+
      theme(text=element_text(size=11,  family="Century Gothic"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11))->municipal
    
    ggplotly(municipal, tooltip = "text") %>% 
      layout(title = paste0("Total de carpetas por el delito de \n" , municipios_data()$Subtipo.de.delito[1]),
             legend = list(orientation = "h", x = 0.1, y = -0.4),
             margin = list(b=0,t=30))
    
    
  })
  
  
  
  output$grafico_municipal_periodo<- renderPlotly({
    
    municipios_data() %>% 
      ggplot()+ 
      aes(x=Periodo, y=total,
          fill=Municipio, colour = Municipio, group = Municipio,
          text=text)+
      geom_line(aes(x=Periodo, y=total),size=1) +
      geom_point(aes(x=Periodo, y=total), size=3)+
      labs(x="", y="Total de carpetas"
           #title = paste0("Total de carpetas por el delito de \n"#, Regiones_data()$Subtipo.de.delito[1])
      ) +
      scale_fill_manual(values = mycolors) +
      scale_color_manual(values = mycolors)+
      scale_y_continuous(labels = scales::comma) + 
      # scale_x_discrete(breaks = c("2015-12","2015-03","2015-06","2015-09",
      #                             "2016-12","2016-03","2016-06","2016-09",
      #                             "2017-12","2017-03","2017-06","2017-09",
      #                             "2018-12","2018-03","2018-06","2018-09",
      #                             "2019-12","2019-03","2019-06","2019-09",
      #                             "2020-12","2020-03","2020-06","2020-09",
      #                             "2021-12","2021-03","2021-06","2021-09",
      #                             "2022-03","2022-06", "2022-09"))+
      labs(x="", y="Total de carpetas iniciadas")+
      theme_minimal()+
      theme(text=element_text(size=11,  family="Century Gothic"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11))->municipal_periodo
    
    ggplotly(municipal_periodo, tooltip="text") %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.8),
             margin = list(b=0,t=30), 
             title = paste0("Total de carpetas por el delito de \n" , municipios_data()$Subtipo.de.delito[1])
      )
    
    
    
  })
  
  
  ################################################################################
  #                              ATENCIONES
  ################################################################################
  
  
  output$atencion_año <- renderUI({
    selectInput("Año",
                label =  "Seleccione año",
                choices = sort(unique(atenciones$año)))
  })
  
  output$atencion_mes <- renderUI({
    selectInput("Mes",
                label =  "Seleccione mes",
                choices = sort(unique(atenciones$Mes)))
  })
  
  output$atencion_tipo <- renderUI({
    selectInput("Tipo",
                label =  "Seleccione tipo de violencia atendida",
                choices = sort(unique(atenciones$tipo)))
  })
  
  
  output$atencion_unidad <- renderUI({
    selectInput("Municipio",
                label =  "Seleccione la unidad de atención",
                choices = sort(unique(atenciones$Unidad)))
  })
  
  
  atenciones_reactive <- reactive({
    
    atenciones %>%
      filter(if(!is.null(input$atencion_año))           año %in% input$atencion_año      else año != "",
             if(!is.null(input$atencion_mes))           Mes %in% input$atencion_mes      else Mes != "",
             if(!is.null(input$atencion_tipo))         tipo %in% input$atencion_tipo     else tipo != "",
             if(!is.null(input$atencion_unidad))     Unidad %in% input$atencion_unidad   else Unidad != ""
      )
  })
  
  output$downloadData_municipal <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv( atenciones_reactive(), file, row.names = FALSE)
    })
  
  output$total_atenciones_grafico <- renderPlotly({
    
    atenciones_reactive() %>% 
      #atenciones %>% 
      filter(año >= 2022,
             mes != "2022-11") %>% 
      #   pivot_longer(
      #     cols=Psicológica:Digital,
      #     names_to = "tipo", 
      #     values_to = "total") %>% 
      group_by(mes) %>% 
      summarise(total=n(),.groups = "drop")%>% 
      mutate(text = paste("Total de atenciones: ", scales::comma(total), 
                          "\nPeríodo: ", mes, sep="")) %>% 
      ggplot() +
      aes(x =mes, y = total, text=text) +
      #geom_col()+
      geom_point(color="#C91682", size=6, alpha=0.6) + 
      geom_segment(aes(x=mes, xend=mes, y=0, yend=total, color="#C91682"))+
      scale_y_continuous(labels = scales::comma) +
      labs(title = paste("Total de mujeres atendidas"),
           #violencia_familiar_diario_reactive()$municipio[1]),
           x="", y="", color="", fill="")+
      #facet_grid(.~año, space = "free_x", scales = "free_x", switch="x") +
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(size=11,  family="Century Gothic"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=9))->grafico_total_atenciones
    
    ggplotly(grafico_total_atenciones, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.3),
             margin = list(b=0,t=30),
             title = paste("Total de atenciones realizadas \n" 
                            #,atenciones_reactive()$Unidad
                           ))
    
    
    
  }) 
  
  
  
  
  
  
  
  output$atencion_año2 <- renderUI({
    selectInput("Año",
                label =  "Seleccione año",
                choices = sort(unique(atenciones$año)))
  })
  
  output$atencion_mes2 <- renderUI({
    selectInput("Mes",
                label =  "Seleccione mes",
                choices = sort(unique(atenciones$Mes)))
  })
  
  output$atencion_tipo2 <- renderUI({
    selectInput("Tipo",
                label =  "Seleccione tipo de violencia atendida",
                choices = sort(unique(atenciones$tipo)))
  })
  
  
  output$atencion_unidad2 <- renderUI({
    selectInput("Unidad",
                label =  "Seleccione la unidad de atención",
                choices = sort(unique(atenciones$Unidad)))
  })
  
  
  atenciones_reactive2 <- reactive({
    
    atenciones %>%
      filter(if(!is.null(input$atencion_año2))           año %in% input$atencion_año2      else año != "",
             if(!is.null(input$atencion_mes2))           Mes %in% input$atencion_mes2      else Mes != "",
             if(!is.null(input$atencion_tipo2))         tipo %in% input$atencion_tipo2     else tipo != "",
             if(!is.null(input$atencion_unidad2))     Unidad %in% input$atencion_unidad2   else Unidad != ""
      )
  })
  
  
  output$downloadData_anual_violencia <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(atenciones_reactive2(), file, row.names = FALSE)
    })
  
  
  
  
  
  output$atenciones_mensuales<- renderPlotly({
    
    
    atenciones_reactive2() %>% 
      #atenciones %>%     
      filter(año >= 2022,
             mes != "2022-11") %>% 
      # pivot_longer(
      #   cols=Psicológica:Digital,
      #   names_to = "tipo", 
      #   values_to = "total") %>% 
      filter(total >=0) %>% 
      group_by(tipo)%>% 
      mutate(total= as.numeric(total)) %>% 
      summarise(total=sum(total, na.rm = T),.groups = "drop")%>% 
      mutate(text = paste("Total de atenciones: ", scales::comma(total), 
                          "\nTipo de violencia: ", tipo, sep="")) %>% 
      ggplot()+
      aes(x = reorder(tipo, total), y = total, fill=tipo, text=text) +
      geom_col() +
      scale_fill_manual(
        values = c(
          Psicológica = "#6737ab",
          Económica = "#88419d",
          Física = "#8c6bb1",
          Patrimonial ="#8c96c6",
          Sexual = "#9e9ac8",
          Digital="#bcbddc"))+  
      scale_y_continuous(labels = scales::comma) +
      labs(x="", y="")+
      coord_flip() +
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(size=11,  family="Century Gothic"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=9))->grafico_anteciones_mensuales
    
    ggplotly(grafico_anteciones_mensuales, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.3),
             margin = list(b=0,t=30),
             title = paste0("Total de atenciones realizadas por tipo de violencia",
                            #,atenciones_reactive()$Unidad, 
                            '</sup>',
                            '<br>'))
    
  })
  
  
  # - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - - - - -
  # - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - - - - -
  # - - - - - - - -  - - - - - - - -  - - - - - - - -  - - - - - - - -
  
  
  
  output$atencion_año3 <- renderUI({
    selectInput("Año",
                label =  "Seleccione año",
                choices = sort(unique(atenciones$año)))
  })
  
  output$atencion_mes3 <- renderUI({
    selectInput("Mes",
                label =  "Seleccione mes",
                choices = sort(unique(atenciones$Mes)))
  })
  
  output$atencion_tipo3 <- renderUI({
    selectInput("Tipo",
                label =  "Seleccione tipo de violencia atendida",
                choices = sort(unique(atenciones$tipo)))
  })
  
  
  output$atencion_unidad3 <- renderUI({
    selectInput("Municipio",
                label =  "Seleccione la unidad de atención",
                choices = sort(unique(atenciones$Unidad)))
  })
  
  
  atenciones_reactive3 <- reactive({
    
    atenciones %>%
      filter(if(!is.null(input$atencion_año3))           año %in% input$atencion_año3      else año != "",
             if(!is.null(input$atencion_mes3))           Mes %in% input$atencion_mes3      else Mes != "",
             if(!is.null(input$atencion_tipo3))         tipo %in% input$atencion_tipo3     else tipo != "",
             if(!is.null(input$atencion_unidad3))     Unidad %in% input$atencion_unidad3   else Unidad != ""
      )
  })
  
  
  output$downloadData_atenciones_violencia <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(atenciones_reactive3(), file, row.names = FALSE)
    })
  
  

  output$atenciones_tipo<- renderPlotly({
    
    atenciones_reactive3() %>% 
      filter(año >= 2022,
             mes != "2022-11") %>% 
      # pivot_longer(
      #   cols=Psicológica:Digital,
      #   names_to = "tipo", 
      #   values_to = "total") %>% 
      filter(total >=0) %>% 
      group_by(mes, tipo)%>% 
      mutate(total= as.numeric(total)) %>% 
      summarise(total=sum(total, na.rm = T),.groups = "drop")%>% 
      mutate(tipo=factor(tipo,
                         levels=c("Psicológica", "Económica", "Física", "Patrimonial", "Sexual", "Digital"))) %>% 
      filter(total !=0) %>% 
      mutate(text = paste("Total de atenciones: ", scales::comma(total), 
                          "\nTipo de violencia: ", tipo,
                          "\nPeríodo: ", mes, sep="")) %>% 
      ggplot()+
      aes(x=mes, y=tipo, 
          size=total, text=text)+  
      geom_point(mapping=aes(colour=tipo))+
      geom_text(aes(label=total, accuracy = 1),#hjust=.5, vjust=-.8,
                size=4.5, color="ghostwhite")+
      scale_y_discrete(limits = rev , 
                       labels = function(x) str_wrap(x, width = 15)) + 
      scale_size_continuous(range = c(2,20)) +
      scale_fill_manual(
        values = c(
          Psicológica = "#6737ab",
          Económica = "#88419d",
          Física = "#8c6bb1",
          Patrimonial ="#8c96c6",
          Sexual = "#9e9ac8",
          Digital="#bcbddc"))+
      scale_color_manual(
        values = c(
          Psicológica = "#6737ab",
          Económica = "#88419d",
          Física = "#8c6bb1",
          Patrimonial ="#8c96c6",
          Sexual = "#9e9ac8",
          Digital="#bcbddc"))+
      labs(title="", 
           x="", y="")+
      theme_minimal()+
      theme(legend.position = "none",
            legend.key.height= unit(1.3, 'cm'),
            legend.key.width= unit(1.3, 'cm'),
            legend.title = element_text(size=14),
            legend.text = element_text(size=12),
            text=element_text(size=12, family="Century Gothic"),
            plot.title = element_text(size = 18L, hjust = 0, family="Century Gothic"), 
            plot.caption = element_text(size = 12L, hjust = 0),
            strip.text.x = element_text(size = 11, color = "black", face = "bold.italic"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))->grafico_anteciones_tipo
    
    ggplotly(grafico_anteciones_tipo, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.3),
             margin = list(b=0,t=30),
             title = paste0("Total de atenciones realizadas\n", 
                            atenciones_reactive()$Unidad, 
                            '</sup>',
                            '<br>'))
    
    
    
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
