
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
library(tm)
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




# 
# drive_find(n_max = 6)
# 1
 
 # drive_download("codigo_violeta_bases.xlsx",overwrite = TRUE)
 # drive_download("reportes_llamadas_2021_2022.xlsx",overwrite = TRUE)
 # drive_download("reportes_llamadas_2019_2020.xlsx",overwrite = TRUE)
 # drive_download("medidas_ordenes",overwrite = TRUE)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
 
# reportes_llamadas_2021_2022 <- read_excel("reportes_llamadas_2021_2022.xlsx")
# reportes_llamadas_2019_2020 <- read_excel("reportes_llamadas_2019_2020.xlsx")
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
#   clasificación=="Violencia familiar"~ "Violencia familiar"))


# reportes_llamadas %>% group_by(year, cuatrimestre) %>% summarise(count=sum(count)) -> Nacional_total
# entidad<- c("Acumulado nacional")
# cbind(entidad, Nacional_total)->Nacional_total
# names(Nacional_total)[names(Nacional_total) == "...1"] <- "entidad"
# rbind(cd_total, Nacional_total)->cd_total
#
#write.csv(reportes_llamadas, "reportes_llamadas_2.csv")
  
st <- ymd("2019-01-01")
en <- ymd("2022-07-31")
dates <- seq.Date(from = st, to = en, by = 1)

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
      month=="septiembre" ~ "septiembre",
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
      zona=="Estado"~"Estado de Jalisco"
    ))
  # mutate(text = paste("Año: ", año,
  #                     "\nMes: ", mes,
  #                     "\nSexo: ", sexo,
  #                     "\nZona: ", zona,
  #                     "\nTotal de denuncias: ", comma(registro, accuracy = 1), sep=""))
  

#violencia_familiar_diario$fecha <-substr(violencia_familiar_diario$fecha, start = 1, stop = 10)
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
      month=="septiembre" ~ "septiembre",
      month=="octubre" ~ "Octubre",
      month=="noviembre" ~ "Noviembre",
      month=="diciembre" ~ "Diciembre"),
    month=factor(month,
                 levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio",
                          "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")))


#violencia_familiar_diario$registro <- abs(violencia_familiar_diario$registro)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#Ordenes y medidas



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






medidas_y_ordenes %>%  filter(tipo=="medidas", año==2019)->ordenes_2019
medidas_y_ordenes %>%  filter(tipo=="medidas", año==2020)->ordenes_2020
medidas_y_ordenes %>%  filter(tipo=="medidas", año==2021)->ordenes_2021
medidas_y_ordenes %>%  filter(tipo=="medidas", año==2022)->ordenes_2022



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
`Region Norte`<- c("Bolaños", "Chimaltitán", "Colotlán", "Huejúcar", "Huejuquilla el Alto",
                   "Mezquitic", "San Martín de Bolaños", "Santa María de los Ángeles", "Totatiche", "Villa Guerrero")

`Region Altos Norte`<- c("Encarnación de Díaz", "Lagos de Moreno", "Ojuelos de Jalisco",
                         "San Diego de Alejandría", "San Juan de los Lagos", "Teocaltiche",
                         "Unión de San Antonio", "Villa Hidalgo")

`Region Altos Sur`<- c("Acatic", "Arandas", "Cañadas de Obregón", "Jalostotitlán", "Jesús María",
                       "Mexticacán", "San Ignacio Cerro Gordo", "San Julián", "San Miguel el Alto",
                       "Tepatitlán de Morelos", "Valle de Guadalupe", "Yahualica de González Gallo")

`Region Ciénega`<- c("Atotonilco el Alto", "Ayotlán", "Degollado", "Jamay", "La Barca", "Ocotlán",
                     "Poncitlán", "Tototlán", "Zapotlán del Rey")


`Region Sureste`<- c("Chapala", "Concepción de Buenos Aires", "Jocotepec", "La Manzanilla de la Paz",
                     "Mazamitla", "Quitupan", "Santa María del Oro", "Tizapán el Alto",
                     "Tuxcueca","Valle de Juárez")


`Region Sur`<- c("Gómez Farías", "Jilotlán de los Dolores", "Pihuamo", "San Gabriel",
                 "Tamazula de Gordiano", "Tecalitlán", "Tolimán", "Tonila", "Tuxpan",
                 "Zapotiltic", "Zapotitlán de Vadillo", "Zapotlán el Grande")

`Region Sierra de Amula`<- c("Atengo", "Autlán de Navarro", "Ayutla", "Chiquilistlán",
                             "Cuautla", "Ejutla", "El Grullo", "El Limón", "Juchitlán",
                             "Tecolotlán", "Tenamaxtlán", "Tonaya", "Tuxcacuesco", "Unión de Tula")


`Region Costa Sur`<- c("Casimiro Castillo", "Cihuatlán", "Cuautitlán de García Barragán",
                       "La Huerta", "Tomatlán", "Villa Purificación")



`Region Costa-Sierra Occidental`<- c("Atenguillo", "Cabo Corrientes", "Guachinango",
                                     "Mascota", "Mixtlán", "Puerto Vallarta",
                                     "San Sebastián del Oeste", "Talpa de Allende")



`Region Valles`<- c("Ahualulco de Mercado", "Amatitán", "Ameca", "El Arenal",
                    "Etzatlán", "Hostotipaquillo", "Magdalena",
                    "San Juanito de Escobedo", "San Marcos", "Tala",
                    "Tequila", "Teuchitlán")


`Region Lagunas`<- c("Acatlán de Juárez", "Amacueca", "Atemajac de Brizuela",
                     "Atoyac", "Cocula", "San Martín Hidalgo", "Sayula",
                     "Tapalpa", "Techaluta de Montenegro", "Teocuitatlán de Corona",
                     "Villa Corona", "Zacoalco de Torres")


`Region Centro`<- c("Cuquío", "El Salto", "Guadalajara", "Ixtlahuacán de los Membrillos",
                    "Ixtlahuacán del Río", "Juanacatlán", "San Cristóbal de la Barranca",
                    "San Pedro Tlaquepaque", "Tlajomulco de Zúñiga", "Tonalá", "Zapopan",
                    "Zapotlanejo")




#Carpetas
regiones<-read.csv("IDM_NM_jun22.csv", encoding="latin1", check.names = T) %>% 
# #names(regiones)[names(regiones) == "A\xf1o"] <- "Año"
# names(regiones)[names(regiones) == "Subtipo de delito"] <- "Subtipo.de.delito"

# regiones<-regiones%>% 
  filter(Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual", "Hostigamiento sexual",
                                  "Violación simple", "Violación equiparada", "Feminicidio", 
                                  "Violencia familiar", 
                                  "Violencia de género en todas sus modalidades distinta a la violencia familiar"),
         Entidad=="Jalisco") %>% 
  mutate(Region = case_when(
    Municipio %in% `Region Norte` ~ "Region Norte",
    Municipio %in% `Region Altos Norte` ~ "Region Altos Norte",
    Municipio %in% `Region Altos Sur` ~ "Region Altos Sur",
    Municipio %in% `Region Ciénega` ~ "Region Ciénega",
    Municipio %in% `Region Sureste` ~ "Region Sureste",
    Municipio %in% `Region Sur` ~ "Region Sur",
    Municipio %in% `Region Sierra de Amula` ~ "Region Sierra de Amula",
    Municipio %in% `Region Costa Sur` ~ "Region Costa Sur",
    Municipio %in% `Region Costa-Sierra Occidental` ~ "Region Costa-Sierra Occidental",
    Municipio %in% `Region Valles` ~ "Region Valles",
    Municipio %in% `Region Lagunas` ~ "Region Lagunas",
    Municipio %in% `Region Centro` ~ "Region Centro")) %>% 
  group_by(Año, Region, Subtipo.de.delito) %>%
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



# - - - -  - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - -



municipios<-read.csv("IDM_NM_jun22.csv",check.names = T, encoding = "latin1") %>% 
# #names(municipios)[names(municipios) == "A\xf1o"] <- "Año"
# names(municipios)[names(municipios) == "Subtipo de delito"] <- "Subtipo.de.delito"
# 
# municipios<-municipios %>% 
  filter(Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual", "Hostigamiento sexual",
                                  "Violación simple", "Violación equiparada", "Feminicidio", 
                                  "Violencia familiar", 
                                  "Violencia de género en todas sus modalidades distinta a la violencia familiar"),
         Entidad=="Jalisco") %>% 
  mutate(Region = case_when(
    Municipio %in% `Region Norte` ~ "Region Norte",
    Municipio %in% `Region Altos Norte` ~ "Region Altos Norte",
    Municipio %in% `Region Altos Sur` ~ "Region Altos Sur",
    Municipio %in% `Region Ciénega` ~ "Region Ciénega",
    Municipio %in% `Region Sureste` ~ "Region Sureste",
    Municipio %in% `Region Sur` ~ "Region Sur",
    Municipio %in% `Region Sierra de Amula` ~ "Region Sierra de Amula",
    Municipio %in% `Region Costa Sur` ~ "Region Costa Sur",
    Municipio %in% `Region Costa-Sierra Occidental` ~ "Region Costa-Sierra Occidental",
    Municipio %in% `Region Valles` ~ "Region Valles",
    Municipio %in% `Region Lagunas` ~ "Region Lagunas",
    Municipio %in% `Region Centro` ~ "Region Centro")) %>% 
  group_by(Año, Municipio, Subtipo.de.delito) %>%
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






################################################################################

victimas <- read.csv("IDVFC_NM_jun22.csv", encoding="latin1", check.names = T)




ui <- shinyUI(
    tagList(
    includeCSS("./www/style.css"),
    fluidPage(
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
      navbarMenu(title = "Código violeta", icon = icon("dot-circle"),
        tabPanel(title = "Reportes 911",
                 tabsetPanel(
                   sidebarLayout(
                     sidebarPanel("Seleccione algunas características",
                                  
                                    # #Numeric Inputs
                                    # numericInput("slider", "Año inicial", min(reportes_llamadas$año), max(reportes_llamadas$año), step = 1 ,value=2019
                                    #              ),
                                    # numericInput("slider", "Año final", min(reportes_llamadas$año), max(reportes_llamadas$año),  step = 1, value=2022
                                    #             ),

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
                                  downloadButton("downloadData_llamadas", "Descarga (.csv)")),
                     mainPanel(h3(align="center", "Total de llamadas al 911",
                                  plotlyOutput("grafico_llamadas",  height = "auto", width = "auto"),
                                  h6("Datos propornionados por el C5"))
                   )))
                 
                 ),
        tabPanel(title = "Violencia familiar",
                 tabsetPanel(
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
                            downloadButton("downloadData_total", "\nDescarga (.csv)")),
               mainPanel(plotOutput("grafico_violencia_familiar"#,  height = "auto", #width = "auto"
                                    ),
                         h6("Fuente: Elaborado con datos de la Fiscalía Estatal."))
             ))),
        
        # - - - - - - - - -- - - - - - - - - - - - - - - - - - 
    tabPanel(title = "Medidas de protección",
             fluidRow(width=12,  
                      h3(align="center", "Medidas de protección trabajadas"),
             tabsetPanel(
               tabPanel("Total de medidas",  
                      box(width=12,
                          tabsetPanel(
                            #h2(align="center", "Medidas de protección trabajadas")
                            ),
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
                                       downloadButton("download_medidas", "Descarga (.csv)")),
                          mainPanel(plotlyOutput("grafico_medidas", height = "auto", width = "auto"),
                                    h6("Fuente: Elaborado con datos de la Fiscalía Estatal.")))),
               tabPanel("Mapa de medidas de protección",
                 #tags$br(),
                 column(12, align="center",
                        h2(""),
          
                        #h2("Total de medidas trabajadas en el estado de Jalisco, 2019 a 2022"),
                          #h6("Datos de la Fiscalía del Estado"),
                          selectInput("mapa_medidas", "Seleccione el año" ,
                                      choices = c("Año 2019", "Año 2020", 
                                                  "Año 2021", "Año 2022 (mayo)"),
                                      selected = "Año 2022 (mayo)",  multiple = FALSE, 
                                      selectize = TRUE),
                        # h3(text=paste0("Total de medidas de protección otorgadas: ", input$mapa_medidas ,
                        #           '<br>','<sup>',
                        #           'Datos de la Fiscalía del Estado de Jalisco ', align = "left")),
                        plotlyOutput("mapa_1",height = "auto", width = "auto"),
                        h5("Datos de la Fiscalía del Estado de Jalisco", align="left"),
                          h5("*La etiqueta del mapa 'value' hace referencia al valor de la tasa de homicidios dolosos de mujeres.", 
                             align="left", face="italic")))
             ))),
    
    #--------------------------------------------
    
    tabPanel(title = "Ordenes de protección",
                      fluidRow(width=12,  
                               h3(align="center", "Ordenes de protección emitidas"),
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
                                                           downloadButton("download_ordenes", "Descarga (.csv)")),
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
                                                                         "Año 2021", "Año 2022 (mayo)"),
                                                             selected = "Año 2022 (mayo)",  multiple = FALSE, 
                                                             selectize = TRUE),
                                                 # h3(text=paste0("Total de ordenes de protección otorgadas: ", input$mapa_ordenes ,
                                                 #           '<br>','<sup>',
                                                 #           'Datos de la Fiscalía del Estado de Jalisco ', align = "left")),
                                                 plotlyOutput("mapa_2",height = "auto", width = "auto"),
                                                 h5("Datos de la Fiscalía del Estado de Jalisco", align="left"),
                                                 h5("*La etiqueta del mapa 'value' hace referencia al valor de la tasa de homicidios dolosos de mujeres.", 
                                                    align="left", face="italic")))
                               ))),
             
    tabPanel(title = "Muertes violentas",
             column(12, align="center",
                    h2("Muertes violentas de mujeres"),
                    box(width = 8,
                        tabsetPanel(
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
                            mainPanel(h3(align="center", "Total de muertes violentas de mujeres",
                                         plotlyOutput("grafico_victimas",  height = "auto", width = "auto"),
                                         h6("Datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública, SESNSP")))))),
                    
                    box(width=4,
             mainPanel(dataTableOutput("table_muertes"))
             )
  ))),
  navbarMenu(
    title = "Incidencia delictiva",
    icon = icon("dot-circle"),
    tabPanel(title = "Municipal",
             
             tabPanel("Total por municipio",
                      sidebarLayout(
                        sidebarPanel("\nSeleccione algunas características",
                                     selectInput(
                                       inputId = "municipla_delito",
                                       label = "Delito",
                                       choices = sort(unique(municipios$Subtipo.de.delito)),
                                       multiple = F,
                                       selected = "Feminicidio"
                                     ),
                                     selectInput(
                                       inputId = "municipal_municipio",
                                       label = "Municipio",
                                       choices = sort(unique(municipios$Municipio)),
                                       multiple = T,
                                       selected = "Acatic"
                                       # options = list(
                                       #   `actions-box` = TRUE,
                                       #   `deselect-all-text` = "Sin selección filtro",
                                       #   `select-all-text` = "Seleccionar todos",
                                       #   `none-selected-text` = "Sólo los de la Region")
                                     )),
                        
                        #downloadButton("downloadData_total", "\nDescarga (.csv)")),
                        mainPanel(plotlyOutput("grafico_municipal"))))),
    
    
    ###############################33333333
    tabPanel(title = "Comparativa regional",
             tabPanel("Incidencia delictiva por regiones",
                      sidebarLayout(
                        sidebarPanel("nSeleccione algunas características",
                                     selectInput(
                                       inputId = "regional_delito",
                                       label = "Delito",
                                       choices = sort(unique(regiones$Subtipo.de.delito)),
                                       multiple = F,
                                       selected = "Feminicidio"
                                     ),
                                     selectInput(
                                       inputId = "regional_region",
                                       label = "Region",
                                       choices = sort(unique(regiones$Region)),
                                       multiple = T,
                                       selected = c("Región Centro","Region Altos Norte", "Región Lagunas")
                                     )),
                        #downloadButton("downloadData_total", "\nDescarga (.csv)")),
                        
                        
                        mainPanel(plotlyOutput("grafico_regional"))
                      ))
             
             )
  ),
  navbarMenu(
    title = "Unidades de atención",
    icon = icon("dot-circle"),
    tabPanel(title = "Perfil de mujeres atendidas"),
    tabPanel(title = "Perfil de unidades de atención")
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
 


  
  output$downloadData_llamadas <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(llamadas_reactive(), file, row.names = FALSE)
    })


  output$slider <- renderUI({
    sliderInput("Slider",
                min=input$min_val, value=2019,
                max=input$max_val, value=2022)
  })
  
  output$slider2 <- renderUI({
    sliderInput("Año final",
                min=input$min_val[1], value=2019,
                max=input$max_val[1], value=2022)
  })  
  
  output$llamadas_año <- renderUI({
    selectInput("llamadasr_año",
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
  
  
  output$grafico_llamadas <- renderPlotly ({
    
    reportes_llamadas_reactive() %>% 
      group_by(año, mes, clasificación) %>% 
      summarise(total=n()) %>% 
      ggplot() +
      aes(x = as.factor(mes), y = total, 
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
           x="Mes", y="Total de llamadas", fill="", color="")+
      theme_minimal()+
      theme(text=element_text(size=11,  family="Montserrat"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Montserrat"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10))->grafico_llamadas
    
    
    ggplotly(grafico_llamadas) %>%
      layout(title = list(text = paste0("",#"Total de denuncias por violencia familiar \n", 
                                        reportes_llamadas_reactive()$municipio,
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
  output$downloadData_total <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(violencia_familiar_diario_reactive(), file, row.names = FALSE)
    }
  )
  
  
  output$violencia_familiar_año <- renderUI({
    selectInput("violencia_familiar_año",
                label =  "Seleccione el año",
                choices = sort(unique(violencia_familiar_diario$año)),
                multiple = T)
  })
  
  # output$violencia_familiar_mes <- renderUI({
  #   selectInput("violencia_familiar_mes",
  #               label =  "Seleccione el mes",
  #               choices = sort(unique(violencia_familiar_diario$mes)),
  #               multiple = T)
  # })
  
  output$violencia_familiar_month <- renderUI({
    selectInput("violencia_familiar_month",
                label =  "Seleccione el mes",
                choices = sort(unique(violencia_familiar_diario$month)),
                multiple = T)
  })
  
  output$violencia_familiar_sexo <- renderUI({
    selectInput("violencia_familiar_sexo",
                label =  "Selecciona la zona",
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
        if(!is.null(input$violencia_familiar_mes))             mes %in% input$violencia_familiar_mes          else mes != "",
        if(!is.null(input$violencia_familiar_month))         month %in% input$violencia_familiar_month        else month != "",
        if(!is.null(input$violencia_familiar_zona))           zona %in% input$violencia_familiar_zona         else zona != "",
        if(!is.null(input$violencia_familiar_sexo))           sexo %in% input$violencia_familiar_sexo         else sexo != ""
      )
    
  })
  
  
  output$grafico_violencia_familiar <- renderPlot ({

    violencia_familiar_diario_reactive() %>% 
      group_by(mes,sexo) %>% 
      summarise(registro=sum(registro)) %>% 
    ggplot() +
      aes(x = as.factor(mes), y = registro, 
          fill = sexo, color = sexo, 
          group = 1
          ) +
      geom_point(size=3)+
      geom_line(size=1)+
      scale_color_manual(
        values = c(Mujeres = "#C91682",
                   Hombres = "#7E3794"))+
      scale_fill_manual(values =
                          c(Mujeres = "#C91682",
                            Hombres = "#7E3794"))+
      scale_y_continuous(labels = scales::comma) +
      labs(x="Mes", y="Total de denuncias", fill="Sexo", color="Sexo")+
      theme_minimal()+
      theme(text=element_text(size=11,  family="Montserrat"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Montserrat"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Montserrat"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=11))->grafico_violencia_familiar
    
    grafico_violencia_familiar
    # ggplotly(grafico_violencia_familiar) %>%
    #   layout(title = list(text = paste0("Total de denuncias por violencia familiar \n",
    #                                     violencia_familiar_diario_reactive()$zona,
    #                                    '<br>',
    #                                    '<sup>')),
    #         margin = list(b=0,t=50),
    #         xaxis = list(side = "bottom"),legend = list(side="bottom"))



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
              ordenes=sum(ordenes_aceptadas + ordenes_rechazadas)) %>% 
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
    ggplot() +
    aes(x =fecha, y = total, color="#C91682") +
    #geom_col()+
      geom_point(color="#C91682", size=4, alpha=0.6) + 
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
    #scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    labs(title = paste("Total de medidas trabajadas"),
         #violencia_familiar_diario_reactive()$municipio[1]),
         x="Mes", y="Total", fill="Tipo", color="Tipo")+
    #facet_grid(.~año, space = "free_x", scales = "free_x", switch="x") +
    theme_minimal()+
    theme(legend.position='none',
      text=element_text(size=11,  family="Montserrat"),
      plot.margin = margin(2, 2, 2, 2, "cm"),
      strip.text.x = element_text(size = 12, face = "bold", angle=90),
      plot.tag = element_text(size = 15L, hjust = 0, family="Montserrat"),
      plot.title = element_text(size = 15L, hjust = 0.5, family="Montserrat"),
      plot.caption = element_text(size = 12L, hjust = 0.5),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9))->gr_medidas
  
    ggplotly(gr_medidas) %>%
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
                             zoom = subset(medidas_2019, state_name %in% c("Jalisco"))$region,
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
              text=element_text(size=12, family="Montserrat"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Montserrat"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_1
    }
    
    if (input$mapa_medidas == "Año 2020") {
      
      mxmunicipio_choropleth(medidas_2020, num_colors = 1,
                             zoom = subset(medidas_2020, state_name %in% c("Jalisco"))$region,
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
              text=element_text(size=12, family="Montserrat"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Montserrat"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_1
    }
    
    if (input$mapa_medidas == "Año 2021") {
      
      mxmunicipio_choropleth(medidas_2021, num_colors = 1,
                             zoom = subset(medidas_2021, state_name %in% c("Jalisco"))$region,
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
              text=element_text(size=12, family="Montserrat"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Montserrat"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_1
    }
    
    if (input$mapa_medidas == "Año 2022 (mayo)") {
      
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
              text=element_text(size=12, family="Montserrat"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Montserrat"), 
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
  
  
  
  
  
  
  
  
  
  output$downloadData_total <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ordenes_reactive(), file, row.names = FALSE)
    }
  )
  
  
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
  
  
  output$grafico_ordenes <- renderPlotly ({
    
    
    ordenes_reactive() %>% 
      #medidas_ordenes_municipal %>% 
      group_by(año, mes) %>% 
      summarise(medidas=sum(medidas_aceptadas + medidas_rechazadas),
                ordenes=sum(ordenes_aceptadas + ordenes_rechazadas)) %>% 
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
      ggplot() +
      aes(x =fecha, y = total, color="7E3794") +
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
      #scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      labs(title = paste("Total de ordenes emitidas"),
           #violencia_familiar_diario_reactive()$municipio[1]),
           x="Mes", y="Total", fill="Tipo", color="Tipo")+
      #facet_grid(.~año, space = "free_x", scales = "free_x", switch="x") +
      theme_minimal()+
      theme(legend.position='none',
            text=element_text(size=11,  family="Montserrat"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Montserrat"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Montserrat"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=9))->gr_ordenes
    
    ggplotly(gr_ordenes) %>%
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
              text=element_text(size=12, family="Montserrat"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Montserrat"), 
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
              text=element_text(size=12, family="Montserrat"),
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
              text=element_text(size=12, family="Montserrat"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Montserrat"), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))-> mapa_2
    }
    
    if (input$mapa_ordenes == "Año 2022 (mayo)") {
      
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
        theme(legend.position = "bottom",
              legend.key.height= unit(.8, 'cm'),
              legend.key.width= unit(.8, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=11),
              text=element_text(size=12, family="Montserrat"),
              plot.title = element_text(size = 12L, hjust = 0.5, family="Montserrat"), 
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
    datatable(
      filter = 'top',
      colnames = c('Año', 'Feminicidios', 
                                  'Homicidios', 'Total','Variación'), 
      
      extensions = 'Buttons',
      options = list(
        
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#C91682', 'color': '#fff', align:'center'});","}"),
        
        dom = 'Blfrtip',
        buttons = c('copy', 'excel', 'print'),
        lengthMenu = list(c(8,1,4,8, "All"),
                          c(8,1,4,8, "All")),
        columnDefs = list(list(className = 'dt-center', targets = 1:5)))) %>% 
    formatCurrency('total',currency = "", interval = 3, mark = ",", digits = 0) %>% 
    
    #formatStyle('entidad', target = "row", fontWeight = "bold") %>% 
    #formatStyle("subtipo.de.delito", color = styleEqual(c("Homicidio doloso"), c("#5F55A0"))) %>% 
    formatStyle(
      columns = c(1:5),
      fontFamily = "Montserrat",
      fontSize = "11px",
      #color = '#008080',
      fontWeight = 'plain',
      paddingRight = "0.5em",
      borderRightWidth = "1px",
      borderRightStyle = "solid",
      borderRightColor = "white",
      borderBottomColor = "#ffffff",
      borderBottomStyle = "solid",
      borderBottomWidth = "0.5px",
      borderCollapse = "collapse",
      verticalAlign = "middle",
      textAlign = "center",
      wordWrap = "break-word"#,
      #backgroundColor = '#e6e6e5'
    )
  
  
  })
  
  
  
  
  # ---------------------------------------------------------------------------- #  
  output$downloadData_total <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(victimas_reactive(), file, row.names = FALSE)
    }
  )
  
  
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
                            sep+oct+nov+dic)) %>% 
      select(Año, Subtipo.de.delito, value) %>% 
      
      ggplot(aes(x=as.factor(Año), y=value, fill=Subtipo.de.delito))+
      geom_col(position="dodge2") +
      scale_fill_manual(values =
                          c(Feminicidio = "#C91682",
                            `Homicidio doloso` = "#7E3794"))+
      scale_y_continuous(labels = scales::comma) +
      labs(x="Mes", y="Total de denuncias", fill="Delito", color="Delito")+
      theme_minimal()+
      theme(text=element_text(size=11,  family="Montserrat"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Montserrat"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Montserrat"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11))->grafico_victimas
    
    ggplotly(grafico_victimas) %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.3),
             margin = list(b=0,t=30),
        title = paste0("Total de víctimas \n", victimas_reactive()$Rango.de.edad, 
                      '</sup>',
                      '<br>'))
    
    
  })  
  
  
  
  
  ######################################################################3
  
  
  
  
  output$regional_delito <- renderUI({
    selectInput("Delito",
                label =  "Seleccione tipo de delito",
                choices = sort(unique(regiones$Subtipo.de.delito)))
  })
  
  output$regional_region <- renderUI({
    selectInput("Region",
                label =  "Seleccione alguna region",
                choices = sort(unique(regiones$Region)))
  })
  
  
  
  regiones_data <- reactive({
    
    regiones %>%
      filter(if(!is.null(input$regional_delito))  Subtipo.de.delito %in% input$regional_delito     else Subtipo.de.delito != "",
             if(!is.null(input$regional_region))                      Region %in% input$regional_region              else Region != "")  
  })
  
  
  output$grafico_regional<- renderPlotly({
    
    ggplot(regiones_data()) +
      aes(x = as.factor(Año), y = Total, colour = Region) +
      geom_line(aes(x=Año, y=Total, colour=Region),size=1) +
      geom_point(aes(x=Año, y=Total, colour=Region), size=3)+
      scale_color_viridis_d() +
      labs(x="Año", y="Total")+
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()+
      theme(text=element_text(size=11,  family="Montserrat"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Montserrat"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Montserrat"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11))->regional
    
    ggplotly(regional)
    
    
    
  })
  
  
  ##########################################################33
  
  output$municipal_delito <- renderUI({
    selectInput("municipal_delito",
                label =  "Seleccione tipo de delito",
                choices = sort(unique(municipios$Subtipo.de.delito)))
  })
  output$municipal_municipio <- renderUI({
    selectInput("Municipio",
                label =  "Seleccione uno o varios municipios",
                choices = sort(unique(municipios$Municipio)))
    
  })
  
  #######################################################################33
  
  
  
  municipios_data <- reactive({
    
    municipios %>%
      filter(if(!is.null(input$municipal_delito))            Subtipo.de.delito %in% input$municipal_delito        else Subtipo.de.delito != "",
             if(!is.null(input$municipal_municipio))                 Municipio %in% input$municipal_municipio     else Municipio != "")
  })
  
  
  output$grafico_municipal<- renderPlotly({
    
    ggplot(municipios_data()) +
      aes(x =as.factor(Año), y = Total, colour = Municipio) +
      geom_line(aes(x=Año, y=Total, colour=Municipio),size=1) +
      geom_point(aes(x=Año, y=Total, colour=Municipio), size=3)+
      labs(x="Año", y="Total")+
      scale_color_viridis_d() +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()+
      theme(text=element_text(size=11,  family="Montserrat"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Montserrat"),
            plot.title = element_text(size = 15L, hjust = 0.5, family="Montserrat"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11))->municipal
    
    ggplotly(municipal)
    
    
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
