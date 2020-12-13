#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##############################################################################
############################## AUTORES: ######################################
######################### JORGE GÓMEZ BERENGUER ##############################
########################## RAÚL GÓMEZ SERRANO ################################

#Cargamos librerías
library(tidyverse)
library(shiny)
library(ggplot2)
#library(gridExtra)

#Cargamos los datos de las bases de datos que vamos a utilizar
datosFutbol = read.csv(file = "FMEL_Dataset.csv")
datosGanadores = read.csv(file = "La_Liga_Winners.csv")

#Hacemos que la columna Season sea común en ambas BD
datosFutbol$season = substr(datosFutbol$season, start = 1, stop = 4)
datosFutbol$season = as.numeric(datosFutbol$season)
datosGanadores$Season = substr(datosGanadores$Season, start = 1, stop = 4)
datosGanadores$Season = as.numeric(datosGanadores$Season)

#Vamos a comprobar que el rango de temporadas para ambas BD es el mismo
MinTempBD1 = min(datosFutbol$season)
MaxTempBD1 = max(datosFutbol$season)
MinTempBD2 = min(datosGanadores$Season)
MaxTempBD2 = max(datosGanadores$Season)

#Habría que quitar la segunda división de la tabla datosFutbol
datosFutbol = datosFutbol %>% filter(datosFutbol$division == 1)

#Hacemos el join entre las BD por la temporada jugada y generamos una nueva 
#base de datos que sea la unión de ambas
dataframe = inner_join(datosFutbol, datosGanadores, by=c("season" = "Season"))

#Comprobamos que los campos que tiene nuestra base de datos son del formato 
#que queremos para trabajar con ellos 
str(dataframe)

#Como vemos que hay problemas con ciertos caracteres, vamos a cambiar los 
#que nos van a dar problemas
dataframe = dataframe %>% mutate_all(funs(str_replace(., "Ã©", "e")))
dataframe = dataframe %>% mutate_all(funs(str_replace(., "Ã¡", "a")))
dataframe = dataframe %>% mutate_all(funs(str_replace(., "Ã±", "n")))
dataframe = dataframe %>% mutate_all(funs(str_replace(., "Ã³", "o")))
dataframe = dataframe %>% mutate_all(funs(str_replace(., "Ã", "i")))
dataframe = dataframe %>% mutate_all(funs(str_replace(., "i³", "o")))
dataframe = dataframe %>% mutate_all(funs(str_replace(., "iº", "u")))
dataframe = dataframe %>% mutate_all(funs(str_replace(., "i¼", "ü")))
str(dataframe)

#Tambien observamos que el nombre de algunos equipos no coincide en ambos dataframes,
#por lo que los igualamos
dataframe = dataframe %>% mutate_all(funs(str_replace(., "Espanol", "Espanyol")))
dataframe = dataframe %>% mutate_all(funs(str_replace(., "Athletic Bilbao", "Atletico de Bilbao")))
dataframe = dataframe %>% mutate_all(funs(str_replace(., "Real Betis", "Betis")))
dataframe = dataframe %>% mutate_all(funs(str_replace(., "Atletico Madrid", "Atletico de Madrid")))
dataframe = dataframe %>% mutate_all(funs(str_replace(., "Deportivo La Coruna", "Deportivo")))
dataframe = dataframe %>% mutate_all(funs(str_replace(., "Sporting Gijon", "Sporting de Gijon")))
dataframe = dataframe %>% mutate_all(funs(str_replace(., "Barcelona ", "Barcelona")))
dataframe = dataframe %>% mutate_all(funs(str_replace(., "Real Madrid ", "Real Madrid")))
dataframe = dataframe %>% mutate_all(funs(str_replace(., "Valencia ", "Valencia")))

#Cambiamos el formato de las columnas puesto que nos las ha cambiado a character
cols.int = c("localGoals", "visitorGoals", "Goals", "id","division", "round", "timestamp")
dataframe[cols.int] <- lapply(dataframe[cols.int], as.integer)
cols.num = c("season")
dataframe[cols.num] <- lapply(dataframe[cols.num], as.numeric)
str(datosFutbol)


#Creamos submatrices con diferentes niveles de agregación 
#En ellas obtendremos los resultados que posteriormente graficaremos en shiny

## 1. Base de datos de partidos jugados en 1ª División -> Datos por equipo
### 1.1 Identificamos los partidos ganados, perdidos y empate en local y visitante

#### Modo Test -> Seleccionamos un año determinado o un rango de años 
#### Hay que probar si en shiny se puede filtrar la matriz gorda dataframe
#### Y que afecte a los datos que de las nuevas bases de datos
dataframe <- dataframe
    #filter(season == 2015)
dataframe <- dataframe %>% 
    mutate(GanadorLocal = ifelse(localGoals > visitorGoals, 1, 0), 
           GanadorVisitante = ifelse(localGoals < visitorGoals, 1, 0), 
           PerdedorLocal = ifelse(localGoals < visitorGoals, 1, 0), 
           PerdedorVisitante = ifelse(localGoals > visitorGoals, 1, 0), 
           EmpateLocal = ifelse(localGoals == visitorGoals, 2, 0), 
           EmpateVisitante = ifelse(localGoals == visitorGoals, 2, 0))

### 1.2 Base de Datos con los resultados de los partidos jugados como local
datosEquiposPrimeraLocal <- dataframe %>%
    select(localTeam, GanadorLocal, PerdedorLocal, GanadorVisitante, EmpateLocal, season) %>%
    group_by(localTeam, season)  %>%
    summarise(GanadorLocal = sum(GanadorLocal), PerdedorLocal = sum(GanadorVisitante),
              EmpateLocal = sum(EmpateLocal)/2)

### 1.3 Base de Datos con los resultados de los partidos jugados como visitante 
datosEquiposPrimeraVisitante <- dataframe %>%
    select(visitorTeam, GanadorVisitante, PerdedorVisitante, GanadorLocal, EmpateVisitante, season) %>%
    group_by(visitorTeam, season) %>%
    summarise(GanadorVisitante = sum(GanadorVisitante), PerdedorVisitante = sum(GanadorLocal), 
              EmpateVisitante = sum(EmpateVisitante)/2)

### 1.4 Creamos la base de datos juntando los campos que hemos calculado en las DB anteriores
equiposPrimera <- data.frame(datosEquiposPrimeraLocal$localTeam, datosEquiposPrimeraLocal$season)
names(equiposPrimera)[1] <- "Equipo"
names(equiposPrimera)[2] <- "Season"

### 1.5 Valores absolutos del número de victorias, empates y derrotas 
### Tanto para local como visitante, así como número de partidos jugados 
datosEquiposPrimera_abs <- equiposPrimera %>%
    left_join(datosEquiposPrimeraLocal, by=c("Equipo" = "localTeam", "Season" = "season")) %>%
    left_join(datosEquiposPrimeraVisitante, by=c("Equipo" = "visitorTeam", "Season" = "season")) %>%
    group_by(Equipo, Season) %>%
    mutate(LocalPartidosGanados = GanadorLocal,
           VisitantePartidosGanados = GanadorVisitante, 
           LocalPartidosPerdidos = PerdedorLocal,
           VisitantePartidosPerdidos = PerdedorVisitante,
           LocalPartidosEmpate = EmpateLocal, 
           VisitantePartidosEmpate = EmpateVisitante,
           TotalPartidosGanados = sum(GanadorLocal + GanadorVisitante), 
           TotalPartidosPerdidos = sum(PerdedorLocal + PerdedorVisitante), 
           TotalPartidosEmpate = sum(EmpateLocal + EmpateVisitante),
           LocalPartidos = sum(GanadorLocal + PerdedorLocal + EmpateLocal), 
           VisitantePartidos = sum(GanadorVisitante + PerdedorVisitante + EmpateVisitante), 
           TotalPartidos = sum(TotalPartidosGanados + TotalPartidosPerdidos + TotalPartidosEmpate)) %>%
    select(Equipo, Season, LocalPartidosGanados, VisitantePartidosGanados, TotalPartidosGanados, 
           LocalPartidosPerdidos, VisitantePartidosPerdidos, TotalPartidosPerdidos, 
           LocalPartidosEmpate,VisitantePartidosEmpate, TotalPartidosEmpate,
           LocalPartidos, VisitantePartidos, TotalPartidos)

### 1.6 Porcentajes de victorias derrotas y empates sobre el total de partidos jugados 
### No tenemos en cuenta si juega en casa o fuera 
datosEquiposPrimera_percabs <- datosEquiposPrimera_abs %>%
    mutate(PercPartidosGanadosvsTotal = TotalPartidosGanados/TotalPartidos, 
           PercPartidosPerdidosvsTotal = TotalPartidosPerdidos/TotalPartidos,
           PercPartidosEmpatevsTotal = TotalPartidosEmpate/TotalPartidos) %>%
    select(Equipo, Season, PercPartidosGanadosvsTotal, PercPartidosPerdidosvsTotal,
           PercPartidosEmpatevsTotal)

### 1.7 Porcentajes de victorias, derrotas y empates como local y visitante sobre el total de partidos 
### Ej: El Real Madrid ha ganado el 31.1 % de los partidos totales como local y el 18.3% como visitante 
### La suma de los porcentajes de victorias como local y visitante deben de sumar el porcentaje de victorias 
### total que aparece en la tabla partidosPrimera_percabs
datosEquiposPrimera_percrelabs <- datosEquiposPrimera_abs %>%
    mutate(PercLocalPartidosGanadosvsTotal = LocalPartidosGanados/TotalPartidos,
           PercVisitantePartidosGanadosvsTotal = VisitantePartidosGanados/TotalPartidos, 
           PercLocalPartidosPerdidosvsTotal = LocalPartidosPerdidos/TotalPartidos,
           PercVisitantePartidosPerdidosvsTotal = VisitantePartidosPerdidos/TotalPartidos,
           PercLocalPartidosEmpatevsTotal = LocalPartidosEmpate/TotalPartidos, 
           PercVisitantePartidosEmpatevsTotal = VisitantePartidosEmpate/TotalPartidos) %>%
    select(Equipo, Season, PercLocalPartidosGanadosvsTotal, PercVisitantePartidosGanadosvsTotal,
           PercLocalPartidosPerdidosvsTotal,  PercVisitantePartidosPerdidosvsTotal, 
           PercLocalPartidosEmpatevsTotal, PercVisitantePartidosEmpatevsTotal)

### 1.8 Porcentajes de victorias, derrotas y empates como local y visitante sobre los partidos como local y visitante
### Ej: El Atlético de Madrid ha ganado el 62.3 %, ha perdido el 16.5% y el restante 21.1% de 
### los partidos como local han sido empates. Los porcentajes locales y visitantes deben de sumar el 100% 
datosEquiposPrimera_percrelrel <- datosEquiposPrimera_abs %>%
    mutate(PercLocalPartidosGanadosvsLocal = LocalPartidosGanados/LocalPartidos,
           PercVisitantePartidosGanadosvsVisitante = VisitantePartidosGanados/VisitantePartidos, 
           PercLocalPartidosPerdidosvsLocal = LocalPartidosPerdidos/LocalPartidos,
           PercVisitantePartidosPerdidosvsVisitante = VisitantePartidosPerdidos/VisitantePartidos,
           PercLocalPartidosEmpatevsLocal = LocalPartidosEmpate/LocalPartidos, 
           PercVisitantePartidosEmpatevsVisitante = VisitantePartidosEmpate/VisitantePartidos) %>%
    select(Equipo, Season, PercLocalPartidosGanadosvsLocal, PercVisitantePartidosGanadosvsVisitante,
           PercLocalPartidosPerdidosvsLocal,  PercVisitantePartidosPerdidosvsVisitante, 
           PercLocalPartidosEmpatevsLocal, PercVisitantePartidosEmpatevsVisitante)

### 1.9 Utilizamos ggplot para graficar lo que posteriormene va a mostrar shiny
#### 1.9.1 Plot con Top n equipos con más partidos jugados en primera con los valores de
#### partidos ganados, perdidos y empate 
n = 10
season = 2017
Valorestop_n <- datosEquiposPrimera_percabs %>%
    filter(Season == season) %>%
    group_by(Equipo, Season) %>%
    ungroup() %>%
    top_n(n, PercPartidosGanadosvsTotal) %>%
    gather(Partidos, value, PercPartidosGanadosvsTotal, PercPartidosPerdidosvsTotal,
           PercPartidosEmpatevsTotal)

ggplot(Valorestop_n, aes(x = Equipo, y = value, group = Partidos, color=Partidos)) + 
    geom_line()


# 2. Base de datos de partidos jugados en 1ª División -> Datos por partido
dataframe <- dataframe %>% 
    mutate(Partido = paste(dataframe$localTeam, "-", dataframe$visitorTeam))

partidosPrimera <- dataframe %>%
    select(Partido, localGoals, visitorGoals, season, round, GanadorLocal, GanadorVisitante, EmpateLocal) %>%
    group_by(Partido, season, round) %>%
    mutate(Empate = EmpateLocal/2, 
           GolesTotales = sum(localGoals + visitorGoals))

partidosPrimera <- partidosPrimera %>%
    select(-EmpateLocal)
    

#Usamos fluidPage para que el grafico varíe según los datos del input.
ui <- fluidPage(    
    
    #Damos a la pagina un titulo
    titlePanel("Trabajo Fundamentos Matemáticos del Análisis de Datos"),
    
    #Se muestra un menú superior para cambiar entre gráficas
    navbarPage("",
               # Generamos un panel.
               tabPanel(
                   "Gráfico 1",
                   sidebarLayout(
                       
                       # Definimos los input en el panel lateral.
                       sidebarPanel(
                           #Permite elegir el equipo.
                           selectInput(inputId = "equipo",
                                       label = "Equipo:",
                                       choices = dataframe$localTeam,
                                       selected = "Real Madrid"),
                           hr(), #Horizontal rule
                           
                           #Permite elegir el rango de temporadas que se quieren mostrar en el grafico.
                           sliderInput("rango", "Temporada:", min = 1970, max = 2017, value = c(1980, 2001))
                       ),
                       
                       #Creamos el barplot en el panel principal.
                       mainPanel(
                           plotOutput(outputId = "plotPartidos")  
                       )
                   )
               ),
               tabPanel("Gráfico 2",
                        sidebarLayout(
                            # Definimos los input en el panel lateral.
                            sidebarPanel(
                                #Permite elegir la temporada.
                                selectInput(inputId = "season_graf2",
                                            label = "Temporada:",
                                            choices = datosEquiposPrimera_percabs$Season,
                                            selected = "2000"),
                            ),
                            mainPanel(
                                plotOutput(outputId = "plot2")
                            )
                        )
                ),
               tabPanel("Gráfico 3",
                        sidebarLayout(
                            # Definimos los input en el panel lateral.
                            sidebarPanel(
                                #Permite elegir el equipo.
                                selectInput(inputId = "equipo3",
                                            label = "Equipo:",
                                            choices = dataframe$localTeam,
                                            selected = "Real Madrid"),
                            ),
                            mainPanel(
                                plotOutput(outputId = "plot3")
                            )
                        )
               )
    )
)

#El output va a ser la grafica, mientras que el input son los datos obtenidos de los inputs.
server <- function (input, output){
    
    output$plotPartidos <- renderPlot({
        
        #Calculamos el rango de fechas obtenido del sliderInput.
        rangoFechas = c(input$rango[1]:input$rango[2])
        
        calculoPartidos = c()
        
        #Recorremos las filas de la tabla.
        for (i in 1:nrow(dataframe)){
            #Si la temporada de un partido se encuentra en el rango obtenido
            if(dataframe$season[i] %in% rangoFechas){
                if(dataframe$localTeam[i] == input$equipo & dataframe$localGoals[i] > dataframe$visitorGoals[i]){
                    calculoPartidos = c(calculoPartidos, dataframe$season[i])
                }
                if(dataframe$visitorTeam[i] == input$equipo & dataframe$localGoals[i] < dataframe$visitorGoals[i]){
                    calculoPartidos = c(calculoPartidos, dataframe$season[i])
                }
            }
        }
        
        # Render histogram
        hist(calculoPartidos,
             breaks = rangoFechas,
             main = sprintf("Partidos ganados por %s en cada temporada", input$equipo),
             ylab = "Partidos ganados",
             xlab = "Temporada",
             col = "lightblue")
    })    
    output$plot2 <- renderPlot({
            
        #Hacemos un gráfico de columnas apiladas 
        n = 10
        Valorestop_n <- datosEquiposPrimera_percabs %>%
            filter(Season == input$season_graf2) %>%
            group_by(Equipo, Season) %>%
            ungroup() %>%
            top_n(n, PercPartidosGanadosvsTotal) %>%
            gather(Partidos, value, PercPartidosGanadosvsTotal, PercPartidosPerdidosvsTotal,
                   PercPartidosEmpatevsTotal)
        
        ggplot(Valorestop_n, aes(x = Equipo, y = value, group = Partidos, color=Partidos)) + 
            geom_line() +
            theme(axis.text.x = element_text(angle = 90)) +
            ggtitle (sprintf("Top 10 equipos con más partidos jugados en primera en la temporada %s \ny el número de partidos ganados, perdidos y empatados de cada uno", input$season_graf2))
    })
    output$plot3 <- renderPlot({
        #Goles en contra como local del equipo elegido
        valores_contra1 <- dataframe %>%
            select(season, localTeam, visitorGoals) %>%
            filter(localTeam == input$equipo3) %>%
            group_by(season, localTeam) %>%
            summarise(GolesContra = sum(visitorGoals))
        #Goles en contra como visitante del equipo elegido
        valores_contra2 <- dataframe %>%
            select(season, visitorTeam, localGoals) %>%
            filter(visitorTeam == input$equipo3) %>%
            group_by(season, visitorTeam) %>%
            summarise(GolesContra = sum(localGoals))
        #Goles en contra totales del equipo elegido
        valores_contra = inner_join(valores_contra1, valores_contra2, by=c("season" = "season"))
        
        valores_contra_final <- valores_contra %>%
            summarise(GolesContra = GolesContra.x + GolesContra.y)
        
        #Derrotas por temporada del equipo elegido
        derrotas_equipo <- datosEquiposPrimera_abs %>%
            select(Season, TotalPartidosPerdidos) %>%
            filter(Equipo == input$equipo3)
        
        #Goles en contra y derrotas del equipo elegido
        valores_contra_derrotas = inner_join(valores_contra_final, derrotas_equipo, by=c("season" = "Season"))
        
        #Gráfico que relaciona ambas variables
        ggplot(valores_contra_derrotas, aes(x=GolesContra, y=TotalPartidosPerdidos)) +
            geom_point() + geom_smooth() +
            ggtitle (sprintf("Relación entre los goles en contra y el número de derrotas del %s", input$equipo3))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
