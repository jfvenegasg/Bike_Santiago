archivo_60_borde<-"Data/escenario_borde_60_app_latinr.RData"


#### Interfaz opti borde ####
optiborde60_UI <- function(id, label = "optiborde_60") {
  ns <- NS(id)
  
  tabItem(tabName="dash_borde_60",h2("Resultados gestor logistico con priorizacion de borde al 60%"),
          fluidRow(box(title="Mapa de la red desbalanceada",leafletOutput(ns("mapa_borde_ini_60")),width=6,closable=TRUE,collapsible=TRUE),
                          box(title="Mapa de la red balanceada",leafletOutput(ns("mapa_borde_fin_60")),width=6,closable=TRUE,collapsible=TRUE)),
          
          fluidRow(
            valueBox(value = h4(textOutput(ns("est_def_ini_borde_60"))), "Estaciones deficientes", icon = icon("credit-card"),width = 3,color = "danger"),
            valueBox(value = h4(textOutput(ns("est_equi_ini_borde_60"))), "Estaciones equilibradas", icon = icon("credit-card"),width = 3,color = "primary"),
            valueBox(value = h4(textOutput(ns("est_def_fin_borde_60"))), "Estaciones deficientes", icon = icon("credit-card"),width = 3,color = "danger"),
            valueBox(value = h4(textOutput(ns("est_equi_fin_borde_60"))), "Estaciones equilibradas", icon = icon("credit-card"),width = 3,color = "primary")
            #column(width = 2)
          ),
          fluidRow(
            valueBox(value = h4(textOutput(ns("est_ex_ini_borde_60"))), "Estaciones saturadas", icon = icon("credit-card"),width = 3,color = "success"),
            column(width = 3),
            #column(width = 2),
            valueBox(value = h4(textOutput(ns("est_ex_fin_borde_60"))), "Estaciones saturadas", icon = icon("credit-card"),width = 3,color = "success"),
            valueBox(h4(textOutput(ns("ele_opt_3_60"))),"Distancia Total (Mts.)", icon = icon("credit-card"),width = 3,color = "info")
            #column(width = 2),
          ),
          # fluidRow(box(status="danger",title="Estaciones Iniciales",
          #                        width=4,
          #                        descriptionBlock(header=textOutput(ns("est_def_ini_borde_60")),text="Estaciones con deficiencia"),
          #                        descriptionBlock(header=textOutput(ns("est_equi_ini_borde_60")),text="Estaciones equilibradas"),
          #                        descriptionBlock(header=textOutput(ns("est_ex_ini_borde_60")),text="Estaciones con Exceso")),
          #                 box(title="Distancia de rebalanceo",width=4,closable=TRUE,collapsible=TRUE,status="danger",
          #                     descriptionBlock(header=textOutput(ns("ele_opt_3_60")),text="Distancia total(Mts.)")),
          #                 box(status="success",title="Estaciones Optimas",
          #                     width=4,
          #                     descriptionBlock(header=textOutput(ns("est_def_fin_borde_60")),text="Estaciones con deficiencia"),
          #                     descriptionBlock(header=textOutput(ns("est_equi_fin_borde_60")),text="Estaciones equilibradas"),
          #                     descriptionBlock(header=textOutput(ns("est_ex_fin_borde_60")),text="Estaciones con Exceso"))
          #                 
          # ),
           fluidRow(column(12,box(status="danger",title="Comparacion estaciones desbalanceadas y balanceadas",width=12,plotlyOutput(ns("borde_60_des_bal")))))
  )
  
  
}



#### Servidor opti borde ####
optiborde60_SV <- function(input, output, session){
  
  puntos_estaciones<-read_excel("Data/puntos_estaciones_latinr.xlsx",sheet = "Sheet1",range="c2:g22")
  
  #### Mapas borde ####
  output$mapa_borde_ini_60<-renderLeaflet({
    # puntos_estaciones<-read_excel("puntos_estaciones_latinr.xlsx",sheet = "Sheet1",range="c2:g22")
    puntos_estaciones<-data.frame(puntos_estaciones)
    
    map<-leaflet() %>%
      addProviderTiles(providers$Esri.WorldStreetMap,
                       options = providerTileOptions(noWrap = TRUE))
    alfa<-0.3
    prioridad_1<-1.05
    for(i in seq(1,20)){
      map<-addCircleMarkers(map,label=paste(puntos_estaciones$...1[i],"CAP:",puntos_estaciones$Capacidad[i],"INV:",puntos_estaciones$Inv..Lunes[i]),
                            labelOptions = labelOptions(noHide = F,
                                                        direction = "bottom",
                                                        style = list("font-size" = "10px",padding="3px 8px")),
                            lng=puntos_estaciones[i,2],lat=puntos_estaciones[i,3],
                            color=if(puntos_estaciones$Inv..Lunes[i]<(1-alfa)*prioridad_1*puntos_estaciones$Capacidad[i]/2){"red"}
                            else if(puntos_estaciones$Inv..Lunes[i]>=(1-alfa)*prioridad_1*puntos_estaciones$Capacidad[i]/2 & puntos_estaciones$Inv..Lunes[i]<=(1+alfa)*prioridad_1*puntos_estaciones$Capacidad[i]/2){"blue"}
                            else if(puntos_estaciones$Inv..Lunes[i]>(1+alfa)*prioridad_1*puntos_estaciones$Capacidad[i]/2){"green"},
                            fillColor = "blue",radius = 12,opacity = 10,clusterOptions=markerClusterOptions())
    }
    map
    
  })
  
  output$mapa_borde_fin_60<-renderLeaflet({
    # puntos_estaciones<-read_excel("puntos_estaciones_latinr.xlsx",sheet = "Sheet1",range="c2:g22")
    puntos_estaciones<-data.frame(puntos_estaciones)
    
    load(archivo_60_borde)
    map<-leaflet() %>%
      addProviderTiles(providers$Esri.WorldStreetMap,
                       options = providerTileOptions(noWrap = TRUE))
    alfa<-0.3
    prioridad<-1.05
    
    for(i in seq(1,20)){
      map<-addCircleMarkers(map,label=paste(puntos_estaciones$...1[i],"CAP:",puntos_estaciones$Capacidad[i],"INV:",result$x[i]),
                            labelOptions = labelOptions(noHide = F,
                                                        direction = "bottom",
                                                        style = list("font-size" = "10px",padding="3px 8px")),
                            lng=puntos_estaciones[i,2],lat=puntos_estaciones[i,3],
                            color=if(result$x[i]<(1-alfa)*prioridad*puntos_estaciones$Capacidad[i]/2){"red"}
                            else if(result$x[i]>=(1-alfa)*prioridad*puntos_estaciones$Capacidad[i]/2 & result$x[i]<=(1+alfa)*prioridad*puntos_estaciones$Capacidad[i]/2){"blue"}
                            else if(result$x[i]>(1+alfa)*prioridad*puntos_estaciones$Capacidad[i]/2){"green"},
                            fillColor = "blue",radius = 12,opacity = 10,clusterOptions=markerClusterOptions())
      
    }
    
    map
  })
  
  
  ## ESTACIONES INICIALES borde 60%
  output$est_def_ini_borde_60<-renderText({
    # puntos_estaciones<-read_excel("puntos_estaciones_latinr.xlsx",sheet = "Sheet1",range="c2:g22")
    puntos_estaciones<-data.frame(puntos_estaciones)
    alfa<-0.3
    prioridad_1<-1.05
    estaciones_con_deficiencia<-length(which((puntos_estaciones$Inv..Lunes<(1-alfa)*prioridad_1*(puntos_estaciones$Capacidad/2))==TRUE))
  })
  
  output$est_equi_ini_borde_60<-renderText({
    # puntos_estaciones<-read_excel("puntos_estaciones_latinr.xlsx",sheet = "Sheet1",range="c2:g22")
    puntos_estaciones<-data.frame(puntos_estaciones)
    alfa<-0.3
    prioridad_1<-1.05
    estaciones_equilibradas<-length(which((puntos_estaciones$Inv..Lunes>=(1-alfa)*prioridad_1*puntos_estaciones$Capacidad/2 & puntos_estaciones$Inv..Lunes<=(1+alfa)*prioridad_1*puntos_estaciones$Capacidad/2)==TRUE))
  })
  
  output$est_ex_ini_borde_60<-renderText({
    # puntos_estaciones<-read_excel("puntos_estaciones_latinr.xlsx",sheet = "Sheet1",range="c2:g22")
    puntos_estaciones<-data.frame(puntos_estaciones)
    alfa<-0.3
    prioridad_1<-1.05
    estaciones_con_exceso<-length(which((puntos_estaciones$Inv..Lunes>(1+alfa)*prioridad_1*puntos_estaciones$Capacidad/2)==TRUE))
  })
  
  
  ## ESTACIONES FINALES borde 60%
  output$est_def_fin_borde_60<-renderText({
    
    # puntos_estaciones<-read_excel("puntos_estaciones_latinr.xlsx",sheet = "Sheet1",range="c2:g22")
    load(archivo_60_borde)
    puntos_estaciones<-data.frame(puntos_estaciones)
    alfa<-0.3
    prioridad_1<-1.05
    estaciones_con_deficiencia<-length(which((result$x[1:20]<(1-alfa)*prioridad_1*(puntos_estaciones$Capacidad/2))==TRUE))
  })
  
  output$est_equi_fin_borde_60<-renderText({
    
    # puntos_estaciones<-read_excel("puntos_estaciones_latinr.xlsx",sheet = "Sheet1",range="c2:g22")
    load(archivo_60_borde)
    puntos_estaciones<-data.frame(puntos_estaciones)
    alfa<-0.3
    prioridad_1<-1.05
    estaciones_equilibradas<-length(which((result$x[1:20]>=(1-alfa)*prioridad_1*puntos_estaciones$Capacidad/2 & result$x[1:20]<=(1+alfa)*prioridad_1*puntos_estaciones$Capacidad/2)==TRUE))
    
  })
  
  output$est_ex_fin_borde_60<-renderText({
    
    # puntos_estaciones<-read_excel("puntos_estaciones_latinr.xlsx",sheet = "Sheet1",range="c2:g22")
    load(archivo_60_borde)
    puntos_estaciones<-data.frame(puntos_estaciones)
    alfa<-0.3
    prioridad_1<-1.05
    estaciones_con_exceso<-length(which((result$x[1:20]>(1+alfa)*prioridad_1*puntos_estaciones$Capacidad/2)==TRUE))
  })
  
  
  ### Graficos comparativos ####
  output$borde_60_des_bal<-renderPlotly({
    
    # puntos_estaciones<-read_excel("puntos_estaciones_latinr.xlsx",sheet = "Sheet1",range="c2:g22")
    puntos_estaciones<-data.frame(puntos_estaciones)
    
    load(archivo_60_borde)
    
    estaciones <- puntos_estaciones$...1
    capacidad<-puntos_estaciones$Capacidad
    inventario_desbalanceado<-puntos_estaciones$Inv..Lunes
    inventario_balanceado<-result$x[1:20]
    
    data <- data.frame(estaciones,inventario_desbalanceado,inventario_balanceado)
    
    p <- plot_ly(data, x = factor(estaciones,levels=estaciones), y = capacidad, type = 'bar', name = 'Capacidad') %>%
      add_trace(y = inventario_desbalanceado, name = 'Inventario desbalanceado') %>%
      add_trace(y = inventario_balanceado, name = 'Inventario balanceado') %>%
      layout(yaxis = list(title = 'Count'), barmode = 'group')
  })
  
  #### Costos de inventario ####
  
  output$ele_opt_3_60<-renderText({
    
    load(archivo_60_borde)
    my_coma<-scales::label_comma(accuracy = .1, big.mark = ".", decimal.mark = ",")
    my_coma(costo_instancia_1_1)
  })
  
}  