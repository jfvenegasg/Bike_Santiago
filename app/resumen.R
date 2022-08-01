
#### Interfaz Resumen ####
resumenUI <- function(id, label = "resumen") {
  ns <- NS(id)
  
  tabItem(tabName="Data",h4("Resumen Gestor logistico de bicicletas compartidas"),
          fluidRow(box(boxToolSize = "xs",width=12,status="primary",highchartOutput(ns("grafico_resumen1"),height = "250"),elevation = 4,collapsible = FALSE,title = "Distancia total recorrida segun porcentaje de priorizacion")),
                   #box(width=6,status="primary",highchartOutput(ns("grafico_resumen2")),elevation = 4,collapsible = FALSE,title = "Distancia total recorrida segun tipo de priorizacion")),
          fluidRow(box(height = "230px",width=6,status="info",tableOutput(ns("data_bike1")),elevation = 3,collapsible = FALSE,title = "Distancia total recorrida segun tipo de priorizacion"),
                   box(height = "230px",width=6,status="info",tableOutput(ns("data_bike2")),elevation = 3,collapsible = FALSE,title = "Diferencia de distancias segun tipo de priorizacion")))
  
  
  }

#### Servidor Resumen ####
resumenSV <- function(input, output, session) {

output$grafico_resumen1<-renderHighchart({ 
  
  
  borde<-c(9040.2,4070.6,2644.9)
  centro<-c(10500.6,4693.1,2670.6)
  porcentaje<-c("50%","60%","70%")
  data<-data.frame(porcentaje,borde,centro)
  
  
  highchart() %>%
    hc_add_series(data, type = "column",hcaes(x = porcentaje, y = centro),name="Opti. Centro",color="#FF6347") %>%
    hc_add_series(data, type = "column", hcaes(x = porcentaje, y = borde),name="Opti. Borde",color="#228B22") %>%
    hc_xAxis(categories = porcentaje) %>%
    hc_yAxis(
             gridLineColor = "#B71C1C",
             labels = list(format = "{value} Mts.", useHTML = TRUE))
    
    
  
  
  # p<-plot_ly(x=c("50%","60%","70%"),y=borde,name="Priorización Borde",data,type="scatter",mode="lines") %>%
  #   add_trace(y=centro,name="Priorización Centro") %>%
  #   layout(title="Resumen Escenarios Optimos")
  
})

output$grafico_resumen2<-renderHighchart({ 
  
  my_coma<-scales::label_comma(accuracy = .1, big.mark = ".", decimal.mark = ",")
  borde<-my_coma(c(9040.2,4070.6,2644.9))
  centro<-my_coma(c(10500.6,4693.1,2670.6))
  
  porcentaje<-c("50%","60%","70%")
  data<-data.frame(porcentaje,borde,centro)
  

  
  highchart() %>%
    hc_add_series(data, type = "spline",hcaes(x = porcentaje, y = centro),name="Opti. Centro",color="#FF6347") %>%
    hc_add_series(data, type = "spline", hcaes(x = porcentaje, y = borde),name="Opti. Borde",color="#228B22")
    
  
  # p<-plot_ly(x=c("50%","60%","70%"),y=borde,name="Priorización Borde",data,type="scatter",mode="lines") %>%
  #   add_trace(y=centro,name="Priorización Centro") %>%
  #   layout(title="Resumen Escenarios Optimos")
  
})


output$data_bike1<-renderTable({
  
  puntos_estaciones1<-read_excel("Data/tabla_resumen_escenario_app_latinr.xlsx",sheet = "Sheet1")
  #puntos_estaciones1<-data.frame(puntos_estaciones1)
  puntos_estaciones1
  
})

output$data_bike2<-renderTable({
  
  centro<-c(10500.6,4693.1,2670.6)
  borde<-c(9040.2,4070.6,2644.9)
  dif<-centro-borde
  
  puntos_estaciones1<-read_excel("Data/tabla_resumen_escenario_app_latinr.xlsx",sheet = "Sheet1")
  

  porcentaje<-c("50%","60%","70%")
  
  puntos_estaciones1[1,2:4]<-t(as.character.numeric_version(dif))
  puntos_estaciones1[1,1]<-c("Diferencia de distancias")
  puntos_estaciones1[1,2]<-paste(puntos_estaciones1[1,2],"Mts")
  puntos_estaciones1[1,3]<-paste(puntos_estaciones1[1,3],"Mts")
  puntos_estaciones1[1,4]<-paste(puntos_estaciones1[1,4],"Mts")
  puntos_estaciones1<-puntos_estaciones1[1,]
})


}