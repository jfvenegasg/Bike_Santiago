library(shiny)
library(bs4Dash)
library(Rcpp)
library(ggplot2)
# library(shinydashboard)
# library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjqui)
library(readxl)
library(dplyr)
library(plotly)
library(leaflet)
library(dplyr)
library(highcharter)
library(scales)

source("app/resumen.R")
source("app/opti_centro_50.R")
source("app/opti_centro_60.R")
source("app/opti_centro_70.R")
source("app/opti_borde_50.R")
source("app/opti_borde_60.R")
source("app/opti_borde_70.R")


shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(title = ""),
    sidebar = dashboardSidebar(width = "280px",
      # skin = "light",
      # inputId = "sidebarState",
      sidebarMenu(
        id = "sidebar",
        menuItem("Resumen Distancias",tabName="Data",
                 icon=icon("layer-group"),
                 selected = TRUE),
        menuItem(
          text = "Prio. Centro Red",
          icon = icon("truck"),
          startExpanded = FALSE,
          menuSubItem(
            text = "Amplitud de Inv. 50%",
            tabName = "dash_centro_50",
            icon = icon("dolly")
          ),
          menuSubItem(
            text = "Amplitud de Inv. 60%",
            tabName = "dash_centro_60",
            icon = icon("dolly")
          ),
          menuSubItem(
            text = "Amplitud de Inv. 70%",
            tabName = "dash_centro_70",
            icon = icon("dolly")
          )
        ),
        menuItem(
          text = "Prio. borde Red",
          icon = icon("truck"),
          startExpanded = FALSE,
          menuSubItem(
            text = "Amplitud de Inv. 50%",
            tabName = "dash_borde_50",
            icon = icon("dolly")
          ),
          menuSubItem(
            text = "Amplitud de Inv. 60%",
            tabName = "dash_borde_60",
            icon = icon("dolly")
          ),
          menuSubItem(
            text = "Amplitud de Inv. 70%",
            tabName = "dash_borde_70",
            icon = icon("dolly")
          )
        )
      )
    ),
    body = dashboardBody(
      tabItems(
        resumenUI("resumen"),
        opticentro50_UI("opticentro_50"),
        opticentro60_UI("opticentro_60"),
        opticentro70_UI("opticentro_70"),
        optiborde50_UI("optiborde_50"),
        optiborde60_UI("optiborde_60"),
        optiborde70_UI("optiborde_70")
      )
    ),
    # controlbar = dashboardControlbar(
    #   skin = "light",
    #   sliderInput(
    #     inputId = "controller",
    #     label = "Update the first tabset",
    #     min = 1,
    #     max = 6,
    #     value = 2
    #   )
    # ),
    #footer = bs4DashFooter()
  ),
  server = function(input, output, session) {
    
  callModule(resumenSV, "resumen")
  callModule(opticentro50_SV, "opticentro_50")
  callModule(opticentro60_SV, "opticentro_60")
  callModule(opticentro70_SV, "opticentro_70")
  callModule(optiborde50_SV, "optiborde_50")
  callModule(optiborde60_SV, "optiborde_60")
  callModule(optiborde70_SV, "optiborde_70")
   
  }
)
