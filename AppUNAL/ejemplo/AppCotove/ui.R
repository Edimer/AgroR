# ----- Bibliotecas -----
library(shiny)
library(shinythemes)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(plotly)

# ----- Datos -----
load("cotoveFinal.Rdata")

shinyUI(fluidPage(
  
  theme = shinytheme("cosmo"),
  
  navbarPage(
    "App Cotové",
    tabPanel(
      "Descriptivo",
      sidebarLayout(
        sidebarPanel(
          dateRangeInput(
            inputId = "fecha1",
            label = "Fecha de nacimiento:",
            start  = min(datos4$fecha_nac, na.rm = TRUE),
            end = max(datos4$fecha_nac, na.rm = TRUE),
            language = "es"
          ),
          pickerInput(
            inputId = "raza1",
            label = "Raza:",
            choices = levels(datos4$raza),
            selected = levels(datos4$raza),
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "Ninguna",
              `select-all-text` = "Todas",
              `none-selected-text` = "Seleccione una o más razas",
              size = 5
            )
          ),
          radioButtons(
            inputId = "tipo1",
            label = "Resumen:",
            choices = c("General", "Sexo", "Raza", "Año"),
            selected = "General"
          )
        ),
        mainPanel(
          withSpinner(
            dataTableOutput(outputId = "tabla"),
            type = 8
          )
        )
      )
    ),
    tabPanel(
      "Variación Temporal",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "raza2",
            label = "Raza:",
            choices = levels(datos4$raza),
            selected = "CEB/100",
            multiple = FALSE
          ),
          prettyRadioButtons(
            inputId = "variable",
            label = "Variable:", 
            choices = names(datos4 %>% select(peso_nto, peso_dest, ganancia_dia, edad_dest)),
            icon = icon("check"), 
            bigger = TRUE,
            status = "danger"
          )
        ),
        mainPanel(
          radioGroupButtons(
            inputId = "tipo2",
            choices = c("Boxplot", "Tendencias"),
            status = "primary",
            checkIcon = list(
              yes = icon("ok", lib = "glyphicon"),
              no = icon("remove", lib = "glyphicon")),
            justified = TRUE
          ),
          withSpinner(
            plotlyOutput(outputId = "grafico"),
            type = 8
          )
        )
      )
    )
  )
))
