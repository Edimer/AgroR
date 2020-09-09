# ----- Bibliotecas -----
library(shiny)
library(tidyverse)
library(rlang)
library(DT)

# ----- Datos -----
load("../cotoveFinal.Rdata")
datos4 <- datos4 %>% 
  filter(edad_dest > 0) %>% 
  filter(peso_dest > 0) %>% 
  filter(peso_nto > 0) %>% 
  filter(ganancia_dia > 0)

shinyServer(function(input, output) {
  # Descriptivo ----
  funDescrip <- reactive({
    dataDescrip = datos4 %>% 
      filter(fecha_nac >= input$fecha1[1] & fecha_nac <= input$fecha1[2]) %>% 
      filter(raza %in% input$raza1) 
    
    if(input$tipo1 == "General"){
      dataDescrip %>% 
        select(peso_nto, peso_dest, ganancia_dia, edad_dest) %>% 
        gather(key = "Variable", value = "value") %>% 
        group_by(Variable) 
    } else if(input$tipo1 == "Sexo"){
      dataDescrip %>% 
        select(peso_nto, peso_dest, ganancia_dia, edad_dest, sexo) %>% 
        gather(key = "Variable", value = "value", -sexo) %>% 
        rename(Sexo = sexo) %>% 
        group_by(Variable, Sexo)
    } else if(input$tipo1 == "Raza") {
      dataDescrip %>% 
        select(peso_nto, peso_dest, ganancia_dia, edad_dest, raza) %>% 
        gather(key = "Variable", value = "value", -raza) %>% 
        rename(Raza = raza) %>% 
        group_by(Variable, Raza)
    } else {
      dataDescrip %>% 
        select(peso_nto, peso_dest, ganancia_dia, edad_dest, año_nac) %>% 
        gather(key = "Variable", value = "value", -año_nac) %>% 
        rename(Año = año_nac) %>% 
        group_by(Variable, Año)
    }
  })
  
  output$tabla <- renderDataTable({
    funDescrip() %>%  
      summarise(Promedio = round(mean(value, na.rm = TRUE), digits = 2),
                Mínimo = round(min(value, na.rm = TRUE), digits = 2),
                Máximo = round(max(value, na.rm = TRUE), digits = 2),
                DE = round(sd(value, na.rm = TRUE), digits = 2),
                CV = round((DE/Promedio)*100, digits = 2),
                N = n()) %>% 
      mutate(Variable = gsub("edad_dest", "Edad destete (días)", Variable),
             Variable = gsub("ganancia_dia", "Ganancia diaria (g)", Variable),
             Variable = gsub("peso_dest", "Peso destete (kg)", Variable),
             Variable = gsub("peso_nto", "Peso nacimiento (kg)", Variable)) %>% 
      datatable(rownames = FALSE,
                extensions = c("Buttons"),
                options = list(
                  dom = "Bfrtip",
                  buttons = c("pdf", "excel"),
                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                ))
  })
  
  
  # Gráfico -----
  output$grafico <- renderPlotly({
    if(input$tipo2 == "Boxplot"){
      datos4 %>% 
        mutate(año_nac = as.factor(año_nac)) %>% 
        filter(raza %in% input$raza2) %>% 
        ggplot(aes(x = año_nac, y = !!parse_quo(input$variable, env = global_env()))) +
        geom_boxplot() +
        labs(x = "Año") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 35, hjust = 1))
      
    } else if(input$tipo2 == "Tendencias"){
      datos4 %>% 
        mutate(año_nac = as.factor(año_nac)) %>% 
        filter(raza %in% input$raza2) %>% 
        group_by(año_nac) %>% 
        summarise(promedio = mean(!!parse_quo(input$variable, env = global_env()),
                                  na.rm = TRUE)) %>% 
        ggplot(aes(x = año_nac, y = promedio)) +
        geom_point(size = 2) +
        geom_line(group = 1) +
        labs(x = "Año", y = "Promedio") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 35, hjust = 1))
    }
  })
  
  
  
  
})
