library(shiny)
library(ggplot2)

#Clasificación SUCS
clasificar_SUCS <- function( tamiz4, tamiz200, LL, LP, CU, CG){
  finos <- tamiz200
  grava <- 100 - tamiz4
  arena <- tamiz4 - tamiz200

  #Suelos Finos 
  if (finos > 50) {
    is_categoria <- "Finos"
    IP <- LL - LP
    if (LL < 50) {
      if (IP >= 4 && IP < 7) {
        is_clase <- "Arcilla-Limo"
        is_subclase <- "Zona límite"
        is_simbolo <- "CL/ML"
      } else if (IP >= 7) {
        is_clase <- "Arcilla"
        is_subclase <- "Baja plasticidad"
        is_simbolo <- "CL"
      } else {
        is_clase <- "Limo"
        is_subclase <- "Baja plasticidad"
        is_simbolo <- "ML"
      }
    } else {
      if (IP >= 4 && IP < 7) {
        is_clase <- "Arcilla-Limo"
        is_subclase <- "Zona límite"
        is_simbolo <- "CH/MH"
      } else if (IP >= 7) {
        is_clase <- "Arcilla"
        is_subclase <- "Alta plasticidad"
        is_simbolo <- "CH"
      } else {
        is_clase <- "Limo"
        is_subclase <- "Alta plasticidad"
        is_simbolo <- "MH"
      }
    }
  } else {
    # Suelos gruesos
    is_categoria <- "Gruesos"
    tipo_suelo <- ifelse(tamiz4 < 50, "grava", "arena")
    porcentaje_finos <- tamiz200
    
    if (porcentaje_finos <= 5) {
      if (tipo_suelo == "grava") {
        is_clase <- "Grava"
        is_subclase <- ifelse(CU > 4 && CG > 1 && CG < 3, "Bien Gradado", "Pobremente Gradado")
        is_simbolo <- ifelse(CU > 4 && CG > 1 && CG < 3, "GW", "GP")
      } else {
        is_clase <- "Arena"
        is_subclase <- ifelse(CU > 4 && CG > 1 && CG < 3, "Bien Gradado", "Pobremente Gradado")
        is_simbolo <- ifelse(CU > 6 && CG > 1 && CG < 3, "SW", "SP")
      } 
    } else if (porcentaje_finos > 12) {
      if (tipo_suelo == "grava") {
        if (LL - LP >= 7) {
          is_clase <- "Grava"
          is_subclase <- "Con Arcilla"
          is_simbolo <- "GC"
        } else {
          is_clase <- "Grava"
          is_subclase <- "Con Limo"
          is_simbolo <- "GM"
        } 
      } else {
        if (LL - LP >= 7) {
          is_clase <- "Arena"
          is_subclase <- "Con Arcilla"
          is_simbolo <- "SC"
        } else {
          is_clase <- "Arena"
          is_subclase <- "Con Limo"
          is_simbolo <- "SM"
        } 
      }
    } else {
      if (tipo_suelo == "grava") {
        is_clase <- "Grava"
        is_subclase <- "Zona límite"
        is_simbolo <- "GW-GM-GC"
      } else {
        is_clase <- "Arena"
        is_subclase <- "Zona límite"
        is_simbolo <- "SW-SM-SC"
      }
    }
  }
  return(list(categoria = is_categoria, clase = is_clase, subclase = is_subclase, simbolo = is_simbolo))
}
  
# Clasificación AASHTO
clasificar_AASHTO <- function(tamiz10, tamiz40, tamiz200, LL, LP) {
  IP <- LL - LP   # Índice plástico
  
  # Clasificaciones principales según porcentaje de finos
  if (tamiz200 <= 35) {
    is_clasificacion_gral <- "Material Granular"
    if (tamiz200 <= 10 && tamiz40 > 30 && tamiz10 > 50) { # A-3
      is_grupo <- "A-3"
      is_subgrupo <- "A-3"
    } else if (tamiz200 <= 25) { # A-1
      is_grupo <- "A-1"
      if (tamiz10 <= 50 && tamiz40 < 30 && tamiz200 <= 15) {
        is_subgrupo <- "A-1-a"
      } else if (tamiz10 > 50 && tamiz40 <= 50 && tamiz200 <= 25) {
        is_subgrupo <- "A-1-b"
      } else {
        is_subgrupo <- "Sin clasificar"
      }
    } else if (tamiz200 <= 35) { # A-2
      is_grupo <- "A-2"
      if (LL <= 40) {
        is_subgrupo <- ifelse (IP <= 10, "A-2-4", "A-2-6")
      } else {
        is_subgrupo <- ifelse (IP <= 10, "A-2-5", "A-2-7")
      }
    }
  } else { # condición cuando es material limo arcilloso
    is_clasificacion_gral <- "Material limo o arcilloso (fino)"
    if (LL <= 40) {
      is_grupo <- ifelse(IP <= 10, "A-4", "A-6")
      is_subgrupo <- ifelse(IP <= 10, "A-4", "A-6")
    } else {
      if(IP <= 10) {
      is_grupo <- "A-5"
      is_subgrupo <- "A-5"
      } else {
        is_grupo <- "A-7"
        comparacion <- LL - 30
        is_subgrupo <- ifelse(IP <= comparacion, "A-7-5", "A-7-6")
      }
    }
  }

  return(list(clasificacion_gral = is_clasificacion_gral, grupo = is_grupo, subgrupo = is_subgrupo))
}

# UI
ui <- fluidPage(
  tags$head(tags$style(
    HTML('#sidebar {
            background-color: white; } 
         #main {
            background-color: lightgrey;}
        hr.gradient {
  height: 3px;
  border: none;
  border-radius: 6px;
  background: linear-gradient(
    90deg,
    rgba(13, 8, 96, 1) 0%,
    rgba(9, 9, 121, 1) 21%,
    rgba(6, 84, 170, 1) 51%,
    rgba(0, 255, 113, 1) 100%
  );
}
         ')                  
  )),
  titlePanel("Clasificación de suelos - SUCS y AASHTO"),
  sidebarLayout(
    sidebarPanel(id="sidebar", 
                
                h5("Ingrese % pasa por cada tamiz:"),
                numericInput("tamiz1_1_2", "Tamiz 1 1/2'' (38.5mm)", min = 0, max = 100, value = 100),
                numericInput("tamiz1", "Tamiz 1'' (25.4mm)", min = 0, max = 100, value = 100),
                numericInput("tamiz3_4", "Tamiz 3/4'' (19mm)", min = 0, max = 100, value = 82.18),
                numericInput("tamiz1_2", "Tamiz 1/2'' (12.7mm)", min = 0, max = 100, value = 47.70),
                numericInput("tamiz4", "Tamiz #4 (4.75mm)", min = 0, max = 100, value = 6.79),
                numericInput("tamiz10", "Tamiz #10 (2.36mm)", min = 0, max = 100, value = 0.25),
                numericInput("tamiz40", "Tamiz #40 (0.425mm)", min = 0, max = 100, value = 0.25),
                numericInput("tamiz200", "Tamiz #200 (0.075mm)", min = 0, max = 100, value = 0.25),
                HTML('<hr class="gradient">'),
                numericInput("LL","Limite Liquido", min = 0, max = 100, value = 52),
                numericInput("LP","Limite Plastico", min = 0, max = 100, value = 4),

                actionButton("generate", "Generar resultados")
    ),

    mainPanel(id="main",
              h2("Resultados"),
              h3("Clasificación SUCS"),
              tableOutput("SUCS"),  
              verbatimTextOutput("cu_cg"),
              h3("Clasificación AASHTO"),
              tableOutput("AASHTO"),
              plotOutput("curva")
    )
  )
)

# Server
server <- function(input, output) {
  
  datos <- eventReactive(input$generate, {
    tamiz <- c(37.5, 25, 19, 12.7 ,4.75, 2.36, 0.425, 0.075) #mm
    pasa <- c(input$tamiz1_1_2, 
              input$tamiz1, 
              input$tamiz3_4, 
              input$tamiz1_2, 
              input$tamiz4, 
              input$tamiz10, 
              input$tamiz40, 
              input$tamiz200)

    porcentajes_a_interpolar <- c(10, 30, 60)
    interpolados <- approx(pasa, tamiz, xout = porcentajes_a_interpolar)
    cu_value <- interpolados$y[3]/interpolados$y[1]
    cg_value <- interpolados$y[2]^2/(interpolados$y[1]*interpolados$y[3])
    list(
      tamiz1_1_2 = input$tamiz1_1_2,
      tamiz1 = input$tamiz1,
      tamiz3_4 = input$tamiz3_4,
      tamiz1_2 = input$tamiz1_2,
      tamiz4 = input$tamiz4,
      tamiz10 = input$tamiz10,
      tamiz40 = input$tamiz40,
      tamiz200 = input$tamiz200,
      LL = input$LL,
      LP = input$LP,
      D10 = interpolados$y[1],
      D30 = interpolados$y[2],
      D60 = interpolados$y[3],
      CU = cu_value,
      CG = cg_value
    )
  })
  
  # Tabla SUCS
  output$SUCS <- renderTable({
    req(datos())
    d <- datos()
    class_SUCS <- clasificar_SUCS(
      d$tamiz4,
      d$tamiz200,
      d$LL,
      d$LP,
      d$CU,
      d$CG
    )

    data.frame(
      'Categoria' = class_SUCS$categoria,
      'Clase' = class_SUCS$clase,
      'Subclase' = class_SUCS$subclase,
      'Simbolo' = class_SUCS$simbolo 
    )
  })
  
    # Tabla AASHTO
  output$AASHTO <- renderTable({
    req(datos())
    d <- datos()

    class_AASHTO <- clasificar_AASHTO(
      d$tamiz10,
      d$tamiz40,
      d$tamiz200,
      d$LL,
      d$LP
    )

    data.frame(
      'Clasificación General' = class_AASHTO$clasificacion_gral,
      'Grupo' = class_AASHTO$grupo,
      'Subgrupo' = class_AASHTO$subgrupo
    )
  })

  #Grafico de la curva
  output$curva <- renderPlot({
    
    req(datos())
    d <- datos()  

    tamiz <- c(37.5, 25, 19, 12.7, 4.75, 2.36, 0.425, 0.075) # mm
    pasa <- c(d$tamiz1_1_2, d$tamiz1, d$tamiz3_4, d$tamiz1_2, d$tamiz4, d$tamiz10, d$tamiz40, d$tamiz200)

    df <- data.frame(tamiz, pasa)

    ggplot(df, aes(x = tamiz, y = pasa)) +
      geom_line(color = "blue", linewidth = 1.2) +
      geom_point(size = 3, color = "red") +
      scale_x_log10(breaks = tamiz, labels = tamiz) +
      scale_x_reverse() +  # <-- Esto invierte el eje X
      labs(x = "Tamaño de partícula (mm) - Escala logarítmica",
           y = "% de suelo que pasa",
           title = "Curva Granulométrica") +
      theme_minimal(base_size = 14)
  })

  output$cu_cg <- renderText({
    req(datos())
    d <- datos() 

    paste0("D10: ", sprintf("%.2f", d$D10), 
       ", D30: ", sprintf("%.2f", d$D30), 
       ", D60: ", sprintf("%.2f", d$D60), "\n",
       "Coeficiente de uniformidad: ", sprintf("%.2f", d$CU), "\n",
       "Coeficiente de gradación: ", sprintf("%.2f", d$CG))
  }) 
}

# Run the application
shinyApp(ui = ui, server = server)


