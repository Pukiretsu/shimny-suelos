library(shiny)
library(ggplot2)

#Clasificación SUCS
clasificar_SUCS <- function( tamiz4, tamiz200, LL, LP){
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
        is_subclase <- "Limpia"
        is_simbolo <- "GW o GP"
      } else {
        is_clase <- "Arena"
        is_subclase <- "Limpia"
        is_simbolo <- "SW o SP"
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
    if (LL <= 40 && IP <= 10) return("A-1-a")
    else return("A-1-b")
  } else if (tamiz200 <= 35 && tamiz10 <= 50 && tamiz40 <= 30) {
    return("A-3")
  } else if (tamiz200 <= 35 && tamiz10 <= 50) {
    return("A-2-4 o A-2-5")
  } else if (tamiz200 > 35) {
    if (LL <= 40 && IP <= 10) return("A-4")
    else if (LL <= 40 && IP > 10) return("A-6")
    else return("A-7")
  }
  
  return("No clasificado AASHTO")
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
                numericInput("tamiz1_1_2", "Tamiz 1 1/2'' (37.5mm)", min = 0, max = 100, value = 10),
                numericInput("tamiz1", "Tamiz 1'' (25mm)", min = 0, max = 100, value = 10),
                numericInput("tamiz3_4", "Tamiz 3/4'' (19mm)", min = 0, max = 100, value = 10),
                numericInput("tamiz1_2", "Tamiz 1/2'' (12.7mm)", min = 0, max = 100, value = 10),
                numericInput("tamiz4", "Tamiz #4 (4.75mm)", min = 0, max = 100, value = 10),
                numericInput("tamiz10", "Tamiz #10 (2.36mm)", min = 0, max = 100, value = 10),
                numericInput("tamiz40", "Tamiz #40 (0.425mm)", min = 0, max = 100, value = 10),
                numericInput("tamiz200", "Tamiz #200 (0.075mm)", min = 0, max = 100, value = 4),
                HTML('<hr class="gradient">'),
                numericInput("LL","Limite Liquido", min = 0, max = 100, value = 30),
                numericInput("LP","Limite Plastico", min = 0, max = 100, value = 20),

                actionButton("generate", "Generar resultados")
    ),

    mainPanel(id="main",
              h2("Resultados"),
              h3("Clasificación SUCS"),
              tableOutput("SUCS"),  
              h3("Clasificación AASHTO"),
              tableOutput("AASHTO"),
              plotOutput("curva")
    )
  )
)

# Server
server <- function(input, output) {
  
  datos <- eventReactive(input$generate, {
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
      LP = input$LP
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
      d$LP
    )

    data.frame(
      'Categoria' = class_SUCS$categoria,
      'Clase' = class_SUCS$clase,
      'Subclase' = class_SUCS$subclase,
      'Simbolo' = class_SUCS$simbolo 
    )
  })
  
  #Grafico de la curva
  output$curva <- renderPlot({
    
    req(datos())
    d <- datos()  
    



    tamiz <- c(37.5, 25, 19, 12.7 ,4.75, 2.36, 0.425, 0.075) #mm
    pasa <- c( d$tamiz1_1_2, d$tamiz1, d$tamiz3_4, d$tamiz1_2, d$tamiz4, d$tamiz10, d$tamiz40, d$tamiz200)
    
    df <- data.frame(tamiz, pasa)
    
    ggplot(df, aes(x = tamiz, y = pasa)) +
      geom_line(color = "blue", linewidth = 1.2) +
      geom_point(size = 3, color = "red") +
      scale_x_log10(breaks = tamiz, labels = tamiz) +
      scale_y_reverse() +
      labs(x = "Tamaño de partícula (mm) - Escala logarítmica",
           y = "% de suelo que pasa",
           title = "Curva Granulométrica") +
      theme_minimal(base_size = 14)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


