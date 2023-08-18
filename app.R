# Load libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

# Define UI
ui <- fluidPage(
  # Application title
  titlePanel(strong("Estima cuanto pedir por tu carro")),
  sidebarLayout(
    # Define fields to add to the UI
    sidebarPanel(
      p("Si no encuentras la marca y modelo es porque todavia no hay data suficiente para estimar el precio de venta"),
      inputPanel(selectInput("Modelo",label = "Selecciona la marca y modelo de tu carro",choices = modelos_finales[4] %>% arrange(Modelo_unico))),
      textInput('Anio', "Ingresa el año"),
      textInput("Kilometraje", "Ingresa el kilometraje"),
      textOutput("guia_parametros"),
      tableOutput("parametros_guia"),
      strong("Vender hoy o mantener por unos años mas?"),
      p("Si quieres evaluar la posibilidad de no vender tu carro hoy y esperar unos años te podemos ayudar a entender en cuanto lo podrias vender en los proximos años. Para esto necesitamos el recorrido promedio por año que realizas. La forma mas facil de calcular es dividir el kilometraje actual de tu carro entre su antiguedad. Otra opcion es que uses el recorrido promedio por año que es 10,000 km."),
      textInput("kilometraje_anual", "Ingresa el recorrido promedio anual", 10000),
      strong("Disclaimer:"),
      p('Los valores presentados en esta página web son únicamente referenciales y se basan en una estimación generada mediante técnicas de machine learning. Si bien hemos realizado nuestros mejores esfuerzos para brindar estimaciones precisas, debes tener en cuenta que estas estimaciones están sujetas a posibles errores y variaciones debido a la naturaleza del modelo utilizado. Por lo tanto, nos eximimos de cualquier responsabilidad por cualquier pérdida, daño o inconveniente que pueda surgir como resultado de confiar en estas estimaciones.'),
      p('Version experimental')
      ),
    mainPanel(
      h3(strong('Rango de precios')),
      plotOutput("resultado"),
      h3(strong('Interpretacion de los resultados')),
      p('Los valores mostrados son precios referenciales a los cuales publicar o empezar la negociacion para la venta del carro. Los 3 precios que hay representan la incertidumbre que existe al momento de definir el precio de un carro usado. Mientras mas amplio sea el rango entre el rango inferior y superior, mayor incertidumbe hay sobre el precio de venta.'),
      tableOutput("precio_esperar")
    )
    )
  )

# Define server logic
server <- function(input, output) {
  # Run model with inputs
  data_set <- reactive({data.frame(Anio = as.numeric(input$Anio), Kilometraje = as.numeric(input$Kilometraje))})
  precio_estimado <- reactive({as.data.frame(predict(lista_regresiones[[which(modelos_finales$Modelo_unico == input$Modelo)]], data_set(), interval = 'confidence', level = 0.99))})
  tabla <- reactive(pivot_longer(precio_estimado(),cols = everything()))
  output$resultado <- renderPlot({tabla() %>% ggplot(aes(x = name, y = value)) + geom_col() + geom_text(aes(label = round(value,0), vjust = -0.5), size = 5) +
      scale_x_discrete(labels = c('Precio promedio', 'Rango inferior', 'Rango superior')) +
      theme(panel.background = element_blank(),
            text = element_text(size=20),
            panel.grid.major = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 45))})
  
  # Provide user with parameters for estimation of price
  kilometraje_min <- reactive(base_final %>% filter(Modelo_unico == input$Modelo, Kilometraje > 0, Anio > 0) %>% summarise(Kilometraje_minimo = round(min(Kilometraje),0)))
  kilometraje_max <- reactive(base_final %>% filter(Modelo_unico == input$Modelo, Kilometraje > 0, Anio > 0) %>% summarise(Kilometraje_maximo = round(max(Kilometraje),0)))
  anio_min <- reactive(base_final %>% filter(Modelo_unico == input$Modelo, Kilometraje > 0, Anio > 0) %>% summarise(Año_minimo = round(min(Anio),0)))
  anio_max <- reactive(base_final %>% filter(Modelo_unico == input$Modelo, Kilometraje > 0, Anio > 0) %>% summarise(Año_maximo = round(max(Anio),0)))
  
  output$guia_parametros <- renderText('Para una mejor estimacion del precio venta, el kilometraje y año del carro que estas cotizando deberia caer en los rangos mostrados en la siguiente tabla:')
  output$parametros_guia <- renderTable({pivot_longer(data.frame(kilometraje_min(),kilometraje_max(),anio_min(),anio_max()), cols = everything())})
  
  #Prepare data for simulation of holding vs selling
  anios_test <- reactive(seq(as.numeric(input$Anio), as.numeric(input$Anio) - 5, -1))
  anios_futuros <- reactive(abs(anios_test() - as.numeric(input$Anio)))
  kilometrajes_futuros <- reactive(anios_futuros()*as.numeric(input$kilometraje_anual) + as.numeric(input$Kilometraje))
  data_test <- reactive(data.frame(Anio = anios_test(), Kilometraje = kilometrajes_futuros()))
  anios_finales <- reactive(data.frame(Anio_venta = (anios_futuros() + 2023)))
  
  #Choose regression model
  modelo_test <- reactive(lista_regresiones[[which(modelos_finales$Modelo_unico == input$Modelo)]])
  
  #Calculate prices for hold option
  Precio_1 <- reactive(predict(modelo_test(), data_test()[1,]))
  Precio_2 <- reactive(predict(modelo_test(), data_test()[2,]))
  Precio_3 <- reactive(predict(modelo_test(), data_test()[3,]))
  Precio_4 <- reactive(predict(modelo_test(), data_test()[4,]))
  Precio_5 <- reactive(predict(modelo_test(), data_test()[5,]))
  Precio_6 <- reactive(predict(modelo_test(), data_test()[6,]))
  Precios_futuros <- reactive(rbind(as.numeric(Precio_1()), as.numeric(Precio_2()), as.numeric(Precio_3()), as.numeric(Precio_4()), as.numeric(Precio_5()), as.numeric(Precio_6())))
  
  output$precio_esperar <- renderTable(data.frame(anios_finales(), Precios_futuros()))
}

# Run the application 
shinyApp(ui = ui, server = server)
