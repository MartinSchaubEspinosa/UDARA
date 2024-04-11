library(shiny)
library(readxl)
library(dplyr)
library (magrittr)
library(ggplot2)
library(stringr)

colores=c("#004c93","#ffd344","#e63329","#007e47","#007693","#ffb144","#527e00","#e68429","#77aad9","#bd9206","#044f2e","#66120d")
d=read.csv2("https://cdn.dev.pregradouchile.cl/otros/udara/AnalisisPostulaciones.csv")
#d=read.csv2("Analisis Postulaciones.csv")
colnames(d)[26:83]=chartr("ÁÉÍÓÚÀÈÌÒÙÄËÏÖÃÕÇ", "AEIOUAEIOUAEIOAOC",colnames(d[,26:83]))
e=d %>% select(MRUN,COD_CARRERA_PREF)
e$U=str_sub(e$COD_CARRERA_PREF,1,2)
e %<>% select(-c(COD_CARRERA_PREF)) %>% distinct(MRUN,U) %>% filter(U==11)
d %<>% left_join(e,by="MRUN") 
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type="text/css", "#plot1, #plot2, #plot3 { width: 100%; height: 100%; }"),
    # Application title
    titlePanel("Postulaciones 2024"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectizeInput("carrera_u", "Seleccione la carrera que quiere analizar:",
                      choices = sort(colnames(d[26:83])),multiple = FALSE),
          textInput("otra_carrera", "Carrera de otra U:"),
          sliderInput("slider", 
                      "Selecciona un valor para que el gráfico muestre solo datos sobre el numero elegido", 
                      min = 0, 
                      max = 30, 
                      value = 0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Gráfico Universidades", plotOutput("plot1", height = "calc(100vh - 80px)", width = "100%")),
            tabPanel("Gráfico Carreras", plotOutput("plot2", height = "calc(100vh - 80px)", width = "100%")),
            tabPanel("Gráfico Colegios", plotOutput("plot3", height = "calc(100vh - 80px)", width = "100%")))
        )
    )
)
server <- function(input, output) {
# Define server logic required to draw a histogram
  output$plot1 <- renderPlot({
    
    d %>% 
      filter({d %>% select(input$carrera_u)}==1&
               str_detect(Carreras.o.Programas.Académicos,toupper(as.character(input$otra_carrera)))&
               str_sub(COD_CARRERA_PREF,1,2)!=11&
               is.na(U)) %>% 
      group_by(Universidad,ESTADO_PREF) %>% 
      summarise(Cantidad=n()) %>% 
      filter(Cantidad>input$slider) %>% 
      ggplot(aes(x=reorder(Universidad,Cantidad),y=Cantidad))+
      geom_col(fill=colores[1])+
      coord_flip()+
       facet_grid(~ESTADO_PREF)+
      geom_text(aes(label=Cantidad),position = position_stack(vjust = 0.5),color=colores[2])+
      theme(axis.text.y = element_text(size = 9))+labs(x="Universidad",y="Cantidad Estudiantes")
    #   
    
    
  })
  
  
  output$plot2 <- renderPlot({
  
  d %>% 
    filter({d %>% select(input$carrera_u)}==1&
             str_detect(Carreras.o.Programas.Académicos,toupper(as.character(input$otra_carrera)))&
             str_sub(COD_CARRERA_PREF,1,2)!=11&
             is.na(U)) %>% 
    group_by(carrera2,ESTADO_PREF) %>% 
    summarise(Cantidad=n()) %>% 
    filter(Cantidad>input$slider) %>% 
    ggplot(aes(x=reorder(carrera2,Cantidad),y=Cantidad))+
    geom_col(fill=colores[1])+
    coord_flip()+
      facet_grid(~ESTADO_PREF)+
     geom_text(aes(label=Cantidad),position = position_stack(vjust = 0.5),color=colores[2])+
     theme(axis.text.y = element_text(size = 7))+labs(x="Carrera",y="Cantidad Estudiantes")
#   
  
  
})
  
  output$plot3 <- renderPlot({
    
    d %>% 
      filter(!is.na(NOMBRE_OFICIAL)&{d %>% select(input$carrera_u)}==1&
               str_detect(Carreras.o.Programas.Académicos,toupper(as.character(input$otra_carrera)))&
               str_sub(COD_CARRERA_PREF,1,2)!=11&
               is.na(U)) %>% 
      group_by(NOMBRE_OFICIAL) %>% 
      summarise(Cantidad=n()) %>% 
      filter(Cantidad>input$slider) %>% 
      ggplot(aes(x=reorder(NOMBRE_OFICIAL,Cantidad),y=Cantidad))+
      geom_col(fill=colores[1])+
      coord_flip()+
      # #    facet_wrap(ESTADO_PREF)+
      geom_text(aes(label=Cantidad),position = position_stack(vjust = 0.5),color=colores[2])+
      theme(axis.text.y = element_text(size = 7))+labs(x="Colegio",y="Cantidad Estudiantes")
    #   
    
    
  })

}
# Run the application 
shinyApp(ui = ui, server = server)


