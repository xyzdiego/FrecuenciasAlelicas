# Carga los paquetes necesarios
if(!require(pacman)) install.packages("pacman")
pacman::p_load(shiny, DT, tidyverse)

# Define la interfaz de usuario
ui <- fluidPage(
    tags$head(
        tags$style(HTML("
            .shiny-output-error {
                visibility: hidden;
            }
            .shiny-output-error:before {
                visibility: visible;
                content: 'Cargando...';
            }
            .shiny-text-output {
                font-size: 10px;
            }
            .well {
                background-color: #99A3A4;
                width: 80%;
                margin: auto;
            }
        "))
    ),
    titlePanel("Shiny App con entradas numéricas, tabla de frecuencias y diagrama de barras"),
    
    sidebarLayout(
        sidebarPanel(
            wellPanel(
                titlePanel("Casos"),
                numericInput("Homocigotos1Casos", "Homocigotos 1:", value = 60),
                numericInput("Homocigotos2Casos", "Homocigotos 2:", value = 30),
                numericInput("HeterocigotosCasos", "Heterocigotos:", value = 10),
                height = 0.8
            ),
            wellPanel(
                titlePanel("Controles"),
                numericInput("Homocigotos1Controles", "Homocigotos 1:", value = 60),
                numericInput("Homocigotos2Controles", "Homocigotos 2:", value = 40),
                numericInput("HeterocigotosControles", "Heterocigotos:", value = 0),
                height = 0.8
            )
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Tabla de Frecuencias", DTOutput("freqTable")),
                tabPanel("Diagrama de Barras", plotOutput("barPlot"))
            )
        )
    )
)

# Define el servidor
server <- function(input, output) {
    data <- reactive({
        Fenotipos <- data.frame(Homocigotos1 = c(input$Homocigotos1Casos, input$Homocigotos1Controles),
                                Homocigotos2 = c(input$Homocigotos2Casos, input$Homocigotos2Controles),
                                Heterocigotos = c(input$HeterocigotosCasos, input$HeterocigotosControles))
        rownames(Fenotipos) <- c("Casos", "Controles")
        Alelos <- data.frame(matrix(ncol = 2, nrow = 2))
        rownames(Alelos) <- c("Casos", "Controles"); names(Alelos) <- c("Alelo1", "Alelo2")
        
        for (i in 1:nrow(Fenotipos)){
            if (Fenotipos$Heterocigotos[i] > 0){
                Alelos$Alelo1[i] <- Fenotipos$Homocigotos1[i] * 2 + Fenotipos$Heterocigotos[i]
                Alelos$Alelo2[i] <- Fenotipos$Homocigotos2[i] * 2 + Fenotipos$Heterocigotos[i]
            } else {
                Alelos$Alelo1[i] <- Fenotipos$Homocigotos1[i] * 2
                Alelos$Alelo2[i] <- Fenotipos$Homocigotos2[i] * 2
            }
        }
        Alelos
    })
    
    output$freqTable <- renderDT({
        df <- data()
        df$Total <- rowSums(df)
        df <- rbind.data.frame(df, colSums(df))
        row.names(df) <- c("Casos", "Controles", "Total")
        df
    })
    
    output$barPlot <- renderPlot({
        df <- data()
        df <- df %>%
            mutate(Grupo = c("Casos", "Controles")) %>% 
            select(3,1,2) %>% 
            gather(key = "Fenotipo", value = "Cantidad", 2:3)
        Casos <- factor(c(rep("Alelo1", df[1,3]), rep("Alelo2", df[3,3])))
        Controles <- factor(c(rep("Alelo1", df[2,3]), rep("Alelo2", df[4,3])))
        freqs <- data.frame(Grupo = c(rep("Casos", length(Casos)),
                                      rep("Controles", length(Controles))),
                            Alelo = c(Casos, Controles))
        p_value <- chisq.test(table(freqs$Grupo, freqs$Alelo))$p.value
        caption <- ifelse(p_value < 0.05,
                          paste("Se rechaza la hipótesis nula con un nivel de significancia de 0.05 y un p-valor de: ", p_value),
                          paste("No se puede rechazar la hipótesis nula con un nivel de significancia de 0.05 y un p-valor de: ", p_value))
        ggplot(df, aes(Fenotipo, Cantidad, fill = Fenotipo)) +
            geom_bar(stat = "identity") +
            facet_wrap(~Grupo) +
            scale_fill_manual(values = c("#A569BD", "#0097A7")) +
            theme_bw() +
            theme(legend.position = "none") +
            labs(caption = caption)
    })
}

# Ejecuta la aplicación
shinyApp(ui = ui, server = server)

