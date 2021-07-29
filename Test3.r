library(readr)
library(ggplot2)
library(dplyr)
library(choroplethrAdmin1)
library(choroplethr)
library(choroplethrMaps)
library(shiny)
library(plotly)


# language is spoken in how many countries
server <- function(input, output, session){
    
    test <- read_csv("compiled5.csv", col_names = TRUE)
    
    # Summarize the data
    data <- reactive({
        req(input$sel_Continent)
        df <- filter(test, Continent == input$sel_Continent, 
                     Syllable !=" ")
        df <- group_by(df, Syllable)
        df <- summarize(df, Number_of_Languages = n())
        return(df)
    })
    
    # Plot
    output$my_plot <- renderPlotly({      
        g <- ggplot(data(), aes( y = Number_of_Languages  , x = Syllable))
        p <- g + geom_bar(stat = "identity", fill = "blue") + 
             theme(axis.text.x = element_text(angle = 90))
        g1 <- ggplotly(p)
 
    })
}

ui <- basicPage(
    h4(" Interactive Bar Chart Showing Number of Languages using the Syllable"),
    selectInput(inputId = "sel_Continent", label = "Choose Continent",
                list("Asia", "Americas", "Europe", "Oceania", "Africa")),
    plotlyOutput(outputId = "my_plot")
    
)

shinyApp(ui = ui, server = server)