library(readr)
library(ggplot2)
library(dplyr)
library(choroplethrAdmin1)
library(choroplethr)
library(choroplethrMaps)
library(shiny)

# language is spoken in how many countries
server <- function(input, output, session){
    
    test <- read_csv("compiled3.csv", col_names = TRUE)    
    data <- reactive({
        browser()
        req(input$sel_Continent)
        df <- test %>% filter(Continent %in% input$sel_Continent) %>% 
            group_by(Name) %>% 
            summarize(Number_of_Countries = n())
        #summarize(Number_of_Countries = n())
    })
    
    # update input dynamically
    observe({
        updateSelectInput(session, "sel_Continent", choices = test$Continent)
    })
    
    output$my_plot <- renderPlot({
        g <- ggplot(data(), aes( y = Number_of_Countries, x = Name))
        g + geom_bar(stat = "identity", fill = "blue") + 
            theme(axis.text.x = element_text(angle = 90))
    })
    
}

ui <- fluidPage(
    h4(" Interactive Bar Chart Showing How Many Languages per Continent"),
    selectInput(inputId = "sel_Continent", label = "Choose Continent",
                "Names"),
    plotOutput(outputId = "my_plot")
    
)

shinyApp(ui = ui, server = server)
