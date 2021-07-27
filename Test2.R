library(readr)
library(ggplot2)
library(dplyr)
library(choroplethrAdmin1)
library(choroplethr)
library(choroplethrMaps)
library(shiny)
library(scales)

server <- function(input, output, session){
    
    test <- read_csv("compiled4.csv", col_names = TRUE)
    data <- reactive({
        req(input$sel_Name)
        
        df <- filter(test, Name == input$sel_Name)
        df <- group_by(df, Name)
        df <- select(df, Name, region, value)
        return(df)
        # df <- test %>% filter(compiled4, Name == sel_Name) %>%
        #     select(region, value)
    })
    
    # update input dynamically
    observe({
        updateSelectInput(session, "sel_Name", choices = test$Name)
    })
    
    output$my_plot <- renderPlot({
        g <- suppressWarnings(
            country_choropleth(data(),title = "Countries by Language") + 
            scale_fill_discrete(na.value = "grey"))
        g 
    })
    
}

ui <- basicPage(
    h4(" Interactive C-Map Showing Countries, by Language"),
    selectInput(inputId = "sel_Name", label = "Choose Name", "Name"),
    plotOutput(outputId = "my_plot")
    
)

shinyApp(ui = ui, server = server)
