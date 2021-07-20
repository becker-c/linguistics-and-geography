library(readr)
library(ggplot2)
library(dplyr)
library(choroplethrAdmin1)
library(choroplethr)
library(choroplethrMaps)
library(shiny)

server <- function(input, output, session){
    
    test <- read_csv("test.csv", col_names = TRUE)    
    #    m_country <-  c("Select All", as.character(sort(unique(test$Country))))
    #    m_Name <-  c("Select All", as.character(sort(unique(test$Name))))
    data <- reactive({
        req(input$sel_Name)
        df <- test %>% group_by(Name %in% input$sel_Name) %>% 
            #filter(as.character(Name) %in% input$sel_Name) %>% 
            summarize(Country)
        #summarize(Number_of_Countries = n())
    })
    
    # update input dynamically
    observe({
        updateSelectInput(session, "sel_Name", choices = test$Name)
    })
    
    output$my_plot <- renderPlot({
        m_Country <- c("Select All", tolower(sort(unique(test$Country))))
        #        g <- ggplot(data(), aes( y = Number_of_Countries, x = Name))
        #        g + geom_bar(stat = "identity") + 
        #            theme(axis.text.x = element_text(angle = 90))
        g <- country_choropleth(data(), title = "Language by Country",
                                num_colors = 2, zoom = m_Country)    
    })
    
}

ui <- basicPage(
    h4(" Interactive Bar Chart Showing How Many Languages per Continent"),
    selectInput(inputId = "sel_Name", label = "Choose Name",
                "Names"),
    plotOutput(outputId = "my_plot")
    
)

shinyApp(ui = ui, server = server)