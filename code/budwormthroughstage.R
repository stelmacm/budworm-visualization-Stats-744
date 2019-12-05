library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

budworms <- readr::read_csv("https://raw.githubusercontent.com/stelmacm/budwormvisualizationStats744/master/ttm_tall.csv")
head(budworms)
#Factor provinces and temperatures
budworms$prov <-sapply(budworms$prov, toupper)
budworms$prov <- factor(budworms$prov)
budworms$temp <- factor(budworms$temp)
budworms$individual <- factor(budworms$individual)
budworms$stage <- factor(budworms$stage)


#Create the Shiny App
ui <- fluidPage(
  titlePanel("Budworm Evolution over Time"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("prov", "Province:",
                  c("Ontario" = "ON",
                    "North West Territories" = "NWT",
                    "New Brunswick" = "NB",
                    "Quebec" = "QC")),
      br(),
      selectInput("temp", "Temperature:",
                  c("5" = "5",
                    "10" = "10",
                    "15" = "15",
                    "20" = "20",
                    "25" = "25",
                    "30" = "30",
                    "35" = "35"
                  ))
      
    ),               
    mainPanel(
      plotlyOutput("budwormplot", width = "100%", height = "450px")
    ),
  )
)

#Shiny Server
server <- function(input, output){
  
  #MAKE SOMETHING THAT RETURNS JUST THE RESPECTIVE PROVINCE AND TEMP
  target_data <- reactive({
    a <- subset(budworms, budworms$prov %in% budworms$prov)
    a <- droplevels(a)
    a <- subset(a, a$temp %in% input$temp)
    return(a)
  })
  
  # Reactive expression to create data frame of all input values ---
  output$budwormplot <- renderPlotly({
    
    
    attempt1 <- ggplot(target_data(), aes(day, individual,
                                          group = stage
                                          )) +
      geom_point(aes(frame = day, ids = individual, group = stage, color = stage)) +
      theme_classic() 
    
    
    graphicplot <- ggplotly(attempt1, tooltip ="") %>%
      layout(
        title = "",
        yaxis = list(
          title = "Larvae",
          ticks = "",
          showticklabels = FALSE
        ),
        xaxis = list(
          title = "Number of Days")) %>%
      animation_slider(
        currentvalue = list(prefix= "Day = ", font = list(color = "black"))
      ) %>%
      animation_opts(
        frame = 100, transition = 0
      )
    
    
  })
}

# Create Shiny app ----
shinyApp(ui, server)
