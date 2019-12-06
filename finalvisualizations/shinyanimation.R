library(plotly)
library(ggplot2)
library(tidyverse)
library(gapminder)
library(gganimate)
library(shiny)

cbPalette <- c( "#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

newdata <- readr::read_csv("https://raw.githubusercontent.com/stelmacm/budwormvisualizationStats744/master/datasets/ttm_tall2.csv")
head(newdata)

newdata$prov <-sapply(newdata$prov, toupper)
newdata$prov <- factor(newdata$prov)
newdata$temp <- factor(newdata$temp)
newdata$individual <- factor(newdata$individual)
newdata$stage <- factor(newdata$stage)
head(newdata)


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
                  )),
      br(),
      selectInput("temp1", "Temperature:",
                  c("5" = "5",
                    "10" = "10",
                    "15" = "15",
                    "20" = "20",
                    "25" = "25",
                    "30" = "30",
                    "35" = "35"
                  ), selected = "10")
      
    ),               
    mainPanel(
      imageOutput("budwormplot")
    ),
  )
)

#Shiny Server
server <- function(input, output){
  
  target_data <- reactive({
    a <- subset(newdata, newdata$prov %in% input$prov)
    a <- droplevels(a)
    r <- subset(a, a$temp %in% input$temp)
    b <- subset(a, a$temp %in% input$temp1)
    d <- rbind(r,b)
    d <- droplevels(d)
    return(d)
  })
  
  # Reactive expression to create data frame of all input values ---
  output$budwormplot <- renderImage({
    
    outfile <- tempfile('tempe.gif')
    
    tempe <- ggplot(target_data(), aes(progress, individual, group = individual, shape = temp)) + 
      geom_point(size = 2, aes(color = stage), stroke = 2) + 
      scale_color_manual(values = cbPalette[c(7, 2, 5, 4, 3, 6, 8)])+
      scale_shape_manual(values = c(1,16)) +
      transition_reveal(day) + 
      coord_cartesian(clip = 'off') + 
      theme_classic() + 
      theme(plot.margin = margin(5.5, 40, 5.5, 5.5),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            text = element_text(size=15)) + 
      labs(colour = "Stage", x = "Larval Stage Progression", y = "Budworm",
           title = "Development of Budworm Larvae Through Time", shape = "Temperature")+
      scale_x_continuous(breaks = 0:5, labels = c("L2", "L3","L4","L5","L6","Pupa"))
    
    
    anim_save("tempe.gif", tempe)
    
    list(src = "tempe.gif",
         contentType = 'gif', 
         width = 600,
         height = 600,
         alt = "This is alternate text"
    )}, deleteFile = FALSE)
}

# Create Shiny app ----
shinyApp(ui, server)
