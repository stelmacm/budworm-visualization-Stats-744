library(plotly)
library(ggplot2)
library(tidyverse)
library(gapminder)
library(gganimate)
library(shiny)
library(grid)
library(colorspace)

#Set up palette and import and data and factor
cbPalette <- rev(sequential_hcl(7, 'BurgYl'))

newdata <- readr::read_csv("https://raw.githubusercontent.com/stelmacm/budwormvisualizationStats744/master/datasets/ttm_tall2.csv")


newdata$prov <-sapply(newdata$prov, toupper)
newdata$prov <- factor(newdata$prov)
newdata$temp <- factor(newdata$temp)
newdata$individual <- factor(newdata$individual)
newdata$stage <- factor(newdata$stage)



#Create the Shiny App
ui <- fluidPage(
  titlePanel("Budworm Evolution over Time"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("prov", "Colony:",
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
  
  #segment the data and have the subsets be reactive
  target_data <- reactive({
    a <- subset(newdata, newdata$prov %in% input$prov)
    a <- droplevels(a)
    r <- subset(a, a$temp %in% input$temp)
    r$labs <- "df1"
    b <- subset(a, a$temp %in% input$temp1)
    b$labs <- "df2"
    d <- rbind(r,b)
    d <- droplevels(d)
    
    return(d)
  })
  
  # Reactive expression to create data frame of all input values 
  output$budwormplot <- renderImage({
    #create the horizontal line
    outfile <- tempfile('tempe.gif')
    df <- target_data()
    d1 <- (df
          %>% filter(labs=="df2" & (temp == input$temp1))
          %>% pull(individual)
          %>% as.numeric()
          %>% max()
    )
    #Create the graphic
    tempe <- ggplot(df, aes(progress, individual, group = individual, shape = temp)) + 
      geom_point(size = 2, aes(color = stage), stroke = 2) + 
      scale_color_manual(values = cbPalette)+
      scale_shape_manual(values = c(1,16)) +
      transition_reveal(day) + 
      coord_cartesian(clip = 'off') + 
      theme_classic() + 
      theme(plot.margin = margin(5.5, 40, 5.5, 5.5),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            text = element_text(size=15)) + 
      labs(colour = "Stage", x = "Larval Stage", y = "Budworm",
           title = "Development of Budworm Larvae Through Time", shape = "Temperature in °")+
      scale_x_continuous(breaks = 0:5, labels = c("L2", "L3","L4","L5","L6","Pupa")) +
      geom_hline(yintercept = d1, linetype = "dashed")
    
    #save the animation and plot it
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
 
