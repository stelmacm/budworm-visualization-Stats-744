library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(imputeTS)

sims.all.df <- readr::read_csv("https://raw.githubusercontent.com/stelmacm/budwormvisualizationStats744/master/datasets/sd_data.csv")

prov.names <- c('in','qc','nb','on','ip')
stages <- c('L2o', 'L2', 'L3', 'L4', 'L5', 'L6')
prov.labs <- c('Northwest Territories', 'Quebec', 
               'New Brunswick', 'Ontario', 'Lab-Reared')



cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sadf.tt.lst <- lapply(1:nrow(sims.all.df), function(x) {
  row <- sims.all.df[x,]
  d <- as.character(row$date)
  p <- as.character(row$prov)
  ss <- subset(sims.all.df, date == d & prov == p)
  props <- ss$proportion
  nms <- as.character(ss$stage)
  names(props) <- nms
  row <- c(row, props)
  row <- as.data.frame(row)
  return(row)
})

sadf.tt <- do.call('rbind', sadf.tt.lst)

sadf.tt[,c(stages, 'Pupa')] <- round(sadf.tt[,c(stages, 'Pupa')], 2)

names(sadf.tt)[1] <- 'Date'
sadf.tt$Province <- toupper(sadf.tt$prov)

ui <- fluidPage(
  titlePanel("Stacked Density Plot for the Inputted Year"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Year:",
                  c("2013","2014","2015","2016","2017","2018")),
      
    ),               
    mainPanel(
      plotlyOutput("stacked")
    ),
  )
)

server <- function(input, output){
  
  
  output$stacked <- renderPlotly({
    g <- ggplot(data=subset(sadf.tt, year == input$year), 
                aes(x=Date, y = proportion, group=stage, fill=stage,
                    lab1 = L2o, lab2 = L2, lab3 = L3, lab4 = L4,
                    lab5 = L5, lab6 = L6, lab7 = Pupa, lab8 = Province)) +
      scale_fill_manual(values = cbPalette[c(7, 2, 5, 4, 3, 6, 8)]) +
      geom_density(position="fill", stat = 'identity', lwd = 0.05) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
      #facet_wrap(~prov, scales = 'free_x', ncol = 1, 
      #           labeller = labeller(prov = prov.labs)) +
      facet_grid(prov~., scales = 'free_x',  
                 labeller = labeller(prov = prov.labs)) +
      labs(y = 'Proportion of Population in Stage', 
           fill = 'Larval Stage', x = 'Date', 
           title = 'Spruce Budworm Development by Location') +
      guides(alpha = FALSE)
    
    ggplotly(g, tooltip = c("Date", "Province", stages, "Pupa")) %>%
      layout(xaxis = list(showspikes = TRUE, spikedash = 'solid', 
                          spikemode = 'across', spikesnap = 'cursor',
                          spikethickness = 1, spikecolor = 'black'),
             hoverlabel = list(font = list(size = 10)))
  })
}

shinyApp(ui, server)
