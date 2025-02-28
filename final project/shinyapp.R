library(tidyverse)
library(janitor)
library(gtools)
library(sf)
library(ggmap)
library(shiny)

register_stadiamaps("1d92513d-1a7f-407d-8c65-60d0d510c9c0", write = FALSE)

tf_ps <- read_csv("data/landscapegenetics/genomics_tf_ps.csv")

latitude_tf <- c(28.01, 28.58)
longitude_tf <- c(-16.92, -16.18)
tf_bbox <- make_bbox(longitude_tf, latitude_tf, f = 0.05)
tf_map <- get_stadiamap(tf_bbox, maptype = "stamen_terrain", zoom=6)

tf_coordinates <- tf_ps %>% 
  filter(island == "TF")

latitude_ps <- c(33.03, 33.10)
longitude_ps <- c(-16.39, -16.3)
ps_bbox <- make_bbox(longitude_ps, latitude_ps, f = 0.05)
ps_map <- get_stadiamap(ps_bbox, maptype = "stamen_terrain", zoom=13)

ps_coordinates <- tf_ps %>% 
  filter(island == "PS")




ui <- dashboardPage(
  dashboardHeader(title = "Birds of Porto Santo and Tenerife"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Maps", 
               tabName = "dashboard", 
               icon = icon("dashboard")),
      
      menuItem("Counts", 
               tabName = "widgets", 
               icon = icon("th"))
    )
  ),
  
  ## Body content
  dashboardBody(
    
    tabItems(
      
      ## First tab content    
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1")), # box is a container for the plot
                box(title = "Controls", # box is a container for the controls
                    selectInput("fill", 
                                "Select variable to fill by:", 
                                choices = c("malaria", "distwater_cat", "disturb_cat", "distfarm_cat", "distpoul_cat"),
                                selected = "malaria")
                )
              )
      ),
      
      ## Second tab item 
      tabItem(tabName = "widgets",
              fluidRow(
                box(plotOutput("plot2", height = 250)), # box is a container for the plot
                box(title = "Controls", # box is a container for the controls
                    radioButtons("x", 
                                 "Select Fill Variable", 
                                 choices=c("trophic.guild", "thermoregulation"),
                                 selected="trophic.guild")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$plot1 <- renderPlot({
    
      ggmap(ps_map)+
      geom_point(data = ps_coordinates, aes_string("longitude", "latitude", group = input$fill, color = input$fill))
  })

  output$plot2 <- renderPlot({
    
    homerange %>% 
      ggplot(aes_string(x="locomotion", fill=input$x))+
      geom_bar(position="dodge", alpha=0.8, color="black")+
      labs(x=NULL, y=NULL, fill="Fill Variable")
    
  })
  
}

shinyApp(ui, server)