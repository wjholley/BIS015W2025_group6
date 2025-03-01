library(tidyverse)
library(janitor)
library(gtools)
library(sf)
library(ggmap)
library(shiny)
library(shinydashboard)

register_stadiamaps("1d92513d-1a7f-407d-8c65-60d0d510c9c0", write = FALSE)

tf_ps <- read_csv("data/landscapegenetics/genomics_tf_ps.csv")

latitude_tf <- c(28.01, 28.58)
longitude_tf <- c(-16.92, -16.18)
tf_bbox <- make_bbox(longitude_tf, latitude_tf, f = 0.1)
tf_map <- get_stadiamap(tf_bbox, maptype = "stamen_terrain", zoom=10)

tf_coordinates <- tf_ps %>% 
  filter(island == "TF")

latitude_ps <- c(33.03, 33.10)
longitude_ps <- c(-16.39, -16.3)
ps_bbox <- make_bbox(longitude_ps, latitude_ps, f = 0.1)
ps_map <- get_stadiamap(ps_bbox, maptype = "stamen_terrain", zoom=12)

ps_coordinates <- tf_ps %>% 
  filter(island == "PS")




ui <- dashboardPage(
  dashboardHeader(title = "Title Placeholder"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Porto Santo Map", 
               tabName = "psmap", 
               icon = icon("dashboard")),
      
      menuItem("Tenerife Map", 
               tabName = "tfmap", 
               icon = icon("dashboard"))
    )
  ),
  
  ## Body content
  dashboardBody(
    
    tabItems(
      
      ## First tab content    
      tabItem(tabName = "psmap",
              fluidRow(
                box(title = "Map", plotOutput("psplot")), # box is a container for the plot
                box(title = "Controls", # box is a container for the controls
                    selectInput("pscolor", 
                                "Select variable to color by:", 
                                choices = c("malaria", "distwater_cat", "disturb_cat", "distfarm_cat", "distpoul_cat"),
                                selected = "malaria")
                )
              )
      ),
      
      tabItem(tabName = "tfmap",
              fluidRow(
                box(plotOutput("plot2")), # box is a container for the plot
                box(title = "Controls", # box is a container for the controls
                    selectInput("tfcolor", 
                                "Select variable to color by:", 
                                choices = c("malaria", "distwater_cat", "disturb_cat", "distfarm_cat", "distpoul_cat"),
                                selected = "malaria")
                )
              )
      )
    )
  )#,
  #plotOutput("psplot", width = "1000px", height = "500px")
)

server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp) #Stop the app from running immediately after it's closed
  
  output$psplot <- renderPlot({
    
      ggmap(ps_map)+
      geom_point(data = ps_coordinates, 
                 aes_string("longitude", "latitude", group = input$pscolor, color = input$pscolor))+
      labs(x = "Longitude", y = "Latitude")
  
    })

  output$plot2 <- renderPlot({
    
    ggmap(tf_map)+
      geom_point(data = tf_coordinates, 
                 aes_string("longitude", "latitude", group = input$tfcolor, color = input$tfcolor))+
      labs(x = "Longitude", y = "Latitude")
    
  })
  
}

shinyApp(ui, server)