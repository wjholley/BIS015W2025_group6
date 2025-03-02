library(tidyverse)
library(janitor)
library(gtools)
library(sf)
library(ggmap)
library(shiny)
library(shinydashboard)
library(bslib)
library(patchwork)

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
ps_map <- get_stadiamap(ps_bbox, maptype = "stamen_terrain", zoom=13)

ps_coordinates <- tf_ps %>% 
  filter(island == "PS")

ui <- page_navbar(
  
  title = "Avians of Tenerife and Porto Santo",
  nav_panel(title = "Porto Santo", p("Mapping options for Porto Santo."),
            
            fluidRow(
              box(selectInput("pscolor",
                              "Select what to color samples by:",
                              choices = c("Malaria" = "malaria", "Relative Distance from Water" = "distwater_cat", "Relative Distance from Urban Areas" = "disturb_cat", "Relative Distance from Farmland" = "distfarm_cat", "Relative Distance from Pollution" = "distpoul_cat"),
                              selected = ("malaria"))),
              box(plotOutput("psplot")),
              box(plotOutput("tfplot"))
            )
            
            ),
  
  nav_panel(title = "Tenerife", p("Mapping options for Tenerife."),
            selectInput("tfcolor",
                        "Select what to color samples by:",
                        choices = c("Malaria" = "malaria", "Relative Distance from Water" = "distwater_cat", "Relative Distance from Urban Areas" = "disturb_cat", "Relative Distance from Farmland" = "distfarm_cat", "Relative Distance from Pollution" = "distpoul_cat"),
                        selected = ("malaria")),
            plotOutput("tfplot")
            ),
  
  nav_menu(title = "Links",
           nav_item(tags$a("Study", href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC6875583/")),
           nav_item(tags$a("Data", href = "https://datadryad.org/dataset/doi:10.5061/dryad.228986b")),
           nav_item(tags$a("Our Github", href = "https://github.com/wjholley/BIS15W2025_group6"))
           )
)

server <- function(input, output, session) {
  
  output$psplot <- renderPlot({
    
    ggmap(ps_map)+
      geom_point(data = ps_coordinates, 
                 aes_string("longitude", "latitude", color = input$pscolor))+
      labs(x = "Longitude", y = "Latitude")
      
  
    })

  output$tfplot <- renderPlot({
    
    ggmap(tf_map)+
      geom_point(data = tf_coordinates, 
                 aes_string("longitude", "latitude", group = input$tfcolor, color = input$tfcolor))+
      labs(x = "Longitude", y = "Latitude")
    
  })
  
}

shinyApp(ui, server)