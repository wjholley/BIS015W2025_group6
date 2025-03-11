library(tidyverse)
library(janitor)
library(gtools)
library(sf)
library(ggmap)
library(shiny)
library(shinydashboard)
library(bslib)
library(ggthemes)
library(thematic)

thematic::thematic_shiny(font = "auto")

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
ps_map <- get_stadiamap(ps_bbox, maptype = "stamen_terrain", zoom=14)

ps_coordinates <- tf_ps %>% 
  filter(island == "PS")

project_theme <- bs_theme(
  bg = "#ADDE8B", fg = "black", primary = "#FCC780",
  font_scale = 1.5,
  base_font = font_google("Inter"),
  code_font = font_google("Inter")
)

ps_cards <- list(
                  card(
                    full_screen = TRUE,
                    card_header("Map 1"),
                    plotOutput("psplot1", width = "100%", height = "100%")
                  ),
                  card(
                    full_screen = TRUE,
                    card_header("Map 2"),
                    plotOutput("psplot2", width = "100%", height = "100%")
                  )
                )

ps_inputs <-  list(selectInput("pscolor1",
                          "Select what to color samples by for map #1:",
                          choices = c("Malaria" = "malaria", "Relative Distance from Water" = "distwater_cat", "Relative Distance from Urban Areas" = "disturb_cat", "Relative Distance from Livestock Farms" = "distfarm_cat", "Relative Distance from Poultry Farms" = "distpoul_cat", "Altitude" = "altitude"),
                          selected = ("malaria")),
                  selectInput("pscolor2",
                               "Select what to color samples by for map #2:",
                               choices = c("Malaria" = "malaria", "Relative Distance from Water" = "distwater_cat", "Relative Distance from Urban Areas" = "disturb_cat", "Relative Distance from Livestock Farms" = "distfarm_cat", "Relative Distance from Poultry Farms" = "distpoul_cat", "Altitude" = "altitude"),
                               selected = ("distwater_cat")))

tf_cards <- list(
                   card(
                    full_screen = TRUE,
                    card_header("Map 1"),
                    plotOutput("tfplot1", width = "100%", height = "100%")
                  ),
                  card(
                    full_screen = TRUE,
                    card_header("Map 2"),
                    plotOutput("tfplot2", width = "100%", height = "100%")
                  )
                )

tf_inputs <-  list(selectInput("tfcolor1",
                               "Select what to color samples by for map #1:",
                               choices = c("Malaria" = "malaria", "Relative Distance from Water" = "distwater_cat", "Relative Distance from Urban Areas" = "disturb_cat", "Relative Distance from Livestock Farms" = "distfarm_cat", "Relative Distance from Poultry Farms" = "distpoul_cat", "Altitude" = "altitude"),
                               selected = ("malaria")),
                   selectInput("tfcolor2",
                               "Select what to color samples by for map #2:",
                               choices = c("Malaria" = "malaria", "Relative Distance from Water" = "distwater_cat", "Relative Distance from Urban Areas" = "disturb_cat", "Relative Distance from Livestock Farms" = "distfarm_cat", "Relative Distance from Poultry Farms" = "distpoul_cat", "Altitude" = "altitude"),
                               selected = ("distwater_cat")))

malaria_cat_cards <- list(
  card(
    full_screen = TRUE,
    card_header("% of Birds Positive for Malaria vs (Your Choice)"),
    plotOutput("malariacatplot", height = "500px")
  )
)

malaria_cat_inputs <- list(selectInput("malariacat",
                                   "Choose what categorical variable to plot malaria against:",
                                   choices = c("Relative Distance from Water" = "distwater_cat", "Relative Distance from Urban Areas" = "disturb_cat", "Relative Distance from Livestock Farms" = "distfarm_cat", "Relative Distance from Poulty Farms" = "distpoul_cat"),
                                   selected = ("distwater_cat")))

malaria_cont_cards <- list(
  card(
    full_screen = TRUE,
    card_header("Stats for birds with or without malaria"),
    plotOutput("malariacontplot")
  ),
  card(
    full_screen = TRUE,
    card_header("Summary Statistics")
  )
)

malaria_cont_inputs <- list(selectInput("malariacont",
                                   "Choose what continuous variable to plot malaria against:",
                                   choices = c("Distance from Water" = "distwater", "Distance from Urban Areas" = "dist_urb", "Distance from Livestock Farms" = "distfarm", "Distance from Poultry Farms" = "distpoul", "Altitude" = "altitude", "Minimum Temperature" = "mintemp"),
                                   selected = ("distwater")),
                            checkboxInput("show_outliers", "Show Outliers", value = TRUE))

malaria_genetics_input <- list(selectInput("protein", "Select TLR4 Haploid:", 
                          choices = c("TLR4 Protein 1" = "tlr4_prot_1", "TLR4 Protein 2" = "tlr4_prot_2", "TLR4 Protein 3" = "tlr4_prot_3", "TLR4 Protein 4" = "tlr4_prot_4"), 
                          selected = "tlr4_prot_1"))

malaria_genetics_cards <- list(
  card(
    full_screen = TRUE,
    card_header("TLR4 Haplotypes & Malaria Infection"),
    plotOutput("TLR4plot")
  )
)

malaria_snp_input <- list(selectInput("snp", "Select SNP Genotype:", 
                                      choices = c("5239s1", "7259s1"), 
                                      selected = "5239s1"))

malaria_snp_cards <- list(
  card(
    full_screen = TRUE,
    card_header("SNP genotypes & Malaria Infection"),
    plotOutput("snpplot")
  )
)

faceting_cards <- list(
  card(
    full_screen = TRUE,
    card_header("SNP genotypes & Malaria Infection"),
    plotOutput("facetingplot")
  )
)

faceting_inputs <- list(
  column(12,
  selectInput("x_faceting",
              "Allele 1",
              choices = c("5239s1_a","5239s1_t", "7259s1_a","7259s1_t", "tlr4_1_a", "tlr4_1_g", "tlr4_2_a", "tlr4_2_g", "tlr4_3_c", "tlr4_3_t", "tlr4_4_a", "tlr4_4_c"),
              selected = "5239s1_a"),
  selectInput("y_faceting",
              "Allele 2",
              choices = c("5239s1_a","5239s1_t", "7259s1_a","7259s1_t", "tlr4_1_a", "tlr4_1_g", "tlr4_2_a", "tlr4_2_g", "tlr4_3_c", "tlr4_3_t", "tlr4_4_a", "tlr4_4_c"),
              selected = "5239s1_t")
  )
)

##Beginning of the app itself/UI Section
ui <- page_navbar(
  title = "Avians of Tenerife and Porto Santo",
  theme = project_theme,
  nav_panel(title = "Porto Santo", p("Mapping options for Porto Santo."),
            layout_columns(ps_inputs[[1]], ps_inputs[[2]]),
            layout_columns(ps_cards[[1]], ps_cards[[2]])
            ),
              
  nav_panel(title = "Tenerife", p("Mapping options for Tenerife."),
            layout_columns(tf_inputs[[1]], tf_inputs[[2]]),
            layout_columns(tf_cards[[1]], tf_cards[[2]])
            ),
  
  nav_panel(title = "Malaria vs Environmental Factors", p("Plotting options for malaria and an environmental variable"),
            accordion(
              accordion_panel("Categorical Input",
            layout_columns(malaria_cat_inputs[[1]]),
            layout_columns(malaria_cat_cards[[1]])
              ),
            accordion_panel("Continuous Input",
                            layout_columns(malaria_cont_inputs[[1]], malaria_cont_inputs[[2]]),
                            layout_columns(col_widths = c(6, 3, 3),
                                            malaria_cont_cards[[1]], 
                                             value_box(
                                               title = "Malaria Positive Average",
                                               value = textOutput("malariapositiveaverage"),
                                               showcase = bsicons::bs_icon("align-bottom"),
                                               theme = "red"
                                             ),
                                             value_box(
                                               title = "Malaria Negative Average",
                                               value = textOutput("malarianegativeaverage"),
                                               showcase = bsicons::bs_icon("align-bottom"),
                                               theme = "blue"))))
              ),
  nav_panel(title = "Malaria vs Genetic Factors", p("Plotting options for malaria and a genome-related variable"),
            accordion(
              accordion_panel("TLR4 Haplotypes",
            layout_columns(malaria_genetics_input[[1]]),
            layout_columns(col_widths = c(6, 3, 3),
                            malaria_genetics_cards[[1]],
                            column(12, value_box(
                               title = "% of all samples from Tenerife with malaria",
                               value = textOutput("tenerifepositive"),
                               theme = "green"
                             ),
                             value_box(
                               title = "% of all samples with selected haplotype from Tenerife with malaria",
                               value = textOutput("tenerifeselectpositive"),
                               theme = "green"
                             )
                            ),
                            column(12, value_box(
                               title = "% of all samples from Porto Santo with malaria",
                               value = textOutput("portosantopositive"),
                               theme = "blue"
                             ),
                             value_box(
                               title = "% of all samples with selected haplotype from Porto Santo with malaria",
                               value = textOutput("portosantoselectpositive"),
                               theme = "blue"
                              )
                            )
                          )
                        ),
            accordion_panel("SNP Genotype",
                            layout_columns(malaria_snp_input[[1]]),
                            layout_columns(malaria_snp_cards[[1]])
                            )
                      )
  ),
  nav_panel(title = "Faceting for Genetic Factors", p("Plotting options for malaria and two genome-related variables"),
            layout_columns(col_widths = c(3,9),
                           faceting_inputs[[1]], layout_columns(faceting_cards[[1]])),
            ),
  
  nav_menu(title = "Links",
           nav_item(tags$a("Study", href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC6875583/")),
           nav_item(tags$a("Data", href = "https://datadryad.org/dataset/doi:10.5061/dryad.228986b")),
           nav_item(tags$a("Our Github", href = "https://github.com/wjholley/BIS15W2025_group6"))
           )
)

##Server functions
server <- function(input, output, session) {
  output$psplot1 <- renderPlot({
    
    ggmap(ps_map)+
      geom_point(data = ps_coordinates, 
                 aes_string("longitude", "latitude", color = input$pscolor1), size = 2)+
      theme_stata()+
      theme(legend.position = "bottom", axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.text = element_text(size = 15))+
      labs(x = "Longitude", y = "Latitude")
    })
  
  output$psplot2 <- renderPlot({
    
    ggmap(ps_map)+
      geom_point(data = ps_coordinates, 
                 aes_string("longitude", "latitude", color = input$pscolor2), size = 2)+
      theme_stata()+
      theme(legend.position = "bottom", axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.text = element_text(size = 15))+
      labs(x = "Longitude", y = "Latitude")
  })

  output$tfplot1 <- renderPlot({
    
    ggmap(tf_map)+
      geom_point(data = tf_coordinates, 
                 aes_string("longitude", "latitude", group = input$tfcolor1, color = input$tfcolor1), size = 2)+
      theme_stata()+
      theme(legend.position = "bottom", axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.text = element_text(size = 15))+
      labs(x = "Longitude", y = "Latitude")
  })
  
  output$tfplot2 <- renderPlot({
    
    ggmap(tf_map)+
      geom_point(data = tf_coordinates, 
                 aes_string("longitude", "latitude", group = input$tfcolor2, color = input$tfcolor2), size = 2)+
      theme_stata()+
      theme(legend.position = "bottom", axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.text = element_text(size = 15))+
      labs(x = "Longitude", y = "Latitude")
  })
  
  output$malariacatplot <- renderPlot({
    tf_ps %>% 
      group_by(malaria, !!sym(input$malariacat)) %>% 
      summarize(n=n(), .groups = 'keep') %>%
      group_by(!!sym(input$malariacat)) %>% 
      mutate(perc = 100*n/sum(n)) %>%
      filter(malaria == "Y") %>%
      mutate(!!sym(input$malariacat) := factor(!!sym(input$malariacat), levels = c("close", "median close", "median far", "far"))) %>%
      ggplot(aes(!!sym(input$malariacat), perc, fill = perc))+
      geom_col(color = "black")+
      scale_y_continuous(limits = c(0, 100))+
      theme_stata()+
      theme(legend.position = "bottom", axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.text = element_text(size = 15))+
      scale_fill_gradient(low = "white", high = "salmon")+  
      labs(y = "Percent of Birds Positive for Malaria")
  })
  
  output$malariacontplot <- renderPlot({
    plot_data <- tf_ps %>%
      filter(malaria == "Y" | malaria == "N")
    
    # Compute Y-axis limits without outliers
    if (!input$show_outliers) {
      y_limits <- plot_data %>%
        group_by(malaria) %>%
        summarise(ymin = quantile(!!sym(input$malariacont), 0.25, na.rm = TRUE) - 1.5 * IQR(!!sym(input$malariacont), na.rm = TRUE),
                  ymax = quantile(!!sym(input$malariacont), 0.75, na.rm = TRUE) + 1.5 * IQR(!!sym(input$malariacont), na.rm = TRUE)) %>%
        summarise(ymin = min(ymin, na.rm = TRUE), ymax = max(ymax, na.rm = TRUE))
    }
    
    ggplot(plot_data, aes_string(x = "malaria", y = input$malariacont, fill = "malaria")) +
      geom_boxplot(color = "black", alpha = 0.5, 
                   outlier.shape = ifelse(input$show_outliers, 16, NA)) +  # Toggle outliers
      coord_cartesian(ylim = if (!input$show_outliers) c(y_limits$ymin, y_limits$ymax) else NULL) + 
      theme_stata() +
      theme(legend.position = "bottom", axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.text = element_text(size = 15))+
      labs(x = "Malaria Status")
  })
  
  output$malariapositiveaverage <- renderText({
    tf_ps %>%
      group_by(malaria) %>% 
      filter(malaria == "Y") %>% 
      summarize(average_variable = mean(!!sym(input$malariacont))) %>% 
      select(average_variable) %>% 
      .[[1]]
  })
  
  output$malarianegativeaverage <- renderText({
    tf_ps %>%
      group_by(malaria) %>% 
      filter(malaria == "N") %>% 
      summarize(average_variable = mean(!!sym(input$malariacont))) %>% 
      select(average_variable) %>% 
      .[[1]]
  })
  
  output$TLR4plot <- renderPlot({
    tf_ps %>% 
      filter(!!sym(input$protein) == "present" | !!sym(input$protein) == "absent") %>% 
      group_by(malaria, !!sym(input$protein), island) %>%
      summarize(n=n(), .groups = 'keep') %>% 
      group_by(!!sym(input$protein), island) %>% 
      mutate(perc = 100*n/sum(n)) %>%
      ggplot(aes(x = malaria, y = perc, fill = !!sym(input$protein)))+
      geom_col(color = "black", position = "dodge", alpha = 0.5)+
      facet_grid(island ~ .)+
      labs(title = paste("Effect of", input$protein, "on Malaria Infection"),
        x = "Malaria",
        y = "Percent",
        fill = "TLR4 variant specified")+
      theme_stata()+
      theme(legend.position = "bottom", axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.text = element_text(size = 15))+
      theme(legend.position = "bottom", axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.text = element_text(size = 15))
  })
  
  output$tenerifepositive <- renderText({
    tf_ps %>%
      filter(island == "TF") %>% 
      group_by(malaria) %>% 
      summarize(n=n(), groups = 'keep') %>%
      mutate(perc = 100*n/sum(n)) %>% 
      filter(malaria == "Y") %>% 
      select(perc) %>%
      .[[1]]
  })
  
  output$tenerifeselectpositive <- renderText({
    tf_ps %>%
      filter(island == "TF") %>%
      group_by(malaria, !!sym(input$protein)) %>%
      summarize(n=n(), .groups = 'keep') %>% 
      group_by(!!sym(input$protein)) %>% 
      mutate(perc = 100*n/sum(n)) %>% 
      filter(malaria == "Y" & !!sym(input$protein) == "present") %>% 
      select(perc) %>% 
      .[[2]]
  })
  
  output$portosantopositive <- renderText({
    tf_ps %>%
      filter(island == "PS") %>% 
      group_by(malaria) %>% 
      summarize(n=n(), groups = 'keep') %>%
      mutate(perc = 100*n/sum(n)) %>% 
      filter(malaria == "Y") %>% 
      select(perc) %>%
      .[[1]]
  })
  
  output$portosantoselectpositive <- renderText({
    tf_ps %>%
      filter(island == "PS") %>%
      group_by(malaria, !!sym(input$protein)) %>%
      summarize(n=n(), .groups = 'keep') %>% 
      group_by(!!sym(input$protein)) %>% 
      mutate(perc = 100*n/sum(n)) %>% 
      filter(malaria == "Y" & !!sym(input$protein) == "present") %>% 
      select(perc) %>% 
      .[[2]]
  })
  
  output$snpplot <- renderPlot({
    tf_ps %>%  
      filter(!!sym(input$snp) == "AA" | !!sym(input$snp) == "AT" | !!sym(input$snp) == "TT") %>%
      group_by(malaria, !!sym(input$snp), island) %>%
      summarize(n=n(), .groups = 'keep') %>% 
      group_by(!!sym(input$snp), island) %>% 
      mutate(perc = 100*n/sum(n)) %>% 
      ggplot(aes(x = malaria, y = perc, fill = !!sym(input$snp)))+
      geom_col(color = "black", position = "dodge", alpha = 0.5)+
      facet_grid(. ~ island)+
      labs(title = paste("Effect of", input$snp, "on Malaria Infection"),
           x = "Malaria",
           y = "Percent",
           fill = "SNP variant specified")+
      theme_stata()+
      theme(legend.position = "bottom", axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.text = element_text(size = 15))
  })

output$facetingplot <- renderPlot({
  snp_plot_data <- tf_ps %>% 
    filter(!is.na(malaria)&!is.na(!!sym(input$x_faceting))&!is.na(!!sym(input$y_faceting))) %>%
    group_by(malaria, !!sym(input$x_faceting), !!sym(input$y_faceting)) %>% 
    summarize(n=n(), .groups = 'keep') %>%
    group_by(!!(input$x_faceting), !!sym(input$y_faceting)) %>% 
    mutate(perc = 100*n/sum(n))
  
  snp_plot_data %>% 
    ggplot(aes(malaria, perc, fill = malaria))+
    geom_col(color = "black", alpha = 0.5)+
    facet_grid(get(input$y_faceting)~get(input$x_faceting), labeller = label_both)+
    labs(y = "Percent of Population",
         x = NULL,
         title = "Absence or Presence of Malaria by SNP Allele")+
    theme_stata()+
    theme(legend.position = "bottom", axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.text = element_text(size = 15))
})

}
shinyApp(ui, server)