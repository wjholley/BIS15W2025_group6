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

ps_cards <- list(
                  card(
                    full_screen = TRUE,
                    card_header("Map 1"),
                    plotOutput("psplot1")
                  ),
                  card(
                    full_screen = TRUE,
                    card_header("Map 2"),
                    plotOutput("psplot2")
                  )
                )

ps_inputs <-  list(selectInput("pscolor1",
                          "Select what to color samples by for map #1:",
                          choices = c("Malaria" = "malaria", "Relative Distance from Water" = "distwater_cat", "Relative Distance from Urban Areas" = "disturb_cat", "Relative Distance from Farmland" = "distfarm_cat", "Relative Distance from Pollution" = "distpoul_cat"),
                          selected = ("malaria")),
                  selectInput("pscolor2",
                               "Select what to color samples by for map #2:",
                               choices = c("Malaria" = "malaria", "Relative Distance from Water" = "distwater_cat", "Relative Distance from Urban Areas" = "disturb_cat", "Relative Distance from Farmland" = "distfarm_cat", "Relative Distance from Pollution" = "distpoul_cat"),
                               selected = ("distwater_cat")))

tf_cards <- list(
                   card(
                    full_screen = TRUE,
                    card_header("Map 1"),
                    plotOutput("tfplot1")
                  ),
                  card(
                    full_screen = TRUE,
                    card_header("Map 2"),
                    plotOutput("tfplot2")
                  )
                )

tf_inputs <-  list(selectInput("tfcolor1",
                               "Select what to color samples by for map #1:",
                               choices = c("Malaria" = "malaria", "Relative Distance from Water" = "distwater_cat", "Relative Distance from Urban Areas" = "disturb_cat", "Relative Distance from Farmland" = "distfarm_cat", "Relative Distance from Pollution" = "distpoul_cat"),
                               selected = ("malaria")),
                   selectInput("tfcolor2",
                               "Select what to color samples by for map #2:",
                               choices = c("Malaria" = "malaria", "Relative Distance from Water" = "distwater_cat", "Relative Distance from Urban Areas" = "disturb_cat", "Relative Distance from Farmland" = "distfarm_cat", "Relative Distance from Pollution" = "distpoul_cat"),
                               selected = ("distwater_cat")))

ui <- page_navbar(
  
  title = "Avians of Tenerife and Porto Santo",
  
  nav_panel(title = "Porto Santo", p("Mapping options for Porto Santo."),
            layout_columns(ps_inputs[[1]], ps_inputs[[2]]),
            layout_columns(ps_cards[[1]], ps_cards[[2]])
            ),
              
  nav_panel(title = "Tenerife", p("Mapping options for Tenerife."),
            layout_columns(tf_inputs[[1]], tf_inputs[[2]]),
            layout_columns(tf_cards[[1]], tf_cards[[2]])
            ),
  
  nav_menu(title = "Links",
           nav_item(tags$a("Study", href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC6875583/")),
           nav_item(tags$a("Data", href = "https://datadryad.org/dataset/doi:10.5061/dryad.228986b")),
           nav_item(tags$a("Our Github", href = "https://github.com/wjholley/BIS15W2025_group6"))
           ),
)

server <- function(input, output, session) {
  
  output$psplot1 <- renderPlot({
    
    ggmap(ps_map)+
      geom_point(data = ps_coordinates, 
                 aes_string("longitude", "latitude", color = input$pscolor1))+
      labs(x = "Longitude", y = "Latitude")
      
  
    })
  output$psplot2 <- renderPlot({
    
    ggmap(ps_map)+
      geom_point(data = ps_coordinates, 
                 aes_string("longitude", "latitude", color = input$pscolor2))+
      labs(x = "Longitude", y = "Latitude")
    
    
  })

  output$tfplot1 <- renderPlot({
    
    ggmap(tf_map)+
      geom_point(data = tf_coordinates, 
                 aes_string("longitude", "latitude", group = input$tfcolor, color = input$tfcolor1))+
      labs(x = "Longitude", y = "Latitude")
    
  })
  output$tfplot2 <- renderPlot({
    
    ggmap(tf_map)+
      geom_point(data = tf_coordinates, 
                 aes_string("longitude", "latitude", group = input$tfcolor, color = input$tfcolor2))+
      labs(x = "Longitude", y = "Latitude")
    
  })
  
}

shinyApp(ui, server)