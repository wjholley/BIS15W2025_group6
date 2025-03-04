library(tidyverse)
library(janitor)
library(gtools)
library(sf)
library(ggmap)
library(shiny)
library(shinydashboard)
library(bslib)
library(ggthemes)

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

malaria_cat_cards <- list(
  card(
    full_screen = TRUE,
    card_header("% of Birds Positive for Malaria vs (Your Choice)"),
    plotOutput("malariacatplot")
  )
)

malaria_cat_inputs <- list(selectInput("malariacat",
                                   "Choose what categorical variable to plot malaria against:",
                                   choices = c("Relative Distance from Water" = "distwater_cat", "Relative Distance from Urban Areas" = "disturb_cat", "Relative Distance from Farmland" = "distfarm_cat", "Relative Distance from Pollution" = "distpoul_cat"),
                                   selected = ("distwater_cat")))

malaria_cont_cards <- list(
  card(
    full_screen = TRUE,
    card_header("Stats for birds with or without malaria"),
    plotOutput("malariacontplot")
  ),
  card(
    full_screen = TRUE,
    card_header("Summary Statistics"),
  )
)

malaria_cont_inputs <- list(selectInput("malariacont",
                                   "Get stats for different continuous variables",
                                   choices = c("Distance from Water" = "distwater", "Distance from Urban Areas" = "dist_urb", "Distance from Farmland" = "distfarm", "Distance from Pollution" = "distpoul", "Altitude" = "altitude", "Minimum Temperature" = "mintemp"),
                                   selected = ("distwater")))

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
  
  nav_panel(title = "Malaria vs Environmental Factors", p("Plotting options for malaria and an environmental variable"),
            accordion(
              accordion_panel("Categorical Input",
            layout_columns(malaria_cat_inputs[[1]]),
            layout_columns(malaria_cat_cards[[1]])
              ),
            accordion_panel("Continuous Input",
                            layout_columns(malaria_cont_inputs[[1]]),
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
                                               theme = "blue",
                                               alpha = 0.5
                                             )
                                           )
              )
            )
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
      theme_stata()+
      theme(legend.position = "bottom")+
      labs(x = "Longitude", y = "Latitude")
    })
  
  output$psplot2 <- renderPlot({
    
    ggmap(ps_map)+
      geom_point(data = ps_coordinates, 
                 aes_string("longitude", "latitude", color = input$pscolor2))+
      theme_stata()+
      theme(legend.position = "bottom")+
      labs(x = "Longitude", y = "Latitude")
  })

  output$tfplot1 <- renderPlot({
    
    ggmap(tf_map)+
      geom_point(data = tf_coordinates, 
                 aes_string("longitude", "latitude", group = input$tfcolor1, color = input$tfcolor1))+
      theme_stata()+
      theme(legend.position = "bottom")+
      labs(x = "Longitude", y = "Latitude")
  })
  
  output$tfplot2 <- renderPlot({
    
    ggmap(tf_map)+
      geom_point(data = tf_coordinates, 
                 aes_string("longitude", "latitude", group = input$tfcolor2, color = input$tfcolor2))+
      theme_stata()+
      theme(legend.position = "bottom")+
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
      scale_fill_gradient(low = "white", high = "salmon")+  
      labs(y = "Percent of Birds Positive for Malaria")
  })
  
  output$malariacontplot <- renderPlot({
    tf_ps %>%
      filter(malaria == "Y" | malaria == "N") %>% 
      ggplot(aes_string(x = "malaria", y = input$malariacont))+
      geom_boxplot()+
      theme_stata()+
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
  
}

shinyApp(ui, server)