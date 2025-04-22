# Installer les packages nécessaires
library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)

ui <- fluidPage(
  titlePanel("Visualization of Biological Alterations (SEQ-Eau 2003)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ville", "Chose a city :", choices = NULL),
      selectInput("annee", "Chose a year :", choices = NULL),
      width = 3
    ),
    mainPanel(
      plotOutput("radarPlot", height = "800px"),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  # Charger les données
  data <- reactive({
    req(input$file)
    read_excel(input$file$datapath) %>%
      mutate(across(starts_with("etat_"), ~ifelse(. == "", NA, .)))
  })
  
  # Mettre à jour les sélections en conservant le choix actuel
  observe({
    req(data())
    df <- data()
    
    # Sauvegarder la sélection actuelle
    current_ville <- input$ville
    current_annee <- input$annee
    
    # Mettre à jour les villes disponibles
    villes <- sort(unique(df$urbanaggl))
    updateSelectInput(session, "ville", choices = villes, selected = current_ville)
    
    # Mettre à jour les années si la ville existe toujours
    if (!is.null(current_ville) && current_ville %in% villes) {
      annees <- df %>%
        filter(urbanaggl == current_ville) %>%
        distinct(year) %>%
        pull(year)
      updateSelectInput(session, "annee", choices = annees, selected = current_annee)
    } else {
      updateSelectInput(session, "annee", choices = NULL)
    }
  })
  # Créer le graphique
  output$radarPlot <- renderPlot({
    req(input$ville, input$annee, data())
    
    alterations <- c("OM", "NM", "N", "PM", "SP", "T", "A", "M", "MM")
    reaches <- c("upstream", "city", "downstream")
    
    plot_data <- expand.grid(Alteration = alterations, Reach = reaches) %>%
      left_join(
        data() %>%
          filter(urbanaggl == input$ville, year == input$annee) %>%
          select(reach, etat_om:etat_mm) %>%
          pivot_longer(-reach, names_to = "Alteration", values_to = "Etat") %>%
          mutate(
            Alteration = gsub("etat_", "", Alteration) %>% toupper(),
            Etat = ifelse(is.na(Etat), "Unknown", Etat)
          ),
        by = c("Alteration", "Reach" = "reach")
      ) %>%
      mutate(
        Etat = ifelse(is.na(Etat), "Unknown", Etat),
        Reach = factor(Reach, levels = reaches, labels = c("Amont", "Ville", "Aval")),
        Niveau = as.numeric(Reach)
      )
    
    couleurs_etat <- c(
      "Excellent" = "#1F77B4", "Good" = "#2CA02C", "Medium" = "#FFD700",
      "Bad" = "#FF7F0E", "Poor" = "#D62728", "Unknown" = "#CCCCCC"
    )
    
    ggplot(plot_data) +
      geom_tile(
        aes(x = Alteration, y = Niveau, fill = Etat),
        color = "white", width = 0.9, height = 0.9
      ) +
      geom_segment(
        data = data.frame(x = seq(1.5, 8.5, by = 1)),
        aes(x = x, xend = x, y = 0.5, yend = 3.5),
        color = "black", size = 1, inherit.aes = FALSE
      ) +
      geom_text(
        aes(x = Alteration, y = 3.8, label = Alteration),
        size = 5, fontface = "bold", inherit.aes = FALSE
      ) +
      coord_polar(start = -pi/9) +
      scale_fill_manual(values = couleurs_etat, name = "State") +
      scale_y_continuous(
        limits = c(0.5, 4),
        breaks = 1:3,
        labels = c("Upstream\n(center)", "City", "Downstream\n(exterior)")
      ) +
      labs(
        title = paste("Alteration states for", input$ville, "in", input$annee),
        subtitle = "Concentric structure with rivers water quality evaluation between reachs",
        x = "", y = ""
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold"),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "right"
      )
  })
  
  # Gestion du chargement de fichier
  observe({
    if (is.null(input$file)) {
      showModal(modalDialog(
        title = "Charger les données",
        fileInput("file", "Sélectionner le fichier Excel :", 
                  accept = c(".xlsx", ".xls")),
        footer = modalButton("Fermer"),
        size = "m"
      ))
    }
  }) 
  
 
}

shinyApp(ui, server)
