library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(sf)
library(stringi)
library(stringr)
library(tidyr)
library(DT)
library(corrplot)
library(MASS)
library(caTools)
library(caret)
library(forcats)
library(DiagrammeR)
library(shinyjs)
library(htmlwidgets)
library(viridis)
library(leaflet)



# Préférer les versions de dplyr en cas de conflit
select <- dplyr::select
filter <- dplyr::filter

# --- Chargement des données ---
data2 <- read.csv("C:/Users/nesri/Downloads/freMTPLsev.csv", sep = ",", header = TRUE)
data <- read.csv("C:/Users/nesri/Downloads/freMTPLfreq.csv", sep = ",", header = TRUE)

# --- Nettoyage des données ---
data_clean <- data %>%
  separate(col = 1,
           into = c("PolicyID", "ClaimNb", "Exposure", "Power", "CarAge",
                    "DriverAge", "Brand", "Gas", "Region", "Density"),
           sep = ",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)",
           extra = "merge",
           fill = "right")

data$PolicyID <- as.character(data$PolicyID)
data2$PolicyID <- as.character(data2$PolicyID)

data2_aggregated <- data2 %>%
  group_by(PolicyID) %>%
  summarise(Severity_Total = sum(ClaimAmount, na.rm = TRUE))

data_clean <- data_clean %>%
  mutate(across(c(ClaimNb, Exposure, CarAge, DriverAge, Density), as.numeric))

data2_aggregated <- data2_aggregated %>%
  mutate(Severity_Total = as.numeric(Severity_Total))

data_joined <- merge(data_clean, data2_aggregated, by = "PolicyID")
# ✅ Nettoyage + capping AVANT toute modélisation
data_joined <- data_joined %>%
  filter(Exposure > 0) %>%
  mutate(Claimfre = ClaimNb / Exposure)

threshold <- quantile(data_joined$Claimfre, 0.99, na.rm = TRUE)
data_joined <- data_joined %>%
  mutate(Claimfre = ifelse(Claimfre > threshold, threshold, Claimfre))



# Création de CarAgeClass si non existant
if (!"CarAgeClass" %in% names(data_joined)) {
  data_joined <- data_joined %>%
    mutate(CarAgeClass = case_when(
      CarAge == 0 ~ "Neuf (0 an)",
      CarAge >= 1 & CarAge <= 3 ~ "1-3 ans",
      CarAge >= 4 & CarAge <= 7 ~ "4-7 ans",
      CarAge >= 8 & CarAge <= 12 ~ "8-12 ans",
      CarAge > 12 ~ "13 ans et plus",
      TRUE ~ "Inconnu"
    ))
}
# Définir les classes d'âge
data_joined <- data_joined %>%
  mutate(Age_Class = case_when(
    DriverAge < 25  ~ "Moins de 25 ans",
    DriverAge >= 25 & DriverAge < 35  ~ "25-34 ans",
    DriverAge >= 35 & DriverAge < 45  ~ "35-44 ans",
    DriverAge >= 45 & DriverAge < 55  ~ "45-54 ans",
    DriverAge >= 55 & DriverAge < 75  ~ "55-75 ans",
    DriverAge >= 75  ~ "75 ans et plus",
    TRUE ~ NA_character_
  )) %>%
  mutate(Age_Class = factor(Age_Class, levels = c(
    "Moins de 25 ans", "25-34 ans", "35-44 ans",
    "45-54 ans", "55-75 ans", "75 ans et plus"
  )))


numeric_vars <- c("ClaimNb", "Exposure", "CarAge", "DriverAge", "Density", "Severity_Total")
categorical_vars <- c("Brand", "Gas", "Region", "Power")


# --- UI ---


ui <- fluidPage(
  useShinyjs(),
  
  # === CSS pour image de fond ===
  tags$head(tags$style(HTML("
    body {
      background-image: url('img3.png');
      background-size: cover;
      background-position: center;
      background-repeat: no-repeat;
    }
  "))),
  
  # === Page d'accueil ===
  div(id = "landing_page",
      br(), br(), br(),
      h1("Bienvenue dans votre application", align = "center", style = "color: white;"),
      br(),
      div(align = "center",
          actionButton("start_btn", "Veuillez consulter le dashboard", class = "btn-primary btn-lg")
      )
  ),
  
  # === Dashboard masqué au départ === 
  hidden(
  div(id = "main_dashboard",dashboardPage(
  
  dashboardHeader(title = "Dashboard Assurance"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Données", tabName = "donnees", icon = icon("table")),
      menuItem("Carte", tabName = "carte", icon = icon("map")),
      menuItem("Analyse", icon = icon("chart-bar"),
               menuSubItem("Univariée", tabName = "univariee"),
               menuSubItem("Bivariée", tabName = "bivariee")
      ),
      menuItem("Modèles", icon = icon("cogs"),
               menuSubItem("GLM", tabName = "glm_model"),
               menuSubItem("XGBoost", tabName = "xgboost_model")
      )
  )),
  dashboardBody(
    useShinyjs(),
   
    

    tags$head(
      tags$style(HTML("
    /* === FOND DU CONTENU PRINCIPAL === */
    .content-wrapper {
      position: relative;
      background-image: url('img3.png');
      background-size: cover;
      background-position: center;
      background-attachment: fixed;
      background-repeat: no-repeat;
    }

    .content-wrapper::before {
      content: '';
      position: absolute;
      top: 0; left: 0;
      width: 100%;
      height: 100%;
      background-color: rgba(255, 255, 255, 0.5); /* blanc semi-transparent */
      z-index: 1;
    }

    .content-wrapper > * {
      position: relative;
      z-index: 2;
    }

    /* === SIDEBAR BLEUE CLAIRE TRANSPARENTE === */
    .main-sidebar, .left-side, .skin-blue .main-sidebar, .skin-blue .left-side {
      background-color: rgba(135, 206, 250, 0.3) !important; /* bleu clair transparent */
    }

    /* === TEXTE DU MENU === */
    .sidebar-menu > li > a,
    .sidebar .sidebar-menu .treeview-menu > li > a {
      color: #e6f7ff !important;
    }

    /* === ÉLÉMENT ACTIF === */
    .sidebar-menu > li.active > a {
      background-color: rgba(135, 206, 250, 0.5) !important;
      color: #e6f7ff !important;
    }
    /* Fond bleu très clair pour les boxes et outputs */
  .box, .well, .shiny-output-error-validation, 
  .form-control, .dataTables_wrapper, .table, .tab-content {
    background-color: #e6f7ff !important;  /* bleu très clair */
    color: black;
  }

  /* Enlever bordure grise si besoin */
  .box {
    border: 1px solid #b3e0ff !important;
  }
  "))
    ),
    tabItems(
      tabItem(tabName = "carte",  
              leafletOutput("mapPlot", height = "600px")
      ),
      tabItem(tabName = "donnees",
              h4(strong("Dimention")),
              column(2, img(src = "img2.png", height = "100px")),
              column(3, h4("LIGNES"), textOutput("rowCount")),
              column(2, img(src = "img1.png", height = "100px")),
              column(1, h4("COLONNES"), textOutput("colCount")),
              DT::dataTableOutput("table")
      ),
     
      tabItem(tabName = "univariee",
              fluidPage(
                h3("Analyse Univariée"),
                
                fluidRow(
                  column(6, selectInput("num_var", "Variable quantitative :", choices = numeric_vars)),
                  column(6, selectInput("cat_var", "Variable catégorique :", choices = categorical_vars))
                ),
                
                verbatimTextOutput("summary_output"),
                
                fluidRow(
                  column(6,
                         radioButtons("plot_type", "Type de graphique :",
                                      choices = c("Histogramme" = "hist",
                                                  "Boxplot" = "box",
                                                  "Barplot" = "bar"),
                                      selected = "hist"),
                         sliderInput("nb_bins", "Nombre de barres :", min = 5, max = 50, value = 20),
                         sliderInput("box_width", "Largeur du boxplot :", min = 0.1, max = 1, value = 0.5)
                  ),
                  column(6,
                         plotOutput("univar_plot", height = "400px"))
                )
              )
      ),
      
      tabItem(tabName = "bivariee",
              fluidPage(
                h2("Analyse Bivariée", align = "center"),
                fluidRow(
                  box(title = "Matrice de Corrélation", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                      plotOutput("corrplot"),
                      DTOutput("correlation_table")
                  )
                ),
                fluidRow(
                  box(title = "Scatterplot: Relation Âge Conducteur vs Montant du Sinistre", status = "info", solidHeader = TRUE, collapsible = TRUE, width = 12,
                      plotOutput("scatter_age_severity")
                  )
                ),
                fluidRow(
                  box(title = "Scatterplot: Âge Conducteur vs Nombre de Sinistres (par Marque)", status = "info", solidHeader = TRUE, collapsible = TRUE, width = 12,
                      plotOutput("scatter_brand")
                  )
                ),
                fluidRow(
                  box(title = "Boxplots par Marque", status = "warning", solidHeader = TRUE, collapsible = TRUE, width = 12,
                      plotOutput("boxplot_brand")
                  )
                ),
                fluidRow(
                  box(title = "Distribution du Montant par Classe d'Âge", status = "success", solidHeader = TRUE, collapsible = TRUE, width = 12,
                      plotOutput("boxplot_ageclass")
                  )
                ),
                fluidRow(
                  box(title = "Montant Moyen par Classe d'Âge (Barplot)", status = "success", solidHeader = TRUE, collapsible = TRUE, width = 12,
                      plotOutput("barplot_ageclass")
                  )
                ),
                fluidRow(
                  box(title = "Tests Statistiques (ANOVA, T-Test, Kruskal-Wallis)", status = "danger", solidHeader = TRUE, collapsible = TRUE, width = 12,
                      verbatimTextOutput("anova_test"),
                      verbatimTextOutput("t_test"),
                      verbatimTextOutput("kruskal_test")
                  )
                )
              )
      ),
      tabItem(tabName = "glm_model",
              fluidPage(
                h2("Modèle GLM", align = "center"),
                tabBox(id = "tab_glm", width = 12,
                       tabPanel("Modèle de fréquence",
                                fluidRow(
                                  box(title = "Résumé du modèle", width = 12, status = "primary", solidHeader = TRUE,
                                      verbatimTextOutput("glm_freq_output"))
                                ),
                                fluidRow(
                                  column(4,
                                         selectInput("glm_freq_variable", "Choisir une variable :", 
                                                     choices = c("Classe d'âge" = "age", "Région" = "region", "Puissance" = "power"))
                                  )
                                ),
                                fluidRow(
                                  box(title = "Courbe dynamique", width = 12,
                                      plotOutput("glm_freq_selected_plot"))
                                ),
                                fluidRow(
                                  box(title = "Indice de Gini", width = 12, status = "info", solidHeader = TRUE,
                                      verbatimTextOutput("gini_freq_output"))
                                )
                       ),
                       
                       
                       tabPanel("Modèle de coût moyen",
                                verbatimTextOutput("glm_cost_output"),
                                selectInput("cost_var_select", "Choisir une variable :", 
                                            choices = c("Classe d'âge" = "Age_Class", 
                                                        "Région" = "Region", 
                                                        "Puissance" = "Power_Class")),
                                h4("Courbe dynamique"),
                                plotOutput("glm_cost_plot_dynamic"),
                                verbatimTextOutput("gini_cost_output")
                       ),
                       tabPanel("Modèle Coût × Fréquence", 
                                verbatimTextOutput("glm_mix_output"), 
                                plotOutput("glm_mix_plot"))
                )
              )
      ),
      tabItem(tabName = "xgboost_model",
              fluidPage(
                h2("Modèle XGBoost", align = "center"),
                tabBox(id = "xgb_tabs", width = 12,
                       
                       # Résultats ClaimNb
                       tabPanel("📊 Résultats ClaimNb",
                                verbatimTextOutput("xgb_metrics_claimnb")
                       ),
                       
                       # Résultats Claimfre
                       tabPanel("📊 Résultats Claimfre",
                                verbatimTextOutput("xgb_metrics_claimfre")
                       ),
                       
                       # Arbre ClaimNb
                       tabPanel("🌳 Arbre ClaimNb",
                                selectInput("tree_index_claimnb", "Choisir l'arbre ClaimNb :", choices = 0:4),
                                htmlOutput("xgb_tree_claimnb")
                       ),
                       
                       # Arbre Claimfre
                       tabPanel("🌳 Arbre Claimfre",
                                selectInput("tree_index_claimfre", "Choisir l'arbre Claimfre :", choices = 0:2),
                                htmlOutput("xgb_tree_claimfre")
                       ),
                       # Onglet Random Search
                       tabPanel("🎯 Random Search",
                                actionButton("run_random_search", "Lancer la recherche aléatoire"),
                                verbatimTextOutput("xgb_random_results")
                       )
                       
                )
              )
      )
      
      
    )
)
)
)
)
)



# --- Server ---

server <- function(input, output) {
    observeEvent(input$start_btn, {
      hide("landing_page")
      show("main_dashboard")
    })
  
  output$mapPlot <- renderLeaflet({
    leaflet(data = France_map) %>%
      addProviderTiles("CartoDB.Positron") %>%  # Tu peux changer pour "Esri.WorldImagery" si tu veux satellite
      addPolygons(
        fillColor = ~colorNumeric("YlOrRd", domain = France_map$Total_Claim)(Total_Claim),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0(nom, ": ", Total_Claim, " sinistres")
      )
  })
  
  

  # Function to show the welcome message modal
  showWelcomeMessage <- function() {
    showModal(modalDialog(
      title = "Bienvenue dans mon application!",
      HTML("Merci d'utiliser mon application !😊"),
      footer = NULL
    ))
    # Fermer la fenêtre automatiquement après 2 secondes
    shinyjs::runjs("setTimeout(function(){ $('.modal').modal('hide'); }, 2000);")
  

  }
  # Afficher le nombre de lignes de data_joined
  output$rowCount <- renderText({
    paste("Nombre de lignes :", nrow(data_joined))
  })
  
  # Afficher le nombre de colonnes de data_joined
  output$colCount <- renderText({
    paste("Nombre de colonnes :", ncol(data_joined))
  })
  
  # Call the function to show the welcome message when the app is loaded
  showWelcomeMessage()
   output$table <- DT::renderDataTable({
    data_joined
  })
   
  output$glm_freq_selected_plot <- renderPlot({
    req(input$glm_freq_variable)
    
    if (input$glm_freq_variable == "age") {
      data_group <- test_data %>%
        group_by(Age_Class) %>%
        summarise(ClaimNb = sum(ClaimNb, na.rm = TRUE),
                  MeanClaimNb = mean(ClaimNb, na.rm = TRUE),
                  MeanPredictedClaimNb = mean(PredictedClaimNb, na.rm = TRUE))
      
      ggplot(data_group, aes(x = Age_Class)) +
        geom_bar(aes(y = ClaimNb, fill = "Réclamations"), stat = "identity") +
        geom_point(aes(y = MeanClaimNb * max(ClaimNb) / max(MeanClaimNb), color = "Observé"), size = 3) +
        geom_point(aes(y = MeanPredictedClaimNb * max(ClaimNb) / max(MeanPredictedClaimNb), color = "Prévu"), size = 3) +
        labs(title = "Réclamations par Classe d'âge", x = "Classe d'âge", y = "Nombre total") +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$glm_freq_variable == "region") {
      data_group <- test_data %>%
        group_by(Region) %>%
        summarise(ClaimNb = sum(ClaimNb, na.rm = TRUE),
                  MeanClaimNb = mean(ClaimNb, na.rm = TRUE),
                  MeanPredictedClaimNb = mean(PredictedClaimNb, na.rm = TRUE))
      
      ggplot(data_group, aes(x = reorder(Region, -ClaimNb))) +
        geom_bar(aes(y = ClaimNb, fill = "Réclamations"), stat = "identity") +
        geom_point(aes(y = MeanClaimNb * max(ClaimNb) / max(MeanClaimNb), color = "Observé"), size = 3) +
        geom_point(aes(y = MeanPredictedClaimNb * max(ClaimNb) / max(MeanPredictedClaimNb), color = "Prévu"), size = 3) +
        labs(title = "Réclamations par Région", x = "Région", y = "Nombre total") +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$glm_freq_variable == "power") {
      data_group <- test_data %>%
        group_by(Power) %>%
        summarise(ClaimNb = sum(ClaimNb, na.rm = TRUE),
                  MeanClaimNb = mean(ClaimNb, na.rm = TRUE),
                  MeanPredictedClaimNb = mean(PredictedClaimNb, na.rm = TRUE))
      
      ggplot(data_group, aes(x = Power)) +
        geom_bar(aes(y = ClaimNb, fill = "Réclamations"), stat = "identity") +
        geom_point(aes(y = MeanClaimNb * max(ClaimNb) / max(MeanClaimNb), color = "Observé"), size = 3) +
        geom_point(aes(y = MeanPredictedClaimNb * max(ClaimNb) / max(MeanPredictedClaimNb), color = "Prévu"), size = 3) +
        labs(title = "Réclamations par Puissance", x = "Puissance", y = "Nombre total") +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  }) 

  
  output$summary_output <- renderPrint({
    if (input$plot_type %in% c("hist", "box")) {
      req(input$num_var)
      summary(data_joined[[input$num_var]])   
    } else {
      req(input$cat_var)
      summary(data_joined[[input$cat_var]])
    }
  })
  
  output$univar_plot <- renderPlot({
    req(input$plot_type)
    
    if (input$plot_type == "hist") {
      req(input$num_var)
      
      # Découpage manuel en bins
      df_hist <- data_joined %>%
        mutate(bin = cut(.data[[input$num_var]],
                         breaks = input$nb_bins,
                         include.lowest = TRUE)) %>%
        group_by(bin) %>%
        summarise(count = n()) %>%
        mutate(bin = as.factor(bin))
      
      ggplot(df_hist, aes(x = bin, y = count, fill = bin)) +
        geom_bar(stat = "identity", color = "white") +
        scale_fill_brewer(palette = "Paired") +
        labs(title = paste("Histogramme  de", input$num_var),
             x = "Classe",
             y = "Nombre") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")
      
    }  else if (input$plot_type == "box") {
      req(input$num_var)
      ggplot(data_joined, aes(x = "", y = .data[[input$num_var]])) +
        geom_boxplot(width = input$box_width, fill = "orange", color = "black") +
        labs(title = paste("Boxplot de", input$num_var), y = input$num_var, x = "") +
        theme_minimal()
    }
    
      
    else if (input$plot_type == "bar") {
      req(input$cat_var)
      ggplot(data_joined, aes(x = .data[[input$cat_var]], fill = .data[[input$cat_var]])) +
        geom_bar() +
        scale_fill_brewer(palette = "Set3") +
        labs(title = paste("Barplot de", input$cat_var), x = input$cat_var, y = "Nombre") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")
    }
  })
  
  
  # Corrélation
  output$corrplot <- renderPlot({
    cor_matrix <- cor(data_joined[numeric_vars], use = "complete.obs")
    corrplot(cor_matrix, method = "color", type = "upper",
             tl.col = "black", tl.cex = 0.8, number.cex = 0.7,
             col = colorRampPalette(c("blue", "white", "red"))(200))
  })
  
  output$correlation_table <- renderDT({
    cor_matrix <- cor(data_joined[numeric_vars], use = "complete.obs")
    datatable(as.data.frame(round(cor_matrix, 2)), options = list(pageLength = 10))
  })
  
  # Scatterplot Age Conducteur vs Montant Sinistre
  output$scatter_age_severity <- renderPlot({
    data_filtered <- data_joined %>% filter(Severity_Total > 0)
    ggplot(data_filtered, aes(x = DriverAge, y = Severity_Total)) +
      geom_point(alpha = 0.5, color = "blue") +
      ggtitle("Relation entre l'âge du conducteur et le montant du sinistre") +
      xlab("Âge du conducteur") +
      ylab("Montant du sinistre") +
      theme_minimal()
  })
  
  # Scatterplot Age Conducteur vs Nombre de Sinistres (par Marque)
  output$scatter_brand <- renderPlot({
    data_filtered <- data_joined %>%
      filter(!is.na(DriverAge), !is.na(ClaimNb), !is.na(Brand)) %>%
      mutate(Brand = as.factor(Brand))
    
    ggplot(data_filtered, aes(x = DriverAge, y = ClaimNb, color = Brand)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
      facet_wrap(~ Brand) +
      labs(title = "Relation Âge Conducteur - Nombre Sinistres (par Marque)",
           x = "Âge Conducteur", y = "Nombre de Sinistres", color = "Marque") +
      theme_minimal()
  })
  
  # Boxplot Nombre de Sinistres par Âge (par Marque)
  output$boxplot_brand <- renderPlot({
    data_filtered <- data_joined %>%
      filter(!is.na(DriverAge), !is.na(ClaimNb), !is.na(Brand)) %>%
      mutate(Brand = as.factor(Brand))
    
    ggplot(data_filtered, aes(x = as.factor(DriverAge), y = ClaimNb, fill = Brand)) +
      geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
      facet_wrap(~ Brand, scales = "free_y") +
      labs(title = "Distribution Nombre de Sinistres par Âge Conducteur (par Marque)",
           x = "Âge Conducteur", y = "Nombre de Sinistres") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  # Boxplot Montant Sinistres par Classe d'Âge
  output$boxplot_ageclass <- renderPlot({
    data_filtered <- data_joined %>%
      filter(!is.na(Age_Class), !is.na(Severity_Total))
    
    ggplot(data_filtered, aes(x = Age_Class, y = Severity_Total, color = Age_Class)) +
      geom_jitter(alpha = 0.5, width = 0.2) +
      geom_boxplot(alpha = 0.3, outlier.shape = NA) +
      labs(title = "Montant des Sinistres par Classe d'Âge",
           x = "Classe d'Âge", y = "Montant Sinistres") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  })
  
  # Barplot Montant Moyen par Classe d'Âge
  output$barplot_ageclass <- renderPlot({
    data_age_summary <- data_joined %>%
      group_by(Age_Class) %>%
      summarise(Mean_ClaimAmount = mean(Severity_Total, na.rm = TRUE))
    
    ggplot(data_age_summary, aes(x = Age_Class, y = Mean_ClaimAmount, fill = Age_Class)) +
      geom_bar(stat = "identity", width = 0.6, color = "black") +
      geom_text(aes(label = round(Mean_ClaimAmount, 2)), vjust = -0.5, size = 5) +
      labs(title = "Montant Moyen Sinistres par Classe d'Âge",
           x = "Classe d'Âge", y = "Montant Moyen Sinistres") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  })
  
  # Tests Statistiques
  output$anova_test <- renderPrint({
    anova_test <- aov(Severity_Total ~ Brand, data = data_joined)
    summary(anova_test)
  })
  
  output$t_test <- renderPrint({
    t_test <- t.test(Severity_Total ~ Gas, data = data_joined)
    t_test
  })
  
  output$kruskal_test <- renderPrint({
    kruskal_test <- kruskal.test(Severity_Total ~ Brand, data = data_joined)
    kruskal_test
  })
  # --- Préparation des données et modèle GLM fréquence ---
  # ➕ Création de log(Density)
  # 1. Nettoyage
  data_joined <- data_joined %>%
    filter(Exposure > 0) %>%
    mutate(Claimfre = ClaimNb / Exposure)
  
  # 2. 🔥 Plafonnement AVANT split
  threshold <- quantile(data_joined$Claimfre, 0.99, na.rm = TRUE)
  data_joined <- data_joined %>%
    mutate(Claimfre = ifelse(Claimfre > threshold, threshold, Claimfre))
  
  # ➕ Ajout log_Density ici
  data_joined$log_Density <- log(data_joined$Density + 1)
  
  # 3. ✅ Split en train/test APRES capping
  set.seed(123)
  split <- sample.split(data_joined$ClaimNb, SplitRatio = 0.75)
  train_data <- subset(data_joined, split == TRUE)
  test_data <- subset(data_joined, split == FALSE)
  
  
  freq_model_train <- glm(ClaimNb ~ Age_Class + CarAgeClass + Power + Region + Gas + Brand + offset(log(Exposure)), 
                          family = quasipoisson, data = train_data)
  
  test_data$PredictedClaimNb <- predict(freq_model_train, newdata = test_data, type = "response")
  
  gini_index <- function(actual, predicted) {
    df <- data.frame(actual = actual, predicted = predicted)
    df <- df[order(-df$predicted), ]  # Tri décroissant sur les prédictions
    df$random <- (1:nrow(df)) / nrow(df)
    total_actual <- sum(df$actual)
    df$cum_actual <- cumsum(df$actual) / total_actual
    gini <- sum(df$cum_actual - df$random)
    return(gini)
  }
  normalized_gini <- function(actual, predicted) {
    gini_model <- gini_index(actual, predicted)
    gini_perfect <- gini_index(actual, actual)
    return(gini_model / gini_perfect)
  }
  
  output$glm_freq_output <- renderPrint({
    summary(freq_model_train)
  })
  
  output$gini_freq_output <- renderPrint({
    normalized_gini <- function(actual, predicted) {
      gini_model <- gini_index(actual, predicted)
      gini_perfect <- gini_index(actual, actual)
      return(gini_model / gini_perfect)
    }
    
    pred_train <- predict(freq_model_train, newdata = train_data, type = "response")
    gini_train <- normalized_gini(train_data$ClaimNb, pred_train)
    gini_test <- normalized_gini(test_data$ClaimNb, test_data$PredictedClaimNb)
    
    cat("Indice de Gini normalisé sur Train:", round(gini_train, 4), "
")
    cat("Indice de Gini normalisé sur Test:", round(gini_test, 4))
  })
  
  output$glm_freq_plot_age <- renderPlot({
    data_age <- test_data %>%
      group_by(Age_Class) %>%
      summarise(MeanClaimNb = mean(ClaimNb, na.rm = TRUE),
                MeanPredictedClaimNb = mean(PredictedClaimNb, na.rm = TRUE),
                ClaimNb = sum(ClaimNb, na.rm = TRUE))
    ggplot(data_age, aes(x = Age_Class)) +
      geom_bar(aes(y = ClaimNb, fill = "Réclamations"), stat = "identity") +
      geom_point(aes(y = MeanClaimNb * max(ClaimNb) / max(MeanClaimNb), color = "Observé"), size = 3) +
      geom_point(aes(y = MeanPredictedClaimNb * max(ClaimNb) / max(MeanPredictedClaimNb), color = "Prévu"), size = 3) +
      scale_y_continuous(name = "Nombre total des réclamations") +
      labs(title = "Réclamations par classe d'âge", x = "Classe d'âge") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$glm_freq_plot_region <- renderPlot({
    data_region <- test_data %>%
      group_by(Region) %>%
      summarise(MeanClaimNb = mean(ClaimNb, na.rm = TRUE),
                MeanPredictedClaimNb = mean(PredictedClaimNb, na.rm = TRUE),
                ClaimNb = sum(ClaimNb, na.rm = TRUE))
    ggplot(data_region, aes(x = reorder(Region, -ClaimNb))) +
      geom_bar(aes(y = ClaimNb, fill = "Réclamations"), stat = "identity") +
      geom_point(aes(y = MeanClaimNb * max(ClaimNb) / max(MeanClaimNb), color = "Observé"), size = 3) +
      geom_point(aes(y = MeanPredictedClaimNb * max(ClaimNb) / max(MeanPredictedClaimNb), color = "Prévu"), size = 3) +
      scale_y_continuous(name = "Nombre total des réclamations") +
      labs(title = "Réclamations par Région", x = "Région") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$glm_freq_plot_power <- renderPlot({
    data_power <- test_data %>%
      group_by(Power) %>%
      summarise(MeanClaimNb = mean(ClaimNb, na.rm = TRUE),
                MeanPredictedClaimNb = mean(PredictedClaimNb, na.rm = TRUE),
                ClaimNb = sum(ClaimNb, na.rm = TRUE))
    ggplot(data_power, aes(x = Power)) +
      geom_bar(aes(y = ClaimNb, fill = "Réclamations"), stat = "identity") +
      geom_point(aes(y = MeanClaimNb * max(ClaimNb) / max(MeanClaimNb), color = "Observé"), size = 3) +
      geom_point(aes(y = MeanPredictedClaimNb * max(ClaimNb) / max(MeanPredictedClaimNb), color = "Prévu"), size = 3) +
      scale_y_continuous(name = "Nombre total des réclamations") +
      labs(title = "Réclamations par Puissance du véhicule", x = "Puissance") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  # === MODELE COUT MOYEN ===
  
  gamma_model <- glm(
    Severity_Total ~ Age_Class + CarAgeClass + Region + Brand + Gas + log_Density,
    family = Gamma(link = "log"),
    data = train_data
  )
  
  train_data$PredictedSeverity_Total <- predict(gamma_model, newdata = train_data, type = "response")
  test_data$PredictedSeverity_Total <- predict(gamma_model, newdata = test_data, type = "response")
  
  output$glm_cost_output <- renderPrint({
    summary(gamma_model)
  })
  
  
  # Gini pour le modèle de coût
  output$gini_cost_output <- renderPrint({
    gini_train <- normalized_gini(train_data$Severity_Total, train_data$PredictedSeverity_Total)
    gini_test <- normalized_gini(test_data$Severity_Total, test_data$PredictedSeverity_Total)
    
    cat("Indice de Gini normalisé sur Train:", round(gini_train, 4), "\n")
    cat("Indice de Gini normalisé sur Test:", round(gini_test, 4))
  })
  
  # Courbe dynamique pour coût moyen
  output$glm_cost_plot_dynamic <- renderPlot({
    var_selected <- input$cost_var_select
    
    req(var_selected)
    req(test_data[[var_selected]])
    
    plot_data <- test_data %>%
      filter(!is.na(.data[[var_selected]])) %>%
      group_by(.data[[var_selected]]) %>%
      summarise(
        TotalCost = sum(Severity_Total, na.rm = TRUE),
        MeanObs = mean(Severity_Total, na.rm = TRUE),
        MeanPred = mean(PredictedSeverity_Total, na.rm = TRUE)
      )
    
    ggplot(plot_data, aes(x = .data[[var_selected]])) +
      geom_bar(aes(y = TotalCost, fill = "Total Observé"), stat = "identity") +
      geom_point(aes(y = MeanObs * max(TotalCost) / max(MeanObs), color = "Moyenne Observée"), size = 3) +
      geom_point(aes(y = MeanPred * max(TotalCost) / max(MeanPred), color = "Moyenne Prédite"), size = 3) +
      scale_y_continuous(name = "Montant total des sinistres") +
      labs(title = paste("Montant par", var_selected), x = var_selected) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # === GLM Tweedie pour Prime Pure ===
  library(tweedie)
  library(statmod)
  library(MASS)
  
  # 💡 Construction de la variable cible
  train_data$PurePremium_Obs <- train_data$ClaimNb * train_data$Severity_Total
  test_data$PurePremium_Obs <- test_data$ClaimNb * test_data$Severity_Total
  
  # 🔧 Winsorisation : on limite les extrêmes (ex. 99e percentile)
  pp_threshold <- quantile(train_data$PurePremium_Obs, 0.99, na.rm = TRUE)
  train_data <- train_data %>%
    mutate(PurePremium_Obs = ifelse(PurePremium_Obs > pp_threshold, pp_threshold, PurePremium_Obs))
  
  # 🔍 Grid search sur var.power
  powers <- seq(1.1, 2.0, by = 0.1)
  cv_results <- data.frame()
  
  for (p in powers) {
    model <- glm(
      PurePremium_Obs ~ Age_Class + CarAgeClass + Region + Brand + Gas + log_Density +
        Age_Class:Region + Brand:CarAgeClass,  # interactions ajoutées
      family = tweedie(var.power = p, link.power = 0),
      data = train_data
    )
    
    pred <- predict(model, newdata = test_data, type = "response")
    rmse <- sqrt(mean((test_data$PurePremium_Obs - pred)^2, na.rm = TRUE))
    mae <- mean(abs(test_data$PurePremium_Obs - pred), na.rm = TRUE)
    mape <- mean(abs((test_data$PurePremium_Obs - pred) / 
                       ifelse(test_data$PurePremium_Obs == 0, NA, test_data$PurePremium_Obs)), na.rm = TRUE) * 100
    
    cv_results <- rbind(cv_results, data.frame(var_power = p, RMSE = rmse, MAE = mae, MAPE = mape))
  }
  
  # 📈 Sélection du meilleur var.power
  best_p <- cv_results$var_power[which.min(cv_results$RMSE)]
  
  # ✅ Modèle final avec meilleur var.power
  glm_tweedie_best <- glm(
    PurePremium_Obs ~ Age_Class + CarAgeClass + Region + Brand + Gas + log_Density +
      Age_Class:Region + Brand:CarAgeClass,
    family = tweedie(var.power = best_p, link.power = 0),
    data = train_data
  )
  
  # 🧠 Prédictions
  test_data$PurePremium_Pred_Tweedie <- predict(glm_tweedie_best, newdata = test_data, type = "response")
  
  # 📊 Évaluation finale
  output$glm_mix_output <- renderPrint({
    rmse <- sqrt(mean((test_data$PurePremium_Obs - test_data$PurePremium_Pred_Tweedie)^2, na.rm = TRUE))
    mae <- mean(abs(test_data$PurePremium_Obs - test_data$PurePremium_Pred_Tweedie), na.rm = TRUE)
    mape <- mean(abs((test_data$PurePremium_Obs - test_data$PurePremium_Pred_Tweedie) / 
                       ifelse(test_data$PurePremium_Obs == 0, NA, test_data$PurePremium_Obs)), na.rm = TRUE) * 100
    
    cat(sprintf("Modèle Tweedie (optimisé)\nBest var.power: %.1f\nRMSE: %.2f\nMAE: %.2f\nMAPE: %.2f%%\n",
                best_p, rmse, mae, mape))
  })
  
  # 🔍 Visualisation
  output$glm_mix_plot <- renderPlot({
    ggplot(test_data, aes(x = PurePremium_Obs, y = PurePremium_Pred_Tweedie)) +
      geom_point(alpha = 0.4, color = "darkgreen") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Prime pure : Observée vs Prédite (GLM Tweedie Optimisé)",
           x = "Prime pure observée",
           y = "Prime pure prédite") +
      theme_minimal()
  })
  
  
  
  

  # === Préparation XGBoost ===
  library(recipes)
  library(xgboost)
  library(shinyjs)
  library(caret)
  
  # --- Capping Claimfre & Density ---
  threshold <- quantile(data_joined$Claimfre, 0.99, na.rm = TRUE)
  data_joined <- data_joined %>%
    mutate(
      Claimfre = ifelse(Claimfre > threshold, threshold, Claimfre),
      Density = ifelse(Density > 20000, 20000, Density)
    )
  
  # --- Modèle XGBoost pour ClaimNb (inchangé) ---
  X1 <- data_joined %>%
    dplyr::select(Power, Brand, Gas, Region, Density, Age_Class, Severity_Total)
  y1 <- data_joined$ClaimNb
  
  prepare_model <- function(X, y, max_depth = 6, eta = 0.1) {
    recipe_obj <- recipe(~ ., data = X) %>%
      step_impute_median(all_numeric(), -all_outcomes()) %>%
      step_impute_mode(all_nominal(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes()) %>%
      step_center(all_numeric(), -all_outcomes()) %>%
      step_scale(all_numeric(), -all_outcomes())
    
    train_index <- createDataPartition(y, p = 0.8, list = FALSE)
    X_train <- X[train_index, ]
    X_test <- X[-train_index, ]
    y_train <- y[train_index]
    y_test <- y[-train_index]
    
    trained_recipe <- prep(recipe_obj, training = X_train)
    X_train_processed <- bake(trained_recipe, new_data = X_train)
    X_test_processed <- bake(trained_recipe, new_data = X_test)
    
    dtrain <- xgb.DMatrix(data = as.matrix(X_train_processed), label = y_train)
    dtest <- xgb.DMatrix(data = as.matrix(X_test_processed), label = y_test)
    
    params <- list(objective = "reg:squarederror", eta = eta, max_depth = max_depth, eval_metric = "rmse")
    
    model <- xgb.train(params = params, data = dtrain, nrounds = 100, verbose = 0)
    
    list(model = model, dtrain = dtrain, dtest = dtest, y_train = y_train, y_test = y_test)
  }
  
  model_claimnb <- prepare_model(X1, y1)
  
  output$xgb_metrics_claimnb <- renderPrint({
    y_pred <- predict(model_claimnb$model, model_claimnb$dtest)
    cat(sprintf("ClaimNb\nRMSE: %.2f\nMAE: %.4f\nMSE: %.4f\nMAPE: %.2f%%\n",
                sqrt(mean((model_claimnb$y_test - y_pred)^2)),
                mean(abs(model_claimnb$y_test - y_pred)),
                mean((model_claimnb$y_test - y_pred)^2),
                mean(abs((model_claimnb$y_test - y_pred) / model_claimnb$y_test)) * 100))
  })
  
  # --- Nouveau modèle XGBoost pour Claimfre (optimisé) ---
  target <- "Claimfre"
  y <- data_joined[[target]]
  
  X <- data_joined %>%
    dplyr::select(Power, Brand, Gas, Region, Density, Age_Class, Severity_Total)
  
  recipe_obj_fre <- recipe(~ ., data = X) %>%
    step_impute_median(all_numeric(), -all_outcomes()) %>%
    step_impute_mode(all_nominal(), -all_outcomes()) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_center(all_numeric(), -all_outcomes()) %>%
    step_scale(all_numeric(), -all_outcomes())
  
  set.seed(42)
  train_index <- createDataPartition(y, p = 0.8, list = FALSE)
  X_train <- X[train_index, ]
  X_test <- X[-train_index, ]
  y_train <- y[train_index]
  y_test <- y[-train_index]
  
  trained_recipe <- prep(recipe_obj_fre, training = X_train)
  X_train_processed <- bake(trained_recipe, new_data = X_train)
  X_test_processed <- bake(trained_recipe, new_data = X_test)
  
  dtrain_fre <- xgb.DMatrix(data = as.matrix(X_train_processed), label = y_train)
  dtest_fre <- xgb.DMatrix(data = as.matrix(X_test_processed), label = y_test)
  
  params_fre <- list(
    objective = "reg:squarederror",
    eta = 0.8,
    max_depth = 10,
    subsample = 0.8,
    colsample_bytree = 0.8,
    eval_metric = "rmse"
  )
  
  model_claimfre <- xgb.train(params = params_fre, data = dtrain_fre, nrounds = 100, verbose = 0)
  
  output$xgb_metrics_claimfre <- renderPrint({
    y_pred <- predict(model_claimfre, dtest_fre)
    rmse_val <- sqrt(mean((y_test - y_pred)^2))
    mae_val <- mean(abs(y_test - y_pred))
    mse_val <- mean((y_test - y_pred)^2)
    mape_val <- mean(abs((y_test - y_pred) / y_test)) * 100
    
    cat(sprintf("Claimfre\nRMSE: %.4f\nMAE: %.4f\nMSE: %.4f\nMAPE: %.2f%%\n",
                rmse_val, mae_val, mse_val, mape_val))
  })
  
  

  output$xgb_tree_claimnb <- renderUI({
    req(input$tree_index_claimnb)
    
    widget <- xgb.plot.tree(
      model = model_claimnb$model,
      trees = as.numeric(input$tree_index_claimnb),
      render = TRUE
    )
    outfile <- "www/tree_view_nb.html"
    htmlwidgets::saveWidget(widget, file = outfile, selfcontained = TRUE)
    
    tags$iframe(src = "tree_view_nb.html",
                width = "2000px",
                height = "1500px",
                style = "zoom: 1.5; transform: scale(1.5); transform-origin: 0 0;",
                frameborder = 0)
  })
  
  output$xgb_tree_claimfre <- renderUI({
    req(input$tree_index_claimfre)
    
    widget <- xgb.plot.tree(
      model = model_claimfre,  # <--- ici changement !
      trees = as.numeric(input$tree_index_claimfre),
      render = TRUE
    )
    outfile <- "www/tree_view_fre.html"
    htmlwidgets::saveWidget(widget, file = outfile, selfcontained = TRUE)
    
    tags$iframe(src = "tree_view_fre.html",
                width = "2000px",
                height = "2000px",
                style = "zoom:2; transform: scale(2); transform-origin: 0 0;",
                frameborder = 0)
  })
  
  
  
  
  
  
  observeEvent(input$run_random_search, {
    set.seed(123)
    param_grid <- expand.grid(
      eta = runif(5, 0.01, 0.3),
      max_depth = sample(3:10, 5, replace = TRUE),
      subsample = runif(5, 0.5, 1),
      colsample_bytree = runif(5, 0.5, 1)
    )
    
    results <- apply(param_grid, 1, function(params) {
      mod <- xgb.train(
        params = list(
          objective = "reg:squarederror",
          eta = params["eta"],
          max_depth = as.integer(params["max_depth"]),
          subsample = params["subsample"],
          colsample_bytree = params["colsample_bytree"]
        ),
        data = dtrain,
        nrounds = 100,
        verbose = 0
      )
      pred <- predict(mod, dtest)
      rmse <- sqrt(mean((y_test - pred)^2))
      return(c(rmse = rmse, eta = params["eta"], depth = params["max_depth"]))
    })
    
    results_df <- as.data.frame(t(results))
    best <- results_df[which.min(results_df$rmse), ]
    
    output$xgb_random_results <- renderPrint({
      cat("Meilleur paramètre trouvé par recherche aléatoire :\n")
      print(best)
    })
  })

 

  output$glm_step_output <- renderPrint({ "Modèle Stepwise à compléter" })
  output$glm_step_plot <- renderPlot({ plot(1, 1, main = "Placeholder Stepwise") })
  output$xgb_placeholder <- renderPrint({ "XGBoost en cours d'intégration..." })
}

shinyApp(ui, server)
