library(shinyalert)
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(stringr)
library(DT)
library(shinyjs)
library(tidyr)
library(shinythemes)

##############################################################
#                    DATA EVALUATION FILE                    # 
##############################################################

data_evaluation <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
file.copy("data_evaluation.csv", paste0("backup_data_evaluation/data_evaluation",format(Sys.time(), "_%d-%m-%Y_%H-%M-%S"),".csv"))

# Comptérences RCnum

competences_rcnum <- c(
  "RCnum 1.0.1",
  "RCnum 1.0.2",
  "RCnum 1.0.3",
  "RCnum 1.0.4",
  "RCnum 1.1.1",
  "RCnum 1.1.2",
  "RCnum 1.2.1",
  "RCnum 1.2.2",
  "RCnum 1.2.3",
  "RCnum 1.2.4",
  "RCnum 1.3.1",
  "RCnum 1.3.2",
  "RCnum 1.3.3",
  "RCnum 1.4.1",
  "RCnum 1.4.2",
  "RCnum 1.4.3",
  "RCnum 2.1.1",
  "RCnum 2.1.2",
  "RCnum 2.1.3",
  "RCnum 2.1.4",
  "RCnum 2.2.1",
  "RCnum 2.2.2",
  "RCnum 2.3.1",
  "RCnum 2.3.2",
  "RCnum 2.4.1",
  "RCnum 2.4.2",
  "RCnum 2.4.3",
  "RCnum 2.4.4",
  "RCnum 3.1.1",
  "RCnum 3.1.2",
  "RCnum 3.1.3",
  "RCnum 3.2.1",
  "RCnum 3.2.2",
  "RCnum 3.2.3",
  "RCnum 3.2.4",
  "RCnum 3.3.1",
  "RCnum 3.4.1",
  "RCnum 3.4.2",
  "RCnum 3.4.3",
  "RCnum 3.5.1",
  "RCnum 3.5.2",
  "RCnum 3.6.1",
  "RCnum 3.6.2",
  "RCnum 4.1.1",
  "RCnum 4.1.2",
  "RCnum 4.1.3",
  "RCnum 4.1.4",
  "RCnum 4.1.5",
  "RCnum 4.1.6",
  "RCnum 4.2.1",
  "RCnum 4.2.2",
  "RCnum 4.3.1",
  "RCnum 4.3.2",
  "RCnum 4.3.3",
  "RCnum 4.4.1",
  "RCnum 4.4.2",
  "RCnum 4.4.3",
  "RCnum 5.1.1",
  "RCnum 5.1.2",
  "RCnum 5.2.1",
  "RCnum 5.2.2",
  "RCnum 5.3.1",
  "RCnum 5.3.2",
  "RCnum 5.3.3",
  "RCnum 6.1.1",
  "RCnum 6.1.2",
  "RCnum 6.1.3",
  "RCnum 6.2.1",
  "RCnum 6.2.2",
  "RCnum 6.2.3",
  "RCnum 6.2.4",
  "RCnum 6.3.1",
  "RCnum 6.3.2",
  "RCnum 6.3.3",
  "RCnum 6.3.4",
  "RCnum non spécifié")

# function to add an entry to the table used by the dashboard

new_entry <- function(table,date,cycle_formation,annee_volee,phase,participants,journee,nombre_participants,niveau,critere,activite,rcnum,pernum,score,url) {
  table[nrow(table) + 1,] = c(date, cycle_formation, annee_volee, phase, participants, journee, nombre_participants, niveau, critere, activite, rcnum, pernum, score,url)
  return(table)
}

ui <- dashboardPage(title="Traitement des données d'évaluation",
  
  dashboardHeader(title = (span("Traitement des données d'évaluation",
                               style = "font-size: 28px")),
                  titleWidth = 400,
                  tags$li(actionLink("openModal", label = "", icon = icon("info")),
                          class = "dropdown")),

  dashboardSidebar(
    
    useShinyjs(),
    
    width = 300,
    
    HTML("<p align='justify'>Ce tableau de bord permet de traiter les données destinées aux retours de questionnaires et au tableau de bord de l'évaluation de la formation EduNum</p>"),
    
    sidebarMenu(id = "sidebarid",
                
         menuItem("1. Renommer les données", tabName = "page2"),
         conditionalPanel(
           'input.sidebarid == "page2"',
           fileInput("file2", label = "Charger le fichier CSV brut:")),
    
         menuItem("2. Sélectionner les données d'intérêt", tabName = "page1"),
         conditionalPanel(
           'input.sidebarid == "page1"',
           fileInput("file1", label = "Charger le fichier CSV traité:", accept = ".csv"),
           textInput("url", label = "Indiquer l'url du rapport détaillé:"),
           selectInput(inputId = "var1",label = "Sélectionner le cycle ou la formation:",choices = c("C1", "C2a (5-6P)", "C2b (7-8P)","C2b (7-8P) 20 périodes", "C3 MUS", "Secondaire 2", "C3 SI")),
           selectInput(inputId = "var2",label = "Sélectionner la phase:",choices = c("Pilote", "Déploiement","Non concerné")),
           selectInput(inputId = "var3",label = "Sélectionner la volée:",choices = c(1:4, "Non concerné")),
           selectInput(inputId = "var4",label = "Sélectionner le type de participants:",choices = c("Enseignants", "Personnes-ressources", "Enseignants-formateurs")),
           selectInput(inputId = "var5",label = "Sélectionner la journée:",choices = c(1:20)),
           HTML("<br>")),
         
         menuItem("•  Voir les dernières entrées du tableau", tabName = "page4"),
         conditionalPanel(
           'input.sidebarid == "page4"'))),
         
  
  
  dashboardBody(
    
    tags$head(
      tags$style(
        HTML(
          "p {
      margin: 15px;
         }"
        )
      )
    ),
    
    
    tabItems(

      tabItem(tabName = "page1",

    fluidRow(
      tabBox(title = "Variables participant.e.s",
             id = "tabset0", height = "250px", width=12,
             tabPanel("Motivation prélable",
                      HTML("<h5>Le score reflète la motivation préalable des participants à la formation.<br><br></h5>"),
                      selectizeInput(inputId = "var_motivation", label = "Colonne(s) contenant les scores de motivation préalable:" , choices = "", multiple = T, width = 700, options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions à choix multiple (o) entre 1 à 7...")),
                      actionButton("add_motivation", label = "Ajouter au tableau"),
                      textOutput("confirm_motivation")),
             tabPanel("Attitude envers l'éducation numérique",
                      HTML("<h5>Le score reflète le degré d'adhésion des participants envers l'éducation numérique.<br><br></h5>"),
                      selectizeInput(inputId = "var_attitude", label = "Colonne(s) contenant les scores d'attitude préalable:" , choices = "", multiple = T, width = 700, options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions à choix multiple (o) entre 1 à 7...")),
                      actionButton("add_attitude", label = "Ajouter au tableau"),
                      textOutput("confirm_attitude"))
             )),
              
    
    fluidRow(
    tabBox(title = "Variables formation",
           id = "tabset1", height = "260px", width=12,
      tabPanel("Implication/Appréciation",
               HTML("<h5>Le score reflète le degré d'implication ou d'appréciation de l'équipe de formation rapporté par les participants à l'issue de la journée de formation.<br><br></h5>"),
               selectizeInput(inputId = "var_implication", label = "Colonne(s) contenant les scores d'implication des format.eur.rices:" , choices = "", multiple = T, width = 700, options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions à choix multiple (o) entre 1 à 7...")),
               actionButton("add_implication", label = "Ajouter au tableau"),
               textOutput("confirm_implication")),
      tabPanel("Qualité perçue des méthodes pédagogiques employées",
               HTML("<h5>Le score reflète la qualité perçue par les participants des méthodes pédagogiques employées par l'équipe de formation.<br><br></h5>"),
               selectizeInput(inputId = "var_pedagogie", label = "Colonne(s) contenant les scores concernant la qualité des méthodes pédagogiques employées:" , choices = "", multiple = T, width = 700, options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions à choix multiple (o) entre 1 à 7...")),
               actionButton("add_pedagogie", label = "Ajouter au tableau"),
               textOutput("confirm_pedagogie"))
    )),
  
    fluidRow(
      tabBox(title = "Niveau 1 (Réactions immédiates)",
        id = "tabset2", height = "330px", width=12,
        tabPanel("Intérêt perçu pour les ressources",
                 HTML("<h5>Le score reflète à quel point les ressources de formation sont perçues par les participants comme intéressantes à l'issue de la journée de formation.<br><br></h5>"),
                 selectizeInput(inputId = "var_interet", label = "Colonne(s) contenant les scores concernant l'intérêt pour la journée de formation:" , choices = "", multiple = T, width = 800, options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions à choix multiple (o) entre 1 à 7... ⚠ Une seule colonne par activité...")),
                 textInput("activities_interet", label = "Activités/Modules de formation réalisées:", placeholder = "Entrez les activités dans l'ordre des colonnes ci-dessus séparées par des virgules...",width = 800),
                 actionButton("add_interet", label = "Ajouter au tableau"),
                 textOutput("confirm_interet")),
        tabPanel("Utilité perçue des ressources",
                 HTML("<h5>Le score reflète à quel point les ressources de formation sont perçues par les participants comme utiles pour leur pratique enseignante à l'issue de la journée de formation.<br><br></h5>"),
                 selectizeInput(inputId = "var_utilite", label = "Colonne(s) contenant les scores concernant l'utilité pour la journée de formation:" , choices = "", multiple = T, width = 800, options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions à choix multiple (o) entre 1 à 7... ⚠ Une seule colonne par activité...")),
                 textInput("activities_utilite", label = "Activités/Modules de formation réalisées:", placeholder = "Entrez les activités dans l'ordre des colonnes ci-dessus séparées par des virgules...",width = 800),
                 actionButton("add_utilite", label = "Ajouter au tableau"),
                 textOutput("confirm_utilite")),
        tabPanel("Facilité perçue d'utilisation", 
                 HTML("<h5>Le score reflète à quel point les ressources de formation sont perçues par les participants comme faciles d'utilisation à l'issue de la journée de formation.<br><br></h5>"),
                 selectizeInput(inputId = "var_facilite", label = "Colonne(s) contenant les scores concernant la facilité d'utilisation pour la journée de formation:" , choices = "", multiple = T, width = 800,  options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions à choix multiple (o) entre 1 à 7... ⚠ Une seule colonne par activité...")),
                 textInput("activities_facilite", label = "Activités/Modules de formation réalisées:", placeholder = "Entrez les activités dans l'ordre des colonnes ci-dessus séparées par des virgules...",width = 800),
                 actionButton("add_facilite", label = "Ajouter au tableau"),
                 textOutput("confirm_facilite")),
        tabPanel("Auto-efficacité vis-à-vis des ressources", 
                 HTML("<h5>Le score reflète à quel point les participants se sentent capable d'utilisation les ressources de formation en classe à l'issue de la journée de formation.<br><br></h5>"),
                 selectizeInput(inputId = "var_autoefficacite", label = "Colonne(s) contenant les scores concernant la capacité d'utilisation des ressources pour la journée de formation:" , choices = "", multiple = T, width = 800,  options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions à choix multiple (o) entre 1 à 7... ⚠ Une seule colonne par activité...")),
                 textInput("activities_autoefficacite", label = "Activités/Modules de formation réalisées:", placeholder = "Entrez les activités dans l'ordre des colonnes ci-dessus séparées par des virgules...",width = 800),
                 actionButton("add_autoefficacite", label = "Ajouter au tableau"),
                 textOutput("confirm_autoefficacite")),
        tabPanel("Intention d'utiliser les ressources", 
                 HTML("<h5>Le score reflète à quel point les participants ont l'intention d'utiliser les ressources de formation en classe à l'issue de la journée de formation.<br><br></h5>"),
                 selectizeInput(inputId = "var_intention", label = "Colonne(s) contenant les scores concernant l'intention d'utiliser les ressources de la journée de formation:" , choices = "", multiple = T, width = 800,  options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions à choix multiple (o) entre 1 à 7... ⚠ Une seule colonne par activité...")),
                 textInput("activities_intention", label = "Activités/Modules de formation réalisées:", placeholder = "Entrez les activités dans l'ordre des colonnes ci-dessus séparées par des virgules...",width = 800),
                 actionButton("add_intention", label = "Ajouter au tableau"),
                 textOutput("confirm_intention"))
      )),
  
    fluidRow(
      tabBox(
        
        title = "Niveau 2 (Acquisition des connaissances et compétences numériques)",
        id = "tabset3", height = "430px", width=12,
        tabPanel("Acquisition des contenus théoriques", 
                 HTML("<h5>Le score reflète à quel point les participants pensent avoir acquis les contenus théoriques dispensés à l'issue de la journée de formation.<br><br></h5>"),
                 selectizeInput(inputId="var_notions", label="Colonne(s) contenant les scores concernant les notions abordées:", choices = "", multiple = T, width = 1000, options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions à choix multiple (o) entre 1 à 7...")),
                 actionButton("add_notions", label = "Ajouter au tableau"),
                 textOutput("confirm_notions")),
        tabPanel("Acquisition des compétences numériques (cf. RCnum)",
                 HTML("<h5>Le score reflète à quel point les participants pensent avoir acquis les compétences numériques (Référentiel de Compétences numériques) dispensées à l'issue de la journée de formation.<br><br></h5>"),
                 selectizeInput(inputId = "var_competences", label = "Colonne(s) contenant les scores concernant les compétences abordées pour la journée de formation:" , choices = "", multiple = T, width = 830, options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions à choix multiple (o) entre 1 à 7... ⚠ Une seule colonne par compétence...")),
                 textInput("competences", label = "Compétences abordées:", placeholder = "Entrez les compétences dans l'ordre des colonnes ci-dessus séparées par des virgules...", width = 830),
                 textInput(inputId = "var_rcnum", label = "Indiquez les références RCnum associées (p.ex. RCnum 1.0.1 ou RCnum non spécifié) séparées par des virgules:", width = 830, placeholder = "Entrez les références dans l'ordre des compétences ci-dessus séparées par des virgules..."),
                 actionButton("add_competences", label = "Ajouter au tableau"),
                 textOutput("confirm_competences"))
      )),
    
    fluidRow(
      tabBox(
        title = "Niveau 3 (Conditions facilitantes/entravantes et efficience perçue des ressources de formation)",
        id = "tabset4", height = "620px", width=12,
        tabPanel("Ressources matérielles",
                 HTML("<h5>Le score reflète le manque de ressources matérielles rapporté par les participants pour l'utilisation des ressources de formation.<br><br></h5>"),
                 selectizeInput(inputId="var_materiel", label="Colonne(s) contenant les scores pour l'évaluation des ressources matérielles:", choices = "", multiple = T, width = 800, options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions contenant des cases à cocher (□) ou choix multiple (o) entre 1 et 7...")),
                 actionButton("add_materiel", label = "Ajouter au tableau"),
                 textOutput("confirm_materiel")),
        tabPanel("Ressources temporelles",
                 HTML("<h5>Le score reflète le manque de ressources temporelles rapporté par les participants pour l'utilisation des ressources de formation.<br><br></h5>"),
                 selectizeInput(inputId="var_temps", label="Colonne(s) contenant les scores pour l'évaluation des ressources temporelles:", choices = "", multiple = T, width = 800, options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions contenant des cases à cocher (□) ou choix multiple (o) entre 1 et 7...")),
                 actionButton("add_temps", label = "Ajouter au tableau"),
                 textOutput("confirm_temps")),
        tabPanel("Soutien de la hiérarchie et des collègues", 
                 HTML("<h5>Le score reflète le manque de soutien de la hiérarchie ou des collègues rapporté par les participants pour l'utilisation des ressources de formation.<br><br></h5>"),
                 selectizeInput(inputId="var_soutien", label="Colonne(s) contenant les scores pour l'évaluation du soutien hiérarchique et/ou des collègues:", choices = "", multiple = T, width = 800, options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions contenant des cases à cocher (□) ou choix multiple (o) entre 1 et 7...")),
                 actionButton("add_soutien", label = "Ajouter au tableau"),
                 textOutput("confirm_soutien")),
        tabPanel("Soutien technique et pédagogique", 
                 HTML("<h5>Le score reflète le manque de soutien technique et/ou pédagogique rapporté par les participants pour l'utilisation des ressources de formation.<br><br></h5>"),
                 selectizeInput(inputId="var_soutien_technique", label="Colonne(s) contenant les scores pour l'évaluation du soutien technique et/ou pédagogique:", choices = "", multiple = T, width = 800, options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions contenant des cases à cocher (□) ou choix multiple (o) entre 1 et 7...")),
                 actionButton("add_soutien_technique", label = "Ajouter au tableau"),
                 textOutput("confirm_soutien_technique")),
        tabPanel("Compatibilité avec les pratiques habituelles",
                 HTML("<h5>Le score reflète le manque compatibilité entre les nouvelle ressources de formation et les pratiques habituelles des enseignant.e.s rapporté pour l'utilisation des ressources de formation.<br><br></h5>"),
                 selectizeInput(inputId="var_compatibilite", label="Colonne(s) contenant les scores pour l'évaluation de la compatibilité avec les pratiques habituelles:", choices = "", multiple = T, width = 800, options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions contenant des cases à cocher (□) ou choix multiple (o) entre 1 et 7...")),
                 actionButton("add_compatibilite", label = "Ajouter au tableau"),
                 textOutput("confirm_compatibilite")),
        tabPanel("Efficience perçue",
                 HTML("<h5>L’efficience perçue représente le rapport entre des bénéfices (<b>progrès perçu chez les élèves</b>, <b>augmentation de l’auto-efficacité perçue vis-à-vis des ressources de formation</b>) et des coûts (<b>effort de réflexion</b>, <b>charge de travail</b>) liés à l’utilisation des ressources de formation dans la classe. Elle est calculée sur la base du rapport entre le pourcentage d’enseignant.e.s ayant perçu ces bénéfices et le pourcentage d’enseignant.e.s ayant perçu ces coûts. Une efficience négative indique que les enseignant.e.s perçoivent en moyenne plus de coûts que de bénéfices dans la mise en place des activités en classe.<br><br> ⚠ Ajouter les données au tableau activité par activité ⚠ <br></h5>"),
                 tags$div(selectizeInput(inputId="var_progres", "Colonne contenant les scores pour l'évaluation du progrès perçu chez les élèves:", choices = "", multiple = T, width = 800, options = list(placeholder = "⚠ La colonne doit contenir les réponses de questions contenant des cases à cocher (□)")),  style="display:inline-block"),
                 tags$div(selectizeInput(inputId="var_auto-efficacite", label="Colonne contenant les scores pour l'évaluation de l'auto-efficacité perçue vis-à-vis des ressources:", choices = "", multiple = T, width = 800,  options = list(placeholder = "⚠ La colonne doit contenir les réponses de questions contenant des cases à cocher (□)")),style="display:inline-block"),
                 tags$div(selectizeInput(inputId="var_reflexion", label="Colonne contenant les scores pour l'évaluation de l'effort de réflexion nécessaire à la mise en oeuvre des ressources:", choices = "", multiple = T, width = 800,  options = list(placeholder = "⚠ La colonne doit contenir les réponses de questions contenant des cases à cocher (□)")),style="display:inline-block"),
                 tags$div(selectizeInput(inputId="var_travail", label="Colonne contenant les scores pour l'évaluation de la charge de travail nécessaire à la mise en oeuvre des ressources:", choices = "", multiple = T, width = 800,  options = list(placeholder = "⚠ La colonne doit contenir les réponses de questions contenant des cases à cocher (□)")),style="display:inline-block"),
                 textInput("activities_efficience", label = "Activité/Module de formation réalisée:", placeholder = "Entrez le nom de l'activité en question",width = 800),
                 tags$div(actionButton("add_efficience", label = "Ajouter au tableau"),style="display:block"),
                 textOutput("confirm_efficience"))
        
      )),
    
    fluidRow(
      tabBox(
        title = "Niveau 4 (Mise en oeuvre des ressources et transfert des compétences numériques)",
        id = "tabset5", height = "430px", width=12,
        tabPanel("Mise en oeuvre en classe d'activités liées aux ressources de formation", 
                 HTML("<h5>Le score reflète le pourcentage de participants ayant réalisé au moins une fois une activité à partir des ressources de formation proposées.<br><br></h5>"),
                 selectizeInput(inputId="var_utilisation", label="Colonne(s) contenant les scores pour l'évaluation de la mise en oeuvre des ressources de formation:", choices = "", multiple = T, width = 830,  options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions contenant des cases à cocher (□)... ⚠ Une seule colonne par activité...")),
                 textInput("activities_utilisation", label = "Activités/Modules de formation associés:", placeholder = "Entrez les activités dans l'ordre des colonnes ci-dessus séparées par des virgules...",width = 830),
                 actionButton("add_utilisation", label = "Ajouter au tableau"),
                 textOutput("confirm_utilisation")),
        tabPanel("Transfert en classe des compétences numériques (RCnum) associées aux ressources de formation",
                 HTML("<h5>Le score reflète à quel point les participants ont mis en oeuvre les compétences numériques associées aux ressources de formation proposées.<br><br></h5>"),
                 selectizeInput(inputId="var_transfert", label="Colonne(s) contenant les scores pour l'évaluation du transfert des compétences numériques:", choices = "", multiple = T, width = 1050,   options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions contenant des cases à cocher (□) ou choix multiple (o) entre 1 et 7...  ⚠ Une seule colonne par compétence...")),
                 textInput("competences_transfert", label = "Compétences abordées:", placeholder = "Entrez les compétences dans l'ordre des colonnes ci-dessus séparées par des virgules...", width = 830),
                 textInput(inputId = "var_rcnum_transfert", label = "Indiquez les références RCnum associées (p.ex. RCnum 1.0.1 ou RCnum non spécifié) séparées par des virgules:", width = 830, placeholder = "Entrez les références dans l'ordre des compétences ci-dessus séparées par des virgules..."),
                 actionButton("add_transfert", label = "Ajouter au tableau"),
                 textOutput("confirm_transfert"))
                 
      )),
    
    fluidRow(
      tabBox(
        title = "Variables élèves",
        id = "tabset6", height = "300px", width=12,
        tabPanel("Stéréotypes préalables envers l'éducation numérique", ""),
        tabPanel("Familiarité préalable envers la technologie", "")
      )),
    
    fluidRow(
      tabBox(
        title = "Niveau 5 (Résultats élèves)",
        id = "tabset7", height = "350px", width=12,
        tabPanel("Compétences disciplinaires (PERnum)",
                 HTML("<h5>Le score reflète à quel point les élèves ont mis en oeuvre les compétences numériques displinaires associées aux ressources de formation proposées.<br><br></h5>"),
                 selectizeInput(inputId="var_pernum", label="Colonne(s) contenant les scores pour l'évaluation des compétences numériques chez les élèves:", choices = "", multiple = T, width = 1050,   options = list(placeholder = "⚠ La ou les colonnes doivent contenir les réponses de questions contenant des cases à cocher (□) ou choix multiple (o) entre 1 et 7...  ⚠ Une seule colonne par compétence...")),
                 textInput("competences_pernum", label = "Compétences numériques (PERnum) associées:", placeholder = "Entrez les compétences dans l'ordre des colonnes ci-dessus séparées par des virgules...",width = 1050),
                 actionButton("add_pernum", label = "Ajouter au tableau"),
                 textOutput("confirm_pernum")),
        tabPanel("Compétences transversales (PER)", ""),
        tabPanel("Changements attitudinaux", ""),
      ))
    
    
    ),
    
    tabItem(tabName = "page2",

        DTOutput("file2"),
        
        actionButton("remove", label = "Supprimer les colonnes inutiles"),
        actionButton("download2", label = "Télécharger le fichier renommé"),
          
    ),
    
    tabItem(tabName = "page4",
            
            DTOutput(outputId = "file4"),
            
            tags$div(actionButton("remove2", label = "Supprimer les lignes sélectionnées"),style="display:inline-block")
    )
    
  )

))
  
server <- function(input, output, session) {
  
  shinyjs::hide(id = "download2")
  shinyjs::hide(id = "remove")
  shinyjs::hide(id = "remove2")
  shinyjs::hide(id = "validate")
  shinyjs::hide(id = "download3")
  
  
  ###############################################################
  #RENAMING
  ###############################################################
  
  getRawColnames <- function(data){ 
    #GetRawColnames permet de recréer des nom des colonnes sur une seule ligne à partir de l'export brute de SurveyMonkey 
    
    # Merge the two first lines into only one line with variables names
    
    firstletters <- substring(colnames(data), 1, 1)
    raw_colnames <- c()
    
    for (i in 1:length(firstletters)){
      
      if (firstletters[i] == "X") {
        raw_colnames[i-1] <- data[1,i-1]
        raw_colnames[i] <- data[1,i]
      }
      
      else {
        
        raw_colnames[i] <- colnames(data)[i]
        
      }
      
    }
    
    # Remove dots
    raw_colnames <- gsub("\\."," ",raw_colnames)
    colnames(data) <- gsub("\\."," ",colnames(data))
    
    for (i in 1:length(firstletters)){
      
      if (firstletters[i] != "X") {
        
        if (raw_colnames[i] != colnames(data)[i]){
          
          raw_colnames[i] <- paste(colnames(data)[i], raw_colnames[i], sep = ": ")
          
        }
        
      }
      
      else {
        
        raw_colnames[i] <- paste(colnames(data)[i-1], raw_colnames[i], sep = ": ")
        
        if (i < length(firstletters)) {
          
          colnames(data)[i] <- colnames(data)[i-1] 
        }
        
      }
      
    }
    
    return(raw_colnames)
  
  }
  

  rvs2 <- reactiveValues(
    data = NULL,
    columns = NULL
  )
  
  
  file2 <- reactive({
    
    inFile2 <- input$file2
    
    if (is.null(inFile2))
      return(NULL)
    
    read.csv(inFile2$datapath)
    
  })
    
  codebook <- reactive({
    
    inFile2 <- input$file2
    
    if (is.null(inFile2))
      return(NULL)
    
    data <- read.csv(inFile2$datapath)
    
    data.frame("Colonnes brutes" = getRawColnames(data), "Colonnes renommées" = c(""), check.names = F)
    
  })
  
  output$file2 <- renderDT({

    rvs2$data
    
    }, editable = TRUE, extensions="Buttons", options = list(scrollY = TRUE,columnDefs = list(list(width = '500px', targets = c(1))), pageLength = 200, dom = 'Bfrtip',buttons = list('copy', 'print', list(
          extend = 'collection',
          buttons = list(
            list(extend = 'csv', filename = "codebook"),
            list(extend = 'excel', filename = "codebook"),
            list(extend = 'pdf', filename = "codebook")),
          text = 'Télécharger le codebook'
        ))
    ))
  
  
  observe({
    rvs2$data <- codebook()
  })
  
  
  observeEvent(input$file2, {
  
      shinyjs::show(id = "download2")
      shinyjs::show(id = "remove")
    
    })
  
  
  observeEvent(input$remove,{
    
    if (!is.null(input$file2_rows_selected)) {
      
      rvs2$columns <- as.numeric(input$file2_rows_selected)
      rvs2$data <- rvs2$data[-as.numeric(input$file2_rows_selected),]
    }
  })

  observeEvent(input$file2_cell_edit, {
    rvs2$data <- editData(rvs2$data, input$file2_cell_edit)
  })
  
  
  observeEvent(input$download2,{
    
    data.ready <- file2()
    data.ready <- data.ready[-rvs2$columns]
    data.ready = data.ready[-1,]
    colnames(data.ready) <- rvs2$data[,2]

    write.csv2(data.ready, file = "renamed.csv",row.names=FALSE)
    
  })
  

  ###############################################################
  # DASHBOARD
  ###############################################################
  
  file1 <- reactive({
    
    inFile1 <- input$file1
    
    if (is.null(inFile1))
      return(NULL)
    
    read.csv(inFile1$datapath,check.names = F,sep = ";")
    
  })
  
  # Motivation préalable
  
  observe({
    updateSelectizeInput(session,'var_motivation',
                         choices=colnames(file1()))
  })
  
  observeEvent(input$add_motivation,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    for (i in 1:length(input[["var_motivation"]])) {
      
      scores <- append(scores, mean(file1()[,input[["var_motivation"]][i]], na.rm=T))
      
    }
    
    score <- round(mean(scores) / 7 * 100)
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    ttb <- new_entry(ttb,
                     date,
                     input[["var1"]],
                     input[["var3"]],
                     input[["var2"]],
                     input[["var4"]],
                     input[["var5"]],
                     nombre_participants,
                     "Participants",
                     "Motivation préalable",
                     "Général",
                     "Non concerné",
                     "Non concerné",
                     score,
                     input[["url"]])
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_motivation <- renderText({"Ajouté!"})
    
  })
  
  # Attitude préalable
  
  observe({
    updateSelectizeInput(session,'var_attitude',
                         choices=colnames(file1()))
  })
  
  observeEvent(input$add_attitude,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    for (i in 1:length(input[["var_attitude"]])) {
      
      scores <- append(scores, mean(file1()[,input[["var_attitude"]][i]], na.rm=T))
      
    }
    
    score <- round(mean(scores) / 7 * 100)
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    ttb <- new_entry(ttb,
                     date,
                     input[["var1"]],
                     input[["var3"]],
                     input[["var2"]],
                     input[["var4"]],
                     input[["var5"]],
                     nombre_participants,
                     "Participants",
                     "Attitude envers l'éducation numérique",
                     "Général",
                     "Non concerné",
                     "Non concerné",
                     score,
                     input[["url"]])
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_attitude <- renderText({" Ajouté!"})
    
  })
  
  # Implication de l'équipe de formation
  
  observe({
    updateSelectizeInput(session,'var_implication',
                         choices=colnames(file1()))
  })
  
  observeEvent(input$add_implication,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    for (i in 1:length(input[["var_implication"]])) {
      
      scores <- append(scores, mean(file1()[,input[["var_implication"]][i]], na.rm=T))
      
    }
    
    score <- round(mean(scores) / 7 * 100)
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    ttb <- new_entry(ttb,
                     date,
                     input[["var1"]],
                     input[["var3"]],
                     input[["var2"]],
                     input[["var4"]],
                     input[["var5"]],
                     nombre_participants,
                     "Formateurs",
                     "Perception de l'implication de l'équipe de formation",
                     "Général",
                     "Non concerné",
                     "Non concerné",
                     score,
                     input[["url"]])
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_implication <- renderText({" Ajouté!"})
    
  })
  
  # Qualité des méthodes pédagogiques employées
  
  observe({
    updateSelectizeInput(session,'var_pedagogie',
                         choices=colnames(file1()))
  })
  
  observeEvent(input$add_pedagogie,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    for (i in 1:length(input[["var_pedagogie"]])) {
      
      scores <- append(scores, mean(file1()[,input[["var_pedagogie"]][i]], na.rm=T))
      
    }
    
    score <- round(mean(scores) / 7 * 100)
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    ttb <- new_entry(ttb,
                     date,
                     input[["var1"]],
                     input[["var3"]],
                     input[["var2"]],
                     input[["var4"]],
                     input[["var5"]],
                     nombre_participants,
                     "Formateurs",
                     "Qualité perçue des méthodes pédagogiques",
                     "Général",
                     "Non concerné",
                     "Non concerné",
                     score,
                     input[["url"]])
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_pedagogie <- renderText({" Ajouté!"})
    
  })
  
  # Intérêt perçu pour les ressources
  
  observe({
    updateSelectizeInput(session,'var_interet',
                         choices=colnames(file1()))
  })

  
  observeEvent(input$add_interet,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    for (i in 1:length(input[["var_interet"]])) {
      
      score <- round(mean(file1()[,input[["var_interet"]][i]], na.rm=T) / 7 * 100)
    
      activities <- strsplit(input[["activities_interet"]], ",")
    
      ttb <- new_entry(ttb,
                       date,
                       input[["var1"]],
                       input[["var3"]],
                       input[["var2"]],
                       input[["var4"]],
                       input[["var5"]],
                       nombre_participants,
                       "1",
                       "Intérêt perçu pour les ressources",
                       activities[[1]][i],
                       "Non concerné",
                       "Non concerné",
                       score,
                       input[["url"]])
    }
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_interet <- renderText({" Ajouté!"})
    output$activities_formation <- renderUI({
      lapply(activities[[1]], wordButton)
    })
    
  })
  
  
  # Utlité perçue pour les ressources
  
  observe({
    updateSelectizeInput(session,'var_utilite',
                         choices=colnames(file1()))
  })
  
  
  observeEvent(input$add_utilite,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    for (i in 1:length(input[["var_utilite"]])) {
      
      score <- round(mean(file1()[,input[["var_utilite"]][i]], na.rm=T) / 7 * 100)
      
      activities <- strsplit(input[["activities_utilite"]], ",")
      
      ttb <- new_entry(ttb,
                       date,
                       input[["var1"]],
                       input[["var3"]],
                       input[["var2"]],
                       input[["var4"]],
                       input[["var5"]],
                       nombre_participants,
                       "1",
                       "Utilité perçue des ressources",
                       activities[[1]][i],
                       "Non concerné",
                       "Non concerné",
                       score,
                       input[["url"]])
    }
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_utilite <- renderText({" Ajouté!"})
    
  })
  
  # Facilité perçue d'utilisation des ressources
  
  observe({
    updateSelectizeInput(session,'var_facilite',
                         choices=colnames(file1()))
  })
  
  
  observeEvent(input$add_facilite,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    for (i in 1:length(input[["var_facilite"]])) {
      
      score <- round(mean(file1()[,input[["var_facilite"]][i]], na.rm=T) / 7 * 100)
      
      activities <- strsplit(input[["activities_facilite"]], ",")
      
      ttb <- new_entry(ttb,
                       date,
                       input[["var1"]],
                       input[["var3"]],
                       input[["var2"]],
                       input[["var4"]],
                       input[["var5"]],
                       nombre_participants,
                       "1",
                       "Facilité perçue d’utilisation des ressources",
                       activities[[1]][i],
                       "Non concerné",
                       "Non concerné",
                       score,
                       input[["url"]])
    }
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_facilite <- renderText({" Ajouté!"})
    
  })
  
  # Sentiment d'auto-efficacité dans la réalisation de l'activité
  
  observe({
    updateSelectizeInput(session,'var_autoefficacite',
                         choices=colnames(file1()))
  })
  
  
  observeEvent(input$add_autoefficacite,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    for (i in 1:length(input[["var_autoefficacite"]])) {
      
      score <- round(mean(file1()[,input[["var_autoefficacite"]][i]], na.rm=T) / 7 * 100)
      
      activities <- strsplit(input[["activities_autoefficacite"]], ",")
      
      ttb <- new_entry(ttb,
                       date,
                       input[["var1"]],
                       input[["var3"]],
                       input[["var2"]],
                       input[["var4"]],
                       input[["var5"]],
                       nombre_participants,
                       "1",
                       "Sentiment d'auto-efficacité dans la réalisation de l'activité",
                       activities[[1]][i],
                       "Non concerné",
                       "Non concerné",
                       score,
                       input[["url"]])
    }
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_autoefficacite <- renderText({" Ajouté!"})
    
  })
  
  # Intention d’utiliser les ressources
  
  observe({
    updateSelectizeInput(session,'var_intention',
                         choices=colnames(file1()))
  })
  
  
  observeEvent(input$add_intention,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    for (i in 1:length(input[["var_intention"]])) {
      
      score <- round(mean(file1()[,input[["var_intention"]][i]], na.rm=T) / 7 * 100)
      
      activities <- strsplit(input[["activities_intention"]], ",")
      
      ttb <- new_entry(ttb,
                       date,
                       input[["var1"]],
                       input[["var3"]],
                       input[["var2"]],
                       input[["var4"]],
                       input[["var5"]],
                       nombre_participants,
                       "1",
                       "Intention d’utiliser les ressources",
                       activities[[1]][i],
                       "Non concerné",
                       "Non concerné",
                       score,
                       input[["url"]])
    }
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_intention <- renderText({" Ajouté!"})
    
  })
  
  # Connaissance du contenu théorique
  
  observe({
    updateSelectizeInput(session,'var_notions',
                         choices=colnames(file1()))
  })
  
  observeEvent(input$add_notions,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    for (i in 1:length(input[["var_notions"]])) {
      
      scores <- append(scores, mean(file1()[,input[["var_notions"]][i]], na.rm=T))
      
    }
    
    score <- round(mean(scores) / 7 * 100)
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    ttb <- new_entry(ttb,
                     date,
                     input[["var1"]],
                     input[["var3"]],
                     input[["var2"]],
                     input[["var4"]],
                     input[["var5"]],
                     nombre_participants,
                     "2",
                     "Connaissance du contenu théorique",
                     "Général",
                     "Non concerné",
                     "Non concerné",
                     score,
                     input[["url"]])
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_notions <- renderText({" Ajouté!"})
    
  })
  
  
  # Acquisition des compétences numériques
  
  observe({
    updateSelectizeInput(session,'var_competences',
                         choices=colnames(file1()))
  })
  
  
  observeEvent(input$add_competences,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    for (i in 1:length(input[["var_competences"]])) {
      
      score <- round(mean(file1()[,input[["var_competences"]][i]], na.rm=T) / 7 * 100)
      
      competences <- strsplit(input[["competences"]], ",")
      
      reference <- strsplit(input[["var_rcnum"]], ",")
      
      ttb <- new_entry(ttb,
                       date,
                       input[["var1"]],
                       input[["var3"]],
                       input[["var2"]],
                       input[["var4"]],
                       input[["var5"]],
                       nombre_participants,
                       "2",
                       "Acquisition des compétences numériques",
                       competences[[1]][i],
                       reference[[1]][i],
                       "Non concerné",
                       score,
                       input[["url"]])
    }
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_competences <- renderText({" Ajouté!"})
    
  })
  
  # Ressources matérielles
  
  observe({
    updateSelectizeInput(session,'var_materiel',
                         choices=colnames(file1()))
  })
  
  
  observeEvent(input$add_materiel,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
   if (is.numeric(file1()[,input[["var_materiel"]][1]])) {
     
     scores <- c()
     
     for (i in 1:length(input[["var_materiel"]])) {
       
       scores <- append(scores, mean(file1()[,input[["var_materiel"]][i]], na.rm=T))
       
     }
     
     score <- round(mean(scores) / 7 * 100)
     
     ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
     
     ttb <- new_entry(ttb,
                      date,
                      input[["var1"]],
                      input[["var3"]],
                      input[["var2"]],
                      input[["var4"]],
                      input[["var5"]],
                      nombre_participants,
                      "3",
                      "Ressources matérielles",
                      "Général",
                      "Non concerné",
                      "Non concerné",
                      score,
                      input[["url"]])
   }
    
   else {
     
     scores <- c()
     
     for (i in 1:length(input[["var_materiel"]])) {
       
       
       scores <- append(scores, length(which(complete.cases(file1()[,input[["var_materiel"]][i]]))))
       
     }
       
     score <- round(mean(scores) / nrow(file1()) * 100)

     ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
     ttb <- new_entry(ttb,
                     date,
                     input[["var1"]],
                     input[["var3"]],
                     input[["var2"]],
                     input[["var4"]],
                     input[["var5"]],
                     nombre_participants,
                     "3",
                     "Ressources matérielles",
                     "Général",
                     "Non concerné",
                     "Non concerné",
                     score,
                     input[["url"]])  
  
   }
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_materiel <- renderText({" Ajouté!"})
    
  })
  
  
  # Ressources temporelles
  
  observe({
    updateSelectizeInput(session,'var_temps',
                         choices=colnames(file1()))
  })
  
  
  observeEvent(input$add_temps,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    if (is.numeric(file1()[,input[["var_temps"]][1]])) {
      
      scores <- c()
      
      for (i in 1:length(input[["var_temps"]])) {
        
        scores <- append(scores, mean(file1()[,input[["var_temps"]][i]], na.rm=T))
        
      }
      
      score <- round(mean(scores) / 7 * 100)
      
      ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
      
      ttb <- new_entry(ttb,
                       date,
                       input[["var1"]],
                       input[["var3"]],
                       input[["var2"]],
                       input[["var4"]],
                       input[["var5"]],
                       nombre_participants,
                       "3",
                       "Ressources temporelles",
                       "Général",
                       "Non concerné",
                       "Non concerné",
                       score,
                       input[["url"]])
    }
    
    else {
      
      scores <- c()
      
      for (i in 1:length(input[["var_temps"]])) {
        
        
        scores <- append(scores, length(which(complete.cases(file1()[,input[["var_temps"]][i]]))))
        
      }
      
      score <- round(mean(scores) / nrow(file1()) * 100)
      
      ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
      
      ttb <- new_entry(ttb,
                       date,
                       input[["var1"]],
                       input[["var3"]],
                       input[["var2"]],
                       input[["var4"]],
                       input[["var5"]],
                       nombre_participants,
                       "3",
                       "Ressources temporelles",
                       "Général",
                       "Non concerné",
                       "Non concerné",
                       score,
                       input[["url"]])  
      
    }
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_temps <- renderText({" Ajouté!"})
    
  })
  
  
  # Soutien hiérarchique ou des collègues
  
  observe({
    updateSelectizeInput(session,'var_soutien',
                         choices=colnames(file1()))
  })
  
  
  observeEvent(input$add_soutien,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    if (is.numeric(file1()[,input[["var_soutien"]][1]])) {
      
      scores <- c()
      
      for (i in 1:length(input[["var_soutien"]])) {
        
        scores <- append(scores, mean(file1()[,input[["var_soutien"]][i]], na.rm=T))
        
      }
      
      score <- round(mean(scores) / 7 * 100)
      
      ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
      
      ttb <- new_entry(ttb,
                       date,
                       input[["var1"]],
                       input[["var3"]],
                       input[["var2"]],
                       input[["var4"]],
                       input[["var5"]],
                       nombre_participants,
                       "3",
                       "Soutien hiérarchique ou des collègues",
                       "Général",
                       "Non concerné",
                       "Non concerné",
                       score,
                       input[["url"]])
    }
    
    else {
      
      scores <- c()
      
      for (i in 1:length(input[["var_soutien"]])) {
        
        
        scores <- append(scores, length(which(complete.cases(file1()[,input[["var_soutien"]][i]]))))
        
      }
      
      score <- round(mean(scores) / nrow(file1()) * 100)
      
      ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
      
      ttb <- new_entry(ttb,
                       date,
                       input[["var1"]],
                       input[["var3"]],
                       input[["var2"]],
                       input[["var4"]],
                       input[["var5"]],
                       nombre_participants,
                       "3",
                       "Soutien hiérarchique ou des collègues",
                       "Général",
                       "Non concerné",
                       "Non concerné",
                       score,
                       input[["url"]])  
      
    }
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_soutien <- renderText({" Ajouté!"})
    
  })
  
  # Soutien technique et/ou pédagogique
  
  observe({
    updateSelectizeInput(session,'var_soutien_technique',
                         choices=colnames(file1()))
  })
  
  
  observeEvent(input$add_soutien_technique,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    if (is.numeric(file1()[,input[["var_soutien_technique"]][1]])) {
      
      scores <- c()
      
      for (i in 1:length(input[["var_soutien_technique"]])) {
        
        scores <- append(scores, mean(file1()[,input[["var_soutien_technique"]][i]], na.rm=T))
        
      }
      
      score <- round(mean(scores) / 7 * 100)
      
      ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
      
      ttb <- new_entry(ttb,
                       date,
                       input[["var1"]],
                       input[["var3"]],
                       input[["var2"]],
                       input[["var4"]],
                       input[["var5"]],
                       nombre_participants,
                       "3",
                       "Soutien technique et/ou pédagogique",
                       "Général",
                       "Non concerné",
                       "Non concerné",
                       score,
                       input[["url"]])
    }
    
    else {
      
      scores <- c()
      
      for (i in 1:length(input[["var_soutien_technique"]])) {
        
        
        scores <- append(scores, length(which(complete.cases(file1()[,input[["var_soutien_technique"]][i]]))))
        
      }
      
      score <- round(mean(scores) / nrow(file1()) * 100)
      
      ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
      
      ttb <- new_entry(ttb,
                       date,
                       input[["var1"]],
                       input[["var3"]],
                       input[["var2"]],
                       input[["var4"]],
                       input[["var5"]],
                       nombre_participants,
                       "3",
                       "Soutien technique et/ou pédagogique",
                       "Général",
                       "Non concerné",
                       "Non concerné",
                       score,
                       input[["url"]])  
      
    }
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_soutien_technique <- renderText({" Ajouté!"})
    
  })
  
  
  # Compatibilité avec mes pratiques habituelles d'enseignement
  
  observe({
    updateSelectizeInput(session,'var_compatibilite',
                         choices=colnames(file1()))
  })
  
  
  observeEvent(input$add_compatibilite,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    if (is.numeric(file1()[,input[["var_compatibilite"]][1]])) {
      
      scores <- c()
      
      for (i in 1:length(input[["var_compatibilite"]])) {
        
        scores <- append(scores, mean(file1()[,input[["var_compatibilite"]][i]], na.rm=T))
        
      }
      
      score <- round(mean(scores) / 7 * 100)
      
      ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
      
      ttb <- new_entry(ttb,
                       date,
                       input[["var1"]],
                       input[["var3"]],
                       input[["var2"]],
                       input[["var4"]],
                       input[["var5"]],
                       nombre_participants,
                       "3",
                       "Compatibilité avec les pratiques habituelles d'enseignement",
                       "Général",
                       "Non concerné",
                       "Non concerné",
                       score,
                       input[["url"]])
    }
    
    else {
      
      scores <- c()
      
      for (i in 1:length(input[["var_compatibilite"]])) {
        
        
        scores <- append(scores, length(which(complete.cases(file1()[,input[["var_compatibilite"]][i]]))))
        
      }
      
      score <- round(mean(scores) / nrow(file1()) * 100)
      
      ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
      
      ttb <- new_entry(ttb,
                       date,
                       input[["var1"]],
                       input[["var3"]],
                       input[["var2"]],
                       input[["var4"]],
                       input[["var5"]],
                       nombre_participants,
                       "3",
                       "Compatibilité avec les pratiques habituelles d'enseignement",
                       "Général",
                       "Non concerné",
                       "Non concerné",
                       score,
                       input[["url"]])  
      
    }
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_compatibilite <- renderText({" Ajouté!"})
    
  })
  
  
 # Efficience globale
  
  observe({
    
    updateSelectizeInput(session,'var_progres',
                         choices=colnames(file1()))
    updateSelectizeInput(session,'var_auto-efficacite',
                         choices=colnames(file1()))
    updateSelectizeInput(session,'var_reflexion',
                         choices=colnames(file1()))
    updateSelectizeInput(session,'var_travail',
                         choices=colnames(file1()))
  })
  
  
  observeEvent(input$add_efficience,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
  
      scores_progres <- length(which(file1()[,input[["var_progres"]]]==3))
      scores_autoefficacite <- length(which(file1()[,input[["var_auto-efficacite"]]]==4))

      scores_reflexion <- length(which(file1()[,input[["var_reflexion"]]]==1))
      scores_travail <- length(which(file1()[,input[["var_travail"]]]==2))
      

      total_participants <- sum(c(scores_progres,scores_autoefficacite,scores_reflexion,scores_travail))
                                
      scores_progres_pourcentage <-  (scores_progres/total_participants)*100
      scores_autoefficacite_pourcentage <- (scores_autoefficacite/total_participants)*100
      scores_reflexion_pourcentage <- (scores_reflexion/total_participants)*100
      scores_travail_pourcentage <- (scores_travail/total_participants)*100
      
      score <- round(mean(c(scores_progres_pourcentage,scores_autoefficacite_pourcentage)) - mean(c(scores_reflexion_pourcentage,scores_travail_pourcentage)))
      
      ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
      
      ttb <- new_entry(ttb,
                       date,
                       input[["var1"]],
                       input[["var3"]],
                       input[["var2"]],
                       input[["var4"]],
                       input[["var5"]],
                       nombre_participants,
                       "3",
                       "Efficience globale",
                       input[["activities_efficience"]],
                       "Non concerné",
                       "Non concerné",
                       score,
                       input[["url"]])
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_efficience <- renderText({
      "Ajouté!"})
  })
  
  # Mise en oeuvre d'activités
  
  observe({
    updateSelectizeInput(session,'var_utilisation',
                         choices=colnames(file1()))
  })
  
  
  observeEvent(input$add_utilisation,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    for (i in 1:length(input[["var_utilisation"]])) {
      
      score <- round(length(which(complete.cases(file1()[,input[["var_utilisation"]][i]]))) / nrow(file1()) * 100)
      
      activities <- strsplit(input[["activities_utilisation"]], ",")
      
      ttb <- new_entry(ttb,
                       date,
                       input[["var1"]],
                       input[["var3"]],
                       input[["var2"]],
                       input[["var4"]],
                       input[["var5"]],
                       nombre_participants,
                       "4",
                       "Mise en oeuvre d'activités",
                       activities[[1]][i],
                       "Non concerné",
                       "Non concerné",
                       score,
                       input[["url"]])
    }
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_utilisation <- renderText({" Ajouté!"})
    
    })
    
  
  
  # Transfert des compétences numériques
  
  observe({
    updateSelectizeInput(session,'var_transfert',
                         choices=colnames(file1()))
  })
  
  
  observeEvent(input$add_transfert,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    
    if (is.numeric(file1()[,input[["var_transfert"]][1]])) {

      
      for (i in 1:length(input[["var_transfert"]])) {
        
        score <- round(mean(file1()[,input[["var_transfert"]][i]], na.rm=T) / 7 * 100)
        
        competences <- strsplit(input[["competences_transfert"]], ",")
        
        reference <- strsplit(input[["var_rcnum_transfert"]], ",")
        
        ttb <- new_entry(ttb,
                         date,
                         input[["var1"]],
                         input[["var3"]],
                         input[["var2"]],
                         input[["var4"]],
                         input[["var5"]],
                         nombre_participants,
                         "4",
                         "Transfert des compétences numériques",
                         competences[[1]][i],
                         reference[[1]][i],
                         "Non concerné",
                         score,
                         input[["url"]])
      }
      
    }
    
    else {
      
      
      for (i in 1:length(input[["var_transfert"]])) {
        
        score <- round(length(which(complete.cases(file1()[,input[["var_transfert"]][i]]))) / nrow(file1()) * 100)
        
        competences <- strsplit(input[["competences_transfert"]], ",")
        
        reference <- strsplit(input[["var_rcnum_transfert"]], ",")
        
        ttb <- new_entry(ttb,
                         date,
                         input[["var1"]],
                         input[["var3"]],
                         input[["var2"]],
                         input[["var4"]],
                         input[["var5"]],
                         nombre_participants,
                         "4",
                         "Transfert des compétences numériques",
                         competences[[1]][i],
                         reference[[1]][i],
                         "Non concerné",
                         score,
                         input[["url"]])
      }
      
    }
      
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_transfert <- renderText({" Ajouté!"})
    
  })
  
  
  # Compétences numériques chez les élèves
  
  
  observe({
    updateSelectizeInput(session,'var_pernum',
                         choices=colnames(file1()))
  })
  
  
  observeEvent(input$add_pernum,{
    
    date <- paste0(substr(file1()$date[1], 4, 5),"/",
                   substr(file1()$date[1], 1, 2),
                   substr(file1()$date[1], 6, 10))
    nombre_participants <- nrow(file1())
    
    scores <- c()
    
    ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
    
    
    if (is.numeric(file1()[,input[["var_pernum"]][1]])) {
      
      
      for (i in 1:length(input[["var_pernum"]])) {
        
        score <- round(mean(file1()[,input[["var_pernum"]][i]], na.rm=T) / 7 * 100)
        
        competences <- strsplit(input[["competences_pernum"]], ",")
        
        ttb <- new_entry(ttb,
                         date,
                         input[["var1"]],
                         input[["var3"]],
                         input[["var2"]],
                         input[["var4"]],
                         input[["var5"]],
                         nombre_participants,
                         "5",
                         "Compétences disciplinaires élèves",
                         "Général",
                         "Non concerné",
                         competences[[1]][i],
                         score,
                         input[["url"]])
      }
      
    }
    
    else {
      
      
      for (i in 1:length(input[["var_pernum"]])) {
        
        score <- round(length(which(complete.cases(file1()[,input[["var_pernum"]][i]]))) / nrow(file1()) * 100)
        
        competences <- strsplit(input[["competences_pernum"]], ",")
        
        ttb <- new_entry(ttb,
                         date,
                         input[["var1"]],
                         input[["var3"]],
                         input[["var2"]],
                         input[["var4"]],
                         input[["var5"]],
                         nombre_participants,
                         "5",
                         "Compétences disciplinaires élèves",
                         "Non concerné",
                         "Général",
                         competences[[1]][i],
                         score,
                         input[["url"]])
      }
      
    }
    
    write.csv2(ttb, "data_evaluation.csv",row.names = FALSE)
    output$confirm_pernum <- renderText({" Ajouté!"})
    
  })
  
  
  
  ###############################################################
  # VISUALIZING DATA
  ###############################################################
  
  ttb <- read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";")
  
  rvs4 <- reactiveValues(
    data = NULL,
    columns = NULL
  )
  
  
  output$file4 <- renderDT({
    
    rvs4$data
    
  }, editable = TRUE)
  
  
  observe({
    
    if(input$sidebarid=="page4") {
      
      rvs4$data <- tail(read.csv("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE, sep = ";"), 10)
      
    }
    
  })
   
  observeEvent(rvs4$data,{
    
    shinyjs::show(id = "remove2")
    
  })
  
  
  observeEvent(input$remove2,{
    
    if (!is.null(input$file4_rows_selected)) {
      
      rvs4$columns <- as.numeric(input$file4_rows_selected)
      rvs4$data <- rvs4$data[-as.numeric(input$file4_rows_selected),]
      data_evaluation <- rbind(data_evaluation %>% filter(row_number() <= n()-10),rvs4$data)
      write.csv2(data_evaluation, "data_evaluation.csv",row.names = FALSE)
      
    }
  })
  
  
  # Contact
  
  observeEvent(input$openModal, {
    showModal(
      modalDialog(title = "Contact",
                  footer = modalButton("Fermer"),
                  p("Ce tableau de bord a été créé par l'équipe de recherche EduNum-EPFL. Pour tout renseignement : sunny.avry@epfl.ch"))
    )
  })
  
}

shinyApp(ui = ui, server = server)
