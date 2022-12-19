library(shiny)
library(shinydashboard)
library(dplyr)
library(stringr)
library(DT)
library(shinyjs)
library(tidyr)
library(shinythemes)
library(shinyBS)
library(httr)
library(curl)
library(RColorBrewer)
library(scales) 
library(ggplot2) 
library(scales)
library(ggplot2)
    
#######################  FIND SURVEYS AND COLLECTORS IDS ################

# # Get the list of surveys to find the id of the wanted survey
# 
# surveys_details <- "https://api.surveymonkey.com/v3/surveys/"
# n_surveys <- content(GET(surveys_details,add_headers(Authorization = paste0("Bearer ", sm_api_key))),"parsed")$total
# 
# n_pages_surveys <- n_surveys %/% 50 + ifelse(n_surveys %% 50 > 0, 1,0)
# 
# connectApiUrl <- c()
# for (i in 1:n_pages_surveys) {
# 
#   connectApiUrl <- c(connectApiUrl,paste0("https://api.surveymonkey.com/v3/surveys?page=",i))
# 
# }
# 
# content_urls <- list()
# for (i in 1:length(connectApiUrl)) {
# 
#   content_urls <- append(content_urls,list(content(GET(connectApiUrl[i],add_headers(Authorization = paste0("Bearer ", sm_api_key))),"parsed")))
# 
# }
# 
# surveys <- data.frame()
# for (i in 1:length(content_urls)) {
# 
#   surveys <- rbind(surveys, data.frame(title=unlist(lapply(1:length(content_urls[[i]]$data), function(x) {content_urls[[i]]$data[[x]]$title})),url=unlist(lapply(1:length(content_urls[[i]]$data), function(x) {content_urls[[i]]$data[[x]]$href}))))
# 
# }
# 
# View(surveys)
# 
# # Get the collectors for a given survey
#
# collectors <-paste0("https://api.surveymonkey.com/v3/surveys/",survey_id,"/collectors")
# content_collectors <- content(GET(collectors,add_headers(Authorization = paste0("Bearer ", sm_api_key))),"parsed")

# Get the survey details

sm_api_key = 'cOmUpOXfevWx560egh1njr2VdHlQBpAay26.xBdsadaswakhEoKWck.rCm9ilv0T9WvWbFCC28WiiH5Xm6Wi-SsjnPELjqqyZ6WoeEOAg7nLOwE4EWe'
sm_secret = '24216551693195515dsdsa6525599903801'
sm_client_id = '_Sb_Cqdsadsad7aKt77vQ'

survey_id <- "505454359"

details <- paste0("https://api.surveymonkey.com/v3/surveys/",survey_id)
content_details <- content(GET(details,add_headers(Authorization = paste0("Bearer ", sm_api_key))),"parsed")

collector <- "447634543539"
collector <- paste0("https://api.surveymonkey.com/v3/collectors/",collector)

# Get the stored data from drive

if (file.exists("data_ef_d1.rds")){
  
  data_participants <- readRDS(file = "data_ef_d1.rds")
  
} else {
  
  data_participants <- list()
  
}

# Get the responses

if (content_details$response_count > length(data_participants)) {
  
  data_participants <- head(data_participants,-(length(data_participants) %% 50))
  
  day <- str_sub(content(GET(collector,add_headers(Authorization = paste0("Bearer ", sm_api_key))),"parsed")$url, -2, -1)
  
  pages <- (length(data_participants) %/% 50 + 1):(content_details$response_count %/% 50 + 1)
  
  responses_urls <- lapply(pages, function(x) {paste0("https://api.surveymonkey.com/v3/surveys/",survey_id,"/responses/bulk?simple=true&per_page=50&page=",x)})
  
  content_responses <- list()
  
  for (i in 1:length(responses_urls)) {
    
    content_responses <- append(content_responses,list(content(GET(responses_urls[[i]],add_headers(Authorization = paste0("Bearer ", sm_api_key))),"parsed")))
  }
  
  # Get the list of data of interest
  
  data <- list()
  
  for (n_batch in 1:length(content_responses)) {
    
    for (n_participant in 1:length(content_responses[[n_batch]]$data)) {
      
      participant <- c()
      
      participant <- append(participant,paste0("h id_binome"))
      participant <- append(participant,paste0("v ",content_responses[[n_batch]]$data[[n_participant]]$custom_variables$EF))
      participant <- append(participant,paste0("h date"))
      participant <- append(participant,paste0("v ",content_responses[[n_batch]]$data[[n_participant]]$date_created))
      
      for (n_page in 1:length(content_responses[[n_batch]]$data[[n_participant]]$pages)) {
        
        total_questions_page <- length(content_responses[[n_batch]]$data[[n_participant]]$pages[[n_page]]$questions)
        
        if (total_questions_page > 0) {
          
          for (n_question in 1:total_questions_page) {
            
            heading <<- gsub("<.*?>","",content_responses[[n_batch]]$data[[n_participant]]$pages[[n_page]]$questions[[n_question]]$heading)
            type <<- paste0(content_responses[[n_batch]]$data[[n_participant]]$pages[[n_page]]$questions[[n_question]]$family," ",content_responses[[n_batch]]$data[[n_participant]]$pages[[n_page]]$questions[[n_question]]$subtype)
            
            participant <- append(participant,paste0("t ",type))
            participant <- append(participant,paste0("h ",heading))
            
            total_subquestions_question <- length(content_responses[[n_batch]]$data[[n_participant]]$pages[[n_page]]$questions[[n_question]]$answers)
            
            for (n_subquestion in 1:total_subquestions_question) {
              
              subtitle <<- gsub("<.*?>","",gsub(" \\|.*","",content_responses[[n_batch]]$data[[n_participant]]$pages[[n_page]]$questions[[n_question]]$answers[[n_subquestion]]$simple_text))
              value <<- gsub(".*\\| ","",content_responses[[n_batch]]$data[[n_participant]]$pages[[n_page]]$questions[[n_question]]$answers[[n_subquestion]]$simple_text)
              
              if (subtitle != value) {
                
                participant <- append(participant,paste0("h ", heading, " - ", subtitle))
                
              }
              
              if (subtitle == value & str_sub(type, 1, 15) == "multiple_choice"){
                
                participant <- append(participant,paste0("h ", heading, " - ", subtitle))
                
              }
              
              participant <- append(participant,paste0("v ",value))
            }
          }
        }
      }
      
      data[[length(data)+1]] <- participant
    }
  }
  
  # Store the data in a list
  
  for (i in 1:length(data)) {
    
    select <- substr(data[[i]][grep("^[hv].*",data[[i]])],1,1)
    toremove <- c()
    
    for (j in 1:(length(select)-1)){
      
      if (select[j] == select[j+1]) {
        
        toremove <-  append(toremove, j)
      }
      
    }
    
    if (!is.null(toremove)) {
      
      selection <- data[[i]][grep("^[hv].*",data[[i]])][-toremove]
      
    }
    
    table_participant <-  data.frame(questions = str_sub(selection[grep("^[h].*", selection)], 3, -1),
                                     values = str_sub(selection[grep("^[v].*", selection)], 3, -1))
    
    data_participants <- c(data_participants, list(table_participant))
    
  }
  
  
  # Store data
  
  saveRDS(data_participants, file = "data_ef_d1.rds")
  
} else {
  
  cat("Pas de nouvelles données enregistrées")
  print(length(data_participants))
  
}

dates <- c()

if (length(data_participants) > 0) {
  
  for (i in 1:length(data_participants)) {
    dates <-  append(substring(filter(data_participants[[i]], questions %in% c("date"))$values,0,10),dates)
  }
}


#####################################################


mapping_agreement <- c("Pas du tout d'accord"=1,
                       "Pas d'accord"=2,
                       "Plutôt pas d'accord"=3,
                       "Ni en accord ni en désaccord"=4,
                       "Plutôt d'accord"=5,
                       "D'accord"=6,
                       "Tout à fait d'accord"=7)

mapping_acquisition <- c("Pas du tout d'acquis"=1,
                         "Pas acquis"=2,
                         "Plutôt pas acquis"=3,
                         "Neutre"=4,
                         "Plutôt d'acquis"=5,
                         "Acquis"=6,
                         "Tout à fait acquis"=7)

#####################################################

ui <- dashboardPage(title="Formation EF - Retours enseignants",
                    
                    dashboardHeader(title = HTML(paste(span("Formation EF - Retours enseignants\n",style = 'font-size:28px'), sep ="")), 
                                    titleWidth = 450,
                                    tags$li(actionLink("openModal", label = "", icon = icon("info")),
                                            class = "dropdown")),
                    
                    dashboardSidebar(width = 200,collapsed = TRUE,
                                     sidebarMenu(id = "sidebarid",
                                                 
                                                 menuItem("Résultats", tabName = "page1"),
                                                 conditionalPanel(
                                                   'input.sidebarid == "page1"'))),
                    
                    dashboardBody(
                      
                      tabItems(
                        
                        tabItem(tabName = "page1",
                                
                                
                                fluidRow(
                                  
                                  column(12, align="center",
                                         textInput("id", label = h3("Entrez votre identifiant :"), value = ""),
                                         HTML("<em>Le chargement peut prendre un moment...</em>"),
                                         br(),br())
                                ),
                                
                                fluidRow(
                                  
                                  column(12, align="center",
                                         div(style = "display:inline-block; float:center", selectInput(inputId="journee",label = "Journée :",choices = c("J4"),width=120)),
                                         div(style = "display:inline-block; float:center", selectInput(inputId="session",label = "Session :",choices = unique(dates),width=120)),
                                         br())
                                ),
                                
                                fluidRow(
                                  
                                  column(3,
                                         box(width=NULL,height = '530px', solidHeader = TRUE, status = "primary",
                                             title = "Données démographiques",
                                             selectInput(inputId = "demographie",label = "",choices = c("Age","Experience d'enseignement","Etablissements","Degrés enseignés"),width=250),
                                             plotOutput("demoPlot"))
                                  ),
                                  
                                  column(3,
                                         box(width=NULL,height = '530px', solidHeader = TRUE, status = "primary",
                                             title = "Motivation pour la formation",
                                             selectInput(inputId = "motivation",label = "",choices = c("Motivation pour apprendre","Utilité perçue des nouvelles technologies"),width=300),
                                             plotOutput("motivationPlot"))
                                         
                                  ),
                                  
                                  column(3,
                                         box(width=NULL,height = '530px', solidHeader = TRUE, status = "primary",
                                             title = "Perception de la formation et des formateurs",
                                             selectInput(inputId = "formateurs",label = "",choices = c("Formation riche et intéressante","Niveau adapté","Engagement des formateurs", "Lien entre formation et pratique"),width=300),
                                             plotOutput("formateursPlot"))
                                  ),
                                  
                                  column(3,
                                         box(width=NULL,height = '530px',style='width:400;overflow-x: scroll;height:488px;overflow-y: scroll;', solidHeader = TRUE, status = "primary",
                                             title = "Commentaires des enseignant.es",
                                             selectInput(inputId = "commentaires",label = "",choices = c("Appréciation générale - Commentaires",
                                                                                                         "Engagement des formateurs - Commentaires",
                                                                                                         "Lien entre formation et pratique - Commentaires",
                                                                                                         "Intérêt des activités - Commentaires",
                                                                                                         "Utilité des activités - Commentaires",
                                                                                                         "Auto-efficacité - Commentaires",
                                                                                                         "Intention d'utilisation - Commentaires",
                                                                                                         "Acquisition Pratiques numériques - Commentaires",
                                                                                                         "Acquisition Algorithmes et Scratch Jr - Commentaires",
                                                                                                         "Acquisition Partage - Commentaires",
                                                                                                         "Mise en oeuvre des activités - Commentaires"),width=350),
                                             DT::dataTableOutput("commentairesTable"))
                                         
                                  )),
                                
                                
                                fluidRow(
                                  
                                  column(6,
                                         
                                         box(width=NULL,height = '530px', solidHeader = TRUE, status = "primary",
                                             title = "Réactions post-journée de formation",
                                             selectInput(inputId = "reactionsImmediates",label = "",choices = c("Intérêt pour les ateliers",
                                                                                                                "Utilité perçue des ateliers",
                                                                                                                "Auto-efficacité vis-à-vis des ressources",
                                                                                                                "Intention d'utiliser les ressources"),width=330),
                                             plotOutput("reactionsPlot"))
                                         
                                  ),
                                  
                                  column(6,
                                         
                                         box(width=NULL,height = '530px', solidHeader = TRUE, status = "primary",
                                             title = "Acquisition des compétences",
                                             plotOutput("competencesPlot"))
                                         
                                  )
                                  
                                  
                                ),
                                
                                
                                fluidRow(
                                  
                                  column(4,
                                         
                                         box(width=NULL,height = '530px', solidHeader = TRUE, status = "primary",
                                             title = "Conditions facilitantes",
                                             selectInput(inputId = "conditionsFacilitantes",label = "",choices = c("Compatibilité avec les pratiques habituelles",
                                                                                                                   "Soutien des collègues",
                                                                                                                   "Charge de travail",
                                                                                                                   "Plus-value des formations"),width=330),
                                             
                                             plotOutput("conditionsPlot"))
                                  ),
                                  
                                  column(8,
                                         
                                         box(width=NULL,height = '530px', solidHeader = TRUE, status = "primary",
                                             title = "Mise en oeuvre en classe des activités vues dans les journées de formations précédentes",
                                             plotOutput("adoptionPlot"))
                                         
                                  )
                                  
                                  
                                ))
                        
                        
                        
                      )
                      
                    )
)

server <- function(input, output) {
  
  observe({
    
    if (input$id %in% c("031oftw",
                        "jv3kmdz",
                        "g6l4gur",
                        "tbu0m9c",
                        "9kdg8oi",
                        "0c0m5dc")) {
      
      ############## Processed data #######################
      
      # Get the stored data
      
      if (nrow(x) != 0) {
        
        data <- readRDS(file = "data_ef_d1_processed.rds")
        
      } else {
      
        data <- data.frame(journee=character(),
                           date=character(),
                           id_binome=character(),
                           age=character(),
                           experience=character(),
                           etablissement=character(),
                           degre_1P=character(),
                           degre_2P=character(),
                           degre_3P=character(),
                           degre_4P=character(),
                           degre_5P=character(),
                           degre_6P=character(),
                           degre_7P=character(),
                           degre_8P=character(),
                           degre_9S=character(),
                           degre_10S=character(),
                           degre_11S=character(),
                           degre_12S=character(),
                           motivation=character(),
                           utilite_techno=character(),
                           contenu_riche=character(),
                           contenu_adapte=character(),
                           appreciation_generale_commentaires=character(),
                           engagement_formateurs=character(),
                           liens_formation_pratique=character(),
                           engagement_commentaires=character(),
                           liens_formation_pratique_commentaires=character(),
                           interet_activites_commentaires=character(),
                           utilite_activites_commentaires=character(),
                           autoefficacite_activites_commentaires=character(),
                           intention_utilisation_commentaires=character(),
                           acquisition_pratiques_commentaires=character(),
                           acquisition_algorithmes_commentaires=character(),
                           acquisition_partage_commentaires=character(),
                           miseenoeuvre_commentaires=character(),
                           interet_pratiques=character(),
                           interet_algorithmes=character(),
                           interet_partages=character(),
                           interet_scratch=character(),
                           utilite_pratiques=character(),
                           utilite_algorithmes=character(),
                           utilite_partages=character(),
                           utilite_scratch=character(),
                           confiance_pratiques=character(),
                           confiance_algorithmes=character(),
                           confiance_partages=character(),
                           confiance_scratch=character(),
                           intention_pratiques=character(),
                           intention_algorithmes=character(),
                           intention_partages=character(),
                           intention_scratch=character(),
                           competence1=character(),
                           competence2=character(),
                           competence3=character(),
                           competence4=character(),
                           competence5=character(),
                           competence6=character(),
                           competence7=character(),
                           compatibilite=character(),
                           plusvalue=character(),
                           soutien=character(),
                           charge=character(),
                           charte=character(),
                           livre=character(),
                           machine=character(),
                           jeu=character(),
                           orchestration=character(),
                           bestioles=character(),
                           thymio=character(),
                           bluebot=character(),
                           edunum=character(),
                           utiliser_application=character(),
                           chope_pub=character(),
                           ecrans=character(),
                           tapis=character(),
                           stopmotion=character(),
                           album_loupe=character(),
                           album_livre=character(),
                           album_pfff=character(),
                           pasconcerne=character())
      }
      
      if (length(data_participants) > nrow(data)) {
        
        row_participant <- c()
        len_data <- nrow(data) + 1
        for (i in len_data:length(data_participants)) {
    
          row_participant <-  c(ifelse(substring(filter(data_participants[[i]], questions %in% c("date"))$values,0,10) < "2022-12-05","J4",NA),
                                ifelse(is.null(substring(filter(data_participants[[i]], questions %in% c("date"))$values,0,10)),NA,substring(filter(data_participants[[i]], questions %in% c("date"))$values,0,10)),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("id_binome"))$values),NA,filter(data_participants[[i]], questions %in% c("id_binome"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Votre âge"))$values),NA,filter(data_participants[[i]], questions %in% c("Votre âge"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Votre expérience dans l'enseignement en année(s)"))$values),NA,filter(data_participants[[i]], questions %in% c("Votre expérience dans l'enseignement en année(s)"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Dans quel établissement enseignez-vous?"))$values),NA,filter(data_participants[[i]], questions %in% c("Dans quel établissement enseignez-vous?"))$values),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Vous enseignez en - 1P"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Vous enseignez en - 1P"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Vous enseignez en - 2P"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Vous enseignez en - 2P"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Vous enseignez en - 3P"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Vous enseignez en - 3P"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Vous enseignez en - 4P"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Vous enseignez en - 4P"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Vous enseignez en - 5P"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Vous enseignez en - 5P"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Vous enseignez en - 6P"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Vous enseignez en - 6P"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Vous enseignez en - 7P"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Vous enseignez en - 7P"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Vous enseignez en - 8P"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Vous enseignez en - 8P"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Vous enseignez en - 9S"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Vous enseignez en - 9S"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Vous enseignez en - 10S"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Vous enseignez en - 10S"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Vous enseignez en - 11S"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Vous enseignez en - 11S"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Vous enseignez en - 12S"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Vous enseignez en - 12S"))$value),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Quand je participe à une formation continue, j'essaie d'apprendre le plus possible."))$values),NA,filter(data_participants[[i]], questions %in% c("Quand je participe à une formation continue, j'essaie d'apprendre le plus possible."))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Je pense que les technologies numériques sont utiles pour l'apprentissage."))$values),NA,filter(data_participants[[i]], questions %in% c("Je pense que les technologies numériques sont utiles pour l'apprentissage."))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Le contenu de la formation d'aujourd'hui - ...était riche et intéressant"))$values),NA,filter(data_participants[[i]], questions %in% c("Le contenu de la formation d'aujourd'hui - ...était riche et intéressant"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Le contenu de la formation d'aujourd'hui - ...avait un niveau adapté"))$values),NA,filter(data_participants[[i]], questions %in% c("Le contenu de la formation d'aujourd'hui - ...avait un niveau adapté"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Le contenu de la formation d'aujourd'hui - Other"))$values),NA,filter(data_participants[[i]], questions %in% c("Le contenu de la formation d'aujourd'hui - Other"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("J'ai apprécié l'engagement des formateurs et formatrices"))$values),NA,filter(data_participants[[i]], questions %in% c("J'ai apprécié l'engagement des formateurs et formatrices"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Les activités et les exercices utilisés par les formateurs et formatrices me permettent de voir comment appliquer ce que j'ai appris dans mon travail."))$values),NA,filter(data_participants[[i]], questions %in% c("Les activités et les exercices utilisés par les formateurs et formatrices me permettent de voir comment appliquer ce que j'ai appris dans mon travail."))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("J'ai apprécié l'engagement des formateurs et formatrices - Other"))$values),NA,filter(data_participants[[i]], questions %in% c("J'ai apprécié l'engagement des formateurs et formatrices - Other"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Les activités et les exercices utilisés par les formateurs et formatrices me permettent de voir comment appliquer ce que j'ai appris dans mon travail. - Other"))$values),NA,filter(data_participants[[i]], questions %in% c("Les activités et les exercices utilisés par les formateurs et formatrices me permettent de voir comment appliquer ce que j'ai appris dans mon travail. - Other"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("J'ai eu beaucoup de plaisir à suivre les ateliers suivants - Other"))$values),NA,filter(data_participants[[i]], questions %in% c("J'ai eu beaucoup de plaisir à suivre les ateliers suivants - Other"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Ma participation aux ateliers suivants est très utile pour mon travail. - Other"))$values),NA,filter(data_participants[[i]], questions %in% c("Ma participation aux ateliers suivants est très utile pour mon travail. - Other"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Je me sens en confiance pour utiliser, dans mon travail, les compétences apprises dans les ateliers suivants - Other"))$values),NA,filter(data_participants[[i]], questions %in% c("Je me sens en confiance pour utiliser, dans mon travail, les compétences apprises dans les ateliers suivants - Other"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Durant cette année scolaire, j'ai l'intention d'utiliser les ressources et contenus des ateliers suivants - Other"))$values),NA,filter(data_participants[[i]], questions %in% c("Durant cette année scolaire, j'ai l'intention d'utiliser les ressources et contenus des ateliers suivants - Other"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("A la fin de l'atelier \"Pratiques numérique\", je sais... - Other"))$values),NA,filter(data_participants[[i]], questions %in% c("A la fin de l'atelier \"Pratiques numérique\", je sais... - Other"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("A la fin des ateliers de sciences informatiques \"Algorithmes divers\" et \"Scratch Jr\", je sais... - Other"))$values),NA,filter(data_participants[[i]], questions %in% c("A la fin des ateliers de sciences informatiques \"Algorithmes divers\" et \"Scratch Jr\", je sais... - Other"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("A la fin de l'atelier \"Partage\", je sais... - Other"))$values),NA,filter(data_participants[[i]], questions %in% c("A la fin de l'atelier \"Partage\", je sais... - Other"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Other"))$values),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Other"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("J'ai eu beaucoup de plaisir à suivre les ateliers suivants - Pratiques numériques"))$values),NA,filter(data_participants[[i]], questions %in% c("J'ai eu beaucoup de plaisir à suivre les ateliers suivants - Pratiques numériques"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("J'ai eu beaucoup de plaisir à suivre les ateliers suivants - Algorithmes divers"))$values),NA,filter(data_participants[[i]], questions %in% c("J'ai eu beaucoup de plaisir à suivre les ateliers suivants - Algorithmes divers"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("J'ai eu beaucoup de plaisir à suivre les ateliers suivants - Partages et droits"))$values),NA,filter(data_participants[[i]], questions %in% c("J'ai eu beaucoup de plaisir à suivre les ateliers suivants - Partages et droits"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("J'ai eu beaucoup de plaisir à suivre les ateliers suivants - Scratch Junior"))$values),NA,filter(data_participants[[i]], questions %in% c("J'ai eu beaucoup de plaisir à suivre les ateliers suivants - Scratch Junior"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Ma participation aux ateliers suivants est très utile pour mon travail. - Pratiques numériques"))$values),NA,filter(data_participants[[i]], questions %in% c("Ma participation aux ateliers suivants est très utile pour mon travail. - Pratiques numériques"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Ma participation aux ateliers suivants est très utile pour mon travail. - Algorithmes divers"))$values),NA,filter(data_participants[[i]], questions %in% c("Ma participation aux ateliers suivants est très utile pour mon travail. - Algorithmes divers"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Ma participation aux ateliers suivants est très utile pour mon travail. - Partages et droits"))$values),NA,filter(data_participants[[i]], questions %in% c("Ma participation aux ateliers suivants est très utile pour mon travail. - Partages et droits"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Ma participation aux ateliers suivants est très utile pour mon travail. - Scratch Junior"))$values),NA,filter(data_participants[[i]], questions %in% c("Ma participation aux ateliers suivants est très utile pour mon travail. - Scratch Junior"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Je me sens en confiance pour utiliser, dans mon travail, les compétences apprises dans les ateliers suivants - Pratiques numériques"))$values),NA,filter(data_participants[[i]], questions %in% c("Je me sens en confiance pour utiliser, dans mon travail, les compétences apprises dans les ateliers suivants - Pratiques numériques"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Je me sens en confiance pour utiliser, dans mon travail, les compétences apprises dans les ateliers suivants - Algorithmes divers"))$values),NA,filter(data_participants[[i]], questions %in% c("Je me sens en confiance pour utiliser, dans mon travail, les compétences apprises dans les ateliers suivants - Algorithmes divers"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Je me sens en confiance pour utiliser, dans mon travail, les compétences apprises dans les ateliers suivants - Partages et droits"))$values),NA,filter(data_participants[[i]], questions %in% c("Je me sens en confiance pour utiliser, dans mon travail, les compétences apprises dans les ateliers suivants - Partages et droits"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Je me sens en confiance pour utiliser, dans mon travail, les compétences apprises dans les ateliers suivants - Scratch Junior"))$values),NA,filter(data_participants[[i]], questions %in% c("Je me sens en confiance pour utiliser, dans mon travail, les compétences apprises dans les ateliers suivants - Scratch Junior"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Durant cette année scolaire, j'ai l'intention d'utiliser les ressources et contenus des ateliers suivants - Pratiques numériques"))$values),NA,filter(data_participants[[i]], questions %in% c("Durant cette année scolaire, j'ai l'intention d'utiliser les ressources et contenus des ateliers suivants - Pratiques numériques"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Durant cette année scolaire, j'ai l'intention d'utiliser les ressources et contenus des ateliers suivants - Algorithmes divers"))$values),NA,filter(data_participants[[i]], questions %in% c("Durant cette année scolaire, j'ai l'intention d'utiliser les ressources et contenus des ateliers suivants - Algorithmes divers"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Durant cette année scolaire, j'ai l'intention d'utiliser les ressources et contenus des ateliers suivants - Partages et droits"))$values),NA,filter(data_participants[[i]], questions %in% c("Durant cette année scolaire, j'ai l'intention d'utiliser les ressources et contenus des ateliers suivants - Partages et droits"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Durant cette année scolaire, j'ai l'intention d'utiliser les ressources et contenus des ateliers suivants - Scratch Junior"))$values),NA,filter(data_participants[[i]], questions %in% c("Durant cette année scolaire, j'ai l'intention d'utiliser les ressources et contenus des ateliers suivants - Scratch Junior"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("A la fin de l'atelier \"Pratiques numérique\", je sais... - Sélectionner et évaluer des ressources numériques existantes"))$values),NA,filter(data_participants[[i]], questions %in% c("A la fin de l'atelier \"Pratiques numérique\", je sais... - Sélectionner et évaluer des ressources numériques existantes"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("A la fin de l'atelier \"Pratiques numérique\", je sais... - Planifier et mettre en oeuvre des ressources numériques variées dans mon enseignement."))$values),NA,filter(data_participants[[i]], questions %in% c("A la fin de l'atelier \"Pratiques numérique\", je sais... - Planifier et mettre en oeuvre des ressources numériques variées dans mon enseignement."))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("A la fin des ateliers de sciences informatiques \"Algorithmes divers\" et \"Scratch Jr\", je sais... - Manipuler l’environnement numérique de Scratch junior"))$values),NA,filter(data_participants[[i]], questions %in% c("A la fin des ateliers de sciences informatiques \"Algorithmes divers\" et \"Scratch Jr\", je sais... - Manipuler l’environnement numérique de Scratch junior"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("A la fin des ateliers de sciences informatiques \"Algorithmes divers\" et \"Scratch Jr\", je sais... - Définir ce qu’est un algorithme"))$values),NA,filter(data_participants[[i]], questions %in% c("A la fin des ateliers de sciences informatiques \"Algorithmes divers\" et \"Scratch Jr\", je sais... - Définir ce qu’est un algorithme"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("A la fin des ateliers de sciences informatiques \"Algorithmes divers\" et \"Scratch Jr\", je sais... - Mettre en oeuvre des activités permettant le développement de compétences de lecture, de compréhension et de production."))$values),NA,filter(data_participants[[i]], questions %in% c("A la fin des ateliers de sciences informatiques \"Algorithmes divers\" et \"Scratch Jr\", je sais... - Mettre en oeuvre des activités permettant le développement de compétences de lecture, de compréhension et de production."))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("A la fin de l'atelier \"Partage\", je sais... - Découvrir des activités permettant d’évaluer les impacts sociaux: liberté individuelle, données personnelles"))$values),NA,filter(data_participants[[i]], questions %in% c("A la fin de l'atelier \"Partage\", je sais... - Découvrir des activités permettant d’évaluer les impacts sociaux: liberté individuelle, données personnelles"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("A la fin de l'atelier \"Partage\", je sais... - Respecter les règles de confidentialité et de droits d'auteur.e.s"))$values),NA,filter(data_participants[[i]], questions %in% c("A la fin de l'atelier \"Partage\", je sais... - Respecter les règles de confidentialité et de droits d'auteur.e.s"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Les contenus de formation correspondent étroitement à ce qui m'est demandé dans ma pratique d'enseignant.e"))$values),NA,filter(data_participants[[i]], questions %in% c("Les contenus de formation correspondent étroitement à ce qui m'est demandé dans ma pratique d'enseignant.e"))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("J'ai l'impression que plus j'applique les contenus et ressources de la formation dans mon enseignement, mieux je fais mon travail."))$values),NA,filter(data_participants[[i]], questions %in% c("J'ai l'impression que plus j'applique les contenus et ressources de la formation dans mon enseignement, mieux je fais mon travail."))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Mes collègues m'encouragent à utiliser les compétences acquises en formation."))$values),NA,filter(data_participants[[i]], questions %in% c("Mes collègues m'encouragent à utiliser les compétences acquises en formation."))$values),
                                ifelse(is.null(filter(data_participants[[i]], questions %in% c("Ma charge de travail me laisse le temps d'essayer les nouvelles choses que j'ai apprises."))$values),NA,filter(data_participants[[i]], questions %in% c("Ma charge de travail me laisse le temps d'essayer les nouvelles choses que j'ai apprises."))$values),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - La Charte (vu en J1)"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - La Charte (vu en J1)"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - BookCreator - le livre multimédia (vu en J1)"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - BookCreator - le livre multimédia (vu en J1)"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - La Machine à tri (vu en J1)"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - La Machine à tri (vu en J1)"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Le Jeu du robot (vu en J1)"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Le Jeu du robot (vu en J1)"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Orchestration et gestion des IPads de la classe (vu en J1)"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Orchestration et gestion des IPads de la classe (vu en J1)"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Les bestioles (vu en J1)"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Les bestioles (vu en J1)"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Le robot Thymio (vu en J2)"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Le robot Thymio (vu en J2)"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Le robot Bluebot (vu en J2)"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Le robot Bluebot (vu en J2)"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Edunum et différenciation (vu en J2)"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Edunum et différenciation (vu en J2)"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Utiliser des applications numériques disciplinaires (vu en J3)"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Utiliser des applications numériques disciplinaires (vu en J3)"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Chope la pub (vu en J3)"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Chope la pub (vu en J3)"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Où sont les écrans - poster de la ville ? (vu en J3)"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Où sont les écrans - poster de la ville ? (vu en J3)"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Le tapis des écrans (vu en J2)"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Le tapis des écrans (vu en J2)"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Stop Motion - film d’animation (vu en J3)"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Stop Motion - film d’animation (vu en J3)"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Album «Loupé» (vu en J3)"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Album «Loupé» (vu en J3)"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Album «C’est un livre» (vu en J3)"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Album «C’est un livre» (vu en J3)"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Album «Pfff» (vu en J2)"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Album «Pfff» (vu en J2)"))$value),
                                ifelse(identical(filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Pas concerné"))$value, character(0)),NA,filter(data_participants[[i]], questions %in% c("Durant l'année scolaire 2021-2022, je me suis servi des modules de formation suivants pour réaliser des activités avec mes élèves : - Pas concerné"))$value))
          
          data[i,] <- row_participant
        
        }
        
        # Store data
        
        saveRDS(data, file = "data_ef_d1_processed.rds")
        
      }

      ###################################################  
      
      output$demoPlot <- renderPlot({
      
        if (input$demographie == "Age") {
          
          p <- ggplot(data[data$journee==input$journee&data$date==input$session&data$id_binome==input$id,], aes(x=id_binome, y=as.numeric(age))) +
            geom_boxplot(fill='#F59138', color="black", alpha=0.7, outlier.shape = NA) +
            geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
            ggtitle(str_wrap("Répartition de l'âge des enseignant.e.s",40)) +
            theme_minimal() +
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size=16),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          return(p)
          
        }
        
        
        if (input$demographie == "Experience d'enseignement") {
          
          p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=as.numeric(experience)))  +
            geom_boxplot(fill='#F59138', color="black", alpha=0.7, outlier.shape = NA) +
            geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
            ggtitle(str_wrap("Répartition des enseignant.e.s par établissement",40)) +
            theme_minimal() +
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size=16),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          return(p)
          
        }
        
        if (input$demographie == "Etablissements") {
          
          data_plot <- data.frame(table(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("etablissement")]))
          
          p <- ggplot(data_plot, aes(x=Var1, y=Freq)) +
            geom_bar(stat = "identity",fill='#F59138',alpha=0.7) +
            geom_label(aes(label = Freq, vjust = 1.5, size=5)) +
            theme_minimal() +
            scale_x_discrete(labels=label_wrap(15)) +
            ggtitle(str_wrap("Répartition des enseignant.e.s par établissement",40)) +
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_text(angle = 70, vjust = 0.5, hjust = 0.5,size=11),
                  axis.text.y = element_blank(),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          
          return(p)
          
        }
        
        if (input$demographie == "Degrés enseignés") {
          
          row_participant <- c()
          
          selection <- data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("degre_1P","degre_2P","degre_3P","degre_4P","degre_5P","degre_6P","degre_7P","degre_8P","degre_9S","degre_10S","degre_11S","degre_12S")]
          
          
          Freq <- c(sum(!is.na(selection$"degre_1P")),
                    sum(!is.na(selection$"degre_2P")),
                    sum(!is.na(selection$"degre_3P")),
                    sum(!is.na(selection$"degre_4P")),
                    sum(!is.na(selection$"degre_5P")),
                    sum(!is.na(selection$"degre_6P")),
                    sum(!is.na(selection$"degre_7P")),
                    sum(!is.na(selection$"degre_8P")),
                    sum(!is.na(selection$"degre_9S")),
                    sum(!is.na(selection$"degre_10S")),
                    sum(!is.na(selection$"degre_11S")),
                    sum(!is.na(selection$"degre_12S")))
          
          degre <-  gsub("degre_\\.*","",colnames(selection))
          
          data_plot <- data.frame(Var1 = factor(degre,levels = degre), Freq = Freq)
          
          data_plot <- data_plot[!data_plot$Freq==0,]
          
          p <- ggplot(data_plot, aes(x=Var1, y=Freq)) +
            geom_bar(stat = "identity",fill='#F59138',alpha=0.7) +
            geom_label(aes(label = Freq, vjust = 1.5, size=5)) +
            theme_minimal() +
            scale_x_discrete(labels=label_wrap(15)) +
            ggtitle(str_wrap("Répartition des enseignant.e.s par degré",40)) +
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5,size=11),
                  axis.text.y = element_blank(),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          
          return(p)
        }
        
      })
      
      output$motivationPlot <- renderPlot({
        
        if (input$motivation == "Motivation pour apprendre") {
          
          data$motivation <- mapping_agreement[data$motivation]
          
          p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=motivation)) +
            geom_boxplot(fill='#AD5B8F', color="black", alpha=0.7, outlier.shape = NA) +
            geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
            ggtitle(str_wrap("Quand je participe à une formation continue, j'essaie d'apprendre le plus possible",width = 40)) +
            scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                          "Pas \nd'accord",
                                          "Plutôt pas \nd'accord",
                                          "Ni en accord \nni en désaccord",
                                          "Plutôt \nd'accord",
                                          "D'accord",
                                          "Tout à fait \nd'accord"), breaks = 1:7) +
            theme_minimal() +
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size=12),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          return(p)
          
        }
        
        if (input$motivation == "Utilité perçue des nouvelles technologies") {
          
          data$utilite_techno <- mapping_agreement[data$utilite_techno]
          
          p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=utilite_techno)) +
            geom_boxplot(fill='#AD5B8F', color="black", alpha=0.7, outlier.shape = NA) +
            geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
            ggtitle(str_wrap("Je pense que les technologies numériques sont \nutiles pour l'apprentissage",40)) +
            scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                          "Pas \nd'accord",
                                          "Plutôt pas \nd'accord",
                                          "Ni en accord \nni en désaccord",
                                          "Plutôt \nd'accord",
                                          "D'accord",
                                          "Tout à fait \nd'accord"), breaks = 1:7) +
            theme_minimal() +   
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size=12),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          return(p)
          
        }
        
      })
      
      output$formateursPlot <- renderPlot({
        
        if (input$formateurs == "Formation riche et intéressante") {
          
          data$contenu_riche <- mapping_agreement[data$contenu_riche]
          
          p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=contenu_riche)) +
            geom_boxplot(fill='#D2CFC8', color="black", alpha=0.7, outlier.shape = NA) +
            geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
            ggtitle(str_wrap("Le contenu de la formation d'aujourd'hui\nétait riche et intéressant",40)) +
            scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                          "Pas \nd'accord",
                                          "Plutôt pas \nd'accord",
                                          "Ni en accord \nni en désaccord",
                                          "Plutôt \nd'accord",
                                          "D'accord",
                                          "Tout à fait \nd'accord"), breaks = 1:7) +
            theme_minimal() +
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size=12),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          return(p)
          
        }
        
        if (input$formateurs == "Niveau adapté") {
          
          data$contenu_adapte <- mapping_agreement[data$contenu_adapte]
          
          p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=contenu_adapte)) +
            geom_boxplot(fill='#D2CFC8', color="black", alpha=0.7, outlier.shape = NA) +
            geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
            ggtitle(str_wrap("Le contenu de la formation d'aujourd'hui\navait un niveau adapté",40)) +
            scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                          "Pas \nd'accord",
                                          "Plutôt pas \nd'accord",
                                          "Ni en accord \nni en désaccord",
                                          "Plutôt \nd'accord",
                                          "D'accord",
                                          "Tout à fait \nd'accord"), breaks = 1:7) +
            theme_minimal() +
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size=12),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          return(p)
          
        }
        
        if (input$formateurs == "Engagement des formateurs") {
          
          data$engagement_formateurs <- mapping_agreement[data$engagement_formateurs]
          
          p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=engagement_formateurs)) +
            geom_boxplot(fill='#D2CFC8', color="black", alpha=0.7, outlier.shape = NA) +
            geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
            ggtitle(str_wrap("J'ai apprécié l'engagement des format.eur.rice.s",40)) +
            scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                          "Pas \nd'accord",
                                          "Plutôt pas \nd'accord",
                                          "Ni en accord \nni en désaccord",
                                          "Plutôt \nd'accord",
                                          "D'accord",
                                          "Tout à fait \nd'accord"), breaks = 1:7) +
            theme_minimal() +
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size=12),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          return(p)
          
        }
        
        if (input$formateurs == "Lien entre formation et pratique") {
          
          data$liens_formation_pratique <- mapping_agreement[data$liens_formation_pratique]
          
          p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=liens_formation_pratique)) +
            geom_boxplot(fill='#D2CFC8', color="black",alpha=0.7, outlier.shape = NA) +
            geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
            theme_minimal() +
            ggtitle(str_wrap("Les activités et les exercices utilisés par les \nformat.eur.rice.s me permettent \nde voir comment appliquer \nce que j'ai appris dans mon travail",40)) +
            scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                          "Pas \nd'accord",
                                          "Plutôt pas \nd'accord",
                                          "Ni en accord \nni en désaccord",
                                          "Plutôt \nd'accord",
                                          "D'accord",
                                          "Tout à fait \nd'accord"), breaks = 1:7) +
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size=12),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          return(p)
          
        }
        
      })
      
      output$commentairesTable <- renderDataTable({
        
        if (input$commentaires == "Appréciation générale - Commentaires") {
          
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("appreciation_generale_commentaires")]))))
          
        }
        
        if (input$commentaires == "Engagement des formateurs - Commentaires") {
          
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("engagement_commentaires")]))))
          
        }
        
        if (input$commentaires == "Engagement des formateurs - Commentaires") {
          
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("engagement_commentaires")]))))
          
        }
        
        if (input$commentaires == "Lien entre formation et pratique - Commentaires") {
          
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("liens_formation_pratique_commentaires")]))))
          
        }
        
        if (input$commentaires == "Intérêt des activités - Commentaires") {
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("interet_activites_commentaires")]))))
          
        }
        
        if (input$commentaires == "Utilité des activités - Commentaires") {
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("utilite_activites_commentaires")]))))
          
        }
        
        if (input$commentaires == "Auto-efficacité - Commentaires") {
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("autoefficacite_activites_commentaires")]))))
          
        }
        
        if (input$commentaires == "Intention d'utilisation - Commentaires") {
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("intention_utilisation_commentaires")]))))
          
        }
        
        if (input$commentaires == "Acquisition Pratiques numériques - Commentaires") {
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("acquisition_pratiques_commentaires")]))))
          
        }
        
        if (input$commentaires == "Acquisition Algorithmes et Scratch Jr - Commentaires") {
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("acquisition_algorithmes_commentaires")]))))
          
        }
        
        
        if (input$commentaires == "Acquisition Partage - Commentaires") {
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("acquisition_partage_commentaires")]))))
          
        }
        
        
        if (input$commentaires == "Mise en oeuvre des activités - Commentaires") {
          
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("miseenoeuvre_commentaires")]))))
          
        }
        
      })
      
      # Réactions immédiates
      
      output$reactionsPlot <- renderPlot({
        
        if (input$reactionsImmediates == "Intérêt pour les ateliers") {
          
          data$interet_pratiques <- mapping_agreement[data$interet_pratiques]
          data$interet_algorithmes <- mapping_agreement[data$interet_algorithmes]
          data$interet_partages <- mapping_agreement[data$interet_partages]
          data$interet_scratch <- mapping_agreement[data$interet_scratch]
          
          selection <- data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("interet_pratiques",
                                                                                                            "interet_algorithmes",
                                                                                                            "interet_partages",
                                                                                                            "interet_scratch")]
          
          table <- data.frame(values = c(selection[,1],selection[,2],selection[,3],selection[,4]),
                              activites = c(rep("Pratiques \nnumériques",length(selection[,1])),
                                            rep("Algorithmes \ndivers",length(selection[,2])),
                                            rep("Partages et \ndroits",length(selection[,3])),
                                            rep("Scratch \nJunior",length(selection[,4]))))
          
          p <- ggplot(table[complete.cases(table), ], aes(x=activites, y=values)) +
            geom_boxplot(fill='#FADE7D', color="black",alpha=0.7, outlier.shape = NA) +
            geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
            theme_minimal() +
            ggtitle(str_wrap("J'ai eu beaucoup de plaisir à suivre les ateliers suivants",40)) +
            scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                          "Pas \nd'accord",
                                          "Plutôt pas \nd'accord",
                                          "Ni en accord \nni en désaccord",
                                          "Plutôt \nd'accord",
                                          "D'accord",
                                          "Tout à fait \nd'accord"), breaks = 1:7) +
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5,size=11),
                  axis.text.y = element_text(size=12),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          return(p)
          
        }
        
        if (input$reactionsImmediates == "Utilité perçue des ateliers") {
          
          data$utilite_pratiques <- mapping_agreement[data$utilite_pratiques]
          data$utilite_algorithmes <- mapping_agreement[data$utilite_algorithmes]
          data$utilite_partages <- mapping_agreement[data$utilite_partages]
          data$utilite_scratch <- mapping_agreement[data$utilite_scratch]
          
          
          selection <- data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("utilite_pratiques",
                                                                                                            "utilite_algorithmes",
                                                                                                            "utilite_partages",
                                                                                                            "utilite_scratch")]
          
          table <- data.frame(values = c(selection[,1],selection[,2],selection[,3],selection[,4]),
                              activites = c(rep("Pratiques \nnumériques",length(selection[,1])),
                                            rep("Algorithmes \ndivers",length(selection[,2])),
                                            rep("Partages et \ndroits",length(selection[,3])),
                                            rep("Scratch \nJunior",length(selection[,4]))))
          
          p <- ggplot(table[complete.cases(table), ], aes(x=activites, y=values)) +
            geom_boxplot(fill='#FADE7D', color="black",alpha=0.7, outlier.shape = NA) +
            geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
            theme_minimal() +
            ggtitle(str_wrap("Ma participation aux ateliers suivants est très utile pour mon travail",40)) +
            scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                          "Pas \nd'accord",
                                          "Plutôt pas \nd'accord",
                                          "Ni en accord \nni en désaccord",
                                          "Plutôt \nd'accord",
                                          "D'accord",
                                          "Tout à fait \nd'accord"), breaks = 1:7) +
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5,size=11),
                  axis.text.y = element_text(size=12),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          return(p)
          
        }
        
        if (input$reactionsImmediates == "Auto-efficacité vis-à-vis des ressources") {
          
          data$confiance_pratiques <- mapping_agreement[data$confiance_pratiques]
          data$confiance_algorithmes <- mapping_agreement[data$confiance_algorithmes]
          data$confiance_partages <- mapping_agreement[data$confiance_partages]
          data$confiance_scratch <- mapping_agreement[data$confiance_scratch]
          
          
          selection <- data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("confiance_pratiques",
                                                                                                            "confiance_algorithmes",
                                                                                                            "confiance_partages",
                                                                                                            "confiance_scratch")]
          
          table <- data.frame(values = c(selection[,1],selection[,2],selection[,3],selection[,4]),
                              activites = c(rep("Pratiques \nnumériques",length(selection[,1])),
                                            rep("Algorithmes \ndivers",length(selection[,2])),
                                            rep("Partages et \ndroits",length(selection[,3])),
                                            rep("Scratch \nJunior",length(selection[,4]))))
          
          p <- ggplot(table[complete.cases(table), ], aes(x=activites, y=values)) +
            geom_boxplot(fill='#FADE7D', color="black",alpha=0.7, outlier.shape = NA) +
            geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
            theme_minimal() +
            ggtitle(str_wrap("Je me sens en confiance pour utiliser dans mon travail \nles compétences apprises dans les ateliers suivants",40)) +
            scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                          "Pas \nd'accord",
                                          "Plutôt pas \nd'accord",
                                          "Ni en accord \nni en désaccord",
                                          "Plutôt \nd'accord",
                                          "D'accord",
                                          "Tout à fait \nd'accord"), breaks = 1:7) +
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5,size=11),
                  axis.text.y = element_text(size=12),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          return(p)
          
        }
        
        if (input$reactionsImmediates == "Intention d'utiliser les ressources") {
          
          selection <- data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("intention_pratiques",
                                                                                                            "intention_algorithmes",
                                                                                                            "intention_partages",
                                                                                                            "intention_scratch")]
          
          table <- data.frame(values = c(selection[,1],selection[,2],selection[,3],selection[,4]),
                              activites = c(rep("Pratiques \nnumériques",length(selection[,1])),
                                            rep("Algorithmes \ndivers",length(selection[,2])),
                                            rep("Partages et \ndroits",length(selection[,3])),
                                            rep("Scratch \nJunior",length(selection[,4]))))
          
          
          p <- ggplot(table[complete.cases(table), ], aes(x=activites, y=values)) +
            geom_boxplot(fill='#FADE7D', color="black", alpha=0.7, outlier.shape = NA) +
            geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
            theme_minimal() +
            ggtitle(str_wrap("J'ai l'intention d'utiliser les ressources et contenus des ateliers suivants",40)) +
            scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                          "Pas \nd'accord",
                                          "Plutôt pas \nd'accord",
                                          "Ni en accord \nni en désaccord",
                                          "Plutôt \nd'accord",
                                          "D'accord",
                                          "Tout à fait \nd'accord"), breaks = 1:7) +
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5,size=11),
                  axis.text.y = element_text(size=12),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          return(p)
          
        }
        
      })
      
      output$competencesPlot <- renderPlot({
        
        data$competence1 <- mapping_acquisition[data$competence1]
        data$competence2 <- mapping_acquisition[data$competence2]
        data$competence3 <- mapping_acquisition[data$competence3]
        data$competence4 <- mapping_acquisition[data$competence4]
        data$competence5 <- mapping_acquisition[data$competence5]
        data$competence6 <- mapping_acquisition[data$competence6]
        data$competence7 <- mapping_acquisition[data$competence7]
        #data <- data[complete.cases(data),]
        
        selection <- data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("competence1",
                                                                                                          "competence2",
                                                                                                          "competence3",
                                                                                                          "competence4",
                                                                                                          "competence5",
                                                                                                          "competence6",
                                                                                                          "competence7")]
        
        table <- data.frame(values = c(selection[,1],selection[,2],selection[,3],selection[,4],selection[,5],selection[,6],selection[,7]),
                            activites = c(rep("Pratiques\nnumériques : \nSélectionner et\névaluer des \nressources\nnumériques \nexistantes",length(selection[,1])),
                                          rep("Pratiques\nnumériques : \nPlanifier et mettre\nen oeuvre des\nressources \nnumériques",length(selection[,2])),
                                          rep("Algo & Scratch\nJunior : \nManipuler \nl'environnement\nnumérique",length(selection[,3])),
                                          rep("Algo & Scratch\nJunior : \nDéfinir algorithme",length(selection[,4])),
                                          rep("Algo & Scratch\nJunior : \nMettre en oeuvre \nactivités",length(selection[,5])),
                                          rep("Partage : \nDécouvrir activités",length(selection[,6])),
                                          rep("Partage : \nRespecter \nconfidentialité",length(selection[,7]))))
        
        
        p <- ggplot(table[complete.cases(table), ], aes(x=activites, y=values)) +
          geom_boxplot(fill='#71B578', color="black", alpha=0.7, outlier.shape = NA) +
          geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
          theme_minimal() +
          ggtitle(str_wrap("A la fin des activités, je sais...",40)) +
          scale_y_continuous(labels = c("Pas du \ntout acquis",
                                        "Pas \nacquis",
                                        "Plutôt pas \nacquis",
                                        "Neutre",
                                        "Plutôt \nacquis",
                                        "Acquis",
                                        "Tout à fait \nacquis"), breaks = 1:7) +
          theme(plot.title.position = "plot",
                plot.title = element_text(face = "italic",size = 10),
                legend.position = "none",
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5,size=8),
                axis.text.y = element_text(size=12),
                #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
        
        return(p)
        
        
      })
      
      output$conditionsPlot <- renderPlot({
        
        if (input$conditionsFacilitantes == "Compatibilité avec les pratiques habituelles") {
          
          data$compatibilite <- mapping_agreement[data$compatibilite]
          
          p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=compatibilite)) +
            geom_boxplot(fill='#23AFD6', color="black", alpha=0.7, outlier.shape = NA) +
            geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
            theme_minimal() +
            ggtitle(str_wrap("Les contenus de formation correspondent étroitement\nà ce qui m'est demandé dans ma pratique d'enseignant.e",40)) +
            scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                          "Pas \nd'accord",
                                          "Plutôt pas \nd'accord",
                                          "Ni en accord \nni en désaccord",
                                          "Plutôt \nd'accord",
                                          "D'accord",
                                          "Tout à fait \nd'accord"), breaks = 1:7) +
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size=12),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          return(p)
          
        }
        
        if (input$conditionsFacilitantes == "Plus-value des formations") {
          
          
          data$plusvalue <- mapping_agreement[data$plusvalue]
          
          p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=plusvalue)) +
            geom_boxplot(fill='#23AFD6', color="black",alpha=0.7, outlier.shape = NA) +
            geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
            theme_minimal() +
            ggtitle(str_wrap("J'ai l'impression que plus j'applique les contenus et ressources\nde la formation dans mon enseignement, mieux je fais mon travail",40)) +
            scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                          "Pas \nd'accord",
                                          "Plutôt pas \nd'accord",
                                          "Ni en accord \nni en désaccord",
                                          "Plutôt \nd'accord",
                                          "D'accord",
                                          "Tout à fait \nd'accord"), breaks = 1:7) +
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size=12),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          return(p)
          
        }
        
        if (input$conditionsFacilitantes == "Soutien des collègues") {
          
          data$soutien <- mapping_agreement[data$soutien]
          
          p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=soutien)) +
            geom_boxplot(fill='#23AFD6', color="black", alpha=0.7, outlier.shape = NA) +
            geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
            theme_minimal() +
            ggtitle(str_wrap("Mes collègues m'encouragent à utiliser\nles compétences acquises en formation",40)) +
            scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                          "Pas \nd'accord",
                                          "Plutôt pas \nd'accord",
                                          "Ni en accord \nni en désaccord",
                                          "Plutôt \nd'accord",
                                          "D'accord",
                                          "Tout à fait \nd'accord"), breaks = 1:7) +
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size=12),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          return(p)
          
        }
        
        
        if (input$conditionsFacilitantes == "Charge de travail") {
          
          data$charge <- mapping_agreement[data$charge]
          
          p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=charge)) +
            geom_boxplot(fill='#23AFD6', color="black", alpha=0.7, outlier.shape = NA) +
            geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
            theme_minimal() +
            ggtitle(str_wrap("Ma charge de travail me laisse le temps \nd'essayer les nouvelles choses que j'ai apprises.",40)) +
            scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                          "Pas \nd'accord",
                                          "Plutôt pas \nd'accord",
                                          "Ni en accord \nni en désaccord",
                                          "Plutôt \nd'accord",
                                          "D'accord",
                                          "Tout à fait \nd'accord"), breaks = 1:7) +
            theme(plot.title.position = "plot",
                  plot.title = element_text(face = "italic",size = 10),
                  legend.position = "none",
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size=12),
                  #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                  #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          
          return(p)
          
        }
        
      })
      
      output$adoptionPlot <- renderPlot({
        
        x <- c("La Charte (J1)",
               "BookCreator\nle livre multimédia (J1)",
               "La Machine à tri (J1)",
               "Le Jeu du robot (J1)",
               "Orchestration et gestion\ndes IPads de la classe (J1)",
               "Les bestioles (J1)",
               "Le robot Thymio (J2)",
               "Le robot Bluebot (J2)",
               "Edunum et différenciation (J2)",
               "Utiliser des applications\nnumériques disciplinaires (J3)",
               "Chope la pub (J3)",
               "Où sont les écrans\nposter de la ville ? (J3)",
               "Le tapis des écrans (J2)",
               "Stop Motion - film d’animation (J3)",
               "Album «Loupé» (J3)",
               "Album «C’est un livre» (J3)",
               "Album «Pfff» (j2)")
        
        selection <- data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("charte",
                                                                                                          "livre",
                                                                                                          "machine",
                                                                                                          "jeu",
                                                                                                          "orchestration",
                                                                                                          "bestioles",
                                                                                                          "thymio",
                                                                                                          "bluebot",
                                                                                                          "edunum",
                                                                                                          "utiliser_application",
                                                                                                          "chope_pub",
                                                                                                          "ecrans",
                                                                                                          "tapis",
                                                                                                          "stopmotion",
                                                                                                          "album_loupe",
                                                                                                          "album_livre",
                                                                                                          "album_pfff")]
        
        Freq <- c(sum(!is.na(selection$"charte")),
                  sum(!is.na(selection$"livre")),
                  sum(!is.na(selection$"machine")),
                  sum(!is.na(selection$"jeu")),
                  sum(!is.na(selection$"orchestration")),
                  sum(!is.na(selection$"bestioles")),
                  sum(!is.na(selection$"thymio")),
                  sum(!is.na(selection$"bluebot")),
                  sum(!is.na(selection$"edunum")),
                  sum(!is.na(selection$"utiliser_application")),
                  sum(!is.na(selection$"chope_pub")),
                  sum(!is.na(selection$"ecrans")),
                  sum(!is.na(selection$"tapis")),
                  sum(!is.na(selection$"stopmotion")),
                  sum(!is.na(selection$"album_loupe")),
                  sum(!is.na(selection$"album_livre")),
                  sum(!is.na(selection$"album_pfff")))
        
        data_plot <- data.frame(Var1 = x, Freq = Freq)
        #data_plot <- data_plot[!data_plot$Freq==0,]
        
        p <- ggplot(data_plot, aes(x=Var1, y=Freq)) +
          geom_bar(stat = "identity",fill='#F59138',alpha=0.7) +
          geom_label(aes(label = Freq, vjust = 1.5, size=5)) +
          theme_minimal() +
          scale_x_discrete(labels=label_wrap(15)) +
          ggtitle(str_wrap("Nombre d'enseignants ayant mis en oeuvre les activités des journées précédentes",40)) +
          theme(plot.title.position = "plot",
                plot.title = element_text(face = "italic",size = 10),
                legend.position = "none",
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.x = element_text(angle = 70, vjust = 0.5, hjust = 0.5,size=11),
                axis.text.y = element_blank(),
                #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
        
        
        return(p)
        
      })
      
    }
    
  })
  
  
}


shinyApp(ui = ui, server = server)
