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


################################# DATA COLLECTION VIA SurveyMonkey API #################################


sm_api_key = ''
sm_secret = ''
sm_client_id = ''

survey_id <- ""

details <- paste0("https://api.surveymonkey.com/v3/surveys/",survey_id)
content_details <- content(GET(details,add_headers(Authorization = paste0("Bearer ", sm_api_key))),"parsed")

collector <- ""
collector <- paste0("https://api.surveymonkey.com/v3/collectors/",collector)

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

# Load the data if some already stored

if (file.exists("data_ef_d1.rds")){
  
  data <- readRDS(file = "data_ef_d1.rds")
  
} else {
  
  data <- data.frame()
  
}

# Get the responses from SurveyMonkey

# The API provides the data by batch of 50 responses. If there is new responses, 
# the number of responses is strictly superior to nrow(data) 

if (content_details$response_count > nrow(data)) {
  
  data <- head(data,-(nrow(data) %% 50)) # Remove the last incomplete batch from the data
  
  #day <- str_sub(content(GET(collector,add_headers(Authorization = paste0("Bearer ", sm_api_key))),"parsed")$url, -2, -1)
  
  pages <- (nrow(data) %/% 50 + 1):(content_details$response_count %/% 50 + 1) # Return the range of batches to be added to stored data
  
  responses_urls <- lapply(pages, function(x) {paste0("https://api.surveymonkey.com/v3/surveys/",survey_id,"/responses/bulk?simple=true&per_page=50&page=",x)}) # Return the urls of the bachtes of interest
  
  content_responses <- list()
  
  ## For each batch, a API call is made to get the data and stored in content_responses
  
  for (i in 1:length(responses_urls)) {
    
    content_responses <- append(content_responses,list(content(GET(responses_urls[[i]],add_headers(Authorization = paste0("Bearer ", sm_api_key))),"parsed")))
    
  }
  
  # Questions
  
  columns <- c("journee",
               "date",
               "id_binome",
               "age",
               "experience",
               "etablissement",
               "degre_1P",
               "degre_2P",
               "degre_3P",
               "degre_4P",
               "degre_5P",
               "degre_6P",
               "degre_7P",
               "degre_8P",
               "degre_9S",
               "degre_10S",
               "degre_11S",
               "degre_12S",
               "motivation",
               "utilite_techno",
               "contenu_riche",
               "contenu_adapte",
               "appreciation_generale_commentaires",
               "engagement_formateurs",
               "engagement_formateurs_commentaires",
               "liens_formation_pratique",
               "liens_formation_pratique_commentaire",
               "interet_pratiques",
               "interet_algorithmes",
               "interet_partages",
               "interet_scratch",
               "interet_activites_commentaires",
               "utilite_pratiques",
               "utilite_algorithmes",
               "utilite_partages",
               "utilite_scratch",
               "utilite_activites_commentaires",
               "confiance_pratiques",
               "confiance_algorithmes",
               "confiance_partages",
               "confiance_scratch",
               "confiance_activites_commentaires",
               "competence1",
               "competence2",
               "acquisition_pratiques_commentaires",
               "competence3",
               "competence4",
               "competence5",
               "acquisition_algorithmes_commentaires",
               "competence6",
               "competence7",
               "acquisition_partage_commentaires",
               "utiliser_nouveaux_apprentissages",
               "intention_pratiques",
               "intention_algorithmes",
               "intention_partages",
               "intention_scratch",
               "intention_utilisation_commentaires",
               "adoption_charte",
               "adoption_livre",
               "adoption_machine",
               "adoption_jeu",
               "adoption_orchestration",
               "adoption_bestioles",
               "adoption_thymio",
               "adoption_bluebot",
               "adoption_edunum",
               "adoption_utiliser_application",
               "adoption_chope_pub",
               "adoption_ecrans",
               "adoption_tapis",
               "adoption_stopmotion",
               "adoption_album_loupe",
               "adoption_album_livre",
               "adoption_album_pfff",
               "adoption_pasconcerne",
               "conditions_compatibilite",
               "conditions_soutien",
               "conditions_charge",
               "conditions_plusvalue")
  
  # Réponses 
  
  table <- NULL
  cycles <- rep(0, 12)
  activites <-  rep(0,18)
  n_batch <- NULL
  
  for (n_batch in 1:length(content_responses)) {
    
    vec <- 1:length(content_responses[[n_batch]]$data)
    
    for (i in vec) {
      
      answers <- c(
        
        "J4",
        
        tryCatch(
        content_responses[[n_batch]]$data[[i]]$date_created,
        error = function(e){return(NA)}
      ),
      
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$custom_variables$EF,
        error = function(e){return(NA)}
      ),
      
      #Q22 Age
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[13]]$questions[[2]]$answers[[1]]$simple_text,
        error = function(e){return(NA)}
      ),
      
      #Q23 Expérience
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[13]]$questions[[3]]$answers[[1]]$simple_text,
        error = function(e){return(NA)}
      ),
      
      #Q2 Etablissement
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[2]]$questions[[2]]$answers[[1]]$simple_text,
        error = function(e){return(NA)}
      ),
      
      #Q21 Degrés
      
      if("try-error" %in% class(try(length(content_responses[[n_batch]]$data[[i]]$pages[[13]]$questions[[1]]$answers)))) {
        
        cycles <- rep(0,12)
        
      } 
      
      else {
        
        for (j in 1:length(content_responses[[n_batch]]$data[[i]]$pages[[13]]$questions[[1]]$answers)){
          
          n <- as.numeric(gsub("([0-9]+).*$", "\\1", content_responses[[n_batch]]$data[[i]]$pages[[13]]$questions[[1]]$answers[[j]]$simple_text))
          
          cycles[n] <- cycles[n] + 1

        }
        
        cycles
        
      },
      
      #Q3 Motivation
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[3]]$questions[[1]]$answers[[1]]$simple_text,
        error = function(e){return(NA)}
      ),
      
      #Q4 Utilité techno
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[3]]$questions[[2]]$answers[[1]]$simple_text,
        error = function(e){return(NA)}
      ),
      
      #Q5 Contenu riche
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[4]]$questions[[1]]$answers[[1]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q5 Contenu adapté
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[4]]$questions[[1]]$answers[[2]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q5 Appreciation générale commentaires
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[4]]$questions[[1]]$answers[[3]]$text,
        error = function(e){return(NA)}
      ),
      
      #Q6 Engagement formateurs
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[5]]$questions[[1]]$answers[[1]]$simple_text,
        error = function(e){return(NA)}
      ),
      
      #Q6 Engagement formateurs commentaires
    
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[5]]$questions[[1]]$answers[[2]]$text,
        error = function(e){return(NA)}
      ),
      
      #Q7 Liens formation pratique
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[5]]$questions[[2]]$answers[[1]]$simple_text,
        error = function(e){return(NA)}
      ),
      
      #Q7 Liens formation pratique commentaires
      
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[5]]$questions[[2]]$answers[[2]]$text,
        error = function(e){return(NA)}
      ),
      
      #Q8 Plaisir pratiques numériques
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[6]]$questions[[1]]$answers[[1]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q8 Plaisir algorithmes divers
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[6]]$questions[[1]]$answers[[2]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q8 Plaisir partages et droits
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[6]]$questions[[1]]$answers[[3]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q8 Plaisir scratch junior
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[6]]$questions[[1]]$answers[[4]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q8 Intéret activités commentaire
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[6]]$questions[[1]]$answers[[5]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q9 Utilite pratiques numeriques
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[7]]$questions[[1]]$answers[[1]]$simple_text),
        error = function(e){return(NA)}
      ),
      #Q9 Utilite algorithmes divers
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[7]]$questions[[1]]$answers[[2]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q9 Utilite partages et droits
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[7]]$questions[[1]]$answers[[3]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q9 Utilite scratch junior
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[7]]$questions[[1]]$answers[[4]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q9 Utilite activites commentaires
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[7]]$questions[[1]]$answers[[5]]$text,
        error = function(e){return(NA)}
      ),
      
      #Q10 Confiance pratiques numeriques
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[8]]$questions[[1]]$answers[[1]]$simple_text),
        error = function(e){return(NA)}
      ),
      #Q10 Confiance algorithmes divers
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[8]]$questions[[1]]$answers[[2]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q10 Confiance partages et droits
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[8]]$questions[[1]]$answers[[3]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q10 Confiance scratch junior
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[8]]$questions[[1]]$answers[[4]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q10 Confiance activites commentaires
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[8]]$questions[[1]]$answers[[5]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q11 Competence 1 pratiques numériques
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[9]]$questions[[1]]$answers[[1]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q11 Competence 2 pratiques numériques
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[9]]$questions[[1]]$answers[[2]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q11 Acquisition pratiques commentaires
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[9]]$questions[[1]]$answers[[3]]$text,
        error = function(e){return(NA)}
      ),
      
      #Q12 Competence 3 Algortithmes divers
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[9]]$questions[[2]]$answers[[1]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q12 Competence 4 Algortithmes divers
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[9]]$questions[[2]]$answers[[2]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q12 Competence 5 Algortithmes divers
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[9]]$questions[[2]]$answers[[3]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q12 Acquisition Algortithmes divers commentaire
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[9]]$questions[[2]]$answers[[4]]$text,
        error = function(e){return(NA)}
      ),
      
      #Q13 Competence 6 Partage
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[9]]$questions[[3]]$answers[[1]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q13 Competence 7 Partage
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[9]]$questions[[3]]$answers[[2]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q13 Acquisition Partage Commentaire
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[9]]$questions[[3]]$answers[[3]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      
      #Q14 Utiliser nouveaux apprentissages
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[10]]$questions[[1]]$answers[[1]]$simple_text,
        error = function(e){return(NA)}
      ),
      
      #Q15 Intention pratiques
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[10]]$questions[[2]]$answers[[1]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q15 Intention algorithmes
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[10]]$questions[[2]]$answers[[2]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q15 Intention partage
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[10]]$questions[[2]]$answers[[3]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q15 Intention scratch
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[10]]$questions[[2]]$answers[[4]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q15 Intention utilisation commentaires
      tryCatch(
        gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[10]]$questions[[2]]$answers[[5]]$simple_text),
        error = function(e){return(NA)}
      ),
      
      #Q16 Adoption
      
      if("try-error" %in% class(try(length(content_responses[[n_batch]]$data[[i]]$pages[[11]]$questions[[1]]$answers)))) {
        
        activites <- rep(0,18)
        
      } 
      
      else {
        
        for (j in 1:length(content_responses[[n_batch]]$data[[i]]$pages[[11]]$questions[[1]]$answers)){
          
          n <- content_responses[[n_batch]]$data[[i]]$pages[[11]]$questions[[1]]$answers[[j]]$simple_text
          n <- case_when(n == "La Charte (vu en J1)" ~ 1,
                         n == "BookCreator - le livre multimédia (vu en J1)" ~ 2,
                         n == "La Machine à tri (vu en J1)" ~ 3,
                         n == "Le Jeu du robot (vu en J1)" ~ 4,
                         n == "Orchestration et gestion des IPads de la classe (vu en J1)" ~ 5,
                         n == "Les bestioles (vu en J1)" ~ 6,
                         n == "Le robot Thymio (vu en J2)" ~ 7,
                         n == "Le robot Bluebot (vu en J2)" ~ 8,
                         n == "Edunum et différenciation (vu en J2)" ~ 9,
                         n == "Utiliser des applications numériques disciplinaires (vu en J3)" ~ 10,
                         n == "Chope la pub (vu en J3)" ~ 11,
                         n == "Où sont les écrans - poster de la ville ? (vu en J3)" ~ 12,
                         n == "Le tapis des écrans (vu en J2)" ~ 13,
                         n == "Stop Motion - film d’animation (vu en J3)" ~ 14,
                         n == "Album «Loupé» (vu en J3)" ~ 15,
                         n == "Album «C’est un livre» (vu en J3)" ~ 16,
                         n == "Album «Pfff» (vu en J2)" ~ 17,
                         n == "Pas concerné" ~ 18)
          
          activites[n] <- activites[n] + 1
          
        }
        
        activites
        
      },
      
      #Q17 Conditions compatibilite
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[12]]$questions[[1]]$answers[[1]]$simple_text,
        error = function(e){return(NA)}
      ),
      
      #Q18 Conditions soutien
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[12]]$questions[[2]]$answers[[1]]$simple_text,
        error = function(e){return(NA)}
      ),
      
      #Q19 Conditions charge
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[12]]$questions[[3]]$answers[[1]]$simple_text,
        error = function(e){return(NA)}
      ),
      
      #Q20 Conditions plusvalue
      tryCatch(
        content_responses[[n_batch]]$data[[i]]$pages[[12]]$questions[[4]]$answers[[1]]$simple_text,
        error = function(e){return(NA)}
      ))
      
      cycles <- rep(0, 12)
      activites <- rep(0,18)
      table <- rbind(table,answers)
      
    }
    
  }
  
  new_data <- as.data.frame(table)
  colnames(new_data) <- columns
  new_data$date <- substring(new_data$date,0,10)
  data <- rbind(data,new_data)
  rownames(data) <- 1:nrow(data)
  
  # Store updated data
  
  saveRDS(data, file = "data_ef_d1.rds")
  
} else {
  
  cat("Pas de nouvelles données trouvées sur SurveyMonkey")
}

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

agreement <- c("Pas du tout d'accord",
               "Pas d'accord",
               "Plutôt pas d'accord",
               "Ni en accord ni en désaccord",
               "Plutôt d'accord",
               "D'accord",
               "Tout à fait d'accord")

for (i in colnames(data)){
  if (any(agreement %in% unique(data[,i]))){
    data[,i] <- mapping_agreement[data[,i]]
  }
}

################################# DASHBOARD #################################

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
                                         textInput("id", label = h3("Entrez votre identifiant EF :"), value = ""),
                                         br())
                                ),
                                
                                fluidRow(
                                  
                                  column(12, align="center",
                                         div(style = "display:inline-block; float:center", selectInput(inputId="journee",label = "Journée :",choices = c("J4"),width=120)),
                                         div(style = "display:inline-block; float:center", selectInput(inputId="session",label = "Session :",choices = unique(data$date),width=120)),
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
          
          
          Freq <- c(sum(as.numeric(selection$"degre_1P")),
                    sum(as.numeric(selection$"degre_2P")),
                    sum(as.numeric(selection$"degre_3P")),
                    sum(as.numeric(selection$"degre_4P")),
                    sum(as.numeric(selection$"degre_5P")),
                    sum(as.numeric(selection$"degre_6P")),
                    sum(as.numeric(selection$"degre_7P")),
                    sum(as.numeric(selection$"degre_8P")),
                    sum(as.numeric(selection$"degre_9S")),
                    sum(as.numeric(selection$"degre_10S")),
                    sum(as.numeric(selection$"degre_11S")),
                    sum(as.numeric(selection$"degre_12S")))
          
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
          
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("engagement_formateurs_commentaires")]))))
          
        }
        
        if (input$commentaires == "Lien entre formation et pratique - Commentaires") {
          
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("liens_formation_pratique_commentaire")]))))
          
        }
        
        if (input$commentaires == "Intérêt des activités - Commentaires") {
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("interet_activites_commentaires")]))))
          
        }
        
        if (input$commentaires == "Utilité des activités - Commentaires") {
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("utilite_activites_commentaires")]))))
          
        }
        
        if (input$commentaires == "Auto-efficacité - Commentaires") {
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("confiance_activites_commentaires")]))))
          
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
          
          
          return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("intention_utilisation_commentaires")]))))
          
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
          
          data$compatibilite <- mapping_agreement[data$conditions_compatibilite]
          
          p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=conditions_compatibilite)) +
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
          
          
          data$plusvalue <- mapping_agreement[data$conditions_plusvalue]
          
          p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=conditions_plusvalue)) +
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
          
          data$soutien <- mapping_agreement[data$conditions_soutien]
          
          p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=conditions_soutien)) +
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
          
          data$charge <- mapping_agreement[data$conditions_charge]
          
          p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=conditions_charge)) +
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
               "Album «Pfff» (j2)",
               "Pas concerné")
        
        selection <- data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("adoption_charte",
                                                                                                          "adoption_livre",
                                                                                                          "adoption_machine",
                                                                                                          "adoption_jeu",
                                                                                                          "adoption_orchestration",
                                                                                                          "adoption_bestioles",
                                                                                                          "adoption_thymio",
                                                                                                          "adoption_bluebot",
                                                                                                          "adoption_edunum",
                                                                                                          "adoption_utiliser_application",
                                                                                                          "adoption_chope_pub",
                                                                                                          "adoption_ecrans",
                                                                                                          "adoption_tapis",
                                                                                                          "adoption_stopmotion",
                                                                                                          "adoption_album_loupe",
                                                                                                          "adoption_album_livre",
                                                                                                          "adoption_album_pfff",
                                                                                                          "adoption_pasconcerne")]
        
        Freq <- c(sum(as.numeric(selection$"adoption_charte")),
                  sum(as.numeric(selection$"adoption_livre")),
                  sum(as.numeric(selection$"adoption_machine")),
                  sum(as.numeric(selection$"adoption_jeu")),
                  sum(as.numeric(selection$"adoption_orchestration")),
                  sum(as.numeric(selection$"adoption_bestioles")),
                  sum(as.numeric(selection$"adoption_thymio")),
                  sum(as.numeric(selection$"adoption_bluebot")),
                  sum(as.numeric(selection$"adoption_edunum")),
                  sum(as.numeric(selection$"adoption_utiliser_application")),
                  sum(as.numeric(selection$"adoption_chope_pub")),
                  sum(as.numeric(selection$"adoption_ecrans")),
                  sum(as.numeric(selection$"adoption_tapis")),
                  sum(as.numeric(selection$"adoption_stopmotion")),
                  sum(as.numeric(selection$"adoption_album_loupe")),
                  sum(as.numeric(selection$"adoption_album_livre")),
                  sum(as.numeric(selection$"adoption_album_pfff")),
                  sum(as.numeric(selection$"adoption_pasconcerne")))
        
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
