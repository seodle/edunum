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


sm_api_key = '' # Indicate the API key SurveyMonkey
sm_secret = '' # Indicate the secret code SurveyMonkey
sm_client_id = '' # Indicate the cient code SurveyMonkey

survey_id <- "" # Indicate the survey ID

details <- paste0("https://api.surveymonkey.com/v3/surveys/",survey_id)
content_details <- content(GET(details,add_headers(Authorization = paste0("Bearer ", sm_api_key))),"parsed")

collector <- "" # Indicate the collector ID
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

if (file.exists("data_ef_d2.rds")){
  
  data <- readRDS(file = "data_ef_d2.rds")
  
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
  
  columns <- c("date",
               "journee",
               "id_binome",
               "accord_participation",
               "etablissement",
               "motivation",
               "utilite_numerique",
               "engagement_formateurs",
               "engagement_formateurs_commentaire",
               "lien_formation_travail",
               "lien_formation_travail_commentaire",
               "interet_gestes_de_base",
               "interet_SAMR",
               "interet_charte",
               "interet_livre",
               "interet_machine",
               "interet_jeu",
               "interet_concepts",
               "interet_commentaire",
               "utile_gestes_de_base",
               "utile_SAMR",
               "utile_charte",
               "utile_livre",
               "utile_machine",
               "utile_jeu",
               "utile_concepts",
               "utile_commentaire",
               "confiance_gestes_de_base",
               "confiance_SAMR",
               "confiance_charte",
               "confiance_livre",
               "confiance_machine",
               "confiance_jeu",
               "confiance_concepts",
               "confiance_commentaire",
               "competences_techniques",
               "competences_techniques_commentaire",
               "selection_technologie",
               "selection_technologie_commentaire",
               "entousiaste_apprentissage",
               "intention_gestes_de_base",
               "intention_charte",
               "intention_livre",
               "intention_machine",
               "intention_jeu",
               "intention_commentaire",
               "ressources_disponibles",
               "lien_contenus_pratique",
               "interet_direction",
               "encouragement_collegues",
               "soutien",
               "savoir_aide",
               "charge_travail",
               "classe_1P",
               "classe_2P",
               "classe_3P",
               "classe_4P",
               "classe_5P",
               "classe_6P",
               "classe_7P",
               "classe_8P",
               "classe_9S",
               "classe_10S",
               "classe_11S",
               "classe_12S",
               "age",
               "experience_enseignement")
  
  # Réponses 
  
  table <- NULL
  cycles <- rep(0, 12)
  n_batch <- NULL
  
  for (n_batch in 1:length(content_responses)) {
    
    vec <- 1:length(content_responses[[n_batch]]$data)
  
    for (i in vec) {
  
      answers <- c(tryCatch(
                      content_responses[[n_batch]]$data[[i]]$date_created,
                      error = function(e){return(NA)}
                    ),
    
                    "J1",
    
                   tryCatch(
                     content_responses[[n_batch]]$data[[i]]$custom_variables$EF,
                     error = function(e){return(NA)}
                   ),
    
                    #Q1
                    tryCatch(
                      content_responses[[n_batch]]$data[[i]]$pages[[2]]$questions[[1]]$answers[[1]]$simple_text,
                      error = function(e){return(NA)}
                    ),
                   
                    #Q2
                    tryCatch(
                      content_responses[[n_batch]]$data[[i]]$pages[[2]]$questions[[2]]$answers[[1]]$simple_text,
                      error = function(e){return(NA)}
                    ),
                   
                    #Q3
                    tryCatch(
                      content_responses[[n_batch]]$data[[i]]$pages[[3]]$questions[[1]]$answers[[1]]$simple_text,
                      error = function(e){return(NA)}
                    ),
                   
                    #Q4
                    tryCatch(
                      content_responses[[n_batch]]$data[[i]]$pages[[3]]$questions[[2]]$answers[[1]]$simple_text,
                      error = function(e){return(NA)}
                    ),
                    #Q5
                    tryCatch(
                      content_responses[[n_batch]]$data[[i]]$pages[[4]]$questions[[1]]$answers[[1]]$simple_text,
                      error = function(e){return(NA)}
                    ),
                    #Q5 commentaire
                    tryCatch(
                      content_responses[[n_batch]]$data[[i]]$pages[[4]]$questions[[1]]$answers[[2]]$text,
                      error = function(e){return(NA)}
                    ),
                    #Q6
                   tryCatch(
                    content_responses[[n_batch]]$data[[i]]$pages[[4]]$questions[[2]]$answers[[1]]$simple_text,
                    error = function(e){return(NA)}
                   ),
                    #Q6 commentaire
                    tryCatch(
                      content_responses[[n_batch]]$data[[i]]$pages[[4]]$questions[[2]]$answers[[2]]$text,
                      error = function(e){return(NA)}
                    ),
                    #Q7 Gestes de bases
                   tryCatch(
                    gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[5]]$questions[[1]]$answers[[1]]$simple_text),
                    error = function(e){return(NA)}
                   ),
                    #Q7 SAMR
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[5]]$questions[[1]]$answers[[2]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q7 Charte
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[5]]$questions[[1]]$answers[[3]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q7 Livre multimédia
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[5]]$questions[[1]]$answers[[4]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q7 Machine à tri
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[5]]$questions[[1]]$answers[[5]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q7 Jeu du robot
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[5]]$questions[[1]]$answers[[6]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q7 Concepts de bases en SI
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[5]]$questions[[1]]$answers[[7]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q7 commentaire
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[5]]$questions[[1]]$answers[[8]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q8 Gestes de bases
                   tryCatch(
                     gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[6]]$questions[[1]]$answers[[1]]$simple_text),
                     error = function(e){return(NA)}
                   ),
                    #Q8 SAMR
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[6]]$questions[[1]]$answers[[2]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q8 Charte
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[6]]$questions[[1]]$answers[[3]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q8 Livre multimédia
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[6]]$questions[[1]]$answers[[4]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q8 Machine à tri
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[6]]$questions[[1]]$answers[[5]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q8 Jeu du robot
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[6]]$questions[[1]]$answers[[6]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q8 Concepts de bases en SI
                    tryCatch(
                    gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[6]]$questions[[1]]$answers[[7]]$simple_text),
                    error = function(e){return(NA)}
                    ),
                    #Q8 commentaire
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[6]]$questions[[1]]$answers[[8]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q9 Gestes de bases
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[7]]$questions[[1]]$answers[[1]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q9 SAMR
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[7]]$questions[[1]]$answers[[2]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q9 Charte
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[7]]$questions[[1]]$answers[[3]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q9 Livre multimédia
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[7]]$questions[[1]]$answers[[4]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q9 Machine à tri
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[7]]$questions[[1]]$answers[[5]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q9 Jeu du robot
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[7]]$questions[[1]]$answers[[6]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q9 Concepts de bases en SI
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[7]]$questions[[1]]$answers[[7]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q9 commentaire
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[7]]$questions[[1]]$answers[[8]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q10
                   tryCatch(
                    content_responses[[n_batch]]$data[[i]]$pages[[8]]$questions[[1]]$answers[[1]]$simple_text,
                    error = function(e){return(NA)}
                   ),
                    #Q10 commentaire
                    tryCatch(
                      content_responses[[n_batch]]$data[[i]]$pages[[8]]$questions[[1]]$answers[[2]]$text,
                      error = function(e){return(NA)}
                    ),
                    #Q11
                    tryCatch(
                    content_responses[[n_batch]]$data[[i]]$pages[[8]]$questions[[2]]$answers[[1]]$simple_text,
                    error = function(e){return(NA)}
                    ),
                    #Q11 commentaire
                    tryCatch(
                      content_responses[[n_batch]]$data[[i]]$pages[[8]]$questions[[2]]$answers[[2]]$text,
                      error = function(e){return(NA)}
                    ),
                    #Q12
                    tryCatch(
                    content_responses[[n_batch]]$data[[i]]$pages[[9]]$questions[[1]]$answers[[1]]$simple_text,
                    error = function(e){return(NA)}
                    ),
                    #Q13 Gestes de bases
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[9]]$questions[[2]]$answers[[1]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q13 Charte
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[9]]$questions[[2]]$answers[[2]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q13 Livre multimédia
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[9]]$questions[[2]]$answers[[3]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q13 Machine à tri
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[9]]$questions[[2]]$answers[[4]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q13 Jeu du robot
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[9]]$questions[[2]]$answers[[5]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q13 commentaire
                    tryCatch(
                      gsub(".*\\| ","",content_responses[[n_batch]]$data[[i]]$pages[[9]]$questions[[2]]$answers[[6]]$simple_text),
                      error = function(e){return(NA)}
                    ),
                    #Q14
                    tryCatch(
                      content_responses[[n_batch]]$data[[i]]$pages[[10]]$questions[[1]]$answers[[1]]$simple_text,
                      error = function(e){return(NA)}
                    ),
                    #Q15
                    tryCatch(
                      content_responses[[n_batch]]$data[[i]]$pages[[10]]$questions[[2]]$answers[[1]]$simple_text,
                      error = function(e){return(NA)}
                    ),
                    #Q16
                    tryCatch(
                      content_responses[[n_batch]]$data[[i]]$pages[[10]]$questions[[3]]$answers[[1]]$simple_text,
                      error = function(e){return(NA)}
                    ),
                    #Q17
                    tryCatch(
                      content_responses[[n_batch]]$data[[i]]$pages[[10]]$questions[[4]]$answers[[1]]$simple_text,
                      error = function(e){return(NA)}
                    ),
                    #Q18
                    tryCatch(
                      content_responses[[n_batch]]$data[[i]]$pages[[10]]$questions[[5]]$answers[[1]]$simple_text,
                      error = function(e){return(NA)}
                    ),
                    #Q19
                    tryCatch(
                      content_responses[[n_batch]]$data[[i]]$pages[[10]]$questions[[6]]$answers[[1]]$simple_text,
                      error = function(e){return(NA)}
                    ),
                    #Q20
                    tryCatch(
                      content_responses[[n_batch]]$data[[i]]$pages[[10]]$questions[[7]]$answers[[1]]$simple_text,
                      error = function(e){return(NA)}
                    ),
    
                    #Q21
                   
                   if("try-error" %in% class(try(length(content_responses[[n_batch]]$data[[i]]$pages[[11]]$questions[[1]]$answers)))) {
                     
                     
                     cycles <- rep(0,12)
                   
                   } 
                   
                   else {
                     
                     for (j in 1:length(content_responses[[n_batch]]$data[[i]]$pages[[11]]$questions[[1]]$answers)){
                       
                       n <- as.numeric(gsub("([0-9]+).*$", "\\1", content_responses[[n_batch]]$data[[i]]$pages[[11]]$questions[[1]]$answers[[j]]$simple_text))
                       
                       cycles[n] <- cycles[n] + 1
                       
                     }
                     
                     cycles
                     
                     },
                     
            
                    #Q22
                    tryCatch(
                    content_responses[[n_batch]]$data[[i]]$pages[[11]]$questions[[2]]$answers[[1]]$simple_text,
                    error = function(e){return(NA)}
                    ),
                   
                    #Q23
                    tryCatch(
                    content_responses[[n_batch]]$data[[i]]$pages[[11]]$questions[[3]]$answers[[1]]$simple_text,
                    error = function(e){return(NA)}
                    ))
    
      cycles <- rep(0, 12)
      table <- rbind(table,answers)
      
    }
  
  }
  
  new_data <- as.data.frame(table)
  colnames(new_data) <- columns
  new_data$date <- substring(new_data$date,0,10)
  data <- rbind(data,new_data)
  rownames(data) <- 1:nrow(data)
  

  # Store updated data
  
  saveRDS(data, file = "data_ef_d2.rds")

} else {
  
  cat("Pas de nouvelles données enregistrées")
}
  
mapping_agreement <- c("Pas du tout d'accord"=1,
                       "Pas d'accord"=2,
                       "Plutôt pas d'accord"=3,
                       "Ni en accord ni en désaccord"=4,
                       "Plutôt d'accord"=5,
                       "D'accord"=6,
                       "Tout à fait d'accord"=7)

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

#####################################################

ui <- dashboardPage(title="Formation EF - Retours enseignants",
  
  dashboardHeader(title = (span("Formation EF - Retours enseignants",
                                style = "font-size: 28px")),
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
                       br())
              ),
              
              fluidRow(
                
                column(12, align="center",
                       div(style = "display:inline-block; float:center", selectInput(inputId="journee",label = "Journée :",choices = c(unique(data$journee)),width=120)),
                       div(style = "display:inline-block; float:center", selectInput(inputId="session",label = "Session :",choices = rev(c(unique(data$date))),width=120)),
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
                           title = "Equipe de formation",
                           selectInput(inputId = "formateurs",label = "",choices = c("Engagement des formateurs", "Lien entre formation et pratique"),width=300),
                           plotOutput("formateursPlot"))
                ),
                
                column(3,
                       box(width=NULL,height = '530px',style='width:400;overflow-x: scroll;height:488px;overflow-y: scroll;', solidHeader = TRUE, status = "primary",
                           title = "Commentaires des enseignant.es",
                           selectInput(inputId = "commentaires",label = "",choices = c("Engagement des formateurs - Commentaires",
                                                                                       "Lien entre formation et pratique - Commentaires",
                                                                                       "Intérêt des activités - Commentaires",
                                                                                       "Utilité des activités - Commentaires",
                                                                                       "Auto-efficacité - Commentaires",
                                                                                       "Intention d'utilisation - Commentaires",
                                                                                       "Compétences techiniques - Commentaires",
                                                                                       "Compétences techno-pédagogiques - Commentaires"),width=350),
                           DT::dataTableOutput("commentairesTable"))
                       
                )),
              
              
              fluidRow(
                
                column(4,
                       
                       box(width=NULL,height = '530px', solidHeader = TRUE, status = "primary",
                           title = "Réactions post-journée de formation",
                           selectInput(inputId = "reactionsImmediates",label = "",choices = c("Intérêt pour les ateliers",
                                                                                              "Utilité perçue des ateliers",
                                                                                              "Auto-efficacité vis-à-vis des ressources",
                                                                                              "Intention d'utiliser les ressources"),width=330),
                           plotOutput("reactionsPlot"))
                       
                ),
                
                column(4,
                       
                       box(width=NULL,height = '530px', solidHeader = TRUE, status = "primary",
                           title = "Acquisition des compétences",
                           selectInput(inputId = "competences",label = "",choices = c("Compétences techniques",
                                                                                      "Compétences techno-pédagogiques"),width=330),
                           
                           plotOutput("competencesPlot"))
                       
                ),
                
                column(4,
                       
                       box(width=NULL,height = '530px', solidHeader = TRUE, status = "primary",
                           title = "Conditions facilitantes",
                           selectInput(inputId = "conditionsFacilitantes",label = "",choices = c("Matériel",
                                                                                                 "Compatibilité avec les pratiques habituelles",
                                                                                                 "Soutien hiérarchique",
                                                                                                 "Soutien des collègues",
                                                                                                 "Soutien technique et pédagogique",
                                                                                                 "Aide disponible",
                                                                                                 "Charge de travail"),width=330),
                           
                           plotOutput("conditionsPlot"))
                       
                )
                
                
              ))
    )
    
  )
)


server <- function(input, output) {
  
  
  observe({
    
    if (input$id %in% c("4t0ugjo",
                        "amrl926",
                        "1ox2g53",
                        "5ve4wgf",
                        "9lp0h28",
                        "va75qgr",
                        "031oftw",
                        "jv3kmdz",
                        "g6l4gur",
                        "tbu0m9c",
                        "9kdg8oi",
                        "0c0m5dc")) {
      
      output$demoPlot <- renderPlot({
        
        
        if (input$demographie == "Age") {
          
            p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=as.numeric(age))) +
              geom_boxplot(fill='#F59138', color="black", alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              labs(title = "                 Répartition de l'âge des enseignant.e.s") +
              theme_minimal() +
              theme(plot.title = element_text(face = "italic", size = 10),
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
            
            p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=as.numeric(experience_enseignement)))  +
              geom_boxplot(fill='#F59138', color="black", alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              labs(title = "                    Répartition du nombre d'années d'expérience") +
              theme_minimal() +
              theme(plot.title = element_text(face = "italic", size = 10),
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
              geom_bar(stat = "identity",fill='#F59138', alpha=0.7,outlier.shape = NA) +
              geom_label(aes(label = Freq, vjust = 1.5, size=5)) +
              theme_minimal() +
              scale_x_discrete(labels=label_wrap(15)) +
              labs(title = "                  Répartition des enseignant.e.s par établissement") +
              theme(plot.title = element_text(face = "italic", size = 10),
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
            
            selection <- data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("classe_1P","classe_2P","classe_3P","classe_4P","classe_5P","classe_6P","classe_7P","classe_8P","classe_9S","classe_10S","classe_11S","classe_12S")]
            
            Freq <- c(sum(as.numeric(selection$classe_1P)),
                      sum(as.numeric(selection$classe_2P)),
                      sum(as.numeric(selection$classe_3P)),
                      sum(as.numeric(selection$classe_4P)),
                      sum(as.numeric(selection$classe_5P)),
                      sum(as.numeric(selection$classe_6P)),
                      sum(as.numeric(selection$classe_7P)),
                      sum(as.numeric(selection$classe_8P)),
                      sum(as.numeric(selection$classe_9S)),
                      sum(as.numeric(selection$classe_10S)),
                      sum(as.numeric(selection$classe_11S)),
                      sum(as.numeric(selection$classe_12S)))
            
            data_plot <- data.frame(Var1 = gsub(".*\\classe_","",colnames(selection)), Freq = Freq)
            data_plot <- data_plot[!data_plot$Freq==0,]
            
            
            p <- ggplot(data_plot, aes(x=Var1, y=Freq)) +
              geom_bar(stat = "identity",fill='#F59138', alpha=0.7,outlier.shape = NA) +
              geom_label(aes(label = Freq, vjust = 1.5, size=5)) +
              theme_minimal() +
              scale_x_discrete(labels=label_wrap(15)) +
              labs(title = "                            Répartition des enseignant.e.s par degré") +
              theme(plot.title = element_text(face = "italic", size = 10),
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
            
            p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=motivation)) +
              geom_boxplot(fill='#AD5B8F', color="black", alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              labs(title = "Quand je participe à une formation continue, \nj'essaie d'apprendre le plus possible") +
              scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                            "Pas \nd'accord",
                                            "Plutôt pas \nd'accord",
                                            "Ni en accord \nni en désaccord",
                                            "Plutôt \nd'accord",
                                            "D'accord",
                                            "Tout à fait \nd'accord"), breaks = 1:7) +
              theme_minimal() +
              theme(plot.title = element_text(face = "italic", size = 10),
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
            
            p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=utilite_numerique)) +
              geom_boxplot(fill='#AD5B8F', color="black", alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              labs(title = "Je pense que les technologies numériques sont \nutiles pour l'apprentissage") +
              scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                            "Pas \nd'accord",
                                            "Plutôt pas \nd'accord",
                                            "Ni en accord \nni en désaccord",
                                            "Plutôt \nd'accord",
                                            "D'accord",
                                            "Tout à fait \nd'accord"), breaks = 1:7) +
              theme_minimal() +   
              theme(plot.title = element_text(face = "italic", size = 10),
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
        
        if (input$formateurs == "Engagement des formateurs") {
          
            n <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=engagement_formateurs)) +
              geom_boxplot(fill='#D2CFC8', color="black", alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              labs(title = "J'ai apprécié l'engagement des format.eur.rice.s") +
              scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                            "Pas \nd'accord",
                                            "Plutôt pas \nd'accord",
                                            "Ni en accord \nni en désaccord",
                                            "Plutôt \nd'accord",
                                            "D'accord",
                                            "Tout à fait \nd'accord"), breaks = 1:7) +
              theme_minimal() +
              theme(plot.title = element_text(face = "italic", size = 10),
                    legend.position = "none",
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    axis.text.x = element_blank(),
                    axis.text.y = element_text(size=12),
                    #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                    #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
            
            return(n)
    
        }
        
        if (input$formateurs == "Lien entre formation et pratique") {
            
            n <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=lien_formation_travail)) +
              geom_boxplot(fill='#D2CFC8', color="black", alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              theme_minimal() +
              labs(title = "Les activités et les exercices utilisés par les \nformat.eur.rice.s me permettent \nde voir comment appliquer \nce que j'ai appris dans mon travail") +
              scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                            "Pas \nd'accord",
                                            "Plutôt pas \nd'accord",
                                            "Ni en accord \nni en désaccord",
                                            "Plutôt \nd'accord",
                                            "D'accord",
                                            "Tout à fait \nd'accord"), breaks = 1:7) +
              theme(plot.title = element_text(face = "italic", size = 10),
                    legend.position = "none",
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    axis.text.x = element_blank(),
                    axis.text.y = element_text(size=12),
                    #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                    #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
            
            return(n)
            
          }
        
      })
      
      output$commentairesTable <- renderDataTable({
        
        if (input$commentaires == "Engagement des formateurs - Commentaires") {
            
            return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("engagement_formateurs_commentaire")]))))
            
          }
        
        if (input$commentaires == "Lien entre formation et pratique - Commentaires") {
            
            return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("lien_formation_travail_commentaire")]))))
            
          }
        
        if (input$commentaires == "Intérêt des activités - Commentaires") {
            
            return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("interet_commentaire")]))))
            
        }
        
        if (input$commentaires == "Utilité des activités - Commentaires") {
            
            return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("utile_commentaire")]))))
  
        }
        
        if (input$commentaires == "Auto-efficacité - Commentaires") {
            
            return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("confiance_commentaire")]))))

        }
        
        if (input$commentaires == "Intention d'utilisation - Commentaires") {
          
            
            return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("intention_commentaire")]))))
            
        }
        
        if (input$commentaires == "Compétences techiniques - Commentaires") {
            
            return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("competences_techniques_commentaire")]))))
            
        }
        
        if (input$commentaires == "Compétences techno-pédagogiques - Commentaires") {
          
            
            return(datatable(rownames = NULL, colnames = NULL,options = list(dom = 't'),na.omit(data.frame(data=data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("selection_technologie_commentaire")]))))
            
        }
        
      })
      
      # Réactions immédiates
      
      output$reactionsPlot <- renderPlot({
        
        if (input$reactionsImmediates == "Intérêt pour les ateliers") {
            
            selection <- data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("interet_gestes_de_base",
                                                                                     "interet_SAMR",
                                                                                     "interet_charte",
                                                                                     "interet_livre",
                                                                                     "interet_machine",
                                                                                     "interet_jeu",
                                                                                     "interet_concepts")]
            
            table <- data.frame(values = c(selection[,1],selection[,2],selection[,3],selection[,4],selection[,5],selection[,6],selection[,7]),
                                activites = c(rep("Gestes de base",length(selection[,1])),
                                              rep("SAMR",length(selection[,2])),
                                              rep("Charte",length(selection[,3])),
                                              rep("Livre multimédia",length(selection[,4])),
                                              rep("Machine à tri",length(selection[,5])),
                                              rep("Jeu du robot",length(selection[,6])),
                                              rep("Concepts SI",length(selection[,7]))))
            
            
            
            p <- ggplot(table[complete.cases(table), ], aes(x=activites, y=values)) +
              geom_boxplot(fill='#FADE7D', color="black", alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              theme_minimal() +
              labs(title = "J'ai eu beaucoup de plaisir à suivre les ateliers suivants") +
              scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                            "Pas \nd'accord",
                                            "Plutôt pas \nd'accord",
                                            "Ni en accord \nni en désaccord",
                                            "Plutôt \nd'accord",
                                            "D'accord",
                                            "Tout à fait \nd'accord"), breaks = 1:7) +
              theme(plot.title = element_text(face = "italic", size = 10),
                    legend.position = "none",
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    axis.text.x = element_text(angle = 70, vjust = 0.5, hjust = 0.5,size=11),
                    axis.text.y = element_text(size=12),
                    #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                    #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
            
            return(p)
            
        }
        
        if (input$reactionsImmediates == "Utilité perçue des ateliers") {
            
            selection <- data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("utile_gestes_de_base",
                                                                                     "utile_SAMR",
                                                                                     "utile_charte",
                                                                                     "utile_livre",
                                                                                     "utile_machine",
                                                                                     "utile_jeu",
                                                                                     "utile_concepts")]
            
            table <- data.frame(values = c(selection[,1],selection[,2],selection[,3],selection[,4],selection[,5],selection[,6],selection[,7]),
                                activites = c(rep("Gestes de base",length(selection[,1])),
                                              rep("SAMR",length(selection[,2])),
                                              rep("Charte",length(selection[,3])),
                                              rep("Livre multimédia",length(selection[,4])),
                                              rep("Machine à tri",length(selection[,5])),
                                              rep("Jeu du robot",length(selection[,6])),
                                              rep("Concepts SI",length(selection[,7]))))
            
            
            
            p <- ggplot(table[complete.cases(table), ], aes(x=activites, y=values)) +
              geom_boxplot(fill='#FADE7D', color="black", alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              theme_minimal() +
              labs(title = "Ma participation aux ateliers suivants est très utile pour mon travail") +
              scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                            "Pas \nd'accord",
                                            "Plutôt pas \nd'accord",
                                            "Ni en accord \nni en désaccord",
                                            "Plutôt \nd'accord",
                                            "D'accord",
                                            "Tout à fait \nd'accord"), breaks = 1:7) +
              theme(plot.title = element_text(face = "italic", size = 10),
                    legend.position = "none",
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    axis.text.x = element_text(angle = 70, vjust = 0.5, hjust = 0.5,size=11),
                    axis.text.y = element_text(size=12),
                    #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                    #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
            
            return(p)
            
        }
        
        if (input$reactionsImmediates == "Auto-efficacité vis-à-vis des ressources") {
  
            
            selection <- data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("confiance_gestes_de_base",
                                                                                     "confiance_SAMR",
                                                                                     "confiance_charte",
                                                                                     "confiance_livre",
                                                                                     "confiance_machine",
                                                                                     "confiance_jeu",
                                                                                     "confiance_concepts")]
            
            table <- data.frame(values = c(selection[,1],selection[,2],selection[,3],selection[,4],selection[,5],selection[,6],selection[,7]),
                                activites = c(rep("Gestes de base",length(selection[,1])),
                                              rep("SAMR",length(selection[,2])),
                                              rep("Charte",length(selection[,3])),
                                              rep("Livre multimédia",length(selection[,4])),
                                              rep("Machine à tri",length(selection[,5])),
                                              rep("Jeu du robot",length(selection[,6])),
                                              rep("Concepts SI",length(selection[,7]))))
            
            
            
            p <- ggplot(table[complete.cases(table), ], aes(x=activites, y=values)) +
              geom_boxplot(fill='#FADE7D', color="black", alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, position=position_jitter(width = 0.1, height = 0)) +
              theme_minimal() +
              labs(title = "Je me sens en confiance pour utiliser dans mon travail \nles compétences apprises dans les ateliers suivants") +
              scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                            "Pas \nd'accord",
                                            "Plutôt pas \nd'accord",
                                            "Ni en accord \nni en désaccord",
                                            "Plutôt \nd'accord",
                                            "D'accord",
                                            "Tout à fait \nd'accord"), breaks = 1:7) +
              theme(plot.title = element_text(face = "italic", size = 10),
                    legend.position = "none",
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    axis.text.x = element_text(angle = 70, vjust = 0.5, hjust = 0.5,size=11),
                    axis.text.y = element_text(size=12),
                    #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                    #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
            
            return(p)
            
        }
        
        if (input$reactionsImmediates == "Intention d'utiliser les ressources") {
            
            selection <- data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,c("intention_gestes_de_base",
                                                                                     "intention_charte",
                                                                                     "intention_livre",
                                                                                     "intention_machine",
                                                                                     "intention_jeu")]
            
            table <- data.frame(values = c(selection[,1],selection[,2],selection[,3],selection[,4],selection[,5]),
                                activites = c(rep("Gestes de base",length(selection[,1])),
                                              rep("Charte",length(selection[,2])),
                                              rep("Livre multimédia",length(selection[,3])),
                                              rep("Machine à tri",length(selection[,4])),
                                              rep("Jeu du robot",length(selection[,5]))))
            
            
            
            p <- ggplot(table[complete.cases(table), ], aes(x=activites, y=values)) +
              geom_boxplot(fill='#FADE7D', color="black",alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              theme_minimal() +
              labs(title = "J'ai l'intention d'utiliser les ressources et contenus des ateliers suivants") +
              scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                            "Pas \nd'accord",
                                            "Plutôt pas \nd'accord",
                                            "Ni en accord \nni en désaccord",
                                            "Plutôt \nd'accord",
                                            "D'accord",
                                            "Tout à fait \nd'accord"), breaks = 1:7) +
              theme(plot.title = element_text(face = "italic", size = 10),
                    legend.position = "none",
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    axis.text.x = element_text(angle = 70, vjust = 0.5, hjust = 0.5,size=11),
                    axis.text.y = element_text(size=12),
                    #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                    #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
            
            return(p)
            
          }

      })
      
      output$competencesPlot <- renderPlot({
        
        if (input$competences == "Compétences techniques") {
          
            p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=competences_techniques)) +
              geom_boxplot(fill='#71B578', color="black", alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              theme_minimal() +
              labs(title = "J’ai les compétences techniques nécessaires pour \nutiliser les technologies numériques") +
              scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                            "Pas \nd'accord",
                                            "Plutôt pas \nd'accord",
                                            "Ni en accord \nni en désaccord",
                                            "Plutôt \nd'accord",
                                            "D'accord",
                                            "Tout à fait \nd'accord"), breaks = 1:7) +
              theme(plot.title = element_text(face = "italic", size = 10),
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
        
        if (input$competences == "Compétences techno-pédagogiques") {
          
            p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=selection_technologie)) +
              geom_boxplot(fill='#71B578', color="black", alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              theme_minimal() +
              labs(title = "Je suis capable de sélectionner des technologies numériques \nqui améliorent ce que j'enseigne, \nla façon dont j'enseigne et ce que les élèves apprennent.") +
              scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                            "Pas \nd'accord",
                                            "Plutôt pas \nd'accord",
                                            "Ni en accord \nni en désaccord",
                                            "Plutôt \nd'accord",
                                            "D'accord",
                                            "Tout à fait \nd'accord"), breaks = 1:7) +
              theme(plot.title = element_text(face = "italic", size = 10),
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
      
      output$conditionsPlot <- renderPlot({
        
        if (input$conditionsFacilitantes == "Matériel") {
            
            p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=ressources_disponibles)) +
              geom_boxplot(fill='#23AFD6', color="black", alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              theme_minimal() +
              labs(title = "Les ressources dont j'ai besoin pour utiliser ce que j'ai appris \nsont à ma disposition après la formation") +
              scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                            "Pas \nd'accord",
                                            "Plutôt pas \nd'accord",
                                            "Ni en accord \nni en désaccord",
                                            "Plutôt \nd'accord",
                                            "D'accord",
                                            "Tout à fait \nd'accord"), breaks = 1:7) +
              theme(plot.title = element_text(face = "italic", size = 10),
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
        
        if (input$conditionsFacilitantes == "Compatibilité avec les pratiques habituelles") {
          
            p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=lien_contenus_pratique)) +
              geom_boxplot(fill='#23AFD6', color="black", alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              theme_minimal() +
              labs(title = "Les contenus de formation correspondent étroitement \nà ce qui m'est demandé dans ma pratique d'enseignant.e") +
              scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                            "Pas \nd'accord",
                                            "Plutôt pas \nd'accord",
                                            "Ni en accord \nni en désaccord",
                                            "Plutôt \nd'accord",
                                            "D'accord",
                                            "Tout à fait \nd'accord"), breaks = 1:7) +
              theme(plot.title = element_text(face = "italic", size = 10),
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
        
        if (input$conditionsFacilitantes == "Soutien hiérarchique") {
            
            p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=interet_direction)) +
              geom_boxplot(fill='#23AFD6', color="black", alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              theme_minimal() +
              labs(title = "La Direction s’intéresse à ce que j’apprends pendant la formation") +
              scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                            "Pas \nd'accord",
                                            "Plutôt pas \nd'accord",
                                            "Ni en accord \nni en désaccord",
                                            "Plutôt \nd'accord",
                                            "D'accord",
                                            "Tout à fait \nd'accord"), breaks = 1:7) +
              theme(plot.title = element_text(face = "italic", size = 10),
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
          
            p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=encouragement_collegues)) +
              geom_boxplot(fill='#23AFD6', color="black", alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              theme_minimal() +
              labs(title = "Mes collègues m'encouragent à utiliser les compétences acquises en formation") +
              scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                            "Pas \nd'accord",
                                            "Plutôt pas \nd'accord",
                                            "Ni en accord \nni en désaccord",
                                            "Plutôt \nd'accord",
                                            "D'accord",
                                            "Tout à fait \nd'accord"), breaks = 1:7) +
              theme(plot.title = element_text(face = "italic", size = 10),
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
        
        if (input$conditionsFacilitantes == "Soutien technique et pédagogique") {

            p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=soutien)) +
              geom_boxplot(fill='#23AFD6', color="black", alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              theme_minimal() +
              labs(title = "J'ai assez de soutien (PR, RI, collègues) pour \nmobiliser les compétences acquises en formation") +
              scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                            "Pas \nd'accord",
                                            "Plutôt pas \nd'accord",
                                            "Ni en accord \nni en désaccord",
                                            "Plutôt \nd'accord",
                                            "D'accord",
                                            "Tout à fait \nd'accord"), breaks = 1:7) +
              theme(plot.title = element_text(face = "italic", size = 10),
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
        
        if (input$conditionsFacilitantes == "Aide disponible") {
          
            p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=savoir_aide)) +
              geom_boxplot(fill='#23AFD6', color="black",alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              theme_minimal() +
              labs(title = "Quand j'essaie d'utiliser les nouvelles choses \nque j'ai apprises, je sais qui peut m'aider") +
              scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                            "Pas \nd'accord",
                                            "Plutôt pas \nd'accord",
                                            "Ni en accord \nni en désaccord",
                                            "Plutôt \nd'accord",
                                            "D'accord",
                                            "Tout à fait \nd'accord"), breaks = 1:7) +
              theme(plot.title = element_text(face = "italic", size = 10),
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
          
            p <- ggplot(data[data$id_binome==input$id&data$journee==input$journee&data$date==input$session,], aes(x=id_binome, y=charge_travail)) +
              geom_boxplot(fill='#23AFD6', color="black", alpha=0.7,outlier.shape = NA) +
              geom_jitter(shape=16, size=3, position=position_jitter(width = 0.3, height = 0)) +
              theme_minimal() +
              labs(title = "Ma charge de travail me laisse le temps d'essayer \nles nouvelles choses que j'ai apprises") +
              scale_y_continuous(labels = c("Pas du \ntout d'accord",
                                            "Pas \nd'accord",
                                            "Plutôt pas \nd'accord",
                                            "Ni en accord \nni en désaccord",
                                            "Plutôt \nd'accord",
                                            "D'accord",
                                            "Tout à fait \nd'accord"), breaks = 1:7) +
              theme(plot.title = element_text(face = "italic", size = 10),
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

    }
    
  })
  
  
}


shinyApp(ui = ui, server = server)
