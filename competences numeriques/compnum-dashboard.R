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

#######################  API  #######################

#connectApiUrl <- "https://api.surveymonkey.com/v3/surveys" #needed first to get questionnaire id

details <- "https://api.surveymonkey.com/v3/surveys/507153613/details"
responses <- "https://api.surveymonkey.com/v3/surveys/507153613/responses/bulk?simple=true"
connectApiKey <- "cOmUpOXfevWx560egh1njr2VdHlQBpAay26.xBEyidJz6GAapRctumcP0wakhEoKWck.rCm9ilv0T9WvWbFCC28WiiH5Xm6Wi-SsjnPELjqqyZ6WoeEOAg7nLOwE4EWe"

resp_details <- GET(details,
                    add_headers(Authorization = paste0("Bearer ", connectApiKey)))

resp_responses <- GET(responses,
            add_headers(Authorization = paste0("Bearer ", connectApiKey)))

result_details <- content(resp_details, "parsed")
result_responses <- content(resp_responses, "parsed")

# Questions

#result_details$pages[[3]]$questions[[1]]$headings[[1]]$heading

columns <- c("date",
             "avant_enseignement",
             "pendant_enseignement",
             "contraintes_facilitateurs",
             "realise_auparavant",
             "combien_fois",
             "aide_formation_edunum",
             "debut_formation",
             "modules",
             "sexe",
             "pr",
             "experience",
             "matieres",
             "etablissement")

# Reponses

for (j in 1:length(result_responses$data)) {

    x <- function() {
      
        if (result_responses$data[[j]]$pages[[5]]$questions[[1]]$answers[[1]]$simple_text == "Oui"){
      
          if (result_responses$data[[j]]$pages[[6]]$questions[[1]]$answers[[1]]$simple_text == "2020"){
          
            x <- c()
        
        
            for (i in 1:length(result_responses$data[[j]]$pages[[7]]$questions[[1]]$answers)) {
              
              x <- append(x,gsub("<.*?>","",result_responses$data[[j]]$pages[[7]]$questions[[1]]$answers[[i]]$simple_text))
              
            }
            
          } else {
            
            x <- c()
            
            for (i in 1:length(result_responses$data[[j]]$pages[[8]]$questions[[1]]$answers)) {
              
              x <- append(x,gsub("<.*?>","",result_responses$data[[j]]$pages[[8]]$questions[[1]]$answers[[i]]$simple_text))
              
            }
            
          }
        
        return(x)
        
        } else {
          
          return(NA)
          
        }
    }
    
    
    z <- function() {
    
      z <- c()
    
      for (i in 1:length(result_responses$data[[j]]$pages[[9]]$questions[[4]]$answers)) {
        
        z <- append(z,result_responses$data[[j]]$pages[[9]]$questions[[4]]$answers[[i]]$simple_text)
        
      }
      
      return(z)
      
    }
    
    
    answers <- c(result_responses$data[[j]]$date_created,
                 result_responses$data[[j]]$pages[[2]]$questions[[1]]$answers[[1]]$text,
                 result_responses$data[[j]]$pages[[2]]$questions[[2]]$answers[[1]]$text,
                 result_responses$data[[j]]$pages[[2]]$questions[[3]]$answers[[1]]$text,
                 
                 result_responses$data[[j]]$pages[[3]]$questions[[1]]$answers[[1]]$simple_text,
                 
                 if (length(result_responses$data[[j]]$pages[[4]]$questions) == 0) {
                   
                   NA
                   
                 } else {
                   
                   result_responses$data[[j]]$pages[[4]]$questions[[1]]$answers[[1]]$simple_text
                   
                 },
                   
                   
                 #result_responses$data[[j]]$pages[[5]]$questions[[1]]$answers[[1]]$simple_text,
                 
                 NA,
                 
                 if (length(result_responses$data[[j]]$pages[[6]]$questions) == 0) {
                   
                   NA
                 
                 } else {
                   
                   result_responses$data[[j]]$pages[[6]]$questions[[1]]$answers[[1]]$simple_text
                   
                 },
                 
                 
                 if (length(result_responses$data[[j]]$pages[[6]]$questions) != 0) {
                   
                   list(x())
                 
                 } else {
                   
                   
                   NA
                   
                   
                 },

                 result_responses$data[[j]]$pages[[9]]$questions[[1]]$answers[[1]]$simple_text,
                 result_responses$data[[j]]$pages[[9]]$questions[[2]]$answers[[1]]$simple_text,
                 result_responses$data[[j]]$pages[[9]]$questions[[3]]$answers[[1]]$simple_text,
                 
                 list(z()),
                 
                 result_responses$data[[j]]$pages[[9]]$questions[[5]]$answers[[1]]$simple_text)
    
    
    if (j == 1) {
      
      data <- rbind(columns,answers)
      
    } else {
      
      data <- rbind(data,answers)
      
    }
    
}

table <- as.data.frame(data)
colnames(table) <- columns
rownames(table) <- 1:nrow(table)
table = table[-1,]


#####################################################


highlight <- '
                function getSelectionText() {
var text = "";
if (window.getSelection) {
text = window.getSelection().toString();
} else if (document.selection) {
text = document.selection.createRange().text;
}
return text;
}

document.onmouseup = document.onkeyup = document.onselectionchange = function() {
var selection = getSelectionText();
Shiny.onInputChange("mydata", selection);
};
'

coded_text <- character(0)

ui <- dashboardPage(
  
  dashboardHeader(title = (span("Codage des compétences RCnum",
                                style = "font-size: 28px")),
                  titleWidth = 550,
                  tags$li(actionLink("openModal", label = "", icon = icon("info")),
                          class = "dropdown")),
  
  dashboardSidebar(width = 200,
                   sidebarMenu(id = "sidebarid",
                               
                               menuItem("Codage des compétences", tabName = "page1"),
                               conditionalPanel(
                                 'input.sidebarid == "page1"'),
                               
                               menuItem("Visualisation des résultats", tabName = "page2"),
                               conditionalPanel(
                                 'input.sidebarid == "page2"'))),
  
  dashboardBody(
    
    tags$head(tags$script(highlight)),
    
    tabItems(
      
      tabItem(tabName = "page1",
    
    fluidRow(
      
    column(4,
           tags$h2("Réponses des enseignant.e.s"),
           selectInput(inputId = "no_repondant",label = "Répondant : ",choices = c("",1:nrow(table)),selected=""),
           tableOutput("table_text"),
           actionButton("export_all", "Exporter toutes les données récoltées"),
           div(style="display:inline-block",textOutput("text_all"))),
    column(8,
           tags$h2("Compétences RCNum"),
           actionButton("rcnum101", "RCnum 1.0.1",width = "100px",style="color: white;background-color: #E26C09", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum101", 
                     title = "Gérer son identité numérique en tant qu agent.e de la fonction publique"),
           actionButton("rcnum102", "RCnum 1.0.2",style="color: white;background-color: #E26C09", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum102", 
                     title = "Manipuler divers environnements numériques, notamment des solutions libres"),
           actionButton("rcnum103", "RCnum 1.0.3",style="color: white;background-color: #E26C09", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum103", 
                     title = "Identifier et tenir compte des enjeux de société et en particulier de la numérisation des phénomènes scolaires"),
           actionButton("rcnum104", "RCnum 1.0.4",style="color: white;background-color: #E26C09", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum104", 
                     title = "Choisir les environnements numériques en étant conscients des intérêts en jeu"),
           actionButton("rcnum111", "RCnum 1.1.1",style="color: white;background-color: #E26C09", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum111", 
                     title = "Sélectionner et utiliser des outils ou environnements de communication numérique"),
           actionButton("rcnum112", "RCnum 1.1.2",style="color: white;background-color: #E26C09", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum112", 
                     title = "Contribuer au développement et amélioration des stratégies de communication organisationnelles"),
           actionButton("rcnum121", "RCnum 1.2.1",style="color: white;background-color: #E26C09", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum121", 
                     title = "Sélectionner les technologies numériques pour collaborer avec autres enseignant.es..."),
           actionButton("rcnum122", "RCnum 1.2.2",style="color: white;background-color: #E26C09", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum122", 
                     title = "Participer à la production et la diffusion des ressources éducatives libres (REL)"),
           actionButton("rcnum123", "RCnum 1.2.3",style="color: white;background-color: #E26C09", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum123", 
                     title = "Mutualiser des pratiques techno-pédagogiques"),
           actionButton("rcnum124", "RCnum 1.2.4",style="color: white;background-color: #E26C09", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum124", 
                     title = "Effectuer une curation de contenus"),
           actionButton("rcnum131", "RCnum 1.3.1",style="color: white;background-color: #E26C09", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum131", 
                     title = "Développer activement ses propres pratiques techno-pédagogiques et celles de sa communauté pédagogique"),
           actionButton("rcnum132", "RCnum 1.3.2",style="color: white;background-color: #E26C09", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum132", 
                     title = "Questionner ses pratiques professionnelles numériques"),
           actionButton("rcnum133", "RCnum 1.3.3",style="color: white;background-color: #E26C09", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum133", 
                     title = "Co-construire de nouvelles connaissances techno-pédagogiques dans des communautés de pratiques"),
           actionButton("rcnum141", "RCnum 1.4.1",style="color: white;background-color: #E26C09", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum141", 
                     title = "Sélectionner les sources et les ressources numériques pour le développement professionnel continu"),
           actionButton("rcnum142", "RCnum 1.4.2",style="color: white;background-color: #E26C09", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum142", 
                     title = "Effectuer une veille techno-pédagogiques par exemple en participant à des réseaux professionnels"),
           actionButton("rcnum143", "RCnum 1.4.3",style="color: white;background-color: #E26C09", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum143", 
                     title = "Développer une culture numérique en continu"),
           actionButton("rcnum211", "RCnum 2.1.1",style="color: white;background-color: #00AF50", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum211", 
                     title = "Identifier, évaluer et sélectionner des ressources existantes"),
           actionButton("rcnum212", "RCnum 2.1.2",style="color: white;background-color: #00AF50", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum212", 
                     title = "Modifier et développer des ressources existantes"),
           actionButton("rcnum213", "RCnum 2.1.3",style="color: white;background-color: #00AF50", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum213", 
                     title = "Créer ou co-créer de nouvelles ressources"),
           actionButton("rcnum214", "RCnum 2.1.4",style="color: white;background-color: #00AF50", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum214", 
                     title = "Partager ses propres ressources"),
           actionButton("rcnum221", "RCnum 2.2.1",style="color: white;background-color: #00AF50", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum221", 
                     title = "Identifier les apports possibles de l usage de ressources pédagogiques...connaissances disciplinaires"),
           actionButton("rcnum222", "RCnum 2.2.2",style="color: white;background-color: #00AF50", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum222", 
                     title = "Identifier les apports possibles de l usage de ressources pédagogiques...capacités transversales"),
           actionButton("rcnum231", "RCnum 2.3.1",style="color: white;background-color: #00AF50", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum231", 
                     title = "Dans le travail avec des ressources pédagogiques, prendre en considération leurs caractéristiques..."),
           actionButton("rcnum232", "RCnum 2.3.2",style="color: white;background-color: #00AF50", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum232", 
                     title = "Dans le travail avec des ressources pédagogiques, prendre en considération les conditions pédagogiques..."),
           actionButton("rcnum241", "RCnum 2.4.1",style="color: white;background-color: #00AF50", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum241", 
                     title = "Organiser le contenu numérique et le mettre à la disposition..."),
           actionButton("rcnum242", "RCnum 2.4.2",style="color: white;background-color: #00AF50", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum242", 
                     title = "Protéger efficacement les contenus numériques sensibles"),
           actionButton("rcnum243", "RCnum 2.4.3",style="color: white;background-color: #00AF50", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum243", 
                     title = "Respecter les règles de confidentialité et de droits d auteurs"),
           actionButton("rcnum244", "RCnum 2.4.4",style="color: white;background-color: #00AF50", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum244", 
                     title = "Favoriser l utilisation et la création de licences ouvertes et de ressources éducatives libres..."),
           actionButton("rcnum311", "RCnum 3.1.1",style="color: white;background-color: #538DD3", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum311", 
                     title = "Planifier et mettre en oeuvre des dispositifs et des ressources numériques dans l enseignement..."),
           actionButton("rcnum312", "RCnum 3.1.2",style="color: white;background-color: #538DD3", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum312", 
                     title = "Gérer et orchestrer de façon appropriée les interventions d enseignement numérique"),
           actionButton("rcnum313", "RCnum 3.1.3",style="color: white;background-color: #538DD3", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum313", 
                     title = "Expérimenter et développer de nouveaux formats et méthodes pédagogiques pour l enseignement"),
           actionButton("rcnum321", "RCnum 3.2.1",style="color: white;background-color: #538DD3", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum321", 
                     title = "Améliorer l interaction avec les apprenant.es..."),
           actionButton("rcnum322", "RCnum 3.2.2",style="color: white;background-color: #538DD3", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum322", 
                     title = "Offrir des formats d accompagnement opportuns et ciblés"),
           actionButton("rcnum323", "RCnum 3.2.3",style="color: white;background-color: #538DD3", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum323", 
                     title = "Favoriser et améliorer la collaboration entre les apprenant.es"),
           actionButton("rcnum324", "RCnum 3.2.4",style="color: white;background-color: #538DD3", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum324", 
                     title = "Soutenir les processus d apprentissage autorégulés..."),
           actionButton("rcnum331", "RCnum 3.3.1",style="color: white;background-color: #538DD3", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum331", 
                     title = "Planifier et mettre en œuvre une séquence d’enseignement mobilisant des technologies..."),
           actionButton("rcnum332", "RCnum 3.3.2",style="color: white;background-color: #538DD3", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum332", 
                     title = "Identifier les possibilités et limites des outils dans la réalisation de tâches inscrites dans une discipline scolaire"),
           actionButton("rcnum333", "RCnum 3.3.3",style="color: white;background-color: #538DD3", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum333", 
                     title = "Connaître les effets des outils numériques sur la construction des savoirs visés..."),
           actionButton("rcnum341", "RCnum 3.4.1",style="color: white;background-color: #538DD3", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum341", 
                     title = "Planifier et mettre en œuvre une séquence d’enseignement-apprentissage..."),
           actionButton("rcnum342", "RCnum 3.4.2",style="color: white;background-color: #538DD3", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum342", 
                     title = "Identifier les difficultés ou compréhensions erronées courantes des apprenant.es..."),
           actionButton("rcnum343", "RCnum 3.4.3",style="color: white;background-color: #538DD3", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum343", 
                     title = "Implémenter des stratégies permettant le développement de compétences de lecture..."),
           actionButton("rcnum351", "RCnum 3.5.1",style="color: white;background-color: #538DD3", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum351", 
                     title = "Planifier et mettre en œuvre une séquence d’enseignement-apprentissage..."),
           actionButton("rcnum352", "RCnum 3.5.2",style="color: white;background-color: #538DD3", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum352", 
                     title = "Implémenter des stratégies permettant le développement de l esprit critique..."),
           actionButton("rcnum361", "RCnum 3.6.1",style="color: white;background-color: #538DD3", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum361", 
                     title = "Planifier et mettre en oeuvre une séquence d enseignement-apprentissage..."),
           actionButton("rcnum362", "RCnum 3.6.2",style="color: white;background-color: #538DD3", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum362", 
                     title = "Implémenter des stratégies permettant d’évaluer les impacts sociaux de la numérisation..."),
           actionButton("rcnum411", "RCnum 4.1.1",style="color: white;background-color: #30849B", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum411", 
                     title = "Utiliser les technologies numériques pour l évaluation diagnostique, formative et sommative"),
           actionButton("rcnum412", "RCnum 4.1.2",style="color: white;background-color: #30849B", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum412", 
                     title = "Diversifier les formats et les approches d évaluation, en adéquation avec les besoins des apprenant.es"),
           actionButton("rcnum413", "RCnum 4.1.3",style="color: white;background-color: #30849B", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum413", 
                     title = "Exploiter les technologies numériques pour évaluer les capacités transversales"),
           actionButton("rcnum414", "RCnum 4.1.4",style="color: white;background-color: #30849B", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum414", 
                     title = "Garantir un cadre éthique dans la collecte et la gestion des données issues de processus d évaluation"),
           actionButton("rcnum415", "RCnum 4.1.5",style="color: white;background-color: #30849B", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum415", 
                     title = "Faire preuve d ouverture pour d éventuelles nouvelles formes d évaluation"),
           actionButton("rcnum416", "RCnum 4.1.6",style="color: white;background-color: #30849B", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum416", 
                     title = "Mobiliser des stratégies permettant d’évaluer l acquisition de compétences et de connaissances..."),
           actionButton("rcnum421", "RCnum 4.2.1",style="color: white;background-color: #30849B", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum421", 
                     title = "Générer, sélectionner, analyser de manière critique et interpréter les données numériques..."),
           actionButton("rcnum422", "RCnum 4.2.2",style="color: white;background-color: #30849B", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum422", 
                     title = "Exploiter avec discernement les capacités des technologies numériques à documenter..."),
           actionButton("rcnum431", "RCnum 4.3.1",style="color: white;background-color: #30849B", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum431", 
                     title = "Utiliser les technologies numériques pour fournir une rétroaction..."),
           actionButton("rcnum432", "RCnum 4.3.2",style="color: white;background-color: #30849B", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum432", 
                     title = "Permettre aux apprenants et aux parents de comprendre les résultats fournis par les technologies numériques..."),
           actionButton("rcnum433", "RCnum 4.3.3",style="color: white;background-color: #30849B", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum433", 
                     title = "Identifier les données qui peuvent et méritent d être transmises à des partenaires pédago-thérapeutiques..."),
           actionButton("rcnum441", "RCnum 4.4.1",style="color: white;background-color: #30849B", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum441", 
                     title = "Adapter les stratégies d enseignement en conséquence et apporter un soutien ciblé..."),
           actionButton("rcnum511", "RCnum 5.1.1",style="color: white; background-color: #6F2F9F", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum511", 
                     title = "Veiller à ce que tous.toutes les apprenants...aient accès aux apprentissages"),
           actionButton("rcnum512", "RCnum 5.1.2",style="color: white; background-color: #6F2F9F", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum512", 
                     title = "Veiller à ce que tous.toutes les apprenants...développent des compétences numériques"),
           actionButton("rcnum521", "RCnum 5.2.1",style="color: white; background-color: #6F2F9F", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum521", 
                     title = "Varier les environnements numériques d apprentissage..."),
           actionButton("rcnum522", "RCnum 5.2.2",style="color: white; background-color: #6F2F9F", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum522", 
                     title = "Adapter l enseignement en différenciant les médias, outils, et/ou environnements..."),
           actionButton("rcnum531", "RCnum 5.3.1",style="color: white; background-color: #6F2F9F", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum531", 
                     title = "Utiliser les technologies numériques pour favoriser l engagement actif et créatif des apprenants..."),
           actionButton("rcnum532", "RCnum 5.3.2",style="color: white; background-color: #6F2F9F", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum532", 
                     title = "Promouvoir les capacités transversales à l aide de technologies numériques"),
           actionButton("rcnum533", "RCnum 5.3.3",style="color: white; background-color: #6F2F9F", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum533", 
                     title = "Favoriser des pédagogies actives, collaboratives et ouvertes"),
           actionButton("rcnum611", "RCnum 6.1.1",style="color: white; background-color: #E84E76", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum611", 
                     title = "Mettre en oeuvre des activités mettant l accent sur les enjeux citoyens..."),
           actionButton("rcnum612", "RCnum 6.1.2",style="color: white; background-color: #E84E76", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum612", 
                     title = "Favoriser l utilisation des médias dans les activités afin d amener les apprenantes à s informer..."),
           actionButton("rcnum613", "RCnum 6.1.3",style="color: white; background-color: #E84E76", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum613", 
                     title = "Intégrer des activités qui requièrent des apprenants une définition explicite de leurs besoins..."),
           actionButton("rcnum621", "RCnum 6.2.1",style="color: white; background-color: #E84E76", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum621", 
                     title = "Mettre en œuvre des activités permettant aux apprenants de développer leur compréhension des impacts..."),
           actionButton("rcnum622", "RCnum 6.2.2",style="color: white; background-color: #E84E76", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum622", 
                     title = "Intégrer des activités permettant aux apprenant.es d aborder des problèmes de manière structurée..."),
           actionButton("rcnum623", "RCnum 6.2.3",style="color: white; background-color: #E84E76", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum623", 
                     title = "Favoriser la compréhension chez les apprenantes de la manière dont tous types de données sont représentées..."),
           actionButton("rcnum624", "RCnum 6.2.4",style="color: white; background-color: #E84E76", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum624", 
                     title = "Proposer des activités permettant aux apprenants de construire une représentation d’un ordinateur"),
           actionButton("rcnum631", "RCnum 6.3.1",style="color: white; background-color: #E84E76", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum631", 
                     title = "Développer des pratiques citoyennes pour soutenir la prévention"),
           actionButton("rcnum632", "RCnum 6.3.2",style="color: white; background-color: #E84E76", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum632", 
                     title = "Proposer aux apprenantes des activités permettant d acquérir des savoirs, des compétences et des habiletés..."),
           actionButton("rcnum633", "RCnum 6.3.3",style="color: white; background-color: #E84E76", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum633", 
                     title = "Intégrer des activités scolaires qui facilitent la recherche de contenus et d informations..."),
           actionButton("rcnum634", "RCnum 6.3.4",style="color: white; background-color: #E84E76", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
           bsTooltip(id = "rcnum634", 
                     title = "Mettre en oeuvre des activités scolaires qui favorisent le réinvestissement et l expression..."),
           tags$h2("Codes réalisés"),
           DTOutput("table_code"),
           verbatimTextOutput("selected_text"),
           actionButton("remove", "Supprimer le(s) codage(s) sélectionné(s)"),
           actionButton("export", "Exporter le codage"),
           div(style="display:inline-block",textOutput("text")),
           
    )
  )),
  
  tabItem(tabName = "page2",
          
          fluidRow(
            
            column(3,
                   box(width=NULL,height = '530px', solidHeader = TRUE, status = "primary",
                       title = "Données démographiques",
                       selectInput(inputId = "variable",label = "",choices = c("Sexe","PR","Expérience"),width=120),
                       plotOutput("demoPlot"))
            ),
            
            column(4,
                   box(width=NULL,height = '530px', solidHeader = TRUE, status = "primary",
                       title = "Matières investies",
                       div(style="display:inline-block",selectInput(inputId = "repondant_matiere",label = "",choices = c("Tous",1:nrow(table)),width=80)),
                      plotOutput("matierePlot"))
            ),
            
            column(5,
                   box(width=NULL,height = '530px', solidHeader = TRUE, status = "primary",
                   title = "Apport des modules de formation aux activités réalisées",
                   div(style="display:inline-block",selectInput(inputId = "repondant_module",label = "",choices = c("Tous",1:nrow(table)),width=80)),
                   plotOutput("modulePlot"))
                       
            )),
          
          
          fluidRow(
            
            column(12,
                   box(width=NULL,height = '530px', solidHeader = TRUE, status = "primary",
                   title = "Compétences numériques mise en oeuvre",
                   div(style="display:inline-block",selectInput(inputId = "repondant_rcnum",label = "",choices = c("Tous",1:nrow(table)),width=80)),
                   plotOutput("rcnumPlot"))
            )))
  )
  
  )
)


server <- function(input, output) {
  
###############################################################
#CODAGE DES COMPETENCES
###############################################################
  
  date <- c()
  for (i in 1:nrow(table)){
    
    date <- append(date,table$date[i]$answers)
  }
  
  avant_enseignement <- c()
  for (i in 1:nrow(table)){
    
    avant_enseignement <- append(avant_enseignement,table$avant_enseignement[i]$answers)
  }
  
  pendant_enseignement <- c()
  for (i in 1:nrow(table)){
    
    pendant_enseignement <- append(pendant_enseignement,table$pendant_enseignement[i]$answers)
  }
  
  contraintes_facilitateurs <- c()
  for (i in 1:nrow(table)){
    
    contraintes_facilitateurs <- append(contraintes_facilitateurs,table$contraintes_facilitateurs[i]$answers)
  }
  
  realise_auparavant <- c()
  for (i in 1:nrow(table)){
    
    realise_auparavant <- append(realise_auparavant,table$realise_auparavant[i]$answers)
  }
  
  combien_fois <- c()
  for (i in 1:nrow(table)){
    
    combien_fois <- append(combien_fois,table$combien_fois[i]$answers)
  }
  
  aide_formation_edunum <- c()
  for (i in 1:nrow(table)){
    
    aide_formation_edunum <- append(aide_formation_edunum,table$aide_formation_edunum[i]$answers)
  }
  
  debut_formation <- c()
  for (i in 1:nrow(table)){
    
    debut_formation <- append(debut_formation,table$debut_formation[i]$answers)
  }
  
  modules <- c()
  for (i in 1:nrow(table)){
    
    modules <- append(modules,toString(table$modules[i]$answers))
  }
  
  sexe <- c()
  for (i in 1:nrow(table)){
    
    sexe <- append(sexe,table$sexe[i]$answers)
  }
  
  pr <- c()
  for (i in 1:nrow(table)){
    
    pr <- append(pr,table$pr[i]$answers)
  }
  
  experience <- c()
  for (i in 1:nrow(table)){
    
    experience <- append(experience,table$experience[i]$answers)
  }
  
  
  matieres <- c()
  for (i in 1:nrow(table)){
    
    matieres <- append(matieres,toString(table$matieres[i]$answers))
  }
  
  etablissement <- c()
  for (i in 1:nrow(table)){
    
    etablissement <- append(etablissement,table$etablissement[i]$answers)
  }
  
  table_stats <- as.data.frame(cbind(date,
                                     avant_enseignement,
                                     pendant_enseignement,
                                     contraintes_facilitateurs,
                                     realise_auparavant,
                                     combien_fois,
                                     aide_formation_edunum,
                                     debut_formation,
                                     modules,
                                     sexe,
                                     pr,
                                     experience,
                                     matieres,
                                     etablissement), row.names = FALSE)
  
  output$table_text <- renderTable({
    text1 <- if (input$no_repondant=="") {""} else {table$avant_enseignement[as.numeric(input$no_repondant)]$answers}
    text2 <- if (input$no_repondant=="") {""} else {table$pendant_enseignement[as.numeric(input$no_repondant)]$answers}
    text3 <- if (input$no_repondant=="") {""} else {table$contraintes_facilitateurs[as.numeric(input$no_repondant)]$answers}
    data.frame(Questions = c("Avant l'enseignement", "Pendant l'enseignement", "Facilitateurs/Obstacles"), Réponses = c(text1, text2, text3))
  })
  
  rvs <- reactiveValues(
    
    data = NULL
    
  )
  
  updateTable <- observeEvent(input$rcnum101 |
                              input$rcnum102 |
                                input$rcnum103 |
                                input$rcnum104 |
                                input$rcnum111 |
                                input$rcnum112 |
                                input$rcnum121 |
                                input$rcnum122 |
                                input$rcnum123 |
                                input$rcnum124 |
                                input$rcnum131 |
                                input$rcnum132 |
                                input$rcnum133 |
                                input$rcnum141 |
                                input$rcnum142 |
                                input$rcnum143 |
                                input$rcnum211 |
                                input$rcnum212 |
                                input$rcnum213 |
                                input$rcnum214 |
                                input$rcnum221 |
                                input$rcnum222 |
                                input$rcnum231 |
                                input$rcnum232 |
                                input$rcnum241 |
                                input$rcnum242 |
                                input$rcnum243 |
                                input$rcnum244 |
                                input$rcnum311 |
                                input$rcnum312 |
                                input$rcnum313 |
                                input$rcnum321 |
                                input$rcnum322 |
                                input$rcnum323 |
                                input$rcnum324 |
                                input$rcnum331 |
                                input$rcnum332 |
                                input$rcnum333 |
                                input$rcnum341 |
                                input$rcnum342 |
                                input$rcnum343 |
                                input$rcnum351 |
                                input$rcnum352 |
                                input$rcnum361 |
                                input$rcnum362 |
                                input$rcnum411 |
                                input$rcnum412 |
                                input$rcnum413 |
                                input$rcnum414 |
                                input$rcnum415 |
                                input$rcnum416 |
                                input$rcnum421 |
                                input$rcnum422 |
                                input$rcnum431 |
                                input$rcnum432 |
                                input$rcnum433 |
                                input$rcnum441 |
                                input$rcnum442 |
                                input$rcnum443 |
                                input$rcnum511 |
                                input$rcnum512 |
                                input$rcnum521 |
                                input$rcnum522 |
                                input$rcnum531 |
                                input$rcnum532 |
                                input$rcnum533 |
                                input$rcnum611 |
                                input$rcnum612 |
                                input$rcnum613 |
                                input$rcnum621 |
                                input$rcnum622 |
                                input$rcnum623 |
                                input$rcnum624 |
                                input$rcnum631 |
                                input$rcnum632 |
                                input$rcnum633 |
                                input$rcnum634,
    
    {
      
    if(input$mydata != "") {
      
      if(is.null(rvs$data)){
        
        rvs$data <- data.frame("Répondant"=c(as.numeric(input$no_repondant)),"Code RCNum" = c(input$btnLabel), "Sélection" = c(input$mydata))
        
      }
        
      else {
        
        if(nrow(rvs$data)==0) {
          
          rvs$data <- data.frame("Répondant"=c(as.numeric(input$no_repondant)),"Code RCNum" = c(input$btnLabel), "Sélection" = c(input$mydata))
          
        }
        
        else {
          
          #rvs$data[nrow(rvs$data) + 1,] <- c(input$btnLabel, input$mydata)
          
          rvs$data <- rbind(c(as.numeric(input$no_repondant),input$btnLabel, input$mydata),rvs$data)
          
        }
      }
    }
  
    
    },ignoreInit = TRUE)
  
  
  output$table_code = renderDT(rvs$data[rvs$data$"Répondant"==as.numeric(input$no_repondant),],escape = FALSE, server = TRUE, editable = FALSE, extensions="Buttons", rownames = FALSE, 
                               options = list(pageLength = 5, columnDefs = list(list(width = '30px', targets = c(0)))))
  
  
  
  observeEvent(input$remove,{
    
    if (!is.null(input$table_code_rows_selected)) {
      
      rvs$data <- rvs$data[-as.numeric(input$table_code_rows_selected),]
      
    }
  })
  
  
  observeEvent(input$export,{
    
    write.csv2(rvs$data, file = "data_competences_numeriques.csv",row.names=FALSE)
    output$text <- renderText({"  Téléchargé!"})
  
  })
  
  observeEvent(input$export_all,{
    
    write.csv2(table_stats, file = "data_enseignants.csv",row.names=FALSE)
    output$text_all <- renderText({"  Téléchargé!"})
    
  })
  
###############################################################
#VISUALISATION DES RESULTATS
###############################################################
  
output$demoPlot <- renderPlot({
  
  if (input$variable == "Sexe") {
    
  data <- data.frame(category=c("Enseignants", "Enseignantes"),
               count=c(sum(table_stats[,"sexe"]=="Enseignant"), sum(table_stats[,"sexe"]=="Enseignante")))
  
  # Compute percentages
  data$fraction <- data$count / sum(data$count)
  
  # Compute the cumulative percentages (top of each rectangle)
  data$ymax <- cumsum(data$fraction)
  
  # Compute the bottom of each rectangle
  data$ymin <- c(0, head(data$ymax, n=-1))
  
  # Compute label position
  data$labelPosition <- (data$ymax + data$ymin) / 2
  
  # Compute a good label
  data$label <- paste0(data$category, "\n : ", data$count)
  
  # Make the plot
  p <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
    geom_rect() +
    geom_label( x=3.5, aes(y=labelPosition, label=label), size=6,color="#FFFFFF") +
    scale_fill_manual(values=c("#4EB3D3","#2B8CBE")) +
    coord_polar(theta="y") +
    xlim(c(2, 4)) +
    theme_void() +
    theme(legend.position = "none"
          #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
          #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5")
          )
          
  
  return(p)
  
  }
  
  if (input$variable == "PR") {
    
    data <- data.frame(category=c("Oui", "Non"),
                       count=c(sum(table_stats[,"pr"]=="Oui"), sum(table_stats[,"pr"]=="Non")))
    
    # Compute percentages
    data$fraction <- data$count / sum(data$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    # Compute the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n=-1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    data$label <- paste0(data$category, "\n : ", data$count)
    
    # Make the plot
    p <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() +
      geom_label( x=3.5, aes(y=labelPosition, label=label), size=6,color="#FFFFFF") +
      scale_fill_manual(values=c("#4EB3D3","#2B8CBE")) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none"
            #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
            #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5")
            )
    
    return(p)
    
  }
  
  
  if (input$variable == "Expérience") {
    
    data <- data.frame(category=c("Années d'expérience",""),
                       count=c(round(mean(as.numeric(table_stats[,"experience"]),na.rm=TRUE),0),
                               38-mean(as.numeric(table_stats[,"experience"]),na.rm=TRUE)))
    
    # Compute percentages
    data$fraction <- data$count / sum(data$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    # Compute the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n=-1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    data$label <- paste0(data$count," ans")
    data$label[2] <- ""
    
    # Make the plot
    p <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() +
      geom_text( x=3.5, aes(y=labelPosition, label=label), size=6, color="#FFFFFF") +
      scale_fill_manual(values=c("#ECF0F5","#2B8CBE")) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none"
            #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
            #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5")
            )
    
    return(p)
    
  }
  
  })
  
  output$matierePlot <- renderPlot({
    
    if (input$repondant_matiere == "Tous") {
      
      matiere <- unlist(strsplit(table_stats$matieres,","))
      value <- table(matiere)
      
      data <- data.frame(table(matiere))
      
      p <- ggplot(data, aes(x=matiere,y=Freq)) +
        geom_bar(stat = "identity",fill="#2B8CBE") +
        geom_label(aes(label = Freq, vjust = 1.5, size=5)) +
        theme_minimal() +
        scale_x_discrete(labels=label_wrap(15)) +
        theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.x = element_text(angle = 0, hjust = 0.5,size=11),
              axis.text.y = element_blank(),
              #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
              #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

      return(p)
      
    }
    
    if (input$repondant_matiere != "Tous") {
      
      matiere <- unlist(strsplit(table_stats$matieres[as.numeric(input$repondant_matiere)],","))
      value <- table(matiere)
      
      data <- data.frame(table(matiere))
      
      p <- ggplot(data, aes(x=matiere,y=Freq)) +
        geom_bar(stat = "identity",fill="#2B8CBE") +
        geom_label(aes(label = Freq, vjust = 1.5, size=5)) +
        theme_minimal() +
        scale_x_discrete(labels=label_wrap(15)) +
        theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.x = element_text(angle = 0, hjust = 0.5,size=11),
              axis.text.y = element_blank(),
              #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
              #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
      return(p)
      
    }
  
  })
  
  output$modulePlot <- renderPlot({
    
    
    if (input$repondant_module == "Tous") {
      
      module <- unlist(strsplit(gsub('\\(.*?\\)', '', table_stats$modules),","))
      
      module <- module[!(module %in% c("NA"))]
      
      data <- data.frame(table(module))
      
      p <- ggplot(data, aes(x=module,y=Freq)) +
        geom_bar(stat = "identity",fill="#2B8CBE") +
        geom_label(aes(label = Freq, vjust = 1.5, size=5)) +
        theme_minimal() +
        scale_x_discrete(labels=label_wrap(15)) +
        theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.x = element_text(angle = 0, hjust = 0.5,size=11),
              axis.text.y = element_blank(),
              #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
              #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
      return(p)
      
    }
    
    if (input$repondant_module != "Tous") {
      
      module <- unlist(strsplit(gsub('\\(.*?\\)', '', table_stats$modules[as.numeric(input$repondant_module)]),","))
      
      module <- module[!(module %in% c("NA"))]
      
      #module <- gsub(" ", "\n",  module)
      
      data <- data.frame(table(module))
      
      p <- ggplot(data, aes(x=module,y=Freq)) +
        geom_bar(stat = "identity",fill="#2B8CBE") +
        geom_label(aes(label = Freq, vjust = 1.5, size=5)) +
        theme_minimal() +
        scale_x_discrete(labels=label_wrap(15)) +
        theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.x = element_text(angle = 0, hjust = 0.5,size=11),
              axis.text.y = element_blank(),
              #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
              #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
      return(p)
      
    }
      
    })
  
  output$rcnumPlot <- renderPlot({
    
    if (!is.null(rvs$data)) {
    
    
      if (input$repondant_rcnum == "Tous") {
        
        rcnum <- rvs$data[,c("Code.RCNum")]
        
        data <- data.frame(table(rcnum))
        
        p <- ggplot(data, aes(x=rcnum,y=Freq)) +
          geom_bar(stat = "identity",fill="#2B8CBE") +
          geom_label(aes(label = Freq, vjust = 1.5, size=5)) +
          theme_minimal() +
          theme(legend.position = "none",
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.x = element_text(angle = 0, hjust = 0.5,size=11),
                axis.text.y = element_blank(),
                #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
        
        return(p)
        
      }
      
      if (input$repondant_rcnum != "Tous") {
        
        rcnum <- rvs$data[rvs$data$"Répondant"==as.numeric(input$repondant_rcnum),c("Code.RCNum")]
        
        print(rcnum)
        
        data <- data.frame(table(rcnum))
        
        p <- ggplot(data, aes(x=rcnum,y=Freq)) +
          geom_bar(stat = "identity",fill="#2B8CBE") +
          geom_label(aes(label = Freq,vjust = 1.5, size=5)) +
          theme_minimal() +
          theme(legend.position = "none",
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.x = element_text(angle = 0, hjust = 0.5,size=11),
                axis.text.y = element_blank(),
                #plot.background = element_rect(fill = "#ECF0F5",color = "#ECF0F5"),
                #panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
        
        return(p)
        
      }
    }
    
  })
  
}


shinyApp(ui = ui, server = server)