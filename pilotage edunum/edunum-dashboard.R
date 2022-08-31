library(shinyalert)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggthemes)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(shinyauthr)
library(sodium)
library(shinymanager)

#setwd("/Volumes/GoogleDrive/Drive partagés/Projet canton de vaud_ EPFL interne/Recherche/Analyse/ShinyEvaluationDashboard")

# sample logins dataframe with passwords hashed by sodium package
user_base <- dplyr::tibble(
  user = c("user1", "user2"),
  password = sapply(c("pass1", "pass2"), sodium::password_store), 
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

data <- read.csv2("data_evaluation.csv",header = TRUE, na.strings=c("","NA"), check.names = FALSE)
data <- data[rev(order(as.Date(data$date, format="%d/%m/%Y"))),]
data$x_axis <- paste0(if_else(data$phase == "Non concerné", paste0("Volée ", data$annee_volee),data$phase), " J", data$journee, "\n", "N=", data$nombre_participants, "\n", data$date, "\n", paste0(if_else(data$activite != "Général",data$activite,""),"\n",if_else(data$rcnum != "Non concerné",data$rcnum,""),if_else(data$pernum != "Non concerné",data$pernum,"")))
data$x_axis <- str_wrap(data$x_axis, 10)
data$score_percent <- paste0(data$score, "%")

colors <- c("Faible"="red",
            "Moyen"= "orange",
            "Bon" = "lightgreen",
            "Excellent" = "darkgreen")

ui <- dashboardPage(title="Pilotage de la formation EduNum",
  
  dashboardHeader(title = HTML(paste(span("Pilotage de la formation EduNum\n",style = 'font-size:28px'), sep ="")), 
                  titleWidth = 450,
                  tags$li(class = "dropdown", style = "padding: 8px;",shinyauthr::logoutUI("logout"))),

  dashboardSidebar(
    
    HTML("<p align='justify'>Ce tableau de bord résume les indicateurs clé du pilotage de la formation et des ressources de formation dans le cadre de la réforme EduNum.</p>"),
    
    sidebarMenu(id = "sidebarid",
                
         menuItem("Les résultats", tabName = "page1"),
         conditionalPanel(
           'input.sidebarid == "page1"',
           selectInput(inputId = "var1",label = "Cycle/Formation:",choices = unique(data$cycle_formation)),
           selectInput(inputId = "var2",label = "Phase/Année:",choices = NULL),
           selectInput(inputId = "var3",label = "Participants:",choices = NULL),
           HTML("<br>")),
    
         menuItem("Le modèle d'évaluation", tabName = "page2")), collapsed = TRUE),
            
  
  dashboardBody(
    
    shinyjs::useShinyjs(),
    
    shinyauthr::loginUI("login"),
    
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
      
      tabItem(tabName = "page2",
              HTML("<center><h2> De la formation à la classe : un modèle dédié au pilotage de la mise en œuvre de l’éducation numérique </h2></center>"),
              HTML('<center><img src="epfl_learn_logo.png", height = 150></center>'),
              br(),
              HTML("<h4>Ce modèle d'évaluation à été développé par l’équipe de recherche dédiée au projet EduNum au sein du centre LEARN. Il vise à évaluer le processus global d’intégration des contenus d’éducation numérique par le corps enseignant en exercice, de la formation à la classe. Chaque niveau d’évaluation cible différents critères basés sur une revue de la littérature scientifique dans les domaines de l’évaluation du développement professionnel en éducation et l’usage des technologies en classe.</h4>"),
              HTML("<h4>Ce modèle permet d’adopter une démarche d’évaluation complète (au-delà de la satisfaction post-formation) visant à mettre en place des changements adaptatifs (modifier les contenus non satisfaisants) et proactifs (anticiper les besoins pour maximiser la mise en œuvre des ressources/compétences en classe) tout au long du développement de la formation.</h4>"),
              br(),
              HTML('<center><img src="modele.png", height = 500></center>'),
              br(),
              HTML("<h4>Ce modèle cible 5 niveaux d’évaluation et de 3 groupes de variables reflétant certaines caractériques des participants, de la formation et des élèves :</h4>"),
              br(),
              HTML("<h4><ul><li><b>Variables participant.e.s</b></li></ul></h4>"),
              HTML("<h4>Les variables participant.e.s reflètent certaines caractéristiques des participants comme le degré de motivation pour suivre la formation ou l'attitude préalable envers la réforme</h4>"),
              br(),
              HTML("<h4><ul><li><b>Variables formation</b></li></ul></h4>"),
              HTML("<h4>Les variables formation reflètent l'implication de l'équipe de formation et la qualité des méthodes pédagogiques employées</h4>"),
              br(),
              HTML("<h4><ul><li><b>Niveau 1 (Réactions immédiates post-journée de formation)</b> </li></ul></h4>"),
              HTML("<h4>Ce niveau évalue si les ressources pédagogiques proposées en formation sont appréciées et répondent aux besoins des participants (intérêt, perception d’utilité pour l’enseignant, etc.)</h4>"),
              br(),
              HTML("<h4><ul><li> <b>Niveau 2 (Acquisition des notions théoriques et des compétences)</b> </li></ul></h4>"),
              HTML("<h4>Ce niveau évalue si les notions théoriques véhiculées par la formation et les compétences numériques pour la formation en éducation numérique (CIIP) sont bien transmises.</h4>"),
              br(),
              HTML("<h4><ul><li> <b>Niveau 3 (Conditions facilitantes/entravantes) </b></li></ul></h4>"),
              HTML("<h4>Ce niveau évalue la présence de facilitateurs et de freins à la mise en œuvre des ressources et compétences en classe (matériel, temps, soutien hiérarchique et des collègues, soutien technique et pédagogique, etc.)</h4>"),
              br(),
              HTML("<h4><ul><li> <b>Niveau 4 (Mise en œuvre et niveau d’appropriation des ressources/compétences en classe)</b> </li></ul></h4>"),
              HTML("<h4>Ce niveau évalue si les ressources/compétences vues en formation sont bien mises en œuvre en classe et le transfert des compétences numériques des enseignant.e.s</h4>"),
              br(),
              HTML("<h4><ul><li> <b>Niveau 5 (Apprentissage des élèves)</b> </li></ul></h4>"),
              HTML("<h4>Ce niveau évalue si l’apprentissage des élèves est conforme aux prescriptions du PERnum.</h4>"),
              br(),
              HTML("<h4><ul><li><b>Variables élèves</b></li></ul></h4>"),
              HTML("<h4>Les variables élèves reflètent certaines caractéristiques des élèves pouvant modérer leur apprentissage.</h4>"),
              br(), br(),
              HTML("<h4>Pour tout renseignement : Dr. Sunny Avry – sunny.avry@epfl.ch</h4>"),
              br(),
              HTML("<h4>Référence: <i> Avry, S., Monnier, E-M., El-Hamamsy, L., Caneva,  C., Pulfrey, C., & Dehler-Zufferey, J. (2022). Evaluating the implementation of digital education by teachers: an integrated theoretical model [Manuscript submitted for publication]</i></h4>"),
      ),

    
    tabItem(tabName = "page1",
              
              
    fluidRow(id = "row0",
      tabBox(title = HTML("<b>Variables enseignant.e.s</b>"),
             id = "tabset0", height = "530px", width=12,
             tabPanel("Motivation prélable",
                      HTML("<h5>Le score reflète le degré de motivation préalable des participants à la formation.</h5>"),
                      plotlyOutput("participants_motivation"),
                      HTML("<h5>Score moyen sur une échelle de Likert entre 1 (Pas du tout d'accord) et 7 (Tout à fait d'accord) converti en pourcentage à des affirmations telles que <i>Je me sens motivé pour participer à cette formation.</i></h5>")),
             tabPanel("Attitude envers l'éducation numérique",
                      HTML("<h5>Le score reflète le degré d'adhésion des participants à la réforme.</h5>"),
                      plotlyOutput("participants_attitude"),
                      HTML("<h5>Score moyen sur une échelle de Likert entre 1 (Pas du tout d'accord) et 7 (Tout à fait d'accord) converti en pourcentage à des affirmations telles que <i>Je trouve que la réforme de l'éducation numérique est une bonne chose.</i></h5>")),
      )),
              
    
    fluidRow(id = "row1",
    tabBox(title = HTML("<b>Variables formation</b>"),
           id = "tabset1", height = "530px", width=12,
      tabPanel("Implication/Appréciation",
               HTML("<h5>Le score reflète le degré d'implication ou d'appréciation de l'équipe de formation rapporté par les participants à l'issue de la journée de formation.</h5>"),
               plotlyOutput("formation_implication"),
               HTML("<h5>Score moyen sur une échelle de Likert entre 1 (Pas du tout d'accord) et 7 (Tout à fait d'accord) converti en pourcentage à des affirmations telles que <i>J'ai trouvé l'équipe de formation impliquée lors de cette journée</i> ou <i>J'ai apprécié l'équipe de formation lors de cette journée.</i></h5>")),
      tabPanel("Qualité perçue des méthodes pédagogiques employées",
               HTML("<h5>Le score reflète la qualité perçue par les participants des méthodes pédagogiques employées par l'équipe de formation.</h5>"),
               plotlyOutput("formation_pedagogie"),
               HTML("<h5>Score moyen sur une échelle de Likert entre 1 (Pas du tout d'accord) et 7 (Tout à fait d'accord) converti en pourcentage à des affirmations telles que <i>La façon dont a été présenté le contenu était adaptée.</i></h5>")),
    )),
  
    fluidRow(id = "row2",
      tabBox(title = HTML("<b style='color:#ffd500;'>Niveau 1 (Réactions immédiates)</b>"),
        id = "tabset2", height = "530px", width=12,
        tabPanel("Intérêt perçu pour les ressources",
                 HTML("<h5>Le score reflète à quel point les ressources de formation sont perçues par les participants comme intéressantes à l'issue de la journée de formation.</h5>"),
                 plotlyOutput("niveau1_interet"),
                 HTML("<h5>Score moyen sur une échelle de Likert entre 1 (Pas du tout d'accord) et 7 (Tout à fait d'accord) converti en pourcentage à des affirmations telles que <i>J'ai trouvé cette ressource intéressante</i> ")),
        tabPanel("Utilité perçue des ressources",
                 HTML("<h5>Le score reflète à quel point les ressources de formation sont perçues par les participants comme utiles pour leur pratique enseignante à l'issue de la journée de formation.</h5>"),
                 plotlyOutput("niveau1_utilite"),
                 HTML("<h5>Score moyen sur une échelle de Likert entre 1 (Pas du tout d'accord) et 7 (Tout à fait d'accord) converti en pourcentage à des affirmations telles que <i>J'ai trouvé cette ressource utile pour mon enseignant</i>")),
        tabPanel("Facilité perçue d'utilisation", 
                 HTML("<h5>Le score reflète à quel point les ressources de formation sont perçues par les participants comme faciles d'utilisation à l'issue de la journée de formation.</h5>"),
                 plotlyOutput("niveau1_facilite"),
                 HTML("<h5>Score moyen sur une échelle de Likert entre 1 (Pas du tout d'accord) et 7 (Tout à fait d'accord) converti en pourcentage à des affirmations telles que <i>J'ai trouvé cette ressource facile à intégrer dans ma pratique enseignante</i> ")),
        tabPanel("Auto-efficacité vis-à-vis des ressources", 
                 HTML("<h5>Le score reflète à quel point les participants se sentent capable d'utilisation les ressources de formation en classe à l'issue de la journée de formation.</h5>"),
                 plotlyOutput("niveau1_autoefficacite"),
                 HTML("<h5>Score moyen sur une échelle de Likert entre 1 (Pas du tout d'accord) et 7 (Tout à fait d'accord) converti en pourcentage à des affirmations telles que <i>Je me sens capable d'intégrer cette ressource dans mon enseignement</i> ")),
        tabPanel("Intention d'utiliser les ressources", 
                 HTML("<h5>Le score reflète à quel point les participants ont l'intention d'utiliser les ressources de formation en classe à l'issue de la journée de formation.</h5>"),
                 plotlyOutput("niveau1_intention"),
                 HTML("<h5>Score moyen sur une échelle de Likert entre 1 (Pas du tout d'accord) et 7 (Tout à fait d'accord) converti en pourcentage à des affirmations telles que <i>J'ai l'intention d'intégrer cette ressource dans mon enseignement</i> "))
      )),
  
    fluidRow(id = "row3",
      tabBox(
        title = HTML("<b style='color:#6DA845;'>Niveau 2 (Acquisition des connaissances et compétences numériques)</b>"),
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset3", height = "530px", width=12,
        tabPanel("Acquisition des contenus théoriques", 
                 HTML("<h5>Le score reflète à quel point les participants pensent avoir acquis les contenus théoriques dispensés à l'issue de la journée de formation.</h5>"),
                 plotlyOutput("niveau2_theorie"),
                 HTML("<h5>Score moyen sur une échelle de Likert entre 1 (Pas du tout acquis) et 7 (Tout à fait acquis) converti en pourcentage à des questions telles que <i>Est-ce que cette notion théorique est maintenant acquise pour vous?</i> ")),
        tabPanel("Acquisition des compétences numériques (cf. RCnum)",
                 HTML("<h5>Le score reflète à quel point les participants pensent avoir acquis les compétences numériques (RCnum) dispensées lors de la journée de formation.</h5>"),
                 plotlyOutput("niveau2_techno_pedagogie"),
                 HTML("<h5>Score moyen sur une échelle de Likert entre 1 (Pas du tout acquis) et 7 (Tout à fait acquis) converti en pourcentage à des questions telles que <i>Est-ce que cette compétence est maintenant acquise pour vous?</i> "))
        
      )),
    
    fluidRow(id = "row4",
      tabBox(
        title = HTML("<b style='color:#5A9AD3;'>Niveau 3 (Conditions entravantes et efficience perçue des ressources de formation)</b>"),
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset4", height = "600px", width=12,
        tabPanel("Ressources matérielles",
                 HTML("<h5>Le score reflète le manque de ressources matérielles rapporté par les participants pour l'utilisation des ressources de formation.</h5>"),
                 plotlyOutput("niveau3_materiel"),
                 HTML("<h5>Le score peut représenter : <br><br> 1) le score moyen sur une échelle de Likert entre 1 (Pas du tout d'accord) et 7 (Tout à fait d'accord) converti en pourcentage à des affirmations telles que <i>Je manque du matériel nécessaire pour intégrer les ressources de formation dans ma pratique</i> <br>
                                                       2) le pourcentage de participants rapportant un manque de ressources matérielles parmi les différentes conditions entravantes proposées")),
        tabPanel("Ressources temporelles",
                 HTML("<h5>Le score reflète le manque de ressources temporelles rapporté par les participants pour l'utilisation des ressources de formation.</h5>"),
                 plotlyOutput("niveau3_temporel"),
                 HTML("<h5>Le score peut représenter : <br><br> 1) le score moyen sur une échelle de Likert entre 1 (Pas du tout d'accord) et 7 (Tout à fait d'accord) converti en pourcentage à des affirmations telles que <i>Je manque du temps nécessaire pour intégrer les ressources de formation dans ma pratique</i> <br>
                                                       2) le pourcentage de participants rapportant un manque de ressources temporelles parmi les différentes conditions entravantes proposées")),
        tabPanel("Soutien de la hiérarchie et des collègues", 
                 HTML("<h5>Le score reflète le manque de soutien de la hiérarchie ou des collègues rapporté par les participants pour l'utilisation des ressources de formation.</h5>"),
                 plotlyOutput("niveau3_hierarchie"),
                 HTML("<h5>Le score peut représenter : <br><br> 1) le score moyen sur une échelle de Likert entre 1 (Pas du tout d'accord) et 7 (Tout à fait d'accord) converti en pourcentage à des affirmations telles que <i>Je manque du soutien de ma hiérarchie ou des mes collègues pour intégrer les ressources de formation dans ma pratique</i> <br>
                                                       2) le pourcentage de participants rapportant un manque de soutien de la hiérarchie ou des collègues parmi les différentes conditions entravantes proposées")),
        tabPanel("Soutien technique et pédagogique", 
                 HTML("<h5>Le score reflète le manque de soutien technique et/ou pédagogique rapporté par les participants pour l'utilisation des ressources de formation.</h5>"),
                 plotlyOutput("niveau3_techno_pedagogie"),
                 HTML("<h5>Le score peut représenter : <br><br> 1) le score moyen sur une échelle de Likert entre 1 (Pas du tout d'accord) et 7 (Tout à fait d'accord) converti en pourcentage à des affirmations telles que <i>Je manque de soutien technique ou pédagogique pour intégrer les ressources de formation dans ma pratique</i> <br>
                                                       2) le pourcentage de participants rapportant un manque de soutien technique et/ou pédagogique parmi les différentes conditions entravantes proposées")),
        tabPanel("Compatibilité avec les pratiques habituelles",
                 HTML("<h5>Le score reflète le manque compatibilité entre les nouvelle ressources de formation et les pratiques habituelles des participants rapporté pour l'utilisation des ressources de formation.</h5>"),
                 plotlyOutput("niveau3_compatibilite"),
                 HTML("<h5>Le score peut représenter : <br><br> 1) le score moyen sur une échelle de Likert entre 1 (Pas du tout d'accord) et 7 (Tout à fait d'accord) converti en pourcentage à des affirmations telles que <i>Les ressources proposées ne s'intègrent pas facilement à mes pratiques habituelles d'enseignement</i> <br>
                                                       2) le pourcentage de participants rapportant un manque de compatibilité avec les pratiques habituelles d'enseignement parmi les différentes conditions entravantes proposées")),
        tabPanel("Efficience perçue",
                 HTML("<h5>L’efficience perçue représente le rapport entre des bénéfices (progrès perçu chez les élèves, augmentation de l’auto-efficacité perçue vis-à-vis des ressources de formation) et des coûts (effort de réflexion, charge de travail) liés à l’utilisation des ressources de formation dans la classe. Elle est calculée sur la base du rapport entre le pourcentage d’enseignant.e.s ayant perçu ces bénéfices et le pourcentage d’enseignant.e.s ayant perçu ces coûts. Une efficience négative indique que les enseignant.e.s perçoivent en moyenne plus de coûts que de bénéfices dans la mise en place des activités en classe.</h5>"),
                 plotlyOutput("niveau3_efficience"))
        
      )),
    
    fluidRow(id = "row5",
      tabBox(
        title =  HTML("<b style='color:#ED7D31;'>Niveau 4 (Mise en oeuvre des ressources et transfert des compétences numériques)</b>"),
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset5", height = "650px", width=12,
        tabPanel("Mise en oeuvre en classe d'activités liées aux ressources de formation", 
                 HTML("<h5>Le score reflète le pourcentage de participants ayant réalisé au moins une fois une activité clé-en-main en classe ou ayant réalisé au moins une fois une activité à partir des ressources de formation proposées.</h5>"),
                 plotlyOutput("niveau4_utilisation"),
                 HTML("<h5>Pourcentage de particpants rapportant avoir mis en oeuvre dans la classe les ressources de formation</h5>")),
        tabPanel("Transfert en classe des compétences numériques (RCnum) associées aux ressources de formation",
                 HTML("<h5>Le score reflète à quel point les participants ont mis en oeuvre les compétences numériques associées aux ressources de formation proposées.</h5>"),
                 plotlyOutput("niveau4_appropriation"),
                 HTML("<h5>Pourcentage de particpants ayant mis (ou rapportant avoir mis) en oeuvre dans leur pratique d'enseignant les compétences numériques vus en formation.</h5>")),
      )),
    
    fluidRow(id = "row6",
      tabBox(
        title = HTML("<b>Variables élèves</b>"),
        id = "tabset6", height = "300px", width=12,
        tabPanel("Stéréotypes préalables envers l'éducation numérique", ""),
        tabPanel("Familiarité préalable envers la technologie", "")
      )),
    
    fluidRow(id ="row7",
      tabBox(
        title = HTML("<b style='color:#A5A5A5;'>Niveau 5 (Résultats élèves)</b>"),
        id = "tabset7", height = "300px", width=12,
        tabPanel("Compétences disciplinaires (PERnum)",
                 HTML("<h5>Le score reflète à quel point les élèves ont mis en oeuvre les compétences numériques displinaires associées aux ressources de formation proposées.<br><br></h5>"),
                 plotlyOutput("niveau5_pernum")),
        tabPanel("Compétences transversales (PER)", ""),
        tabPanel("Changements attitudinaux", ""),
      ))
    
    ))
))
  
server <- function(input, output, session) {
  
  # login status and info will be managed by shinyauthr module and stores here
  credentials <- shinyauthr::loginServer(
                            id ="login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password,
                            sodium_hashed = TRUE,
                            log_out = reactive(logout_init()))
  
  # logout status managed by shinyauthr module and stored here
  logout_init <- shinyauthr::logoutServer(
                           id = "logout", 
                           active = reactive(credentials()$user_auth))
  
  
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      show(id = "row0", anim = FALSE)
      show(id = "row1", anim = FALSE)
      show(id = "row2", anim = FALSE)
      show(id = "row3", anim = FALSE)
      show(id = "row4", anim = FALSE)
      show(id = "row5", anim = FALSE)
      show(id = "row6", anim = FALSE)
      show(id = "row7", anim = FALSE)
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      hide(id = "row0", anim = FALSE)
      hide(id = "row1", anim = FALSE)
      hide(id = "row2", anim = FALSE)
      hide(id = "row3", anim = FALSE)
      hide(id = "row4", anim = FALSE)
      hide(id = "row5", anim = FALSE)
      hide(id = "row6", anim = FALSE)
      hide(id = "row7", anim = FALSE)
    }
    
  })
  
  output$tab2_ui <- renderUI({
    req(credentials()$user_auth)
  })
  
  
  observeEvent(input$openModal, {
    showModal(
      modalDialog(title = "Contact",
                  footer = modalButton("Fermer"),
                  p("Ce tableau de bord a été créé par l'équipe de recherche EduNum-EPFL. Pour tout renseignement : sunny.avry@epfl.ch"))
    )
  })

  observeEvent(input$var1,{
    updateSelectInput(session,'var2',
                      choices=unique(data$phase[data$cycle_formation==input$var1]))
  })
  
  observeEvent(input$var1,{
    updateSelectInput(session,'var3',
                      choices=unique(data$participants[data$cycle_formation==input$var1]))
  })
  
  
  output$participants_motivation <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Motivation préalable")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 57  ~ "Faible" ,
      score >= 57  & score < 71 ~ "Moyen"  ,
      score >= 71  & score < 85  ~ "Bon"  ,
      score >= 85 ~ "Excellent"))
    
    if (nrow(data) > 0) {
      
      plot_ly(
        data,
        x = ~x_axis,
        y = ~score,
        marker=list(color=colors[data$grade]),
        type = 'bar',
        text = ~score_percent, 
        textposition = 'inside',
        hoverinfo = "text") %>%
        rangeslider(start=-1, end=12, width = 0.2) %>% 
        layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
               xaxis = list(title=""))
      
    }
    
    else {plot_ly(type = 'bar') %>%
        layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
               xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
    }
    
  })
  
  output$participants_attitude <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Attitude envers l'éducation numérique")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 57  ~ "Faible" ,
      score >= 57  & score < 71 ~ "Moyen"  ,
      score >= 71  & score < 85  ~ "Bon"  ,
      score >= 85 ~ "Excellent"))
    
    if (nrow(data) > 0) {
      
      plot_ly(
        data,
        x = ~x_axis,
        y = ~score,
        marker=list(color=colors[data$grade]),
        type = 'bar',
        text = ~score_percent, 
        textposition = 'inside',
        hoverinfo = "text") %>%
        rangeslider(start=-1, end=12, width = 0.2) %>% 
        layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
               xaxis = list(title=""))
      
    }
    
    else {plot_ly(type = 'bar') %>%
        layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
               xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
    }
    
  })
  
  
  output$formation_implication <- renderPlotly({ 
    
      data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Perception de l'implication de l'équipe de formation")
      data$x_axis <- factor(data$x_axis, unique(data$x_axis))
      
      data <- data %>% mutate(grade = case_when(
        score < 57  ~ "Faible" ,
        score >= 57  & score < 71 ~ "Moyen"  ,
        score >= 71  & score < 85  ~ "Bon"  ,
        score >= 85 ~ "Excellent"))
      
      if (nrow(data) > 0) {
        
        plot_ly(
          data,
          x = ~x_axis,
          y = ~score,
          marker=list(color=colors[data$grade]),
          type = 'bar',
          text = ~score_percent, 
          textposition = 'inside',
          hoverinfo = "text") %>%
          rangeslider(start=-1, end=12, width = 0.2) %>% 
          layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
                 xaxis = list(title=""))
        
      }
      
      else {plot_ly(type = 'bar') %>%
          layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
                 xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
      }
      
    })
  
  output$formation_pedagogie <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Qualité perçue des méthodes pédagogiques")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 57  ~ "Faible" ,
      score >= 57  & score < 71 ~ "Moyen"  ,
      score >= 71  & score < 85  ~ "Bon"  ,
      score >= 85 ~ "Excellent"))
    
    if (nrow(data) > 0) {
      
      plot_ly(
        data,
        x = ~x_axis,
        y = ~score,
        marker=list(color=colors[data$grade]),
        type = 'bar',
        text = ~score_percent, 
        textposition = 'inside',
        hoverinfo = "text") %>%
        rangeslider(start=-1, end=12, width = 0.2) %>% 
        layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
               xaxis = list(title=""))
      
    }
    
    else {plot_ly(type = 'bar') %>%
        layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
               xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
    }
    
  })
  
  output$niveau1_interet <- renderPlotly({ 
    
      data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Intérêt perçu pour les ressources")
      data$x_axis <- factor(data$x_axis, unique(data$x_axis))
      
      data <- data %>% mutate(grade = case_when(
        score < 57  ~ "Faible" ,
        score >= 57  & score < 71 ~ "Moyen"  ,
        score >= 71  & score < 85  ~ "Bon"  ,
        score >= 85 ~ "Excellent"))
      
      if (nrow(data) > 0) {
        
        plot_ly(
          data,
          x = ~x_axis,
          y = ~score,
          marker=list(color=colors[data$grade]), 
          type = 'bar',
          text = ~score_percent, 
          textposition = 'inside',
          hoverinfo = "text") %>%
          rangeslider(start=-1, end=12, width = 0.2) %>% 
          layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
                 xaxis = list(title=""))
        
      }
      
      else {plot_ly(type = 'bar') %>%
          layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
                 xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
      }

  })
  
  output$niveau1_utilite <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Utilité perçue des ressources")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 57  ~ "Faible" ,
      score >= 57  & score < 71 ~ "Moyen"  ,
      score >= 71  & score < 85  ~ "Bon"  ,
      score >= 85 ~ "Excellent"))
    
    if (nrow(data) > 0) {
      
      plot_ly(
        data,
        x = ~x_axis,
        y = ~score,
        marker=list(color=colors[data$grade]), 
        type = 'bar',
        text = ~score_percent, 
        textposition = 'inside',
        hoverinfo = "text") %>%
        rangeslider(start=-1, end=12, width = 0.2) %>% 
        layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
               xaxis = list(title=""))
      
    }
    
    else {plot_ly(type = 'bar') %>%
        layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
               xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
    }
    
  })
  
  output$niveau1_facilite <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Facilité perçue d’utilisation des ressources")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 57  ~ "Faible" ,
      score >= 57  & score < 71 ~ "Moyen"  ,
      score >= 71  & score < 85  ~ "Bon"  ,
      score >= 85 ~ "Excellent"))
    
      if (nrow(data) > 0) {
        
        plot_ly(
          data,
          x = ~x_axis,
          y = ~score,
          marker=list(color=colors[data$grade]), 
          type = 'bar',
          text = ~score_percent, 
          textposition = 'inside',
          hoverinfo = "text") %>%
          rangeslider(start=-1, end=12, width = 0.2) %>% 
          layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
                 xaxis = list(title=""))
        
      }
      
      else {plot_ly(type = 'bar') %>%
          layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
                 xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
      }
    
  })
  
  output$niveau1_autoefficacite <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Sentiment d'auto-efficacité dans la réalisation de l'activité")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 57  ~ "Faible" ,
      score >= 57  & score < 71 ~ "Moyen"  ,
      score >= 71  & score < 85  ~ "Bon"  ,
      score >= 85 ~ "Excellent"))
    
    if (nrow(data) > 0) {
      
      plot_ly(
        data,
        x = ~x_axis,
        y = ~score,
        marker=list(color=colors[data$grade]), 
        type = 'bar',
        text = ~score_percent, 
        textposition = 'inside',
        hoverinfo = "text") %>%
        rangeslider(start=-1, end=12, width = 0.2) %>% 
        layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
               xaxis = list(title=""))
      
    }
    
    else {plot_ly(type = 'bar') %>%
        layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
               xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
    }
    
  })
  
  output$niveau1_intention <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Intention d’utiliser les ressources")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 57  ~ "Faible" ,
      score >= 57  & score < 71 ~ "Moyen"  ,
      score >= 71  & score < 85  ~ "Bon"  ,
      score >= 85 ~ "Excellent"))
    
    
    if (nrow(data) > 0) {
    
    plot_ly(
      data,
      x = ~x_axis,
      y = ~score,
      marker=list(color=colors[data$grade]), 
      type = 'bar',
      text = ~score_percent, 
      textposition = 'inside',
      hoverinfo = "text") %>%
      rangeslider(start=-1, end=12, width = 0.2) %>% 
      layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
             xaxis = list(title=""))
      
    }
    
    else {plot_ly(type = 'bar') %>%
        layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
               xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
    }
    
  })
  
  output$niveau2_theorie <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Connaissance du contenu théorique")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 57  ~ "Faible" ,
      score >= 57  & score < 71 ~ "Moyen"  ,
      score >= 71  & score < 85  ~ "Bon"  ,
      score >= 85 ~ "Excellent"))
    
    if (nrow(data) > 0) {
    
    plot_ly(
      data,
      x = ~x_axis,
      y = ~score,
      marker=list(color=colors[data$grade]), 
      type = 'bar',
      text = ~score_percent, 
      textposition = 'inside',
      hoverinfo = "text") %>%
      rangeslider(start=-1, end=12, width = 0.2) %>% 
      layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
             xaxis = list(title=""))
      
    }
    
    else {plot_ly(type = 'bar') %>%
      layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
             xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
      
      }
  
  }) 
  
  output$niveau2_techno_pedagogie <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Acquisition des compétences numériques")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 57  ~ "Faible" ,
      score >= 57  & score < 71 ~ "Moyen"  ,
      score >= 71  & score < 85  ~ "Bon"  ,
      score >= 85 ~ "Excellent"))
    
    if (nrow(data) > 0) {
      
      plot_ly(
        data,
        x = ~x_axis,
        y = ~score,
        marker=list(color=colors[data$grade]), 
        type = 'bar',
        text = ~score_percent, 
        textposition = 'inside',
        hoverinfo = "text") %>%
        rangeslider(start=-1, end=12, width = 0.2) %>% 
        layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
               xaxis = list(title=""))
      
    }
    
    else {plot_ly(type = 'bar') %>%
        layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
               xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
    }
    
  }) 
  
  output$niveau3_materiel <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Manque de ressources matérielles")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 15  ~ "Excellent" ,
      score >= 15  & score < 29 ~ "Bon"  ,
      score >= 29  & score < 43  ~ "Moyen"  ,
      score >= 43 ~ "Faible"))
    
    if (nrow(data) > 0) {
      
      plot_ly(
        data,
        x = ~x_axis,
        y = ~score,
        marker=list(color=colors[data$grade]), 
        type = 'bar',
        text = ~score_percent, 
        textposition = 'inside',
        hoverinfo = "text") %>%
        rangeslider(start=-1, end=12, width = 0.2) %>% 
        layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
               xaxis = list(title=""))
      
    }
    
    else {plot_ly(type = 'bar') %>%
        layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
               xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
    }
    
  }) 
  
  output$niveau3_temporel <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Manque de ressources temporelles")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 15  ~ "Excellent" ,
      score >= 15  & score < 29 ~ "Bon"  ,
      score >= 29  & score < 43  ~ "Moyen"  ,
      score >= 43 ~ "Faible"))
    
    if (nrow(data) > 0) {
      
      plot_ly(
        data,
        x = ~x_axis,
        y = ~score,
        marker=list(color=colors[data$grade]), 
        type = 'bar',
        text = ~score_percent, 
        textposition = 'inside',
        hoverinfo = "text") %>%
        rangeslider(start=-1, end=12, width = 0.2) %>% 
        layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
               xaxis = list(title=""))
      
    }
    
    else {plot_ly(type = 'bar') %>%
        layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
               xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
    }
    
  }) 
  
  output$niveau3_hierarchie <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Manque de soutien hiérarchique ou des collègues")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 15  ~ "Excellent" ,
      score >= 15  & score < 29 ~ "Bon"  ,
      score >= 29  & score < 43  ~ "Moyen"  ,
      score >= 43 ~ "Faible"))
    
    if (nrow(data) > 0) {
      
      plot_ly(
        data,
        x = ~x_axis,
        y = ~score,
        marker=list(color=colors[data$grade]), 
        type = 'bar',
        text = ~score_percent, 
        textposition = 'inside',
        hoverinfo = "text") %>%
        rangeslider(start=-1, end=12, width = 0.2) %>% 
        layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
               xaxis = list(title=""))
      
    }
    
    else {plot_ly(type = 'bar') %>%
        layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
               xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
    }
    
  }) 
  
  output$niveau3_techno_pedagogie <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Manque de soutien technique et/ou pédagogique")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 15  ~ "Excellent" ,
      score >= 15  & score < 29 ~ "Bon"  ,
      score >= 29  & score < 43  ~ "Moyen"  ,
      score >= 43 ~ "Faible"))
    
    if (nrow(data) > 0) {
      
      plot_ly(
        data,
        x = ~x_axis,
        y = ~score,
        marker=list(color=colors[data$grade]), 
        type = 'bar',
        text = ~score_percent, 
        textposition = 'inside',
        hoverinfo = "text") %>%
        rangeslider(start=-1, end=12, width = 0.2) %>% 
        layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
               xaxis = list(title=""))
      
    }
    
    else {plot_ly(type = 'bar') %>%
        layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
               xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
    }
    
  }) 
  
  output$niveau3_compatibilite <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Manque de compatibilité avec les pratiques habituelles d'enseignement")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 15  ~ "Excellent" ,
      score >= 15  & score < 29 ~ "Bon"  ,
      score >= 29  & score < 43  ~ "Moyen"  ,
      score >= 43 ~ "Faible"))
    
    if (nrow(data) > 0) {
      
      plot_ly(
        data,
        x = ~x_axis,
        y = ~score,
        marker=list(color=colors[data$grade]), 
        type = 'bar',
        text = ~score_percent, 
        textposition = 'inside',
        hoverinfo = "text") %>%
        rangeslider(start=-1, end=12, width = 0.2) %>% 
        layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
               xaxis = list(title=""))
      
    }
    
    else {plot_ly(type = 'bar') %>%
        layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
               xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
    }
    
  }) 
  
  output$niveau3_efficience <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Efficience")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 0  ~ "Faible" ,
      score > 0  ~ "Excellent"))
    
    if (nrow(data) > 0) {
      
      plot_ly(
        data,
        x = ~x_axis,
        y = ~score,
        marker=list(color=colors[data$grade]), 
        type = 'bar',
        text = ~score_percent, 
        textposition = 'inside',
        hoverinfo = "text") %>%
        rangeslider(start=-1, end=12, width = 0.2) %>% 
        layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
               xaxis = list(title=""))
      
    }
    
    else {plot_ly(type = 'bar') %>%
        layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
               xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
    }
    
  }) 
  
  output$niveau4_utilisation <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Mise en oeuvre d'activités")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 57  ~ "Faible" ,
      score >= 57  & score < 71 ~ "Moyen"  ,
      score >= 71  & score < 85  ~ "Bon"  ,
      score >= 85 ~ "Excellent"))
    
    if (nrow(data) > 0) {
      
      plot_ly(
        data,
        x = ~x_axis,
        y = ~score,
        marker=list(color=colors[data$grade]), 
        type = 'bar',
        text = ~score_percent, 
        textposition = 'inside',
        hoverinfo = "text") %>%
        rangeslider(start=-1, end=12, width = 0.2) %>% 
        layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
               xaxis = list(title=""))
      
    }
    
    else {plot_ly(type = 'bar') %>%
        layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
               xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
    }
    
  }) 
  
  output$niveau4_appropriation <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Transfert des compétences numériques")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 57  ~ "Faible" ,
      score >= 57  & score < 71 ~ "Moyen"  ,
      score >= 71  & score < 85  ~ "Bon"  ,
      score >= 85 ~ "Excellent"))
    
    if (nrow(data) > 0) {
      
      plot_ly(
        data,
        x = ~x_axis,
        y = ~score,
        marker=list(color=colors[data$grade]), 
        type = 'bar',
        text = ~score_percent, 
        textposition = 'inside',
        hoverinfo = "text") %>%
        rangeslider(start=-1, end=12, width = 0.2) %>% 
        layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
               xaxis = list(title=""))
      
    }
    
    else {plot_ly(type = 'bar') %>%
        layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
               xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
    }
    
  }) 
  
  output$niveau5_pernum <- renderPlotly({ 
    
    data <- data %>% filter(cycle_formation == input$var1, phase == input$var2, participants == input$var3, critere == "Compétences disciplinaires élèves")
    data$x_axis <- factor(data$x_axis, unique(data$x_axis))
    
    data <- data %>% mutate(grade = case_when(
      score < 15  ~ "Excellent" ,
      score >= 15  & score < 29 ~ "Bon"  ,
      score >= 29  & score < 43  ~ "Moyen"  ,
      score >= 43 ~ "Faible"))
    
    if (nrow(data) > 0) {
      
      plot_ly(
        data,
        x = ~x_axis,
        y = ~score,
        marker=list(color=colors[data$grade]), 
        type = 'bar',
        text = ~score_percent, 
        textposition = 'inside',
        hoverinfo = "text") %>%
        rangeslider(start=-1, end=12, width = 0.2) %>% 
        layout(yaxis = list(title = "", fixedrange=T, range(c(20,40)),categoryorder = "category descending"),
               xaxis = list(title=""))
      
    }
    
    else {plot_ly(type = 'bar') %>%
        layout(yaxis = list(showgrid = F, zeroline = F, visible = F),
               xaxis = list(showgrid = F, showline = F, zeroline = F, visible = F))
    }
    
  }) 
  
}

shinyApp(ui = ui, server = server)
