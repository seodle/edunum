# French version

## Fonctionnement général

Les tableaux de bord "enseignants-formateurs" sont des applications R Shiny interactives permettent de disposer d'un retour en temps réel de sondages SurveyMonkey sous forme de visualisations graphiques.

![alt text](screenshot_ef_d1.png)

## Mise en marche rapide

Télécharger le tableau de bord de votre choix (ef_phase1_dashboard.R ou ef_phase2_dashboard.R) dans R Studio et les données fictives qui s'y rapportent (data_ef_d1_random.rds ou data_ef_d2_random.rds). Placer le script et les données dans un même fichier. Lancer le tableau de bord avec R Studio. Indiquez un identifiant EF, correspondant à un code unique donné aux enseignants formateurs pour accéder uniquement à leur données. La liste des identifiants est présente dans le script :

```R

observe({
    
    if (input$id %in% c("5090on3",
                        "6ev1wd5",
                        "bsay5t9",
                        "34vnyfj",
                        "xt8tb0u",
                        "4d9ug6k",
                        "ug8a6dr",
                        "z9jza5v",
                        "zoehjqg",
                        "6hz3qkm",
                        "hg0ybjf",
                        "3yufg4t",
                        "svx837j",
                        "40aw4kh",
                        "s1v9vnj",
                        "p2jie1k",
                        "omos3ix",
                        "2l0xtqz",
                        "43omp4v",
                        "8c4kpz3")) {


```

# Marche à suivre pour utiliser les tableaux de bord

## Fonctionnement détaillé

### ef_phase1_dashboard.R

Le tableau de bord accède directement aux données d'un sondage SurveyMonkey réalisé préalablement (voir survey_ef_d1.pdf).

Lorsque lancé, le tableau de bord met à jour via l'API de SurveyMonkey les données récoltés. Ces données sont stockés dans un fichier data_ef_d1.rds lu et mis à jour par l'application. 

Pour faire tourner l'application avec des données réelles, vous pouvez donc remplacer dans le script le nom *data_ef_d1_random* (données fictives) par *data_ef_d1*. 

## Accès à l'API de SurveyMonkey

L'accès à l'API nécessite la création d'une app sur le compte https://developer.surveymonkey.com/apps/

Pour se connecter à l'API, vous devez récupérer les identifiants de votre app SurveyMonkey et les utiliser pour compléter le script :

- Access Token (sm_api_key)
- Secret (sm_secret)
- Client ID (sm_client_id)

## Trouver les identifiants de votre sondage et votre collecteur

Une fois votre sondage SurveyMonkey créé et ouvert, il possède un id spécifique ainsi qu'un ou plusieurs collecteurs. Chaque collecteur possède également un id spécifique. Pour récupérer ces id, lancer la partie du script mis en commentaire FIND SURVEYS AND COLLECTOR IDS

```R
#######################  FIND SURVEYS AND COLLECTORS IDS  ################

# Once used, you can comment this part of code, you do not need it anymore

# Get the list of surveys to find the id of the wanted survey

surveys_details <- "https://api.surveymonkey.com/v3/surveys/"
n_surveys <- content(GET(surveys_details,add_headers(Authorization = paste0("Bearer ", sm_api_key))),"parsed")$total # Number of surveys

n_pages_surveys <- n_surveys %/% 50 + ifelse(n_surveys %% 50 > 0, 1,0) # Number of pages of surveys

connectApiUrl <- c()
for (i in 1:n_pages_surveys) {

  connectApiUrl <- c(connectApiUrl,paste0("https://api.surveymonkey.com/v3/surveys?page=",i))

}

content_urls <- list()
for (i in 1:length(connectApiUrl)) {

  content_urls <- append(content_urls,list(content(GET(connectApiUrl[i],add_headers(Authorization = paste0("Bearer ", sm_api_key))),"parsed")))

}

surveys <- data.frame()
for (i in 1:length(content_urls)) {

  surveys <- rbind(surveys, data.frame(title=unlist(lapply(1:length(content_urls[[i]]$data), function(x) {content_urls[[i]]$data[[x]]$title})),url=unlist(lapply(1:length(content_urls[[i]]$data), function(x) {content_urls[[i]]$data[[x]]$href}))))

}

surveys # Display a data frame with all the surveys and the associated urls (more recent survey first). The id of your survey is indicated at the end of the url corresponding to your survey of interest

# Get the collectors for a given survey

survey_id <- "" # Indicate here your survey id

collectors <-paste0("https://api.surveymonkey.com/v3/surveys/",survey_id,"/collectors")
content_collectors <- content(GET(collectors,add_headers(Authorization = paste0("Bearer ", sm_api_key))),"parsed")

content_collectors$data[[1]]$id  # the id of the first collector of your survey

#########################################################################

```

## Adapter le tableau de bord à votre propre sondage

Le script doit être adapté aux questions de votre sondage. Trois types de questions sont traitées ici. Ils ne couvrent pas l'ensemble des types de questions possibles proposés par SurveyMonkey qui peuvent nécessiter une modification spécifique du script. 

### Mettre à jour le nom des colonnes du tableau de données

Lorsque l'application met à jour les données via l'API de SurveyMonkey, les données sont stockées dans un data frame appelé *data* lui-même stockés dans le fichier *data_ef_d1.rds*.

Dans SurveyMonkey, chaque valeur des questions de type *case à cocher* (plusieurs valeurs peuvent être sélectionnées) possède une colonne spécfique. Par exemple, pour la question 2 (voir survey_ef_d1.pdf) *Dans quel établissement enseignez-vous?, les répondants peuvent indiqués 1 ou plusieurs degrés (de 1P à 12S) (voir code ci-dessous).

Au contraire, les questions de type *choix multiple* (une seule valeur parmi celles proposées peuvent être sélectionnées) possèdent une seule colonne correpondant à la valeur choisie. Par exemple, pour la question 3 (voir survey_ef_d1.pdf) *Quand je participe à une formation continue, j'essaie d'apprendre le plus possible*,les répondants peuvent choisir une seule réponse sur l'échelle de Likert proposée et la colonne correspondante est *motivation* (voir code ci-dessous).

Les questions de type *Matrice/échelle d'évaluation* sont similaires aux questions *choix multiple*. Chaque question de la matrice possède une seule colonne correspond à la valeur choisie. Par exemple, la question 5 (voir survey_ef_d1.pdf) *Le contenu de la formation d'aujourd'hui*, les colonnes correspondantes sont *contenu_riche* et *contenu_adapte". Pour cette matrice, la possiblité de laisser un commentaire a également été ajouté, celui-ci possède également une colonne spécifique *appreciation_generale_commentaires*.


```R
#Questions

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
```



## Mettre en ligne l'application

L'application peut fonctionner en local et en ligne. Pour la rendre disponible en ligne, la solution la plus simple est de la télécharger sur la plateforme https://www.shinyapps.io. Pour cela, dans R Studio, cliquer sur le bouton bleu *Publish options* à côté du bouton Run app. Cliquez sur Add New Account puis ShinyApps.io puis suivre la procédure indiquée. Une fois le compte ajouté, sélectionnez le fichier ef_phase1_dashboard.R. Si vous avez déjà enregistré une partie des données localement, ajoutez également le fichier data_ef_d1.rds. Donnez un titre à votre application et cliquez sur Publish. Par défaut, l'application s'ouvre dans votre navigateur par défaut lorsque la compilation est terminée. 


### ef_phase2_dashboard.R

Le tableau de bord ef_phase2_dashboard.R est similaire au tableau de bord ef_phase1_dashboard.R. Seules l'intitulé des questions posées dans le questionnaire changent. 
