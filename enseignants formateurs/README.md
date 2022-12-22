# French version

## Fonctionnement général

Les tableaux de bord "enseignants-formateurs" sont des applications R Shiny interactives permettent de disposer d'un retour en temps réel de sondages SurveyMonkey sous forme de visualisations graphiques.

## Mise en marche rapide

Télécharger le tableau de bord de votre choix (ef_phase1_dashboard.R ou ef_phase2_dashboard.R) dans R Studio et les données fictives qui s'y rapportent (data_ef_d1_random.rds ou data_ef_d2_random.rds). Placer le script et les données dans un même fichier. Lancer le tableau de bord avec R Studio. 

# Marche à suivre pour utiliser les tableaux de bord

## Fonctionnement détaillé

### ef_phase1_dashboard.R

Le tableau de bord accède directement aux données d'un sondage SurveyMonkey réalisé préalablement (voir survey_ef_d1.pdf).

Lorsque lancé, le tableau de bord met à jour via l'API de SurveyMonkey les données récoltés. Ces données sont stockés dans un fichier data_ef_d1.rds lu et mis à jour par l'application. 

Pour faire tourner l'application avec des données réelles, vous pouvez donc remplacer dans le script le nom data_ef_d1_random (données fictives) par data_ef_d1. 

## Accès à l'API de SurveyMonkey

L'accès à l'API nécessite la création d'une app sur le compte https://developer.surveymonkey.com/apps/

Pour se connecter à l'API, vous devez récupérer les identifiants de votre app SurveyMonkey et les utiliser pour compléter le script :

- Access Token (sm_api_key)
- Secret (sm_secret)
- Client ID (sm_client_id)

## Trouver les identifiants de votre sondage et votre collecteur

Votre sondage SurveyMonkey possède un id spécifique. Votre sondage possède aussi un ou plusieurs collecteurs. Chaque collecteur possède également un id spécifique. Pour récupérer ces id, lancer la partie du script mis en commentaire FIND SURVEYS AND COLLECTOR IDS

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

## Adapter le tableau de bord à votre propre questionnaire





## Mettre en ligne l'application

L'application peut fonctionner en local et online. Pour la rendre disponible online, la solution la plus simple est de l'uploader sur la plateforme https://www.shinyapps.io. Pour cela, dans R Studio, cliquer sur le bouton bleu *Publish options* à côté du bouton Run app. Cliquez sur Add New Account puis ShinyApps.io puis suivre la procédure indiquée. Une fois le compte ajouté, sélectionnez le fichier ef_phase1_dashboard.R. Si vous avez déjà enregistrer une partie des données localement, ajoutez également le fichier data_ef_d1.rds. Donnez un titre à votre application et cliquez sur Publish. Par défaut l'application s'ouvre lorsque la compilation est terminée. 


### ef_phase2_dashboard.R

Le tableau de bord ef_phase2_dashboard.R est similaire au tableau de bord ef_phase1_dashboard.R. Seules les questions posées dans le questionnaire changent. 
