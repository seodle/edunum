# French version

## Fonctionnement général

Le tableau de bord "traitement des données" est une application R Shiny interactive destinée à faciliter le traitement de données .csv brutes issues d'un sondage SurveyMonkey

![alt text](screenshot_traitement_donnees.png)

## Marche à suivre pour utiliser le tableau de bord

### Fonctionnement détaillé

#### Téléchargement des données brutes sur SurveyMonkey

Dans SurveyMonkey, allez dans Analysez les résultats puis Enregistrer sous -> Fichier d'exportation -> Toutes les réponses indivduelles -> Exporter avec la configuration suivante.

<center>
  <img src="https://i.imgur.com/mmIrvC5.png" alt="" width="500"/>
</center>

Un fenêtre s'affiche. Cliquez sur le bouton Télécharger, un fichier intitulé Données_Tous_XXXXXX.zip se télécharge. Décompressez ce fichier et ne conservez que le fichier .csv portant comme nom l'intitulé du sondage. Renommez-le textual_data.csv.

Il peut être intéressant de conserver également une copie .pdf du sondage. Pour ce faire allez dans Concevez un sondage et cliquez sur l'icône d'impression en bas à gauche. Définissez les options de votre choix et cliquez sur le bouton Télécharger le PDF.

#### Lancer le tableau de bord

Ouvrir le fichier 




