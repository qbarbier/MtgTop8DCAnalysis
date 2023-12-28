# Analyse du format Duel Commander d'après les données de MTG-TOP8

## Résumé
Magic The Gathering (MTG) est un jeu de cartes stratégique où les joueurs s'affrontent avec des decks personnalisés. Le format Duel Commander (DC) implique des duels un contre un avec des decks de 100 cartes et une liste de cartes interdites. Le méta-jeu, ou "meta-game", représente les tendances des decks et des stratégies populaires au sein de la communauté, influençant les choix des joueurs pour adapter leurs decks et leurs tactiques en fonction de ce qui est courant ou dominant sur la scène compétitif. MTG-top8 est un site populaire pour les joueurs de Magic: The Gathering. Il suit les résultats des tournois, donnant des détails sur les decks et stratégies utilisés par les meilleurs joueurs. Cela aide les joueurs à comprendre les tendances du jeu compétitif et à améliorer leurs propres decks. C'est la principale source en ligne pour les données du format Duel Commander.En se basant sur des données extraites automatiquement depuis ce site par le biais du scraping, nous avons réalisé une analyse des meilleurs decks du format en tenant compte de l'importance des événements et des périodes. Pour visualiser et parcourir ces données, nous avons utilisé la technologie R Shiny pour proposer une interface intuitive affichant le classement des decks compétitifs. De plus, grâce aux informations sur chaque carte unique utilisée dans ces decks, nous pouvons poursuivre l'analyse en réalisant des regroupements d'archétypes et en identifiant les cartes les plus présentes dans le format et/ou dans les différents types de decks.

## Database and Knowledge

Le format Duel Commander

MTG-Top8

Précédent travaux et communauté (Aliquanto)

## Materiel et Methode

Collecte de données depuis MTGTOP8 : scraping et traitement des données

Développement de l'application Shiny pour l'analyse des données du méta-jeu

Choix des outils et des technologies pour la visualisation des données et l'analyse des données (machine leanrning)

## Resultats

Présentation de l'interface de l'application Shiny développée

Analyse des meilleurs decks en fonction de l'importance des événements et des périodes, mais la méta étant evolutif avec les nouvelles sortie, et les banliste, il est important de données que les résultats de tier list a cette instant ne seras plus valable. C'est aussi pour cette raison que nous avons fait un outils shiny qui lui va évoluer avec les sortie et les banlist en ce basant sur mtgtop8.

Clustering des archétypes et identification des cartes les plus présentes dans le format et les types de decks

## Conclusion 
Récapitulation des contributions de l'étude : développement d'une application Shiny pour analyser le méta-jeu du format Duel Commander

Importance et implications des résultats pour la communauté Magic: The Gathering

Pistes pour de futures recherches dans le domaine de l'analyse du méta-jeu



