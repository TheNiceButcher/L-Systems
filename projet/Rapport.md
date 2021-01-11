### Equipe
L'équipe pour ce projet est constituée de:
* HAMDANI Harris hamdanih 22014430
* DJELLOUL ABBOU Mohammed Adel djelloul 22018734

### Fonctionnalités
Les fonctionnalités implémentées sont les suivantes :
- Lecture d'un L-Systeme via un fichier
- Lecture d'un L-Systeme via l'invite de commande
- Interprétation à la volée
-

### Compilation
La compilation s'effectue via la commande `make` et l'exécution est lancée avec la commande `./run`.

### Découpage modulaire
Le projet contient de nombreux modules dont nous allons vous préciser l'utilité:
- `Turtle` : module gérant les commandes et les positions dans les L-systèmes.
- `Systems` : module gérant les L-systèmes, en général, faisant appel à `Turtle`, pour tout ce qui est commande et position.
- `Read_files` : module permettant de récupérer d'un fichier des informations pour créer un nouveau L-système. Ce module est bien évidemment indispensable, pour pouvoir créer un L-système à partir d'un fichier.

### Répartition du travail

 Ce projet a été entièrement réalisé par Harris.
 Ceci est la conséquence de nombreux projets, il a fallu faire des choix sur la répartition des tâches parmi les différents projets. Au départ, Adel devait s'occuper de la partie graphique du programme. Mais le temps ne vous a pas permis de faire cela.

 En ce qui concerne la chronologie du travail sur ce projet, on peut mettre en évidence 2 phases :
 * Le mois de novembre et début décembre
 * Début janvier.
Le "reconfinement" de novembre nous a permis de découvrir le sujet et de nous familiariser avec les L-systèmes, les tortues et autres concepts explorés dans ce projet.
Puis, après un mois de décembre rempli de révisions pour les examens, de Devoir Maison à rendre ou encore de projets à rendre, on s'est remis au travail au début du mois de janvier, une fois ces tempêtes passées. On a pu alors s'attaquer à la lecture des fichiers, afin de récupérer les L-Systèmes de l'utilisateur et d'interpréter à la volée les L-Systèmes.  
