# Méthodes biostatistiques

Dépôt de données et de matériel pour la formation au logiciel R du Cancéropôle proposée par [RITME Academy](http://www.ritme.com).

## Pré-requis

Si R n'est pas déjà installé sur votre machine, vous pouvez télécharger la dernière version sur le site [CRAN](http://cran.r-project.org). Le logiciel [RStudio](http://www.rstudio.com) peut également être installé afin de faciliter l'édition des scripts R et la visualisation des données tabulaires et des sorties graphiques. Le cas échéant, un éditeur de texte supportant l'édition de script R tel que [Atom](https://atom.io), [VS Code](https://code.visualstudio.com) ou [Sublime Text](https://www.sublimetext.com) devrait être suffisant.

## Organisation

Le répertoire [handout](https://bitbucket.org/chlalanne/rstats-canceropole/src/master/handout/) contient l'ensemble des documents de travail. Il s'agit de fichiers HTML (standalone, donc qui peuvent être copiés sur une clé USB ou envoyés par mail) avec le code R et les sorties. Tous les exemples ont été testés avec R version 3.4.3 (2017-11-30) sur Mac OS X 10.13.4. Les packages nécessaires à l'exécution des commandes R sont mentionnés dans chaque document.

## Fichiers de données

Les différents jeux de données utilisés pour illustrer les concepts abordés dans cette formation sont soit accesibles depuis des packages installés avec R, soit dans le répertoire [data](https://bitbucket.org/chlalanne/rstats-canceropole/src/master/data/) de ce dépôt. Pour les sources de données externes disponibles dans ce répertoire, l'origine ou la source du fichier est mentionnée dans un fichier à part.


## Ressources

Les traitements réalisés avec R font extensivement appel aux packages [Hmisc](https://cran.r-project.org/web/packages/Hmisc/index.html) et [rms](https://cran.r-project.org/web/packages/rms/index.html) pour la modélisation statistique et au package [ggplot2](http://ggplot2.tidyverse.org) pour les visualisations graphiques. Le reste des commandes R se conforme au langage R de base (et non au [tidyverse](https://www.tidyverse.org) en vogue).

- [Biostatistics for Biomedical Research](http://fharrell.com/doc/bbr.pdf) 
- Ressources connexes : [site de référence BBR](http://biostat.mc.vanderbilt.edu/wiki/Main/ClinStat), [site de référence RMS](http://biostat.mc.vanderbilt.edu/wiki/Main/RmS)
