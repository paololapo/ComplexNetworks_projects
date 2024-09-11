# Social Connectedness Index (#44)
This folder contains the material for the analysis of the *Social Connectedness Index* from Facebook. The files containing the results of the simulations can be found in the ```data``` folder, except those that refer to the whole world (in the .gitignore file due to their sizes). </br>
This folder is divided into different files for different purposes. In particular:
* ```singleCountries.R``` is a script to obtain the single networks (node metadata and edge list) for all the countries in the world that are divided in more than one geographical are (except the USA).
* ```fullWorld.R``` is a script to obtain the network for the full world (including the USA). 
* ```networkComparison.ipynb``` and ```networkComparison.R``` contain the simple pipelines implemented to analyze both the single country networks and the whole world network. 