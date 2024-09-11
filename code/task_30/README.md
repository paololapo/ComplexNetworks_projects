# Axelrod's model for dissemination of culture (#30)
This folder contains the material for the analysis of the *Axelrod's model* for dissemination of culture. The files containing the results of the simulations can be found in the ```data``` folder. </br>
This folder is divided into different files for different purposes. In particular:
* ```simpleDynamics.R``` is a simple file to test the dynamics in R, containing the definition of the functions used in the simple attempt contained in ```scriptLattice.R```. While, the main simulations have been actually done in Python, this script contains also the definition of the plots shown in the report.
* ```dynamics.py``` contains the definition of all the functions used to simulate this model, both on lattices and on arbitrary networks. 
* ```pyscriptLattice.py``` and ```pyscriptNetworks.py``` are the scripts used to run the simulations, preferably in a TMUX session.
* ```social_influcence.ipynb``` contains a few cells to plot the figures, whose definitions are in the ```simpleDynamics.R``` file. 

## Why not use only R
The first attempts were made in plain R. However, at every iteration some hidden data was stored in the RAM, leading to memory problems for long simulations. Even manually calling the garbage collector with ```gc()``` and investigating the objects of the session and their sizes via ```ls()```, it's not trivial to figure out what is that data and how to get rid of it. Calling the garbage collector is actually helping but 1) it leads to an increased simulation time, 2) it does not solve the problem completely. </br> Thus, the original script has been rewritten in Python, maintaining the same structure but optimizing it further. 