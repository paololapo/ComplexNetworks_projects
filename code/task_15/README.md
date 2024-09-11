# SOC model (#15)
This folder contains the material for the analysis of the *Bak–Tang–Wiesenfeld sandpile model* for self-organized criticality. Unfortunately, the files containing the results of the simulations are too heavy to be shared in GitHub and thus they are in folders hidden by the .gitignore. </br>
This folder is divided into different files for different purposes. In particular:
* ```simpleAttempt.R``` is a simple script to test the dynamics in R. However, the main simulations have been done in Python.
* ```sandpileDynamics.py``` contains the definition of all the functions used to simulate this model, both in simple and interdependent networks. 
* ```pyscript.py``` and ```pyscriptCoupled.py``` are the scripts used to run the simulations, preferably in a TMUX session. 
* ```tauAnalysis.ipynb``` and ```tauAnalysis.py``` contain the results of the simulations, in terms of power-law distributed avalanche sizes.
* ```coupledAnalysis.ipynb``` and ```coupledAnalysis.R``` contain the results of the simulations for interdependent networks. 