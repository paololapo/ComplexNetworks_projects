# Complex Networks | projects
In this repository, you can find the simulations and analysis done for the final exam of the *Physics of Complex Network: Structure and Dynamics* course in the *Physics of Data* master's program, University of Padova, AY 2023/24. </br>
In particular, you can find the code and the report for three different tasks.

* **SOC model (#15)** </br>
Theoretical characterization and numerical analysis of the *Bak–Tang–Wiesenfeld sandpile model* for self-organized criticality. This is a well-known mechanism leading to complex behaviors and power-law distributed events. **Score: 0.6.**

* **Axelrod's model for dissemination of culture (#30)** </br>
Simulation of the social influence and consensus formation, describing how local convergence can lead to global polarization. This model from sociophysics exhibits an order-disorder phase transition in cultural diffusion. **Score: 0.5.**

* **Social Connectedness Index (#44)** </br>
The SCI is a measure of the strength of the connection between pairs of geographical regions, based on an anonymized snapshot of active Facebook users. Analysis and comparison of the networks obtained at the country level. **Score: 1.0.**


## About reproducibility
Running the numerical simulations can be computationally challenging. Thus, the calculations were made by exploiting the <a href=https://cloudveneto.it/> CloudVeneto </a> computational resources and writing the code to run parallel over multiple process units. The virtual machine runs on an *AMD EPYC 7413* processor, with the instance configured to use 8 single-threaded cores across 8 sockets. The system has 15.6 GB of RAM and 8 GB of swap memory. </br>
The code is written both in R (version 4.1.2, *"Bird Hippie"*) and in Python (version 3.10.12). Below is a non-exhaustive list of the packages and libraries used. R: *BioStatR* (4.0.1), *doParallel* (1.0.17), *dplyr* (1.1.4), *eurostat* (4.0.0), *foreach* (1.5.2), *geodata* (0.6-2), *igraph* (2.0.3), *terra* (1.7-78), *tidyr* (1.3.1), *tigris* (2.1). Python: *networkx* (3.3), *numpy* (2.0.1), *pandas* (2.2.2), *powerlaw* (1.5).

