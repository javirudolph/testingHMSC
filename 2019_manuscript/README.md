# 2019 Manuscript Figures

This repo contains the functions and information necessary to generate the figures included in the manuscript. 


## Subdirectories
- [functions:](https://github.com/javirudolph/testingHMSC/tree/master/2019_manuscript/functions)
  - metacom_sim_fx.R includes all the functions necessary to simulate a metacommunity. This model was developed by Dominique
  - main_sim_fx.R it uses all the functions for simulating a metacommunity and takes a snapshot of occupancy. The output is a matrix od species occupancy per site. It allows for specification of a quadratic or gaussian response to the environment. 
  - prep_pars_fx.R is a function to organize the parameters in appropriate format for input into the full process. This function is set up for one environmental variable for now, but it can be changed. Some defaults are set for certain parameters, which can be checked within the script. 
  - lanscape_fx.R these functions are to simulate a landscape. However, we are using the set landscape provided by Guillaume. It is possible to change this, run a new landscape simulation and select significant MEMs from that new simulation. 
  - output_processing_fx.R these functions are all designed to organize the output from HMSC to visualize and analyze data. 
    - `get_sites_data` and `get_species_data` will organize the variation partitioning 'overlaps' output from `variPart`
    - `species_plot` and `sites_plot` will create the default ternary plots for the variation partitioning output.
    - `interaction_plot` will take the hmsc fit model and calculate the species interaction matrix. This is using the `corRandomEff` function from the `HMSC` package.
