# testingHMSC

This repository contains some of the code associated to the manuscript "Rethinking Metacommunity Ecology". It has all the scripts associated to the simulations and analysis, but not RDS or data files. All data files should be able to be recreated by the user with the provided scripts. 

If cloning this repository, to get our simulated data, open the repository's project in RStudio, and then run the script `run_all_manuscript.R`, which should save all the outputs in a folder created by the script. 

## manuscript_functions
- [functions:](https://github.com/javirudolph/testingHMSC/tree/master/functions)
  - **metacom_sim_fx.R** includes all the functions necessary to simulate a metacommunity. This model was developed by Dominique
  - **main_sim_fx.R** it uses all the functions for simulating a metacommunity and takes a snapshot of occupancy. The output is a matrix od species occupancy per site. It allows for specification of a quadratic or gaussian response to the environment. 
  - **prep_pars_fx.R** is a function to organize the parameters in appropriate format for input into the full process. This function is set up for one environmental variable for now, but it can be changed. Some defaults are set for certain parameters, which can be checked within the script. 
  - **lanscape_fx.R** these functions are to simulate a landscape. However, we are using the set landscape provided by Guillaume in the `fixed_landscapes` folder. It is possible to change this, run a new landscape simulation and select significant MEMs from that new simulation. 
  - **output_processing_fx.R** these functions are all designed to organize the output from HMSC to visualize and analyze data. 
    - `get_sites_data` and `get_species_data` will organize the variation partitioning 'overlaps' output from `variPart`
    - `species_plot` and `sites_plot` will create the default ternary plots for the variation partitioning output.
    - `interaction_plot` will take the hmsc fit model and calculate the species interaction matrix. This is using the `corRandomEff` function from the `HMSC` package.
  - **full_process_fx.R** these will generate the metacommunity simulation based on the given set of parameters, it will fit the hmsc and get an output model, also run the variation partitioning component. This script provides the full process for simulations and fitting. Any plotting, analysis and data wrangling is found in the output processing scripts.

To save space and avoid issues we had previously, this repository doesn't track any pdf, or image files, just scripts. So, if you wanted to see the figures for previous explorations you need to run the independent projects within these folders and create the figures again.

Some information on the folders:  
* [**old**](https://github.com/javirudolph/testingHMSC/tree/master/old) we are not using these anymore, but they show the history of our work. These include: 
     * [**HMSC-DGravel2016**](https://github.com/javirudolph/testingHMSC/tree/master/old/hmsc-DGravel2016) - this project contains the original scripts provided in the dropbox folder in January 2019.  
     * [**update1_Jan18**](https://github.com/javirudolph/testingHMSC/tree/master/old/update1_jan18) - this project uses the original functions, but edits were made to the parameters in the simulations. These data files are the ones used for the manuscript, and data for plotting and further analysis can be found in the [dataWrangling](https://github.com/javirudolph/testingHMSC/tree/master/update1_jan18/dataWrangling) folder. Updated figures can be found in this [repository](https://github.com/javirudolph/testingHMSC/tree/master/update1_jan18/newFigures/allFigs) as well. 
     * [**testingHMSCv2**](https://github.com/javirudolph/testingHMSC/tree/master/old/testingHMSCv2) - new simulations/scenarios and work associated to the 2019 cycle will be found here. We have changed the response to the environment in the simulations, from a gaussian response to a quadratic response (details can be found [here](https://github.com/javirudolph/testingHMSC/blob/master/aboutScripts-FAQ/environmentOnSpeciesOccupancy.md). A comparisson between these two response types is described in the [reports](https://github.com/javirudolph/testingHMSC/tree/master/old/testingHMSCv2/reports), specifically the ones for fifteen species ([Fig2](https://github.com/javirudolph/testingHMSC/blob/master/old/testingHMSCv2/reports/fifteen_spp_figure2.md) and [Fig3](https://github.com/javirudolph/testingHMSC/blob/master/old/testingHMSCv2/reports/fifteen_spp_figure3.md) )
    
* [**aboutScripts-FAQ**](https://github.com/javirudolph/testingHMSC/tree/master/aboutScripts-FAQ) - this folder has some questions/answers regarding the code, the functions and some of the provided data. Examples such as a quick view of the landscape for simulations, visualization tests and a description of functions used for simulations are provided.  
   * The landscape used for all of our simulations is visualized [here](https://github.com/javirudolph/testingHMSC/blob/master/aboutScripts-FAQ/whatsOurLandscapeLike.md)  
   * A description of the functions and model used to simulate our metacommunity data is found [over here](https://github.com/javirudolph/testingHMSC/blob/master/aboutScripts-FAQ/metacom_sims_functions.md). This basically helps visualize the appendix section of the manuscript.  
   * A failed attempt to move away from ternary plots is shown [here](https://github.com/javirudolph/testingHMSC/blob/master/aboutScripts-FAQ/ideasVisualizeVP.md)  
   * To visualize the response to the environment we compared a gaussian and quadratic term used in the `S_f` function of the  metacommunity model we made [figures](https://github.com/javirudolph/testingHMSC/blob/master/aboutScripts-FAQ/environmentOnSpeciesOccupancy.md)  
   


