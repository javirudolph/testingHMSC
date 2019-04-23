# testingHMSC

This repository contains some of the code associated to the manuscript "Rethinking Metacommunity Ecology". It has all the scripts associated to the simulations and analysis, but not RDS or data files. All data files should be able to be recreated by the user with the provided scripts. 

The master run for the manuscript results is [here](https://github.com/javirudolph/testingHMSC/blob/master/testingHMSCv2/runScripts/MANUSCRIPT-RUNS.R)


Some information on the folders:  
* [**old**](https://github.com/javirudolph/testingHMSC/tree/master/old) we are not using these anymore, but they show the history of our work.
   * [**HMSC-DGravel2016**](https://github.com/javirudolph/testingHMSC/tree/master/hmsc-DGravel2016) - this project contains the original scripts provided in the dropbox folder in January 2019.  
   * [**update1_Jan18**](https://github.com/javirudolph/testingHMSC/tree/master/update1_jan18) - this project uses the original functions, but edits were made to the parameters in the simulations. These data files are the ones used for the manuscript, and data for plotting and further analysis can be found in the [dataWrangling](https://github.com/javirudolph/testingHMSC/tree/master/update1_jan18/dataWrangling) folder. Updated figures can be found in this [repository](https://github.com/javirudolph/testingHMSC/tree/master/update1_jan18/newFigures/allFigs) as well. 
    
* [**aboutScripts-FAQ**](https://github.com/javirudolph/testingHMSC/tree/master/aboutScripts-FAQ) - this folder has some questions/answers regarding the code, the functions and some of the provided data. Examples such as a quick view of the landscape for simulations, visualization tests and a description of functions used for simulations are provided.  
   * The landscape used for all of our simulations is visualized [here](https://github.com/javirudolph/testingHMSC/blob/master/aboutScripts-FAQ/whatsOurLandscapeLike.md)  
   * A description of the functions and model used to simulate our metacommunity data is found [over here](https://github.com/javirudolph/testingHMSC/blob/master/aboutScripts-FAQ/metacom_sims_functions.md). This basically helps visualize the appendix section of the manuscript.  
   * A failed attempt to move away from ternary plots is shown [here](https://github.com/javirudolph/testingHMSC/blob/master/aboutScripts-FAQ/ideasVisualizeVP.md)  
   * To visualize the response to the environment we compared a gaussian and quadratic term used in the `S_f` function of the  metacommunity model we made [figures](https://github.com/javirudolph/testingHMSC/blob/master/aboutScripts-FAQ/environmentOnSpeciesOccupancy.md)  
   
* [**testingHMSCv2**](https://github.com/javirudolph/testingHMSC/tree/master/testingHMSCv2) - new simulations/scenarios and work associated to the 2019 cycle will be found here. We have changed the response to the environment in the simulations, from a gaussian response to a quadratic response (details can be found [here](https://github.com/javirudolph/testingHMSC/blob/master/aboutScripts-FAQ/environmentOnSpeciesOccupancy.md). A comparisson between these two response types is described in the [reports](https://github.com/javirudolph/testingHMSC/tree/master/testingHMSCv2/reports), specifically the ones for fifteen species ([Fig2](https://github.com/javirudolph/testingHMSC/blob/master/testingHMSCv2/reports/fifteen_spp_figure2.md) and [Fig3](https://github.com/javirudolph/testingHMSC/blob/master/testingHMSCv2/reports/fifteen_spp_figure3.md) )
