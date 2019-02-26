## Testing HMSC version 2.0

This project contains edits and modifications to test the performance of HMSC type models and the variation partitioning approach in a metacommunity framework.

This directory is organized in the following way:
 * The [functions](https://github.com/javirudolph/testingHMSC/tree/master/testingHMSCv2/functions) folder has all the functions associated to the simulations, model fitting, variation partitioning and plotting. 
 
 
 
### Outline the process

1. Simulate an environment and keep it constant for all simulations. You can do this by using the functions in the [landscape](https://github.com/javirudolph/testingHMSC/blob/master/testingHMSCv2/functions/landscape_fx.R) script. Make sure to use a seed and save the .RDS files for use in the following functions.

1. We set the desired parameters as a list. We can do this with the [prep_pars function](https://github.com/javirudolph/testingHMSC/blob/master/testingHMSCv2/functions/prep_pars_fx.R)

1. Use that list of parameters to go into the [main_sim function](https://github.com/javirudolph/testingHMSC/blob/master/testingHMSCv2/functions/main_sim_fx.R) which will generate the simulated metacommunity. The output is a matrix of species occurrence in each site.
    1. The process functions used in the main simulation can be found in the [metacom_sim_fx script](https://github.com/javirudolph/testingHMSC/blob/master/testingHMSCv2/functions/metacom_sim_fx.R) and a description of each of these functions can be found [here](https://github.com/javirudolph/testingHMSC/blob/master/aboutScripts-FAQ/metacom_sims_functions.md)

1. Fit HMSC

1. Variation Partitioning

1. Data wrangling

1. Plotting

1. Render reports with figures and associated table of parameters
