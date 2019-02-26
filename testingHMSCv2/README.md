## Testing HMSC version 2.0

This project contains edits and modifications to test the performance of HMSC type models and the variation partitioning approach in a metacommunity framework.

This directory is organized in the following way:
 * The [functions](https://github.com/javirudolph/testingHMSC/tree/master/testingHMSCv2/functions) folder has all the functions associated to the simulations, model fitting, variation partitioning and plotting. 
 
 
 
### Outline the process

1. Simulate an environment and keep it constant for all simulations. You can do this by using the functions in the [landscape](https://github.com/javirudolph/testingHMSC/blob/master/testingHMSCv2/functions/landscape_fx.R) script. Make sure to use a seed and save the .RDS files for use in the following functions.

2. We set the desired parameters as a list. We can do this with the [prep_pars function](https://github.com/javirudolph/testingHMSC/blob/master/testingHMSCv2/functions/prep_pars_fx.R)

3. Use that list of parameters to go into the [main_sim function](https://github.com/javirudolph/testingHMSC/blob/master/testingHMSCv2/functions/main_sim_fx.R) which will generate the simulated metacommunity. The output is a matrix of species occurrence in each site.

4. Fit HMSC

5 Variation Partitioning

6. Data wrangling

7. Plotting

8. Render reports with figures and associated table of parameters
