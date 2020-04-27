
  - **metacom_sim_fx.R** includes all the functions necessary to simulate a metacommunity. This model was developed by Dominique
  - **main_sim_fx.R** it uses all the functions for simulating a metacommunity and takes a snapshot of occupancy. The output is a matrix of species occupancy per site. It allows for specification of a quadratic or gaussian response to the environment. 
  - **prep_pars_fx.R** is a function to organize the parameters in appropriate format for input into the full process. This function is set up for one environmental variable for now, but it can be changed. Some defaults are set for certain parameters, which can be checked within the script. 
  - **lanscape_fx.R** these functions are to simulate a landscape. However, we are using the set landscape provided by Guillaume in the `fixed_landscapes` folder. It is possible to change this, run a new landscape simulation and select significant MEMs from that new simulation. 
  - **output_processing_fx.R** these functions are all designed to organize the output from HMSC to visualize and analyze data. 
    - `get_sites_data` and `get_species_data` will organize the variation partitioning 'overlaps' output from `variPart`
    - `species_plot` and `sites_plot` will create the default ternary plots for the variation partitioning output.
    - `interaction_plot` will take the hmsc fit model and calculate the species interaction matrix. This is using the `corRandomEff` function from the `HMSC` package.
  - **full_process_fx.R** these will generate the metacommunity simulation based on the given set of parameters, it will fit the hmsc and get an output model, also run the variation partitioning component. This script provides the full process for simulations and fitting. Any plotting, analysis and data wrangling is found in the output processing scripts.

To save space and avoid issues we had previously, this repository doesn't track any pdf, or image files, just scripts. So, if you wanted to see the figures for previous explorations you need to run the independent projects within these folders and create the figures again.