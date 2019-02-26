
# Testing the whole process from the functions developed in the 'full_process_fx.R'



# Source functions --------------------------------------------------------

# Function to organize parameters in a list
source("functions/prep_pars_fx.R")

# Individual functions to simulate the metacommunity
source("functions/metacom_sim_fx.R")

# The main function that actually outputs the metacommunity
source("functions/main_sim_fx.R")

# You could generate a landscape with the functions provided in:
# source("functions/main_sim_fx.R")
# Or use the fixed landscape and MEMs from
# source("runScripts/fixedLandscape_and_MEMs.R")
# Or, if available locally, just load the RDS files saved from that script in the outputs folder
# These RDS files should include the XY, E and MEMsel