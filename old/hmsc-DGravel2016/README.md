# testingHMSC - original scripts

This folder has its own RStudio Project, so that the working directories are automatically setup. 
The scripts will source functions from this directory, and the file paths are written relative to this project. Therefore, to run these scripts you need to open the **hmsc-DGravel2016.proj** first.

If you want to have all the files in your local computer, you should run the `run_all.R` script, which sources all files and will create all RDS files and figures.


The functions for these simulations are the files: `functions.R` and `main.R` but running the actual simulations is done in the scripts `scenario[Number].R` and the variation partitioning is done with files: `VP_Fig2.R` and `VP_Fig3.R`. The parameters are described for each script, and they all run Hmsc with `niter = 10000, nburn = 5000, thin = 5`.



You can check the results for these files and the figures in the folder figures.
