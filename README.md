# This repo has been archived [click here for r4Casal2](https://github.com/NIWAFisheriesModelling/r4Casal2)

# Casal2_contrib
R packages and auxiliary material that assist with interacting and applying [Casal2](https://github.com/NIWAFisheriesModelling/CASAL2) software

## r4Casal2
An R-package for post processing Casal2 output as well as model configurations using ggplot plots. To install see the [readme found here](https://github.com/NIWAFisheriesModelling/Casal2_contrib/tree/main/r4Casal2)
```r
devtools::install_github("NIWAFisheriesModelling/Casal2_contrib/r4Casal2", build_vignettes  = TRUE)
```

## ABM-OM
A folder with R code that show how you can use this [Agent Based Model](https://github.com/Craig44/IBM) (ABM) to simulate data and get Casal2 to reestimate parameters and derived quantities.
Also will show how you can use the ABM as a closed loop MSE with Casal2. Which basically does the first component, but then projects forward an assessment period before waiting for a R-script (which will run a Casal2 estimation) to provide 
future catches for the next assessment period.

### Query Functionality
Once the library is installed you can query the functionality `library(help="r4Casal2")` also see the vignette with the command `browseVignettes("r4Casal2")`


## Collaborations
This repository is a resource for Casal2 users to share graphics and tabulation code for summarising Casal2 models and outputs. Please feel free to share your code to help standardise outputs and improve Casal2 useage.

