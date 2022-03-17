# ABM-OM
A folder with R code and configuration files that show how you can use this [Agent Based Model](https://github.com/Craig44/IBM) (ABM) to simulate data and get Casal2 to reestimate parameters and derived quantities.
Also will show how you can use the ABM as a closed loop MSE with Casal2. Which basically does the first component, but then projects forward an assessment period before waiting for a R-script (which will run a Casal2 estimation) to provide 
future catches for the next assessment period.

## Dependencies
In order to Run this code, you will need both Casal2.exe (and relavent .dll or .so) in your path, in addition to the ABM.exe. It is recommended that you 
clone the repo and install but if not you can download it here
If running the ABM with MSE mode you will need the following R-packages installed in R for it to run
`install.packages(c("RInside","Rcpp"));`


## Collaborations
This repository is a resource for Casal2 users to share graphics and tabulation code for summarising Casal2 models and outputs. Please feel free to share your code to help standardise outputs and improve Casal2 useage.

