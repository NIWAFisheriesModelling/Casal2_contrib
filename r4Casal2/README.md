# r4Casal2
An R package that extends the functionality of the base [Casal2](https://github.com/NIWAFisheriesModelling/CASAL2) R package, to aid in visualising, model reporting and diagnostics. It depends on the latest `Casal2` R package which is not on CRAN you can download the latest package from [here](https://github.com/NIWAFisheriesModelling/CASAL2/tree/master/R-libraries) or try and use the command below. The Casal2 base package is used to read and write Casal2 output and configuration files, where as `r4Casal2` is more visualising summarising that we hope other users will contribute to.
```r
install_github(https://github.com/NIWAFisheriesModelling/CASAL2/tree/master/R-libraries/casal2, ref = "HEAD")
```


## Installation `r4Casal2`
`r4Casal2` is a recent package so may require package updates if you don't update your `R` environment. It is recommended to successfully install the following packages before installing `r4Casal2`
```r
install.packages(c("reshape2", "dplyr", "ggplot2", "mvtnorm", "DHARMa","gridExtra","MASS"))
```

Once successfully installed the above run the following code.
```r
devtools::install_github("NIWAFisheriesModelling/Casal2_contrib/r4Casal2", build_vignettes  = TRUE)
```
We have found users with old R versions and R package versions, that this may fail when building the `vignettes`. If this happens try installing without the vignettes
```r
devtools::install_github("NIWAFisheriesModelling/Casal2_contrib/r4Casal2", build_vignettes  = FALSE)
```
An issue you might find when installing this is the Casal2 R package that is not on hosted on CRAN i.e. `library(Casal2)`. The package should be part of the installation or zip package that you recieved with your Casal2 executable, which can be downloaded from here. [here](https://casal2.github.io/casal/).

## Query Functionality
Once the library is installed you can query the functionality `library(help="r4Casal2")` also see the vignette with the command `browseVignettes("r4Casal2")`

## Issues
If you have an issues please create a github issue, or reach out on the [discourse page](https://casal2.discourse.group/) 

