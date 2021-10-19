SummariseMPD
================

``` r
library(Casal2, warn.conflicts = F)
library(r4Casal2, warn.conflicts = F)
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.5     v purrr   0.3.4
#> v tibble  3.1.3     v dplyr   1.0.7
#> v tidyr   1.1.3     v stringr 1.4.0
#> v readr   2.0.0     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(ggplot2)
```

### Introduction

to see these vignettes type in `browseVignettes("r4Casal2")` or
`vignette("r4Casal2")` or compile it yourself with knitr. Also hopefully
all functions are documented so to query the inputs to a function use
the `?` i.e. `?plot.pressure`

## Plotting pressures

``` r
file_name = system.file("extdata", "estimate.out", package = "r4Casal2", mustWork = TRUE)
mpd = extract.mpd(file = file_name)
# Report labels
# names(mpd) 
# plot fishing pressures
my_plot = r4Casal2::plot.pressure(model = mpd, report_label = "Instantaneous_Mortality")
# this will generate a generic ggplot
print(my_plot)
```

![](C:/Users/marshc/AppData/Local/Temp/Rtmp8Cik5B/preview-4644e851be9.dir/SummariseMPD_files/figure-gfm/pressures-1.png)<!-- -->

Flexibility using standard ggplot functions

``` r
# you can add adjust it as you please, for example if you want 3 panels for each fishery
my_plot + 
  facet_wrap(~Fishery) + 
  theme(axis.text.x = element_text(angle = 90))
```

![](C:/Users/marshc/AppData/Local/Temp/Rtmp8Cik5B/preview-4644e851be9.dir/SummariseMPD_files/figure-gfm/pressures_alt-1.png)<!-- -->

``` r
# Adjust ylim and add a reference limit
my_plot + ylim(0,0.05) + geom_hline(yintercept = 0.03, linetype = "dashed")
```

![](C:/Users/marshc/AppData/Local/Temp/Rtmp8Cik5B/preview-4644e851be9.dir/SummariseMPD_files/figure-gfm/pressures_alt_again-1.png)<!-- -->

## Multiple runs with -i Casal2 formulation

``` r
file_name = system.file("extdata", "multi_run.out", package = "r4Casal2", mustWork = TRUE)
mpd = extract.mpd(file = file_name)
#> loading a run from -i format
# Report labels
# names(mpd) 
# plot fishing pressures
my_plot = r4Casal2::plot.pressure(model = mpd, report_label = "Instantaneous_Mortality")
#> [1] "multi iteration report found"
my_plot = my_plot + theme(axis.text.x = element_text(angle = 90))
# this will generate a generic ggplot
print(my_plot)
```

![](C:/Users/marshc/AppData/Local/Temp/Rtmp8Cik5B/preview-4644e851be9.dir/SummariseMPD_files/figure-gfm/pressures_multi-1.png)<!-- -->
