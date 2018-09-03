# Functional Dependence and Depression

This repo contains `R` scripts to analyze the links between depression and functional dependence using the [SHARE](http://www.share-project.org/) panel data survey.

## Data
The scripts expect CSV files that are subsequently imported by `R`. We cannot redistribute data files as per the SHARE project’s [conditions of use](http://www.share-project.org/data-access/share-conditions-of-use.html).

## Necessary Packages
The scripts assume/require the following packages to be installed in `R`:
- [`plyr`](http://had.co.nz/plyr/)
- [`lme4`](https://github.com/lme4/lme4/)
- [`lmerTest`](https://github.com/runehaubo/lmerTestR)

## Optional packages
- [`ggplot2`](https://ggplot2.tidyverse.org/) if plots are required
- [`future`](https://github.com/HenrikBengtsson/future) in for easier multicore/multiprocess evaluations