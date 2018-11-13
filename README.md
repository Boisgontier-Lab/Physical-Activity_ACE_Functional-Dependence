# Functional Dependence and Depression

This repo contains `R` scripts to analyze the links between adverse childhood experiences (ACEs), depression, functional dependence, and physical activity, using the [SHARE](http://www.share-project.org/) panel data survey.

## Data
The scripts expect CSV files that are subsequently imported by `R`.
We cannot redistribute data files as per the SHARE project’s [conditions of use](http://www.share-project.org/data-access/share-conditions-of-use.html).
The SHARE project itself does not provide CSV files.
These need to be exported either from Stata or SPSS.
By default both will export CSV files with UTF-8 encoding but SPSS includes a byte order mark while Stata does not.
Set the relevant file encoding option when importing the CSV files into `R` (see comments in `R/importSHARECSV.R` or `R/importSHARECSVFuture.R`).

## Necessary Packages
The scripts assume/require the following packages to be installed in `R`:
- [`plyr`](http://had.co.nz/plyr/)
- [`lme4`](https://github.com/lme4/lme4/)
- [`lmerTest`](https://github.com/runehaubo/lmerTestR)

## Optional packages
- [`ggplot2`](https://ggplot2.tidyverse.org/) if plots are required
- [`future`](https://github.com/HenrikBengtsson/future) for easier multicore/multiprocess evaluation
