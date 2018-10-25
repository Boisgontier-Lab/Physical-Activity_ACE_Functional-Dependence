#### This is the master script that imports data and runs the anylses

### Check if necessary packages are installed

useOptional <- FALSE

if (useOptional) {
    pkgList <- c("plyr", "lme4", "lmerTest", "ggplot2", "future")
} else {
    pkgList <- c("plyr", "lme4", "lmerTest")
}

.chkPkgInstall <- function(...) {
    as.character(unlist(...))

    notInstalled <- ...[which(!(... %in% rownames(installed.packages())))]

    if (length(notInstalled) > 0) {
        cat("The following packages will be installed:\n")

        cat(notInstalled, "\n", sep = "")

        if (askYesNo("Do you want to install them?\n")) {
            install.packages(notInstalled, Ncpus = 4)
        }
        
    }
}

.chkPkgInstall(pkgList)

### Load the libraries
library("plyr")
library("lme4")
library("lmerTest")

if (useOptional) {
    library("ggplot2")
    library("future")
}

### Choose whether or not to use the future package’s parallel
### processsing options; by default set to TRUE
useFuture <- TRUE

if (useFuture) {
    plan(multiprocess)
}

### This calls a script that imports the raw SHARE data
### It assumes that the data is in CSV format
### There is a parllel version that is called when the useFuture options is TRUE
if (useFuture) {
    source(file = "importSHARECSVFuture.R")
} else {
    source(file = "importSHARECSV.R")
}

### Now we merge the individual modules for each wave
source(file ="mergeModules.R")

### Then we append the regular waves and merge in the SHARELife information
source(file = "appendMerge.R")

### Get only the variables we may actually need for the analysis
source(file = "subsetVars.R")

### Recode and generate new variables
source(file = "recode.R",
       echo = TRUE)

