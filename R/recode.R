#### This is the script which recodes and creates new variables

### Load the "subsetted" data
load(file = "../data/shareAllSubset.RData")

### Load script containing some useful basic functions
source(file = "recodeFunctions.R")

### Clean up all columns
shareAllSubsetRecode <- as.data.frame(lapply(shareAllSubset,
                                             SetMissing))
                                      
### Any ".x" columns should be made stripped of the suffix
names(shareAllSubsetRecode) <- gsub("\\.x", "",
                                    names(shareAllSubsetRecode))

### Any ".y" columns should be denoted as applying to wave 3
names(shareAllSubsetRecode) <- gsub("\\.y", ".w3",
                                    names(shareAllSubsetRecode))

### Make sure the data frame is sorted by mergeid and wave
shareAllSubsetRecode <-
    shareAllSubsetRecode[order(shareAllSubsetRecode$mergeid,
                               shareAllSubsetRecode$wave), ]

### Generate dropout indicator
source(file = "dropout.R")

### Generate sociodemographic variables
source(file = "socioDemo.R")

### Generate childhood socioeconomic condition variables
source(file = "childSEC.R")

### Generate variables related to ACE
source(file = "ACE.R")

### Generate variables related to HACE
source(file = "HACE.R")

### Generate physical activity variables
source(file = "physAct.R")

### Correct coding of EURO-D depression scale
source(file = "depression.R")

### Correct coding of ADL
source(file = "ADL.R")

### Correct coding of IADL
source(file = "IADL.R")
