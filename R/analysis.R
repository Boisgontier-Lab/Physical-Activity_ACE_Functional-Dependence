#### Script that actually performs the analysis in the paper

### Reload the recoded data frame
load(file = "../data/shareAllSubsetRecode.RData")

### Keep only the necessary variables
shareFunctionalDep <- shareAllSubsetRecode[, c("mergeid", "wave", "ageBase",
                                               "age_int", "genderMode",
                                               "birthCo", "dropout",
                                               "childSECFact", "ACEScore15Cat3",
                                               "HACEScore15Bin", "depression",
                                               "country", "physActBinMean",
                                               "adl", "iadl")]

### Remove everything in workspace *except* the above data frame
rm(list = grep("shareFunctionalDep", ls(),
               value = TRUE,
               fixed = TRUE,
               invert = TRUE))

### Some extra data preparation

## Get rid of observations where participants are too old or too young
shareFunctionalDep <- subset(shareFunctionalDep, age_int > 49, age_int < 97)

## Get rid of participants from two oldest birth cohorts
shareFunctionalDep <- shareFunctionalDep[which(!(shareFunctionalDep$birthCo %in%
                                                 c("before 1913",
                                                   "between 1913 and 1918"))), ]

## Merge together the two youngest cohorts
shareFunctionalDep$birthCo <- revalue(shareFunctionalDep$birthCo,
                                      c("after 1955" = "after 1945",
                                        "between 1946 and 1955" = "after 1945"))

## Get rid of unused factor levels
shareFunctionalDep$birthCo <- factor(shareFunctionalDep$birthCo)

## Center wave variable at wave 1
shareFunctionalDep$waveCentW1 <- shareFunctionalDep$wave - 1

## Squared version
shareFunctionalDep$waveCentW1Square <- shareFunctionalDep$waveCentW1^2

## Center baseline age measurement
shareFunctionalDep$ageBaseCent <- shareFunctionalDep$ageBase - 63.50

## Set reference levels for factor variables
vars <- c("birthCo", "childSECFact", "country")
refLev <- c("after 1945", "Most disadvantaged", "Belgium")

for (i in c(1:3)) {
    shareFunctionalDep[, vars[i]] <- relevel(shareFunctionalDep[, vars[i]],
                                             refLev[i])
}

rm(i, vars, refLev)

MakeFormula <- function(depVar = NULL, indepVars = NULL,
                        idVar = NULL, randSlopeVars = NULL) {

    indepVars <- paste(indepVars, collapse = " + ")

    ret <- paste(depVar, indepVars, sep = " ~ ")

    if (is.null(randSlopeVars)) {
        ranef <- paste0("(",
                        paste("1 |",
                              idVar,
                              ")"))
    } else {
        ranef <- paste("(",
                        paste("1",
                              paste(randSlopeVars,
                                    collapse = " + "), sep = " + "),
                        "|",
                        idVar,
                        ")")
    }
    

    ret <- paste(ret, ranef, sep = " + ")

    ret <- as.formula(ret)

    ret
}

#### The independent variables for each model

### m1 corresponds to the direct effect of ACE on ADL
### m2 corresponds to the direct effect of ACE on depression
### m3 corresponds to the effect of ACE on ADL taking into account depression
### m4 through m6 are the same except for the addition of an interaction between
### ACE and the level of physical activity
indepVarsList <- list(m1 = c("waveCentW1*ageBaseCent",
                             "waveCentW1Square*ageBaseCent",
                             "genderMode", "country",
                             "birthCo", "dropout", "childSECFact",
                             "HACEScore15Bin",
                             "ACEScore15Cat3"),
                      m2 = c("waveCentW1*ageBaseCent",
                             "waveCentW1Square*ageBaseCent",
                             "genderMode", "country",
                             "birthCo", "dropout", "childSECFact",
                             "HACEScore15Bin",
                             "ACEScore15Cat3"),
                      m3 = c("waveCentW1*ageBaseCent",
                             "waveCentW1Square*ageBaseCent",
                             "genderMode", "country",
                             "birthCo", "dropout", "childSECFact",
                             "HACEScore15Bin",
                             "ACEScore15Cat3", "depression"))

indepVarsList <- c(indepVarsList,
                   list(
                       m4 = gsub("(ACEScore15Cat3)",
                                 "\\1*physActBinMean",
                                 indepVarsList[["m1"]]),
                       m5 = gsub("(ACEScore15Cat3)",
                                 "\\1*physActBinMean",
                                 indepVarsList[["m2"]]),
                       m6 = gsub("(ACEScore15Cat3)",
                                 "\\1*physActBinMean",
                                 indepVarsList[["m3"]]),
                       full = c("waveCentW1",
                                "waveCentW1Square",
                                "ageBaseCent",
                                "country",
                                "birthCo",
                                "dropout",
                                "childSECFact",
                                "ACEScore15Cat3",
                                "HACEScore15Bin",
                                "physActBinMean",
                                "depression",
                                "genderMode")))

### Get the final samples for ADL, and for IADL

## High physical activity as reference
finalSampleADL <-
    lme4::lFormula(formula =
                       MakeFormula(depVar = "adl",
                                   indepVars =
                                       indepVarsList[["full"]],
                                   idVar = "mergeid"),
                   data = shareFunctionalDep,
                   REML = FALSE,
                   na.action = na.omit)$fr

finalSampleIADL <-
    lme4::lFormula(formula =
                       MakeFormula(depVar = "iadl",
                                   indepVars =
                                       indepVarsList[["full"]],
                                   idVar = "mergeid"),
                   data = shareFunctionalDep,
                   REML = FALSE,
                   na.action = na.omit)$fr

## Low physical activity as reference
finalSampleADLLowPA <- finalSampleADL
finalSampleADLLowPA$physActBinMean <-
    relevel(finalSampleADLLowPA$physActBinMean,
            "low physical activity")

finalSampleIADLLowPA <- finalSampleIADL
finalSampleIADLLowPA$physActBinMean <-
    relevel(finalSampleIADLLowPA$physActBinMean,
            "low physical activity")

### List of the formulas for the models we'll be running

for (dep in c("adl", "iadl")) {
    listName <- paste0("formulaList", toupper(dep))
    assign(listName,
           list(m1 =
                    MakeFormula(depVar = dep,
                                indepVars = indepVarsList[["m1"]],
                                idVar = "mergeid",
                                randSlopeVars = c("waveCentW1")),
                m2 =
                    MakeFormula(depVar = "depression",
                                indepVars = indepVarsList[["m2"]],
                                idVar = "mergeid",
                                randSlopeVars = c("waveCentW1")),
                m3 =
                    MakeFormula(depVar = dep,
                                indepVars = indepVarsList[["m3"]],
                                idVar = "mergeid",
                                randSlopeVars = c("waveCentW1")),
                m4 =
                    MakeFormula(depVar = dep,
                                indepVars = indepVarsList[["m4"]],
                                idVar = "mergeid",
                                randSlopeVars = c("waveCentW1")),
                m5 =
                    MakeFormula(depVar = "depression",
                                indepVars = indepVarsList[["m5"]],
                                idVar = "mergeid",
                                randSlopeVars = c("waveCentW1")),
                m6 =
                    MakeFormula(depVar = dep,
                                indepVars = indepVarsList[["m6"]],
                                idVar = "mergeid",
                                randSlopeVars = c("waveCentW1"))))
}

### Now we actually run the models
if (useFuture) {
    system.time(source(file = "runModelsFuture.R"))
} else {
    system.time(source(file = "runModels.R"))
}
