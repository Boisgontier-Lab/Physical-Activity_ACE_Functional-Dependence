#### Generate variables related to adverse childhood events (ACE)

### Make sure that the variables containing year an event took place are numeric
### dn127_1 and dn127_2 are the year the mother and the father died respectively
### sl_gl033_1 is the year household had property taken away
### sl_gl015_ is the year the period of hunger began
shareAllSubsetRecode[, c(paste0("dn127_", c(1:2)),
                         "sl_gl033_1",
                         "sl_gl015_")] <-
    as.data.frame(lapply(
        shareAllSubsetRecode[, c(paste0("dn127_", c(1:2)),
                                 "sl_gl033_1",
                                 "sl_gl015_")],
        FUN = factorToNumeric))

### Death of parents at age 15 or younger
shareAllSubsetRecode$deathFather15 <-
    IsEventAtAge(x = shareAllSubsetRecode$dn127_2,
                 birthVar = shareAllSubsetRecode$yrbirth,
                 ub = 15)

shareAllSubsetRecode$deathMother15 <-
    IsEventAtAge(x = shareAllSubsetRecode$dn127_1,
                 birthVar = shareAllSubsetRecode$yrbirth,
                 ub = 15)
### Assign a score making construction of ACE score easier; if neither parents
### perished before or at age 15 score of 0, one parent score of 1, both parents
### score of 2
shareAllSubsetRecode$deathParents15 <- 0L
shareAllSubsetRecode[which(shareAllSubsetRecode$deathMother15 |
                           shareAllSubsetRecode$deathFather15),
                     "deathParents15"] <- 1L
shareAllSubsetRecode[which(shareAllSubsetRecode$deathMother15 &
                           shareAllSubsetRecode$deathFather15),
                     "deathParents15"] <- 2L
shareAllSubsetRecode[which(is.na(shareAllSubsetRecode$deathMother15) &
                           is.na(shareAllSubsetRecode$deathFather15)),
                     "deathParents15"] <- NA

### This is a time-constant variable so duplicate info
shareAllSubsetRecode$deathParents15 <-
    tapplySaferUnlist(tapply(shareAllSubsetRecode$deathParents15,
                             as.character(shareAllSubsetRecode$mergeid),
                             function(x) {
                                 if (all(is.na(x))) {
                                     NA
                                 } else {
                                     max(x, na.rm = TRUE)
                                 }
                             }, simplify = FALSE),
                      shareAllSubsetRecode$mergeid
                      )

### We don't need to duplicate info for the SHARELife modules as this was done
### when we merged those variables in

### Experienced period of hunger at age 15 or younger
shareAllSubsetRecode$hunger15 <-
    IsEventAtAge(x = shareAllSubsetRecode$sl_gl015_,
                 birthVar = shareAllSubsetRecode$yrbirth,
                 ub = 15)

### Had property taken away at age 15 or younger
shareAllSubsetRecode$propertyAway15  <-
    IsEventAtAge(x = shareAllSubsetRecode$sl_gl033_1,
                 birthVar = shareAllSubsetRecode$yrbirth,
                 ub = 15)

### Respondent lived outside of parent's care
shareAllSubsetRecode$livedInCare <- 0L
shareAllSubsetRecode[which(shareAllSubsetRecode$sl_ac002d1 == "Selected" |
                          shareAllSubsetRecode$sl_ac002d2 == "Selected"),
                     "livedInCare"] <- 1L
shareAllSubsetRecode[which(is.na(shareAllSubsetRecode$sl_ac002d1) &
                           is.na(shareAllSubsetRecode$sl_ac002d2)),
                     "livedInCare"] <- NA

### Parents had drinking problems
shareAllSubsetRecode$parentsDrinkingProb <- NA
shareAllSubsetRecode[which(shareAllSubsetRecode$sl_hs045d2 == "Not selected"),
                     "parentsDrinkingProb"] <- 0L
shareAllSubsetRecode[which(shareAllSubsetRecode$sl_hs045d2 == "Selected"),
                     "parentsDrinkingProb"] <- 1L

### Parents had mental problems
shareAllSubsetRecode$parentsMentalProb <- NA
shareAllSubsetRecode[which(shareAllSubsetRecode$sl_hs045d3 == "Not selected"),
                     "parentsMentalProb"] <- 0L
shareAllSubsetRecode[which(shareAllSubsetRecode$sl_hs045d3 == "Selected"),
                     "parentsMentalProb"] <- 1L

### The composite adverse childhood events indicator
shareAllSubsetRecode$ACEScore15 <-
    rowSums(shareAllSubsetRecode[,
                                 c("deathParents15",
                                   "hunger15",
                                   "propertyAway15",
                                   "livedInCare",
                                   "parentsDrinkingProb",
                                   "parentsMentalProb")],
            na.rm = TRUE)
