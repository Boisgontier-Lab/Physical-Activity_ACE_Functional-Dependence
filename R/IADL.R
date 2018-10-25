#### Generate variables related to instrumental activities of daily living
#### (IADL)

### Make the existing IADL variables numeric
shareAllSubsetRecode$iadl <- factorToNumeric(shareAllSubsetRecode$iadl)

### SHARE changed the way the IADL variable was constructed in wave 6 by adding
### to new items; revert back to previous construction for wave 6 observations

### First convert the dummy indicators to numeric
shareAllSubsetRecode[, c(paste0("ph049d", c(7:13)))] <-
    as.data.frame(
        lapply(shareAllSubsetRecode[, c(paste0("ph049d", c(7:13)))],
               function(x) {
                   factorToNumeric(revalue(x,
                                           c("Not selected" = 0,
                                             "Selected" = 1)))
                   }))

### Then add them all together to make the corrected IADL score for wave 6
shareAllSubsetRecode[which(shareAllSubsetRecode$wave == 6), "iadl"] <-
    rowSums(shareAllSubsetRecode[which(shareAllSubsetRecode$wave == 6),
                                 c(paste0("ph049d", c(7:13)))])
