#### Generate variables related to health-related adverse childhood events (ACE)

### HACE score at age 15 = longHospit + multHospit + illnessChildScore +
### diseaseChildScore + physicalinjury15

### Variable indicating whether experienced
### long period (1 month+) of hospitalization
shareAllSubsetRecode$longHospit <- NA
shareAllSubsetRecode[which(shareAllSubsetRecode$sl_hs006_ == "No"),
                     "longHospit"] <- 0L
shareAllSubsetRecode[which(shareAllSubsetRecode$sl_hs006_ == "Yes"),
                     "longHospit"] <- 1L

### Variable whether experienced multiple stays in hospital
### (more than 3 times in 12 month period between ages 0-15)
shareAllSubsetRecode$multHospit <- NA
shareAllSubsetRecode[which(shareAllSubsetRecode$sl_hs007_ == "No"),
                     "multHospit"] <- 0L
shareAllSubsetRecode[which(shareAllSubsetRecode$sl_hs007_ == "Yes"),
                     "multHospit"] <- 1L

### Illnesses experienced in childhood (3 of interest: polio -- sl_hs008d2,
### asthma -- sl_hs008d3, meningitis/encephalitis -- sl_hs008d7)
illnessesChild <-
    shareAllSubsetRecode[, paste0("sl_hs008d", c("2", "3", "7"))] == "Selected"
mode(illnessesChild) <- "integer"
shareAllSubsetRecode$illnessChildScore <-
    apply(illnessesChild, 1, function(x) {
        if(all(is.na(x))) {
            NA
        } else {
            sum(x, na.rm = TRUE)
        }
    }
    )
rm(illnessesChild)

### Number of serious health conditions or diseases in childhood
diseasesChild <-
    shareAllSubsetRecode[, paste0("sl_hs009d", c(1:9))] == "Selected"
mode(diseasesChild) <- "integer"
shareAllSubsetRecode$diseaseChildScore <-
    apply(diseasesChild, 1, function(x) {
                if(all(is.na(x))) {
            NA
        } else {
            sum(x, na.rm = TRUE)
        }
    }
    )
rm(diseasesChild)

### Physical injury between 0 and 15 years old
shareAllSubsetRecode$yrInjury <-
    factorToNumeric(x = shareAllSubsetRecode$sl_hs053_)
shareAllSubsetRecode$physicalInjury15 <-
    IsEventAtAge(x = shareAllSubsetRecode$yrInjury,
                 birthVar = shareAllSubsetRecode$yrbirth,
                 ub = 15)

### The final score
shareAllSubsetRecode$HACEScore15 <-
    rowSums(shareAllSubsetRecode[, c("longHospit",
                                     "multHospit",
                                     "illnessChildScore",
                                     "diseaseChildScore",
                                     "physicalInjury15")],
            na.rm = TRUE)
