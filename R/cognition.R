#### This script generates congnition related variables

### Delayed recall of a list of 10 words
shareAllSubsetRecode$delayedRecall <-
    factorToNumeric(shareAllSubsetRecode$cf016tot)

shareAllSubsetRecode[which(shareAllSubsetRecode$delayedRecall > 4),
                     "delayedRecall"] <- 4

## Compute the mean for each individual
shareAllSubsetRecode$delayedRecallAverage <-
    unlist(tapply(X = shareAllSubsetRecode$delayedRecall,
           INDEX = shareAllSubsetRecode$mergeid,
           FUN = RepMean))

### Verbal fluency (number of animals named)
shareAllSubsetRecode$verbalFluency <-
    factorToNumeric(shareAllSubsetRecode$cf010_)

## Returns a vector with values ranging from 1 to 5 with 1 being [0; 12[
## animals named, 2 being [12; 16[, 3 [16; 19[, 4 [19; 24[, 5 [24; 101[
shareAllSubsetRecode$verbalFluency <-
    cut(shareAllSubsetRecode$verbalFluency,
        breaks = c(min(shareAllSubsetRecode$verbalFluency, na.rm = TRUE),
                   12, 16, 19, 24,
                   max(shareAllSubsetRecode$verbalFluency, na.rm = TRUE) + 1),
        right = FALSE,
        labels = FALSE)

## We actually want the values to range from 0 to 4
shareAllSubsetRecode$verbalFluency <-
    shareAllSubsetRecode$verbalFluency - 1

## Compute the mean for each individual
shareAllSubsetRecode$verbalFluencyAverage <-
    unlist(tapply(X = shareAllSubsetRecode$verbalFluency,
           INDEX = shareAllSubsetRecode$mergeid,
           FUN = RepMean))

### Short-term memory recall
shareAllSubsetRecode$shortRecall <-
    factorToNumeric(shareAllSubsetRecode$cf008tot)

shareAllSubsetRecode[which(shareAllSubsetRecode$shortRecall < 2),
                     "shortRecall"] <- 1

shareAllSubsetRecode[which(shareAllSubsetRecode$shortRecall > 5),
                     "shortRecall"] <- 5

## Again we want the values to range from 0 to 4
shareAllSubsetRecode$shortRecall <- shareAllSubsetRecode$shortRecall - 1

## Compute the mean for each individual
shareAllSubsetRecode$shortRecallAverage <-
    unlist(tapply(X = shareAllSubsetRecode$shortRecall,
                  INDEX = shareAllSubsetRecode$mergeid,
                  FUN = RepMean))

### Time "awareness"/"orientation"
shareAllSubsetRecode[, c(paste0("cf00", c(3:6), "_"))] <-
    as.data.frame(lapply(shareAllSubsetRecode[, c(paste0("cf00", c(3:6), "_"))],
                         FUN = function(x) {
                             levels(x) <- gsub("Given correctly",
                                               "1",
                                               levels(x))

                             levels(x) <- gsub("Given incorrectly/doesn't know",
                                               "0",
                                               levels(x))
                             x
                         }))

shareAllSubsetRecode[, c(paste0("cf00", c(3:6), "_"))] <-
    as.data.frame(lapply(shareAllSubsetRecode[, c(paste0("cf00", c(3:6), "_"))],
                         FUN = function(x) {
                             factorToNumeric(x = x)
                         }))

## Combined index
shareAllSubsetRecode$timeOrient <- shareAllSubsetRecode$cf003_ +
    shareAllSubsetRecode$cf004_ + shareAllSubsetRecode$cf005_ +
    shareAllSubsetRecode$cf006_

## Average of combined index for each subject
shareAllSubsetRecode$timeOrientAverage <-
    unlist(tapply(X = shareAllSubsetRecode$timeOrient,
                  INDEX = shareAllSubsetRecode$mergeid,
                  FUN = RepMean))

