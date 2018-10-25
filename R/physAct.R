#### Generate variables for physical activity

### We use two variables to measure physical activity
shareAllSubsetRecode$physAct1 <- as.numeric(
    factor(shareAllSubsetRecode$br015_,
           exclude = NULL,
           levels = c("Hardly ever, or never",
                      "One to three times a month",
                      "Once a week",
                      "More than once a week"),
           ordered = TRUE))

shareAllSubsetRecode$physAct2 <- as.numeric(
    factor(shareAllSubsetRecode$br016_,
           exclude = NULL,
           levels = c("Hardly ever, or never",
                      "One to three times a month",
                      "Once a week",
                      "More than once a week"),
           ordered = TRUE))

### We take the maximum of the two
shareAllSubsetRecode$physActBin <- pmax(shareAllSubsetRecode$physAct1,
                                        shareAllSubsetRecode$physAct2,
                                        na.rm = TRUE)

### We make it binary — individuals are physically active more than once a week
### are considered to be physically active
shareAllSubsetRecode$physActBin[which(shareAllSubsetRecode$physActBin <
                                      max(shareAllSubsetRecode$physActBin, na.rm
                                          = TRUE))] <- 0
shareAllSubsetRecode$physActBin[which(shareAllSubsetRecode$physActBin ==
                                      max(shareAllSubsetRecode$physActBin, na.rm
                                          = TRUE))] <- 1

### Then take the average across all measurements for each participant
shareAllSubsetRecode$physActBinMean <- tapplySaferUnlist(
    tapply(shareAllSubsetRecode$physActBin,
           shareAllSubsetRecode$mergeid,
           function(x) {
               if (all(is.na(x))) {
                   return(NA)
               } else {
                   return(mean(x,
                               na.rm = TRUE))
               }
           },
           simplify = FALSE),
    shareAllSubsetRecode$mergeid)

### Participants are classified as physically active if mean above 0.5
shareAllSubsetRecode[which(shareAllSubsetRecode$physActBinMean <= 0.5),
                     "physActBinMean"] <- 0
shareAllSubsetRecode[which(shareAllSubsetRecode$physActBinMean > 0.5),
                     "physActBinMean"] <- 1

### Make it a factor with high physical activity as reference
shareAllSubsetRecode$physActBinMean <-
    factor(shareAllSubsetRecode$physActBinMean,
           levels = c(1, 0),
           labels = c("high physical activity",
                      "low physical activity"))

                                                      
                                                      
