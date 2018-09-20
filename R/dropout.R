#### Take into account why respondents dropped out of study; this is a
#### time-constant variable

dropoutDF <- data.frame(mergeid = unique(shareAllSubsetRecode$mergeid))
dropoutDF$dropout <- "No dropout"

dropoutInterview <- function(df = NULL, wave = NULL) {
    df <- df
    wave <- wave

    ret <- df[which(df$wave == wave),
              c("interview", "mergeid")]
    ret <- ret[which(ret$interview == "No interview" |
                     is.na(ret$interview)),
               "mergeid"]

    ret
}

dropoutNA <- function(df = NULL, wave = NULL) {
    df <- df
    wave <- wave

    ret <- setdiff(df$mergeid,
                   df[df$wave == wave, "mergeid"])

    ret
}

### People who didn't respond to waves 5 & 6 are classified as drop-outs
noResponseW5 <- dropoutInterview(df = shareAllSubsetRecode, wave = 5)

noResponseW6 <- dropoutInterview(df = shareAllSubsetRecode, wave = 6)

### As are IDs that don't occur at waves 5 & 6
notInW5 <- dropoutNA(df = shareAllSubsetRecode, wave = 5)

notInW6 <- dropoutNA(df = shareAllSubsetRecode, wave = 6)

### Now we take all these
droppedW5W6 <- intersect(union(noResponseW5,
                               notInW5),
                         union(noResponseW6,
                               notInW6))

dropoutDF[which(dropoutDF$mergeid %in% droppedW5W6), "dropout"] <-
    "dropped"

### Find those that died during the study
deceasedW1To6 <-
    shareAllSubsetRecode[which(shareAllSubsetRecode$interview ==
                               "End-of-Life interview"), "mergeid"]

deceasedW3 <-
        shareAllSubsetRecode[which(shareAllSubsetRecode$interview.w3 ==
                                   "End-of-Life interview"), "mergeid"]

deceasedAnyWave <- intersect(dropoutDF$mergeid,
                             union(deceasedW1To6, deceasedW3))

dropoutDF[which(dropoutDF$mergeid %in% deceasedAnyWave), "dropout"] <-
    "deceased"

dropoutDF$dropout <- factor(dropoutDF$dropout,
                            levels = c("No dropout",
                                       "dropped",
                                       "deceased"))

### Now we merge back onto the main data frame
shareAllSubsetRecode <- merge(shareAllSubsetRecode,
                              dropoutDF,
                              by = "mergeid")
