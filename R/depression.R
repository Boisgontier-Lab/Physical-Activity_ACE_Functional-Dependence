#### Fix the depression variable
shareAllSubsetRecode$depression <- revalue(shareAllSubsetRecode$eurod,
                                           c("Not depressed" = "0",
                                             "Very depressed" = "12"))

shareAllSubsetRecode$depression <- factorToNumeric(shareAllSubsetRecode$depression)
