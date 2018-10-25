#### Generate variables related to activities of daily living

### Convert existing SHARE-created ADL variable to numeric
shareAllSubsetRecode$adl <- factorToNumeric(shareAllSubsetRecode$adl)
