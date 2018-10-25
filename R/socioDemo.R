#### Recoding and correction of socio-demographic variables

### Make age a numeric variable
shareAllSubsetRecode$age_int <- factorToNumeric(shareAllSubsetRecode$age_int)

### Birth cohorts

## In case there are NAs, fill in with information from the 3rd wave
shareAllSubsetRecode[which(is.na(shareAllSubsetRecode$yrbirth)), "yrbirth"] <-
    shareAllSubsetRecode[which(is.na(shareAllSubsetRecode$yrbirth)), "yrbirth.w3"]

## Make it a numeric variable
shareAllSubsetRecode$yrbirth <- factorToNumeric(shareAllSubsetRecode$yrbirth)

## Now make the cohorts
shareAllSubsetRecode$birthCo <-
    cut(shareAllSubsetRecode$yrbirth,
        breaks = c((min(shareAllSubsetRecode$yrbirth, na.rm = TRUE) - 1),
                   1913, 1918, 1928, 1938, 1945, 1955,
                   (max(shareAllSubsetRecode$yrbirth, na.rm = TRUE) + 1)),
        right = TRUE,
        labels = c("before 1913", "between 1913 and 1918",
                   "between 1919 and 1928", "between 1929 and 1938",
                   "between 1939 and 1945", "between 1946 and 1955",
                   "after 1955"))

### Gender
## Fill in information from wave 3
shareAllSubsetRecode[which(is.na(shareAllSubsetRecode$dn042_)), "dn042_"] <-
    shareAllSubsetRecode[which(is.na(shareAllSubsetRecode$dn042_)), "gender.w3"]

## Get the mode over the entire survey for each individual
shareAllSubsetRecode$genderMode <-
    tapplySaferUnlist(tapply(X = as.character(shareAllSubsetRecode$gender),
                             INDEX = shareAllSubsetRecode$mergeid,
                             FUN = function(x) {
                                 GetMode(x)
                             },
                             simplify = FALSE),
                      shareAllSubsetRecode$mergeid)

shareAllSubsetRecode$genderMode <- factor(shareAllSubsetRecode$genderMode)

### Country welfare regime
shareAllSubsetRecode$welfreg <- shareAllSubsetRecode$country
levels(shareAllSubsetRecode$welfreg) <-
    list("Scandinavian" =
             c("Denmark", "Sweden"),
         "Bismarckian" =
             c("Austria", "Belgium", "France", "Germany", "Netherlands",
               "Switzerland", "Luxembourg"),
         "Southern" =
             c("Greece", "Italy", "Spain", "Portugal"),
         "Eastern European" =
             c("Croatia", "Czech Republic", "Estonia", "Hungary", "Poland",
               "Slovenia"),
         "Anglo-Saxon" = c("Ireland"),
         "Israel" = c("Israel"))

### Highest level of education achieved in 3 categories based on ISCED 1997
shareAllSubsetRecode$edu3CatMax <-
    SetMissing(shareAllSubsetRecode$isced1997_r,
               values = c("Other", "Still in school"))

levels(shareAllSubsetRecode$edu3CatMax) <-
    list(primary = c("None",
                     "ISCED-97 code 1"),
         secondary = c("ISCED-97 code 2",
                       "ISCED-97 code 3",
                       "ISCED-97 code 4"),
         tertiary = c("ISCED-97 code 5",
                      "ISCED-97 code 6"))

shareAllSubsetRecode$edu3CatMax <-
    tapplySaferUnlist(tapply(X = as.character(shareAllSubsetRecode$edu3CatMax),
           INDEX = shareAllSubsetRecode$mergeid,
           FUN = function(x) {
               x <- factor(x, levels = c("primary", "secondary", "tertiary"),
                           ordered = TRUE)
               if(all(is.na(x))) {
                   ret <- NA
               } else {
                   ret <- max(x, na.rm = TRUE)
               }

               as.character(ret)
           },
           simplify = FALSE),
           shareAllSubsetRecode$mergeid)

shareAllSubsetRecode$edu3CatMax <-
    factor(shareAllSubsetRecode$edu3CatMax,
           levels = c("primary", "secondary", "tertiary"))

### Mode of marital status
shareAllSubsetRecode$MaritalStatMode <-
    tapplySaferUnlist(tapply(X = as.character(shareAllSubsetRecode$dn014_),
           INDEX = shareAllSubsetRecode$mergeid,
           FUN = function(x) {
               GetMode(x)
           },
           simplify = FALSE),
           shareAllSubsetRecode$mergeid)
