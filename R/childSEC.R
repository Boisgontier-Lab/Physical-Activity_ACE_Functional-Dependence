#### Script generating variables for the composite childhood socioeconomic
#### conditions indicator as defined by Wharendrof and Blane (2014);
#### https://doi.org/10.1080/13607863.2014.938604

### All of these retrospective questions on childhood socioeconomic conditions
### asked in SHARELife reflect conditions at age 10

### Breadwinner's main occupational skill level; binary/logical variable with
### TRUE being a low skill profession and FALSE being a high skill profession
lowSkillProfessions <- c("Clerk",
                         "Service, shop or market sales worker",
                         "Skilled agricultural or fishery worker",
                         "Craft or related trades worker",
                         "Plant/machine operator or assembler",
                         "Elementary occupation",
                         "Armed forces")
shareAllSubsetRecode$occBread <-
    shareAllSubsetRecode$sl_cs009_ %in% lowSkillProfessions
shareAllSubsetRecode[is.na(shareAllSubsetRecode$sl_cs009_), "occBread"] <- NA
shareAllSubsetRecode[which(shareAllSubsetRecode$sl_cs009_ ==
                     "Spontaneous only: there was no main breadwinner"),
                     "occBread"] <- NA
shareAllSubsetRecode$occBread <- as.numeric(shareAllSubsetRecode$occBread)

### Number of books in household
shareAllSubsetRecode$books <-
    shareAllSubsetRecode$sl_cs008_ %in% "None or very few (0-10 books)"
shareAllSubsetRecode[is.na(shareAllSubsetRecode$sl_cs008_), "books"] <- NA
shareAllSubsetRecode$books <- as.numeric(shareAllSubsetRecode$books)

### Measure of overcrowding

## Number of rooms in household
shareAllSubsetRecode$rooms <- factorToNumeric(shareAllSubsetRecode$sl_cs002_)
shareAllSubsetRecode[which(shareAllSubsetRecode$rooms == 0), "rooms"] <- NA

## Number of people in household
shareAllSubsetRecode$hhpeople <- factorToNumeric(shareAllSubsetRecode$sl_cs003_)
shareAllSubsetRecode[which(shareAllSubsetRecode$hhpeople == 0), "hhpeople"] <-
    NA

## Now the actual measure of overcrowding; if there are 1 or fewer people per
## room in the househod there is no overcrowding
shareAllSubsetRecode$overcrowd <-
    ifelse(shareAllSubsetRecode$hhpeople/
           shareAllSubsetRecode$rooms <= 1,
           0, 1)

### Housing quality; low housing quality if household didn't have any of the
### following: fixed bath, cold running water, hot running water, inside toilet,
### central heating
shareAllSubsetRecode$lowHousingQuality <-
    shareAllSubsetRecode$sl_cs007dno %in% "Selected"
shareAllSubsetRecode[is.na(shareAllSubsetRecode$sl_cs007dno),
                     "lowHousingQuality"] <- NA
shareAllSubsetRecode$lowHousingQuality <-
    as.numeric(shareAllSubsetRecode$lowHousingQuality)

### The score is simply the addition of the four sub-components: 1) occupational
### skill level of main breadwinner; 2) number of books in household; 3)
### overcrowding; 4) housing quality. A higher score means a more disadvantaged
### childhood socioeconomic situation
shareAllSubsetRecode$childSEC <-
    shareAllSubsetRecode$occBread +
    shareAllSubsetRecode$books +
    shareAllSubsetRecode$overcrowd +
    shareAllSubsetRecode$lowHousingQuality

shareAllSubsetRecode$childSECFact <-
    factor(shareAllSubsetRecode$childSE,
           levels = c(4:0),
           labels = c("Most disadvantaged",
                      "Disadvantaged",
                      "Middle",
                      "Advantaged",
                      "Most advantaged"))
