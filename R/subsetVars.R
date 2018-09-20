#### Subset the combined data frame

### Load the combined data frame
load(file = "../data/shareAll.RData")

### These are the variables we will need for the analysis
varsToKeep <- c("mergeid", "country", "gender", "wave", "age_int", "sl_cs009_",
"sl_cs008_", "sl_cs002_", "sl_cs003_", "sl_cs007dno", "yrbirth", "interview",
"isced1997_r", "dn014_", "eurod", "cf016tot", "cf010_", "br015_", "br016_",
"br002_", "sl_re047_", "sl_re013", "sl_re040_", "sl_re005_", "sl_cs004d1",
"sl_cs004d2", "co007_", "br029_", "br019_", "cf008tot", "cf003_", "cf004_",
"cf005_", "cf006_", "chronic2w", "sl_wq002_", "sl_wq016_", "sl_ac002d1",
"sl_ac002d2", "dn127_2", "dn127_1", "sl_gl015_", "sl_gl033_1", "sl_hs045d2",
"sl_hs045d3", "adl", "iadl", "ph049d7", "ph049d8", "ph049d9", "ph049d10",
"ph049d11", "ph049d12", "ph049d13", "bmi", "sl_hs006_", "sl_hs007_",
"sl_hs008d2", "sl_hs008d3", "sl_hs008d7", "sl_hs009d1", "sl_hs009d2",
"sl_hs009d3", "sl_hs009d4", "sl_hs009d5", "sl_hs009d6", "sl_hs009d7",
"sl_hs009d8", "sl_hs009d9", "sl_hs053_")

### Make it into a grep-compatible list
varsToKeepGrep <- paste0("^", varsToKeep, collapse = "|")
varsToKeepGrep <- grep(varsToKeepGrep, names(shareAll), value = TRUE)

### The actual subsetting
shareAllSubset <- shareAll[, varsToKeepGrep]

### Fix the chronic conditions variable which has a different naming scheme (it
### actaully includes wave suffices for some reason)
shareAllSubset$chronic2 <- NA

for (i in c(1:2, 4:6)) {
    shareAllSubset[which(shareAllSubset$wave == i), "chronic2"] <-
        shareAllSubset[which(shareAllSubset$wave == i), paste0("chronic2w", i)]
}

### Save it
save(shareAllSubset, file = "../data/shareAllSubset.RData")
