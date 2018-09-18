#### This script first appends the normal survey waves and then merges the
#### retrospective SHARELife data with that data

toLoad <- list.files(path = "../data", full.names = TRUE,
                     pattern = "sharew[[:digit:]]\\.")

for (file in toLoad) {
    load(file)
}

### Add wave indicator to each data frame
AddWave <- function(df = NULL, wave = NULL) {
    df$wave <- wave
    df
}

for (i in c(1:6)) {
    assign(paste0("sharew", i),
           AddWave(df = get(paste0("sharew", i)),
                   wave = i))
}

shareAll <- sharew1

AppendDFs <- function(df1 = NULL, df2 = NULL) {

    df1 <- df1
    df2 <- df2

    namesDF1 <- names(df1)
    namesDF2 <- names(df2)

    ## Make sure we don't get NAs in factors by combining levels
    commonVars <- intersect(namesDF1, namesDF2)

    ## If either or both are factors and have the same name combine levels
    areFactorsDF1 <- unlist(lapply(df1[, commonVars], is.factor))
    areFactorsDF2 <- unlist(lapply(df2[, commonVars], is.factor))

    fixFactors <- names(which(areFactorsDF1 | areFactorsDF2))

    for (var in fixFactors) {
        df1[, var] <- factor(df1[, var])
        df2[, var] <- factor(df2[, var])

        levels(df1[, var]) <- c(levels(df1[, var]), levels(df2[, var]))
        levels(df2[, var]) <- c(levels(df1[, var]), levels(df2[, var]))
    }        

    ## Check which variables are not in the first data frame
    notInDF1 <- setdiff(namesDF2, namesDF1)

    ## Check which variables are not in the second data frame
    notInDF2 <- setdiff(namesDF1, namesDF2)

    df1[, notInDF1] <- NA
    df2[, notInDF2] <- NA

    if(ncol(df1) == ncol(df2)) {
        ret <- rbind(df1, df2)
    }

    ret
}

### Append all the regular waves together
for (i in c(2, 4:6)) {
    shareAll <- AppendDFs(df = shareAll, df2 = get(paste0("sharew", i)))
}

### Now merge in the time constant information from SHARELife
shareAll <- merge(shareAll, sharew3)

### And save the new data frame
save(shareAll, file = ("../data/shareAll.RData"))
