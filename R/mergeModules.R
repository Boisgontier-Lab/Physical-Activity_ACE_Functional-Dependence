#### This script merges the modules for each wave together.

### This a list containing the modules that will be used for each wave.

moduleList <- list("normal" =
                       c("cv_r", "ph", "gv_health", "gv_isced", "dn", "br",
                         "co", "cf"),
                   "SHARELife" = c("cv_r", "cs", "re", "wq", "gl", "hs"))

### Load the required modules that are saved as RData files for
for (i in c(1:6)) {
    if (i != 3) {
        dfFileNames <- paste0("sharew", i, "_", moduleList[["normal"]], ".RData")
    } else {
        ## The third wave is the retrospective SHARELife survey
        dfFileNames <- paste0("sharew", i, "_", moduleList[["SHARELife"]], ".RData")
    }

    for (dfFile in dfFileNames) {
        load(file = paste0("../data/", dfFile))
    }
}

### Load the required modules saves as RData files for retrospective survey wave


### Now for each wave merge the modules together
for (i in c(1:6)) {
    dfs <- ls(pattern = paste0("sharew", i))

    tmp <- get(dfs[1])

    for (df in dfs[c(2:length(dfs))]) {
        tmp <- merge(tmp, get(df), all = TRUE)
    }

    assign(paste0("sharew", i), tmp)
}

### Save the merged wave files
dfs <- ls(pattern = "sharew[[:digit:]]$")
print(dfs)
for (df in dfs) {
    save(list = df, file = paste0("../data/", df, ".RData"))
}

rm(moduleList, tmp, i, dfFile, dfFileNames, dfs, df)

