#### This actually runs the models

### This version uses future to run the models in parallel

### The only thing that changes is the formula, everything else remains the same
### between models

plan(multisession)

### Now we loop through the ADL models
modResListADL <- lapply(formulaListADL,
                        function(x) {
                            substitute(future(
                                lmer(formula = x,
                                     data = finalSampleADL,
                                     REML = FALSE,
                                     na.action = na.omit)), list(x = x))
                        })

### Actually evaluate the call
modResListADL <- lapply(modResListADL, eval)

### Get the values (asynchronously)
modResListADL <- resolve(values(modResListADL))

### Same for low physical activity (only applies to models 4 through 6)
modResListADLLowPA <- lapply(formulaListADL[paste0("m", c(4:6))],
                             function(x) {
                                 substitute(future(
                                     lmer(formula = x,
                                          data = finalSampleADLLowPA,
                                          REML = FALSE,
                                          na.action = na.omit)), list(x = x))
                             })

### Actually evaluate the call
modResListADLLowPA <- lapply(modResListADLLowPA, eval)

### Get the values (asynchronously)
modResListADLLowPA <- resolve(values(modResListADLLowPA))

### And the same for IADL
modResListIADL <- lapply(formulaListIADL,
                        function(x) {
                            substitute(future(
                                lmer(formula = x,
                                     data = finalSampleIADL,
                                     REML = FALSE,
                                     na.action = na.omit)), list(x = x))
                        })

### Actually evaluate the call
modResListIADL <- lapply(modResListIADL, eval)

### Get the values (asynchronously)
modResListIADL <- resolve(values(modResListIADL))

### Same for low physical activity (only models 4 through 6)
modResListIADLLowPA <- lapply(formulaListIADL[paste0("m", c(4:6))],
                             function(x) {
                                 substitute(future(
                                     lmer(formula = x,
                                          data = finalSampleIADLLowPA,
                                          REML = FALSE,
                                          na.action = na.omit)), list(x = x))
                             })

### Actually evaluate the call
modResListIADLLowPA <- lapply(modResListIADLLowPA, eval)

### Get the values (asynchronously)
modResListIADLLowPA <- resolve(values(modResListIADLLowPA))
