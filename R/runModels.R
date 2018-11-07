#### This actually runs the models

### The only thing that changes is the formula, everything else remains the same
### between models

### Now we loop through the ADL models
modResListADL <- lapply(formulaListADL,
                        function(x) {
                            lmer(formula = x,
                                 data = finalSampleADL,
                                 REML = FALSE,
                                 na.action = na.omit)
                        })

### Loop through the models with low physical activity as reference (only
### applies to models 4 through 6)
modResListADLLowPA <- lapply(formulaListADL[paste0("m", c(4:6))],
                                            function(x) {
                                                lmer(formula = x,
                                                     data = finalSampleADLLowPA,
                                                     REML = FALSE,
                                                     na.action = na.omit)
                                            })

### And the same for IADL
modResListIADL <- lapply(formulaListIADL,
                         function(x) {
                             lmer(formula = x,
                                  data = finalSampleIADL,
                                  REML = FALSE,
                                  na.action = na.omit)
                         })

### Loop through the models with low physical activity as reference (only
### applies to models 4 through 6)
modResListIADLLowPA <- lapply(formulaListIADL[paste0("m", c(4:6))],
                              function(x) {
                                  lmer(formula = x,
                                       data = finalSampleIADLLowPA,
                                       REML = FALSE,
                                       na.action = na.omit)
                              })                                            
