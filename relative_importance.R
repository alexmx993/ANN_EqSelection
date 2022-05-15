
parameters                <- list()
parameters[["maindir"]]   <- "C:/Users/Alessio/Desktop/ANN_model"
parameters[["inputdir"]]  <- paste0(parameters$maindir, "/input")
parameters[["outputdir"]] <- paste0(parameters$maindir, "/output")

setwd(parameters$maindir)
options(max.print = 1e9)
source("functions.R")
libraries(c('quantmod', 'BatchGetSymbols', 'pracma', 'roll', 
            'paleotree', 'rlist', 'neuralnet', 'caret', 
            'leaps', 'qpcR', 'glmnet', 'openxlsx', 
            'R.utils', 'devtools', 'quantmod'))

indicators <- importance.assessment(parameters, "partial_indicator")
lookback   <- importance.assessment(parameters, "partial_lookback")

saveRDS(list(indicators = indicators, lookback = lookback), 
        file = paste0(parameters$outputdir, "/relative_imp_full_", Sys.Date(), ".RDS"))
