
parameters                      <- list()
parameters[["maindir"]]         <- "C:/Users/Alessio/Desktop/ANN_model"
parameters[["inputdir"]]        <- paste0(parameters$maindir, "/input")
parameters[["outputdir"]]       <- paste0(parameters$maindir, "/output")
parameters[["cache_folder"]]    <- paste0(parameters$maindir, "/cache_folder")
parameters[["month"]]           <- 30
parameters[["tickers_used"]]    <- 0.2
parameters[["estimation_date"]] <- Sys.Date()
parameters[["mom_percentile"]]  <- 50 + 1
parameters[["est_range"]]       <- 1   * parameters[["month"]]
parameters[["estimation_lb"]]   <- 1.2 * parameters[["month"]]
parameters[["training_size"]]   <- 0.75
parameters[["sigma"]]           <- 1
parameters[["test"]]            <- FALSE
parameters[["alpha"]]           <- 0.025
parameters[["layers"]]          <- 2
parameters[["pf_currency"]]     <- "EUR"
parameters[["benchmark"]]       <- "URTH"
parameters[["filename"]]        <- "ANN_simulation.R"
parameters[["file_names"]]      <- c("URTH_holdings.xlsx",
                                     "ticker_extension.xlsx")

setwd(parameters$maindir)
options(max.print = 1e9)
source("functions.R")
libraries(c('quantmod', 'BatchGetSymbols', 'pracma', 'roll', 
            'paleotree', 'rlist', 'neuralnet', 'caret', 
            'leaps', 'qpcR', 'glmnet', 'openxlsx', 
            'R.utils', 'devtools', 'quantmod', 'quadprog',
            'PerformanceAnalytics', 'stringr', 'fAssets'))

tickers    <- tickers.sourcing(parameters)
benchmark  <- benchmark.download(ticker = parameters$benchmark, parameters, metadata = tickers, neuralnet = NA)
data       <- data.download(symbols = tickers$tickers, parameters, benchmark, metadata = tickers)
indicators <- build.indicators(data, parameters, benchmark, tickers)
ann_input  <- ann.inputdef(indicators, parameters, tickers)
train      <- train.ann(parameters, ann_input)
ann        <- ann.simulation(parameters, train)
rel_imp    <- performance.measures(ann, parameters)

write.nn(parameters, ann, train, rel_imp)
