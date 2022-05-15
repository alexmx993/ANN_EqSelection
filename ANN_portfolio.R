
parameters                      <- list()
parameters[["maindir"]]         <- "C:/Users/Alessio/Desktop/ANN_model"
parameters[["inputdir"]]        <- paste0(parameters$maindir, "/input")
parameters[["outputdir"]]       <- paste0(parameters$maindir, "/output")
parameters[["cache_folder"]]    <- paste0(parameters$maindir, "/cache_folder")
parameters[["estimation_date"]] <- Sys.Date()
parameters[["month"]]           <- 30
parameters[["pf_size"]]         <- 15
parameters[["investment"]]      <- 7000
parameters[["alpha"]]           <- 0.025
parameters[["pf_currency"]]     <- "EUR"
parameters[["benchmark"]]       <- "URTH"
parameters[["filename"]]        <- "ANN_portfolio.R"
parameters[["file_names"]]      <- c("URTH_holdings.xlsx",
                                     "ticker_extension.xlsx")

setwd(parameters$maindir)
options(max.print = 1e9)
source("functions.R")
libraries(c('quantmod', 'BatchGetSymbols', 'pracma', 'roll', 
            'paleotree', 'rlist', 'caret', 
            'leaps', 'qpcR', 'glmnet', 'openxlsx', 'R.utils', 
            'stringr', 'fAssets', 'quantmod', 'quadprog'))

neuralnet  <- read.ann(parameters)
tickers    <- tickers.sourcing(parameters)
benchmark  <- benchmark.download(ticker = parameters$benchmark, parameters, neuralnet = neuralnet)
data       <- data.download(tickers$tickers, parameters, benchmark, metadata = tickers)
indicators <- build.indicators(data, parameters, benchmark, tickers)
ann_input  <- ann.inputdef(indicators, parameters, tickers)
datasets   <- ann.asset.returns(ann_input, neuralnet, data, parameters)

ret                <- datasets$returns
ret                <- ret[, colMeans(ret) > mean(colMeans(ret))]
prices             <- datasets$prices[,  colMeans(ret) > mean(colMeans(ret))]
names              <- names(ret)
random_pfs         <- as.data.frame(matrix(nrow = 5000, ncol = ncol(ret) + 3, NA))
params             <- c("sd", "er", "sharpe")
names(random_pfs)  <- c(names, params)
cols               <- 1:ncol(ret)
size               <- parameters$pf_size
ew                 <- 1/size

for (r in 1:nrow(random_pfs)) {

  random_pfs[r, cols] <- names %in% sample(names, size = size, replace = FALSE)
  tmp                 <- ret[, which(random_pfs[r, cols] == 1)]
  frontier            <- setNames(rep(ew, size), colnames(tmp))
  
  countries <- agg.features(x = macrolocations(tickers, parameters)$macroloc$Region[macrolocations(tickers, parameters)$macroloc$Ticker %in% 
                                                                                      names(frontier)], eff_frontier = frontier, pf_size = size)
  caps      <- agg.features(x = tickers$equities$Size[tickers$equities$Ticker %in% names(frontier)], eff_frontier = frontier, pf_size = size)
  sectors   <- agg.features(x = tickers$equities$Sector[tickers$equities$Ticker %in% names(frontier)], eff_frontier = frontier, pf_size = size)
  
  if (!any(sectors > 0.25)) {

    random_pfs[r, -cols] <- c(sqrt(sum(as.numeric(frontier) * colSums((assetsMeanCov(tmp, method = "bagged")$cov * as.numeric(frontier))))), 
                              as.numeric(as.numeric(frontier) %*% colMeans(tmp)), 
                              (as.numeric(as.numeric(frontier) %*% colMeans(tmp))/
                               sqrt(sum(as.numeric(frontier) * colSums((assetsMeanCov(tmp, method = "bagged")$cov * as.numeric(frontier)))))))
    
  }
}

random_pfs   <- random_pfs[!is.na(random_pfs$er), ]
random_pfs   <- random_pfs[rev(order(random_pfs$er))[1:(parameters$pf_size * 3)], ]
toselect     <- random_pfs$sharpe == max(random_pfs$sharpe, na.rm = TRUE)
who          <- random_pfs[which(toselect), 1:ncol(ret)] == 1
eff_frontier <- setNames(rep(ew, size), names(ret[, who]))

