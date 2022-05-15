
parameters                <- list()
parameters[["maindir"]]   <- "C:/Users/Alessio/Desktop/ANN_model"
parameters[["inputdir"]]  <- paste0(parameters$maindir, "/input")
parameters[["outputdir"]] <- paste0(parameters$maindir, "/output")

setwd(parameters$maindir)
source("functions.R")
source2("ANN_simulation.R", 1, 32)

nsim       <- 10
out        <- as.data.frame(matrix(NA, nrow = nsim, ncol = 13))
names(out) <- c("estimation_date", "mean", "sd", "var", "total_return", 
                "relative_return", "b_mean", 
                "b_sd", "b_var", "b_tot_ret", "beta", "alpha", "alpha_p")

dates <- as.character(seq(as.Date("2022-01-01"), Sys.Date() - parameters$est_range, by = "days"))

parameters[["estimation_date"]] <- max(dates)

full_tickers    <- tickers.sourcing(parameters)
full_benchmark  <- benchmark.download(ticker = parameters$benchmark, parameters, metadata = full_tickers, neuralnet = NA)
full_data       <- data.download(symbols = full_tickers$tickers, parameters, benchmark = full_benchmark, metadata = full_tickers)
full_indicators <- build.indicators(data = full_data, parameters = parameters, benchmark = full_benchmark, tickers = full_tickers)

save.image("C:/Users/Alessio/Desktop/ANN_model/wkspace.RData")

for (i in 1:nsim) {
  
  gc()

  est_date                        <- as.Date(sample(dates, size = 1))
  parameters[["estimation_date"]] <- NULL
  parameters[["estimation_date"]] <- est_date
  
  if (exists("tickers")) {rm(tickers, data, benchmark)}
  
  tickers    <- full_tickers
  data       <- full_data
  benchmark  <- full_benchmark
  indicators <- list()
  
  sample <- sort(sample(1:ncol(data$price.adjusted), size = round(ncol(data$price.adjusted)/2), replace = FALSE))
  
  for (j in names(data)[!names(data) %in% c("index", "fx_index", "tickers", "fundamentals")]) {
    data[[j]] <- as.data.frame(data[[j]][data$index <= est_date, sample])
  }
  
  data$index      <- data$index[data$index <= est_date]
  data$fx_index   <- data$fx_index[data$fx_index <= est_date]
  data$tickers    <- names(data$price.adjusted)
  tickers$tickers <- data$tickers
  
  for (j in names(benchmark)[!names(benchmark) %in% c("index", "fx_index", "tickers", "fundamentals")]) {
    benchmark[[j]] <- as.data.frame(benchmark[[j]][benchmark$index <= est_date, 1])
  }
  
  benchmark$index    <- benchmark$index[benchmark$index <= est_date]
  benchmark$fx_index <- benchmark$fx_index[benchmark$fx_index <= est_date]
  benchmark$tickers  <- names(benchmark$price.adjusted)
  
  for (j in names(full_indicators)) {
    indicators[[j]] <- as.data.frame(full_indicators[[j]][data$index <= est_date, sample])
  }
  
  if (exists("ann_input")) {rm(ann_input, train)}
  
  parameters[["pf_ret_FLAG"]] <- FALSE
  
  ann_input <- ann.inputdef(indicators, parameters, tickers)
  train     <- train.ann(parameters, ann_input)
  
  ann <- withTimeout({
    suppressWarnings(suppressMessages(ann.simulation(parameters, train)))
  }, timeout = 2 * 3600, onTimeout = "silent")
  
  if (exists("ann") && !is.null(ann)) {

    rel_imp <- performance.measures(ann, parameters)
    
    write.nn(parameters, ann, train, rel_imp)
    
    parameters[["pf_ret_FLAG"]] <- TRUE
    
    source2("ANN_portfolio.R", 8, 11)
    source2("ANN_portfolio.R", 26, 26)
    source2("ANN_portfolio.R", 31, 150)
    
    frontier <- eff_frontier
    stocks   <- names(eff_frontier)
    
    parameters[["pf_ret_FLAG"]]     <- NULL
    parameters[["estimation_date"]] <- NULL
    parameters[["estimation_date"]] <- as.Date(est_date) + parameters$est_range
    
    pf_benchmark <- benchmark.download(ticker = parameters$benchmark, parameters, neuralnet = neuralnet)
    pf_data      <- data.download(symbols = stocks, parameters, benchmark = pf_benchmark, metadata = tickers)
    prices       <- pf_data$price.adjusted[pf_data$index >= est_date, ]
    b_prices     <- pf_benchmark$price.adjusted[pf_benchmark$index >= est_date, ]
    b_returns    <- pf_benchmark$ret.adjusted.prices[pf_benchmark$index >= est_date, ][-1]
    
    all.equal(names(frontier), names(prices))
    
    value <- t(apply(prices, 1, FUN = function(x, y, z) {(y / z) * as.numeric(x)}, y = as.numeric(frontier), z = as.numeric(prices[1, ])))
    value <- as.numeric(rowSums(value, na.rm = T))
    ret   <- as.numeric(Delt(value)[-1])
    
    mean    <- mean(ret)
    sd      <- sd(ret)
    var     <- as.numeric(quantile(ret, probs = 0.05))
    tot_ret <- (value[length(value)] / value[1]) - 1
    
    b_mean    <- mean(b_returns)
    b_sd      <- sd(b_returns)
    b_var     <- as.numeric(quantile(b_returns, probs = 0.05))
    b_tot_ret <- (b_prices[length(b_prices)] / b_prices[1]) - 1
    
    beta    <- summary(lm(ret ~ b_returns))$coefficients[2, 1]
    alpha   <- summary(lm(ret ~ b_returns))$coefficients[1, 1]
    alpha_p <- summary(lm(ret ~ b_returns))$coefficients[1, 4]
    rel_ret <- tot_ret - b_tot_ret
    
    out[i, -1]              <- c(mean, sd, var, tot_ret, rel_ret, b_mean, b_sd, b_var, b_tot_ret, beta, alpha, alpha_p)
    storage.mode(out[i, 1]) <- "character"
    out[i, 1]               <- as.character(as.Date(est_date))
    
    View(out)
    
    mean(out$relative_return, na.rm = T)
    
    saveRDS(list(table = out, parameters = parameters, comments = "third ew"), 
            file = paste0(parameters$outputdir, "/portfolio_returns_onemonth_2ew", Sys.Date(), ".RDS"))
    
    rm(ann, tickers, data, benchmark, indicators, ann_input, train, rel_imp)
    
    gc()
      
  } else {
    
    message("Simulation jumped due to time failure. Next.")
    
  }
  
}

