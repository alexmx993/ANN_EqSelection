
# ----------------------------------------------------------------------------------------------------------- #

macrolocations <- function(tickers, parameters) {
  
  regions <- readxl::read_xlsx(path = paste0(parameters$inputdir, "/macrolocations.xlsx"))
  out     <- data.frame(Ticker   = as.character(tickers$equities$Ticker),
                        Location = as.character(tickers$equities$Location),
                        Region   = rep(NA, length(tickers$equities$Location)))
  out     <- out[!is.na(out$Location), ]
  
  for (i in unique(regions$Region)) {
    out$Region[out$Location %in% regions$Country[regions$Region %in% i]] <- i
  }
  
  return(list(macroloc = out, macrolocs = unique(out$Location)))
}

# ----------------------------------------------------------------------------------------------------------- #

libraries <- function
(package)
{
  for (i in 1:length(package)) {
    library(package[i], character.only = TRUE)
  }
}

# ----------------------------------------------------------------------------------------------------------- #

tickers.sourcing <- function(
  parameters
) 
{
  ticker_ext <- readxl::read_xlsx(paste0(parameters$inputdir, "/", parameters$file_names[2]))
  holdings   <- readxl::read_xlsx(paste0(parameters$inputdir, "/", parameters$file_names[1]))
  holdings   <- holdings[order(holdings$Ticker), ]
  
  for (q in 1:nrow(holdings)) {
    ext <- ticker_ext$ticker_extension[ticker_ext$exchange %in% holdings$Exchange[q]]
    if (length(ext) == 0) {next} else {
      holdings[q, 1] <- paste0(as.character(holdings[q, 1]), ".", ext)
    }
  }
  
  if (parameters$filename != "ANN_portfolio.R") {
    sample  <- sample(seq_len(length(holdings$Ticker)), size = floor(parameters$tickers_used * length(holdings$Ticker)))
    tickers <- holdings$Ticker[sample]
  } else {
    tickers <- holdings$Ticker
  }
  
  holdings$Size <- ifelse(holdings$`Market Value` > quantile(holdings$`Market Value`, probs = 0.5), "Large", "Mid")
  holdings      <- as.data.frame(holdings)
  
  return(list(tickers = tickers, equities = holdings))
}

# ----------------------------------------------------------------------------------------------------------- #

benchmark.download <- function(
  ticker,
  parameters,
  metadata
)
{
  
  month      <- parameters$month
  first_date <- as.Date(parameters$estimation_date) - (month * 12 * 2) 

  data_out <- yfR::yf_get(
    thresh_bad_data = 0.01,
    tickers         = ticker,
    first_date      = first_date,
    last_date       = as.Date(parameters$estimation_date),
    do_cache        = FALSE
  )
  
  data_wide <- yf_convert_to_wide(data_out)
  index     <- as.Date(as.matrix(data_wide$price_adjusted[, 1]))
  
  for (i in names(data_wide)) {
    data_wide[[i]]        <- as.data.frame(na.approx(data_wide[[i]][, -1], na.rm = FALSE, maxgap = 4))
    names(data_wide[[i]]) <- ticker
  }
  
  data_wide[["ret_closing_prices"]]  <- NULL
  data_wide[["ret_adjusted_prices"]] <- NULL
  
  symbols <- ticker
  
  fx <- if (symbols == "URTH") {"USD"} else {stop("Please update benchmark's currency.")}
  fx <- paste0(paste0(fx, parameters$pf_currency), "=X")
  
  fx_out <- yfR::yf_get(
    thresh_bad_data = 0.01,
    tickers         = fx,
    first_date      = first_date,
    last_date       = as.Date(parameters$estimation_date),
    do_cache        = FALSE
  )
  
  fx_wide        <- yf_convert_to_wide(fx_out)
  fx_index       <- as.Date(as.matrix(fx_wide$price_adjusted[, 1]))
  fx_final_index <- fx_index %in% index
  
  for (i in names(fx_wide)) {
    fx_wide[[i]]        <- fx_wide[[i]][fx_final_index, ]
    fx_wide[[i]]        <- as.data.frame(na.approx(fx_wide[[i]][, -1], na.rm = FALSE, maxgap = 4))
    names(fx_wide[[i]]) <- fx
  }
  
  fx_rates <- as.numeric(as.matrix(fx_wide$price_adjusted))

  for (i in names(data_wide)[!names(data_wide) %in% "volume"]) {
    data_wide[[i]][, 1] <- as.numeric(data_wide[[i]][, 1]) / fx_rates
  }
  
  data_wide$ret_adjusted_prices <- apply(data_wide$price_adjusted, 2, Delt)
  
  for (i in names(data_wide)) {data_wide[[i]] <- as.data.frame(data_wide[[i]])}
  
  data_wide$fx_index <- fx_index[fx_final_index]
  data_wide$index    <- index
  data_wide$tickers  <- names(data_wide$price_adjusted)
  
  return(data_wide)
}

# ----------------------------------------------------------------------------------------------------------- #

data.download <- function(
  symbols,
  parameters,
  benchmark,
  metadata
)
{
  
  unlink(paste0(parameters$cache_folder, "/",
         list.files(parameters$cache_folder)))
  
  month      <- parameters$month
  first_date <- as.Date(parameters$estimation_date) - (month * 12 * 2) 
  
  data_out <- yfR::yf_get(
    thresh_bad_data = 0.01,
    tickers         = symbols,
    first_date      = first_date,
    last_date       = as.Date(parameters$estimation_date),
    do_cache        = FALSE
  )
  
  data_wide   <- yf_convert_to_wide(data_out)
  or_index    <- as.Date(as.matrix(data_wide$price_adjusted[, 1]))
  final_index <- or_index %in% as.Date(benchmark$index)
  
  for (i in names(data_wide)) {
    data_wide[[i]] <- data_wide[[i]][final_index, ]
    data_wide[[i]] <- as.data.frame(na.approx(data_wide[[i]][, -1], na.rm = FALSE, maxgap = 4))
  }
  
  data_wide[["ret_closing_prices"]]  <- NULL
  data_wide[["ret_adjusted_prices"]] <- NULL
  
  symbols <- names(data_wide$price_adjusted)
  
  fx <- unique(metadata$equities$`Market Currency`[metadata$equities$Ticker %in% symbols])
  fx <- paste0(paste0(fx, parameters$pf_currency), "=X")

  fx_out <- yfR::yf_get(
    thresh_bad_data = 0.01,
    tickers         = fx,
    first_date      = first_date,
    last_date       = as.Date(parameters$estimation_date),
    do_cache        = FALSE
  )
  
  fx_wide        <- yf_convert_to_wide(fx_out)
  fx_index       <- as.Date(as.matrix(fx_wide$price_adjusted[, 1]))
  fx_final_index <- fx_index %in% as.Date(benchmark$index)
  
  for (i in names(fx_wide)) {
    fx_wide[[i]] <- fx_wide[[i]][fx_final_index, ]
    fx_wide[[i]] <- as.data.frame(na.approx(fx_wide[[i]][, -1], na.rm = FALSE, maxgap = 4))
  }
  
  fx_rates        <- as.data.frame(fx_wide$price_adjusted)
  names           <- names(fx_rates)
  fx_rates        <- cbind(fx_rates, 1)
  names(fx_rates) <- c(names, paste0(parameters$pf_currency, parameters$pf_currency, "=X"))
  
  fxs       <- metadata$equities$`Market Currency`[metadata$equities$Ticker %in% symbols]
  fxs       <- paste0(paste0(fxs, parameters$pf_currency), "=X")
  to_remove <- symbols[fxs %in% names(fx_rates)]  
  
  for (i in names(data_wide)) {
    data_wide[[i]] <- data_wide[[i]][, to_remove]
  }
  
  symbols <- names(data_wide$price_adjusted)
  fxs     <- metadata$equities$`Market Currency`[metadata$equities$Ticker %in% symbols]
  fxs     <- paste0(paste0(fxs, parameters$pf_currency), "=X")
  
  for (i in names(data_wide)[!names(data_wide) %in% "volume"]) {
    for (n in names(data_wide[[i]])) {
      id                   <- names(data_wide[[i]]) %in% n
      data_wide[[i]][, id] <- as.numeric(data_wide[[i]][, id]) / as.numeric(fx_rates[, names(fx_rates) %in% fxs[symbols %in% n]])
    }
  }
  
  data_wide$ret_adjusted_prices <- apply(data_wide$price_adjusted, 2, Delt)
  
  for (i in names(data_wide)) {data_wide[[i]] <- as.data.frame(data_wide[[i]])}
  
  data_wide$index    <- or_index[final_index]
  data_wide$fx_index <- fx_index[fx_final_index]
  data_wide$tickers  <- symbols

  gc()
  
  return(data_wide)
}

# ----------------------------------------------------------------------------------------------------------- #

mlag <- function
(
  m,
  nlag = 1
)
{
  if( is.null(dim(m)) ) {
    n = length(m)
    if(nlag > 0) {
      m[(nlag+1):n] = m[1:(n-nlag)]
      m[1:nlag] = NA
    } else if(nlag < 0) {
      m[1:(n+nlag)] = m[(1-nlag):n]
      m[(n+nlag+1):n] = NA
    }
  } else {
    n = nrow(m)
    if(nlag > 0) {
      m[(nlag+1):n,] = m[1:(n-nlag),]
      m[1:nlag,] = NA
    } else if(nlag < 0) {
      m[1:(n+nlag),] = m[(1-nlag):n,]
      m[(n+nlag+1):n,] = NA
    }
  }
  return(m)
}

# ----------------------------------------------------------------------------------------------------------- #

na.identif <- function (x, n = 0) 
{
  if (is.null(dim(x)[2])) {
    NAs <- sum(is.na(x))
    if (NAs > 0) {
      if (any(is.na(x[-(1:NAs)]))) 
        return(1)
    }
  }
  else {
    NAs <- sum(rowSums(is.na(x)) > 0)
    if (NAs > 0) {
      if (any(is.na(x[-(1:NAs), ]))) 
        return(1)
    }
  }
  return(0)
}

# ----------------------------------------------------------------------------------------------------------- #

build.indicators <- function(
  data,
  parameters,
  benchmark,
  tickers
)
{
  indicators <- list()
  prices     <- data$price.adjusted
  return     <- data$ret.adjusted.prices
  volume     <- data$volume
  nrow       <- nrow(prices)
  ncol       <- ncol(prices)
  rm_return  <- benchmark$ret.adjusted.prices
  
  if (parameters$filename == "ANN_portfolio.R") {
    month     <- neuralnet$parameters$month
    est_range <- neuralnet$parameters$est_range
  } else {
    month     <- parameters$month
    est_range <- parameters$est_range
  }

  if (parameters$full_lookback) {
    lookback <- c(1 * month, 3 * month, 6 * month, 12 * month, 24 * month)
  } else {
    lookback <- c(1 * month, 6 * month, 12 * month)
  }

  # DEPENDENT VARIABLE
  er       <- as.data.frame(apply(prices, 2, Delt))
  roll_er  <- apply(er, 2, FUN = function(x, y) {RcppRoll::roll_meanl(x, n = y, na.rm = TRUE)}, y = est_range)
  roll_er  <- ifelse(is.nan(roll_er), NA, roll_er)
  quantile <- apply(roll_er, 1, FUN = function(x) {as.numeric(quantile(x, probs = seq(0, 1, 0.01), na.rm = T)[parameters$mom_percentile])})
  logical  <- roll_er * NA
  
  for (i in 1:nrow(roll_er)) {logical[i, ] <- roll_er[i, ] > quantile[i]}
  indicators[[paste0("dep_variable")]]     <- as.data.frame(logical)
  
  # 1. Price momentum
  for (i in lookback) {
    if (i > nrow) {break} else {
      indicators[[paste0("price_mom_", i)]] <- as.data.frame(prices/mlag(prices, i))
    }
  }
  
  # Price range
  for (i in lookback) {
    if (i > nrow) {break} else {
      min <- apply(prices, 2, FUN = function(x, y) {RcppRoll::roll_minr(x, n = y, na.rm = TRUE)}, y = i)
      max <- apply(prices, 2, FUN = function(x, y) {RcppRoll::roll_maxr(x, n = y, na.rm = TRUE)}, y = i)
      indicators[[paste0("price_range_", i)]] <- 
        as.data.frame(pmin((pmax(prices - min, 0))/(max - min), 1))
    }
  }
  
  # 2. Price momentum on EMA
  for (i in lookback) {
    if (i > nrow) {break} else {
      ma <- apply(prices, 2, FUN = function(x, y) {movavg(x, n = y, type = "w")}, y = round(i/6, 0))
      indicators[[paste0("price_mom_ema_", i)]] <- as.data.frame(ma/mlag(ma, i))
      rm(ma)
    }
  }

  # 3. EMA
  for (i in lookback) {
    if (i > nrow) {break} else {
      indicators[[paste0("prices_ema_", i)]] <-
        as.data.frame(prices - apply(prices, 2, FUN = function(x, y) {movavg(x, n = y, type = "w")}, y = i))
    }
  }
  
  # Volume EMA
  for (i in lookback) {
    if (i > nrow) {break} else {
      indicators[[paste0("volume_ema_", i)]] <-
        as.data.frame(volume - apply(volume, 2, FUN = function(x, y) {movavg(x, n = y, type = "w")}, y = i))
    }
  }
  
  # 4. RSI
  for (i in lookback) {
    if (i*2 > nrow) {break} else {

      rsi    <- prices * NA
      valid1 <- which(apply(prices, 2, FUN = na.identif) == 0)
      valid2 <- which(apply(prices, 2, FUN = function(x) {
        if (length(which(!is.na(x))) > (i + 1)) {1} else {0}}) == 1)
      valid  <- Reduce(intersect, list(valid1, valid2))

      for (s in valid) {rsi[, s] <- as.numeric(RSI(prices[, s], n = i))}

      indicators[[paste0("rsi_", i)]] <- as.matrix.POSIXlt(rsi)

      rm(valid, rsi)

    }
  }
  
  # 5. EMA(OBV)
  for (i in lookback) {
    if (i > nrow) {break} else {
      
      obv <- prices * NA
      for (x in 1:ncol) {obv[, x] <- OBV(prices[, x], volume[, x])}
      indicators[[paste0("ema_obv_", i)]] <- 
        as.data.frame(apply(obv, 2, FUN = function(x, y) {movavg(x, n = y, type = "w")}, y = i))
      
    }
  }
  
  # 6. Mean
  for (f in lookback) {
    if (f > nrow) {break} else {

      indicators[[paste0("mean_", f)]] <-
        as.data.frame(apply(return, 2, FUN = function(x, y) {RcppRoll::roll_meanr(x, n = y)}, y = f))

    }
  }
  
  # 6. SD
  for (i in lookback) {
    if (i > nrow) {break} else {
      
      indicators[[paste0("sd_", i)]] <- 
        as.data.frame(apply(return, 2, FUN = function(x, y) {RcppRoll::roll_sdr(x, n = y)}, y = i))
      
    }
  }
  
  # 6. Sharpe ratio
  for (i in lookback) {
    if (i > nrow) {break} else {
      
      indicators[[paste0("sharpe_", i)]] <- 
        as.data.frame(apply(return, 2, FUN = function(x, y) {RcppRoll::roll_meanr(x, n = y)}, y = i)/
                      apply(return, 2, FUN = function(x, y) {RcppRoll::roll_sdr(x, n = y)}, y = i))
      
    }
  }
  
  # 7. Beta
  for (i in lookback) {
    if (i > nrow) {break} else {

      indicators[[paste0("beta_", i)]] <-
        as.data.frame(apply(as.matrix.POSIXlt(return), 2, FUN = function(x, y) {
          as.numeric(roll_lm(y, x, width = i, min_obs = i)$coefficients[, 2])
        }, y = as.matrix.POSIXlt(rm_return)))

    }
  }
  
  # 8. MACD
  for (i in lookback) {
    if (i > nrow) {break} else {

      fastMa   <- apply(prices, 2, FUN = function(x) {movavg(x, n = round(i/2, 0), type = "w")})
      slowMa   <- apply(prices, 2, FUN = function(x) {movavg(x, n = i, type = "w")})
      macd     <- (fastMa - slowMa)/slowMa
      signMa   <- apply(macd, 2, FUN = function(x) {movavg(x, n = round(i/3, 0),  type = "w")})
      signMACD <- as.matrix.POSIXlt(macd - signMa)

      indicators[[paste0("macd_", i)]] <- signMACD

    }
  }
  
  # 8. SML
  for (m in lookback) {
    if (m > nrow) {break} else {

      beta <- apply(as.matrix.POSIXlt(return), 2, FUN = function(x, y) {
        as.numeric(roll_lm(y, x, width = m, min_obs = m)$coefficients[, 2])
      }, y = as.matrix.POSIXlt(rm_return))

      asset_ret <- apply(return, 2, FUN = function(x, y) {RcppRoll::roll_meanr(x, n = y)}, y = m)
      rm_ret    <- apply(rm_return, 2, FUN = function(x, y) {RcppRoll::roll_meanr(x, n = y)}, y = m)
      SML       <- apply(beta, 2, FUN = function(x, y) {x * y}, y = rm_ret)
      score_SML <- asset_ret - SML

      indicators[[paste0("sml_", m)]] <- as.data.frame(score_SML)

    }
  }
  
  # Country FLAG
  countries <- data.frame(Ticker  = as.character(tickers$equities$Ticker),
                          Country = as.character(tickers$equities$Location))
  
  for (s in unique(countries$Country)[!is.na(unique(countries$Country))]) {
    indicators[[paste0("country_", s)]] <- indicators[[paste0("price_mom_", lookback[1])]] * NA
    for (q in 1:nrow(indicators[[paste0("country_", s)]])) {
      indicators[[paste0("country_", s)]][q, ] <- ifelse(names(indicators[[paste0("country_", s)]]) 
                                                        %in% countries$Ticker[countries$Country %in% s], 1, 0)
    }
  }
  
  indicators[["country_United States"]] <- NULL
  
  # Market cap
  indicators[["mktcap"]] <- indicators[[paste0("price_mom_", lookback[1])]] * NA
  for (z in 1:nrow(indicators[["mktcap"]])) {
    indicators[["mktcap"]][z, ] <- tickers$equities$`Market Value`[tickers$equities$Ticker
                                                                   %in% names(indicators[["mktcap"]])]
  }

  # Sector FLAG
  sectors <- data.frame(Ticker = as.character(tickers$equities$Ticker),
                        Sector = as.character(tickers$equities$Sector))

  for (s in unique(sectors$Sector)[!is.na(unique(sectors$Sector))]) {
    indicators[[paste0("sector_", s)]] <- indicators[[paste0("price_mom_", lookback[1])]] * NA
    for (q in 1:nrow(indicators[[paste0("sector_", s)]])) {
      indicators[[paste0("sector_", s)]][q, ] <- ifelse(names(indicators[[paste0("sector_", s)]]) 
                                                        %in% sectors$Ticker[sectors$Sector %in% s], 1, 0)
    }
  }
  
  indicators[["Industrials"]] <- NULL

  # EPS growth rate
  indicators[["eps_growthr"]] <- indicators[[paste0("price_mom_", lookback[1])]] * NA
  for (z in 1:nrow(indicators[["eps_growthr"]])) {
    indicators[["eps_growthr"]][z, names(indicators[["eps_growthr"]]) %in% colnames(data$fundamentals)] <-
      as.numeric(data$fundamentals[9, colnames(data$fundamentals) %in% names(indicators[["eps_growthr"]])])
  }
  
  # Shares outstanding
  indicators[["shares_out"]] <- indicators[[paste0("price_mom_", lookback[1])]] * NA
  for (z in 1:nrow(indicators[["shares_out"]])) {
    indicators[["shares_out"]][z, names(indicators[["shares_out"]]) %in% colnames(data$fundamentals)] <-
      as.numeric(data$fundamentals[10, colnames(data$fundamentals) %in% names(indicators[["shares_out"]])])
  }
  
  # Dividend yield
  indicators[["dividend_yield"]] <- indicators[[paste0("price_mom_", lookback[1])]] * NA
  for (z in 1:nrow(indicators[["dividend_yield"]])) {
    indicators[["dividend_yield"]][z, names(indicators[["dividend_yield"]]) %in% colnames(data$fundamentals)] <-
      as.numeric(data$fundamentals[11, colnames(data$fundamentals) %in% names(indicators[["dividend_yield"]])])
  }
  
  # forward_pe
  indicators[["forward_pe"]] <- indicators[[paste0("price_mom_", lookback[1])]] * NA
  for (z in 1:nrow(indicators[["forward_pe"]])) {
    indicators[["forward_pe"]][z, names(indicators[["forward_pe"]]) %in% colnames(data$fundamentals)] <-
      as.numeric(data$fundamentals[2, colnames(data$fundamentals) %in% names(indicators[["forward_pe"]])])
  }

  # pb
  indicators[["pb"]] <- indicators[[paste0("price_mom_", lookback[1])]] * NA
  for (z in 1:nrow(indicators[["pb"]])) {
    indicators[["pb"]][z, names(indicators[["pb"]]) %in% colnames(data$fundamentals)] <-
      as.numeric(data$fundamentals[7, colnames(data$fundamentals) %in% names(indicators[["pb"]])])
  }
  
  return(indicators)

}

# ----------------------------------------------------------------------------------------------------------- #

ann.inputdef <- function(
  indicators,
  parameters,
  tickers
)
{
  out        <- list()
  names      <- names(indicators)[names(indicators) != "dep_variable"]
  full_names <- names(indicators)
  
  if (parameters$filename == "ANN_portfolio.R") {
    month     <- neuralnet$parameters$month
    est_range <- neuralnet$parameters$est_range
    est_lb    <- neuralnet$parameters$estimation_lb
  } else {
    month     <- parameters$month
    est_range <- parameters$est_range
    est_lb    <- parameters$estimation_lb
  }
  
  # 3. Standardizing x variables
  for (i in names) { 
    indicators[[i]] <- as.data.frame(t(apply(indicators[[i]], 1, FUN = function(x) {
      as.numeric((x - mean(x, na.rm = T))/sqrt(var(x, na.rm =T)))
      })))
  }
  
  # 4. Cropping all variables
  if (parameters$filename != "ANN_portfolio.R") {
    if (length(parameters$pf_ret_FLAG == FALSE) == 0) {
      for (x in full_names) {
        
        # indicators[[x]] <- indicators[[x]][(nrow(indicators[[x]]) - est_lb):(nrow(indicators[[x]]) - est_range), ]

        id    <- (nrow(indicators[[x]]) - est_lb - est_range):(nrow(indicators[[x]]) - est_range)
        upper <- length(id) - round((est_lb - est_range) * (1 - parameters$training_size))
        lower <- upper - est_range
        id    <- id[c(1:lower, upper:length(id))]
        
        indicators[[x]] <- indicators[[x]][id, ]
        
      }
    } else {
      if (parameters$pf_ret_FLAG == FALSE) {
        for (x in full_names) {
          indicators[[x]] <- indicators[[x]][(nrow(indicators[[x]]) - est_lb):(nrow(indicators[[x]]) - est_range), ]
        }
      } else {
        for (x in full_names) {
          indicators[[x]] <- indicators[[x]][(nrow(indicators[[x]]) - est_lb):nrow(indicators[[x]]), ]
        }
      }
    }
  } else {
    for (x in full_names) {
      indicators[[x]] <- indicators[[x]][(nrow(indicators[[x]]) - est_lb):nrow(indicators[[x]]), ]
    }
  }

  # 5. Organizing the final data frame
  tmp <- reverseList(indicators)
  for (n in names(tmp)) {tmp[[n]] <- as.data.frame(tmp[[n]])}
  
  final  <- list.rbind(tmp)
  keep_1 <- !apply(final, 2, FUN = function(x) {all(is.na(x))})
  final  <- final[, keep_1]
  keep_2 <- as.numeric(apply(final, 2, FUN = function(x) {length(which((is.nan(x))))}))
  final  <- final[, keep_2 == 0]
  keep_3 <- as.numeric(apply(final, 2, FUN = function(x) {length(which((is.na(x))))}))
  final  <- final[, (keep_3 < nrow(final) * 0.20)]
  final  <- na.omit(final)
  
  return(final)
}

# ----------------------------------------------------------------------------------------------------------- #

fundamental.ratios <- function(
  prices,
  metadata
)
{
  
  source("getQuote.R")

  fund         <- c("trailingPE", "forwardPE", "priceToBook",
                    "epsTrailingTwelveMonths", "epsForward", 
                    "sharesOutstanding", "trailingAnnualDividendYield")
  fund_names   <- c("P/E Ratio", "Price/EPS Estimate Next Year",
                    "Price/Book", "Earnings/Share", "EPS Forward", 
                    "Shares Outstanding", "Dividend Yield")
  
  fundamentals <- list()
  tickers      <- names(prices)
  
  for (i in fund) {
    repeat {
      tmp <- try(getQuote(tickers, what = i))
      if (!("try-error" %in% class(tmp)))
        break
    }
    fundamentals[[i]] <- t(tmp)
  }
  
  fundamental_table           <- as.matrix(prices[1:12, ]) * NA
  rownames(fundamental_table) <- c("NA", "Trailing PE", "Forward PE", 
                                   "Trailing EPS", "Forward EPS", "PEG", "1Y Growth Factor", "PB",
                                   "Value Judgement", "EPS growth rate", "Shares Outstanding (B)", "Dividend Yield")
  
  eps_growthr <- ((as.numeric(fundamentals$epsForward[2, ])-
                     as.numeric(fundamentals$epsTrailingTwelveMonths[2, ]))/
                    as.numeric(fundamentals$epsTrailingTwelveMonths[2, ]))*100
  
  c       <- 1
  sectors <- fundamental_table[1, ]
  
  for (j in tickers) {
    sectors[c] <- paste0("", unique(metadata$equities$Sector[metadata$equities$Ticker %in% j]))
    c <- c + 1
  }
  
  sectors <- ifelse(sectors == "", NA, sectors)
  
  fundamental_table[2, ]  <- as.numeric(fundamentals$trailingPE[2, ])
  fundamental_table[3, ]  <- as.numeric(fundamentals$forwardPE[2, ])
  fundamental_table[4, ]  <- as.numeric(fundamentals$epsTrailingTwelveMonths[2, ])
  fundamental_table[5, ]  <- as.numeric(fundamentals$epsForward[2, ])
  fundamental_table[6, ]  <- as.numeric(fundamentals$trailingPE[2, ])/eps_growthr
  fundamental_table[7, ]  <- as.numeric(fundamentals$trailingPE[2, ])/as.numeric(fundamentals$forwardPE[2, ])
  fundamental_table[8, ]  <- as.numeric(fundamentals$priceToBook[2, ])
  fundamental_table[11, ] <- as.numeric(fundamentals$sharesOutstanding[2, ])/1e9
  fundamental_table[12, ] <- as.numeric(fundamentals$trailingAnnualDividendYield[2, ])
  
  fundamental_table <- round(fundamental_table, 3)
  alpha             <- parameters$alpha * 10
  
  for (h in unique(metadata$equities$Sector)) {
    
    pe       <- as.numeric(fundamental_table[2, sectors %in% h])
    pe       <- pe[pe < quantile(pe, probs = 1 - alpha, na.rm = T) & pe > quantile(pe, probs = alpha, na.rm = T)]
    quant_pe <- as.numeric(quantile(pe, probs = c(0.33, 0.66), na.rm = T))
    
    pb       <- as.numeric(fundamental_table[8, sectors %in% h])
    pb       <- pb[pb < quantile(pb, probs = 1 - alpha, na.rm = T) & pb > quantile(pb, probs = alpha, na.rm = T)]
    quant_pb <- as.numeric(quantile(pb, probs = c(0.33, 0.66), na.rm = T))
    
    for (s in which(sectors %in% h)) {
      fundamental_table[9, s] <- ifelse(
        as.numeric(fundamental_table[2, s]) <= quant_pe[1] &
          as.numeric(fundamental_table[8, s]) <= quant_pb[2] , "VALUE",
        ifelse(
          as.numeric(fundamental_table[2, s]) >= quant_pe[2] &
            as.numeric(fundamental_table[8, s]) >= quant_pb[2] , "GROWTH", "NEUTRAL"   
        )
        
      )
    }
  }
  
  fundamental_table[10, ] <- round(eps_growthr/100, 3)
  
  return(fundamental_table[-1, ])
  
}

# ----------------------------------------------------------------------------------------------------------- #

write.hypo <- function(
  hypo, 
  parameters, 
  id, 
  sys.date
  ) 
{
  View(hypo)
  saveRDS(hypo, file = paste0(parameters$outputdir, "/hypo_", sys_date, ".RDS"))
}

write.nn <- function(
  parameters, 
  ann,
  train,
  rel_imp
) 
{
  if (parameters$test == FALSE) {
    if (parameters$filename == "ANN_simulation.R") {
      saveRDS(list(parameters = parameters, nn = ann$nn, 
                   predictors = names(train$training), rel_imp = rel_imp
      ), file = paste0(parameters$outputdir, "/ann_", parameters$estimation_date, ".RDS"))
    }
  }
}

# ----------------------------------------------------------------------------------------------------------- #

train.ann <- function(
  parameters,
  ann_input
)
{
  if (parameters$test == TRUE) {
    training_size <- parameters$training_size
    num           <- gregexpr("[0-9]+", rownames(ann_input))
    numbers       <- as.numeric(unlist(lapply(regmatches(rownames(ann_input), num),FUN = function(x) {x[length(x)]})))
    training_rows <- which(numbers <= round(max(numbers) * training_size))
    training      <- ann_input[training_rows,]
    validation    <- ann_input[-training_rows,]
    return(list(training = training, validation = validation))
  } else {
    return(list(training = ann_input, validation = NULL))
  }
}

# ----------------------------------------------------------------------------------------------------------- #

ann.simulation <- function(
  parameters,
  train
) 
{
  training <- train$training
  if (parameters$test == TRUE) {
    validation <- train$validation
    
    sigma    <- parameters$sigma
    n        <- names(training)
    f        <- as.formula(paste("dep_variable ~", paste(n[!n %in% "dep_variable"], collapse = " + ")))
    neurons  <- nrow(training)/(sigma * ncol(training))

    if (parameters$layers == 1) {
      hidden <- max(round(neurons), 2)
    } else if (parameters$layers == 2) {
      hidden <- c(max(round(neurons * 2/3), 2), max(round(neurons * 1/3), 1))
    } else if (parameters$layers == 3) {
      hidden <- c(max(round(neurons * 1.5/3), 2), max(round(neurons * 1/3), 1), max(round(neurons * 0.5/3), 1))
    } else {
      stop("Computation stopped: hidden layers cannot be bigger than 3.")
    }
    
    nn      <- neuralnet(f, data = as.matrix(training), hidden = hidden, act.fct = "logistic",
                         linear.output = FALSE, stepmax = 1e7, threshold = 0.5, err.fct = "ce")
    pr.nn   <- compute(nn, validation[, -1])
    results <- data.frame(actual = validation$dep_variable, prediction = round(pr.nn$net.result, 0))
    
    if (exists("nn")) {
      if (sum(results$prediction) == 0) {
        accuracy <- NA
      } else {
        accuracy <- confusionMatrix(table(results$actual, results$prediction))
        # print(accuracy)
      }
      
      return(list(sigma = sigma, f = f, neurons = neurons, hidden = hidden, 
                  nn = nn, pr.nn = pr.nn, accuracy = accuracy))
    } else {
      return(list(sigma = sigma, f = f, neurons = neurons, hidden = hidden, 
                  nn = NA, pr.nn = NA, accuracy = NA))
    }
    
  } else {
    sigma    <- parameters$sigma
    n        <- names(training)
    f        <- as.formula(paste("dep_variable ~", paste(n[!n %in% "dep_variable"], collapse = " + ")))
    neurons  <- nrow(training)/(sigma * ncol(training))
    
    if (parameters$layers == 1) {
      hidden <- max(round(neurons), 2)
    } else if (parameters$layers == 2) {
      hidden <- c(max(round(neurons * 2/3), 2), max(round(neurons * 1/3), 1))
    } else if (parameters$layers == 3) {
      hidden <- c(max(round(neurons * 1.5/3), 2), max(round(neurons * 1/3), 1), max(round(neurons * 0.5/3), 1))
    } else {
      stop("Computation stopped: hidden layers cannot be bigger than 3.")
    }
    
    nn <- neuralnet(f, data = as.matrix(training), hidden = hidden, act.fct = "logistic",
                    linear.output = FALSE, stepmax = 1e7, threshold = 0.5, err.fct = "ce")
    
    return(list(sigma = sigma, f = f, neurons = neurons, hidden = hidden, nn = nn))
  }
}

# ----------------------------------------------------------------------------------------------------------- #

eff.frontier <- function(returns, cov, short, max_alloc, risk_premium_up,
                         risk_increment) {
  n <- ncol(returns)
  
  if (short == TRUE) {
    Amat <- matrix(1, nrow = n)
    bvec <- 1
    meq  <- 1
  } else if (short == FALSE) {
    Amat <- cbind(1, diag(n))
    bvec <- c(1, rep(0, n))
    meq  <- 1
    if (!is.null(max_alloc)) {
      if (max_alloc > 1 | max_alloc < 0) {
        stop("Maximum allocation must be greater than 0 and less than 1.")
      }
      if ((max_alloc * n) < 1) {
        stop("Maximum allocation must be set higher, enough assets needed to add to 1.")
      }
      Amat <- cbind(Amat, -diag(n))
      bvec <- c(bvec, rep(-max_alloc, n))
    }
    loops         <- (risk_premium_up / risk_increment + 1)
    loop          <- 1
    eff           <- matrix(nrow = loops, ncol = n + 3)
    colnames(eff) <- c(colnames(returns), "Std.Dev", "Exp.Return", "Sharpe")
    L             <- seq(from = 0, to = risk_premium_up, by = risk_increment)
    for (i in L) {
      dvec <- colMeans(returns) * i
      sol  <- solve.QP(cov, dvec = dvec, Amat = Amat, bvec = bvec, meq = meq)
      
      eff[loop, "Std.Dev"]    <- sqrt(sum(sol$solution * colSums((cov * sol$solution))))
      eff[loop, "Exp.Return"] <- as.numeric(sol$solution %*% colMeans(returns))
      eff[loop, "Sharpe"]     <- eff[loop, "Exp.Return"] / eff[loop, "Std.Dev"]
      eff[loop, 1:n]          <- sol$solution
      loop                    <- loop + 1
    }
  }
  out           <- t(as.data.frame(eff[1, ]))
  rownames(out) <- "GMV"
  return(out)
}

# ----------------------------------------------------------------------------------------------------------- #

performance.measures <- function(
  ann,
  parameters
)
{
  if (parameters$test == TRUE & parameters$filename == "ANN_simulation.R") {
    print(ann$accuracy)
  }
  
  if (parameters$filename == "ANN_simulation.R" | parameters$filename == "ANN_hypotetical.R") {

    source_gist('6206737')
    
    rel_imp         <- gar.fun(out.var = 'dep_variable', mod.in = ann$nn)$data
    rel_imp$rel.imp <- abs(rel_imp$rel.imp)
    rel_imp         <- rel_imp[order(rel_imp$rel.imp), ]
    
    chr <- sort(unique(gsub('[0-9]+', '', as.character(rel_imp$x.names))))
    chr <- c(chr[!grepl("sector_", chr, fixed = TRUE)], "sector")
    chr <- c(chr[!grepl("country_", chr, fixed = TRUE)], "country")
    chr <- c(chr[!grepl("size_", chr, fixed = TRUE)], "size")
    chr <- c(chr[!grepl("valuation_", chr, fixed = TRUE)], "valuation")
    out <- as.data.frame(matrix(0, nrow = length(chr), ncol = 2))
    
    names(out) <- c("Indicator", "Average importance")
    
    for (i in chr) {
      id <- which(chr %in% i)
      out[id, ] <- c(i, round(mean(rel_imp$rel.imp[grepl(i, as.character(rel_imp$x.names), fixed = TRUE)], na.rm = T), 3))
    }
    
    out <- out[rev(order(out$`Average importance`)), ]
    
    num     <- unique(str_extract(as.character(rel_imp$x.names), "[[:digit:]]+"))
    num     <- as.character(sort(as.numeric(num[!is.na(num)])))
    out_num <- as.data.frame(matrix(0, nrow = length(num), ncol = 2))
    
    names(out_num) <- c("LB period", "Average importance")
    
    for (i in num) {
      id <- which(num %in% i)
      out_num[id, ] <- c(i, round(mean(rel_imp$rel.imp[grepl(i, as.character(rel_imp$x.names), fixed = TRUE)], na.rm = T), 3))
    }
    
    out_num <- out_num[rev(order(out_num$`Average importance`)), ]
    
    output <- list(full = rel_imp, partial_indicator = out, partial_lookback = out_num, 
                   initial = gar.fun(out.var = 'dep_variable', mod.in = ann$nn)$data)
    
    saveRDS(output, file = paste0(parameters$outputdir, "/relative_importance_", parameters$estimation_date, ".RDS"))
    
    return(output)
  }
}

# ----------------------------------------------------------------------------------------------------------- #

read.ann <- function(
  parameters
)
{
  
  if (parameters$filename != "ANN_portfolio.R") {
    path <- paste0(parameters$outputdir, "/", list.files(path = parameters$outputdir, pattern = paste0("ann_", parameters$estimation_date)))
  } else {
    ctime <- file.info(paste0(parameters$outputdir, "/", list.files(path = parameters$outputdir, pattern = "ann")))$ctime
    if (as.Date(max(ctime)) < (Sys.Date() - parameters$month)) {
      stop("Process stopped: neural network older than a month. Please re-estimate your model.")
    } else {
      path <- paste0(parameters$outputdir, "/", list.files(path = parameters$outputdir, pattern = "ann"))[ctime %in% max(ctime)]
    }
  }
  
  neuralnet <- readRDS(path)
  
  return(neuralnet)
}

# ----------------------------------------------------------------------------------------------------------- #

ann.asset.returns <- function(
  ann_input,
  neuralnet,
  data,
  parameters
)
{
  month      <- neuralnet$parameters$month
  num        <- gregexpr("[0-9]+", rownames(ann_input))
  numbers    <- as.numeric(unlist(lapply(regmatches(rownames(ann_input), num),FUN = function(x) {x[length(x)]})))
  pf_rows    <- which(numbers == max(numbers))
  ann_input  <- ann_input[pf_rows, colnames(ann_input)[colnames(ann_input) != "dep_variable"]]
  prediction <- neuralnet::compute(neuralnet$nn, ann_input)
  names      <- str_replace(rownames(ann_input)[ifelse(prediction$net.result >= 0.99, TRUE, FALSE)], 
                            paste0(".", max(numbers)), "")
  id         <- (nrow(data$ret.adjusted.prices) - month) : nrow(data$ret.adjusted.prices)
  prices     <- data$price.adjusted[id, names(data$price.adjusted) %in% names]
  
  cat("available equities: ", length(which(ifelse(prediction$net.result >= 0.99, TRUE, FALSE))), sep = "", "\n")
  
  ret <- apply(prices, 2, Delt)[-1, ]
  iqr <- apply(ret, 2, FUN = function(x) {IQR(x, na.rm = T)}) 
  mad <- apply(ret, 2, FUN = function(x) {mad(x, na.rm = T)})
  nas <- apply(ret, 2, FUN = function(x) {length(which(is.na(x)))})
  ret <- ret[, (iqr != 0 & mad != 0 & nas == 0)]
  out <- apply(ret, 2, mean)     <= quantile(apply(ret, 2, mean),     probs = 0.01) |
         apply(ret, 2, sd)       >= quantile(apply(ret, 2, sd),       probs = 0.99) |
         apply(ret, 2, skewness) >= quantile(apply(ret, 2, skewness), probs = 0.99) |
         apply(ret, 2, kurtosis) <= quantile(apply(ret, 2, kurtosis), probs = 0.01)
  
  ret    <- as.data.frame(ret[, out == FALSE])
  names  <- colnames(ret)
  prices <- data$price.adjusted[id, names]
  prices <- as.data.frame(prices[-1, ])
  
  return(list(prices = prices, returns = ret, prediction = prediction))
}

# ----------------------------------------------------------------------------------------------------------- #

agg.features <- function(
  x,
  eff_frontier,
  pf_size
)
{
  tab           <- eff_frontier[1:pf_size]
  names(tab)    <- x
  tab           <- data.frame(t(aggregate(tab, by = list(names(tab)), FUN = sum)))
  colnames(tab) <- tab[1, ]
  tab           <- setNames(as.numeric(tab[-1, ]), names(tab))
  
  return(tab)
}

# ----------------------------------------------------------------------------------------------------------- #

composition <- function(
  tickers,
  data,
  eff_frontier
) {
  
  par(mfrow = c(2, 2))
  
  list              <- list()
  list[["country"]] <- macrolocations(tickers)$macroloc$Location[macrolocations(tickers)$macroloc$Ticker %in% names(eff_frontier)]
  list[["sector"]]  <- tickers$equities$Sector[tickers$equities$Ticker %in% names(eff_frontier)]
  list[["size"]]    <- tickers$equities$Size[tickers$equities$Ticker %in% names(eff_frontier)]
  list[["value"]]   <- data$fundamentals[8, names(eff_frontier)]
  
  for (j in names(list)) {
    tab <- agg.features(x = list[[j]], eff_frontier = eff_frontier, pf_size = length(eff_frontier))
    pie(sort(tab), col = hcl.colors(length(tab), "BluYl"))
  }
  
}

# ----------------------------------------------------------------------------------------------------------- #

source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}

# ----------------------------------------------------------------------------------------------------------- #

outlier.detection <- function(
  parameters,
  indicators
) {
  names <- names(indicators)[names(indicators) != "dep_variable"]
  alpha <- parameters$alpha
  for (i in names) {
    indicators[[i]] <- as.data.frame(t(apply(indicators[[i]], 1, FUN = function(x, y) {
      x <- as.numeric(x)
      if (all(is.na(x)) == TRUE) {
        return(x)
      } else {
        upper <- quantile(x, 1 - y, na.rm = T)
        lower <- quantile(x, y, na.rm = T)
        if (is.na(upper) | is.na(lower)) {
          return(x)
        } else {
          x[which(x < lower | x > upper)] <- NA
          return(x)
        }
      }
    }, y = alpha)))
  }
  return(indicators)
}

# ----------------------------------------------------------------------------------------------------------- #

importance.assessment <- function(
  parameters,
  type
) 
{
  names <- list.files(path = parameters$outputdir, pattern = "relative_importance")
  files <- paste0(parameters$outputdir, "/", names)
  rds   <- list()
  
  for (i in files) {
    rds[[names[files %in% i]]] <- readRDS(i)[[type]]
  }
  
  ln  <- length(names(rds))
  out <- as.data.frame(matrix(NA, nrow = nrow(rds[[1]]), ncol = ln + 1))
  
  if (type == "partial_indicator") {
    out$V1 <- sort(rds[[1]]$Indicator)
  } else {
    out$V1 <- as.character(sort(as.numeric(rds[[1]]$`LB period`)))
  }
  
  for (i in 1:ln) {
    out[out$V1 %in% rds[[i]][, 1], 1 + i] <- rds[[i]][rds[[i]][, 1] %in% out$V1, 2]
  }
  
  out$mean <- apply(out[, 2:ncol(out)], 1, FUN = function(x) {mean(as.numeric(x), na.rm = T)})
  out      <- out[rev(order(out$mean)), ]
  
  View(out)
  
  return(out)
}

# ----------------------------------------------------------------------------------------------------------- #
