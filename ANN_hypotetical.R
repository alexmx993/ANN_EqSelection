

names <- c("sim_id", "time_secs", 
           "tickers_used", "estimation_date", "date_id", "mom_percentile", "est_range", 
           "estimation_lb", "training_size", "predictors", "sigma", 
           "neurons", "tickers_nr", "train_rows", "alpha", "layers", "seed_hypo", "seed_tickers",
           "accuracy", "kappa", "accuracy_lower", "accuracy_upper", "accuracy_null", "accuracy_pvalue", "mcnemar_pvalue",
           "sensitivity", "specificity", "pos_pred_value", "neg_pred_value", "precision", "recall", "f1", "prevalence", 
           "detection_rate", "detection_prevalence", "balanced_accuracy")

rep      <- 1000
seed     <- round(runif(1, 1, 99999))
sys_date <- Sys.Date()

set.seed(seed)

hypo                 <- as.data.frame(matrix(0, nrow = rep, ncol = length(names)))
colnames(hypo)       <- names
hypo$tickers_used    <- 0.20
hypo$estimation_date <- sample(as.character(seq(Sys.Date() - (365 * 2), Sys.Date(), by = "days")), size = rep, replace = T)
hypo$mom_percentile  <- 50 + 1
hypo$est_range       <- 1
hypo$estimation_lb   <- sample(seq(1.1, 2, 0.1), size = rep, replace = T)
hypo$training_size   <- 0.75
hypo$sigma           <- sample(1:10, size = rep, replace = T)
hypo$alpha           <- sample(seq(0.001, 0.1, 0.001), size = rep, replace = T)
hypo$layers          <- sample(2:3, size = rep, replace = T)
hypo$full_lookback   <- sample(c(T, F), size = rep, replace = T)
hypo$seed_hypo       <- seed
hypo$seed_tickers    <- round(runif(rep, 1, 99999))
hypo                 <- hypo[!hypo$estimation_lb <= hypo$est_range, ]

rep <- nrow(hypo)

for(id in 1:rep) {
  
  set.seed(hypo$seed_tickers[id])
  
  start <- Sys.time()
  
  # ------------------------------------------------------------------------------------------------ #
  
  parameters                      <- list()
  parameters[["maindir"]]         <- "C:/Users/Alessio/Desktop/ANN_model"
  parameters[["inputdir"]]        <- paste0(parameters$maindir, "/input")
  parameters[["outputdir"]]       <- paste0(parameters$maindir, "/output")
  parameters[["cache_folder"]]    <- paste0(parameters$maindir, "/cache_folder")
  parameters[["month"]]           <- 30
  parameters[["tickers_used"]]    <- hypo$tickers_used[id]
  parameters[["estimation_date"]] <- hypo$estimation_date[id]
  parameters[["mom_percentile"]]  <- hypo$mom_percentile[id]
  parameters[["est_range"]]       <- hypo$est_range[id] * parameters[["month"]]
  parameters[["estimation_lb"]]   <- round(hypo$estimation_lb[id] * parameters[["month"]])
  parameters[["sigma"]]           <- hypo$sigma[id]
  parameters[["training_size"]]   <- hypo$training_size[id]
  parameters[["benchmark"]]       <- "URTH"
  parameters[["test"]]            <- TRUE
  parameters[["alpha"]]           <- hypo$alpha[id]
  parameters[["layers"]]          <- hypo$layers[id]
  parameters[["full_lookback"]]   <- hypo$full_lookback[id]
  parameters[["pf_currency"]]     <- "EUR"
  parameters[["filename"]]        <- "ANN_hypotetical.R"
  parameters[["file_names"]]      <- c("URTH_holdings.xlsx",
                                       "ticker_extension.xlsx")

  withTimeout({
    suppressWarnings(suppressMessages(source("ANN_simulation.R")))
  }, timeout = 1500, onTimeout = "silent")

  # ------------------------------------------------------------------------------------------------ #
  
  if (exists("ann")) {
    
    end <- Sys.time()
    who <- sapply(tickers$tickers, FUN = function(x, y) {length(which(grepl(x, y, fixed = TRUE)))}, y = rownames(ann_input))
    
    hypo$sim_id[id]     <- id
    hypo$time_secs[id]  <- as.numeric(difftime(end, start, units = 'secs'))
    hypo$neurons[id]    <- sum(ann$hidden)
    hypo$tickers_nr[id] <- length(which(who > 0))
    hypo$train_rows[id] <- nrow(train$training)
    hypo$predictors[id] <- ncol(train$training) - 1
    
    if (!is.na(ann$accuracy)) {
      hypo[id, 19:25] <- as.numeric(ann$accuracy$overall)
      hypo[id, 26:36] <- as.numeric(ann$accuracy$byClass)
    }
    
    write.hypo(hypo, parameters, id, sys.date)
    rm(ann)
    
  } else {
    
    write.hypo(hypo, parameters, id, sys.date)
    
  }
  
}

