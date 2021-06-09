
h2o_automl <- function(df0, classifn = 'clf', 
                       train_propn_ui=train_propn_ui,max_runtime_secs_ui=30){
  
  # Partition input Data
  set.seed(222)
  ind <- sample(2, nrow(df0), replace = TRUE, 
                prob = c(train_propn_ui, (1 - train_propn_ui))) # from UI
  train <- df0[ind==1,]
  test <- df0[ind==2,]
  
  #h2o.init()
  # build H2O frames
  train_h2o = as.h2o(train)
  test_h2o = as.h2o(test)
  #pred_h2o = as.h2o(pred_data)
  
  y <- "y"  # select DV variable name by user
  x <- setdiff(names(train), y)
  
  'discern classifn vs regn for autoML'
  if (classifn == 'clf'){
    train_h2o[,y] <- as.factor(train_h2o[,y]) 
  }
  
  # Create automl obj by passing the mandatory parms.
  system.time({
    aml <- h2o.automl(x = x, y = y,
                      training_frame = train_h2o,
                      max_runtime_secs = max_runtime_secs_ui,seed = 222) # from UI.
  }) # 46 s
  
  return(aml)
}
