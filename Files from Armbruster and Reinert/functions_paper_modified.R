fit_and_predict <- function(alpha, beta, 
                            globalalpha, 
                            net,
                            vts = covid_cases, 
                           
                            
                            # if not NULL, coefficients are computed for 
                            # vertex classes  
                            weight_factor = NULL, 
                            
                            # if TRUE, fits INV-D weighting 
                            inverse_distance = TRUE,
                            
                           
                            
                            # data frame with column CountyName and its numerical encoding 
                            county_index = NULL, 
                            
                            # if TRUE, the original GNARfit() function is applied
                            old = TRUE, 
                            
                            return_model = FALSE, 
                            forecast_window
) {
  
  train_window <- dim(vts)[1] - forecast_window
  
  # fit model according to given settings 
  if (weight_factor %>% is.null()) {
    # if (!old) {
    #   model <- GNARfit_weighting(vts = vts[1:train_window, ], 
    #                              net = net,
    #                              alphaOrder = alpha, 
    #                              betaOrder = beta, 
    #                              globalalpha = globalalpha, 
    #                              inverse_distance = inverse_distance,
    #                              county_index = county_index
    #   )
    # } 
    if (old) {
      model <- GNARfit(vts = vts[1:train_window, ], 
                       net = net,
                       alphaOrder = alpha, 
                       betaOrder = beta, 
                       globalalpha = globalalpha
      )
    }
    
  } else {
    # if (!old) {
    #   model <- GNARfit_weighting(vts = vts[1:train_window, ],
    #                              net = net, 
    #                              alphaOrder = alpha, 
    #                              betaOrder = beta, 
    #                              globalalpha = globalalpha, 
    #                              fact.var = weight_factor, 
    #                              inverse_distance = inverse_distance,
    #                              county_index = county_index
    #   )
    # }
    if (old) {
      model <- GNARfit(vts = vts[1:train_window, ], 
                       net = net,
                       alphaOrder = alpha, 
                       betaOrder = beta, 
                       globalalpha = globalalpha, 
                      
      )
    }
  }
  
  if (!return_model) {
    # return data frame with RSS and BIC value for model 
    return(data.frame("RSS" = model$mod$residuals^2 %>% sum(), 
                      "BIC" = BIC(model)))
  } 
  if (return_model) {
    # return model 
    return(model)
  }
}


fit_and_predict_for_many <- function(alpha_options = seq(1, 7), 
                                     beta_options = list(0, 1, 2, 3, 
                                                         4, 5,
                                                         c(1, 1), 
                                                         c(2, 1), 
                                                         c(2, 2), 
                                                         c(3, 1),
                                                         c(4, 1),
                                                         c(5, 1),
                                                         c(1, 1, 1), 
                                                         c(2, 1, 1),
                                                         c(3, 1, 1),
                                                         c(4, 1, 1),
                                                         c(5, 1, 1),
                                                         c(2, 2, 1), 
                                                         c(2, 2, 2),
                                                         c(3, 2, 2), 
                                                         c(4, 2, 2),
                                                         c(5, 2, 2), 
                                                         c(1, 1, 1, 1), 
                                                         c(2, 1, 1, 1),
                                                         c(3, 1, 1, 1), 
                                                         c(4, 1, 1, 1),
                                                         c(5, 1, 1, 1),
                                                         c(2, 2, 1, 1),
                                                         c(2, 2, 2, 1), 
                                                         c(3, 2, 2, 1), 
                                                         c(4, 2, 2, 1),
                                                         c(5, 2, 2, 1),
                                                         c(2, 2, 2, 2),
                                                         # c(7, 2, 2, 2, 0),
                                                         c(1, 1, 1, 1, 1),
                                                         c(2, 1, 1, 1, 1),
                                                         c(3, 1, 1, 1, 1), 
                                                         c(4, 1, 1, 1, 1),
                                                         # c(7, 1, 1, 1, 1),
                                                         c(2, 2, 1, 1, 1), 
                                                         c(2, 2, 2, 1, 1),
                                                         c(3, 2, 2, 1, 1),
                                                         c(4, 2, 2, 1, 1),
                                                         c(5, 2, 2, 1, 1),
                                                         # c(7, 2, 2, 1, 1),
                                                         c(2, 2, 2, 2, 1), 
                                                         c(2, 2, 2, 2, 2)
                                     ), # in form of list
                                     globalalpha = c("TRUE", "FALSE"), 
                                     upper_limit = 5, 
                                     net, 
                                     vts = flu_norm_ts, 
                                     
                                     # if not NULL, coefficients are computed for 
                                     # vertex classes
                                     weight_factor = NULL, 
                                     
                                     # if TRUE, fits INV-D weighting 
                                     inverse_distance = TRUE, 
                                     # data frame with column CountyName and its numerical encoding 
                                     county_index = NULL, 
                                     
                                     # if TRUE, the original GNARfit() function is applied
                                     old = TRUE,
                                     
                                     forecast_window = 5) {
  
  train_window <- dim(vts)[1] - forecast_window
  
  # create all possible combinations of alpha and beta order 
  model_options <-  expand.grid(alpha_options, 
                                beta_options, 
                                globalalpha)
  
  model_options$valid <- NA
  
  for(i in seq(1, nrow(model_options))) {
    
    current_option <- model_options[i, ]
    
    model_options[i, ]$valid <-  ifelse(length(current_option$Var2[[1]]) > current_option$Var1 | 
                                          any(current_option$Var2[[1]] > upper_limit),  
                                        FALSE, 
                                        TRUE
    )
    
    if(model_options$valid[i]) {
      model_options[i, ]$Var2[[1]] <- c(current_option$Var2[[1]], 
                                        rep(0, current_option$Var1 - length(current_option$Var2[[1]])))
    } else {
      model_options[i, ]$Var2[[1]] <- NA
    }
  }
  
  model_options_valid <- model_options %>% 
    filter(!is.na(Var2), 
           !duplicated(Var2))
  
  # filter out valid parameter combinations 
  # model_options$valid <- (model_options$Var1 == model_options$Var2 %>% 
  #                           lapply(FUN = function(i) {length(i)}) %>% 
  #                           unlist())
  # model_options_valid <- model_options %>% filter(valid == TRUE)
  
  # for fully connected graph, only the first stage neighbourhood can be 
  # constructed 
  # if (any(net %>% 
  #         GNARtoigraph() %>% 
  #         igraph::degree() == net$edges %>% 
  #         length() - 1)) {
  #   only_1 <- model_options_valid %>% 
  #     pull(Var2) %>% 
  #     lapply(FUN = function(i) {not(2 %in% i)}) %>% 
  #     unlist() 
  #   
  #   model_options_valid <- model_options_valid[only_1, ]
  # }
  
  BIC_RSS <- data.frame()
  
  for (i in seq(1, nrow(model_options_valid))) {
    # select model settings 
    model_setting <- model_options_valid[i, ]
    
    # create name 
    name <- paste("GNAR", 
                  model_setting$Var1 %>% as.character(), 
                  model_setting$Var2[[1]] %>% paste0(collapse = ""), 
                  model_setting$Var3 %>% as.character(), 
                  sep = "-")
    
    # fit model
    results <- fit_and_predict(alpha = model_setting$Var1, 
                               beta = model_setting$Var2[[1]], 
                               globalalpha = model_setting$Var3 %>% as.logical(), 
                               net = net, vts = vts, 
                               weight_factor = weight_factor, 
                               inverse_distance = inverse_distance,
                               county_index = county_index, 
                               old = old, 
                               forecast_window = forecast_window)
    results$name <- name
    BIC_RSS <- rbind(BIC_RSS, 
                     results)
    
  }
  
  return(BIC_RSS[, c(3, 1, 2)])
}


