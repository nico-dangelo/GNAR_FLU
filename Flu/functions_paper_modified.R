#Convert State USPS abbreviation to FIPS prefix
require(tidyverse)
STUSPStoSTATEFP <- function(USPS){
  if(!is.null(USPS))
    if(!is.null(US_county_shape))
      fips_lookup <- c(
        "AL" = "01", "AK" = "02", "AZ" = "04", "AR" = "05", "CA" = "06",
        "CO" = "08", "CT" = "09", "DE" = "10", "FL" = "12", "GA" = "13",
        "HI" = "15", "ID" = "16", "IL" = "17", "IN" = "18", "IA" = "19",
        "KS" = "20", "KY" = "21", "LA" = "22", "ME" = "23", "MD" = "24",
        "MA" = "25", "MI" = "26", "MN" = "27", "MS" = "28", "MO" = "29",
        "MT" = "30", "NE" = "31", "NV" = "32", "NH" = "33", "NJ" = "34",
        "NM" = "35", "NY" = "36", "NC" = "37", "ND" = "38", "OH" = "39",
        "OK" = "40", "OR" = "41", "PA" = "42", "RI" = "44", "SC" = "45",
        "SD" = "46", "TN" = "47", "TX" = "48", "UT" = "49", "VT" = "50",
        "VA" = "51", "WA" = "53", "WV" = "54", "WI" = "55", "WY" = "56",
        "DC" = "11", "AS" = "60", "GU" = "66", "MP" = "69", "PR" = "72",
        "VI" = "78"
      )
  STATEFP <- fips_lookup[USPS]
  
  return(STATEFP)
}
#function to extract neighbor dataframe
neighborsDataFrame <- function(nb) {
  
  ks = data.frame(k = unlist(mapply(rep, 1:length(nb), 
                                    sapply(nb, length), 
                                    SIMPLIFY = FALSE) ), 
                  k_nb = unlist(nb) )
  
  nams = data.frame(id = attributes(nb)$region.id, 
                    k = 1:length(nb))
  
  o = merge(ks, nams, 
            by.x = 'k', 
            by.y = 'k')
  o = merge(o, nams, 
            by.x = 'k_nb', 
            by.y = 'k', 
            suffixes = c("","_neigh"))
  
  o[, c("id", "id_neigh")] %>% return()
}
#Function to fit KNN objects  
KNN <- function(state_fp, state_USPS=NULL, county_fips_list=NULL,...){
  #take either single state prefix, postal abbreviation, or list of county fips codes
  if(!is.null(state_USPS){state_fp=USPStoSTATEFP(state_USPS)}
  if(!is.null(state_fp))
    if(is.null(state_USPS)){state_USPS=}
  if(state_fp %in% US_county_shape$STATEFP)
  if(!is.null(county_fips_list))
  if(is.character(county_fips_list))

  assign(paste0("knn_best","" <- list()
  
  
  
  

for(k in length())
  
  }

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
    # return data frame with RSS, log likelihood, and BIC value for model 
    return(data.frame(
    "RSS" = model$mod$residuals^2 %>% sum(), 
                      "LogLik" = logLik(model),
                      "BIC" = BIC(model)))
  } 
  if (return_model) {
    # return model 
    return(model)
  }
}


fit_and_predict_for_many <- function(alpha_options = seq(1, 10), 
                                     beta_options = list(1, 2, 3, 
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
                               forecast_window = forecast_window, return_model = FALSE)
    results$name <- name
    BIC_RSS <- rbind(BIC_RSS, 
                     results)
    
  }
  
  return(BIC_RSS
         # [, c(3, 1, 2)]
         )
}

network_characteristics <- function(igraph_obj, 
                                    network_name) {
  
  density <- igraph_obj %>% edge_density() 
  apl <- igraph_obj %>%  mean_distance(directed = FALSE) 
  
  global_clust <- igraph_obj %>% transitivity(type = "global") 
  mean_local_clust <- igraph_obj %>% 
    transitivity(type = "local",
                 isolates = "zero") %>% 
    mean()
  
  
  degree_v <- igraph_obj %>% igraph::degree()
  av_degree <- degree_v %>% mean() 
  
  # model Bernoulli Random Graph to check for small world behaviour 
  brg <- sample_gnm(n = igraph_obj %>% gorder(), 
                          m = igraph_obj %>% gsize(), 
                          directed = FALSE,
                          loops = FALSE)
  
  apl_brg <- brg %>% mean_distance(directed = FALSE)
  mean_clustering_brg <- brg %>% 
    transitivity(type = "local",
                 isolates = "zero") %>% 
    mean()
  
  
  max_degree <- degree_v %>% max() 
  which(degree_v == max_degree) 
  
  min_degree <- degree_v %>% min() 
  which(degree_v == min_degree) 
  
  # betweenness
  bet <- betweenness(igraph_obj, 
                     v=V(igraph_obj), 
                     directed = FALSE)
  
  min_bet <- bet %>% min() 
  bet[which(bet == min_bet)] 
  
  max_bet <- bet %>% max()
  bet[which(bet == max_bet)] 
  
  
  graph_char <- data.frame("metric" = c("av. degree", 
                                        "density", 
                                        "av. SPL", 
                                        "global clust.", 
                                        "av. local clust.", 
                                        "av. betw.", 
                                        "s.d. betw.", 
                                        "BRG av. SPL", 
                                        "BRG av. local clust."), 
                           "values" = c(av_degree, 
                                        density, 
                                        apl, 
                                        global_clust, 
                                        mean_local_clust, 
                                        mean(bet), 
                                        sd(bet),
                                        apl_brg, 
                                        mean_clustering_brg)) 
  colnames(graph_char) <- c("metric", network_name)
  return(graph_char)
}
