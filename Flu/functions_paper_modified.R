

library(igraph)
library(GNAR)
library(geosphere)
source("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Flu/flu_data_preprocessing.R")

# Functions to create universal FIPS vector for use in other functions ---------------
#Convert State USPS abbreviation to FIPS prefix
state_fips_lookup <- function(usps){
  if(!is.null(usps))
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
  statefp <- fips_lookup[usps]
  
  return(statefp)
}
# create universal FIPS vector 
make_fips_vec <- function(fips_query, US_county_shape){
  fips_query <- as.character(fips_query)
  #All available FIPS codes
  # all_fips<- US_county_shape$GEOID
fips_vec <- c()
# if(fips_query=="all"){fips_vec<- all_fips}
for (i in fips_query){
  if (grepl("^[A-Za-z]{2}$", i)) {
    # USPS abbreviation
    i <- toupper(i)
    state_prefix <- state_fips_lookup(i) |> unname()
    fips <- US_county_shape$GEOID[US_county_shape$STATEFP==state_prefix]
    fips_vec <- c(fips_vec,fips)  
  } else if (grepl("^[0-9]{1,2}$", i)) {
      #State FIPS Prefix
    state_prefix <- sprintf("%02s", i)
    fips <- US_county_shape$GEOID[US_county_shape$STATEFP==state_prefix]
    fips_vec <- c(fips_vec, fips)
  } else if (grepl("^[0-9]{4,5}$", i)){
    #County FIPS code
    county_fips <- sprintf("%05s",i)
    if (county_fips %in% US_county_shape$GEOID){
      fips_vec <- c(fips_vec, county_fips)
    } else {
      warning(paste("Invalid input format:", i))
      next
    }
  }
}
return(unique(fips_vec))
}

# Accessory functions for network construction ----------------------------


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

#Subset county shape object
create_county_shape <- function(fips_vec, sub_name){
  if(!is.null(fips_vec))
  if(!is.null(US_county_shape)){
  county_shape<-US_county_shape%>% subset(.,GEOID %in% fips_vec)
  }
return(county_shape)}
# Create cent_coord object for subset from county shape object
create_cent_coord <- function(county_shape){
  cent_coord<-  county_shape %>%
    st_geometry() %>%
    st_centroid() %>%
    st_coordinates()
rownames(cent_coord) <- county_shape$GEOID
return(cent_coord)}


# Great circle distance matrix for Inverse distance weighting

# data should be a cent_coord object
circle_distance <- function(data) { 
  pairwise_dist <- matrix(nrow = nrow(data), ncol = nrow(data))
  for (i in seq(1, dim(data)[1])) {
    for (j in seq(1, dim(data)[1])) {
      long1 <- data[i, 1]
      long2 <- data[j, 1]
      lat1 <- data[i, 2]
      lat2 <- data[j, 2]
      pairwise_dist[i, j] <- distm(x = c(long1, lat1), 
                                   y = c(long2, lat2), 
                                   fun = distHaversine)
    }
  }
  # transform into data frame
  dist_df <- pairwise_dist %>% as.data.frame()
  rownames(dist_df) <- rownames(data)
  colnames(dist_df) <- rownames(data)

  # substitute zero diagonals with NA
  dist_df[dist_df == 0] <- NA
   # return(pairwise_dist)
  return(dist_df)
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

# Time series functions -------------------------------------

create_ts <- function(df = county_flu_ac_season_norm, county_shape, forMoran=FALSE, asMatrix=TRUE) {
  #need to source preprocessing file first!
  # take normalized flu data frame "df", and county shape object
  # create overall time series data frame object
  if(forMoran){
    ts_moran <- df %>% select(county_fips, year_week_dt, conf_flu_norm) %>% filter(year(year_week_dt) <2020 & county_fips %in% county_shape$GEOID) 
  return(ts_moran)
    }
  ts_df<- df %>% select(county_fips, year_week_dt, conf_flu_norm) %>% filter(year(year_week_dt) <2020 & county_fips %in% county_shape$GEOID) %>% spread(county_fips, conf_flu_norm) %>% column_to_rownames(var =
                                                                                                                                           "year_week_dt") 
  #convert to matrix
  if(asMatrix){
  ts <-as.matrix(ts_df)  
  return(list(ts_df,ts))}
  else{
    return(ts_df)
  }
  }
  
  

# Moran's I permutation test 
moran_I_permutation_test <- function(data = county_flu_ac_season_norm,
                                     g, 
                                     county_index = NULL, 
                                     name, 
                                     time_col = "year_week_dt", 
                                     cases_col = "conf_flu_norm") {
  
  # compute shortest path length for each vertex pair
  distMatrix <- exp(shortest.paths(g, v=V(g), to=V(g)) * (-1))
  
  # if igraph does not have county names for vertices 
  if (!is.null(county_index)) {
    # assign names 
    county_ordering <- match(seq(1, nrow(county_index)), 
                             county_index$index)
    county_names <- county_index[county_ordering, ]$GEOID
    
    rownames(distMatrix) <- county_names
    colnames(distMatrix) <- county_names
  }
  # assign county names
  county_distMatrix <- distMatrix %>% rownames()
  
  
  # loop through all dates
  dates <- data[[time_col]] %>% 
    unique() %>% 
    as.character()
  
  moran_list <- list()
  # for each date, compute Moran's I
  for (date in dates[-1]) {
    cases_date <- data[data[[time_col]] == date, ]
    county_df <- cases_date$county_fips
    ordering <- match(county_distMatrix, county_df)
    
    number_cases_date <- cases_date[ordering, ][[cases_col]]
    
    morans_I_values <- array(NA, dim = 100)
    for (r in seq(1, 100)) {
      set.seed(r)
      # permutate case numbers 
      permutate <- sample(seq(1, nrow(county_index)), 
                          size = nrow(county_index), 
                          replace = FALSE) 
      
      # compute Moran's I for permutated cases 
      morans_I_values[r] <- ape::Moran.I(number_cases_date[permutate], 
                                         distMatrix, 
                                         scaled = FALSE, 
                                         na.rm = FALSE,
                                         alternative = "two.sided")$observed
    }
    
    quantile_morans_I <- quantile(morans_I_values, 
                                  probs = c(0.025, 0.5, 0.975)) %>% 
      unname()
    
    orig_result <- ape::Moran.I(number_cases_date, 
                                distMatrix, 
                                scaled = FALSE, 
                                na.rm = FALSE,
                                alternative = "two.sided") %>% 
      rlist::list.cbind()
    
    moran_list[[date]] <- cbind(orig_result, 
                                "lower_ci" = quantile_morans_I[1], 
                                "upper_ci" = quantile_morans_I[3], 
                                "median" = quantile_morans_I[2])
  }
  
  
  moran_df <- moran_list %>% rlist::list.rbind() %>% as.data.frame()
  moran_df$dates <- dates[-1] %>% as.Date()
  
  # p-value 
  morans_p <- moran_df %>% 
    mutate(p = ifelse(observed > upper_ci | observed < lower_ci, 1, 0)) %>% 
    pull(p) %>% 
    mean()
  
  morans_number_outside <- moran_df %>% 
    mutate(p = ifelse(observed > upper_ci | observed < lower_ci, 1, 0)) %>% 
    pull(p) %>% 
    sum()
  
  
  # Visualize and save plot 
  ggplot(moran_df, 
         aes(x = dates, 
             y = observed)) +
    geom_line() +
    xlab("Time") +
    ylab("Moran's I") +
    geom_line(aes(x = dates, 
                  y = lower_ci), 
              linetype = "dashed", color = "#3A3B3C") +
    geom_line(aes(x = dates, 
                  y = upper_ci), 
              linetype = "dashed", color = "#3A3B3C") +
    geom_line(aes(x = dates, 
                  y = median), 
              linetype = "dashed", color = "#3A3B3C") +
    scale_color_brewer(palette = "Set1") + 
    theme(legend.position = "None")
  ggsave(paste0("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Figures/MoransI/Flu/", name, ".pdf", collapse = ""), 
         width = 27, height = 14, unit = "cm")
  return(list(p = morans_p, 
              number = morans_number_outside))
}



# GNAR Network object construction ----------------------------------------


#Function to create KNN GNAR objects  
create_KNN_objects <- function(cent_coord, min_k=2, max_k=(nrow(cent_coord)-1), iter.k=1, keep.igraph=TRUE){
knn_GNAR_list <- list()
max_SPL_knn_list <- list()
if(keep.igraph){
knn_igraph_list <- list()}
for(k in seq(min_k, max_k, by=iter.k)){
#knn neighborhood object
  nb_knn <- knearneigh(x=cent_coord,
                       k=k,
                       longlat=T) %>%
    knn2nb(row.names=row.names(cent_coord))
    
#igraph object from adjacency matrix
knn_igraph <- neighborsDataFrame(nb=nb_knn) %>%
  igraph::graph_from_data_frame(directed=FALSE) %>%
  igraph::simplify()
if(keep.igraph){
  knn_igraph_list[[length(knn_igraph_list)+1]] <- knn_igraph
}
# create GNAR object  
knn_GNAR <- GNAR::igraphtoGNAR(knn_igraph)
knn_GNAR_list[[length(knn_GNAR_list)+1]] <- knn_GNAR
#create ordered county index data frame

county_index_knn <- data.frame("GEOID" = knn_igraph %>%
                                 V() %>%
                                 names(),
                               "index" = seq(1, nrow(cent_coord)))

#compute neighborhood stage upper limit

max_SPL_knn <- knn_igraph %>%
  get_diameter(directed = FALSE) %>%
  length()
max_SPL_knn_list[[length(max_SPL_knn_list)+1]] <- max_SPL_knn
}
if(keep.igraph){
  return(list(county_index_knn, max_SPL_knn_list, knn_igraph_list, knn_GNAR_list))
}
else return(list(county_index_knn, max_SPL_knn_list, knn_GNAR_list))
}    


# #GNAR fitting and prediction functions. Modified from Armbruster --------


    
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
    if (!old) {
      model <- GNARfit_weighting(vts = vts[1:train_window, ],
                                 net = net,
                                 alphaOrder = alpha,
                                 betaOrder = beta,
                                 globalalpha = globalalpha,
                                 inverse_distance = inverse_distance,
                                 county_index = county_index
      )
    }
    if (old) {
      model <- GNARfit(vts = vts[1:train_window, ], 
                       net = net,
                       alphaOrder = alpha, 
                       betaOrder = beta, 
                       globalalpha = globalalpha
      )
    }
    
  } else {
    if (!old) {
      model <- GNARfit_weighting(vts = vts[1:train_window, ],
                                 net = net,
                                 alphaOrder = alpha,
                                 betaOrder = beta,
                                 globalalpha = globalalpha,
                                 fact.var = weight_factor,
                                 inverse_distance = inverse_distance,
                                 county_index = county_index
      )
    }
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



# GNAR Model Diagnostics --------------------------------------------------


