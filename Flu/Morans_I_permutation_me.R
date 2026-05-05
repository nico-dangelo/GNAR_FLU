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
  ggsave(paste0("Figures/MoransI/covid_moran_", name, ".pdf", collapse = ""), 
         width = 27, height = 14, unit = "cm")
  return(list(p = morans_p, 
              number = morans_number_outside))
}
