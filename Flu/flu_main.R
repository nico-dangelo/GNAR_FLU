# load libraries ----------------------------------------------------------
library(magrittr)
library(tidyverse)
# library(lubridate)
library(spiralize)
# library(ggplot2)
library(GNAR)
library(igraph)
library(spdep)
library(sf)
library(sp)
library(mcstatsim)
library(modelsummary)
# setwd("G:/My Drive/Lab Files/GNAR_FLU")
# Read data ---------------------------------------------------------------
county_week_flu_v3_imputed <- readr::read_csv("Data/Flu/county_week_flu_v3_imputed.csv")

# Clean data and fix formats -----------------------------------------------
county_week_flu_v3_imputed_clean <- county_week_flu_v3_imputed
county_week_flu_v3_imputed_clean$year_week_dt <- as_date(county_week_flu_v3_imputed$year_week_dt, format="%G-%V-%u")


# # Subset examples----------------------------------------
# #Palm Beach County 
# county_week_flu_v3_imputed_clean_PBC <- county_week_flu_v3_imputed_clean %>% filter(county_fips==12099)
# #New York County
# county_week_flu_v3_imputed_clean_NYC <- county_week_flu_v3_imputed_clean %>% filter(county_fips==36061)
# # Suffolk County MA
# county_week_flu_v3_imputed_clean_SFK <- county_week_flu_v3_imputed_clean %>% filter(county_fips==25025) 
# 
# # Middlesex county MA
# 
# county_week_flu_v3_imputed_clean_MDX <- county_week_flu_v3_imputed_clean %>% filter(county_fips==25017)
# 
# # Visualize example data on spirals --------------------------------------------------
# 
# # range(county_week_flu_v3_imputed_clean_PBC$conf_flu)
# #PBC
# # spiral_initialize_by_time(xlim=range(county_week_flu_v3_imputed_clean_PBC$year_week_dt), unit_on_axis = "weeks", normalize_year = T)
# # spiral_track(height =  0.8, ylim = c(0, 1.5e3))
# # spiral_lines(county_week_flu_v3_imputed_clean_PBC$year_week_dt, county_week_flu_v3_imputed_clean_PBC$conf_flu, type="h", gp = gpar(fill = 2, col = 2))
# # spiral_points(county_week_flu_v3_imputed_clean_PBC$year_week_dt, county_week_flu_v3_imputed_clean_PBC$conf_flu,pch = 16, gp = gpar(col = 2))
# # spiral_yaxis(at = c(0, 100, 500, 1000, 1.5e3), labels = c("0", "100", "500", "1000", "1500"), 
# #              labels_gp = gpar(fontsize = 7))
# # spiral_PBC <- spiralize::current_spiral()
# # 
# # #NYC
# # spiral_initialize_by_time(xlim=range(county_week_flu_v3_imputed_clean_NYC$year_week_dt), unit_on_axis = "weeks", normalize_year = T)
# # spiral_track(height =  0.8, ylim = c(0, 1.6e3))
# # spiral_bars(county_week_flu_v3_imputed_clean_NYC$year_week_dt, county_week_flu_v3_imputed_clean_NYC$conf_flu, gp = gpar(fill = 2, col = 2))
# # spiral_yaxis(at = c(0, 100, 500, 1000, 1.6e3), labels = c("0", "100", "500", "1000", "1600"), 
# #              labels_gp = gpar(fontsize = 7))
# # 
# # spiral_NYC <- current_spiral()
# # 
# # plot(county_week_flu_v3_imputed_clean_NYC$conf_flu)
# 
# 
# # Visualize on overlaid linear plots  -------------------------------------
# 
# plot_PBC_NYC_SFK_MDX <- county_week_flu_v3_imputed_clean %>% filter(county_fips %in% c(12099, 36061, 25025, 25017), year(year_week_dt)<2020) %>% ggplot(aes(x =
#                                                                                                                                                               year_week_dt, y = conf_flu, color = county_fips)) + geom_point() + geom_line() + xlab("Date (year_week_dt)") +ylab("Confirmed flu case counts")
# 
# plot_PBC_NYC_SFK_MDX
# 
# 
# plot_Miami_Cook_Fulton <-  county_week_flu_v3_imputed_clean %>% filter(county_fips %in% c(12086,17031,13121), year(year_week_dt)<2020) %>% ggplot(aes(x =
#                                                                                                                                                         year_week_dt, y = conf_flu, color = county_fips)) + geom_point() + geom_line() + xlab("Date (year_week_dt)") +ylab("Confirmed flu case counts")
# plot_Miami_Cook_Fulton
# 
# # Normalization -----------------------------------------------------------
# #Import all cause mortality data
# 
county_week_ac_v3_imputed <- readr::read_csv("Data/Flu/county_week_ac_v3_imputed.csv")

# Normalize flu data by ac

# extract year from year_week
county_week_ac_v3_imputed_year <-  county_week_ac_v3_imputed %>% mutate(year=sub("-.*","",year_week))

#compute year-level mean ac
county_year_ac_mean <- county_week_ac_v3_imputed_year %>% group_by(county_fips, year) %>% mutate(year_ac_mean=mean(all_cause_wtd))

# Normalize county-week ac by yearly means

county_week_ac_norm <-  county_year_ac_mean %>% group_by(county_fips, year) %>% mutate(week_ac_norm=all_cause_wtd/year_ac_mean)



#merge ac with flu data by county and year_week

county_week_flu_ac_merged <- merge.data.frame(county_week_flu_v3_imputed_clean, county_week_ac_norm)

# adjust conf_flu by normalized county-week ac

county_week_flu_ac_merged_adj<- county_week_flu_ac_merged  %>% mutate(conf_flu_adj=conf_flu/week_ac_norm)

# import seasonal file
county_season_ac_v3_imputed <- readr::read_csv("Data/Flu/county_season_ac_v3_imputed.csv") %>% rename(season_all_cause=all_cause, season_all_cause_wtd=all_cause_wtd)


# Note: season = July to June; partition weekly data with season flag

county_flu_ac_season <- county_week_flu_ac_merged_adj %>% mutate(season=ifelse(month(year_week_dt) >= 7,
                                                                               paste0(year(year_week_dt), "-", year(year_week_dt) + 1),     # e.g., "2023-2024"
                                                                               paste0(year(year_week_dt) - 1, "-", year(year_week_dt))))

county_flu_ac_season_merged <- merge.data.frame(county_flu_ac_season, county_season_ac_v3_imputed)

# normalize by season_all_cause

county_flu_ac_season_norm <- county_flu_ac_season_merged %>% mutate(conf_flu_norm= conf_flu_adj/season_all_cause_wtd)


# # plot normalized data
# 
# plot_PBC_NYC_SFK_MDX_norm <- county_flu_ac_season_norm %>% filter(county_fips %in% c(12099, 36061, 25025, 25017), year(year_week_dt)<2020) %>% ggplot(aes(x =
#                                                                                                                                                             year_week_dt, y = conf_flu_norm, color = county_fips)) + geom_point() + geom_line() + xlab("Date (year_week_dt)") +ylab("Normalized Confirmed flu case counts")
# 
# plot_PBC_NYC_SFK_MDX_norm
# 
# plot_Miami_Cook_Fulton_norm <-  county_flu_ac_season_norm %>% filter(county_fips %in% c(12086,17031,13121), year(year_week_dt)<2020) %>% ggplot(aes(x =
#                                                                                                                                                       year_week_dt, y = conf_flu_norm, color = county_fips)) + geom_point() + geom_line() + xlab("Date (year_week_dt)") +ylab("Normalized Confirmed flu case counts")
# plot_Miami_Cook_Fulton_norm
# 
# #DC
# 
# months<- month(county_flu_ac_season_norm$year_week_dt)
# county_flu_ac_season_norm %>% filter(county_fips==11001) %>% ggplot(aes(x = week(year_week_dt), y = conf_flu_norm, color = county_fips)) + geom_point() + geom_line() + xlab("Date (year_week_dt)") +ylab("Normalized Confirmed flu case counts") +geom_vline(xintercept = month(year_week_dt))


# Prepare Data objects for GNAR -------------------------------------------

#Proceed with unadjusted data for now (9/11/25)

#Using normalized data as of 9-24-2025

# Use county neighbors shapefile


#import county_neighbors

# county_neighbors<- readr::read_csv("Data/Flu/county_neighbors.csv")
# 
# #only keep counties present in flu data
# common_counties <- intersect(county_flu_ac_season_norm$county_fips, county_neighbors$county)
# 
# county_neighbors_clean <- county_neighbors %>% filter(county %in% common_counties) %>% filter (neighbor %in% common_counties)
# 
# flu_norm_common <- county_flu_ac_season_norm %>% filter( county_fips %in% common_counties) 
# # Initial network
# 
# county_neighbors_net <- county_neighbors_clean %>% select(c("county", "neighbor")) %>% as.matrix() %>% igraph::graph_from_edgelist()
# 
# county_neighbors_GNAR <- GNAR::igraphtoGNAR(county_neighbors_net)

# fit GNAR to flu data series -- sensitive to counts? 

# Counts not required
#keep only necessary variables and reshape to matrix needed by GNAR

# Pre-COVID only for now 

#Check that some nodes/counties do not have data in flu/ac set!!!!

# flu_norm_ts_df <- flu_norm_common %>% select(county_fips, year_week_dt, conf_flu_norm) %>% filter(year(year_week_dt)<2020) %>% spread(county_fips,conf_flu_norm) %>% column_to_rownames(var="year_week_dt") 

# flu_norm_ts <-as.matrix(flu_norm_ts_df)
# flu_norm_GNAR <- GNARfit(vts=flu_norm_ts, net=county_neighbors_GNAR, alphaOrder = 2, betaOrder = c(1,1))





# State-level submodels ---------------------------------------------------


# Fit state submodels for more tractable inference -- start with CA

# California
# 
# flu_norm_ts_CA <- flu_norm_ts_df %>% select(starts_with("06")) %>% as.matrix()
# county_neighbors_net_CA <-subgraph(county_neighbors_net,colnames(flu_norm_ts_CA))
# county_neighbors_CA_GNAR <- igraphtoGNAR(county_neighbors_net_CA)
# flu_norm_CA_GNAR <- GNARfit(vts=flu_norm_ts_CA, net=county_neighbors_CA_GNAR,  alphaOrder = 2, betaOrder = c(1,1))
# # BIC cannot be computed 
# # Massachusetts
# 
# flu_norm_ts_MA <- flu_norm_ts_df %>% select(starts_with("25")) %>% as.matrix()
# county_neighbors_net_MA <- subgraph(county_neighbors_net, colnames(flu_norm_ts_MA))
# county_neighbors_MA_GNAR <- igraphtoGNAR(county_neighbors_net_MA)
# flu_norm_MA_GNAR <- GNARfit(vts=flu_norm_ts_MA, net=county_neighbors_MA_GNAR, alphaOrder = 2, betaOrder = c(1,1))
# 
# 
# 
# # Vary alpha parameters -------------------------------
# 
# flu_norm_MA_GNAR_alpha_1 <- GNARfit(vts=flu_norm_ts_MA, net=county_neighbors_MA_GNAR, alphaOrder = 1, betaOrder = 1 )
# View(GNAR:::BIC.GNARfit)
# 



# KNN network -------------------------------------------------------------


# Import and Process Shapefile --------------------------------------------

# County-level shapefile 
US_county_shape<- st_read("Shapefiles/cb_2020_us_county_5m/cb_2020_us_county_5m.shp")

# Check that all counties are included in flu data
#check names
exclude_names <- setdiff(US_county_shape$NAMELSAD, county_flu_ac_season_norm$county_name)
#check fips
exclude_fips <- setdiff(US_county_shape$GEOID, county_flu_ac_season_norm$county_fips)

include_fips <- intersect(US_county_shape$GEOID, county_flu_ac_season_norm$county_fips)




# Construct centroid coordinates


# cent_coord <- US_county_shape%>% subset(.,GEOID %in% include_fips) %>%
#   st_geometry() %>%
#   st_centroid() %>%
#   st_coordinates()
# names(cent_coord) <- US_county_shape$GEOID



# intersect(county_flu_ac_season_norm, names(cent_coord))


# Functions for KNN -------------------------------------------------------



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

# #Find optimal k for KNN GNAR models

# Original code from IR
# knn_best <- list()
# 
# for (k in seq(1, 26, by = 2)) {
#   # create nb list
#   nb_knn <- knearneigh(x = coord_urbanisation,
#                        k = k,
#                        longlat = TRUE) %>% 
#     knn2nb(row.names = coord_urbanisation %>% row.names())
#   
#   # Create igraph from adjacency matrix
#   covid_net_knn_igraph <- neighborsDataFrame(nb = nb_knn) %>% 
#     graph_from_data_frame(directed = FALSE) %>% 
#     igraph::simplify() 
#   
#   # create GNAR object 
#   covid_net_knn <- covid_net_knn_igraph %>% 
#     igraphtoGNAR()
#   
#   # create ordered county index data frame 
#   county_index_knn <- data.frame("CountyName" = covid_net_knn_igraph %>%
#                                    V() %>% 
#                                    names(), 
#                                  "index" = seq(1, 26))
#   
#   # compute an upper limit for neighbourhood stage 
#   max_SPL_knn <- covid_net_knn_igraph %>% 
#     get_diameter(directed = FALSE) %>% 
#     length()
#   
#   # fit GNAR models and select the best performing one for each data subset 
#   res <- fit_and_predict_for_restrictions(net = covid_net_knn, 
#                                           upper_limit = max_SPL_knn - 1, 
#                                           data_list = datasets_list_coarse)
#   
#   res$hyperparam <- k
#   
#   # save best performing model for every k across all data subsets  
#   knn_best[[length(knn_best) + 1]] <- res
#   
# }


#



#test knn for GNAR workflow
# knn_2<- knearneigh(x = cent_coord,
#                    k = 2,
#                    longlat = TRUE) %>% 
#   knn2nb(row.names = cent_coord %>% row.names())
# 
# knn_2_igraph <- neighborsDataFrame(knn_2) %>% 
#   igraph::graph_from_data_frame(directed = FALSE) %>% 
#   igraph::simplify() 
# 
# knn2_GNAR <- knn_2_igraph%>% igraphtoGNAR()

# upper limit on neighborhood stage

# max_SPL_knn_2 <- knn_2_igraph %>% get_diameter(directed = FALSE) %>% 
  # length()

#TS for GNARs
flu_norm_ts_df <-county_flu_ac_season_norm %>% select(county_fips, year_week_dt, conf_flu_norm) %>% filter(year(year_week_dt)<2020) %>% spread(county_fips,conf_flu_norm) %>% column_to_rownames(var="year_week_dt") 
flu_norm_ts <- as.matrix(flu_norm_ts_df)


#   Massachusetts state submodel ------------------------------------------

#Plot normalized Massachusetts series
MA_county_shape <- US_county_shape%>% subset(.,GEOID %in% include_fips & STUSPS=="MA")
cent_coord_MA <-  MA_county_shape %>%
  st_geometry() %>%
  st_centroid() %>%
  st_coordinates()
rownames(cent_coord_MA) <- MA_county_shape$GEOID


county_flu_norm_plot_MA <- county_flu_ac_season_norm %>% filter(county_fips %in% MA_county_shape$GEOID & year(year_week_dt)<2020) %>% ggplot(aes(x =year_week_dt, y = conf_flu_norm, color = county_fips)) + geom_point() + geom_line() + xlab("Date (year_week_dt)") +ylab("Normalized Confirmed flu case counts")
flu_norm_ts_df_MA <- flu_norm_ts_df %>% select(starts_with("25"))

flu_norm_ts_MA <- as.matrix(flu_norm_ts_df_MA)

# res <- GNARfit(vts=flu_norm_ts,net=knn2_GNAR)

#acfs by county for MA

# acf(flu_norm_ts_MA)


# KNN for MA --------------------------------------------------------------

knn_best_MA <- list()
corbit_plots <- list()

for (k in seq(3, 13, by = 1)) {
  # create nb list
  nb_knn <- knearneigh(x = cent_coord_MA,
                       k = k,
                       longlat = TRUE) %>%
    knn2nb(row.names = cent_coord_MA %>% row.names())

  # Create igraph from adjacency matrix
  flu_net_knn_igraph <- neighborsDataFrame(nb = nb_knn) %>%
    graph_from_data_frame(directed = FALSE) %>%
    igraph::simplify()

  # create GNAR object
  flu_net_knn <- flu_net_knn_igraph %>% igraphtoGNAR()

  # create ordered county index data frame
  county_index_knn <- data.frame("GEOID" = flu_net_knn_igraph %>%
                                   V() %>%
                                   names(),
                                 "index" = seq(1, 14))

  # compute an upper limit for neighbourhood stage
  max_SPL_knn <- flu_net_knn_igraph %>%
    get_diameter(directed = FALSE) %>%
    length()

  #Network autocorrelation to choose alpha
  
weight_matrix_knn <- weights_matrix(flu_net_knn, max_r_stage = max_SPL_knn)
corbit_plots[[length(knn_best_MA) + 1]] <- corbit_plot(vts=flu_norm_ts_MA, max_stage = max_SPL_knn, net=flu_net_knn, max_lag = 10, weight_matrix = weight_matrix_knn)

# corbit_plots[[k]] <-  recordPlot()
  # Network partial autocorrelation
   # corbit_plot(vts=flu_norm_ts_MA, max_stage = max_SPL_knn, net=flu_net_knn, max_lag = 10, weight_matrix = weight_matrix_knn, partial = T)

# corbit_plots[[k]] <-  recordPlot()
  
  }
source("Flu/functions_paper_modified.R")
#set k>2 to avoid subgraph disjointness
for(k in seq(3, 13, by = 1)){
  # fit GNAR models and select the best performing one
  res <- fit_and_predict_for_many(alpha_options = seq(1,7), net = flu_net_knn,
                                           upper_limit = max_SPL_knn - 1,
                                          vts = flu_norm_ts_MA, old = T, forecast_window = 25, globalalpha = FALSE)

   res$hyperparam <- k

  # save best performing model for every k across all data subsets
  knn_best_MA[[length(knn_best_MA) + 1]] <- res

}
# fit_and_predict(net = flu_net_knn,
                # upper_limit = max_SPL_knn - 1,
                # vts = flu_norm_ts_MA, old = T, forecast_window = 5)


# Diagnostics for MA models -----------------------------------------------
# Run ARIMA benchmarks to establish autocorrelation



# Find best knn network model based on BIC

# filter the best performing GNAR model for each data subset across all neighbourhood sizes
knn_best_MA_df <- do.call(rbind.data.frame, knn_best_MA) %>%
  filter(BIC == min(BIC)) %>%
  ungroup() %>%
  as.data.frame()
knn_best_MA_df$network <-  "KNN"

# Local relevance, node relevance, and cross-correlation plots -- should reflect clusters
#Clustering and network stats
# for(i in 1:13){
# network_characteristics(flu_net_knn_igraph[[i]], network_name = "KNN")
# }
# cross-correlation

cross_correlation_plot(10, vts=flu_norm_ts_MA)
#node relevance
node_relevance_plot(flu_net_knn, r_star=2, node_names = colnames(flu_norm_ts_df_MA))
#local relevance plot
local_relevance_plot(network=flu_net_knn, r_star = 2)
# active neigborhood plot
# active_node_plot(vts=flu_norm_ts_MA, flu_net_knn,)

# Wagner plot for time dependence of alpha and beta

# Annual Season Frames

vts_season_frames_MA <- county_flu_ac_season_norm %>% select(county_fips, year_week_dt, conf_flu_norm, season) %>% filter(year(year_week_dt)<2020) %>% spread(county_fips,conf_flu_norm) %>% column_to_rownames(var="year_week_dt") %>% split(f=as.factor(.$season)) 
# %>% map(., ~ (.x %>% select(-season)))

# wagner_plot(vts_frames = vts_season_frames_MA, network_list = list(flu_net_knn),  same_net = "no", 10, 3,weight_matrices = list(weight_matrix_knn))


# Submodel for FL ---------------------------------------------------------

# flu_norm_ts_df_FL <- flu_norm_ts_df %>% select(starts_with("12") & !c("12087"))
# 
# flu_norm_ts_FL <- as.matrix(flu_norm_ts_df_FL)
# 
# FL_county_shape <- US_county_shape %>% subset(.,GEOID %in% setdiff(include_fips, c("12087")) & STUSPS=="FL")
# cent_coord_FL <-  FL_county_shape %>%
#   st_geometry() %>%
#   st_centroid() %>%
#   st_coordinates()
# rownames(cent_coord_FL) <- FL_county_shape$GEOID
# 
# 
# knn_best_FL <- list()
# for (k in seq(5, 65, by = 2)) {
#   # create nb list
#   nb_knn <- knearneigh(x = cent_coord_FL,
#                        k = k,
#                        longlat = TRUE) %>%
#     knn2nb(row.names = cent_coord_FL %>% row.names())
#   
#   # Create igraph from adjacency matrix
#   flu_net_knn_igraph <- neighborsDataFrame(nb = nb_knn) %>%
#     graph_from_data_frame(directed = FALSE) %>%
#     igraph::simplify()
#   
#   # create GNAR object
#   flu_net_knn <- flu_net_knn_igraph %>% igraphtoGNAR()
#   
#   # create ordered county index data frame
#   county_index_knn <- data.frame("GEOID" = flu_net_knn_igraph %>%
#                                    V() %>%
#                                    names(),
#                                  "index" = seq(1, 66))
#   
#   # compute an upper limit for neighbourhood stage
#   max_SPL_knn <- flu_net_knn_igraph %>%
#     get_diameter(directed = FALSE) %>%
#     length()
# }
# 
# # visualize knn networks to probe issues with GNAR likelihood
# plot(st_geometry(FL_county_shape), border="grey")
# nb_knn_5 <-knearneigh(x = cent_coord_FL,
#                       k = 5,
#                       longlat = TRUE) %>%
#   knn2nb(row.names = cent_coord_FL %>% row.names())
# 
# plot(nb_knn_5, cent_coord_FL,pch = 19, cex = 0.6,
#           add=TRUE)
# text(cent_coord_FL[, 1],
#           cent_coord_FL[, 2],
#           labels = rownames(cent_coord_FL),
#           cex = 0.8, font = 2, pos = 1)
# 
# knn_5_igraph <- neighborsDataFrame(nb = nb_knn_5) %>%
#   graph_from_data_frame(directed = FALSE) %>%
#   igraph::simplify()
# 
# 
# for (k in seq(15, 66, by = 2)) {
#   # fit GNAR models and select the best performing one for each data subset
#   res_FL<- fit_and_predict_for_many(net = flu_net_knn,
#                                   upper_limit = max_SPL_knn - 1,
#                                   vts = flu_norm_ts_FL, old=T)
#   
#   res_FL$hyperparam <- k
#   
#   # save best performing model for every k across all data subsets
#   knn_best[[length(knn_best) + 1]] <- res
#   
# }
# 
