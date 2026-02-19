library(tidyverse)
library(GNAR)
library(igraph)
# GNAR mobility models restricted to flu season ---------------------------

#restrict flu data to 2018-2019 season
mobility_df_list <- readRDS(file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/mobility_df_list.rds")
flu_ts_df_restricted <- county_flu_ac_season_norm |> filter(season=="2018-2019", county_fips %in% V(mobility_igraph_list[[10]])$name) |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt")  
flu_ts_restricted <- as.matrix(flu_ts_df_restricted)
#full country model, need mobility_missingness script!
mobility_GNAR_restricted_fit <- GNARfit(vts=flu_ts_restricted, net=mobility_GNAR)
summary(mobility_GNAR_restricted_fit)
# drop counties under 10K and season restrict
flu_ts_df_10k_restricted <- county_flu_ac_season_norm_10k |> filter(season=="2018-2019", county_fips %in% V(mobility_igraph_list[[10]])$name) |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt")
flu_ts_10k_restricted <- as.matrix(flu_ts_df_10k_restricted)
mobility_10k_restricted_GNAR_fit <- GNARfit(vts=flu_ts_10k_restricted, net = mobility_10k_GNAR)
summary(mobility_10k_restricted_GNAR_fit)
logLik(mobility_10k_restricted_GNAR_fit)

# New England population-restricted season model --------------------------
flu_ts_df_NEng_10k_restricted <- county_flu_ac_season_norm_10k |> filter(season=="2018-2019", county_fips %in% NEng_counties) |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt")
flu_ts_NEng_10k_restricted <- as.matrix(flu_ts_df_NEng_10k_restricted)
mobility_df_list_NEng_10k <- mobility_df_list_10k |> lapply(function(X){X|> select(origin, destination) |> filter(origin %in% NEng_counties, destination %in% NEng_counties) })
mobility_igraph_list_NEng_10k <- mobility_df_list_NEng_10k |> lapply(function(X){X |> select(origin, destination) |> graph_from_data_frame()|> igraph::simplify()})
mobility_igraph_list_NEng_10k |> lapply(gorder) |> unlist() |> which.max()
mobility_igraph_list_NEng_10k |> lapply(gsize) |> unlist() |> which.max()
#11 
mobility_GNAR_NEng_10k_restricted <- igraphtoGNAR(mobility_igraph_list_NEng_10k[[11]])
mobility_GNAR_NEng_10k_restricted_fit <- GNARfit(vts=flu_ts_NEng_10k_restricted, net=mobility_GNAR_NEng_10k_restricted)
summary(mobility_GNAR_NEng_10k_restricted_fit)
# Diagnose issues with fit by removing states and using GNAR plots 
cross_correlation_plot(2, vts=flu_ts_NEng_10k_restricted)
active_node_plot(vts=flu_ts_NEng_10k_restricted, network=mobility_GNAR_NEng_10k_restricted, max_lag = 2, r_stages=c(1,1))
local_relevance_plot(network=mobility_GNAR_NEng_10k_restricted, r_star=2)
node_relevance_plot(network=mobility_GNAR_NEng_10k_restricted, r_star=2, node_names = V(mobility_igraph_list_NEng_10k[[11]])$name)
#investigate county 23021, Piscataquis County, Maine remove counties below 20k
# NEng_counties_20k <- NEng_counties[NEng_counties!="23021"]
NEng_counties_20k<- county_pop_2024 %>% filter_at(vars(contains("20")), all_vars(.>20000)) |> select(FIPS) |> filter(FIPS %in% NEng_counties)  
flu_ts_df_NEng_20k_restricted <- county_flu_ac_season_norm |> filter(county_fips %in% NEng_counties_20k$FIPS, season=="2018-2019") |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt")
flu_ts_NEng_20k_restricted <- as.matrix(flu_ts_df_NEng_20k_restricted)
mobility_df_list_NEng_20k <- mobility_df_list_NEng_10k |> lapply(function(X){X|> select(origin,destination) |> filter(origin %in% NEng_counties_20k$FIPS, destination %in% NEng_counties_20k$FIPS)})
mobility_igraph_list_NEng_20k <- mobility_df_list_NEng_20k |> lapply(function(X){X|> select(origin, destination) |> graph_from_data_frame()|> igraph::simplify()})
mobility_igraph_list_NEng_20k |> lapply(gorder) |> unlist()  
mobility_igraph_list_NEng_20k |> lapply(gsize) |> unlist() |> which.max()
#11
mobility_GNAR_NEng_20k_restricted <- igraphtoGNAR(mobility_igraph_list_NEng_20k[[11]])

cross_correlation_plot(2, vts=flu_ts_NEng_20k_restricted)
active_node_plot(vts=flu_ts_NEng_20k_restricted, network=mobility_GNAR_NEng_20k_restricted, max_lag = 2, r_stages=c(1,1))
local_relevance_plot(network=mobility_GNAR_NEng_20k_restricted, r_star=2)
node_relevance_plot(network=mobility_GNAR_NEng_20k_restricted, r_star=1, node_names = V(mobility_igraph_list_NEng_20k[[11]])$name)
mobility_GNAR_NEng_20k_restricted_fit <- GNARfit(vts=flu_ts_NEng_20k_restricted, net = mobility_GNAR_NEng_20k_restricted)
summary(mobility_GNAR_NEng_20k_restricted_fit)
