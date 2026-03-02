library(tidyverse)
library(GNAR)
library(igraph)
# library(modelsummary)
library(xtable)
# GNAR mobility models restricted to flu season ---------------------------

#restrict flu data to 2018-2019 season
mobility_df_list <- readRDS(file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/mobility_df_list.rds")
flu_ts_df_restricted <- county_flu_ac_season_norm |> filter(season=="2018-2019", county_fips %in% V(mobility_igraph_list[[10]])$name) |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt")  
flu_ts_restricted <- as.matrix(flu_ts_df_restricted)
county_flu_ac_season_norm_10k<- readRDS(file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Population/county_flu_ac_season_norm_10k.RDS")

mobility_df_list_10k <- mobility_df_list |> lapply(function(X){X|> select(origin, destination) |> filter(origin %in% county_flu_ac_season_norm_10k$county_fips, destination %in% county_flu_ac_season_norm_10k$county_fips)})

#full country model, need mobility_missingness script!
mobility_GNAR_restricted_fit <- GNARfit(vts=flu_ts_restricted, net=mobility_GNAR)
# summary(mobility_GNAR_restricted_fit)
# drop counties under 10K and season restrict
# flu_ts_df_10k_restricted <- county_flu_ac_season_norm_10k |> filter(season=="2018-2019", county_fips %in% V(mobility_igraph_list[[10]])$name) |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt")
# flu_ts_10k_restricted <- as.matrix(flu_ts_df_10k_restricted)
# mobility_10k_restricted_GNAR_fit <- GNARfit(vts=flu_ts_10k_restricted, net = mobility_10k_GNAR)
# summary(mobility_10k_restricted_GNAR_fit)
# logLik(mobility_10k_restricted_GNAR_fit)

# New England population-restricted season model --------------------------
county_flu_ac_season_norm_10k<- readRDS(file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Population/county_flu_ac_season_norm_10k.RDS")
county_pop_2024 <- read_csv("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Population/county_population_with_fips.csv")
NEng_counties <- make_fips_vec(c("MA", "RI", "CT", "VT", "NH", "ME"), county_shape = US_county_shape)
flu_ts_df_NEng_10k_restricted <- county_flu_ac_season_norm_10k |> filter(season=="2018-2019", county_fips %in% NEng_counties) |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt")
flu_ts_NEng_10k_restricted <- as.matrix(flu_ts_df_NEng_10k_restricted)
# mobility_df_list_NEng_10k <- mobility_df_list_10k |> lapply(function(X){X|> select(origin, destination) |> filter(origin %in% NEng_counties, destination %in% NEng_counties) })
# mobility_igraph_list_NEng_10k <- mobility_df_list_NEng_10k |> lapply(function(X){X |> select(origin, destination) |> graph_from_data_frame()|> igraph::simplify()})
# mobility_igraph_list_NEng_10k |> lapply(gorder) |> unlist() |> which.max()
# mobility_igraph_list_NEng_10k |> lapply(gsize) |> unlist() |> which.max()
#11 
# mobility_GNAR_NEng_10k_restricted <- igraphtoGNAR(mobility_igraph_list_NEng_10k[[11]])
mobility_max_NEng_10k_restricted_GNAR <- create_network_max_GNAR(edge_df_list = mobility_df_list_10k, target_counties = NEng_counties)
mobility_max_NEng_10k_restricted_GNAR_fit <- GNARfit(vts=mobility_max_NEng_10k_restricted_GNAR[[2]], net=mobility_max_NEng_10k_restricted_GNAR[[1]])
vcov(mobility_max_NEng_10k_restricted_GNAR_fit) |> corrplot::corrplot()
summary(mobility_max_NEng_10k_restricted_GNAR_fit)
# GNARfit_sandwich(vts=mobility_max_NEng_10k_restricted_GNAR[[2]], net=mobility_max_NEng_10k_restricted_GNAR[[1]])
# sandwich(mobility_max_NEng_10k_restricted_GNAR_fit)
# sandwich::vcovHAC(mobility_max_NEng_10k_restricted_GNAR_fit)
# Diagnose issues with fit by removing states and using GNAR plots 
cross_correlation_plot(2, vts=flu_ts_NEng_10k_restricted)
active_node_plot(vts=flu_ts_NEng_10k_restricted, network=mobility_GNAR_NEng_10k_restricted, max_lag = 2, r_stages=c(1,1))
local_relevance_plot(network=mobility_GNAR_NEng_10k_restricted, r_star=2)
node_relevance_plot(network=mobility_GNAR_NEng_10k_restricted, r_star=2, node_names = V(mobility_igraph_list_NEng_10k[[11]])$name)
#investigate county 23021, Piscataquis County, Maine remove counties below 20k
# NEng_counties_20k <- NEng_counties[NEng_counties!="23021"]
NEng_counties_20k<- county_pop_2024 %>% filter(`2020`>20000) |> select(FIPS, County) |> filter(FIPS %in% NEng_counties | grepl("Connecticut", County))  |> pull()
flu_ts_df_NEng_20k_restricted <- county_flu_ac_season_norm |> filter(county_fips %in% NEng_counties_20k$FIPS, season=="2018-2019") |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt")
flu_ts_NEng_20k_restricted <- as.matrix(flu_ts_df_NEng_20k_restricted)
mobility_df_list_NEng_20k <- mobility_df_list_NEng_10k |> lapply(function(X){X|> select(origin,destination) |> filter(origin %in% NEng_counties_20k$FIPS, destination %in% NEng_counties_20k$FIPS)})
mobility_igraph_list_NEng_20k <- mobility_df_list_NEng_20k |> lapply(function(X){X|> select(origin, destination) |> graph_from_data_frame()|> igraph::simplify()})
mobility_igraph_list_NEng_20k |> lapply(gorder) |> unlist()  
mobility_igraph_list_NEng_20k |> lapply(gsize) |> unlist() |> which.max()
#11
mobility_GNAR_NEng_20k_restricted <- igraphtoGNAR(mobility_igraph_list_NEng_20k[[11]])
corbit_plot(net=mobility_GNAR_NEng_20k_restricted, max_lag=5, max_stage = diameter(mobility_igraph_list_NEng_20k[[11]]), rectangular_plot = "square", vts=flu_ts_NEng_20k_restricted)
cross_correlation_plot(2, vts=flu_ts_NEng_20k_restricted)
active_node_plot(vts=flu_ts_NEng_20k_restricted, network=mobility_GNAR_NEng_20k_restricted, max_lag = 2, r_stages=c(1,1))
local_relevance_plot(network=mobility_GNAR_NEng_20k_restricted, r_star=2)
node_relevance_plot(network=mobility_GNAR_NEng_20k_restricted, r_star=1, node_names = V(mobility_igraph_list_NEng_20k[[11]])$name)
mobility_GNAR_NEng_20k_restricted_fit <- GNARfit(vts=flu_ts_NEng_20k_restricted, net = mobility_GNAR_NEng_20k_restricted)
summary(mobility_GNAR_NEng_20k_restricted_fit)


# Difference NEng 10k series ----------------------------------------------

flu_ts_NEng_10k_restricted_diff <- diff(flu_ts_NEng_10k_restricted)

summary(GNARfit(vts=flu_ts_NEng_10k_restricted_diff, net=mobility_max_NEng_10k_restricted_GNAR$network_max_GNAR))


# Box-Cox transformation on differenced time series -----------------------
flu_ts_NEng_10k_restricted_diff |> apply(2, MASS::boxcox)

# MA,RI,CT ----------------------------------------------------------------
MA_RI_CT_counties <- make_fips_vec(c("MA","RI","CT"), US_county_shape = US_county_shape)
mobility_df_list_MA_RI_CT <- mobility_df_list |> lapply(function(B){B|> select(origin, destination) |> filter(origin %in% MA_RI_CT_counties, destination %in% MA_RI_CT_counties)})
mobility_igraph_list_MA_RI_CT <- mobility_df_list_MA_RI_CT |> lapply(graph_from_data_frame)
mobility_igraph_list_MA_RI_CT |> lapply(gorder) |> unlist()
mobility_igraph_list_MA_RI_CT |> lapply(gsize) |> unlist() |> which.max()
flu_ts_df_MA_RI_CT_restricted <- county_flu_ac_season_norm|> filter(county_fips %in% V(mobility_igraph_list_MA_RI_CT[[11]])$name, season=="2018-2019") |> select(county_fips, year_week_dt, conf_flu_norm) |> filter(county_fips %in% V(mobility_igraph_list_MA_RI_CT[[9]])$name)|> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt") 
flu_ts_MA_RI_CT_restricted <- as.matrix(flu_ts_df_MA_RI_CT_restricted)
mobility_max_MA_RI_CT_GNAR <- mobility_igraph_list_MA_RI_CT[[11]] |> igraph::simplify() |> igraphtoGNAR() 
mobility_max_MA_RI_CT_restricted_GNAR_fit_many <- fit_and_predict_for_many(alpha_options = seq(1,10), net=mobility_max_MA_RI_CT_GNAR, upper_limit = diameter(mobility_igraph_list_MA_RI_CT[[11]]), vts=flu_ts_MA_RI_CT_restricted, globalalpha = T )
return_best_model(mobility_max_MA_RI_CT_restricted_GNAR_fit_many)
#34
mobility_max_MA_RI_CT_restricted_GNAR_fit_many[34,"name"]
# GNAR-10-1111000000-TRUE
mobility_max_MA_RI_CT_restricted_GNAR_fit_best <- fit_and_predict(alpha=10, globalalpha = T, beta=c(1,1,1,1,0,0,0,0,0,0), vts=flu_ts_MA_RI_CT_restricted, forecast_window = 5, return_model = T, net=mobility_max_MA_RI_CT_GNAR) 
summary(mobility_max_MA_RI_CT_restricted_GNAR_fit_best)
# modelsummary(models = mobility_max_MA_RI_CT_restricted_GNAR_fit_best, output = "markdown")
# xtable(summary(mobility_max_MA_RI_CT_restricted_GNAR_fit_best))

residuals_mobility_max_MA_RI_CT_restricted_GNAR_fit_best <- check_and_plot_residuals(model=mobility_max_MA_RI_CT_restricted_GNAR_fit_best, data=flu_ts_df_MA_RI_CT_restricted, network_name = mobility_max_MA_RI_CT_restricted_GNAR_fit_many[34,"name"], alpha = 10, n_ahead = 5, counties = MA_RI_CT_counties)




#10k population restriction
MA_RI_CT_counties_10k <- county_pop_2024 %>% filter_at(vars(contains("20")), all_vars(.>10000)) |> select(FIPS) |> filter(FIPS %in% MA_RI_CT_counties)  
mobility_df_list_MA_RI_CT_10k <- mobility_df_list |> lapply(function(B){B|> select(origin, destination) |> filter(origin %in% MA_RI_CT_counties_10k$FIPS, destination %in% MA_RI_CT_counties_10k$FIPS)})
mobility_igraph_list_MA_RI_CT_10k <- mobility_df_list_MA_RI_CT_10k |> lapply(graph_from_data_frame)
mobility_igraph_list_MA_RI_CT_10k |> lapply(gorder) |> unlist()
mobility_igraph_list_MA_RI_CT_10k |> lapply(gsize) |> unlist() |> which.max()
#9
flu_ts_df_MA_RI_CT_10k_restricted <- county_flu_ac_season_norm_10k |> filter(county_fips %in% V(mobility_igraph_list_MA_RI_CT_10k[[9]])$name, season=="2018-2019") |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt") 
flu_ts_MA_RI_CT_10k_restricted <- as.matrix(flu_ts_df_MA_RI_CT_10k_restricted)
mobility_max_MA_RI_CT_10k_GNAR <- mobility_igraph_list_MA_RI_CT_10k[[9]]  |>  igraph::simplify() |> igraphtoGNAR() 
corbit_plot(vts=flu_ts_MA_RI_CT_10k_restricted, net=mobility_max_MA_RI_CT_10k_GNAR, max_lag = 10, max_stage = diameter(mobility_igraph_list_MA_RI_CT_10k[[9]] |> igraph::simplify()), rectangular_plot = "square")

mobility_max_MA_RI_CT_10k_restricted_GNAR_fit_many <- fit_and_predict_for_many(alpha_options = seq(1,10), net = mobility_max_MA_RI_CT_10k_GNAR, upper_limit = diameter(mobility_igraph_list_MA_RI_CT_10k[[9]]), vts=flu_ts_MA_RI_CT_10k_restricted)
return_best_model(mobility_max_MA_RI_CT_10k_restricted_GNAR_fit_many)
#2
mobility_max_MA_RI_CT_10k_restricted_GNAR_fit_many[2,"name"]
mobility_max_MA_RI_CT_10k_restricted_GNAR_fit_best <- fit_and_predict(alpha=2, beta=c(1,0), net=mobility_max_MA_RI_CT_10k_GNAR, vts=flu_ts_MA_RI_CT_10k_restricted, return_model = T, forecast_window = 5)
summary(mobility_max_MA_RI_CT_10k_restricted_GNAR_fit_best)
residuals_mobility_max_MA_RI_CT_10k_restricted_GNAR_fit_best <- check_and_plot_residuals(model=mobility_max_MA_RI_CT_10k_restricted_GNAR_fit_best, data = flu_ts_MA_RI_CT_10k_restricted, alpha = 2, n_ahead = 5, counties = MA_RI_CT_counties_10k$FIPS, network_name = mobility_max_MA_RI_CT_10k_restricted_GNAR_fit_many[2,"name"] )


# HHS Region 5 ------------------------------------------------------------

HHS_5_counties <- make_fips_vec(c("IL", "IN", "MI", "MN", "OH", "WI"), US_county_shape)
HHS_5_counties_10k <- county_pop_2024 |> filter_at(vars(contains("20")), all_vars(.>10000)) |> select(FIPS) |> filter(FIPS %in% HHS_5_counties) |> pull()
mobility_df_list_HHS_5_10k <- mobility_df_list |> lapply(function(B){B|> select(origin, destination) |> filter(origin %in% HHS_5_counties_10k, destination %in% HHS_5_counties_10k)})
mobility_igraph_list_HHS_5_10k <- mobility_df_list_HHS_5_10k |> lapply(graph_from_data_frame)
mobility_igraph_list_HHS_5_10k |> lapply(gorder) |> unlist()
mobility_igraph_list_HHS_5_10k |> lapply(gsize) |> unlist() |> which.max()
#10
flu_ts_df_HHS_5_10k_restricted <- county_flu_ac_season_norm |> filter(county_fips %in% V(mobility_igraph_list_HHS_5_10k[[10]])$name, season=="2018-2019") |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt") 
flu_ts_HHS_5_10k_restricted <- as.matrix(flu_ts_df_HHS_5_10k_restricted)
mobility_max_HHS_5_10k_restricted_GNAR <- mobility_igraph_list_HHS_5_10k[[10]] |> igraph::simplify() |> igraphtoGNAR()
corbit_plot(vts=flu_ts_HHS_5_10k_restricted, net=mobility_max_HHS_5_10k_restricted_GNAR, max_lag=10, max_stage = diameter(mobility_igraph_list_HHS_5_10k[[10]]), rectangular_plot = "square")
mobility_max_HHS_5_10k_restricted_GNAR_fit_many <- fit_and_predict_for_many(alpha_options = seq(1,10), net=mobility_max_HHS_5_10k_restricted_GNAR, upper_limit = diameter(mobility_igraph_list_HHS_5_10k[[10]]), globalalpha = T, vts = flu_ts_HHS_5_10k_restricted)
mobility_max_HHS_5_10k_restricted_GNAR_fit <- GNARfit(vts=flu_ts_HHS_5_10k_restricted, net=mobility_max_HHS_5_10k_restricted_GNAR)
#singular
#1-lag once difference HHS 5 series
flu_ts_HHS_5_10k_restricted_diff <- diff(flu_ts_HHS_5_10k_restricted)
mobility_max_HHS_5_10k_restricted_diff_GNAR_fit<- GNARfit(vts=flu_ts_HHS_5_10k_restricted_diff, net=mobility_max_HHS_5_10k_restricted_GNAR)
summary(mobility_max_HHS_5_10k_restricted_diff_GNAR_fit)
# twice-difference 1-lag
# flu_ts_HHS_5_10k_restricted_2_diff <- diff(flu_ts_HHS_5_10k_restricted)
# 50k in 2020 restriction
HHS_5_counties_50k <- county_pop_2024 |> filter(`2020`>50000) |> select(FIPS) |> filter(FIPS %in% HHS_5_counties) |> pull()
HHS_5_counties_50k_diff_GNAR <- create_network_max_GNAR(edge_df_list = mobility_df_list, target_counties = HHS_5_counties_50k, ts_df = county_flu_ac_season_norm)
HHS_5_counties_50k_diff_GNAR_fit<- GNARfit(vts=diff(HHS_5_counties_50k_diff_GNAR[[2]]), net = HHS_5_counties_50k_diff_GNAR[[1]])
vcov(HHS_5_counties_50k_diff_GNAR_fit)
summary(HHS_5_counties_50k_diff_GNAR_fit)
#Singular
#100k in 2020
HHS_5_counties_100k <- county_pop_2024 |> filter(`2020`>100000) |> select(FIPS) |> filter(FIPS %in% HHS_5_counties) |> pull()
HHS_5_counties_100k_diff_GNAR <- create_network_max_GNAR(edge_df_list = mobility_df_list, target_counties = HHS_5_counties_100k, ts_df = county_flu_ac_season_norm)
HHS_5_counties_100k_diff_GNARfit <- GNARfit(vts=diff(HHS_5_counties_100k_diff_GNAR[[2]]), net = HHS_5_counties_100k_diff_GNAR[[1]])
summary(HHS_5_counties_100k_diff_GNARfit)
#150k in 2020
HHS_5_counties_150k <- county_pop_2024 |> filter(`2020`>150000) |> select(FIPS) |> filter(FIPS %in% HHS_5_counties) |> pull()
HHS_5_counties_150k_GNAR <- create_network_max_GNAR(edge_df_list = mobility_df_list, ts_df = county_flu_ac_season_norm, target_counties = HHS_5_counties_150k)
HHS_5_counties_150k_diff_GNAR_fit <- GNARfit(vts=diff(HHS_5_counties_150k_GNAR[[2]]), net=HHS_5_counties_150k_GNAR[[1]]) 
summary(HHS_5_counties_150k_diff_GNAR_fit)
# twice-difference series 
#need to check with diff dataframe version
HHS_5_counties_150k_2diff_GNAR <- list(network_max_GNAR=HHS_5_counties_150k_GNAR[[1]], ts_network_max=diff(HHS_5_counties_150k_GNAR[[2]]))
HHS_5_counties_150k_2diff_GNAR_fit<- GNARfit(net=HHS_5_counties_150k_2diff_GNAR[[1]], vts=HHS_5_counties_150k_2diff_GNAR[[2]]) 
summary(HHS_5_counties_150k_2diff_GNAR_fit)
