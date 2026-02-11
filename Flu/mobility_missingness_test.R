# test the GNARdesign and GNARfit handling of zero weight on missingness as a workaround for dynamic networks where nodes appear/are removed dynamically.
# the "tvnets" argument should handle this, but as of package version 1.1.4, only static networks with "tvnets=NULL" are accepted.

# Import Libraries and source function files --------------------------------------------------------
library(tidyverse)
library(GNAR)
library(igraph)
# library(readr)
library(data.table)
library(polars)
library(tidypolars)
library(ISOweek)
library(ggraph)
readRDS("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Flu/county_flu_ac_season_norm.RDS")
# Mobility data processing ------------------------------------------------
#import  monthly data and fips index files (see Pullano et al 2024)
mobility_df_list <- list.files("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/US-Connectivity-Metapop-main/data/US_connectivity_network_2020_county", pattern = "monthly_*") |> lapply(function(X){fread(paste0("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/US-Connectivity-Metapop-main/data/US_connectivity_network_2020_county/", X))}) |> lapply(function(Y){setnames(Y, old=c("V1","V2","V3", "V4"), new=c("origin", "destination", "Month","Connectivity"))})
mobility_county_fips_index <- fread("Data/Mobility/US-Connectivity-Metapop-main/data/counties_fips_index.csv") |> 
  mutate(
    GEO_ID = str_pad(GEO_ID, 5, pad="0"))
NEng_counties <- make_fips_vec(c("MA", "RI", "CT", "VT", "NH", "ME"), US_county_shape = US_county_shape) 
#Match origin and destination to index GEO_IDs
mobility_df_list <-  mobility_df_list |> lapply(function(Z){setkey(mobility_county_fips_index, index) 
Z[, origin:=mobility_county_fips_index[.(origin), GEO_ID]]
Z[, destination:= mobility_county_fips_index[.(destination), GEO_ID]]})
# saveRDS(mobility_df_list, file = "~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/mobility_df_list.rds")
# Create igraphs for mobility networks and find the largest ---------------
mobility_igraph_list <- mobility_df_list |> lapply(function(A){A|> select(origin, destination)|> filter(origin %in% county_flu_ac_season_norm$county_fips, destination %in% county_flu_ac_season_norm$county_fips) |> graph_from_data_frame()|> simplify()})
#nodes
mobility_igraph_list|> lapply(gorder) |> unlist() 
#edges
mobility_igraph_list|> lapply(gsize) |> unlist() |> which.max() 
# Create GNAR objects without mobility weighting --------------------------
mobility_GNAR <- mobility_igraph_list[[10]] |> simplify() |> igraphtoGNAR()
# matched_counties <- V(mobility_igraph_list[[10]])$name
# Create flu ts for GNAR with mobility edges ------------------------------
flu_ts_df<- county_flu_ac_season_norm |> select(county_fips, year_week_dt, conf_flu_norm) |> filter(county_fips %in% V(mobility_igraph_list[[10]])$name, year(year_week_dt)>=2019 & year(year_week_dt)<2021)|> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt") 
flu_ts <- flu_ts_df |> as.matrix()


# Diagnostics on Flu TS ---------------------------------------------------
Ljung_Box_res <- apply(flu_ts, 2, function(X){Box.test(X, type="Ljung-Box")})
adf_res <- apply(flu_ts,2,function(Y){tseries::adf.test(Y)})
# create_ts(flu_df = county_flu_ac_season_norm_mobility_2019_NEng, county_shape = NEng_county_shape, net_type = "Mobility")
# fit unweighted GNAR objects to flu Time Series --------------------------
flu_mobility_GNAR_fit <- fit_and_predict_for_many(net=mobility_GNAR, upper_limit = diameter(mobility_igraph_list[[10]]), old=T, vts = flu_ts)


# NEng subset -------------------------------------------------------------

mobility_df_list_NEng<- mobility_df_list|> lapply(function(B){B|> select(origin, destination) |> filter(origin %in% NEng_counties, destination %in% NEng_counties)})
mobility_igraph_list_NEng <- mobility_df_list_NEng |> lapply(graph_from_data_frame)
mobility_igraph_list_NEng |> lapply(gorder) |> unlist() 
mobility_igraph_list_NEng |> lapply(gsize) |> unlist() |> which.max()
mobility_GNAR_NEng<- mobility_igraph_list_NEng[[11]] |> igraph::simplify() |> igraphtoGNAR()
flu_ts_df_NEng <- county_flu_ac_season_norm|> select(county_fips, year_week_dt, conf_flu_norm) |> filter(county_fips %in% V(mobility_igraph_list_NEng[[11]])$name, year(year_week_dt)>=2019 )|> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt") 
flu_ts_NEng<- flu_ts_df_NEng |> as.matrix()
# flu_mobility_GNAR_NEng_fit <- fit_and_predict_for_many(net=mobility_GNAR_NEng, upper_limit = diameter(mobility_igraph_list_NEng[[11]]), old=T, vts = flu_ts_NEng)
flu_mobility_GNAR_NEng_fit <- GNARfit(net = mobility_GNAR_NEng, vts=flu_ts_NEng)
summary(flu_mobility_GNAR_NEng_fit)








# Try with 1-week differenced time series ---------------------------------

flu_ts_df_diff <- county_flu_ac_season_norm_diff  |> select(county_fips, year_week_dt, conf_flu_norm_Diff) |> filter(county_fips %in% V(mobility_igraph_list[[10]])$name, year(year_week_dt)>=2019 & year(year_week_dt)<2021)|> spread(county_fips, conf_flu_norm_Diff) |> column_to_rownames(var="year_week_dt") 
flu_ts_diff <- as.matrix(flu_ts_df_diff)
flu_diff_mobility_GNAR_fit <- fit_and_predict_for_many(net = mobility_GNAR, vts=flu_ts_diff)

flu_ts_NEng_df_diff<- county_flu_ac_season_norm_diff |> select(county_fips, year_week_dt, conf_flu_norm_Diff) |> filter(county_fips %in% V(mobility_igraph_list_NEng[[11]])$name) |> spread(county_fips, conf_flu_norm_Diff) |> column_to_rownames(var="year_week_dt")
flu_ts_diff_NEng <- as.matrix(flu_ts_NEng_df_diff)
#try log1p transform
# log1p(flu_ts_diff_NEng)
flu_diff_mobility_NEng_GNAR_fit <- GNARfit(vts=flu_ts_diff_NEng, net=mobility_GNAR_NEng)
summary(flu_mobility_GNAR_NEng_fit)
flu_diff_log1p_mobility_NEng_GNAR_fit <- GNARfit(vts=log1p(flu_ts_diff_NEng), net=mobility_GNAR_NEng)
summary(flu_diff_log1p_mobility_NEng_GNAR_fit)
#Try checking for linear dependence
#compute variance-covariance matrix of residuals
flu_diff_log1p_mobility_NEng_var_covar <- 1/flu_diff_log1p_mobility_NEng_GNAR_fit$frbic$time.in * t(residToMat(flu_diff_log1p_mobility_NEng_GNAR_fit, nnodes=flu_diff_log1p_mobility_NEng_GNAR_fit$frbic$nnodes)$resid) %*% residToMat(flu_diff_log1p_mobility_NEng_GNAR_fit, nnodes=flu_diff_log1p_mobility_NEng_GNAR_fit$frbic$nnodes)$resid
is.singular.matrix(flu_diff_log1p_mobility_NEng_var_covar)
#Singular
find_linear_dependent_columns(flu_diff_log1p_mobility_NEng_var_covar, tol = 1e-50)
#No dependent columns???
#compute rank of var-covar residual matrix
matrixcalc::matrix.rank(flu_diff_log1p_mobility_NEng_var_covar)
matrixcalc::matrix.rank(flu_diff_log1p_mobility_NEng_var_covar, method="chol")
matrixcalc::matrix.inverse(flu_diff_log1p_mobility_NEng_var_covar)
#check determinant
det(flu_diff_log1p_mobility_NEng_var_covar)
solve(flu_diff_log1p_mobility_NEng_var_covar)
Matrix::det(flu_diff_log1p_mobility_NEng_var_covar)
Matrix::determinant(flu_diff_log1p_mobility_NEng_var_covar)
#condition and reciprocal condition number
Matrix::rcond(flu_diff_log1p_mobility_NEng_var_covar)
1/Matrix::condest(flu_diff_log1p_mobility_NEng_var_covar)$est
#Full rank???
qr(flu_diff_log1p_mobility_NEng_var_covar)$rank
# Try with undirected mobility network ------------------------------------
mobility_igraph_undir_list <- mobility_igraph_list|> lapply(function(X){as_undirected(X,mode = "collapse")})
mobility_igraph_undir_list |> lapply(gorder) |> unlist()
mobility_igraph_undir_list |> lapply(gsize) |> unlist() |> which.max()
mobility_igraph_undir_list |> lapply(gsize) |> unlist() |> which.min()
# graph with largest number of edges
mobility_undir_max_GNAR <- mobility_igraph_undir_list[[10]] |> igraphtoGNAR()
#graph with smallest number of edges
mobility_undir_min_GNAR <- mobility_igraph_undir_list[[7]] |> igraphtoGNAR()

# mobility_undir_max_GNAR_fit <- fit_and_predict_for_many(vts = flu_ts, net = mobility_undir_max_GNAR)

corbit_plot(net=mobility_undir_max_GNAR, vts=flu_ts, max_lag=5, max_stage = diameter(mobility_igraph_undir_list[[10]]), rectangular_plot = "square")


# MA submodel -------------------------------------------------------------
MA_counties <- NEng_counties[startsWith(NEng_counties,"25")]
mobility_df_list_MA <- mobility_df_list|> lapply(function(B){B|> select(origin, destination) |> filter(origin %in% MA_counties, destination %in% MA_counties)})
mobility_igraph_list_MA <- mobility_df_list_MA |> lapply(graph_from_data_frame)
mobility_igraph_list_MA |> lapply(gorder) |> unlist()
mobility_igraph_list_MA |> lapply(gsize) |> unlist() |> which.min()
mobility_igraph_list_MA |> lapply(gsize) |> unlist() |> which.max()
mobility_dir_min_MA_GNAR <- mobility_igraph_list_MA[[7]] |> igraph::simplify() |> igraphtoGNAR()
flu_ts_df_MA  <- county_flu_ac_season_norm|> select(county_fips, year_week_dt, conf_flu_norm) |> filter(county_fips %in% V(mobility_igraph_list_MA[[7]])$name, year(year_week_dt)>=2019 & year(year_week_dt)<2021)|> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt") 
flu_ts_MA <- as.matrix(flu_ts_df_MA)
corbit_plot(vts=flu_ts_MA, net=mobility_dir_min_MA_GNAR, max_lag = 10, max_stage = diameter(mobility_igraph_list_MA[[7]]), rectangular_plot = "square")

# mobility_dir_min_MA_GNAR_fit <- GNARfit(vts=flu_ts_MA, net=mobility_dir_min_MA_GNAR)
mobility_dir_min_MA_GNAR_fit <- fit_and_predict_for_many(alpha_options = c(1,6), vts=flu_ts_MA, net=mobility_dir_min_MA_GNAR, upper_limit = 2)
summary(mobility_dir_min_MA_GNAR_fit)

# mobility_igraph_list_MA |> lapply(is.directed) |> unlist()
mobility_dir_max_MA_GNAR <- mobility_igraph_list_MA[[9]] |> igraph::simplify() |> igraphtoGNAR() 
mobility_dir_max_MA_GNAR_fit <- GNARfit(vts=flu_ts_MA, net=mobility_dir_max_MA_GNAR)
summary(mobility_dir_max_MA_GNAR_fit)
# CA submodel -------------------------------------------------------------
CA_counties <- make_fips_vec("CA", US_county_shape = US_county_shape)
mobility_df_list_CA <- mobility_df_list|> lapply(function(B){B|> select(origin, destination) |> filter(origin %in% CA_counties, destination %in% CA_counties)})
mobility_igraph_list_CA <- mobility_df_list_CA |> lapply(graph_from_data_frame)
mobility_igraph_list_CA |> lapply(gorder) |> unlist()
mobility_igraph_list_CA |> lapply(gsize) |> unlist() |> which.min()
mobility_igraph_list_CA |> lapply(gsize) |> unlist() |> which.max()
flu_ts_df_CA <- county_flu_ac_season_norm|> select(county_fips, year_week_dt, conf_flu_norm) |> filter(county_fips %in% V(mobility_igraph_list_CA[[9]])$name)|> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt") 
flu_ts_CA <- as.matrix(flu_ts_df_CA)
mobility_dir_max_CA_GNAR <- mobility_igraph_list_CA[[9]] |> igraph::simplify() |> igraphtoGNAR()
corbit_plot(vts=flu_ts_CA, net=mobility_dir_max_CA_GNAR, max_lag = 10, max_stage = diameter(mobility_igraph_list_CA[[9]]), rectangular_plot = "square")
mobility_dir_max_CA_GNAR_fit <- fit_and_predict(alpha = 5, beta = c(2,2,2,2,2), vts=flu_ts_CA, net=mobility_dir_max_CA_GNAR, forecast_window = 5, globalalpha = T)
# mobility_dir_max_CA_GNAR_fit <- GNARfit(vts=flu_ts_CA, net = mobility_dir_max_CA_GNAR)
summary(mobility_dir_max_CA_GNAR_fit)
# find dependent design matrix columns to diagnose singularity
# fullRankMatrix::find_linear_dependent_columns(GNARdesign(mobility_dir_max_CA_GNAR, vts=flu_ts_CA, alphaOrder = 10, betaOrder = c(1,1,1,1,1,1,1,1,1,1)))
# MA, RI, CT submodel -----------------------------------------------------

MA_RI_CT_counties <- make_fips_vec(c("MA","RI","CT"), US_county_shape = US_county_shape)
mobility_df_list_MA_RI_CT <- mobility_df_list |> lapply(function(B){B|> select(origin, destination) |> filter(origin %in% MA_RI_CT_counties, destination %in% MA_RI_CT_counties)})
mobility_igraph_list_MA_RI_CT <- mobility_df_list_MA_RI_CT |> lapply(graph_from_data_frame)

# Old, unused -------------------------------------------------------------
# import mobility data ----------------------------------------------------
# Ony using edgelist to define networks until memory solution found
# social_distancing_county_network_unnorm_edgelist_daily_2019<- data.table::fread("Data/Mobility/social_distancing_county_network_unnorm_edgelist_daily_2019.csv") 
# Reorder dataframe for compatibility with igraph <- -edgelist needs to be first two columns ----------------
# social_distancing_county_network_unnorm_edgelist_daily_2019 <- social_distancing_county_network_unnorm_edgelist_daily_2019 %>% select(origin_county_fips, destination_county_fips, everything())

# Create subset of counties, using New England states ---------------------
# NEng_counties <- make_fips_vec(c("MA", "RI", "CT", "VT", "NH", "ME"), US_county_shape = US_county_shape)
# NEng_county_shape <- create_county_shape(NEng_counties,"NEng")
# subset data to only include New England edges ---------------------------
# social_distancing_county_network_unnorm_edgelist_daily_2019_NEng<- social_distancing_county_network_unnorm_edgelist_daily_2019[origin_county_fips %in% NEng_counties & destination_county_fips %in% NEng_counties]

# Split mobility data into monthly network slices– following Pullano et al. 2024 --------
# social_distancing_county_network_unnorm_edgelist_month_NEng <- social_distancing_county_network_unnorm_edgelist_daily_2019_NEng %>% group_by(month(as.numeric(date))) %>% group_split()

# Coerce each monthly tibble into igraph and remove loops ----------------------------------

# social_distancing_county_network_unnorm_graph <- social_distancing_county_network_unnorm_edgelist_month_NEng %>% lapply(., graph_from_data_frame) %>% lapply(.,simplify)

# Create flu time series object, subset and extract matrix form -------------------------------------------

# flu_ts_NEng<- create_ts(county_shape = NEng_county_shape, net_type = "Mobility")[[1]] %>% filter(year(rownames(.)) %in% year(
#   as.numeric(
#     social_distancing_county_network_unnorm_edgelist_daily_2019_NEng$date
#   )
# )  &
#   month(rownames(.)) %in% month(as.numeric(social_distancing_county_network_unnorm_edgelist_daily_2019_NEng$date)))
# 

# mobility_df <- read_csv_polars("Data/Mobility/social_distancing_county_network_unnorm_edgelist_daily_2019.csv", schema_overrides = list("date"=pl$Date, "origin_county_fips"=pl$String, "destination_county_fips"=pl$String))
# mobility_df<- mobility_df$with_columns(pl$col("origin_county_fips")$cast(pl$String), pl$col("destination_county_fips")$cast(pl$String))
# Map mobility dates back to ISO year-week for compatibility with flu dates
# NEng_county_shape <- create_county_shape(NEng_counties,"NEng")
# mobility_df_NEng <- mobility_df|> filter(origin_county_fips %in% NEng_counties & destination_county_fips %in% NEng_counties)
#use polars to create year_week strings
# dates <- pl$DataFrame(date=mobility_df_NEng
# )$with_columns(date_string=pl$col("date")$dt$strftime("%G-%V"))
# dates<- dates$with_columns(year_week_dt=(pl$col("date_string")+"-1")$str$to_date(format="%G-%V-%u"))
# mobility_df_NEng <-mobility_df_NEng$with_columns(date_string=pl$col("date")$dt$strftime("%G-%V"))
# convert to year_week_dt with ISO monday matching flu data format
# mobility_df_NEng<- mobility_df_NEng$with_columns(year_week_dt=(pl$col("date_string")+"-1")$str$to_date(format="%G-%V-%u"))
#aggregate by weekly mean
# mobility_df_NEng_dates_agg <- mobility_df_NEng$group_by(c("origin_county_fips", "destination_county_fips", "year_week_dt"))$agg(mean=(pl$col("num_visits")/pl$col("visitor_count"))$mean()) |> as.data.table()
# match dates between mobility and flu data
# mobility_dates <- data.table::as.data.table(dates)
# county_week_flu_v3_imputed_clean[county_week_flu_v3_imputed_clean$year_week=="2019-01",c("year_week","year_week_dt")]
# mobility_2019_NEng<- social_distancing_county_network_unnorm_edgelist_daily_2019_NEng[mobility_dates$year_week_dt%in%county_flu_ac_season_norm$year_week_dt]
#merge by origin county for now
# county_flu_ac_season_norm_mobility_2019_NEng<-merge(county_flu_ac_season_norm, mobility_df_NEng_dates_agg, by.x=c("county_fips", "year_week_dt"), by.y=c("origin_county_fips", "year_week_dt")) 

# plots to check data

# county_flu_ac_season_norm_mobility_2019_NEng |> ggplot(aes(x=year_week_dt, y=mean, colour = county_fips)) + geom_line()
# Get edgelist from mobility data
# mobility_igraph <- mobility_df_NEng_dates_agg %>% select(origin_county_fips, destination_county_fips) %>% as.matrix() %>% graph_from_edgelist() |> simplify()
# mobility_GNAR <- mobility_igraph |> igraphtoGNAR()
# plot(mobility_igraph, layout=layout.fruchterman.reingold, main="fruchterman.reingold")