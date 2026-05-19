# Use GNARX to model flu case series with exogenous covariates

# Libraries ---------------------------------------------------------------
library(GNAR)
library(tidyverse)
library(igraph)

# Data imports ------------------------------------------------------------
# Flu data
county_flu_ac_season_norm_10k<- readRDS(file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Flu/county_flu_ac_season_norm_10k.RDS")
# population data
co_est2020_alldata_clean_10k <- readRDS(file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Population/co_est2020_alldata_clean_10k.RDS")

# mobility network data
mobility_df_list_10k <- readRDS(file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/mobility_df_list_10k.RDS")
# Covariate data
 
SVI_by_year_by_county <- readr::read_csv("Data/Covariates/SVI/SVI_by_year_by_county.csv", , 
                                         col_types = cols(FIPS = col_character(), 
                                                          year = col_character()), na="-999")

# Igraph objects ----------------------------------------------------------
mobility_igraph_list_10k <- mobility_df_list_10k |> lapply(graph_from_data_frame)|> lapply(igraph::simplify)|> lapply(function(X){set_edge_attr(X,"weight",value = E(X)$Connectivity)}) 

# Clean-up matches --------------------------------------------------------
#match counties for flu and svi series
common_counties_flu_svi_fips <- intersect(names(V(mobility_igraph_list_10k[[11]])),intersect(SVI_by_year_by_county$FIPS, county_flu_ac_season_norm_10k$county_fips))
# Flu and SVI TS objects ----------------------------------------------------------
flu_svi_common_counties <- county_flu_ac_season_norm_10k |> filter(season %in% c("2018-2019"), county_fips %in% common_counties_flu_svi_fips) |> left_join(SVI_by_year_by_county, by=c("county_fips"= "FIPS", "year")) 
flu_ts_common_counties<- flu_svi_common_counties |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt") |> as.matrix()
SVI_ts_common_counties <- flu_svi_common_counties |> select(county_fips, year_week_dt, RPL_THEMES) |> spread(county_fips, RPL_THEMES) |> column_to_rownames(var="year_week_dt") |> as.matrix()
# GNARX objects -----------------------------------------------------------
flu_svi_mobility_GNAR <- igraphtoGNAR(subgraph(mobility_igraph_list_10k[[11]], common_counties_flu_svi_fips))


# Model Fits --------------------------------------------------------------
flu_svi_mobility_GNARX_fit <- GNARXfit(vts=flu_ts_common_counties, net=flu_svi_mobility_GNAR, xvts=list(SVI_ts_common_counties), lambdaOrder = 1)

# Summary and Diagnostics -------------------------------------------------

summary(flu_svi_mobility_GNARX_fit)
