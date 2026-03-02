
# Import data and libraries -----------------------------------------------
library(tidyverse)
library(GNAR)
library(igraph)
#flu data
flu_data <-readRDS("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Flu/county_flu_ac_season_norm.RDS") |> select(county_fips, year_week_dt, conf_flu, season)
#mobility data
# mobility_county_fips_index <- fread("Data/Mobility/US-Connectivity-Metapop-main/data/counties_fips_index.csv") |> 
#   mutate(
#     GEO_ID = str_pad(GEO_ID, 5, pad="0"))
mobility_df_list <- readRDS("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/mobility_df_list.rds")

# Create igraphs for mobility networks and find the largest ---------------
mobility_igraph_list <- mobility_df_list |> lapply(function(A){A|> select(origin, destination)|> filter(origin %in% flu_data$county_fips, destination %in% flu_data$county_fips) |> graph_from_data_frame()})
#nodes
mobility_igraph_list|> lapply(gorder) |> unlist() 
#edges
mobility_igraph_list|> lapply(gsize) |> unlist() |> which.max() 
# Create GNAR objects without mobility weighting --------------------------
mobility_GNAR <- mobility_igraph_list[[10]] |> igraph::simplify() |> igraphtoGNAR()
#max path length
# Create raw flu count ts for GNAR based on mobility edges ----------------
#restrict to one seaon
flu_count_ts <- flu_data |> filter(county_fips %in% V(mobility_igraph_list[[10]])$name, season=="2018-2019")|> select(conf_flu, county_fips, year_week_dt) |> spread(county_fips, conf_flu) |> column_to_rownames(var="year_week_dt") |> as.matrix()
# fit unweighted GNAR objects to flu count Time Series --------------------------
# flu_mobility_GNAR_fit <- fit_and_predict_for_many(net=mobility_GNAR, upper_limit = diameter(mobility_igraph_list[[10]]), old=T, vts = flu_count_ts)
# GNARdesign(vts=flu_count_ts, net=mobility_GNAR)
# exponentiate?
# flu_count_exp_ts <- exp(flu_count_ts)
# GNARfit(vts=flu_count_exp_ts, net=mobility_GNAR) |> summary()

# Difference counts -------------------------------------------------------

flu_count_ts_diff <- diff(flu_count_ts, differences = 2)
mobility_GNAR_flu_diff_fit <- GNARfit(vts=flu_count_ts_diff, net=mobility_GNAR)
