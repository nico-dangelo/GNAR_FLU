
# Import data and libraries -----------------------------------------------
library(tidyverse)
library(GNAR)
library(igraph)
#flu data
flu_data <-readRDS("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Flu/county_flu_ac_season_norm.RDS") |> select(county_fips, year_week_dt, conf_flu)
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
mobility_GNAR <- mobility_igraph_list[[10]] |> simplify() |> igraphtoGNAR()
#max path length
# Create raw flu count ts for GNAR based on mobility edges ----------------

flu_count_ts <- flu_data |> filter(county_fips %in% V(mobility_igraph_list[[10]])$name, year(year_week_dt)>=2019 & year(year_week_dt)<2021)|> spread(county_fips, conf_flu) |> column_to_rownames(var="year_week_dt") |> as.matrix()
# fit unweighted GNAR objects to flu count Time Series --------------------------
# flu_mobility_GNAR_fit <- fit_and_predict_for_many(net=mobility_GNAR, upper_limit = diameter(mobility_igraph_list[[10]]), old=T, vts = flu_count_ts)
GNARdesign(vts=flu_count_ts, net=mobility_GNAR)
