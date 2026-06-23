library(tidyverse)
library(GNAR)
library(igraph)
library(readr)
source("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Flu/functions_paper_modified.R")

# Read synthetic season flu data from metapopulation model ----------------


# Actual county populations
infected_series_real_pop <- read_csv("Data/Flu/infected_series_real_pop.csv")

#read mobility network data, weight, and select November subset
mobility_df_list <- readRDS(file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/mobility_df_list.rds")
mobility_igraph_list <- readRDS("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/mobility_igraph_list.RDS")
# mobility_weighted_igraph_list <- mobility_df_list |> lapply(graph_from_data_frame) |> lapply(function(X) {
#   set_edge_attr(X, "weight", value = E(X)$Connectivity)
# })
# saveRDS(mobility_weighted_igraph_list, file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/mobility_weighted_igraph_list.RDS")
mobility_weighted_igraph_list <- readRDS(file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/mobility_weighted_igraph_list.RDS")
mobility_weighted_igraph_Nov <- mobility_weighted_igraph_list[[11]]
mobility_weighted_GNAR_Nov <- igraphtoGNAR(mobility_weighted_igraph_Nov) 
# mobility_weighted_GNAR_Nov |> weights_matrix()

# Uniform county populations
infected_series_uniform_pop <- read_csv("Data/Flu/infected_series_uniform_pop.csv")
infected_series_uniform_pop_ts <- infected_series_uniform_pop |>  select(time, county, I) |> spread(county, I)|> column_to_rownames(var="time") |> as.matrix()
