library(tidyverse)
library(GNAR)
library(igraph)
library(readr)
source("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Flu/functions_paper_modified.R")

# Read synthetic season flu data from metapopulation model ----------------


# Actual county populations
infected_series_real_pop <- read_csv("Data/Flu/infected_series_real_pop.csv")

# Read population data and compute disease burden -------------------------

# co_est2020_alldata_clean_10k <- readRDS(file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Population/co_est2020_alldata_clean_10k.RDS")
co_est2020_alldata_clean <- readRDS("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Population/co_est2020_alldata_clean.RDS")
infected_series_real_pop_burden <- infected_series_real_pop |>
  left_join(
    co_est2020_alldata_clean |> select(FIPS, POPESTIMATE2020),
    by = c("county" = "FIPS")
  ) |>
  rename(county_pop = POPESTIMATE2020) |>
  mutate(flu_burden = I / county_pop)
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
mobility_unweighted_igraph_Nov <- mobility_igraph_list[[11]]
mobility_unweighted_GNAR_Nov <- igraphtoGNAR(mobility_unweighted_igraph_Nov)
# Create real population time series --------------------------------------
real_pop_norm_ts<- infected_series_real_pop_burden |> select(time, county, flu_burden) |> spread(county, flu_burden) |> column_to_rownames(var="time")|> as.matrix()
ts_counties  <- colnames(real_pop_norm_ts)
# net_counties <- names(mobility_weighted_GNAR_Nov$edges)   

V(mobility_weighted_igraph_Nov)$name |> length()
setdiff(V(mobility_weighted_igraph_Nov)$name, ts_counties)
net_counties <- V(mobility_weighted_igraph_Nov)$name

real_pop_norm_ts_aligned <- real_pop_norm_ts[, net_counties]   

ncol(real_pop_norm_ts_aligned) == length(mobility_weighted_GNAR_Nov$edges)   # should now be TRUE
all(colnames(real_pop_norm_ts_aligned) == net_counties)                      # order check
# length(ts_counties); length(net_counties)
# setdiff(net_counties, ts_counties)   
# setdiff(ts_counties, net_counties)   
# Fit Real Pop GNAR -------------------------------------------------------

real_pop_GNAR_fit <- GNARfit(vts = real_pop_norm_ts_aligned, net = mobility_weighted_GNAR_Nov, alphaOrder = 1, betaOrder = c(1))
#Singular
# Uniform county populations
infected_series_uniform_pop <- read_csv("Data/Flu/infected_series_uniform_pop.csv")
infected_series_uniform_pop_burden <- infected_series_real_pop |> mutate(county_pop=1E5, flu_burden=100*I/county_pop)
infected_series_uniform_pop_ts <- infected_series_uniform_pop_burden |> select(time, county, flu_burden) |> spread(county, flu_burden) |> column_to_rownames(var="time")|> as.matrix()
infected_series_uniform_pop_ts_aligned <- infected_series_uniform_pop_ts[, net_counties]
uniform_pop_GNAR_fit <- GNARfit(vts=infected_series_uniform_pop_ts_aligned, net=mobility_weighted_GNAR_Nov)
summary(uniform_pop_GNAR_fit)
#Singular

# Synthetic fits with unweighted networks ---------------------------------

uniform_pop_unweighted_GNAR_fit <- GNARfit(vts=infected_series_uniform_pop_ts_aligned, net=mobility_unweighted_GNAR_Nov)
real_pop_unweighted_GNAR_fit <- GNARfit(vts=real_pop_norm_ts_aligned, net=mobility_unweighted_GNAR_Nov)
summary(uniform_pop_unweighted_GNAR_fit)
summary(real_pop_unweighted_GNAR_fit)
