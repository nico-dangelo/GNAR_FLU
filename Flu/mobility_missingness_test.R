# test the GNARdesign and GNARfit handling of zero weight on missingness as a workaround for dynamic networks where nodes appear/are removed dynamically.
# the "tvnets" argument should handle this, but as of package version 1.1.4, onlyy static networks with "tvnets=NULL" are accepted.

# Import Libraries and source function files --------------------------------------------------------
library(tidyverse)
library(GNAR)
library(igraph)
library(readr)
library(data.table)
# import mobility data ----------------------------------------------------
# Ony using edgelist to define networks until memory solution found
social_distancing_county_network_unnorm_edgelist_daily_2019<- data.table::fread("Data/Mobility/social_distancing_county_network_unnorm_edgelist_daily_2019.csv", select=c("date","origin_county_fips", "destination_county_fips")) 
# Reorder dataframe for compatibility with igraph <- -edgelist needs to be first two columns ----------------
# social_distancing_county_network_unnorm_edgelist_daily_2019 <- social_distancing_county_network_unnorm_edgelist_daily_2019 %>% select(origin_county_fips, destination_county_fips, everything())

# Create subset of counties, using New England states ---------------------
NEng_counties <- make_fips_vec(c("MA", "RI", "CT", "VT", "NH", "ME"), US_county_shape = US_county_shape)
NEng_county_shape <- create_county_shape(NEng_counties,"NEng")
# subset data to only include New England edges ---------------------------
social_distancing_county_network_unnorm_edgelist_daily_2019_NEng<- social_distancing_county_network_unnorm_edgelist_daily_2019[origin_county_fips %in% NEng_counties & destination_county_fips %in% NEng_counties]

# Split mobility data into monthly network slices– following Pullano et al. 2024 --------
social_distancing_county_network_unnorm_edgelist_month_NEng <- social_distancing_county_network_unnorm_edgelist_daily_2019_NEng %>% group_by(month(as.numeric(date))) %>% group_split()

# Coerce each monthly tibble into igraph and remove loops ----------------------------------

social_distancing_county_network_unnorm_graph <- social_distancing_county_network_unnorm_edgelist_month_NEng %>% lapply(., graph_from_data_frame) %>% lapply(.,simplify)

# Create flu time series object, subset and extract matrix form -------------------------------------------

flu_ts_NEng<- create_ts(county_shape = NEng_county_shape, net_type = "Mobility")[[1]] %>% filter(year(rownames(.)) %in% year(
  as.numeric(
    social_distancing_county_network_unnorm_edgelist_daily_2019_NEng$date
  )
)  &
  month(rownames(.)) %in% month(as.numeric(social_distancing_county_network_unnorm_edgelist_daily_2019_NEng$date)))


# Create GNAR objects without mobility weighting --------------------------

social_distancing_county_network_month_NEng_GNAR <- lapply(social_distancing_county_network_unnorm_graph, igraphtoGNAR)


# fit unweighted GNAR objects to flu Time Series --------------------------

social_distancing_county_network_month_NEng_GNAR %>% lapply(., GNARfit(vts=flu_ts_NEng))

