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

# GNARX objects -----------------------------------------------------------

# Model Fits --------------------------------------------------------------


# Summary and Diagnostics -------------------------------------------------


