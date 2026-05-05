# Source necessary function and preprocessing scripts ---------------------
#Data preprocessing
source("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Flu/flu_data_preprocessing.R")

# functions

source("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Flu/functions_paper_modified.R")

# Libraries ---------------------------------------------------------------
library(magrittr)
library(mcstatsim)
library(modelsummary)

# State-level models ------------------------------------------------------



# MA ----------------------------------------------------------------------
#make FIPS vector to subset counties
fips_vec_MA <- make_fips_vec("MA", US_county_shape = US_county_shape)
#make county shapefile
MA_county_shape<- create_county_shape(fips_vec_MA)
# make coordinate dataframe
cent_coord_MA <- create_cent_coord(MA_county_shape)
# Great Circle distance matrix
dist_df_MA <- circle_distance(cent_coord_MA)
# make KNN GNAR objects
min_k_MA <- 3
max_k_MA <- nrow(cent_coord_MA)-1
MA_KNN_GNAR <- create_KNN_objects(cent_coord = cent_coord_MA, min_k = min_k_MA, max_k = max_k_MA, keep.igraph = T)
names(MA_KNN_GNAR[[3]]) <- as.character(min_k_MA:max_k_MA)
# Moran's I on normalized Flu data 
flu_norm_ts_moran_MA <- create_ts(county_shape= MA_county_shape, forMoran = T, asMatrix = F)


  for (j in seq(1, ((max_k_MA - min_k_MA) + 1))) {
    moran_I_permutation_test(
      data = flu_norm_ts_moran_MA,
      county_index = MA_KNN_GNAR[[1]],
      g = MA_KNN_GNAR[[3]][[j]] ,
      name = paste("KNN", "MA", names(MA_KNN_GNAR[[3]])[j], sep = "_")
    )
  }
#Normalized flu time series objects for GNAR
flu_norm_ts_df_MA <-create_ts(df=county_flu_ac_season_norm,county_shape=MA_county_shape, asMatrix=F)[[1]]



# Fit a global alpha GNAR model with no distance, population weighting, no factor node weights

