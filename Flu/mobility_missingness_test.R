# test the GNARdesign and GNARfit handling of zero weight on missingness as a workaround for dynamic networks where nodes appear/are removed dynamically.
# the "tvnets" argument should handle this, but as of package version 1.1.4, onlyy static networks with "tvnets=NULL" are accepted.

# Import Libraries and source function files --------------------------------------------------------
# library(tidyverse)
# library(GNAR)
# library(igraph)
# library(readr)
library(data.table)
library(polars)
library(tidypolars)
library(ISOweek)
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

# Mobility data processing ------------------------------------------------
#import data
mobility_df <- read_csv_polars("Data/Mobility/social_distancing_county_network_unnorm_edgelist_daily_2019.csv", schema_overrides = list("date"=pl$Date, "origin_county_fips"=pl$String, "destination_county_fips"=pl$String))
# mobility_df<- mobility_df$with_columns(pl$col("origin_county_fips")$cast(pl$String), pl$col("destination_county_fips")$cast(pl$String))
# Map mobility dates back to ISO year-week for compatibility with flu dates
NEng_counties <- make_fips_vec(c("MA", "RI", "CT", "VT", "NH", "ME"), US_county_shape = US_county_shape) #|> as_polars_series()
NEng_county_shape <- create_county_shape(NEng_counties,"NEng")
mobility_df_NEng <- mobility_df|> filter(origin_county_fips %in% NEng_counties & destination_county_fips %in% NEng_counties)
#use polars to create year_week strings
# dates <- pl$DataFrame(date=mobility_df_NEng
# )$with_columns(date_string=pl$col("date")$dt$strftime("%G-%V"))
# dates<- dates$with_columns(year_week_dt=(pl$col("date_string")+"-1")$str$to_date(format="%G-%V-%u"))
mobility_df_NEng <-mobility_df_NEng$with_columns(date_string=pl$col("date")$dt$strftime("%G-%V"))
work# convert to year_week_dt with ISO monday matching flu data format
mobility_df_NEng<- mobility_df_NEng$with_columns(year_week_dt=(pl$col("date_string")+"-1")$str$to_date(format="%G-%V-%u"))
#aggregate by weekly mean
mobility_df_NEng_dates_agg <- mobility_df_NEng$group_by(c("origin_county_fips", "destination_county_fips", "year_week_dt"))$agg(mean=(pl$col("num_visits")/pl$col("visitor_count"))$mean()) |> as.data.table()
# match dates between mobility and flu data
# mobility_dates <- data.table::as.data.table(dates)
# county_week_flu_v3_imputed_clean[county_week_flu_v3_imputed_clean$year_week=="2019-01",c("year_week","year_week_dt")]
# mobility_2019_NEng<- social_distancing_county_network_unnorm_edgelist_daily_2019_NEng[mobility_dates$year_week_dt%in%county_flu_ac_season_norm$year_week_dt]
#merge by origin county for now
county_flu_ac_season_norm_mobility_2019_NEng<-merge(county_flu_ac_season_norm, mobility_df_NEng_dates_agg, by.x=c("county_fips", "year_week_dt"), by.y=c("origin_county_fips", "year_week_dt")) 






# Create GNAR objects without mobility weighting --------------------------

# social_distancing_county_network_month_NEng_GNAR <- lapply(social_distancing_county_network_unnorm_graph, igraphtoGNAR)


# fit unweighted GNAR objects to flu Time Series --------------------------

# social_distancing_county_network_month_NEng_GNAR %>% lapply(., GNARfit(vts=flu_ts_NEng))

