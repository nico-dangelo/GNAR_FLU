# test the GNARdesign and GNARfit handling of zero weight on missingness as a workaround for dynamic networks where nodes appear/are removed dynamically.
# the "tvnets" argument should handle this, but as of package version 1.1.4, only static networks with "tvnets=NULL" are accepted.

# Import Libraries and source function files --------------------------------------------------------
# library(tidyverse)
# library(GNAR)
# library(igraph)
# library(readr)
library(data.table)
library(polars)
library(tidypolars)
library(ISOweek)
library(ggraph)
readRDS("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Flu/county_flu_ac_season_norm.RDS")
# Mobility data processing ------------------------------------------------
#import  monthly data and fips index files (see Pulano et al 2024)
mobility_df_list <- list.files("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/US-Connectivity-Metapop-main/data/US_connectivity_network_2020_county", pattern = "monthly_*") |> lapply(function(X){fread(paste0("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/US-Connectivity-Metapop-main/data/US_connectivity_network_2020_county/", X))}) |> lapply(function(Y){setnames(Y, old=c("V1","V2","V3", "V4"), new=c("origin", "destination", "Month","Connectivity"))})
mobility_county_fips_index <- fread("Data/Mobility/US-Connectivity-Metapop-main/data/counties_fips_index.csv") |> 
  mutate(
    GEO_ID = str_pad(GEO_ID, 5, pad="0"))
NEng_counties <- make_fips_vec(c("MA", "RI", "CT", "VT", "NH", "ME"), US_county_shape = US_county_shape) 
#Match origin and destination to index GEO_IDs
mobility_df_list |> lapply(function(Z){setkey(mobility_county_fips_index, index) 
Z[, origin:=mobility_county_fips_index[.(origin), GEO_ID]]
Z[, destination:= mobility_county_fips_index[.(destination), GEO_ID]]})
# saveRDS(mobility_df_list, file = "~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/mobility_df_list.rds")
# Create igraphs for mobility networks and find the largest ---------------
mobility_igraph_list <- mobility_df_list |> lapply(function(A){A|> select(origin, destination)|> filter(origin %in% county_flu_ac_season_norm$county_fips, destination %in% county_flu_ac_season_norm$county_fips) |> graph_from_data_frame()})
#nodes
mobility_igraph_list|> lapply(gorder) |> unlist() 
#edges
mobility_igraph_list|> lapply(gsize) |> unlist() |> which.max() 
# Create GNAR objects without mobility weighting --------------------------
mobility_GNAR <- mobility_igraph_list[[10]] |> simplify() |> igraphtoGNAR()
# matched_counties <- V(mobility_igraph_list[[10]])$name
# Create flu ts for GNAR with mobility edges ------------------------------
flu_ts <- county_flu_ac_season_norm|> select(county_fips, year_week_dt, conf_flu_norm) |> filter(county_fips %in% V(mobility_igraph_list[[10]])$name, year(year_week_dt)>=2019 & year(year_week_dt)<2021)|> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt") |> as.matrix()


# Diagnostics on Flu TS ---------------------------------------------------


# create_ts(flu_df = county_flu_ac_season_norm_mobility_2019_NEng, county_shape = NEng_county_shape, net_type = "Mobility")
# fit unweighted GNAR objects to flu Time Series --------------------------
flu_mobility_GNAR_fit <- fit_and_predict_for_many(net=mobility_GNAR, upper_limit = diameter(mobility_igraph_list[[10]]), old=T, vts = flu_ts)


# NEng subset -------------------------------------------------------------

mobility_df_list_NEng<- mobility_df_list|> lapply(function(B){B|> select(origin, destination) |> filter(origin %in% NEng_counties, destination %in% NEng_counties)})
mobility_igraph_list_NEng <- mobility_df_list_NEng |> lapply(graph_from_data_frame)
mobility_igraph_list_NEng |> lapply(gorder) |> unlist() 
mobility_igraph_list_NEng |> lapply(gsize) |> unlist() |> which.max()
mobility_GNAR_NEng<- mobility_igraph_list_NEng[[11]] |> simplify() |> igraphtoGNAR()
flu_ts_NEng <- county_flu_ac_season_norm|> select(county_fips, year_week_dt, conf_flu_norm) |> filter(county_fips %in% V(mobility_igraph_list_NEng[[11]])$name, year(year_week_dt)>=2019 & year(year_week_dt)<2021)|> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt") |> as.matrix()
flu_mobility_GNAR_NEng_fit <- fit_and_predict_for_many(net=mobility_GNAR_NEng, upper_limit = diameter(mobility_igraph_list_NEng[[11]]), old=T, vts = flu_ts_NEng)
summary(flu_mobility_GNAR_NEng_fit)












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