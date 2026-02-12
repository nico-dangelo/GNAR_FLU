
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(spdep)
library(sf)
library(sp)
library(data.table)
# Read data ---------------------------------------------------------------
county_week_flu_v3_imputed <- readr::read_csv("Data/Flu/county_week_flu_v3_imputed.csv")

# Clean data and fix formats -----------------------------------------------
county_week_flu_v3_imputed_clean <- county_week_flu_v3_imputed
county_week_flu_v3_imputed_clean$year_week_dt <- as_date(county_week_flu_v3_imputed$year_week_dt, format =
                                                           "%G-%V-%u")
# # Normalization -----------------------------------------------------------
# #Import all cause mortality data
county_week_ac_v3_imputed <- readr::read_csv("Data/Flu/county_week_ac_v3_imputed.csv")

# Normalize flu data by ac

# extract year from year_week
county_week_ac_v3_imputed_year <-  county_week_ac_v3_imputed %>% mutate(year =
                                                                          sub("-.*", "", year_week))

#compute year-level mean ac
county_year_ac_mean <- county_week_ac_v3_imputed_year %>% group_by(county_fips, year) %>% mutate(year_ac_mean =
                                                                                                   mean(all_cause_wtd))

# Normalize county-week ac by yearly means

county_week_ac_norm <-  county_year_ac_mean %>% group_by(county_fips, year) %>% mutate(week_ac_norm =
                                                                                         all_cause_wtd / year_ac_mean)



#merge ac with flu data by county and year_week

county_week_flu_ac_merged <- merge.data.frame(county_week_flu_v3_imputed_clean, county_week_ac_norm)

# adjust conf_flu by normalized county-week ac

county_week_flu_ac_merged_adj <- county_week_flu_ac_merged  %>% mutate(conf_flu_adj =
                                                                         conf_flu / week_ac_norm)

# import seasonal file
county_season_ac_v3_imputed <- readr::read_csv("Data/Flu/county_season_ac_v3_imputed.csv") %>% rename(season_all_cause =
                                                                                                        all_cause,
                                                                                                      season_all_cause_wtd = all_cause_wtd)


# Note: season = July to June; partition weekly data with season flag

county_flu_ac_season <- county_week_flu_ac_merged_adj %>% mutate(season =
                                                                   ifelse(
                                                                     month(year_week_dt) >= 7,
                                                                     paste0(year(year_week_dt), "-", year(year_week_dt) + 1),
                                                                     # e.g., "2023-2024"
                                                                     paste0(year(year_week_dt) - 1, "-", year(year_week_dt))
                                                                   ))

county_flu_ac_season_merged <- merge.data.frame(county_flu_ac_season, county_season_ac_v3_imputed)

# normalize by season_all_cause

county_flu_ac_season_norm <- county_flu_ac_season_merged %>% mutate(conf_flu_norm = conf_flu_adj /
                                                                      season_all_cause_wtd)
# saveRDS(county_flu_ac_season_norm, "~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Flu/county_flu_ac_season_norm.RDS")
rm(county_flu_ac_season, county_flu_ac_season_merged, county_season_ac_v3_imputed, county_week_ac_norm, county_week_ac_v3_imputed, county_week_ac_v3_imputed_year,county_week_flu_ac_merged, county_week_flu_ac_merged_adj, county_week_flu_v3_imputed, county_week_flu_v3_imputed_clean, county_year_ac_mean)
# Import and Process Shapefile --------------------------------------------

# County-level shapefile 
US_county_shape<- st_read("Shapefiles/cb_2020_us_county_5m/cb_2020_us_county_5m.shp")

# Check that all counties are included in flu data
#check names
exclude_names <- setdiff(US_county_shape$NAMELSAD, county_flu_ac_season_norm$county_name)
#check fips
exclude_fips <- setdiff(US_county_shape$GEOID, county_flu_ac_season_norm$county_fips)

include_fips <- intersect(US_county_shape$GEOID, county_flu_ac_season_norm$county_fips)

# Construct centroid coordinates


cent_coord <- US_county_shape%>% subset(.,GEOID %in% include_fips) %>%
  st_geometry() %>%
  st_centroid() %>%
  st_coordinates()
names(cent_coord) <- US_county_shape$GEOID


# Time Series Diagnostics -------------------------------------------------
# Differencing
county_flu_ac_season_norm_diff<- county_flu_ac_season_norm |> group_by(county_fips) |> mutate(conf_flu_norm_Diff=c(0,diff(conf_flu_norm, lag=1))) |>ungroup()
which.min(county_flu_ac_season_norm_diff$flu_norm_conf_Diff)

# Ljung-Box Tests ---------------------------------------------------------

# Ljung_Box_res <- county_flu_ac_season_norm |> select(year_week_dt,county_fips, conf_flu_norm) |> group_by(county_fips)  |> Box.test(conf_flu_norm, type="Ljung-Box")
