library(tidyverse)
library(GNAR)
library(igraph)
library(readr)
source("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Flu/functions_paper_modified.R")

# Read synthetic season flu data from metapopulation model ----------------


# Actual county populations
infected_series_real_pop <- read_csv("Data/Flu/infected_series_real_pop.csv")
start_date <- as.Date("2023-10-30")
infected_series_real_pop_weeks<- infected_series_real_pop  |> mutate(date = start_date + weeks(time),
                                   year = isoyear(date),
                                   week = isoweek(date), year_week=paste(year, week, sep="-")) |> rename("county_fips"="county")
#read mobility network data, weight, and select November subset
mobility_df_list <- readRDS(file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/mobility_df_list.rds")
mobility_igraph_list <- readRDS("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/mobility_igraph_list.RDS")
# mobility_weighted_igraph_list <- mobility_df_list |> lapply(graph_from_data_frame) |> lapply(function(X) {
#   set_edge_attr(X, "weight", value = E(X)$Connectivity)
# })
# saveRDS(mobility_weighted_igraph_list, file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/mobility_weighted_igraph_list.RDS")
mobility_weighted_igraph_list <- readRDS(file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/mobility_weighted_igraph_list.RDS")
mobility_weighted_igraph_Nov <- mobility_weighted_igraph_list[[11]]

#Normalize by season all-cause
county_week_ac_norm <- readRDS(file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Flu/county_week_ac_norm.RDS")|> mutate(year=as.numeric(year))  
county_week_ac_norm_common_counties <- county_week_ac_norm |> 
  ungroup() |>
  filter(county_fips %in% infected_series_real_pop_weeks$county_fips)
county_week_real_pop_ac_merged <- infected_series_real_pop_weeks |> select(-c("I_total", "I_L", "time")) %>% merge(
  .,
  county_week_ac_norm,
  by = c("county_fips", "year", "year_week"),
  all.x=T
)
county_week_real_pop_ac_merged_adj <- county_week_real_pop_ac_merged |> mutate(I_adj =
                                                                                 I / week_ac_norm)
county_season_ac_v3_imputed <- readr::read_csv("Data/Flu/county_season_ac_v3_imputed.csv") %>% rename(season_all_cause =
                                                                                                        all_cause,
                                                                                                      season_all_cause_wtd = all_cause_wtd)|> as.data.frame()
county_season_ac_real_pop <- county_week_real_pop_ac_merged_adj %>% mutate(season =
                                                     ifelse(
                                                       month(date) >= 7,
                                                       paste0(year(date), "-", year(date) + 1),
                                                       # e.g., "2023-2024"
                                                       paste0(year(date) - 1, "-", year(date))
                                                     ))
county_season_ac_real_pop_merged <- county_season_ac_real_pop %>% merge(.,county_season_ac_v3_imputed, by=c("county_fips", "county_fips_grp","season"), all=F)
county_season_ac_real_pop_norm <- county_season_ac_real_pop_merged %>% mutate(I_norm= I_adj/season_all_cause_wtd)
# plot(county_season_ac_real_pop_norm$I_norm)
real_pop_norm_ts <- county_season_ac_real_pop_norm |> select(year_week, county_fips, I_norm) |> spread(county_fips, I_norm) |> column_to_rownames(var="year_week") |> as.matrix()




# infected_series_real_pop_ts <- infected_series_real_pop|> select(time, county, I) |> spread(county, I)|> column_to_rownames(var="time") |> as.matrix()
# Uniform county populations
infected_series_uniform_pop <- read_csv("Data/Flu/infected_series_uniform_pop.csv")
infected_series_uniform_pop_ts <- infected_series_uniform_pop |>  select(time, county, I) |> spread(county, I)|> column_to_rownames(var="time") |> as.matrix()
