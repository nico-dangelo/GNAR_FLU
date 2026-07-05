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

# Diagnostics on residuals and covariance matrix --------------------------

perform_gnar_residual_checks(real_pop_GNAR_fit)
test_residual_normality(real_pop_GNAR_fit, method = "shapiro")|>  select(Result) |> table()
test_residual_normality(real_pop_GNAR_fit, method="lilliefors") |>  select(Result)|> table()
test_residual_heteroscedasticity(real_pop_GNAR_fit)|> select(Homoscedastic) |> table()
check_residual_diagnostics(real_pop_GNAR_fit) |> summarize_diagnostics()
check_residual_diagnostics(real_pop_GNAR_fit) |>  filter(Lilliefors_Result == "Non-normal",
                                                                                Shapiro_Result == "Normal") %>%
                                                                        arrange(desc(Prop_Ties)) %>% head(10)
# plot_residuals_vs_fitted(real_pop_GNAR_fit)
diagnose_vcov_matrix(real_pop_GNAR_fit)|> print_vcov_diagnostics()
plot_vcov_diagnostics(diagnose_vcov_matrix(real_pop_GNAR_fit))
corbit_plot(vts=real_pop_norm_ts_aligned, net=mobility_weighted_GNAR_Nov)
# Try sqrt tranform
real_pop_norm_ts_aligned_sqrt <- sqrt( real_pop_norm_ts_aligned)
real_pop_GNAR_sqrt_fit <- GNARfit(vts = real_pop_norm_ts_aligned_sqrt, net = mobility_weighted_GNAR_Nov, alphaOrder = 1, betaOrder = c(1))
summary(real_pop_GNAR_sqrt_fit)
perform_gnar_residual_checks(real_pop_GNAR_sqrt_fit)
test_residual_normality(real_pop_GNAR_sqrt_fit)
test_residual_heteroscedasticity(real_pop_GNAR_sqrt_fit)
check_residual_diagnostics(real_pop_GNAR_sqrt_fit)|> summarize_diagnostics()
# Uniform county populations
infected_series_uniform_pop <- read_csv("Data/Flu/infected_series_uniform_pop.csv")
infected_series_uniform_pop_burden <- infected_series_real_pop |> mutate(county_pop=1E5, flu_burden=I/county_pop)
infected_series_uniform_pop_ts <- infected_series_uniform_pop_burden |> select(time, county, flu_burden) |> spread(county, flu_burden) |> column_to_rownames(var="time")|> as.matrix()
infected_series_uniform_pop_ts_aligned <- infected_series_uniform_pop_ts[, net_counties]
uniform_pop_GNAR_fit <- GNARfit(vts=infected_series_uniform_pop_ts_aligned, net=mobility_weighted_GNAR_Nov)
summary(uniform_pop_GNAR_fit)
perform_gnar_residual_checks(uniform_pop_GNAR_fit)
#Singular

# Synthetic fits with unweighted networks ---------------------------------

uniform_pop_unweighted_GNAR_fit <- GNARfit(vts=infected_series_uniform_pop_ts_aligned, net=mobility_unweighted_GNAR_Nov)
real_pop_unweighted_GNAR_fit <- GNARfit(vts=real_pop_norm_ts_aligned, net=mobility_unweighted_GNAR_Nov)
summary(uniform_pop_unweighted_GNAR_fit)
summary(real_pop_unweighted_GNAR_fit)
perform_gnar_residual_checks(real_pop_unweighted_GNAR_fit)
perform_gnar_residual_checks(uniform_pop_unweighted_GNAR_fit)

# Fits with Mean-centered, standardized data --------------------------------------------
# ts.plot(abs(infected_series_uniform_pop_ts-mean(infected_series_uniform_pop_ts))/sd(infected_series_uniform_pop_ts))
infected_series_uniform_pop_ts_aligned_centered<- (infected_series_uniform_pop_ts_aligned- colMeans(infected_series_uniform_pop_ts_aligned))
infected_series_uniform_pop_ts_aligned_centered_fit<- GNARfit(vts=infected_series_uniform_pop_ts_aligned_centered, net=mobility_unweighted_GNAR_Nov)
ce
infected_series_uniform_pop_ts_aligned_standard <- infected_series_uniform_pop_ts_aligned_centered/apply(infected_series_uniform_pop_ts_aligned,2,function(X){sd(X,na.rm = T)})
ts.plot(infected_series_uniform_pop_ts_aligned_standard)
infected_series_uniform_pop_ts_aligned_standard_fit <- GNARfit(vts=infected_series_uniform_pop_ts_aligned_standard, net=mobility_unweighted_GNAR_Nov, alphaOrder = 1, betaOrder = 1)



# Jittered covariance for BIC computation ---------------------------------


