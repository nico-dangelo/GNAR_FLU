library(tidyverse)
library(GNAR)
library(igraph)
# library(modelsummary)
library(xtable)
# GNAR mobility models restricted to flu season ---------------------------

#restrict flu data to 2018-2019 season
mobility_df_list <- readRDS(file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/mobility_df_list.rds")
flu_ts_df_restricted <- county_flu_ac_season_norm |> filter(season=="2018-2019", county_fips %in% V(mobility_igraph_list[[10]])$name) |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt")  
flu_ts_restricted <- as.matrix(flu_ts_df_restricted)
county_flu_ac_season_norm_10k<- readRDS(file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Population/county_flu_ac_season_norm_10k.RDS")

mobility_df_list_10k <- mobility_df_list |> lapply(function(X){X|> select(origin, destination) |> filter(origin %in% county_flu_ac_season_norm_10k$county_fips, destination %in% county_flu_ac_season_norm_10k$county_fips)})

#full country model, need mobility_missingness script!
# mobility_GNAR_restricted_fit <- GNARfit(vts=flu_ts_restricted, net=mobility_GNAR)
# summary(mobility_GNAR_restricted_fit)
# drop counties under 10K and season restrict
# flu_ts_df_10k_restricted <- county_flu_ac_season_norm_10k |> filter(season=="2018-2019", county_fips %in% V(mobility_igraph_list[[10]])$name) |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt")
# flu_ts_10k_restricted <- as.matrix(flu_ts_df_10k_restricted)
# mobility_10k_restricted_GNAR_fit <- GNARfit(vts=flu_ts_10k_restricted, net = mobility_10k_GNAR)
# summary(mobility_10k_restricted_GNAR_fit)
# logLik(mobility_10k_restricted_GNAR_fit)

# New England population-restricted season model --------------------------
county_flu_ac_season_norm_10k<- readRDS(file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Population/county_flu_ac_season_norm_10k.RDS")
county_pop_2024 <- read_csv("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Population/county_population_with_fips.csv")
NEng_counties <- make_fips_vec(c("MA", "RI", "CT", "VT", "NH", "ME"), county_shape = US_county_shape)
# flu_ts_df_NEng_10k_restricted <- county_flu_ac_season_norm_10k |> filter(season=="2018-2019", county_fips %in% NEng_counties) |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt")
# flu_ts_NEng_10k_restricted <- as.matrix(flu_ts_df_NEng_10k_restricted)
# mobility_df_list_NEng_10k <- mobility_df_list_10k |> lapply(function(X){X|> select(origin, destination) |> filter(origin %in% NEng_counties, destination %in% NEng_counties) })
# mobility_igraph_list_NEng_10k <- mobility_df_list_NEng_10k |> lapply(function(X){X |> select(origin, destination) |> graph_from_data_frame()|> igraph::simplify()})
# mobility_igraph_list_NEng_10k |> lapply(gorder) |> unlist() |> which.max()
# mobility_igraph_list_NEng_10k |> lapply(gsize) |> unlist() |> which.max()
#11 
# mobility_GNAR_NEng_10k_restricted <- igraphtoGNAR(mobility_igraph_list_NEng_10k[[11]])
mobility_max_NEng_10k_restricted_GNAR <- create_network_max_GNAR(edge_df_list = mobility_df_list_10k, target_counties = NEng_counties)
mobility_max_NEng_10k_restricted_GNAR_fit <- GNARfit(vts=mobility_max_NEng_10k_restricted_GNAR[[2]], net=mobility_max_NEng_10k_restricted_GNAR[[1]])
GNARtoigraph(mobility_max_NEng_10k_restricted_GNAR[[1]]) |> diameter()
# vcov(mobility_max_NEng_10k_restricted_GNAR_fit) |> corrplot::corrplot()
# summary(mobility_max_NEng_10k_restricted_GNAR_fit)
# GNARfit_sandwich(vts=mobility_max_NEng_10k_restricted_GNAR[[2]], net=mobility_max_NEng_10k_restricted_GNAR[[1]])
# sandwich(mobility_max_NEng_10k_restricted_GNAR_fit)
# sandwich::vcovHAC(mobility_max_NEng_10k_restricted_GNAR_fit)
# Diagnose issues with fit by removing states and using GNAR plots 
cross_correlation_plot(2, vts=flu_ts_NEng_10k_restricted)
active_node_plot(vts=flu_ts_NEng_10k_restricted, network=mobility_GNAR_NEng_10k_restricted, max_lag = 2, r_stages=c(1,1))
local_relevance_plot(network=mobility_GNAR_NEng_10k_restricted, r_star=2)
node_relevance_plot(network=mobility_GNAR_NEng_10k_restricted, r_star=2, node_names = V(mobility_igraph_list_NEng_10k[[11]])$name)
#investigate county 23021, Piscataquis County, Maine remove counties below 20k
# NEng_counties_20k <- NEng_counties[NEng_counties!="23021"]
NEng_counties_20k<- county_pop_2024 %>% filter(`2020`>20000) |> select(FIPS, County) |> filter(FIPS %in% NEng_counties | grepl("Connecticut", County))  |> pull()
flu_ts_df_NEng_20k_restricted <- county_flu_ac_season_norm |> filter(county_fips %in% NEng_counties_20k$FIPS, season=="2018-2019") |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt")
flu_ts_NEng_20k_restricted <- as.matrix(flu_ts_df_NEng_20k_restricted)
mobility_df_list_NEng_20k <- mobility_df_list_NEng_10k |> lapply(function(X){X|> select(origin,destination) |> filter(origin %in% NEng_counties_20k$FIPS, destination %in% NEng_counties_20k$FIPS)})
mobility_igraph_list_NEng_20k <- mobility_df_list_NEng_20k |> lapply(function(X){X|> select(origin, destination) |> graph_from_data_frame()|> igraph::simplify()})
mobility_igraph_list_NEng_20k |> lapply(gorder) |> unlist()  
mobility_igraph_list_NEng_20k |> lapply(gsize) |> unlist() |> which.max()
#11
mobility_GNAR_NEng_20k_restricted <- igraphtoGNAR(mobility_igraph_list_NEng_20k[[11]])
corbit_plot(net=mobility_GNAR_NEng_20k_restricted, max_lag=5, max_stage = diameter(mobility_igraph_list_NEng_20k[[11]]), rectangular_plot = "square", vts=flu_ts_NEng_20k_restricted)
cross_correlation_plot(2, vts=flu_ts_NEng_20k_restricted)
active_node_plot(vts=flu_ts_NEng_20k_restricted, network=mobility_GNAR_NEng_20k_restricted, max_lag = 2, r_stages=c(1,1))
local_relevance_plot(network=mobility_GNAR_NEng_20k_restricted, r_star=2)
node_relevance_plot(network=mobility_GNAR_NEng_20k_restricted, r_star=1, node_names = V(mobility_igraph_list_NEng_20k[[11]])$name)
mobility_GNAR_NEng_20k_restricted_fit <- GNARfit(vts=flu_ts_NEng_20k_restricted, net = mobility_GNAR_NEng_20k_restricted)
summary(mobility_GNAR_NEng_20k_restricted_fit)


# Difference NEng 10k series ----------------------------------------------

flu_ts_NEng_10k_restricted_diff <- diff(flu_ts_NEng_10k_restricted)

summary(GNARfit(vts=flu_ts_NEng_10k_restricted_diff, net=mobility_max_NEng_10k_restricted_GNAR$network_max_GNAR))


# Box-Cox transformation on differenced time series -----------------------
flu_ts_NEng_10k_restricted_diff |> apply(2, MASS::boxcox)

# New England Unrestricted model ------------------------------------------
NEng_counties <- make_fips_vec(c("MA", "RI", "CT", "VT", "NH", "ME"), county_shape = US_county_shape)
mobility_df_list_NEng<- mobility_df_list|> lapply(function(B){B|> select(origin, destination) |> filter(origin %in% NEng_counties, destination %in% NEng_counties)})
mobility_igraph_list_NEng <- mobility_df_list_NEng |> lapply(graph_from_data_frame)
mobility_igraph_list_NEng |> lapply(gorder) |> unlist() 
mobility_igraph_list_NEng |> lapply(gsize) |> unlist() |> which.max()
#11
NEng_county_index <- data.frame("county_fips"=mobility_igraph_list_NEng[[11]]|> V() |> names(), "index"= seq_along(V(mobility_igraph_list_NEng[[11]])))
mobility_igraph_list_NEng_max_adj<- mobility_igraph_list_NEng[[11]] |> igraph::simplify() |> as_adjacency_matrix(sparse=F)
mobility_GNAR_NEng<- mobility_igraph_list_NEng[[11]] |> igraph::simplify() |> igraphtoGNAR()
mobility_GNAR_NEng_adj <- as.matrix(mobility_GNAR_NEng)
# igraph::difference(mobility_igraph_list_NEng[[11]], GNARtoigraph(mobility_GNAR_NEng))

flu_ts_df_NEng <- county_flu_ac_season_norm |> select(county_fips, year_week_dt, conf_flu_norm) |> filter(county_fips %in% V(mobility_igraph_list_NEng[[11]])$name, year(year_week_dt)>=2019 )|> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt") 
# sp_SFK_GNAR_NEng <- mobility_igraph_list_NEng[[11]] |> shortest_paths(from="25025", output= "epath", predecessors = TRUE)
# SFK_2_nb_GNAR_NEng <- mobility_igraph_list_NEng[[11]] |> neighborhood(order=2, nodes="25025", mode="out", mindist = 2)

# plot network ------------------------------------------------------------
NEng_county_shape<- US_county_shape %>% subset(., GEOID %in% NEng_counties)
cent_coord_NEng <- NEng_county_shape %>% st_geometry() %>%
  st_centroid() %>%
  st_coordinates()
rownames(cent_coord_NEng)<- NEng_county_shape$GEOID
plot(st_geometry(NEng_county_shape))
# plot(mobility_max_NEng_10k_restricted_GNAR[[1]], add=TRUE, rescale=FALSE, layout=cent_coord_NEng, vertex.label=NA, vertex.size=3, edge.width=2, edge.arrow.width=0.5, edge.arrow.size=0.3, vertex.color="black", edge.curved=0.3)
#ensure cent-coord is reorder to match graph
graph_layout_NEng <-cent_coord_NEng[match(NEng_county_index$county_fips,rownames(cent_coord_NEng)),]

plot(
  mobility_GNAR_NEng|> GNARtoigraph(),
  add = TRUE,
  rescale = FALSE,
  layout = graph_layout_NEng,
  vertex.label = NEng_county_index$county_fips,
  vertex.size = 3,
  edge.width = 2,
  edge.arrow.width = 0.5,
  edge.arrow.size = 0.3,
  vertex.color = "black",
  edge.curved = 0.3)

#1-stage

stage_1_GNAR_NEng<- mobility_GNAR_NEng |> GNARtoigraph(stage=1)
V(stage_1_GNAR_NEng)$name <- V(mobility_igraph_list_NEng[[11]])$name
plot(st_geometry(NEng_county_shape))
plot( stage_1_GNAR_NEng,
  add = TRUE,
  rescale = FALSE,
  layout = graph_layout_NEng,
  vertex.label = NA,
  vertex.size = 3,
  edge.width = 2,
  edge.arrow.width = 0.5,
  edge.arrow.size = 0.3,
  vertex.color = "black",
  edge.curved = 0.3  
)

#2-stage 
#stage-2 igraph object
stage_2_GNAR_NEng<- mobility_GNAR_NEng |> GNARtoigraph(stage=2)
V(stage_2_GNAR_NEng)$name <- V(mobility_igraph_list_NEng[[11]])$name
stage_2_nb_GNAR_NEng<-  stage_2_GNAR_NEng |> neighborhood(order=2, mindist = 2)
names(stage_2_nb_GNAR_NEng) <- V(stage_2_GNAR_NEng)$name
# stage_2_nb_GNAR_NEng <- stage_2_nb_GNAR_NEng[-which(is.null(stage_2_nb_GNAR_NEng))]
stage_2_nb_GNAR_NEng
stage_2_nb_graph_GNAR_NEng <- make_neighborhood_graph(stage_2_GNAR_NEng, order=2, mindist=2)
names(stage_2_nb_graph_GNAR_NEng) <- V(stage_2_GNAR_NEng)$name
plot(st_geometry(NEng_county_shape))
plot(
  stage_2_GNAR_NEng,
  add = TRUE,
  rescale = FALSE,
  layout = graph_layout_NEng,
  vertex.label =NA,
  vertex.size = 3,
  edge.width = 2,
  edge.arrow.width = 0.5,
  edge.arrow.size = 0.3,
  vertex.color = "black",
  edge.curved = 0.3)

plot(st_geometry(NEng_county_shape))
plot(
  stage_2_nb_graph_GNAR_NEng$`50021`,
  add = TRUE,
  rescale = FALSE,
  layout = graph_layout_NEng,
  vertex.label= NA,
  vertex.size = 3,
  edge.width = 2,
  edge.arrow.width = 0.5,
  edge.arrow.size = 0.3,
  vertex.color = "black",
  edge.curved = 0.3,
  edge.color="red")
# plot diameter path in stage 1  graph
GNAR_NEng_diameter <- stage_1_GNAR_NEng |> get_diameter()
GNAR_NEng_diameter_path <- shortest_paths(stage_1_GNAR_NEng, from = GNAR_NEng_diameter[1], to=GNAR_NEng_diameter[3], output = "epath")
E(stage_1_GNAR_NEng)$color <- "grey"
E(stage_1_GNAR_NEng)[GNAR_NEng_diameter_path$epath[[1]]]$color <- "red"
plot(st_geometry(NEng_county_shape))
plot(
stage_1_GNAR_NEng,
add=T,
rescale=F,
layout=graph_layout_NEng,
vertex.label= NA,
vertex.size = 3,
edge.width = 2,
edge.arrow.width = 0.5,
edge.arrow.size = 0.3,
vertex.color = "black",
edge.curved = 0.3,
edge.color=E(stage_1_GNAR_NEng)$color
)


# Wagner, Correlation, influence/relevance, and active node plots -------------------
#Wagner NACF
corbit_plot(vts=flu_ts_NEng, net=mobility_GNAR_NEng, max_lag = 10, max_stage = 2, rectangular_plot = "square")

#Wagner PNACF
corbit_plot(vts=flu_ts_NEng, net=mobility_GNAR_NEng, max_lag = 10, max_stage = 2, rectangular_plot = "square", partial = "yes")

# Cross-correlation
cross_correlation_plot(vts=flu_ts_NEng, h=1)
# #Local Neighborhood 
local_relevance_plot(network = mobility_GNAR_NEng,2)
# #local node/ active node
active_node_plot(vts=flu_ts_NEng, max_lag=2, r_stages = c(1,0), network=mobility_GNAR_NEng)
# #Global
node_relevance_plot(mobility_GNAR_NEng, r_star=2)
glob_index_NENG <- node_relevance_plot(mobility_GNAR_NEng, r_star=2)[[2]] 
glob_index_NENG$Node <- NEng_county_index[match(glob_index_NENG$Node, NEng_county_index$index), "county_fips"]

# Plot network coloured by globindex --------------------------------------
plot(st_geometry(NEng_county_shape))
plot(stage_1_GNAR_NEng,
     vertex.size = 3,
     edge.width = 1,
     edge.arrow.width = 0.5,
     edge.arrow.size = 0.3,
     vertex.color = glob_index_NENG$Relevance,
     edge.curved = 0.3  
)

# MA,RI,CT ----------------------------------------------------------------
MA_RI_CT_counties <- make_fips_vec(c("MA","RI","CT"), county_shape = US_county_shape)
mobility_df_list_MA_RI_CT <- mobility_df_list |> lapply(function(B){B|> select(origin, destination) |> filter(origin %in% MA_RI_CT_counties, destination %in% MA_RI_CT_counties)})
mobility_igraph_list_MA_RI_CT <- mobility_df_list_MA_RI_CT |> lapply(graph_from_data_frame)
mobility_igraph_list_MA_RI_CT |> lapply(gorder) |> unlist()
mobility_igraph_list_MA_RI_CT |> lapply(gsize) |> unlist() |> which.max()
flu_ts_df_MA_RI_CT <- county_flu_ac_season_norm|> filter(county_fips %in% V(mobility_igraph_list_MA_RI_CT[[11]])$name, season=="2018-2019") |> select(county_fips, year_week_dt, conf_flu_norm) |> filter(county_fips %in% V(mobility_igraph_list_MA_RI_CT[[9]])$name)|> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt")
flu_ts_MA_RI_CT <- as.matrix(flu_ts_df_MA_RI_CT_restricted)

mobility_max_MA_RI_CT_GNAR <- mobility_igraph_list_MA_RI_CT[[11]] |> igraph::simplify() |> igraphtoGNAR()
# mobility_max_MA_RI_CT_GNAR <- create_network_max_GNAR(edge_df_list = mobility_df_list, target_counties = MA_RI_CT_counties)
# mobility_max_MA_RI_CT_GNAR_fit_many <- fit_and_predict_for_many(alpha_options = seq(1,10), net=mobility_max_MA_RI_CT_GNAR[[1]], upper_limit = mobility_max_MA_RI_CT_GNAR[[3]], vts=mobility_max_MA_RI_CT_GNAR[[2]], globalalpha = T )
# return_best_model(mobility_max_MA_RI_CT_GNAR_fit_many)
#34
mobility_max_MA_RI_CT_GNAR_fit_many[34,"name"]
# GNAR-10-1111000000-TRUE
# mobility_max_MA_RI_CT_GNAR_fit_best <- fit_and_predict(alpha=10, globalalpha = T, beta=c(1,1,1,1,0,0,0,0,0,0), vts=mobility_max_MA_RI_CT_GNAR[[2]], forecast_window = 5, return_model = T, net=mobility_max_MA_RI_CT_GNAR[[1]]) 
# summary(mobility_max_MA_RI_CT_GNAR_fit_best)
# modelsummary(models = mobility_max_MA_RI_CT_restricted_GNAR_fit_best, output = "markdown")
# xtable(summary(mobility_max_MA_RI_CT_restricted_GNAR_fit_best))

# residuals_mobility_max_MA_RI_CT_restricted_GNAR_fit_best <- check_and_plot_residuals(model=mobility_max_MA_RI_CT_restricted_GNAR_fit_best, data=flu_ts_df_MA_RI_CT_restricted, network_name = mobility_max_MA_RI_CT_restricted_GNAR_fit_many[34,"name"], alpha = 10, n_ahead = 5, counties = MA_RI_CT_counties)



# Wagner, Corr, Influence, Relevance --------------------------------------
GNARfit_MA_RI_CT <- GNARfit(vts=flu_ts_MA_RI_CT, net = mobility_max_MA_RI_CT_GNAR, alphaOrder = 10, betaOrder = c(1,1,1,1,0,0,0,0,0,0))
#Cross-correlation

cross_correlation_plot(h=1, vts=flu_ts_MA_RI_CT)

# local relevance
active_node_plot(vts=flu_ts_MA_RI_CT, network = mobility_max_MA_RI_CT_GNAR, max_lag=GNARfit_MA_RI_CT$frbic$alphas.in, r_stages =GNARfit_MA_RI_CT$frbic$betas.in)

#global relevance

GNAR::node_relevance_plot(network = mobility_max_MA_RI_CT_GNAR, r_star =1 )
globindex_MA_RI_CT<- node_relevance_plot(network = mobility_max_MA_RI_CT_GNAR, r_star =1 )
#Local neighborhood relevance
local_relevance_plot(network = mobility_max_MA_RI_CT_GNAR, r_star=1)

#10k population restriction
MA_RI_CT_counties_10k <- county_pop_2024 %>% filter_at(vars(contains("20")), all_vars(.>10000)) |> select(FIPS) |> filter(FIPS %in% MA_RI_CT_counties)  |> pull()
mobility_df_list_MA_RI_CT_10k <- mobility_df_list |> lapply(function(B){B|> select(origin, destination) |> filter(origin %in% MA_RI_CT_counties_10k, destination %in% MA_RI_CT_counties_10k)})
mobility_igraph_list_MA_RI_CT_10k <- mobility_df_list_MA_RI_CT_10k |> lapply(graph_from_data_frame)
mobility_igraph_list_MA_RI_CT_10k |> lapply(gorder) |> unlist()
mobility_igraph_list_MA_RI_CT_10k |> lapply(gsize) |> unlist() |> which.max()
#9
flu_ts_df_MA_RI_CT_10k_restricted <- county_flu_ac_season_norm_10k |> filter(county_fips %in% V(mobility_igraph_list_MA_RI_CT_10k[[9]])$name, season=="2018-2019") |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt") 
flu_ts_MA_RI_CT_10k_restricted <- as.matrix(flu_ts_df_MA_RI_CT_10k_restricted)
mobility_max_MA_RI_CT_10k_GNAR <- mobility_igraph_list_MA_RI_CT_10k[[9]]  |>  igraph::simplify() |> igraphtoGNAR() 
MA_RI_CT_10k_county_index <- data.frame("county_fips"=V(mobility_igraph_list_MA_RI_CT_10k[[9]]) |> names(), "index"=seq_along(V(mobility_igraph_list_MA_RI_CT_10k[[9]])))


corbit_plot(vts=flu_ts_MA_RI_CT_10k_restricted, net=mobility_max_MA_RI_CT_10k_GNAR, max_lag = 10, max_stage = diameter(mobility_igraph_list_MA_RI_CT_10k[[9]] |> igraph::simplify()), rectangular_plot = "square")

mobility_max_MA_RI_CT_10k_restricted_GNAR_fit_many <- fit_and_predict_for_many(alpha_options = seq(1,10), net = mobility_max_MA_RI_CT_10k_GNAR, upper_limit = diameter(mobility_igraph_list_MA_RI_CT_10k[[9]]), vts=flu_ts_MA_RI_CT_10k_restricted)
return_best_model(mobility_max_MA_RI_CT_10k_restricted_GNAR_fit_many)
#2
mobility_max_MA_RI_CT_10k_restricted_GNAR_fit_many[2,"name"]
mobility_max_MA_RI_CT_10k_restricted_GNAR_fit_best <- fit_and_predict(alpha=2, beta=c(1,0), net=mobility_max_MA_RI_CT_10k_GNAR, vts=flu_ts_MA_RI_CT_10k_restricted, return_model = T, forecast_window = 5)
summary(mobility_max_MA_RI_CT_10k_restricted_GNAR_fit_best)
residuals_mobility_max_MA_RI_CT_10k_restricted_GNAR_fit_best <- check_and_plot_residuals(model=mobility_max_MA_RI_CT_10k_restricted_GNAR_fit_best, data = flu_ts_MA_RI_CT_10k_restricted, alpha = 2, n_ahead = 5, counties = MA_RI_CT_counties_10k$FIPS, network_name = mobility_max_MA_RI_CT_10k_restricted_GNAR_fit_many[2,"name"] )

# plot MA, RI network -----------------------------------------------------

MA_RI_county_shape <- US_county_shape %>% subset(., GEOID %in% MA_RI_CT_counties_10k)
cent_coord_MA_RI <- MA_RI_county_shape |> st_geometry() %>%
  st_centroid() %>%
  st_coordinates()
rownames(cent_coord_MA_RI) <- MA_RI_county_shape$GEOID
# MA_RI_CT_10k_graph <- GNARtoigraph(mobility_max_MA_RI_CT_10k_GNAR)
# graph2nb(mobility_igraph_list_MA_RI_CT_10k[[9]])


# igraph::as_data_frame(what="edges")
layout_MA_RI <- cent_coord_MA_RI[match(MA_RI_CT_10k_county_index$county_fips,rownames(cent_coord_MA_RI)),]
stage_1_GNAR_MA_RI <- GNARtoigraph(mobility_max_MA_RI_CT_10k_GNAR, stage = 1)
V(stage_1_GNAR_MA_RI)$name<- V(mobility_igraph_list_MA_RI_CT_10k[[9]])$name
plot(st_geometry(MA_RI_county_shape), border = "black")

plot(
  stage_1_GNAR_MA_RI,
  rescale = F,
  add = T,
  layout = layout_MA_RI,
  vertex.label = MA_RI_CT_10k_county_index$county_fips,
  vertex.size = 1,
  edge.width = 2,
  edge.arrow.width = 0.5,
  edge.arrow.size = 0.3,
  vertex.color = "black",
  edge.curved = 0.3
)


stage_1_nb_graph_GNAR_MA_RI<-  make_neighborhood_graph(stage_1_GNAR_MA_RI, order = 1, mindist = 1)
names(stage_1_nb_graph_GNAR_MA_RI) <- V(stage_1_GNAR_MA_RI)$name
stage_1_nb_graph_GNAR_MA_RI
plot(st_geometry(MA_RI_county_shape), border = "black")
plot(
  stage_1_nb_graph_GNAR_MA_RI$`25025`,
  rescale = F,
  add = T,
  layout = layout_MA_RI,
  vertex.label = MA_RI_CT_10k_county_index$county_fips,
  vertex.size = 1,
  edge.width = 2,
  edge.arrow.width = 0.5,
  edge.arrow.size = 0.3,
  vertex.color = "black",
  edge.curved = 0.3,
edge.color="red")
# HHS Region 5 ------------------------------------------------------------

HHS_5_counties <- make_fips_vec(c("IL", "IN", "MI", "MN", "OH", "WI"), US_county_shape)
HHS_5_counties_10k <- county_pop_2024 |> filter_at(vars(contains("20")), all_vars(.>10000)) |> select(FIPS) |> filter(FIPS %in% HHS_5_counties) |> pull()
mobility_df_list_HHS_5_10k <- mobility_df_list |> lapply(function(B){B|> select(origin, destination) |> filter(origin %in% HHS_5_counties_10k, destination %in% HHS_5_counties_10k)})
mobility_igraph_list_HHS_5_10k <- mobility_df_list_HHS_5_10k |> lapply(graph_from_data_frame)
mobility_igraph_list_HHS_5_10k |> lapply(gorder) |> unlist()
mobility_igraph_list_HHS_5_10k |> lapply(gsize) |> unlist() |> which.max()
#10


# Plot graph --------------------------------------------------------------

HHS_5_counties_10_shape <- US_county_shape %>% subset(., GEOID %in% HHS_5_counties_10k)


flu_ts_df_HHS_5_10k_restricted <- county_flu_ac_season_norm |> filter(county_fips %in% V(mobility_igraph_list_HHS_5_10k[[10]])$name, season=="2018-2019") |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt") 
flu_ts_HHS_5_10k_restricted <- as.matrix(flu_ts_df_HHS_5_10k_restricted)
mobility_max_HHS_5_10k_restricted_GNAR <- mobility_igraph_list_HHS_5_10k[[10]] |> igraph::simplify() |> igraphtoGNAR()
corbit_plot(vts=flu_ts_HHS_5_10k_restricted, net=mobility_max_HHS_5_10k_restricted_GNAR, max_lag=10, max_stage = diameter(mobility_igraph_list_HHS_5_10k[[10]]), rectangular_plot = "square")
mobility_max_HHS_5_10k_restricted_GNAR_fit_many <- fit_and_predict_for_many(alpha_options = seq(1,10), net=mobility_max_HHS_5_10k_restricted_GNAR, upper_limit = diameter(mobility_igraph_list_HHS_5_10k[[10]]), globalalpha = T, vts = flu_ts_HHS_5_10k_restricted)
mobility_max_HHS_5_10k_restricted_GNAR_fit <- GNARfit(vts=flu_ts_HHS_5_10k_restricted, net=mobility_max_HHS_5_10k_restricted_GNAR)
#singular
#1-lag once difference HHS 5 series
flu_ts_HHS_5_10k_restricted_diff <- diff(flu_ts_HHS_5_10k_restricted)
mobility_max_HHS_5_10k_restricted_diff_GNAR_fit<- GNARfit(vts=flu_ts_HHS_5_10k_restricted_diff, net=mobility_max_HHS_5_10k_restricted_GNAR)
summary(mobility_max_HHS_5_10k_restricted_diff_GNAR_fit)
# twice-difference 1-lag
# flu_ts_HHS_5_10k_restricted_2_diff <- diff(flu_ts_HHS_5_10k_restricted)
# 50k in 2020 restriction
HHS_5_counties_50k <- county_pop_2024 |> filter(`2020`>50000) |> select(FIPS) |> filter(FIPS %in% HHS_5_counties) |> pull()
HHS_5_counties_50k_diff_GNAR <- create_network_max_GNAR(edge_df_list = mobility_df_list, target_counties = HHS_5_counties_50k, ts_df = county_flu_ac_season_norm)
HHS_5_counties_50k_diff_GNAR_fit<- GNARfit(vts=diff(HHS_5_counties_50k_diff_GNAR[[2]]), net = HHS_5_counties_50k_diff_GNAR[[1]])
vcov(HHS_5_counties_50k_diff_GNAR_fit)
summary(HHS_5_counties_50k_diff_GNAR_fit)
#Singular
#100k in 2020
HHS_5_counties_100k <- county_pop_2024 |> filter(`2020`>100000) |> select(FIPS) |> filter(FIPS %in% HHS_5_counties) |> pull()
HHS_5_counties_100k_GNAR <- create_network_max_GNAR(edge_df_list = mobility_df_list, target_counties = HHS_5_counties_100k, ts_df = county_flu_ac_season_norm)
HHS_5_counties_100k_diff_GNARfit <- GNARfit(vts=diff(HHS_5_counties_100k_diff_GNAR[[2]]), net = HHS_5_counties_100k_diff_GNAR[[1]])
summary(HHS_5_counties_100k_diff_GNARfit)
#150k in 2020
HHS_5_counties_150k <- county_pop_2024 |> filter(`2020`>150000) |> select(FIPS) |> filter(FIPS %in% HHS_5_counties) |> pull()
HHS_5_counties_150k_GNAR <- create_network_max_GNAR(edge_df_list = mobility_df_list, ts_df = county_flu_ac_season_norm, target_counties = HHS_5_counties_150k)
HHS_5_counties_150k_diff_GNAR_fit <- GNARfit(vts=diff(HHS_5_counties_150k_GNAR[[2]]), net=HHS_5_counties_150k_GNAR[[1]]) 
summary(HHS_5_counties_150k_diff_GNAR_fit)
# twice-difference series 
#need to check with diff dataframe version
HHS_5_counties_150k_2diff_GNAR <- list(network_max_GNAR=HHS_5_counties_150k_GNAR[[1]], ts_network_max=diff(HHS_5_counties_150k_GNAR[[2]]))
HHS_5_counties_150k_2diff_GNAR_fit<- GNARfit(net=HHS_5_counties_150k_2diff_GNAR[[1]], vts=HHS_5_counties_150k_2diff_GNAR[[2]]) 
summary(HHS_5_counties_150k_2diff_GNAR_fit)

#check time series correlation matrices?

cor_maxtrix_HHS_5_150k <- cor(HHS_5_counties_150k_GNAR[[2]])
corrplot::corrplot(cor_maxtrix_HHS_5_150k)
corrplot::cor.mtest(cor_maxtrix_HHS_5_150k)
GNAR::cross_correlation_plot(2, HHS_5_counties_150k_GNAR[[2]])


# Filter counties by both 2020 population and all-cause threshold --------

county_flu_ac_season_norm_pop_10k_ac_10k <- county_flu_ac_season_norm_10k |> filter(all_cause_wtd>10000)
counties_pop_10k_ac_10k <- county_flu_ac_season_norm_pop_10k_ac_10k$county_fips |> unique()
mobility_df_list_pop_10k_ac_10k <- mobility_df_list_10k |> lapply(function(X){X|> filter(origin %in% counties_pop_10k_ac_10k, destination %in% counties_pop_10k_ac_10k)})
mobility_max_pop_10k_ac_10k_GNAR <- create_network_max_GNAR(edge_df_list = mobility_df_list_pop_10k_ac_10k, target_counties = counties_pop_10k_ac_10k)                                                                 
mobility_max_pop_10k_ac_10k_GNAR_fit <- GNARfit(vts=mobility_max_pop_10k_ac_10k_GNAR[[1]], net=mobility_max_pop_10k_ac_10k_GNAR[[2]])
summary(mobility_max_pop_10k_ac_10k_GNAR_fit)

#pop and ac 20k
# county_pop_2020_20k <- county_pop_2024|> filter(`2020`>20000) |> pull(FIPS)
county_flu_ac_season_norm_pop_20k_ac_20k <- county_flu_ac_season_norm |> filter( season=="2018-2019" & all_cause_wtd>20000 & county_fips %in% county_pop_2024_20k)
counties_pop_20k_ac_20k <- county_flu_ac_season_norm_pop_20k_ac_20k$county_fips
mobility_df_list_pop_20k_ac_20k <- mobility_df_list |> lapply(function(X){X|> filter(origin %in% counties_pop_20k_ac_20k & destination %in% counties_pop_20k_ac_20k )})
mobility_igraph_list_pop_20k_ac_20k <- mobility_df_list_pop_20k_ac_20k |> lapply(function(X){X|> igraph::graph_from_data_frame() |> igraph::simplify()})
mobility_igraph_list_pop_20k_ac_20k |> lapply(gorder) |> unlist() 
mobility_max_pop_20k_ac_20_graph_index <-  mobility_igraph_list_pop_20k_ac_20k |> lapply(gsize) |> unlist() |> which.max()
mobility_max_pop_20k_ac_20_graph <- mobility_igraph_list_pop_20k_ac_20k[[mobility_max_pop_20k_ac_20_graph_index]]
mobility_max_pop_20k_ac_20_GNAR <- igraphtoGNAR(mobility_max_pop_20k_ac_20_graph)
county_flu_ac_season_norm_pop_20k_ac_20k_ts <- county_flu_ac_season_norm_pop_20k_ac_20k |> select(county_fips, year_week_dt, conf_flu_norm) |> filter(county_fips%in% V(mobility_max_pop_20k_ac_20_graph)$name) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt") |> as.matrix()


# Filter my season ac rather than raw ---------------------------------------
county_pop_2020_20k <- county_pop_2024|> filter(`2020`>20000) |> pull(FIPS)
county_flu_ac_season_norm_pop_20k_season_ac_10k<- county_flu_ac_season_norm|> filter( season=="2018-2019", county_fips %in% county_pop_2020_20k & season_all_cause>10000)
sum(is.na(county_flu_ac_season_norm_pop_20k_season_ac_10k))
county_flu_ac_season_norm_pop_20k_yac_10k_ts <- county_flu_ac_season_norm_pop_20k_season_ac_10k |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt") |> as.matrix()
