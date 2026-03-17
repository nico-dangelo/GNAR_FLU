
# libraries ---------------------------------------------------------------

library(tidyverse)
#import complete data
co_est2020_alldata <- read_csv("Data/Population/co-est2020-alldata.csv")
# drop state-level count rows
co_est2020_alldata_clean <- co_est2020_alldata |> filter(SUMLEV!="040") 
# check dropping
identical(co_est2020_alldata |> filter(COUNTY!="000"), co_est2020_alldata_clean)

# Subsets -----------------------------------------------------------------

# 10k
co_est2020_alldata_clean_10k <- co_est2020_alldata_clean |> filter(POPESTIMATE2020>=10000)
saveRDS(co_est2020_alldata_clean_10k, file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Population/co_est2020_alldata_clean_10k.RDS")
#20k
co_est2020_alldata_clean_20k <- co_est2020_alldata_clean |> filter(POPESTIMATE2020>=20000)
saveRDS(co_est2020_alldata_clean_20k, file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Population/co_est2020_alldata_clean_20k.RDS")

# Check CT ----------------------------------------------------------------

co_est2020_alldata_clean_10k |> filter(STATE=="09")


# Regional subsets --------------------------------------------------------
NENG_states <- state_fips_lookup(c("MA","RI","CT", "VT","ME", "NH"))
co_est2020_NENG_10k <- co_est2020_alldata_clean_10k |> filter(STATE  %in% NENG_states)
saveRDS(co_est2020_NENG_10k, file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Population/co_est2020_NENG_10k.RDS")
co_est2020_NENG_20k <- co_est2020_alldata_clean_20k |> filter(STATE %in% NENG_states)
saveRDS(co_est2020_NENG_20k, file="~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Population/co_est2020_NENG_20k.RDS")
HHS_5_states <- 