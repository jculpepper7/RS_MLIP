# The purpose of this script is to 
# 1. Import Climate NA data
# 2. Extract trends for seasonal and annual climate variables 
# 3. Bind with mann-kendall results from ice phenology data


# Libraries ---------------------------------------------------------------

library(tidyverse) 
library(janitor)
library(here)
library(wql) 
library(lubridate)
library(rnaturalearth)
library(randomForest)
#devtools::install_github("MI2DataLab/randomForestExplainer")
library(randomForestExplainer)
library(plotmo)


# 1. Import data ----------------------------------------------------------

#Ice on data points
climate_na_on <- read_csv(here('data/ice_on_climate_na_1999-2020MSY.csv')) %>% 
  clean_names() %>% 
  rename(
    Hylak_id = id1,
    event = id2
  ) %>% 
  mutate(
    Hylak_id = as.factor(Hylak_id),
    event = as.factor(event)
  )

#Ice off data points
climate_na_off <- read_csv(here('data/ice_off_climate_na_1999-2020MSY.csv')) %>% 
  clean_names() %>% 
  rename(
    Hylak_id = id1,
    event = id2
  ) %>% 
  mutate(
    Hylak_id = as.factor(Hylak_id),
    event = as.factor(event)
  )


# 2. Extract trends -------------------------------------------------------

#Extract slopes from Mann-Kendall trends

climate_na_on_trends <- climate_na_on %>% 
  group_by(Hylak_id) %>% 
  summarise(
    across(6:270, ~mannKen(.x)$sen.slope)
  )

climate_na_off_trends <- climate_na_off %>% 
  group_by(Hylak_id) %>% 
  summarise(
    across(6:270, ~mannKen(.x)$sen.slope)
  )

#Separate full trend dataframes into annual, seasonal, and monthly

climate_na_on_annual <- climate_na_on_trends %>% 
  select(
    #identifier
    Hylak_id,
    #Directly calculated annual variables
    mat, mwmt, mcmt, td, map, msp, ahm, shm,
    #Derived annual variables
    #dd_0, dd5, dd_18, dd18, nffd, ffp, b_ffp, 
    #e_ffp, 
    pas#, emt, ext, eref, cmd, mar, rh, cmi, dd1040
  )

climate_na_on_seasonal <- climate_na_on_trends %>% 
  select(
    #identifier
    Hylak_id,
    #Directly calculated seasonal variables
    tave_wt, tave_sp, tave_sm, tave_at, tmax_wt, tmax_sp, tmax_sm, tmax_at, 
    tmin_wt, tmin_sp, tmin_sm, tmin_at, ppt_wt, ppt_sp, ppt_sm, ppt_at, 
    rad_wt, rad_sp, rad_sm, rad_at,
    #Derived seasonal variables
    #dd_0_wt, dd_0_sp, dd_0_sm, dd_0_at, dd5_wt, dd5_sp, dd5_sm, dd5_at,
    #dd_18_wt, dd_18_sp, dd_18_sm, dd_18_at, dd18_wt, dd18_sp, dd18_sm, dd18_at,
    #nffd_wt, nffd_sp, nffd_sm, nffd_at, 
    pas_wt, pas_sp, pas_sm, pas_at,
    #eref_wt, eref_sp, eref_sm, eref_at, cmd_wt, cmd_sp, cmd_sm, cmd_at,
    #rh_wt, rh_sp, rh_sm, rh_at, cmi_wt, cmi_sp, cmi_sm, cmi_at
  )

climate_na_on_monthly <- climate_na_on_trends %>% 
  select(-c(
    #Directly calculated annual variables
    mat, mwmt, mcmt, td, map, msp, ahm, shm,
    #Derived annual variables
    dd_0, dd5, dd_18, dd18, nffd, ffp, b_ffp, 
    e_ffp, pas, emt, ext, eref, cmd, mar, rh, cmi, dd1040,
    #Directly calculated seasonal variables
    tave_wt, tave_sp, tave_sm, tave_at, tmax_wt, tmax_sp, tmax_sm, tmax_at, 
    tmin_wt, tmin_sp, tmin_sm, tmin_at, ppt_wt, ppt_sp, ppt_sm, ppt_at, 
    rad_wt, rad_sp, rad_sm, rad_at,
    #Derived seasonal variables
    dd_0_wt, dd_0_sp, dd_0_sm, dd_0_at, dd5_wt, dd5_sp, dd5_sm, dd5_at,
    dd_18_wt, dd_18_sp, dd_18_sm, dd_18_at, dd18_wt, dd18_sp, dd18_sm, dd18_at,
    nffd_wt, nffd_sp, nffd_sm, nffd_at, pas_wt, pas_sp, pas_sm, pas_at,
    eref_wt, eref_sp, eref_sm, eref_at, cmd_wt, cmd_sp, cmd_sm, cmd_at,
    rh_wt, rh_sp, rh_sm, rh_at, cmi_wt, cmi_sp, cmi_sm, cmi_at
  ) 
  ) %>% 
  select(
    1:61, pas01, pas02, pas03, pas04, pas05, pas06, pas07, pas08, pas09,
    pas10, pas11, pas12
  )

climate_na_off_annual <- climate_na_off_trends %>% 
  select(
    #identifier
    Hylak_id,
    #Directly calculated annual variables
    mat, mwmt, mcmt, td, map, msp, ahm, shm,
    #Derived annual variables
    #dd_0, dd5, dd_18, dd18, nffd, ffp, b_ffp, 
    #e_ffp, pas, emt, ext, eref, cmd, mar, rh, cmi, dd1040
  )

climate_na_off_seasonal <- climate_na_off_trends %>% 
  select(
    #identifier
    Hylak_id,
    #Directly calculated seasonal variables
    tave_wt, tave_sp, tave_sm, tave_at, tmax_wt, tmax_sp, tmax_sm, tmax_at, 
    tmin_wt, tmin_sp, tmin_sm, tmin_at, ppt_wt, ppt_sp, ppt_sm, ppt_at, 
    rad_wt, rad_sp, rad_sm, rad_at,
    #Derived seasonal variables
    # dd_0_wt, dd_0_sp, dd_0_sm, dd_0_at, dd5_wt, dd5_sp, dd5_sm, dd5_at,
    # dd_18_wt, dd_18_sp, dd_18_sm, dd_18_at, dd18_wt, dd18_sp, dd18_sm, dd18_at,
    # nffd_wt, nffd_sp, nffd_sm, nffd_at, 
    pas_wt, pas_sp, pas_sm, pas_at,
    # eref_wt, eref_sp, eref_sm, eref_at, cmd_wt, cmd_sp, cmd_sm, cmd_at,
    # rh_wt, rh_sp, rh_sm, rh_at, cmi_wt, cmi_sp, cmi_sm, cmi_at
  )

climate_na_off_monthly <- climate_na_off_trends %>% 
  select(-c(
    #Directly calculated annual variables
    mat, mwmt, mcmt, td, map, msp, ahm, shm,
    #Derived annual variables
    dd_0, dd5, dd_18, dd18, nffd, ffp, b_ffp, 
    e_ffp, pas, emt, ext, eref, cmd, mar, rh, cmi, dd1040,
    #Directly calculated seasonal variables
    tave_wt, tave_sp, tave_sm, tave_at, tmax_wt, tmax_sp, tmax_sm, tmax_at, 
    tmin_wt, tmin_sp, tmin_sm, tmin_at, ppt_wt, ppt_sp, ppt_sm, ppt_at, 
    rad_wt, rad_sp, rad_sm, rad_at,
    #Derived seasonal variables
    dd_0_wt, dd_0_sp, dd_0_sm, dd_0_at, dd5_wt, dd5_sp, dd5_sm, dd5_at,
    dd_18_wt, dd_18_sp, dd_18_sm, dd_18_at, dd18_wt, dd18_sp, dd18_sm, dd18_at,
    nffd_wt, nffd_sp, nffd_sm, nffd_at, pas_wt, pas_sp, pas_sm, pas_at,
    eref_wt, eref_sp, eref_sm, eref_at, cmd_wt, cmd_sp, cmd_sm, cmd_at,
    rh_wt, rh_sp, rh_sm, rh_at, cmi_wt, cmi_sp, cmi_sm, cmi_at
  )
  ) %>% 
  select(
    1:61, pas01, pas02, pas03, pas04, pas05, pas06, pas07, pas08, pas09,
    pas10, pas11, pas12
  )

# 3. Prep data for Random Forest ---------------------------------

#Join ice phenoloy and climate data

#Import ice phenology data
mk_suitable <- read_csv(here('data/mk_suitable_data.csv')) %>% 
  mutate(
    Hylak_id = as.factor(Hylak_id),
    event = as.factor(event)
  ) %>% 
  select(
    -c(rel_slope, p.value, continent, country, grand_id, hylak_id, lake_name, lake_type, poly_src, class, prob_suitable, prob_unsuitable)
  )

#Create ice on and ice off datasets
mk_on_cat <- mk_suitable %>% 
  filter(event == 'ice_on') %>% 
  select(-event) %>% 
  mutate(
    sen_cat = if_else(
      sen > 0, as.factor('later'), if_else(
        sen < 0, as.factor('earlier'), as.factor('none')
      )
    )
  ) %>% 
  select(-sen)


mk_off_cat <- mk_suitable %>% 
  filter(event == 'ice_off') %>% 
  select(-event) %>% 
  mutate(
    sen_cat = if_else(
      sen > 0, as.factor('later'), if_else(
        sen < 0, as.factor('earlier'), as.factor('none')
      )
    )
  ) %>% 
  select(-sen)

#Join on and off dataframes with climate dataframes
mk_on_cat_clim_annual <- mk_on_cat %>% 
  full_join(climate_na_on_annual) %>% 
  select(-c(Hylak_id, pour_lat, pour_long))

mk_on_cat_clim_seasonal <- mk_on_cat %>% 
  full_join(climate_na_on_seasonal) %>% 
  select(-c(Hylak_id, pour_lat, pour_long))

mk_on_cat_clim_monthly <- mk_on_cat %>% 
  full_join(climate_na_on_monthly) %>% 
  select(-c(Hylak_id, pour_lat, pour_long))

mk_off_cat_clim_annual <- mk_off_cat %>% 
  full_join(climate_na_off_annual) %>% 
  select(-c(Hylak_id, pour_lat, pour_long))

mk_off_cat_clim_seasonal <- mk_off_cat %>% 
  full_join(climate_na_off_seasonal) %>% 
  select(-c(Hylak_id, pour_lat, pour_long))

mk_off_cat_clim_monthly <- mk_off_cat %>% 
  full_join(climate_na_off_monthly) %>% 
  select(-c(Hylak_id, pour_lat, pour_long))


# 4. Random forest --------------------------------------------------------


# **4a. Ice on random forest ----------------------------------------------

set.seed(42)

#Step 1: Create Random Forest model
ice_on_annual_rf3 <- randomForest(sen_cat ~ ., data = mk_on_cat_clim_annual, ntree = 1000, localImp = TRUE)
#plot(ice_on_annual_rf2)
plotmo(ice_on_annual_rf2) # This shows relationships between top interactions but we get a bit more specific below.

ice_on_seasonal_rf3 <- randomForest(sen_cat ~ ., data = mk_on_cat_clim_seasonal, ntree = 500, localImp = TRUE)
#plot(ice_on_seasonal_rf2)
plotmo(ice_on_seasonal_rf2) # This shows relationships between top interactions but we get a bit more specific below.

ice_on_monthly_rf3 <- randomForest(sen_cat ~ ., data = mk_on_cat_clim_monthly, ntree = 500, localImp = TRUE)
#plot(ice_on_monthly_rf2)
plotmo(ice_on_monthly_rf2)

#Step 2: Get minimum depth distribution for variables
#Get the distribution of minimum depth for ice on

#Annual
min_depth_frame_on_annual2 <- min_depth_distribution(ice_on_annual_rf2)

#Seasonal
min_depth_frame_on_seasonal2 <- min_depth_distribution(ice_on_seasonal_rf2)

#Monthly
min_depth_frame_on_monthly2 <- min_depth_distribution(ice_on_monthly_rf2)
#head(min_depth_frame_on_annual, n = 10)


#Plot the dist. of min. depth for ice on
#Annual
plot_min_depth_distribution(min_depth_frame_on_annual2, min_no_of_trees = 30,  mean_sample = "relevant_trees")

#Seasonal
plot_min_depth_distribution(min_depth_frame_on_seasonal2, min_no_of_trees = 30,  mean_sample = "relevant_trees")

#Monthly
plot_min_depth_distribution(min_depth_frame_on_monthly2, min_no_of_trees = 30,  mean_sample = "top_trees")

#Variable of importance plots
varImpPlot(ice_on_annual_rf)
varImpPlot(ice_on_seasonal_rf)
varImpPlot(ice_on_monthly_rf)


#Step 3: Compose multi-way importance plot
#Get the measured importance to plot the multi-way importance plot

#Annual
#importance_frame_on_annual <- measure_importance(ice_on_annual_rf)
#save(importance_frame_on_annual, file = "IceOn_importance_frame_annual.rda")
#load("IceOn_importance_frame_annual.rda")
#head(importance_frame_on_annual, n = 10)

#Seasonal
# importance_frame_on_seasonal <- measure_importance(ice_on_seasonal_rf)
# save(importance_frame_on_seasonal, file = "IceOn_importance_frame_seasonal.rda")
# load("IceOn_importance_frame_seasonal.rda")
# head(importance_frame_on_seasonal, n = 10)

#Monthly
# importance_frame_on_monthly <- measure_importance(ice_on_monthly_rf)
# save(importance_frame_on_monthly, file = "IceOn_importance_frame_monthly.rda")
# load("IceOn_importance_frame_monthly.rda")
# head(importance_frame_on_monthly, n = 10)

plot_multi_way_importance(
  importance_frame_on_seasonal,
  size_measure = "no_of_nodes"
)
#
plot_multi_way_importance(
  importance_frame_on_monthly,
  x_measure = "mse_increase",
  y_measure = "node_purity_increase",
  size_measure = "p_value",
  no_of_labels = 5
)

#Plot correlations
#Annual
#plot_importance_ggpairs(importance_frame_on_annual)
#plot_importance_rankings(importance_frame_on_annual)
#Seasonal
plot_importance_ggpairs(importance_frame_on_seasonal)
plot_importance_rankings(importance_frame_on_seasonal)
#Monthly
plot_importance_ggpairs(importance_frame_on_monthly)
plot_importance_rankings(importance_frame_on_monthly)

#Step 4: Interactions frame for directions of interactions
#Get variables of importance for plot of variable interactions
vars_annual <- important_variables(importance_frame_on_annual, k = 5, measures = c("mean_min_depth", "no_of_trees"))
vars_seasonal <- important_variables(importance_frame_on_seasonal, k = 5, measures = c("mean_min_depth", "no_of_trees"))
vars_monthly <- important_variables(importance_frame_on_monthly, k = 5, measures = c("mean_min_depth", "no_of_trees"))

#Annual Interactions
#interactions_frame_on_annual <- min_depth_interactions(ice_on_annual_rf, vars_annual)
#save(interactions_frame_on_annual, file = "interactions_frame_on_annual.rda")
#load("interactions_frame_on_annual.rda")
#head(interactions_frame_on_annual[order(interactions_frame_on_annual$occurrences, decreasing = TRUE), ])

#Seasonal interactions
#interactions_frame_on_seasonal <- min_depth_interactions(ice_on_seasonal_rf, vars_seasonal)
#save(interactions_frame_on_seasonal, file = "interactions_frame_on_seasonal.rda")
load("interactions_frame_on_seasonal.rda")
head(interactions_frame_on_seasonal[order(interactions_frame_on_seasonal$occurrences, decreasing = TRUE), ])

#Monthly interactions
#interactions_frame_on_monthly <- min_depth_interactions(ice_on_monthly_rf, vars_monthly)
#save(interactions_frame_on_monthly, file = "interactions_frame_on_monthly.rda")
load("interactions_frame_on_monthly.rda")
head(interactions_frame_on_monthly[order(interactions_frame_on_monthly$occurrences, decreasing = TRUE), ])


#Shows the plot of variable interactions ordered by decreasing number of occurences
plot_min_depth_interactions(interactions_frame_on_annual)
plot_min_depth_interactions(interactions_frame_on_seasonal)
plot_min_depth_interactions(interactions_frame_on_monthly)

#Plot the directions of the interactions
plot_predict_interaction(ice_on_seasonal_rf, mk_on_clim_seasonal, "rad_sp", "dd_18_sm")

#Make a markdown file of gathered evidence using Explain_Forest
#explain_forest(ice_on_annual_rf, interactions = TRUE, data = mk_on_clim_annual)


# **4b. Ice off random forest ---------------------------------------------


#Random forest model for ice off
set.seed(24)

#Step 1: Create Random Forest model
ice_off_annual_rf3 <- randomForest(sen_cat ~ ., data = mk_off_cat_clim_annual, ntree = 500, localImp = TRUE)
#plot(ice_off_annual_rf3)

ice_off_seasonal_rf3 <- randomForest(sen_cat ~ ., data = mk_off_cat_clim_seasonal, ntree = 500, localImp = TRUE)
#plot(ice_on_seasonal_rf3)

ice_off_monthly_rf3 <- randomForest(sen_cat ~ ., data = mk_off_cat_clim_monthly, ntree = 500, localImp = TRUE)
#plot(ice_on_monthly_rf3)


#Step 2: Get minimum depth distribution for variables
#Get the distribution of minimum depth for ice off

#Annual
min_depth_frame_off_annual2 <- min_depth_distribution(ice_off_annual_rf2)

#Seasonal
min_depth_frame_off_seasonal2 <- min_depth_distribution(ice_off_seasonal_rf2)

#Monthly
min_depth_frame_off_monthly2 <- min_depth_distribution(ice_off_monthly_rf2)
#head(min_depth_frame_on_annual, n = 10)


#Plot the dist. of min. depth for ice on
#Annual
plot_min_depth_distribution(min_depth_frame_off_annual2,   mean_sample = "relevant_trees")

#Seasonal
plot_min_depth_distribution(min_depth_frame_off_seasonal2, min_no_of_trees = 30,  mean_sample = "relevant_trees")

#Monthly
plot_min_depth_distribution(min_depth_frame_off_monthly2, min_no_of_trees = 30,  mean_sample = "relevant_trees")

#Variable of importance plots
varImpPlot(ice_off_annual_rf)
varImpPlot(ice_off_seasonal_rf)
varImpPlot(ice_off_monthly_rf)

#Step 3: Compose multi-way importance plot
#Get the measured importance to plot the multi-way importance plot

#Annual
importance_frame_off_annual <- measure_importance(ice_off_annual_rf)
save(importance_frame_off_annual, file = "IceOff_importance_frame_annual.rda")
load("IceOff_importance_frame_annual.rda")
head(importance_frame_off_annual, n = 10)

#Seasonal
importance_frame_off_seasonal <- measure_importance(ice_off_seasonal_rf)
save(importance_frame_off_seasonal, file = "IceOff_importance_frame_seasonal.rda")
load("IceOff_importance_frame_seasonal.rda")
head(importance_frame_off_seasonal, n = 10)

#Monthly
importance_frame_off_monthly <- measure_importance(ice_off_monthly_rf)
save(importance_frame_off_monthly, file = "IceOff_importance_frame_monthly.rda")
load("IceOff_importance_frame_monthly.rda")
head(importance_frame_off_monthly, n = 10)

# plot_multi_way_importance(
#   importance_frame_off_seasonal,
#   size_measure = "no_of_nodes"
# )
# #
# plot_multi_way_importance(
#   importance_frame_off_seasonal,
#   x_measure = "mse_increase",
#   y_measure = "node_purity_increase",
#   size_measure = "p_value",
#   no_of_labels = 5
# )

#Plot correlations
#Annual
plot_importance_ggpairs(importance_frame_off_annual)
plot_importance_rankings(importance_frame_off_annual)
#Seasonal
plot_importance_ggpairs(importance_frame_off_seasonal)
plot_importance_rankings(importance_frame_off_seasonal)
#Monthly
plot_importance_ggpairs(importance_frame_off_monthly)
plot_importance_rankings(importance_frame_off_monthly)

#Step 4: Interactions frame for directions of interactions
#Get variables of importance for plot of variable interactions

vars_off_annual <- important_variables(importance_frame_off_annual, k = 5, measures = c("mean_min_depth", "no_of_trees"))
vars_off_seasonal <- important_variables(importance_frame_off_seasonal, k = 5, measures = c("mean_min_depth", "no_of_trees"))
vars_off_monthly <- important_variables(importance_frame_off_monthly, k = 5, measures = c("mean_min_depth", "no_of_trees"))

#Annual Interactions
interactions_frame_off_annual <- min_depth_interactions(ice_off_annual_rf, vars_off_annual)
save(interactions_frame_off_annual, file = "interactions_frame_off_annual.rda")
load("interactions_frame_off_annual.rda")
head(interactions_frame_off_annual[order(interactions_frame_off_annual$occurrences, decreasing = TRUE), ])

#Seasonal interactions
interactions_frame_off_seasonal <- min_depth_interactions(ice_off_seasonal_rf, vars_off_seasonal)
save(interactions_frame_off_seasonal, file = "interactions_frame_off_seasonal.rda")
load("interactions_frame_off_seasonal.rda")
head(interactions_frame_off_seasonal[order(interactions_frame_off_seasonal$occurrences, decreasing = TRUE), ])

#Monthly interactions
interactions_frame_off_monthly <- min_depth_interactions(ice_off_monthly_rf, vars_off_monthly)
save(interactions_frame_off_monthly, file = "interactions_frame_off_monthly.rda")
load("interactions_frame_off_monthly.rda")
head(interactions_frame_off_monthly[order(interactions_frame_off_monthly$occurrences, decreasing = TRUE), ])


#Shows the plot of variable interactions ordered by decreasing number of occurences
plot_min_depth_interactions(interactions_frame_off_annual)
plot_min_depth_interactions(interactions_frame_off_seasonal)
plot_min_depth_interactions(interactions_frame_off_monthly)

#Plot the directions of the interactions
plot_predict_interaction(ice_off_seasonal_rf, mk_off_clim_seasonal, "rad_sp", "dd_18_sm")








