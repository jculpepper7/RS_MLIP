# The purpose of this data is to perform a cluster analysis on the ice 
# phenology data derived from the remote sensing algorithm


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(factoextra)

# 1. Import trend data ----------------------------------------------------

#Need to treat the ice on and ice off separately, so isolate them
#as dataframes here

#Import ice on trends
ice_on_trends <- read_csv(here('data/mk_suitable_data.csv')) %>% 
  filter(
    event == 'ice_on'
  )

#Import ice off trends
ice_off_trends <- read_csv(here('data/mk_suitable_data.csv')) %>% 
  filter(
    event == 'ice_off'
  )

# 2. Clean data -----------------------------------------------------------

#To make data useful for a cluster analysis, I must isolate the 
#Hylak_id and the Sen's slope. Then I need to convert the 
#Hylak_id to a row name to perform cluster analysis

#remove all by Hylak_id and Sen's slope for ice on data
on_trends_clean <- ice_on_trends %>% 
  select(
    1, 3
  ) %>% 
  column_to_rownames(var = 'Hylak_id') 

#remove all by Hylak_id and Sen's slope for ice off data
off_trends_clean <- ice_off_trends %>% 
  select(
    1, 3
  ) %>% 
  column_to_rownames(var = 'Hylak_id')

# 3. Cluster analysis -----------------------------------------------------

#First, scale the data
on_scaled <- scale(on_trends_clean)

on_clust <- eclust(on_scaled, "kmeans", k = 3, nstart = 25)

#fviz_silhouette(on_clust)

#fviz_cluster(on_clust) #I get an error here because the data is only one column (slope). I would need another column to visualize.

on_clusters <- as_tibble(on_clust$cluster)

#Join on_clusters with on_trends_clean

cluster_on_join <- ice_on_trends %>% 
  bind_cols(on_clusters) %>% 
  mutate(
    cluster = as.factor(value)
  )


#Visualize

na <- rnaturalearth::ne_states(
  returnclass = "sf") 

full_map_on <- ggplot() +
  ggplot2::geom_sf(data = na) +
  coord_sf(xlim = c(-148, -103), ylim = c(34, 64), expand = FALSE)+
  geom_point(data = cluster_on_join %>% filter(p.value >= 0.05), aes(x = pour_long, y = pour_lat, fill = cluster), size = 2, pch =21, stroke = 0.5)+
  geom_point(data = cluster_on_join %>% filter(p.value <= 0.05), aes(x = pour_long, y = pour_lat, fill = cluster), stroke = 0.5, pch = 24, size = 2)+
  #scale_fill_gradient2(midpoint = 0, low = 'blue', mid = 'white', high = 'red', space = 'Lab')+
  #scale_colour_gradient(low = 'blue', high = 'red', space = 'Lab')+
  xlab("")+
  ylab("")+
  labs(fill = 'Ice On Trend \nDays/yr')+
  labs(fill = '', tag = 'A')+
  theme(
    legend.position = 'right',
    legend.key.height = unit(0.4, 'in'),
    aspect.ratio = 1
    #plot.margin = unit(c(0, 0, 0, 1), 'in')
  )
full_map_on

ggsave(here("results/ice_on_clusters.jpeg"), dpi = 300, width = 6, height = 6, units = "in") #, width = 15, height = 15, units = "in"
