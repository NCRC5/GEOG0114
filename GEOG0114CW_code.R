# load packages 
library(tidyverse)
library(sf)
library(tmap)
library(dbscan)
library(readxl)
library(units)
library(ggplot2)

# most of data are produced as Chinese
Sys.setlocale(category = "LC_ALL", locale = "Chinese")

# load data
# load boundary of Kunming (Sub-district level)
# CRS = 4791, New Beijing / 3-degree Gauss-Kruger CM 102E, projection coordinate
KM_main <- st_read('data/KM_main_area.shp') %>%
  st_transform(., 4791)

names(KM_main) <- c('level_1', 'level_2', 'level_3', 'level_4', 'geometry')

# load bus stops of Kunming
KM_bus <- st_read('data/KM_bus_stops.shp') %>%
  st_transform(., 4791)
# load bus name.xlsx to solve the bug in KM_bus
Bus_name <- read_excel('data/Bus_CHN.xlsx', 1)

KM_bus$PointName <- Bus_name$PointName
KM_bus$LineName <- Bus_name$LineName

# load population of Kunming
KM_pop <- read_excel('data/KM_2010_pop.xlsx', 1)

# load buildings of Kunming
KM_building <- st_read('data/KM_buildings.shp') %>%
  st_transform(., 4791)

# clip KM_building by KM_main
KM_main_building <- KM_building[KM_main,]

# clip KM_bus by KM_main
KM_main_bus <- KM_bus[KM_main,]
main_bus_num <- nrow(KM_main_bus)

# create a new data frame to store accuracy
accuracy <- data.frame(eps = numeric(26),
                       clu_num = numeric(26),
                       clu_acc = numeric(26),
                       po_acc = numeric(26))

# it may take about 15 mins
KM_bus_points <- st_coordinates(KM_main_bus)
for(e in 5:30){
  accuracy$eps[e - 4] <- e
  count_worng_in <- 0
  count_not_in <- 0
  
  # cluster KM_main_bus by DBSCAN
  KM_bus_cluster <- dbscan(KM_bus_points, eps = e, minPts = 1)
  # inspect the number of clusters
  cluster_num <- max(KM_bus_cluster$cluster)
  # assign cluster number to each bus station
  KM_main_bus$cluster <- KM_bus_cluster$cluster
  accuracy$clu_num[e - 4] <- cluster_num
  
  # calculate the accuracy of DBCSAN
  for(clu in 1:cluster_num){
    cluster_bus <- filter(KM_main_bus, cluster == clu)
    clu_points <- nrow(cluster_bus)
    bus_unique <- data.frame(Line = character(clu_points))
    bus_unique$Line <- cluster_bus$LineName
    
    for(po in 1:clu_points){
      bus_unique$Line[po] <- unlist(strsplit(bus_unique$Line[po], split = "路"))[1]
    }
    bus_lines <- length(unique(bus_unique$Line))
    
    if(clu_points > bus_lines){
      count_worng_in <- count_worng_in + 1
      count_not_in <- count_not_in + (clu_points - bus_lines)
    }
  }
  
  for(clu in 1:cluster_num){
    cluster_bus <- filter(KM_main_bus, cluster == clu)
    if(nrow(cluster_bus) == 1){
      name <- cluster_bus$PointName
      all_same_bus <- filter(KM_main_bus, PointName == name)
      if(nrow(all_same_bus) == 2){
        point_unique <- data.frame(Line = character(2))
        point_unique$Line <- all_same_bus$LineName
        
        for(li in 1:2){
          point_unique$Line[li] <- unlist(strsplit(point_unique$Line[li], split = "路"))[1]
        }
        if(unique(point_unique$Line) != 1){
          count_not_in <- count_not_in +1
        }
      }
      
      if(nrow(all_same_bus) > 2){
        count_not_in <- count_not_in +1
      }
    }
  }
  accuracy$clu_acc[e - 4] <- (1 - (count_worng_in / cluster_num)) * 100
  accuracy$po_acc[e - 4] <- (1 - (count_not_in / main_bus_num)) * 100
  
  cat('eps =', e, 'completed.\n')
}

# compare the accuracy and visualization
acc_plot <- data.frame(Accuracy = character(52),
                       epsilon = numeric(52),
                       Accuracy_num = numeric(52))

for(ac in 1:26){
  acc_plot$Accuracy[ac] = 'Cluster'
  acc_plot$Accuracy[ac + 26] = 'Point'
  
  acc_plot$epsilon[ac] = ac + 4
  acc_plot$epsilon[ac + 26] = ac + 4
  
  acc_plot$Accuracy_num[ac] = accuracy$clu_acc[ac]
  acc_plot$Accuracy_num[ac + 26] = accuracy$po_acc[ac]
}

ggplot(acc_plot, aes(x = epsilon, y = Accuracy_num, group = Accuracy, color = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_vline(aes(xintercept = 14),
             linetype = 'dashed',
             color = '#3CB371') +
  geom_hline(aes(yintercept = max(accuracy$po_acc)),
             linetype = 'dashed',
             color = '#3CB371') +
  labs(title = 'Accuracy of DBSCAN with different epsilons',
       y = 'Accuracy(%)') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave('data/accuracy.jpg',
       plot = last_plot(),
       width = 20,
       height = 10,
       units = 'cm',
       dpi = 300)

# choose eps = 14
# cluster KM_main_bus by DBSCAN
KM_bus_cluster <- dbscan(KM_bus_points, eps = 14, minPts = 1)
# inspect the number of clusters
cluster_num <- max(KM_bus_cluster$cluster)
# assign cluster number to each bus station
KM_main_bus$cluster <- KM_bus_cluster$cluster

# create a new data frame to store the centroid of each cluster
KM_bus_unique <- data.frame(lon = numeric(cluster_num),
                            lat = numeric(cluster_num),
                            cluster = numeric(cluster_num),
                            lines = numeric(cluster_num))

# use the centroid as the position of bus station
for(clu in 1:cluster_num){
  bus_cluster <- KM_main_bus %>%
    filter(., cluster == clu)
  
  bus_cluster_points <- as.data.frame(st_coordinates(bus_cluster))
  bus_x <- mean(bus_cluster_points$X)
  bus_y <- mean(bus_cluster_points$Y)
  
  KM_bus_unique$lon[clu] <- bus_x
  KM_bus_unique$lat[clu] <- bus_y
  KM_bus_unique$cluster[clu] <- clu
  KM_bus_unique$lines[clu] <- nrow(bus_cluster)
}

# turn centroids into sf
KM_bus_cen <- st_as_sf(KM_bus_unique,
                       coords = c('lon', 'lat'),
                       crs = 4791)

# create buffer for each land use block to calculate the number of bus stops near by
# create progress bar
pb <- txtProgressBar(style=3)
# record time
star_time <- Sys.time()

# it may take 2.5 hours
for(bu in 1:nrow(KM_main_building)){
  build <- KM_main_building[bu,]
  
  build_buffer <- st_buffer(build, 500)
  
  bus_nearby <- KM_bus_cen[build_buffer,]
  KM_main_building$bus_stops[bu] <- nrow(bus_nearby)
  KM_main_building$lines[bu] <- sum(bus_nearby$lines)
  # show progress
  setTxtProgressBar(pb, bu/nrow(KM_main_building))
}
# close progress bar
close(pb)
# show total time
end_time <- Sys.time()
end_time - star_time

# visualization
tm_shape(KM_main) +
  tm_polygons(col = '#B0C4DE', alpha = 0.7) +
  tm_shape(KM_main_building) +
  tm_polygons(col = 'bus_stops',
              style = 'fixed',
              palette = 'RdYlGn',
              breaks = c(0, 1, 5, 10, 15, 20, 25, 32),
              as.count = TRUE,
              border.alpha = 0,
              title = 'Number of Bus Stops') +
  tm_compass(position = c('left', 'top'),
             type = '4star') +
  tm_scale_bar(position = c('right', 'top'))

# link KM_main and KM_pop
KM_main_pop <- merge(KM_main,
                     KM_pop,
                     by.x = 'level_4',
                     by.y = 'area')

# calculate the population density of Kunming
KM_main_pop$area <- st_area(KM_main_pop)
KM_main_pop$area <- set_units(KM_main_pop$area, value = 'km^2')

KM_main_pop$density <- KM_main_pop$population / KM_main_pop$area

# calculate the mean bus stops for each sub-district
for(sd in 1:nrow(KM_main_pop)){
  sub <- filter(KM_main_pop, level_4 == KM_main_pop$level_4[sd])
  sub_building <- KM_main_building[sub,]
  
  KM_main_pop$avg_stops[sd] <- mean(sub_building$bus_stops)
}

# visualization
ggplot(KM_main_building, aes(x = bus_stops)) +
  geom_histogram(binwidth = 2,
                 fill = '#4682B4',
                 color = '#F0F8FF',
                 alpha = 0.5) +
  labs(x = 'Number of Bus Stops for Buildings',
       y = 'Count',
       title = 'Histogram of Bus Stops for Buildings') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(aes(xintercept = mean(bus_stops)),
             color = '#FFA500',
             linetype = 'dashed',
             size = 1)

ggsave('data/cor.jpg',
       plot = last_plot(),
       width = 15,
       height = 10,
       units = 'cm',
       dpi = 300)

# correlation between density and avg_stops
corr <- data.frame(density = numeric(nrow(KM_main_pop)),
                   avg_stops = numeric(nrow(KM_main_pop)))

corr$density <- as.numeric(KM_main_pop$density)
corr$avg_stops <- KM_main_pop$avg_stops

cor(corr$density, corr$avg_stops)

# visualization of correlation coefficient
ggplot(corr, aes(x = density, y = avg_stops)) +
  geom_smooth(method = 'lm',
              formula = y ~ x,
              color = '#FF8C00') +
  geom_point(color = '#FFA500') +
  labs(x = 'Populatino Density(/km^2)',
       y = 'Average Bus Stops',
       title = 'Linear Regression and Correlation Coefficient',
       subtitle = 'R = 0.82') +
  theme(plot.title = element_text(hjust = 0.5))

# visualization of population density
tm_shape(KM_main_pop) +
  tm_polygons(col = 'density',
              style = 'quantile',
              palette = 'OrRd',
              title = 'Population Density(/km^2)',
              alpha = 0.6) +
  tm_compass(position = c('left', 'top'),
             type = '4star') +
  tm_scale_bar(position = c('right', 'top'))