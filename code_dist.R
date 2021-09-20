rm(list=ls())

library(tidyverse)
library(magrittr)
library(sf)
library(nngeo)
library(janitor)
library(tictoc)

set_crs <- 3035

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
setwd("/Users/jeppeviero/Dropbox/02 PhD/18 tutorials/intersections_points")

# ----- Load charging station data
chargers <- st_read("points_in/chargers/opladning.gpx") 

# ----- Transform CRS
chargers <- chargers %>%  
  st_transform(set_crs)

st_crs(chargers)

# ----- Remove charging station duplicates
chargers <- chargers %>% 
  distinct(., name, .keep_all = TRUE) %>% 
  mutate(charger_id = row_number()) # random IDs

# ----- Duplicate chargers dataset
pts1 <- chargers %>% 
  dplyr::select(c(charger_id, geometry))

pts2 <- chargers %>% 
  dplyr::select(c(charger_id, geometry))

# ---------------------------------------------------------
# Calculate distance between charging stations
# ---------------------------------------------------------
# ----- Create distance matrix of N x N dimensions
dist_unclass <- st_distance(pts1, pts2,
                            by_element = FALSE) %>% 
  unclass() %>% 
  as.data.frame()

# ----- Save integer of number of columns (= N)
nn <- ncol(dist_unclass)

# ----- Pivot wide to long
dist_unclass <- dist_unclass %>% 
  pivot_longer(cols = everything(),
               names_to = "v_1",
               values_to = "dist")

# ----- Create new variable to match with chargers
dist_unclass <- dist_unclass %>% 
  mutate(v_2 = paste("V", sort(rep(seq(1:nn), nn)),
                     sep = "")) %>%
  as.data.frame()

# ----- Filter out chargers matched with themselves (the '0 diagonal')
dist_unclass <- dist_unclass %>%
  filter(v_1 != v_2)

# ----- Prepare chargers dataset to match with the long dataset
# First one
chargers_1 <- chargers %>% 
  as.data.frame() %>% 
  dplyr::mutate(v_1 = paste("V",row_number(), sep = "")) %>% 
  dplyr::mutate(charger_id_1 = charger_id) %>% 
  dplyr::select(c(v_1, charger_id_1))

# Second one
chargers_2 <- chargers %>% 
  as.data.frame() %>% 
  dplyr::mutate(v_2 = paste("V",row_number(), sep = "")) %>% 
  dplyr::mutate(charger_id_2 = charger_id) %>% 
  dplyr::select(c(v_2, charger_id_2))

# ----- Merge the distance pairs with the two datasets just created
dist_unclass <- dist_unclass %>% 
  left_join(., chargers_1, by = "v_1") %>%
  left_join(., chargers_2, by = "v_2")

# ----- Arrange dataset
dist_unclass <- dist_unclass %>% 
  arrange(charger_id_1,
          dist,
          charger_id_2)

# ----- Create match ID indicating for each distance which two charging stations it measures distance between
dist_unclass <- dist_unclass %>% 
  mutate(match_id = paste(charger_id_1,
                          charger_id_2,
                          sep = "_"))

# ----- Remove unnecessary variables
dist_unclass <- dist_unclass %>% 
  dplyr::select(c(match_id,
                  dist))

# ----- Check our measures
min(dist_unclass$dist)
hist(dist_unclass$dist)

# ----- Check that the dataset has the right dimensions (= the result should be zero)
nrow(dist_unclass) - (nrow(chargers_1) * (nrow(chargers_2) - 1))
