# install and load nessesary files 
install.packages(c("maps","tidyverse","rgeos","tidyverse","maptools","scales",
                   "rgdal","raster","sf","wesanderson","shiny","mapproj","viridis"))
install.packages("gpclib", type="source")
library(maps)
library(tidyverse)
library(rgeos)
library(maptools)
library(gpclib)  
library(scales)
library(rgdal)
library(raster)
library(sf)
library(wesanderson)
library(shiny)
library(viridis)
 
#read in voter info
temp <- tempfile()
download.file("https://s3.amazonaws.com/dl.ncsbe.gov/data/ncvoter_Statewide.zip", temp)
nc_info <- read_tsv(unz(temp, "ncvoter_Statewide.txt"))
unlink(temp)

# tidy voter information 
nc_info_tidy <- select(nc_info, voter_reg_num, ncid, status_cd, zip_code, race_code, ethnic_code, party_cd, gender_code, birth_age, confidential_ind) %>%
  mutate(unique_id = paste(voter_reg_num, ncid, sep = "")) %>%
  select(-voter_reg_num, -ncid)

#read in voter history  
temp_his <- tempfile()
download.file("https://s3.amazonaws.com/dl.ncsbe.gov/data/ncvhis_Statewide.zip", temp_his)
nc_his <- read_tsv(unz(temp_his, "ncvhis_Statewide.txt"))
unlink(temp_his)

# tidy voter history 
nc_his_tidy <- select(nc_his, county_id, voter_reg_num, ncid, election_lbl, voting_method, voted_party_cd, pct_label) %>%
  mutate(unique_id = paste(voter_reg_num, ncid, sep = "")) %>%
  select(-voter_reg_num, -ncid)

# joining history and information
nc_voter <- inner_join(nc_his_tidy, nc_info_tidy, by = "unique_id")

# cleaning birth age column
nc_voter <- mutate(nc_voter, birth_age = as.integer(birth_age)) %>%
  filter(birth_age <= 100)

# subset the data
set.seed(0)
nc_sample <- sample_n(nc_voter, size=500000, replace = FALSE)


# tidy data for individual maps 
nc_demo <- nc_sample %>% 
  rename(PREC_ID = pct_label)%>%
  group_by(PREC_ID,race_code) %>%
  summarise(n = n()) %>% 
  mutate(per = 100*n/sum(n))

nc_his <- nc_sample %>% 
  rename(PREC_ID = pct_label)%>%
  group_by(PREC_ID,ethnic_code) %>%
  summarise(n = n()) %>% 
  mutate(per = 100*n/sum(n)) %>% 
  filter(ethnic_code == "HL") 

nc_party <- nc_sample %>% 
  filter(party_cd %in% c("REP","DEM")) %>% 
  rename(PREC_ID = pct_label)%>%
  group_by(PREC_ID,party_cd) %>%
  summarise(n = n()) %>% 
  mutate(per = 100*n/sum(n)) %>% 
  filter(party_cd == "DEM")

nc_density <- nc_sample %>% 
  rename(PREC_ID = pct_label)%>%
  group_by(PREC_ID) %>%
  summarise(n = n())

nc_age <- nc_sample %>% 
  rename(PREC_ID = pct_label)%>%
  group_by(PREC_ID) %>%
  summarise(avg = mean(birth_age)) 


#load shapefile for NC 
nc_shape <- readShapeSpatial("Precincts.shp") 
#plot(na_shape) --  VERIFY IT LOADED PROPERLY 
nc_shp_df <- fortify(nc_shape, region = "PREC_ID") #turn inter into a dataframe 

# make pretty theme for map 
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Avenir", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

# create demographics from maps 
density_map <- ggplot() + geom_map(data = nc_density, aes(map_id = PREC_ID, fill = n), 
                                   map = nc_shp_df) + expand_limits(x = nc_shp_df$long, y = nc_shp_df$lat) + 
  theme_map() +
  labs(x = NULL, 
       y = NULL, 
       title = "North Carolina's regional demographics", 
       subtitle = "Voter density, 2016 General Election") + 
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "magma", 
    direction = -1,
    name = "Voter density",
    # here we use guide_colourbar because it is still a continuous scale
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      # some shifting around
      title.hjust = 0.5,
      label.hjust = 0.5
    ))

# saves map to image (png)
ggsave(plot = density_map, "density_map.png", width = 8, height = 5, units = "in")



party_map <- ggplot() + geom_map(data = nc_party, aes(map_id = PREC_ID, fill = per), 
                                 map = nc_shp_df) + expand_limits(x = nc_shp_df$long, y = nc_shp_df$lat) + 
  theme_map() + labs(x = NULL, 
                     y = NULL, 
                     title = "North Carolina's regional demographics", 
                     subtitle = "Voter party, 2016 General Election") + 
  theme(legend.position = "bottom") +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue", midpoint = 50,
    name = "Voter party percentage",
    # here we use guide_colourbar because it is still a continuous scale
    guide = guide_colorbar(
      label = FALSE,
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      # some shifting around
      title.hjust = 0.5,
      label.hjust = 0.5
    ))

ggsave(plot = party_map, "party_map.png", width = 8, height = 5, units = "in")



age_map <- ggplot() + geom_map(data = nc_age, aes(map_id = PREC_ID, fill = avg), 
                                   map = nc_shp_df) + expand_limits(x = nc_shp_df$long, y = nc_shp_df$lat) + 
  theme_map() +
  labs(x = NULL, 
       y = NULL, 
       title = "North Carolina's regional demographics", 
       subtitle = "Voter age, 2016 General Election") + 
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "viridis", 
    direction = -1,
    name = "Voter age",
    # here we use guide_colourbar because it is still a continuous scale
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      # some shifting around
      title.hjust = 0.5,
      label.hjust = 0.5
    ))

ggsave(plot = age_map, "age_map.png", width = 8, height = 5, units = "in")

  
nc_demoW <- nc_demo %>% 
  filter(race_code == "W")
white_map <- ggplot() + geom_map(data = nc_demoW, aes(map_id = PREC_ID, fill = per), 
                         map = nc_shp_df) + expand_limits(x = nc_shp_df$long, y = nc_shp_df$lat) + 
  theme_map() +
  labs(x = NULL, 
       y = NULL, 
       title = "North Carolina's regional demographics", 
       subtitle = "Voter race, 2016 General Election") + 
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "inferno", 
    direction = -1,
    name = "White Voters (%)",
    # here we use guide_colourbar because it is still a continuous scale
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      # some shifting around
      title.hjust = 0.5,
      label.hjust = 0.5
    ))

ggsave(plot = white_map, "white_map.png", width = 8, height = 5, units = "in")


nc_demoB <- nc_demo %>% 
  filter(race_code == "B") 
black_map <- ggplot() + geom_map(data = nc_demoB, aes(map_id = PREC_ID, fill = per), 
                         map = nc_shp_df) + expand_limits(x = nc_shp_df$long, y = nc_shp_df$lat) + 
  theme_map() +
  labs(x = NULL, 
       y = NULL, 
       title = "North Carolina's regional demographics", 
       subtitle = "Voter race, 2016 General Election") + 
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "plasma", 
    direction = -1,
    name = "Black Voters (%)",
    # here we use guide_colourbar because it is still a continuous scale
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      # some shifting around
      title.hjust = 0.5,
      label.hjust = 0.5
    ))

ggsave(plot = black_map, "black_map.png", width = 8, height = 5, units = "in")




nc_demoM <- nc_demo %>% 
  filter(race_code == "M") 
mixed_map <- ggplot() + geom_map(data = nc_demoM, aes(map_id = PREC_ID, fill = per), 
                         map = nc_shp_df) + expand_limits(x = nc_shp_df$long, y = nc_shp_df$lat) + 
  theme_map() +
  labs(x = NULL, 
       y = NULL, 
       title = "North Carolina's regional demographics", 
       subtitle = "Voter race, 2016 General Election") + 
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "viridis", 
    direction = -1,
    name = "Multiracial Voters (%)",
    # here we use guide_colourbar because it is still a continuous scale
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      # some shifting around
      title.hjust = 0.5,
      label.hjust = 0.5
    )) 

ggsave(plot = mixed_map, "mixed_map.png", width = 8, height = 5, units = "in")



nc_demoA <- nc_demo %>% 
  filter(race_code == "A") 
asian_map <- ggplot() + geom_map(data = nc_demoA, aes(map_id = PREC_ID, fill = per), 
                         map = nc_shp_df) + expand_limits(x = nc_shp_df$long, y = nc_shp_df$lat) + 
  theme_map() +
  labs(x = NULL, 
       y = NULL, 
       title = "North Carolina's regional demographics", 
       subtitle = "Voter race, 2016 General Election") + 
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "magma", 
    direction = -1,
    name = "Asian Voters (%)",
    # here we use guide_colourbar because it is still a continuous scale
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      # some shifting around
      title.hjust = 0.5,
      label.hjust = 0.5
    )) 

ggsave(plot = asian_map, "asian_map.png", width = 8, height = 5, units = "in")


hispanic_map <- ggplot() + geom_map(data = nc_his, aes(map_id = PREC_ID, fill = per), 
                                 map = nc_shp_df) + expand_limits(x = nc_shp_df$long, y = nc_shp_df$lat) + 
  theme_map() +
  labs(x = NULL, 
       y = NULL, 
       title = "North Carolina's regional demographics", 
       subtitle = "Voter ethnicity, 2016 General Election") + 
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "magma", 
    direction = -1,
    name = "Hispanic Voters (%)",
    # here we use guide_colourbar because it is still a continuous scale
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      # some shifting around
      title.hjust = 0.5,
      label.hjust = 0.5
    )) 

ggsave(plot = hispanic_map, "hispanic_map.png", width = 8, height = 5, units = "in")






# save file to image for future use 
save.image(file = "ncVis.Rdata")
