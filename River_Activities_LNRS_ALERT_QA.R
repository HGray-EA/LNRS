
library(sf)
library(tidyverse)
library(magrittr)
library(leaflet)

# Load Datasets

  # The input ALERT data
    gdb <- st_read("/dbfs/FileStore/WSX_HGray/WessexRiskAttributedOLF.gdb", quiet = TRUE)
  
  # Import and transform Local Authorities
    LA <- read_sf("/dbfs/mnt/base/unrestricted/source_ons_open_geography_portal/dataset_local_authority_districts_bdy_uk_bfe/format_SHP_local_authority_districts_bdy_uk_bfe/LATEST_local_authority_districts_bdy_uk_bfe/local_auth_dist_bdy_uk_bfe.shp") %>% 
    filter(LAD24NM == "Dorset") %>% 
      st_transform(4326)
  
  # Import WFD waterbodies
  CAT <- read_sf("/dbfs/mnt/lab/unrestricted/harry.gray@environment-agency.gov.uk/Interim_WFD_2022.shp")# Catchment shapefiles
  
  # Load Detailed river network & crop to Dorset
  DRN <- read_sf("/dbfs/mnt/lab/unrestricted/harry.gray@environment-agency.gov.uk/DRN/DRN_Merged_MCAT.shp")
  DRN %<>% st_intersection(LA)

  # Load Dorset LNRS Potential
  PA <- read_sf("/dbfs/FileStore/WSX_HGray/Potential_Activities.gpkg") %>% 
    st_transform(4326)
  
  # Chalk streams
  
  Chalk <-  read_sf("/dbfs/FileStore/WSX_HGray/Chalk_streams_Dorset.shp") %>% 
    st_intersection(LA)
  
# Create a waterbody wide score and transform
ALERT <- gdb %>%
          group_by(WATERBODY_NAME) %>%
          mutate(Catch_Overall_Risk = round(mean(CatchmentRiskScore),1)) %>% 
          ungroup() %>% 
          rename(WB_NAME = WATERBODY_NAME) %>% 
          st_transform(4326)

# Crop ALERT to Dorset LNRS
  
  ALERT_CAT <- ALERT[LA,]
 
# Assign wb risk score to catchment wbs.
  ALERT_WB <- ALERT_CAT %>% select(2,23) %>% 
                             st_drop_geometry() %>%  
                             inner_join(CAT, by = "WB_NAME") %>% 
                             distinct() %>%     # We had a polygon for every linestring so make unique for each wb
                             st_as_sf() %>% 
                             st_transform(4326)
  
# Crop to Local Authority  
  ALERT_WB <- ALERT_WB %>% 

# Transforms
  # Filter out polygons without river activities 
PAR <- PA %>% 
  filter(!(river_activities %in% c("","NA")))
    
  #PAR_R <- PAR %>% filter((river_activities == ("Potential to reduce nutrient run-off through habitat creation or enhancement")))
  
# Plot of all river activities LNRS
  ggplot() +
  geom_sf(data = DRN, fill = "steelblue", col = "steelblue")+
  geom_sf(data = PAR, fill = "green")+
  geom_sf(data=LA, fill = NA, size= 2)+
  labs(title = "All River Activities")+
  theme_void()
  
    filtered_water_quality <- PAR$river_activities[
        grepl("Potential to reduce nutrient run-off through habitat creation or enhancement", PAR$river_activities, ignore.case = TRUE) & 
          grepl("Potential to improve river water quality and ecological condition", PAR$river_activities, ignore.case = TRUE)
    ]
  
    filtered_habitat_planting <- PAR$river_activities[
      grepl("Increase or enhance riparian planting", PAR$river_activities, ignore.case = TRUE) & 
        grepl("Enhance priority river habitat", PAR$river_activities, ignore.case = TRUE) &
        grepl("Enhance priority river habitat - headwaters", PAR$river_activities, ignore.case = TRUE)
    ]
    
    PAR_R <- PAR %>%
          filter((river_activities %in% unique(filtered_water_quality)))
    
    PAR_RI <- PAR %>% 
      filter((river_activities %in% unique(filtered_habitat_planting)))
    

  # Plot comparing EA diffuse pollution modelling and water quality/run-off potential activities spatial distribution 
  ggplot() +
    geom_sf(data = ALERT_WB, aes(fill = Catch_Overall_Risk)) +
    scale_fill_gradientn(colors = MetBrewer::met.brewer("Tam")) +
    geom_sf(data = DRN, fill = "steelblue", col = "steelblue")+
    geom_sf(data = PAR_R, fill = "green", col="green")+
    geom_sf(data=LA, fill = NA, size= 5)+
    labs(title="Potential to reduce nutrient run-off through habitat creation or enhancement & \n Potential to improve river water quality and ecological condition")+
    theme_void()
  
  
 # DRN & riparian planting/ riv hab potential activities 
  ggplot() +
    geom_sf(data = DRN, fill = "steelblue", col = "steelblue")+
    geom_sf(data = PAR_RI, fill = "green", col="green")+
    geom_sf(data=LA, fill = NA, size= 5)+
    labs(title="Increase or enhance riparian plantin & \n Enhance priority river habitat")+
    theme_void()
  
# Chalk streams and riparian planting/ riv hab potential activities 
  ggplot() +
    geom_sf(data = DRN, aes(col = "riv")) +
    geom_sf(data = Chalk, aes(col = "chalk")) +
    geom_sf(data = PAR_RI, aes(col = "riv_a")) +
    geom_sf(data = LA, fill = NA, col = "black", size = 5) +
    scale_color_manual(values = c(chalk = "purple", 
                                  riv = "steelblue",
                                  riv_a = "green"),
                       labels = c(chalk = "Chalk Streams",
                                  riv = "Detailed River Network",
                                  riv_a = "River Activities"),
                       name = "Legend")+
    labs(title="Increase or enhance riparian plantin & \n Enhance priority river habitat")+
    theme_void()
  
  
  