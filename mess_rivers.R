
library(sf)
library(tidyverse)
library(magrittr)
library(leaflet)



# The input file geodatabase
  gdb <- st_read("/dbfs/FileStore/WSX_HGray/WessexRiskAttributedOLF.gdb", quiet = TRUE)

  LA <- read_sf("/dbfs/mnt/base/unrestricted/source_ons_open_geography_portal/dataset_local_authority_districts_bdy_uk_bfe/format_SHP_local_authority_districts_bdy_uk_bfe/LATEST_local_authority_districts_bdy_uk_bfe/local_auth_dist_bdy_uk_bfe.shp") %>% 
  filter(LAD24NM == "Dorset") %>% 
    st_transform(4326)

  # Enter catchment here
Catchments <- c("Avon Bristol Urban")

catch <- read_sf("/dbfs/mnt/lab/unrestricted/harry.gray@environment-agency.gov.uk/Interim_WFD_2022.shp")# Catchment shapefiles
CAT <- catch



# Load Detailed river network & crop to Dorset
DRN <- read_sf("/dbfs/mnt/lab/unrestricted/harry.gray@environment-agency.gov.uk/DRN/DRN_Merged_MCAT.shp")
DRN <- DRN[LA,]

# Create a wb wide score and transform
ALERT <- gdb %>%
          group_by(WATERBODY_NAME) %>%
          mutate(Catch_Overall_Risk = round(mean(CatchmentRiskScore),1)) %>% 
          ungroup() %>% 
          rename(WB_NAME = WATERBODY_NAME) %>% 
          st_transform(4326)

# Crop to catchment & transform to WGS84
  
  ALERT_CAT <- ALERT[LA,]
 
# Assign wb risk score to catchment wbs.
  ALERT_WB <- ALERT_CAT %>% select(2,23) %>% 
                             st_drop_geometry() %>%  
                             inner_join(CAT, by = "WB_NAME") %>% 
                             distinct() %>%     # We had a polygon for every linestring so make unique for each wb
                             st_as_sf() %>% 
                              st_transform(4326)
   
  PA <- read_sf("/dbfs/FileStore/WSX_HGray/Potential_Activities.gpkg") %>% 
  st_transform(4326)

PAR <- PA %>% 
  filter(!(river_activities %in% c("","NA")))
    


  ALERT_WB <- ALERT_WB %>% st_intersection(LA)
  
  DRN <- DRN %>% st_intersection(LA)
  #PAR_R <- PAR %>% filter((river_activities == ("Potential to reduce nutrient run-off through habitat creation or enhancement")))
  
  
  ggplot() +
  geom_sf(data = ALERT_WB, aes(fill = Catch_Overall_Risk)) +
  scale_fill_gradientn(colors = MetBrewer::met.brewer("Tam")) +
   geom_sf(data = DRN, fill = "steelblue", col = "steelblue")+
  geom_sf(data = PAR, fill = "green")+
    geom_sf(data=LA, fill = NA, size= 2)+
    
  theme_void()
  
  
  
  
  filtered_st <- PAR$river_activities[
    grepl("Increase or enhance riparian planting", PAR$river_activities, ignore.case = TRUE) & 
      grepl("Enhance priority river habitat", PAR$river_activities, ignore.case = TRUE) &
      grepl("Enhance priority river habitat - headwaters", PAR$river_activities, ignore.case = TRUE)
  ]
  
  
  PAR_R <- PAR %>% filter((river_activities %in% unique(filtered_strings)))
   
  PAR_RI <- PAR %>% filter((river_activities %in% unique(filtered_st)))
  
  ggplot() +
    geom_sf(data = ALERT_WB, aes(fill = Catch_Overall_Risk)) +
    scale_fill_gradientn(colors = MetBrewer::met.brewer("Tam")) +
    geom_sf(data = DRN, fill = "steelblue", col = "steelblue")+
    geom_sf(data = PAR_R, fill = "green", col="green")+
    geom_sf(data=LA, fill = NA, size= 5)+
    labs(title="Potential to reduce nutrient run-off through habitat creation or enhancement & \n Potential to improve river water quality and ecological condition")+
    theme_void()
  
  
  
  ggplot() +
    geom_sf(data = ALERT_WB, aes(fill = Catch_Overall_Risk)) +
    scale_fill_gradientn(colors = MetBrewer::met.brewer("Tam")) +
    geom_sf(data = DRN, fill = "steelblue", col = "steelblue")+
    geom_sf(data = PAR_RI, fill = "seagreen2", col="seagreen2")+
    geom_sf(data=LA, fill = NA, size= 5)+
    labs(title="Increase or enhance riparian plantin & \n Enhance priority river habitat - headwaters")+
    theme_void()
  