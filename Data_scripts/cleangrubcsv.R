################ Cleaning up data ###########
library(tidyverse)

# Katie used this file to clean csv data

# 1. Create location csv for Grub box 

bulk = read.csv("/Users/katiezarada/Desktop/Oysters/Analyses/Data/Oyster_data/Grub/Grub_box_shell_bulk_weights.csv", header = TRUE)


locations = bulk %>% select(Site.1, Latitude, Longitude) %>% distinct()
colnames(locations) = c("Site", "Latitude", "Longitude")

write.csv(locations, "Grub_locations.csv")

#2. Clean up bulk data- remove locations and change site.1 to site
bulk = bulk[,-c(1,6,7)]
colnames(bulk)[3]= "Site"
write.csv(bulk, "Grub_oyster_size_weights.csv")


#3. Clean up surface count data = remove locations and change site.1 to site
surface.counts = read.csv("/Users/katiezarada/Desktop/Oysters/Analyses/Data/Oyster_data/Grub/Grub_box_surface_oyster_counts.csv", header = TRUE)
surface.counts = surface.counts[,-c(1,6,7)]
colnames(surface.counts)[4] = "Site"

write.csv(surface.counts, "Grub_surface_oyster_counts.csv")

