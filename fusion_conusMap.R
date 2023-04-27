library(dplyr)
library(ggplot2)
library(sf)
library(RColorBrewer)

 # read in the shapefile
rivers <- st_read('/Users/elisafriedmann/Library/CloudStorage/OneDrive-UniversityofMassachusetts/UMass/SNiP/hydroRivers_5plusOrder.shp')

#lakes
lakes <- st_read("/Users/elisafriedmann/Library/CloudStorage/OneDrive-UniversityofMassachusetts/UMass/SNiP/greatLakesShp/greatLakes.shp")

#read in data points
points <- read.csv('/Users/elisafriedmann/Downloads/fusionStats_2013-2023.csv')
points <- points %>%
  filter(lat < 58) 
print(head(points))
summary(points$percentFusionAquaLS2_Total)
print(table(points$bin_values))
points["bin_values"][points["bin_values"] == ''] <- NA
print(unique(points$bin_values))

points$bin_values[is.na(points$bin_values) & points$percentFusionAquaLS2_Total == 60850.000000] <- '500+'
p.sf <- st_as_sf(points, coords = c("long", "lat"), crs = 4326) 
p.sf
pointsSF <- st_write(p.sf, '/Users/elisafriedmann/Library/CloudStorage/OneDrive-UniversityofMassachusetts/UMass/SNiP/fusionStats_2013-2023.shp')

#WBD
wbd <- st_read("/Users/elisafriedmann/Downloads/drive-download-20230425T160459Z-001/wbdHUC02National.shp")
print(head(wbd))

resultsUSA <- st_intersection(rivers, wbd)
print(head(resultsUSA))


#quick results USA map
mapFP <- ggplot(resultsUSA) +
  geom_sf(aes(size=ORD_STRA))
mapFP
theme_set(theme_classic())
mapGW2 <- ggplot(p.sf) +
  geom_sf(data = resultsUSA, color = 'skyblue1') +
  geom_sf(data = lakes, colour = 'skyblue1', fill = 'skyblue1') +
  geom_sf(data = wbd, color = 'black', alpha = 0.0, size = 0.6) +
  geom_sf(data = p.sf, aes(colour=bin_values), size = 1.5) + #actual map, mapping the 'perc_GW' variable
  scale_color_manual(values = colorRampPalette(brewer.pal(9, "OrRd"))(12)[6:12])+
  lims(x = c(130, 60), y = c(25, 50)) +
  labs(color = 'Fusion Data Gain (%)')+
  theme(axis.title = element_text(size=26, face='bold'),
        axis.text = element_text(family="Futura-Medium", size=20))+ #axis text settings
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, 'cm'))+ #legend position settings
  theme(text = element_text(family = "Futura-Medium"), #legend text settings
        legend.title = element_text(face = "bold", size = 22),
        legend.text = element_text(family = "Futura-Medium", size = 18),
        plot.tag = element_text(size=26,
                                face='bold'))+
  xlab('')+
  ylab('')


mapGW2
#scale_colour_brewer(palette = "OrRd")+
#scale_fill_gradient2(name='% lost CO2 sourced from groundwater', low='grey',mid='#7ca982', high='#243e36', midpoint=50, 
#                     limits=c(0,100),guide = guide_colorbar(direction = "horizontal", title.position = "bottom"))+ #color scheme
#scale_colour_binned(guide = "bins", type = "viridis")+
#scale_colour_gradient2(low="yellow", mid = 'orange', high="red")
#  scale_colour_manual(values = c("darkred", "firebrick2", "tomato2", "chocolate", "orange", "yellow2")) +
#  scale_fill_binned(breaks = c(-10, 0, 1, 2, 1000, 5000, 10000))

#kinda orig
theme_set(theme_classic())

mapGW <- ggplot(results) +
  geom_sf(aes(fill=ORD_STRA), #actual map, mapping the 'perc_GW' variable
          color='blue',
          size=0.5) +
  labs(tag='A')+
  scale_fill_gradient2(name='% lost CO2 sourced from groundwater', low='white',mid='#7ca982', high='#243e36', midpoint=50, 
                       limits=c(0,100),guide = guide_colorbar(direction = "horizontal", title.position = "bottom"))+ #color scheme
  theme(axis.title = element_text(size=26, face='bold'),axis.text = element_text(family="Futura-Medium", size=20))+ #axis text settings
  theme(legend.position = c(0.2, 0.1),
        legend.key.size = unit(2, 'cm'))+ #legend position settings
  theme(text = element_text(family = "Futura-Medium"), #legend text settings
        legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(family = "Futura-Medium", size = 18),
        plot.tag = element_text(size=26,
                                face='bold'))+
  xlab('')+
  ylab('')

mapGW + geom_sf(data = p.sf, aes(size=percent),color = 'red', alpha = 0.8)
#Switch points and hydrography


