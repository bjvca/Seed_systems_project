# This program selects 5 random coordinates on a circle at 


shops <- read.csv("/home/bjvca/data/projects/PIMMVC/data/raw_non_public/RawData_Shops_ids.csv")[c("id.agro","hh.maize._gps_latitude","hh.maize._gps_longitude")]
names(shops) <- c("shopID","shop_lat","shop_long")

### we found that a few locations are taken at the hotel where enumerators stay - remove these
shops$shop_lat[shops$shop_lat  > 0.604217379 &
shops$shop_lat  < 0.613710477 &
shops$shop_long > 33.47659906 &
shops$shop_long < 33.4922358] <- NA

shops$shop_long[is.na(shops$shop_lat) &
shops$shop_long > 33.47659906 &
shops$shop_long < 33.4922358]   <- NA

shops <- subset(shops, !is.na(shop_long)) 
shops$shopID <- factor(shops$shopID)

saved <- data.frame(array(NA, dim=c(1,3)))
names(saved) <- c("shopID","lat","long")


for (j in 1:dim(shops)[1]) {
circle_1 <- array(NA,c(5,2))
circle_1 <- data.frame(circle_1)
r <- 0.01*2
names(circle_1) <- c("x","y")
for (i in 1:5) {
angel <- 2*pi*runif(1)
circle_1$y[i] <- shops$shop_long[j] + r*cos(angel)
circle_1$x[i] <- shops$shop_lat[j] + r*sin(angel)
}
r <- 0.01*4
circle_2 <- array(NA,c(5,2))
circle_2 <- data.frame(circle_2)
names(circle_2) <- c("x","y")
for (i in 1:5) {
angel <- 2*pi*runif(1)
circle_2$y[i] <- shops$shop_long[j]  + r*cos(angel)
circle_2$x[i] <- shops$shop_lat[j]  + r*sin(angel)
}

inter <- data.frame(cbind(shops$shopID[j],rbind(circle_1,circle_2)))
names(inter) <- c("shopID","lat","long")
saved <- rbind(saved,inter)
}
saved <- saved[2:dim(saved)[1],] 

library(leaflet)
pal <- colorFactor(
  palette = 'Dark2',
  domain = saved$shopID
)

m <- leaflet() %>% setView(lat = 0.65, lng = 33.62, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=saved, lng=~long, lat=~lat,radius= 1, color=~pal(shopID), popup = ~as.character(shopID))   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography'))

saved <- subset(saved,shopID=="AS030")
