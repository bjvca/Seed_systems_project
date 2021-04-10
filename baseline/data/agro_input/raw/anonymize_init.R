#read in raw data as expored from ONA
#execute from /NWO seed system devt Uganda proposal development/baseline/data/agro_input/raw/

rm(list=ls())
set.seed(05112020)  #today's date

library(pracma)
library(sf)
library(leaflet)
library(leafpop)
library(dplyr)
library(clubSandwich)
library(stringr)
library(reshape2)

path <- getwd()

### reads in raw data (not public)
shops <- rbind(read.csv(paste(path,"Baseline_DealerXX_2020_09_27_03_06_54_713799.csv", sep="/")),read.csv(paste(path,"Baseline_DealerXXXX_2020_10_02_14_55_02_970765.csv", sep="/")))
moisture1 <- read.csv(paste(path,"Moisture_formX2_2020_10_04_07_57_19_019044.csv", sep="/"))[c("id", "reading", "exp",  "date_pack", "origin", "cert", "lot", "verif", "variety", "other_var","company")]
moisture2 <- read.csv(paste(path,"Moisture_formXX_2020_10_05_08_15_54_391127.csv", sep="/"))[c("id", "reading", "exp", "origin", "cert", "lot", "verif", "variety", "other_var","company")]
moisture2$date_pack <- "n/a" 
moisture2 <- moisture2[, c("id", "reading", "exp",  "date_pack", "origin", "cert", "lot", "verif", "variety", "other_var","company")]
moisture <- data.frame(rbind(moisture1,moisture2))


moisture$age <- difftime(strptime("01.10.2020", format = "%d.%m.%Y"),strptime(moisture$date_pack,format="%Y-%m-%d"),units="days")
moisture$age[moisture$date_pack=="n/a"] <- difftime(strptime("01.10.2020", format = "%d.%m.%Y"),strptime(moisture$exp[moisture$date_pack=="n/a"] ,format="%Y-%m-%d"),units="days")+180

#this is for a graph I made for a presentation showing how moisture increases with age
#moisture <- subset(moisture, age > 10)
#moisture <- subset(moisture, age < 110)
#ggplot(data=moisture,aes(age, reading)) +
#  geom_point() +
#  geom_smooth(method = "lm",se = FALSE ) + geom_hline(yintercept=14, color = "red")

### manually correct spelling of IDs to merge seed testing to shop survey data
shops$maize.owner.agree.id <- as.character(shops$maize.owner.agree.id)
moisture$id <- as.character(moisture$id)
moisture$id[moisture$id == " 22 Waswa"] <- "23 Waswa"
moisture$id[moisture$id == "15 Zebulon"] <- "15 Zebuloni"
moisture$id[moisture$id == "20 Nabatu"] <- "20 Nambafu"
moisture$id[moisture$id == "21Answer"] <- "21 No answer"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "21 Ntuyo  "] <- "21 Ntuyo"
moisture$id[moisture$id == "22 Mugoda"] <- "22 Mukodha"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "22 Mutesi  "] <- "22 Mutesi"
moisture$id[moisture$id == "22 Nakayima"] <- "22 Nakaima"
moisture$id[moisture$id == "22 Namugabwa"] <- "22 Namugabwe"
moisture$id[moisture$id == "22 Nyemera"] <- "22 Nyamera"
moisture$id[moisture$id == "23 Bsdajabaka"] <- "23 Basajabaka"
moisture$id[moisture$id == "23 Dhizala"] <- "23 Dhizaala"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "23 Kisige  "] <- "23 Kisige"
moisture$id[moisture$id == "23 Nafula"] <- "23 Nanfula"
moisture$id[moisture$id == "25 Kauli"] <- "25 Kawuli "
moisture$id[moisture$id == "25 Nentunze"] <- "25 Netunze"
moisture$id[moisture$id == "25Nandala"] <- "25 Nandera"
moisture$id[moisture$id == "26 Babirye"] <- "26 Barbirye"
moisture$id[moisture$id == "26 Nakawogo"] <- "26 Nakawago"
moisture$id[moisture$id == "26 Namulondo"] <- "26 Namulonda"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "24 Kageye  Faishali"] <- "24 Kageye"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "26 Nangobi  "] <- "26 Nangobi" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "26 Tefiiro  "] <- "26 Tefiiro" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "26 Tefiro"] <- "26 Tefilo" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "27 Ssetimba"] <- "27 Setimba" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "28 Kaudha"] <- "28 Khauda"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "32 Biribo"] <- "32 Bilibo" 
moisture$id[moisture$id == "32Pandaya"] <- "32 Pendaya"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "36 Fazali"] <- "36 Fazili"   
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "36 Kakayi  "] <- "36 Kakayi"    
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "36 Murwanyi"] <- "36 Mulwanyi"  
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "36 Nakirima  "] <- "36 Nakirima"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "38 Waiswa  "] <- "38 Waiswa"
moisture$id[moisture$id == "39 Siidamwebyo"] <- "39 Sidamwebyo"
moisture$id[moisture$id == "40 Atiibwa"] <- "40 Atibwa"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "40 Kisuubi  "] <- "40 Kisubi"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "40 Naigaga"] <- "40 Naigsga" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "42 Osunye"] <-  "42 Osunyo" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "45 Wampende  "] <-  "45 Wampande"    
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "46 Kirya "] <-  "46 Kirya"   
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "46 Namususwa  "] <-  "46 Namususwa" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "46 Wanama"] <-  "46 Wanawa" 
moisture$id[moisture$id == "46Nabwere" ] <- "46 Nabwire"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "48 Kanaabi  hardware"] <-  "48 Kanaabi"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "50 Iguude"] <-  "50 Iguube" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "50 Kharende  "] <-  "50 Khalende"  
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "50 Mpaunka  "] <-  "50 Mpanuka"   
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "55 Byekwaso  "] <-   "55 Byekwaso" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "21 Mwirugazu  "] <-   "21 Mwelugazu"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "33 Adikini"] <-  "22 Adikini" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "32 Nakasiko"] <-  "22 Nakasiko" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "22 Wagubi  "] <-  "22 Wagubi"  
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "32 Kiraire"] <-  "23 Kiraire"   
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "25 Mukose"] <- "23 Mukose" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "24 Nandigobe"] <- "24 Nandigobo"  
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "24 Talima Irene (Birombo)"] <- "24 Talima" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "18 Nabirye"] <- "50 Nabirye" 

## these have two records in the shops data - these can not be matched... these are probably the two unknowns below
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "19 Masinde"] <- NA
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "22 Muwanguzi"] <- NA
 
#merge in moisture data
shops <- merge(shops,moisture, by.x="maize.owner.agree.id",by.y="id", all.x=T)

### these are records in the testing data that can not be merged:
#shops$maize.owner.agree.id[is.na(shops[,2])]
# [1] "19 Buyenze"     "21 Kabulandala" "27 Sharifa"     "30 Magada"     
# [5] "33 Awali"       "34 Awali"       "42 Nabayo"      "54 Alex "      
# [9] "Unknown"        "Unknown "
 
#create shop_ID

shops$shop_ID <- paste("AD",rownames(shops), sep="_")
shops$shop_ID <- factor(shops$shop_ID)

#Categorizing different input dealers into catchment areas
shops$catchmentID <- NA
counter <- 1

for (shop_1 in names(table(shops$shop_ID))) {

shops$catchmentID[shops$shop_ID == shop_1] <- counter


for (shop_2 in names(table(shops$shop_ID))) {
### key parameter is chosen here: distance to define a catchment area. Here we assume that if shops are less then 5 km apart, they serve the same catchment area
if ( haversine(c(shops$maize.owner.agree._gps_latitude[shops$shop_ID == shop_1] ,shops$maize.owner.agree._gps_longitude[shops$shop_ID == shop_1]),c(shops$maize.owner.agree._gps_latitude[shops$shop_ID == shop_2],shops$maize.owner.agree._gps_longitude[shops$shop_ID == shop_2])) < 2.5) {
if (is.na(shops$catchmentID[shops$shop_ID == shop_2])) {  ## if the shop has not been allocated to a catcchment area yet, create a new one
 shops$catchmentID[shops$shop_ID == shop_2] <- counter
} else {  ## if the shop is already part of a catchment area
## change ID of all shops in catchement area to a new catchment area
 shops$catchmentID[shops$catchmentID == shops$catchmentID[shops$shop_ID == shop_1]]  <- shops$catchmentID[shops$shop_ID == shop_2] 
}

}
}
counter <- counter + 1
}
dim(table(shops$catchmentID))

# reorder catchement ID factor
 	i_catch <- 1
	for (catch in names(table(shops$catchmentID))) {

		shops$catchID[shops$catchmentID == catch] <- i_catch
		i_catch <- i_catch + 1
	}
shops$catchmentID <- NULL
#link to pictures 
shops$maize.owner.agree.q13 <- sub('.*\\/', '',shops$maize.owner.agree.q13 )
shops$maize.owner.agree.q13[shops$maize.owner.agree.q13 == 'a'] <- "no_picture.png"
shops$maize.owner.agree.q13 <-  gsub("jpg","png",shops$maize.owner.agree.q13) ### jpegs were converted to pngs
shops$images <- paste(paste(path,"pictures/converted/resized", sep="/"),shops$maize.owner.agree.q13, sep="/")

### randomization - 4 treatment cells for catchment level interventions
treats <- data.frame(names(table(shops$catchID)),sample(rep(1:4, length=length(table(shops$catchID)))))
names(treats) <- c("catchID", "treat")
shops <- merge(shops,treats, by="catchID")

table(shops$treat)

shops$training <- FALSE
shops$clearing <- FALSE

shops$training[shops$treat %in% c(1,2)] <- TRUE
shops$clearing[shops$treat %in% c(2,4)] <- TRUE

shops$farmer <- NA

shops$farmer[shops$treat == 1] <- sample(rep(c("TRUE","FALSE"), length.out=length(shops$farmer[shops$treat == 1])))
shops$farmer[shops$treat == 2] <- sample(rep(c("TRUE","FALSE"), length.out=length(shops$farmer[shops$treat == 2])))
shops$farmer[shops$treat == 3] <- sample(rep(c("TRUE","FALSE"), length.out=length(shops$farmer[shops$treat == 3])))
shops$farmer[shops$treat == 4] <- sample(rep(c("TRUE","FALSE"), length.out=length(shops$farmer[shops$treat == 4])))

#### prepare sampling list for farmer questionnaire
farmers_list <- merge(shops[ !(names(shops) %in% c("district","sub"))], read.csv(paste(path,"villages_edited_final.csv", sep="/"))[c("shop_ID","district","sub","sampling_village")], by="shop_ID")
### 10 farmers in each village
farmers_list <- farmers_list[rep(seq_len(nrow(farmers_list)), each = 10), ]
#generate farmer ID
#first reset the rownames
rownames(farmers_list) <- NULL

farmers_list$farmer_ID <- paste("F",as.numeric(rownames(farmers_list)),sep="_")

farmers_list[c("district","sub","parish","sampling_village", "catchID", "farmer_ID")]

#by catchment area, give me names of all input dealers
store_shops <- array(dim=c(length(table(shops$catchID)),2+18*8))
for (i in 1:length(table(shops$catchID))) {
#print(c(i, names(table(shops$catchID))[i])) #catchment ID
store_shops[i,1] <-i
store_shops[i,2] <- length(shops$shop_ID[shops$catchID==i])  # number of shops in this catchment ID

#print(shops$shop_ID[shops$catchID==i])
#print(shops$maize.owner.agree.q13[shops$catchID==i] )
for (j in 1:length(shops$shop_ID[shops$catchID==i])) {
store_shops[i,j+2] <- as.character(shops$shop_ID[shops$catchID==i])[j] ##ID
store_shops[i,j+2+18] <- as.character(shops$maize.owner.agree.q13[shops$catchID==i])[j] ##image
store_shops[i,j+2+18*2] <-  as.character(shops$maize.owner.agree.biz_name[shops$catchID==i])[j] #name shop
store_shops[i,j+2+18*3] <-  as.character(shops$maize.owner.agree.family_name[shops$catchID==i])[j] #name owner
store_shops[i,j+2+18*4] <-  as.character(shops$maize.owner.agree.dealer_name[shops$catchID==i])[j] #name interviewee
store_shops[i,j+2+18*5] <-  as.character(shops$maize.owner.agree.nickname[shops$catchID==i])[j] #nick name interviewee
store_shops[i,j+2+18*6] <-  as.character(shops$maize.owner.agree.market_name[shops$catchID==i])[j] ##location
store_shops[i,j+2+18*7] <-  as.character(shops$maize.owner.agree.eye[shops$catchID==i])[j] ##description
}
#line <- cbind(i,length(shops$shop_ID[shops$catchID==i]),as.character(shops$shop_ID[shops$catchID==i]),shops$maize.owner.agree.q13[shops$catchID==i])

}
store_shops <- data.frame(store_shops)
names(store_shops) <- c("catchID", "nr_shops_in_catch",paste("ID_shop",seq(1:18), sep="_"), paste("image_shop", seq(1:18),sep="_"), paste("name_shop", seq(1:18),sep="_"), paste("owner_name_shop", seq(1:18),sep="_"), paste("name_person_interviewed", seq(1:18),sep="_"), paste("nickname_person_interviewed", seq(1:18),sep="_"), paste("location_shop", seq(1:18),sep="_"), paste("description_shop", seq(1:18),sep="_"))

## export for charles:
ODK_imp <- shops[c("catchID", "shop_ID","maize.owner.agree.biz_name", "maize.owner.agree.q13","maize.owner.agree.family_name", "maize.owner.agree.dealer_name","maize.owner.agree.nickname","maize.owner.agree.market_name","maize.owner.agree.eye")]
## remove all trailing and leading spaces - this gives issues in ODK
ODK_imp <- data.frame(lapply( ODK_imp, function(x)  trimws(x, which = "both")))
ODK_imp <- data.frame(lapply( ODK_imp,  function(x) str_replace_all(x, "[\r\n]" , " ")))
ODK_imp <- data.frame(lapply( ODK_imp,  function(x) str_replace_all(x, "\\s+" , " ")))
write.csv(ODK_imp,file="ODK_imp.csv",row.names=FALSE)

test <- merge(farmers_list,store_shops, by="catchID")
test <- test[ c("farmer_ID","district","sub","parish","sampling_village","training","clearing","farmer","catchID", "nr_shops_in_catch",paste("ID_shop",seq(1:18), sep="_"), paste("image_shop", seq(1:18),sep="_"), paste("name_shop", seq(1:18),sep="_"), paste("owner_name_shop", seq(1:18),sep="_"), paste("name_person_interviewed", seq(1:18),sep="_"), paste("nickname_person_interviewed", seq(1:18),sep="_"), paste("location_shop", seq(1:18),sep="_"), paste("description_shop", seq(1:18),sep="_"))]
test <- data.frame(lapply( test, function(x)  trimws(x, which = "both")))
test <- data.frame(lapply( test,  function(x) str_replace_all(x, "[\r\n]" , " ")))
test <- data.frame(lapply( test,  function(x) str_replace_all(x, "\\s+" , " ")))
write.csv(test,file="to_upload.csv", row.names=FALSE)

test2 <- merge(farmers_list,store_shops, by="catchID")
test2 <- test2[ c("farmer_ID",paste("ID_shop",seq(1:18), sep="_"))]
test2 <- data.frame(lapply( test2, function(x)  trimws(x, which = "both")))
test2 <- data.frame(lapply( test2,  function(x) str_replace_all(x, "[\r\n]" , " ")))
test2 <- data.frame(lapply( test2,  function(x) str_replace_all(x, "\\s+" , " ")))
#data needs to be in long form

test2 <- melt(test2, id.vars = c("farmer_ID"))
test2$variable <- NULL
names(test2) <- c("farmer_ID","shop_ID")
test2 <- subset(test2, !is.na(shop_ID))
test2$shop_name <- paste("shop",1:nrow(test2), sep = "_")

write.csv(test2,file="matcher_file.csv", row.names=FALSE)

### make a map with catchment ID coloring and pictures (not public)
pal <- colorFactor(
  palette = 'Dark2',
  domain = shops$catchID
)




m <- leaflet() %>% setView(lat = 0.65, lng = 33.62, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=shops, lng=~maize.owner.agree._gps_longitude, lat=~maize.owner.agree._gps_latitude,radius= 8, color=~pal(catchID), popup = ~as.character(catchID), group = "X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography'))  %>%  addPopupImages(  shops$images, width=137, height =200, group = "X_uuid")


library(htmlwidgets)
saveWidget(m, file="map_input_dealers.html")


#prepare data for public release
##remove GPS coordinates
## remove metadata
#note: use "maize.owner.agree.id" to link records to seed quality test data (moisture, packageing, ...)


to_drop <- c("start","end","deviceid","simserial", "phonenumber", "subscriberid", "start.geopoint","X_start.geopoint_latitude", "X_start.geopoint_longitude","X_start.geopoint_altitude", "X_start.geopoint_precision","maize.owner.agree.eye","maize.owner.agree.id", "meta.instanceID","X_id", "X_uuid","X_submission_time", "X_tags","X_notes","X_version", "X_duration", "X_submitted_by", "X_total_media", "X_media_count","X_media_all_received", "X_xform_id" )   
 
shops <- shops[ , !(names(shops) %in% to_drop)]

##remove names and other data that can be used to id
to_drop <- c("maize.owner.agree.gps","maize.owner.agree._gps_longitude","maize.owner.agree._gps_latitude") 
shops <- shops[ , !(names(shops) %in% to_drop)]

#this links to pictures
to_drop <- c("maize.owner.agree.q13","images")
shops <- shops[ , !(names(shops) %in% to_drop)]


#### create list of villages to be included
## step 1: make sure there are no villages that belong to 2 or more catchment areas

# get proper district and subcounty names from edited villages lists (ex-post)

shops <- merge(shops[ !(names(shops) %in% c("district","sub"))], read.csv(paste(path,"villages_edited_final.csv", sep="/"))[c("shop_ID","district","sub","sampling_village")], by="shop_ID")

write.csv(shops[c("catchID","district","sub","parish","village","shop_ID","maize.owner.agree.catch_area.Village1", "maize.owner.agree.catch_area.Village2" , "maize.owner.agree.catch_area.Village3")], file="villages.csv")

## write file for wilberfoce to organize trainings:
wilber <- subset(shops, training == TRUE)
write.csv(wilber[c("district","sub","parish","village","maize.owner.agree.dealer_name","maize.owner.agree.surname","maize.owner.agree.nickname","maize.owner.agree.phone1","maize.owner.agree.phone2",	"maize.owner.agree.biz_name","maize.owner.agree.family_name",	"maize.owner.agree.market_name","enumerator","other_district","catchID"
)], file="wilber.csv")

## remove villages where most customers are - this needs to be used for sampling of households
to_drop <- c("maize.owner.agree.catch_area.Village1", "maize.owner.agree.catch_area.Village2" , "maize.owner.agree.catch_area.Village3")  
shops <- shops[ , !(names(shops) %in% to_drop)]

## drop location, names and contact details
to_drop <- c("parish","village" ,"maize.owner.agree.dealer_name", "maize.owner.agree.surname", "maize.owner.agree.nickname", "maize.owner.agree.phone1", "maize.owner.agree.phone2", "maize.owner.agree.biz_name", "maize.owner.agree.family_name", "maize.owner.agree.market_name","enumerator","other_district")
 shops <- shops[ , !(names(shops) %in% to_drop)]

##convert district names and subcounty names to numeric codes 
i_dist <- 1
farmers$distID <- NULL
farmers$subID <- NULL

for (dist in names(table(shops$district))) {
	print(dist)
	i_sub <- 1
	for (sub in names(table(shops$sub[shops$district==dist]))) {
		print(sub)

		shops$subID[shops$district==dist & shops$sub == sub] <- i_sub
		i_sub <- i_sub + 1
	}
shops$distID[shops$district==dist ] <- i_dist
i_dist <- i_dist + 1
}
 
### BUT subcounty names was not pre-populated in the ODK app, so enumerators often spell differently/wrong, so better just delete (or manually correct first)
to_drop <- c("subID","district","sub")
 shops <- shops[ , !(names(shops) %in% to_drop)]
 
  
path <- strsplit(path, "/raw")[[1]]
write.csv(shops,paste(path,"public/baseline_dealer.csv", sep="/"), row.names=FALSE)






