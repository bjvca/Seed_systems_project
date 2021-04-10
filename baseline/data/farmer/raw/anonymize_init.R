#read in raw data as expored from ONA
#execute from /NWO seed system devt Uganda proposal development/baseline/data/farmer/raw/

rm(list=ls())
set.seed(10042021)  #today's date



path <- getwd()

### reads in raw data (not public)
farmers <-read.csv(paste(path,"baseline_farmer_2021_04_10_05_17_19_298170-2.csv", sep="/"))

## drop location, names and contact details
to_drop <- c(
"start",                                        
"end",                                          
"deviceid",                                     
"simserial",                                    
"phonenumber",                                  
"subscriberid",                                 
"enumerator",                                   
"district",                                     
"sub",                                          
"village",                                                              
"Check2.check.consent",                         
"Check2.check.sig_consent",                     
"Check2.check.maize.q5",                        
 "Check2.check.maize.phone",                     
 "Check2.check.maize.phone2",   
 "Check2.check.maize.gps",                       
"Check2.check.maize._gps_latitude",             
"Check2.check.maize._gps_longitude",            
"Check2.check.maize._gps_altitude",             
"Check2.check.maize._gps_precision",            
"meta.instanceID",                              
"X_id",                                         
"X_uuid",                                       
"X_submission_time" ,                           
"X_date_modified",                              
"X_tags",                                      
"X_notes",                                      
"X_version",                                    
"X_duration",                                   
"X_submitted_by",                               
"X_total_media",                                
"X_media_count",                                
"X_media_all_received",                         
"X_xform_id")            
 farmers <- farmers[ , !(names(farmers) %in% to_drop)]
 
#remove ID info for input dealers in rating module
to_drop <- c( paste(paste("Check2.check.maize.clear.shops",1:max(as.numeric(farmers$Check2.check.maize.clear.shops_count), na.rm=T),sep = "."),"calc_biz",sep=".."), 
paste(paste("Check2.check.maize.clear.shops",1:max(as.numeric(farmers$Check2.check.maize.clear.shops_count), na.rm=T),sep = "."),"dealer_name",sep=".."), 
paste(paste("Check2.check.maize.clear.shops",1:max(as.numeric(farmers$Check2.check.maize.clear.shops_count), na.rm=T),sep = "."),"nickname",sep=".."), 
paste(paste("Check2.check.maize.clear.shops",1:max(as.numeric(farmers$Check2.check.maize.clear.shops_count), na.rm=T),sep = "."),"market_name",sep=".."), 
paste(paste("Check2.check.maize.clear.shops",1:max(as.numeric(farmers$Check2.check.maize.clear.shops_count), na.rm=T),sep = "."),"eye",sep=".."), 
paste(paste("Check2.check.maize.clear.shops",1:max(as.numeric(farmers$Check2.check.maize.clear.shops_count), na.rm=T),sep = "."),"pic",sep=".."))
farmers <- farmers[ , !(names(farmers) %in% to_drop)]

#remove names for plot in random plot selection module
to_drop <-  c( paste(paste("Check2.check.maize.plot",1:max(as.numeric(farmers$Check2.check.maize.plot_count), na.rm=T),sep = "."),"plot_num",sep=".."), 
paste(paste("Check2.check.maize.plot",1:max(as.numeric(farmers$Check2.check.maize.plot_count), na.rm=T),sep = "."),"q28",sep=".."),
"Check2.check.maize.plot_calc1","Check2.check.maize.plot_calc2","Check2.check.maize.plot_select","Check2.check.maize.plot_select_name","Check2.check.maize.order","Check2.check.maize.clear.placeholder"
)
farmers <- farmers[ , !(names(farmers) %in% to_drop)]

#write public dataset
path <- strsplit(path, "/raw")[[1]]
write.csv(farmers,paste(path,"public/baseline_farmers.csv", sep="/"), row.names=FALSE)






