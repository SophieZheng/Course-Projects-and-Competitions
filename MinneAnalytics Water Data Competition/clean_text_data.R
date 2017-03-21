#library(bigrquery)
library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

merge <- read.csv('sample_lake_parcel_merge_10000.csv')
#names(data)
#a<-na.omit(data$p_SCHOOL_DST)

#replace letters with ""
merge$p_SCHOOL_DST<-gsub("[a-zA-Z]+","",merge$p_SCHOOL_DST,perl=TRUE)
#replace ""/NA with "12345678"
nalength <- length(merge[is.na(merge$p_SCHOOL_DST),]$p_SCHOOL_DST)
merge[is.na(merge$p_SCHOOL_DST),]$p_SCHOOL_DST <- rep("12345678",nalength)
nospacelength <- length(merge[merge$p_SCHOOL_DST=="",]$p_SCHOOL_DST)
merge[merge$p_SCHOOL_DST=="",]$p_SCHOOL_DST <- rep("12345678",nospacelength)
sum(merge$p_SCHOOL_DST == "12345678") #57 is correct

#delete 0 (those start with 0)
merge$p_SCHOOL_DST <- as.numeric(merge$p_SCHOOL_DST)
merge$p_SCHOOL_DST <- as.character(merge$p_SCHOOL_DST)
l<-length(merge[merge$p_SCHOOL_DST=="12345678",]$p_SCHOOL_DST)
merge[merge$p_SCHOOL_DST=="12345678",]$p_SCHOOL_DST <- rep("null",l)

#groupby school district
counted <- merge %>% group_by(pw_MCES_Map_Code1,p_SCHOOL_DST) %>% summarise(counts=n())
school_lists <- filter(counted, counts==max(counts))


#---USE TYPE----
#u<-merge$p_USE1_DESC
#t<-regexpr("$res|Res|RES|Apartment|APARTMENT|apartment|Hous|hous|HOUS$", u, perl=TRUE)
#c<-regmatches(u, t)
#p<-grepl("res|Res|RES|Apartment|APARTMENT|apartment|Hous|hous|HOUS$", u, perl=TRUE)
resRows <- grep(".*res|Res|RES|Apartment|APARTMENT|apartment|Hous|hous|HOUS.*",merge$p_USE1_DESC)
comerRows <- grep("comm|Comm|COMM|commerc|Commerc|COMMERC", merge$p_USE1_DESC)
commonAreaRows <- grep(".*(No Value).*", merge$p_USE1_DESC[comerRows])
comerRows <- comerRows[-commonAreaRows]
indRows <- grep(".*industrial|Industrial|INDUSTRIAL.*",merge$p_USE1_DESC)
agriRows <- grep(".*agric|Agric|AGRIC.*",merge$p_USE1_DES)
excRows <- grep(".*Industr|Residen|Commerc.*",merge$p_USE1_DESC[agriRows])
agriRows <- agriRows[-excRows]

merge$p_USE_TYPE <- rep("others",dim(merge)[1])
merge$p_USE_TYPE[resRows] <- "residential"
merge$p_USE_TYPE[comerRows] <- "commericial"
merge$p_USE_TYPE[indRows] <- "industrial"
merge$p_USE_TYPE[agriRows] <- "agricultural"


#----DELETE DUPLICATES CODE TESTING-----
names(merge) #5,6,7,21
attr_del <- merge[,c(5,6,7,14,20)]
dim(attr_del)
#tf <- duplicated(attr_del)
dupRows <- which(duplicated(attr_del))
rm_dup <- merge[-dupRows,]

#---remove emv all 0 rows----
no_zeros <- rm_dup[(rm_dup$p_EMV_BLDG+rm_dup$p_EMV_LAND+rm_dup$p_EMV_TOTAL)!=0,]


#--- REMOVE DUPLICATES OF REAL DATA-----
library(data.table)
setwd('/Users/SophieZheng/Google Drive/MinneAnalytics-WaterPollution/Sophie-Data-Prep')
mergeData <- fread('yrs6-000000000000.csv')
nums = c(1:4)
for (num in nums){
  name = paste(paste("yrs6-00000000000",num,sep=""),'.csv',sep="")
  data <- fread(name)
  mergeData = rbind.fill(mergeData, data)
}
#project = 'waterdata-146300'
#sql = "SELECT * FROM [waterdata-146300:merge.lake_parcel_merge_6yrs]"
#merge6yrs = query_exec(sql, project = project)
attrs <- mergeData[,c(4,5,6,13,19)]
dupRows <- which(duplicated(attrs))
rmMergeData <- mergeData[-dupRows,]
clean <- rmMergeData[(rmMergeData$m_p_EMV_BLDG+rmMergeData$m_p_EMV_LAND+rmMergeData$m_p_EMV_TOTAL)!=0,]
write.csv(clean,"no_dup_no_EMV0_6yrs.csv")

#-clean school dist---
clean$m_p_SCHOOL_DST<-gsub("[a-zA-Z]+","",clean$m_p_SCHOOL_DST,perl=TRUE)
#replace ""/NA with "12345678"
nalength <- length(clean[is.na(clean$m_p_SCHOOL_DST),]$m_p_SCHOOL_DST)
clean[is.na(clean$m_p_SCHOOL_DST),]$m_p_SCHOOL_DST <- rep("12345678",nalength)
nospacelength <- length(clean[clean$m_p_SCHOOL_DST=="",]$m_p_SCHOOL_DST)
clean[clean$m_p_SCHOOL_DST=="",]$m_p_SCHOOL_DST <- rep("12345678",nospacelength)
sum(clean$m_p_SCHOOL_DST == "12345678") 

#delete 0 (those start with 0)
clean$m_p_SCHOOL_DST <- as.numeric(clean$m_p_SCHOOL_DST)
clean$m_p_SCHOOL_DST <- as.character(clean$m_p_SCHOOL_DST)
l<-length(clean[clean$m_p_SCHOOL_DST=="12345678",]$m_p_SCHOOL_DST)
clean[clean$m_p_SCHOOL_DST=="12345678",]$m_p_SCHOOL_DST <- rep("null",l)

#---use type----
resRows <- grep(".*res|Res|RES|Apartment|APARTMENT|apartment|Hous|hous|HOUS.*",clean$m_p_USE1_DESC)
comerRows <- grep("comm|Comm|COMM|commerc|Commerc|COMMERC", clean$m_p_USE1_DESC)
#commonAreaRows <- grep(".*(No Value).*", clean$m_p_USE1_DESC[comerRows])
#comerRows <- comerRows[-commonAreaRows]
indRows <- grep(".*industrial|Industrial|INDUSTRIAL.*",clean$m_p_USE1_DESC)
agriRows <- grep(".*agric|Agric|AGRIC.*",clean$m_p_USE1_DESC)
excRows <- grep(".*Industr|Residen|Commerc.*",clean$m_p_USE1_DESC[agriRows])
agriRows <- agriRows[-excRows]

clean$m_p_USE_TYPE <- rep("others",dim(clean)[1])
clean$m_p_USE_TYPE[resRows] <- "residential"
clean$m_p_USE_TYPE[comerRows] <- "commericial"
clean$m_p_USE_TYPE[indRows] <- "industrial"
clean$m_p_USE_TYPE[agriRows] <- "agricultural"
unique(clean)

write.csv(clean,'no_dup_no_EMV0_use_schooldist.csv')


#---group by data----
groupby_clean3 <- clean %>%
  filter(m_p_FIN_SQ_FT!=0) %>%
  group_by(m_p_PIN) %>% summarise(city = m_p_CITY[1],
                                          county_id = m_p_COUNTY_ID[1],
                                          dwell_type = m_p_DWELL_TYPE[1],
                                          emv_bldg = mean(m_p_EMV_BLDG,na.rm=TRUE),
                                          emv_land = mean(m_p_EMV_LAND,na.rm=TRUE),
                                          emv_total = mean(m_p_EMV_TOTAL,na.rm=TRUE),
                                          fin_sq_ft = mean(m_p_FIN_SQ_FT,na.rm=TRUE),
                                          green_acre = mean(m_p_GREEN_ACRE,na.rm=TRUE),
                                          homestead = m_p_HOMESTEAD[1],              
                                          home_style = m_p_HOME_STYLE[1],              
                                          num_units = m_p_NUM_UNITS[1],               
                                          parc_code = m_p_PARC_CODE[1],              
                                          p_pin = m_p_PIN[1],                     
                                          school_dst = m_p_SCHOOL_DST[1],              
                                          use1_desc = m_p_USE1_DESC[1],              
                                          wshd_dist = m_p_WSHD_DIST[1],               
                                          year_built = m_p_YEAR_BUILT[1],              
                                          age = mean(m_age,na.rm=TRUE),                      
                                          year = m_p_year[1],                    
                                          zip = m_p_ZIP[1],                     
                                          centroid_long = m_p_centroid_long[1],          
                                          centroid_lat = m_p_centroid_lat[1],            
                                          #mces_map_code1 = m_pw_MCES_Map_Code1[1],         
                                          pw_meters_to_monitor_site = mean(m_pw_meters_to_monitor_site,na.rm=TRUE),
                                          pw_meters_to_lake_edge = mean(m_pw_meters_to_lake_edge,na.rm=TRUE),    
                                          lake_name = m_w_lake_name[1],               
                                          major_water = m_w_major_watershed[1],        
                                          grade = mean(m_w_grade,na.rm=TRUE),                   
                                          phy = mean(m_w_phy,na.rm=TRUE),                     
                                          rec = mean(m_w_rec,na.rm=TRUE),                    
                                          depth = mean(m_w_depth,na.rm=TRUE),                   
                                          tp = mean(m_w_tp,na.rm=TRUE),                      
                                          l_city = l_city[1],                     
                                          l_county = l_county[1],                    
                                          use_type = m_p_USE_TYPE[1])

groupby_clean3$emv_per_sq = groupby_clean3$emv_total/ groupby_clean3$fin_sq_ft
groupby_clean3$n_emv_per_sq = scale(groupby_clean3$emv_total/ groupby_clean3$fin_sq_ft)
groupby_clean3$ln_tp = scale(groupby_clean3$tp)
groupby_clean3$l_tp = log(groupby_clean3$tp)
groupby_clean3$l_depth = log(groupby_clean3$depth)
boxplot(groupby_clean3$depth)
groupby_clean3$n_pw_meters_to_lake_edge = scale(groupby_clean3$pw_meters_to_lake_edge)
boxplot(groupby_clean3$n_pw_meters_to_lake_edge)
boxplot(groupby_clean3$n_emv_per_sq)
ggplot(groupby_clean3,aes(n_pw_meters_to_lake_edge,log(tp))) + geom_point() 
ggplot(groupby_clean3,aes(n_emv_per_sq,log(tp))) + geom_point() 
ggplot(groupby_clean3,aes(n_emv_per_sq,l_depth)) + geom_point() 
groupby_clean3 <- filter(groupby_clean3,n_pw_meters_to_lake_edge<2,tp<0.25,n_emv_per_sq<0.25)

write.csv(groupby_clean3,'distance_model.csv')
fit <- lm(l_tp~n_emv_per_sq +n_pw_meters_to_lake_edge,data=groupby_clean3)
summary(fit)

order_clean<-clean[order(clean$m_w_lake_name,clean$m_p_year),]
clean100 <- filter(order_clean,m_pw_meters_to_lake_edge<=300,m_pw_meters_to_lake_edge>0)
gclean100 <- clean100 %>%
  filter(m_p_FIN_SQ_FT!=0) %>%
  group_by(m_pw_MCES_Map_Code1) %>% summarise(city = m_p_CITY[1],
                                  county_id = m_p_COUNTY_ID[1],
                                  dwell_type = m_p_DWELL_TYPE[1],
                                  emv_bldg = mean(m_p_EMV_BLDG,na.rm=TRUE),
                                  emv_land = mean(m_p_EMV_LAND,na.rm=TRUE),
                                  emv_total = mean(m_p_EMV_TOTAL,na.rm=TRUE),
                                  fin_sq_ft = mean(m_p_FIN_SQ_FT,na.rm=TRUE),
                                  green_acre = mean(m_p_GREEN_ACRE,na.rm=TRUE),
                                  homestead = m_p_HOMESTEAD[1],              
                                  home_style = m_p_HOME_STYLE[1],              
                                  num_units = m_p_NUM_UNITS[1],               
                                  parc_code = m_p_PARC_CODE[1],              
                                  p_pin = m_p_PIN[1],                     
                                  school_dst = m_p_SCHOOL_DST[1],              
                                  use1_desc = m_p_USE1_DESC[1],              
                                  wshd_dist = m_p_WSHD_DIST[1],               
                                  year_built = m_p_YEAR_BUILT[1],              
                                  age = mean(m_age,na.rm=TRUE),                      
                                  year = m_p_year[1],                    
                                  zip = m_p_ZIP[1],                     
                                  centroid_long = m_p_centroid_long[1],          
                                  centroid_lat = m_p_centroid_lat[1],            
                                  #mces_map_code1 = m_pw_MCES_Map_Code1[1],         
                                  pw_meters_to_monitor_site = mean(m_pw_meters_to_monitor_site,na.rm=TRUE),
                                  pw_meters_to_lake_edge = mean(m_pw_meters_to_lake_edge,na.rm=TRUE),    
                                  lake_name = m_w_lake_name[1],               
                                  major_water = m_w_major_watershed[1],        
                                  grade = mean(m_w_grade,na.rm=TRUE),                   
                                  phy = mean(m_w_phy,na.rm=TRUE),                     
                                  rec = mean(m_w_rec,na.rm=TRUE),                    
                                  depth = mean(m_w_depth,na.rm=TRUE),                   
                                  tp = mean(m_w_tp,na.rm=TRUE),                      
                                  l_city = l_city[1],                     
                                  l_county = l_county[1],                    
                                  use_type = m_p_USE_TYPE[1])



#USE TYPE
dt <- clean
resRows <- grep(".*res|Res|RES|Apartment|APARTMENT|apartment|Hous|hous|HOUS.*",dt$m_p_USE1_DESC)
comerRows <- grep("comm|Comm|COMM|commerc|Commerc|COMMERC", dt$m_p_USE1_DESC)
# commonAreaRows <- grep(".*(No Value).*", dt$p_USE1_DESC[comerRows]) -- empty
# comerRows <- comerRows[-commonAreaRows]
indRows <- grep(".*industrial|Industrial|INDUSTRIAL.*",dt$m_p_USE1_DESC)
agriRows <- grep(".*agric|Agric|AGRIC.*",dt$m_p_USE1_DESC)
excRows <- grep(".*Industr|Residen|Commerc.*",dt$m_p_USE1_DESC[agriRows])
agriRows <- agriRows[-excRows]
schoRows <- grep(".*School|College|COLLEGE|SCHOOL|K-12.*",dt$m_p_USE1_DESC)
hosRows <- grep(".*Hospital|hospital|HOSPITAL.*",dt$m_p_USE1_DESC)
federalRows <- grep(".*Federal|federal|FEDERAL.*",dt$m_p_USE1_DESC)
deptRows <- grep(".*Department Store|Shopping|shopping|department store.*",dt$m_p_USE1_DESC)
#hotelRows <- grep(".*Hotel|hotel.*",dt$m_p_USE1_DESC)
homeparkRows <- grep(".*home park|Home Park|HOME PARK.*",dt$m_p_USE1_DESC)

#assign use type to each parcel
dt$p_USE_TYPE <- rep("others",dim(dt)[1])
dt$p_USE_TYPE[resRows] <- "residential"
dt$p_USE_TYPE[comerRows] <- "commericial"
dt$p_USE_TYPE[indRows] <- "industrial"
dt$p_USE_TYPE[agriRows] <- "agricultural"
dt$p_USE_TYPE[schoRows] <- "school"
dt$p_USE_TYPE[hosRows] <- "hospital"
dt$p_USE_TYPE[federalRows] <- "federal property"
dt$p_USE_TYPE[deptRows] <- "department store"
#dt$p_USE_TYPE[hotelRows] <- "hotel"
dt$p_USE_TYPE[homeparkRows] <- "home park"

use_type = as.data.frame.matrix(table(dt$m_pw_MCES_Map_Code1,dt$p_USE_TYPE))
colnames(use_type)[c(3,4,5)] = c('depart_store','fed_property','home_park')
use_type = mutate(use_type, use_type_total = agricultural + commericial + industrial + residential + depart_store + fed_property + home_park
                  + hospital #+ hotel 
                  + school + others,
                  use_type_agricultural = round(agricultural/use_type_total,digits=4), 
                  use_type_commercial = round(commericial/use_type_total, digits=4),
                  use_type_depart_store = round(depart_store/use_type_total,digits=4),
                  use_type_industrial = round(industrial/use_type_total,digits=4),
                  use_type_residential = round(residential/use_type_total,digits=4),
                  use_type_fed_property = round(fed_property/use_type_total,digits=4),
                  use_type_home_park = round(home_park/use_type_total,digits=4),
                  use_type_hospital = round(hospital/use_type_total,digits=4),
                  #use_type_hotel = round(hotel/use_type_total,digits=4),
                  use_type_school = round(school/use_type_total,digits=4),
                  use_type_others = round(others/use_type_total,digits=4))
use_type = use_type[c(12:21)]
uniquelakes = dt %>% group_by(m_pw_MCES_Map_Code1)%>%select(m_pw_MCES_Map_Code1)%>%distinct()
use_type$m_pw_MCES_Map_Code1 = uniquelakes$m_pw_MCES_Map_Code1
dt = left_join(dt,use_type,by='m_pw_MCES_Map_Code1')


gclean100 <- dt %>%
  filter(m_p_FIN_SQ_FT!=0,m_pw_meters_to_lake_edge<=300,m_pw_meters_to_lake_edge>0) %>%
  group_by(m_pw_MCES_Map_Code1,m_p_year) %>% 
  summarise(city = m_p_CITY[1],
                                              county_id = m_p_COUNTY_ID[1],
                                              dwell_type = m_p_DWELL_TYPE[1],
                                              emv_bldg = mean(m_p_EMV_BLDG,na.rm=TRUE),
                                              emv_land = mean(m_p_EMV_LAND,na.rm=TRUE),
                                              emv_total = mean(m_p_EMV_TOTAL,na.rm=TRUE),
                                              fin_sq_ft = mean(m_p_FIN_SQ_FT,na.rm=TRUE),
                                              green_acre = mean(m_p_GREEN_ACRE,na.rm=TRUE),
                                              homestead = m_p_HOMESTEAD[1],              
                                              home_style = m_p_HOME_STYLE[1],              
                                              num_units = m_p_NUM_UNITS[1],               
                                              parc_code = m_p_PARC_CODE[1],              
                                              #p_pin = m_p_PIN[1],                     
                                              school_dst = m_p_SCHOOL_DST[1],              
                                              use1_desc = m_p_USE1_DESC[1],              
                                              wshd_dist = m_p_WSHD_DIST[1],               
                                              year_built = m_p_YEAR_BUILT[1],              
                                              age = mean(m_age,na.rm=TRUE),                      
                                              #year = m_p_year[1],                    
                                              zip = m_p_ZIP[1],                     
                                              centroid_long = m_p_centroid_long[1],          
                                              centroid_lat = m_p_centroid_lat[1],            
                                              #mces_map_code1 = m_pw_MCES_Map_Code1[1],         
                                              pw_meters_to_monitor_site = mean(m_pw_meters_to_monitor_site,na.rm=TRUE),
                                              pw_meters_to_lake_edge = mean(m_pw_meters_to_lake_edge,na.rm=TRUE),    
                                              lake_name = m_w_lake_name[1],               
                                              major_water = m_w_major_watershed[1],        
                                              grade = mean(m_w_grade,na.rm=TRUE),                   
                                              phy = mean(m_w_phy,na.rm=TRUE),                     
                                              rec = mean(m_w_rec,na.rm=TRUE),                    
                                              depth = mean(m_w_depth,na.rm=TRUE),                   
                                              tp = mean(m_w_tp,na.rm=TRUE),                      
                                              l_city = l_city[1],                     
                                              l_county = l_county[1],                    
                                              use_type = m_p_USE_TYPE[1],
                                              use_type_commercial = mean(use_type_commercial),
            use_type_industrial = mean(use_type_industrial),
            use_type_residential = mean(use_type_residential),
            use_type_agricultural = mean(use_type_agricultural),
            use_type_school = mean(use_type_school),
            use_type_fed_property = mean(use_type_fed_property))
gclean100$emv_per_sq = gclean100$emv_total/ gclean100$fin_sq_ft
gclean100$n_emv_per_sq = scale(gclean100$emv_total/ gclean100$fin_sq_ft)
gclean100$n_tp = scale(gclean100$tp)
gclean100$l_tp = log(gclean100$tp)
gclean100$l_depth = log(gclean100$depth)
boxplot(gclean100$depth)
gclean100$n_pw_meters_to_lake_edge = scale(gclean100$pw_meters_to_lake_edge)
boxplot(gclean100$n_pw_meters_to_lake_edge)
boxplot(gclean100$emv_per_sq)
ggplot(gclean100,aes(n_pw_meters_to_lake_edge,log(tp))) + geom_point() 
ggplot(gclean100,aes(emv_per_sq,log(tp))) + geom_point() 
ggplot(gclean100,aes(emv_per_sq,l_depth)) + geom_point() 
ggplot(gclean100,aes(use_type_commercial,log(tp))) + geom_point() 
gclean100 <- filter(gclean100, emv_per_sq<500)
gclean100$use_type <- as.factor(gclean100$use_type)
gclean100 <- within(gclean100, use_typeF <- relevel(use_type,'others'))
gclean100$school_dst <- as.factor(gclean100$school_dst)
fit <- lm(l_tp~n_emv_per_sq+n_pw_meters_to_lake_edge + as.factor(use_typeF) ,data=gclean100)
summary(fit)
d5<-gclean100
d5 <- pdata.frame(d5, index = c("m_pw_MCES_Map_Code1", "m_p_year"), drop.index = TRUE,row.names = TRUE)
fitf <- plm(emv_per_sq~n_tp, model="within",data=d5)
summary(fitf)
fitr <- plm(emv_per_sq~n_tp, model="pooling",data=d5)
summary(fitr)

phtest(fitf,fitr)

