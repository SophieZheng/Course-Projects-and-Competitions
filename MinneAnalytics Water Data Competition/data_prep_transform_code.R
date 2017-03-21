
library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(quantmod)

# load all lake parcel merged results
# data = fread('lake_parcel_merge000000000000.csv')
# data1 = fread('lake_parcel_merge000000000001.csv')
# data2 = fread('lake_parcel_merge000000000002.csv')
# data3 = fread('lake_parcel_merge000000000003.csv')
# data4 = fread('lake_parcel_merge000000000004.csv')
# data5 = fread('lake_parcel_merge000000000005.csv')
# data6 = fread('lake_parcel_merge000000000006.csv')
# data7 = fread('lake_parcel_merge000000000007.csv')
# data8 = fread('lake_parcel_merge000000000008.csv')
# data9 = fread('lake_parcel_merge000000000009.csv')
# data10 = fread('lake_parcel_merge000000000010.csv')
# l = list(data, data1, data2, data3, data4,  data5, data6, data7, data8, data9, data10)
# dt = rbindlist(l, use.names=TRUE)


dt = fread('allLake_parcel_merge.csv')
str(dt)
#clean up data types and errors
temp = filter(dt,p_EMV_TOTAL != 0)
temp$age[temp$age < 0] = 0
temp$age[temp$age > 1000] = 0
temp$p_NUM_UNITS = as.integer(temp$p_NUM_UNITS)
temp$pw_meters_to_lake_edge[temp$pw_meters_to_lake_edge < 1] = 1
temp$p_FIN_SQ_FT[temp$p_FIN_SQ_FT < 1] = 1
#delete duplicate parcels (based on same PIN, EMV values, and year)
names(temp) #5,6,7,21
attr_del <- temp[,c(5,6,7,14,20)]
dim(attr_del)
dupRows <- which(duplicated(attr_del))
temp <- temp[-dupRows,]


#weight EMV values by parcel distance to lake
temp = mutate(temp,EMVTotal_w = p_EMV_TOTAL/pw_meters_to_lake_edge, 
            EMVLand_w = p_EMV_LAND/pw_meters_to_lake_edge, 
            EMVBldg_w = p_EMV_BLDG/pw_meters_to_lake_edge)

#divide EMV values by sqt_ft
temp = mutate(temp,EMVTotal_w_sqt = EMVTotal_w/p_FIN_SQ_FT, EMVLand_w_sqt = EMVLand_w/p_FIN_SQ_FT,
            EMVBldg_w_sqt = EMVBldg_w/p_FIN_SQ_FT)

# checkpoint
# write.csv(temp,'allLake_parcel_merge_treated.csv')
# dt = fread('allLake_parcel_merge_treated.csv')

# SCHOOL DISTRICT
# replace letters with ""
# dt = as.data.frame(dt)
dt$p_SCHOOL_DST<-gsub("[a-zA-Z]+","",dt$p_SCHOOL_DST,perl=TRUE)
#replace ""/NA with "12345678"
nalength <- length(dt[is.na(dt$p_SCHOOL_DST),]$p_SCHOOL_DST)
dt[is.na(dt$p_SCHOOL_DST),]$p_SCHOOL_DST <- rep("12345678",nalength)
nospacelength <- length(dt[dt$p_SCHOOL_DST=="",]$p_SCHOOL_DST)
dt[dt$p_SCHOOL_DST=="",]$p_SCHOOL_DST <- rep("12345678",nospacelength)
# sum(dt$p_SCHOOL_DST == "12345678") 
# delete 0 (those start with 0)
dt$p_SCHOOL_DST <- as.numeric(dt$p_SCHOOL_DST)
dt$p_SCHOOL_DST <- as.character(dt$p_SCHOOL_DST)
l<-length(dt[dt$p_SCHOOL_DST=="12345678",]$p_SCHOOL_DST)
dt[dt$p_SCHOOL_DST=="12345678",]$p_SCHOOL_DST <- rep("null",l)
# dt = dt[-c(1,2)]

#DWELLING TYPE
dtype = read.csv('dwell_type_match.csv')
dtype = dtype[c(1,2)]
dt = left_join(dt,dtype,by='p_DWELL_TYPE')

#USE TYPE
resRows <- grep(".*res|Res|RES|Apartment|APARTMENT|apartment|Hous|hous|HOUS.*",dt$p_USE1_DESC)
comerRows <- grep("comm|Comm|COMM|commerc|Commerc|COMMERC", dt$p_USE1_DESC)
# commonAreaRows <- grep(".*(No Value).*", dt$p_USE1_DESC[comerRows]) -- empty
# comerRows <- comerRows[-commonAreaRows]
indRows <- grep(".*industrial|Industrial|INDUSTRIAL.*",dt$p_USE1_DESC)
agriRows <- grep(".*agric|Agric|AGRIC.*",dt$p_USE1_DESC)
excRows <- grep(".*Industr|Residen|Commerc.*",dt$p_USE1_DESC[agriRows])
agriRows <- agriRows[-excRows]
schoRows <- grep(".*School|College|COLLEGE|SCHOOL|K-12.*",dt$p_USE1_DESC)
hosRows <- grep(".*Hospital|hospital|HOSPITAL.*",dt$p_USE1_DESC)
federalRows <- grep(".*Federal|federal|FEDERAL.*",dt$p_USE1_DESC)
deptRows <- grep(".*Department Store|Shopping|shopping|department store.*",dt$p_USE1_DESC)
hotelRows <- grep(".*Hotel|hotel.*",dt$p_USE1_DESC)
homeparkRows <- grep(".*home park|Home Park|HOME PARK.*",dt$p_USE1_DESC)

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
dt$p_USE_TYPE[hotelRows] <- "hotel"
dt$p_USE_TYPE[homeparkRows] <- "home park"



#clean up home style
hs = read.csv('homestyle_match.csv')
dt = left_join(dt,hs,by='p_HOME_STYLE')
# ---------------------------------------------------------------------------------------------
# write.csv(dt,'allLake_parcel_merge_treated.csv')
# dt = fread('allLake_parcel_merge_treated.csv')
dt = as.data.frame(dt)
dt = dt[-c(1,2,35,36,37,38,39,40)]
dt = mutate(dt,EMVTotal_sqt = p_EMV_TOTAL/p_FIN_SQ_FT, EMVLand_sqt = p_EMV_LAND/p_FIN_SQ_FT,EMVBldg_sqt = p_EMV_BLDG/p_FIN_SQ_FT)
#aggregate data to lake neighborhood level
# gb = dt %>% group_by(pw_MCES_Map_Code1,p_year) %>% 
#   summarize (
#              EMVTotal = mean(EMVTotal),EMVLand = mean(EMVLand), EMVBldg = mean(EMVBldg),
#              
#              avgSQ_FT = mean(p_FIN_SQ_FT), avgNumUnits = mean(p_NUM_UNITS), avgAge = mean(age),
#              avgGrade = mean(w_grade),avgPhy = mean(w_phy), avgRec = mean(w_rec), 
#              avgDepth = mean(w_depth), avgTP = mean(w_tp), counts = n())

#aggregate data for lakeshore properties (<= 1000 feet (300 m))
gb = dt %>% 
  filter(pw_meters_to_lake_edge <= 800, pw_meters_to_lake_edge >= 300) %>%
  group_by(pw_MCES_Map_Code1,p_year) %>% 
  summarize (
    EMVTotal = mean(p_EMV_TOTAL),EMVLand = mean(p_EMV_LAND), EMVBldg = mean(p_EMV_BLDG),
    EMVTotal_sqt = mean(EMVTotal_sqt), EMVLand_sqt = mean(EMVLand_sqt), EMVBldg_sqt = mean(EMVBldg_sqt),
    avgSQ_FT = mean(p_FIN_SQ_FT), avgNumUnits = mean(p_NUM_UNITS), avgAge = mean(age),
    avgGrade = mean(w_grade),avgPhy = mean(w_phy), avgRec = mean(w_rec), 
    avgDepth = mean(w_depth), avgTP = mean(w_tp), counts = n()) %>%
  arrange(pw_MCES_Map_Code1,p_year)

# obtain CPI data from FRED to adjust EMV for inflation
getSymbols("CPIAUCSL", src='FRED')
avgCPI = apply.yearly(CPIAUCSL, mean)
cf <- avgCPI/as.numeric(avgCPI['2016'])
cf = as.data.frame(cf)
cf$p_year = seq(1947,2016)
dat <- merge(gb, cf, by = 'p_year')
colnames(dat)[18] = 'cf'
dat = mutate(dat, rEMVTotal = EMVTotal * cf, 
             rEMVLand = EMVLand * cf,
             rEMVBldg = EMVBldg * cf,
             rEMVTotal_sqt = EMVTotal_sqt * cf,
             rEMVLand_sqt = EMVLand_sqt * cf,
             rEMVBldg_sqt = EMVBldg_sqt * cf)


#correct lake outliers
dat$avgTP[dat$pw_MCES_Map_Code1 == '19008800-01'&dat$p_year==2011]=0.2141
dat$avgTP[dat$pw_MCES_Map_Code1 == '82016200-01'&dat$p_year==2006]=1.0142


# calculate green acreage by lake neighborhood
green_acre = as.data.frame.matrix(table(dt$pw_MCES_Map_Code1,dt$p_GREEN_ACRE))
green_acre = mutate(green_acre, green_y = round(Y/(V1+N+Y),digits=4), 
                    green_n = round(N/(V1+N+Y), digits=4),
                    green_na = round((1- green_y - green_n),digits=4))
#add percentage of green acreage per lake neighborhood
green_acre = select(green_acre,green_y,green_n,green_na)
uniquelakes = dt %>% group_by(pw_MCES_Map_Code1)%>%select(pw_MCES_Map_Code1)%>%distinct()
green_acre$pw_MCES_Map_Code1 = uniquelakes$pw_MCES_Map_Code1
dat = merge(dat,green_acre,by='pw_MCES_Map_Code1')
dat = arrange(dat,pw_MCES_Map_Code1,p_year)


#groupby school district
counted <- dt %>% group_by(pw_MCES_Map_Code1,p_SCHOOL_DST) %>% summarise(counts=n())
school_lists <- filter(counted, counts==max(counts))%>% select(pw_MCES_Map_Code1,p_SCHOOL_DST)

#add top school district to each lake neighborhood
dat = left_join(dat,school_lists,by='pw_MCES_Map_Code1')

#groupby dwell type
dtype_counted <- dt %>% group_by(pw_MCES_Map_Code1,p_NEW_DWELL_TYPE) %>% summarise(counts=n())
dtype_lists <- dtype_counted %>% 
  filter(!is.na(p_NEW_DWELL_TYPE)) %>%
  filter(counts==max(counts))%>%select(pw_MCES_Map_Code1,p_NEW_DWELL_TYPE)

#add top dwell type to each lake neighborhood
dat = left_join(dat,dtype_lists,by='pw_MCES_Map_Code1')

#calculate and add use type percentage to each lake neighborhood
use_type = as.data.frame.matrix(table(dt$pw_MCES_Map_Code1,dt$p_USE_TYPE))
colnames(use_type)[c(3,4,5)] = c('depart_store','fed_property','home_park')
use_type = mutate(use_type, use_type_total = agricultural + commericial + industrial + residential + depart_store + fed_property + home_park
                  + hospital + hotel + school + others,
                  use_type_agricultural = round(agricultural/use_type_total,digits=4), 
                  use_type_commercial = round(commericial/use_type_total, digits=4),
                  use_type_depart_store = round(depart_store/use_type_total,digits=4),
                  use_type_industrial = round(industrial/use_type_total,digits=4),
                  use_type_residential = round(residential/use_type_total,digits=4),
                  use_type_fed_property = round(fed_property/use_type_total,digits=4),
                  use_type_home_park = round(home_park/use_type_total,digits=4),
                  use_type_hospital = round(hospital/use_type_total,digits=4),
                  use_type_hotel = round(hotel/use_type_total,digits=4),
                  use_type_school = round(school/use_type_total,digits=4),
                  use_type_others = round(others/use_type_total,digits=4))
use_type = use_type[c(12:23)]
use_type$pw_MCES_Map_Code1 = uniquelakes$pw_MCES_Map_Code1
dat = left_join(dat,use_type,by='pw_MCES_Map_Code1')

dat = dat[-c(3:8)]

homestyle = as.data.frame.matrix(table(dt$pw_MCES_Map_Code1,dt$NEW_HOME_STYLE))

                
# write.csv(dat,'allLakeNeighborhood_panel_dt_v6_300_800m.csv')

dat_resid = filter(dat, p_USE_TYPE == 'residential')
zz <- plm(rEMVBldg_sqt ~ avgAge,data = dat_resid, index = c("pw_MCES_Map_Code1","p_year"))
summary(zz)
