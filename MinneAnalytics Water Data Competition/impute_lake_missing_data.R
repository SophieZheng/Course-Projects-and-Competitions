#install.packages("bigrquery")
library(bigrquery)
#library(plyr)
#library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(chron) #to use times()
library(mice)
library(VIM)
library(lattice)
#library(pan)
#library(randomForest)


project = 'minneanalytics-146300'
sql = "SELECT * FROM [datadive-142319:mces_lakes.1999_2014_monitoring_data]"
df = query_exec(sql, project = project)

write.csv(df,'raw_data_lakes.csv')

#check data attributes
name = names(df)

#get data from year 1990 or later
df = tbl_df(df)
df90 <- df[df$START_DATE >= '1990',]

#---sort data by date, time and lake name---
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#replace whitespace and convert to time
df90$START_HOURMIN24 <- times(paste(trim(df90$START_HOURMIN24),":00"))
df90$END_HOURMIN24 <- times(paste(trim(df90$END_HOURMIN24),":00"))

df90$LAKE_NAME <- as.character(df90$LAKE_NAME)
df90 <- df90[order(df90$LAKE_NAME,as.Date(df90$START_DATE),df90$START_HOURMIN24),]
#check data
df90[,c(3,10:11,16)][90:150,]


#Earley <- df90[df90$LAKE_NAME=="Earley Lake",]
#Earley[Earley$START_HOURMIN24!="00:00:00",]$SECCHI_DEPTH_RESULT
#df90 <- df90[df90$START_HOURMIN24!="00:00:00",]

#when there is seasonal grade, tp and secchi values are missing;
#where there is not seasonal grade, tp and secchi vauesexist
#tp and secchi, check if there is any non-NA values when there is seasonal grade
sum(!is.na(filter(df90, SEASONAL_LAKE_GRADE_QUALIFIER == "Approved")[,c(25,29)]))

#----remove seasonal grade as it is not useful for analysis-----
df90 <- filter(df90, is.na(SEASONAL_LAKE_GRADE_QUALIFIER))
dim(df90)
ggplot(df90,aes(SECCHI_DEPTH_RESULT)) + geom_histogram()
ggplot(df90,aes(TOTAL_PHOSPHORUS_RESULT)) + geom_histogram() +
  scale_x_continuous(limits = c(0,1))

#----visulization to know the data------
#a1<-data.frame("Type"=rep("",dim(df90)[1]), "Value"=scale(df90$SECCHI_DEPTH_RESULT))
#a2<-data.frame("Type"=rep("",dim(df90)[1]), "Value"=df90$TOTAL_PHOSPHORUS_RESULT)
#a<-rbind(a1,a2)
#ggplot(a2,aes(x=Type,y=Value)) + geom_boxplot() + xlab('') + ylab('Total Phosphorus')
#plot(df90$SAMPLE_DEPTH_IN_METERS,df90$SECCHI_DEPTH_RESULT)
#plot(df90$PHYSICAL_CONDITION_RESULT,df90$SECCHI_DEPTH_RESULT)
#plot(df90$TOTAL_PHOSPHORUS_RESULT,df90$SECCHI_DEPTH_RESULT)

#---examine missing values patterns---
md.pattern(df90[,c(25,29)])
#l5 <- levels(df90$LAKE_NAME)[21:25]
#lakes5 <- filter(df90, LAKE_NAME %in% l5)
matrixplot(df90[,c(25,29)],col=c('darkgray','red','navyblue'))
aggr(df90[c("SECCHI_DEPTH_RESULT","TOTAL_PHOSPHORUS_RESULT")], col=c('navyblue','red'),labels = c("SECCHI_DEPTH","TOTAL_PHOSPHORUS"), prop=TRUE, numbers=TRUE, ylab=c("Histogram of missing data","Pattern"))

#---impute the missing values by mice---
df90$START_DATE = as.Date(df90$START_DATE)
df90$MAJOR_WATERSHED = as.factor(df90$MAJOR_WATERSHED)
#--mice
cities <- unique(df90$CITY)
df90$year <- as.factor(year(df90$START_DATE))
df90$LAKE_NAME <- as.factor(df90$LAKE_NAME)
imputed <- data.frame()
i<-imputed
for (city in cities){
  data <- df90[df90$CITY==city,]
  print(city)
  if (sum(is.na(data[,c(3,25,29,34)])) !=0){
    
     print("mising")
      print(data[,c(3,25,29,34)])
      imp <- mice(data[,c(3,25,29,34)], m=1, method = 'pmm', maxit=10,seed= 2525)
      print(1)
      completedData <- complete(imp,1)
      print(2)
      imputed <- rbind(imputed,completedData)
      print(3)

  }else{
    print("no mising")
    imputed <- rbind(imputed,data[,c(3,25,29,34)])
    print(4)
  }
}

imputedf <- df90
#-----replace raw data with imputed values------
lakes <- unique(imputedf$LAKE_NAME)
for(lake in lakes){
  imputedf[imputedf$LAKE_NAME==lake,][,c(25,29)] <- imputed[imputed$LAKE_NAME==lake,][,c(2,3)]
}
#check missing values
sum(is.na(imputedf$SECCHI_DEPTH_RESULT))
sum(is.na(imputedf$TOTAL_PHOSPHORUS_RESULT))
write.csv(imputedf,'mice_imputed90.csv')

#------check distribution of raw and imputed data-------
df90$group <- rep("raw",dim(df90)[1])
imputedf$group<-rep("imputed",dim(df90)[1])
c<-filter(df90,!is.na(df90$TOTAL_PHOSPHORUS_RESULT))
a<-rbind(df90,imputedf)
ggplot(a,aes(SECCHI_DEPTH_RESULT,color=group)) + geom_density(alpha = 0.2) + 
  #facet_grid(.~group) +
  scale_x_continuous(limits=c(0,1))

ggplot(a,aes(TOTAL_PHOSPHORUS_RESULT,color=group)) + geom_density(alpha = 0.2) + 
  #facet_grid(.~group) +
  scale_x_continuous(limits=c(0,1))

#d <- df90[df90$LAKE_NAME == "Shields Lake",]
#d$month <- as.yearmon(d$START_DATE)
#d$year <- as.numeric(year(d$START_DATE))
#d<-filter(d,TOTAL_PHOSPHORUS_RESULT<10)
#y<-d%>%group_by(year)%>%summarise(mean=mean(TOTAL_PHOSPHORUS_RESULT))
#m<-d%>%group_by(month)%>%summarise(mean=mean(TOTAL_PHOSPHORUS_RESULT))

#impute by pan - imputation of panel data
# nrows <- df90 %>% group_by(LAKE_NAME) %>% summarise(no_rows=length(SECCHI_DEPTH_RESULT))
#int <- rep(1,dim(df90)[1])
#pred <- cbind(int,df90[,c(14)])
#  xcol <- 1:2
#  zcol <- 1
#  a <- 2
#  c <- 2
#  id2 <- matrix(c(1,0,0,1),ncol=2,nrow=2)
#  Binv <- a*id2
#  Dinv <- c*id2
#  prior <- list(a=a, Binv=Binv, c=c, Dinv=Dinv)
#  imputeDepth <- pan(as.matrix(df90[,c(25,29)]),as.vector(nrows),pred,xcol,zcol,prior,seed=2525,iter=10)

#---Inspecting the distribution of original and imputed data---
#xyplot(imp,SECCHI_DEPTH_RESULT ~ TOTAL_PHOSPHORUS_RESULT+SAMPLE_DEPTH_IN_METERS+PHYSICAL_CONDITION_RESULT+RECREATIONAL_SUITABILITY_RESULT,pch=18,cex=1)
#densityplot(imp)

#---------CHECK OUTLIERS----------
boxplot(imputedf$TOTAL_PHOSPHORUS_RESULT)
boxplot(imputed$SECCHI_DEPTH_RESULT)
tpiqr <- abs(quantile(imputedf$TOTAL_PHOSPHORUS_RESULT,0.75) - quantile(imputedf$TOTAL_PHOSPHORUS_RESULT,0.25))
tp3qr <- quantile(imputedf$TOTAL_PHOSPHORUS_RESULT,0.75)
seciqr <- abs(quantile(imputedf$SECCHI_DEPTH_RESULT,0.75) - quantile(imputedf$SECCHI_DEPTH_RESULT,0.25))
sec3qr <- quantile(imputedf$SECCHI_DEPTH_RESULT,0.75)

tp3qrlakes <- filter(imputedf, TOTAL_PHOSPHORUS_RESULT > 10 )
sec3qrlakes <- filter(imputedf, SECCHI_DEPTH_RESULT > 6 )

tp3qrlakes[,c(3,10,25,29)]
sec3qrlakes[,c(3,10,25,29)]

tplists <- unique(tp3qrlakes$LAKE_NAME)
seclists <- unique(sec3qrlakes$LAKE_NAME)

#lakes that have tp outliers: Gaystock Lake  Levander Pond  McCarrons Lake Shields Lake
gaystock <- imputedf[imputedf$LAKE_NAME=="Gaystock Lake", c(3,4,5,10,25,29)]
levander <- imputedf[imputedf$LAKE_NAME=="Levander Pond", c(3,4,5,10,25,29)]
mccarrons <- imputedf[imputedf$LAKE_NAME=="McCarrons Lake", c(3,4,5,10,25,29)]
shields <- imputedf[imputedf$LAKE_NAME=="Shields Lake", c(3,4,5,10,25,29)]


tp110lakes <- filter(imputedf, TOTAL_PHOSPHORUS_RESULT > 1 , TOTAL_PHOSPHORUS_RESULT < 10 )
unique(tp110lakes$LAKE_NAME)

#filter out outliers
imputedf_noout <- filter(imputedf, TOTAL_PHOSPHORUS_RESULT < 10)
#save data that is imputed and has no outliers
write.csv(imputedf_noout,'imputed_no_outliers.csv')

#lakes that have sec outliers: Christmas Lake Halfbreed Lake Little Carnelian Lake Lower Prior Lake     
#Riley Lake Square Lake   
christmas <- imputedf[imputedf$LAKE_NAME=="Christmas Lake", c(3,4,5,10,25,29)]


shield_lake <- imputedf[imputedf$LAKE_NAME=="Shields Lake",][,c(3,10,18,20,25,29)]
sl <- df90[df90$LAKE_NAME=="Shields Lake",][,c(3,10,18,20,25,29)]

#-------


















