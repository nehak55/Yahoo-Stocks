# In this project we analayze the stock data. The data is quarterly,and we have data
# 20 unique quarters.First we built a linear regression model for stocks for 
# first 15 quarters. Then we used ARIMA model to forecast the co-efficients for
# the remaining last 4 quarters. Using these forecasted coefficients we predicted the 
#Price returns of stocks for the last 4 quarters. At last we compare our forecasted 
# returns with the actual returns

library(sqldf)

# Load the data
data_csv <- read.csv('/Users/neha/Documents/Predictive/hw2/data_file_ARQ.csv')

# Indicators
ind1 <- c("ticker", "calendardate","price","marketcap","sps","eps","ncff","ncf","accoci","ncfcommon","bvps","de",
          "pe1","ps1","netmargin","ncfi","capex","currentratio", "epsdil",
          "fcfps", "ncfx","pb","tbvps")

data_ind1<- data_csv[ind1]
data_ind1 <-na.omit(data_ind1)
dim(data_ind1)
# *******Computing Price returns**********************
data_ind1<-data_ind1[order(data_ind1$ticker, data_ind1$calendardate),]
# adding a new cloumn for log price ratio
data_ind1["PriceReturn"] <- NA

records<-nrow(data_ind1)
for (i in 2:records) {
  if(identical(data_ind1$ticker[i],data_ind1$ticker[i-1])){ # if ticker is same
    
    data_ind1$PriceReturn[i]<-(data_ind1$price[i]/data_ind1$price[i-1])# calculate the log returns
    
  } else {
    data_ind1$PriceReturn[i]<-0
  }
  
  
}
data_ind1$PriceReturn[1]<-0

#View(data_ind1)

#### Data Cleaning, remove outliers #######
# 1. marketcap
data_ind1 <-subset(data_ind1, marketcap < 150000000000)
#boxplot(data_ind1$marketcap)

# 2 sps 
data_ind1 <-subset(data_ind1, sps < 800)
data_ind1 <-subset(data_ind1, sps > -100)
#boxplot(data_ind1$sps)

# 3 eps
data_ind1 <-subset(data_ind1, eps < 10)
data_ind1 <-subset(data_ind1, eps > -10)
#boxplot(data_ind1$eps)

# 4 ncff
data_ind1 <-subset(data_ind1, ncff < 1000000000)
data_ind1 <-subset(data_ind1, ncff > -1000000000)
#boxplot(data_ind1$ncff)

#5 ncf can remove later
data_ind1 <-subset(data_ind1, ncf < 700000000)
data_ind1 <-subset(data_ind1, ncf > -1000000000)
#boxplot(data_ind1$ncf)

#6 accoci can remove later
data_ind1 <-subset(data_ind1, accoci < 1000000000)
data_ind1 <-subset(data_ind1, accoci > -2000000000)
#boxplot(data_ind1$accoci)

#7 ncfcommon lot of outliers still
data_ind1 <-subset(data_ind1, ncfcommon < 400000000)
data_ind1 <-subset(data_ind1, ncfcommon > -300000000)
#boxplot(data_ind1$ncfcommon)

# 8 bvps
data_ind1 <-subset(data_ind1, bvps < 200)
#boxplot(data_ind1$bvps)

#9 de
data_ind1 <-subset(data_ind1, de < 100)
data_ind1 <-subset(data_ind1, de > -100)
#boxplot(data_ind1$de)

#10 pe1
data_ind1 <-subset(data_ind1, pe1 < 1500)
data_ind1 <-subset(data_ind1, pe1 > -1500)
#boxplot(data_ind1$pe1)

#11 ps1
data_ind1 <-subset(data_ind1, ps1 < 1000)
data_ind1 <-subset(data_ind1, ps1 > -1000)
#boxplot(data_ind1$ps1)

# 12 netmargin
data_ind1 <-subset(data_ind1, netmargin <  40)
data_ind1 <-subset(data_ind1, netmargin > -80)
#boxplot(data_ind1$netmargin)

# 13 ncfi
data_ind1 <-subset(data_ind1, ncfi <  1000000000)
data_ind1 <-subset(data_ind1, ncfi > -2000000000)
#boxplot(data_ind1$ncfi)

# 14 capex
data_ind1 <-subset(data_ind1, capex > -900000000)
data_ind1 <-subset(data_ind1, capex < 200000000)
#boxplot(data_ind1$capex)

# 15 currentratio
data_ind1 <-subset(data_ind1, currentratio < 40)
#boxplot(data_ind1$currentratio)

# 16 epsdil
data_ind1 <-subset(data_ind1, epsdil < 20)
#boxplot(data_ind1$epsdil)

# 17 fcfps
data_ind1 <-subset(data_ind1, fcfps < 20)
data_ind1 <-subset(data_ind1, fcfps > -20)
#boxplot(data_ind1$fcfps)

# 18 ncfx
data_ind1 <-subset(data_ind1, ncfx < 9000000)
data_ind1 <-subset(data_ind1, ncfx > -9000000)
#boxplot(data_ind1$ncfx)

# 19 pb
data_ind1 <-subset(data_ind1, pb < 200)
data_ind1 <-subset(data_ind1, pb > -150)
#boxplot(data_ind1$pb)

#20 tbvps
data_ind1 <-subset(data_ind1, tbvps < 300)
#boxplot(data_ind1$tbvps)

#************ Normalize data***************
dfNormZ <- as.data.frame( scale(data_ind1[3:23] ))
dfNormZ<-cbind(dfNormZ,data_ind1$ticker)
dfNormZ<-cbind(dfNormZ,data_ind1$calendardate)
dfNormZ<-cbind(dfNormZ,data_ind1$PriceReturn)

#naming the normalized variable
colnames(dfNormZ)[22] <- "ticker"
colnames(dfNormZ)[23] <- "calenderdate"
colnames(dfNormZ)[24] <- "PriceReturn"

#***************** regression model****************************

dfNormZ<-subset(dfNormZ,PriceReturn!=0)
dfNormZ<-subset(dfNormZ,PriceReturn!=Inf)
dfNormZ<-dfNormZ[complete.cases(dfNormZ), ]
dfNormZ <-na.omit(dfNormZ)

cal_date<-unique(dfNormZ$calenderdate)
l<-length(cal_date)
l<-l-5
reg_model <- list()
reg_summary <- list()
betas<-NULL
for (i in 1:l){
  data_set<-subset(dfNormZ,calenderdate==cal_date[i])
  

  #Building a linear regression model
  f<-lm(data_set$PriceReturn~marketcap+sps+eps+ncff+ncf+accoci
        +ncfcommon+bvps+de+pe1+ps1+netmargin+ncfx+pb
        +ncfi+capex+currentratio+epsdil+fcfps+tbvps,data=data_set)
  
  reg_model[[i]] <- f
  reg_summary[[i]] <- summary(f)
  betas<- rbind(betas,reg_model[[i]]$coefficients)
}


###***** Time Series for the Universe ********
colnames(betas) <- c("beta0","beta1","beta2","beta3","beta4","beta5","beta6","beta7",
                     "beta8","beta9","beta10","beta11","beta12","beta13","beta14",
                     "beta15","beta16","beta17","beta18","beta19","beta20")


#**** ARIMA model*******

data_ts<- dfNormZ[order(dfNormZ$calenderdate),]
cal_date<-unique(data_ts$calenderdate)

# Add dates in the betas(coefficients) data frame
betas<-as.data.frame(betas)

betas$date <- cal_date[1:15]
betas$date <- as.Date(betas$date, format = "%Y-%m-%d")

# forecast the betas for 16 through 19 dates
for (j in 1:21){
  for (i in 16:20){
     coeff <- ts(betas[,j], start = c(2011,03), end = c(2014,09), frequency = 15+(i-15))
    fit <- arima(coeff, order = c(1,0,2))
    pr <- predict(fit,n.ahead = 1)
    betas[i, j] <- as.numeric(pr$pred) # append the forecasted betas(16-20)
    
  }
}

# subset the data and get last 4 dates(16 to 19) data in data_ts
cal_date<-as.data.frame(cal_date[16:19])
data_ts<-sqldf('select * from data_ts where calenderdate in cal_date')

for (i in 16:19){
  betas[i,22] <- cal_date[i-15,1]
}



# forecast the return price for each date
data_ts$calenderdate <- as.Date(data_ts$calenderdate, format = "%Y-%m-%d") 


# Here we took 4 df(set1,set2,set3,set4) 
set1<-subset(data_ts,data_ts$calenderdate == "2014-12-31")
set2<-subset(data_ts,data_ts$calenderdate == "2015-03-31")
set3<-subset(data_ts,data_ts$calenderdate == "2015-06-30")
set4<-subset(data_ts,data_ts$calenderdate == "2015-09-30")

n1<-nrow(set1)
n2<-nrow(set2)
n3<-nrow(set3)
n4<-nrow(set4)

# Predicting price returns using forecasted betas for each date(16-19)
gg<-0
set1$for_price<-NA
for(i in 1:n1){
  for(j in 2:21){
    target<- set1[i,j]*betas[16,j]
    target = target + gg 
  }
   set1[i,25] <- target + betas[16,1]
}


set1 <-set1[order(set1$PriceReturn),]
set2$for_price<-NA
for(i in 1:n2){
  for(j in 2:21){
    target<- set2[i,j]*betas[17,j]
    target = target + gg 
  }
  set2[i,25] <- target + betas[17,1]
}


set3$for_price<-NA
for(i in 1:n3){
  for(j in 2:21){
    target<- set3[i,j]*betas[18,j]
    target = target + gg 
  }
  set3[i,25] <- target + betas[18,1]
}


set4$for_price<-NA
for(i in 1:n4){
  for(j in 2:21){
    target<- set4[i,j]*betas[19,j]
    target = target + gg 
  }
  set4[i,25] <- target + betas[19,1]
}

# Combining the data in a single data frame
org_data<-rbind(set1,set2,set3,set4)


n_row<-nrow(org_data)
comp1<-org_data[order(org_data$PriceReturn),] # order by original price returns
comp2<-org_data[order(org_data$for_price),] # order by forecasted price returns

# create 5 buckets for price return
new_df<-NULL
new_df$meanPriceReturn<-0
new_df$meanfor_return<-0

# diving into 5 equal buckets
f1<-1
f2<-2066
for(i in 1:5){
  new_df$meanPriceReturn[i]<-mean(comp1[f1:f2,24])
  new_df$meanfor_return[i]<-mean(comp2[f1:f2,24])
  f1<- f2
  f2<- f2+2066 
  
}

# Actual and forecasted returns for the Universe
names(new_df) <- c("ActualReturns","Forecasted Returns")
new_df<-as.data.frame(new_df)
knitr::kable(new_df)




### ******** Sector wise************** ####
## In this part of the project we will repeat the same process as above but this time
# we will restrict our stocks to a particular sector. We will consider stocks belonging
# to only Information sector
tic <- read.csv('/Users/neha/Documents/Predictive/tic.csv')
tic<-tic[order(tic$Sector),]
t = sqldf("select ticker, sector from tic where sector = 'Information Technology'")

# get all the data in information technology sector 
colnames(t)[1] <- "ticker"
sect_wise <-merge(dfNormZ,t, by = "ticker")


# Run a linear regression model for Information Technology sector
cal_date1<-unique(sect_wise$calenderdate)
l<-length(cal_date1)
l<-l-5
reg_model1 <- list()
reg_summary1 <- list()
betas1<-NULL
for (i in 1:l){
  data_set1<-subset(sect_wise,calenderdate==cal_date1[i])
  
  #Building a linear regression model
  f_sect <- lm(log(data_set1$PriceReturn)~marketcap+sps+eps+ncff+ncf+accoci
               +ncfcommon+bvps+de+pe1+ps1+netmargin+ncfx+pb
               +ncfi+capex+currentratio+epsdil+fcfps+tbvps ,data=data_set1)
  
  
  reg_model1[[i]] <- f_sect
  reg_summary1[[i]] <- summary(f_sect)
 # cat("Summary for Model", i)
 # print(summary(f_sect))
  betas1<- rbind(betas1,reg_model1[[i]]$coefficients)
}



colnames(betas1) <- c("beta0","beta1","beta2","beta3","beta4","beta5","beta6","beta7",
                     "beta8","beta9","beta10","beta11","beta12","beta13","beta14",
                     "beta15","beta16","beta17","beta18","beta19","beta20")


#**** ARIMA model*******

data_ts1<- sect_wise[order(sect_wise$calenderdate),]
cal_date<-unique(data_ts1$calenderdate)

# Add dates in the betas(coefficients) data frame
betas1<-as.data.frame(betas1)

betas1$date <- cal_date[1:15]
betas1$date <- as.Date(betas1$date, format = "%Y-%m-%d")

# forecast the betas for 16 through 19 dates
for (j in 1:21){
  for (i in 16:20){
    
    coeff1 <- ts(betas1[,j], start = c(2011,03), end = c(2014,09), frequency = 15+(i-15))
    #  print(betas[,j])
    fit1 <- arima(coeff1, order = c(1,0,2))
    pr1 <- predict(fit1,n.ahead = 1)
    betas1[i, j] <- as.numeric(pr1$pred) # append the forecasted betas(16-20)
    
  }
}

# subset the data and get last 4 dates data in data_ts
cal_date<-as.data.frame(cal_date[16:19])
data_ts1<-sqldf('select * from data_ts1 where calenderdate in cal_date')

for (i in 16:19){
  betas1[i,22] <- cal_date[i-15,1]
}

#View(betas1)

# forecast the return price
data_ts1$calenderdate <- as.Date(data_ts1$calenderdate, format = "%Y-%m-%d") 


set11<-subset(data_ts1[,2:25],data_ts1$calenderdate == "2014-12-31")
set21<-subset(data_ts1[,2:25],data_ts1$calenderdate == "2015-03-31")
set31<-subset(data_ts1[,2:25],data_ts1$calenderdate == "2015-06-30")
set41<-subset(data_ts1[,2:25],data_ts1$calenderdate == "2015-09-30")

n1<-nrow(set11)
n2<-nrow(set21)
n3<-nrow(set31)
n4<-nrow(set41)
gg<-0
set11$for_price1<-NA
for(i in 1:n1){
  for(j in 2:21){
    target1<- set11[i,j]*betas1[16,j]
    target1 = target1 + gg 
  }
  set11[i,25] <- target1 + betas1[16,1]
}
#View(set11)
gg<-0
#set1 <-set1[order(set1$PriceReturn),]
set21$for_price1<-NA
for(i in 1:n2){
  for(j in 2:21){
    target1<- set21[i,j]*betas1[17,j]
    target1 = target1 + gg 
  }
  set21[i,25] <- target1 + betas1[17,1]
}
#View(set21)

gg<-0
set31$for_price1<-NA
for(i in 1:n3){
  for(j in 2:21){
    target1<- set31[i,j]*betas1[18,j]
    target1 = target1 + gg 
  }
  set31[i,25] <- target1 + betas1[18,1]
}
#View(set31)
gg<-0
set41$for_price1<-NA
for(i in 1:n4){
  for(j in 2:21){
    target1<- set41[i,j]*betas1[19,j]
    target1 = target1 + gg 
  }
  set41[i,25] <- target1 + betas1[19,1]
}
#View(set41)
org_data1<-rbind(set11,set21,set31,set41)

n_row<-nrow(org_data1)
comp11<-org_data1[order(org_data1$PriceReturn),]
comp21<-org_data1[order(org_data1$for_price1),]


# create 5 buckets for price return
new_df1<-NULL
new_df1$meanPriceReturn<-0
new_df1$meanForcasted<-0

# Dividing the data into 5 equal buckets
f1<-1
f2<-185
for(i in 1:5){
  new_df1$meanPriceReturn[i]<-mean(comp11[f1:f2,23])
  new_df1$meanForcasted[i]<-mean(comp21[f1:f2,25])
  f1<- f2
  f2<- f2+185
 
}

# Actual and forecasted returns for Information Technology
names(new_df1) <- c("ActualReturns","Forecasted Returns")
new_df1<-as.data.frame(new_df1)
knitr::kable(new_df1)



#The output shows that the model forecasted extremely well for the universe, where the 
# difference between actual and forecasted returns are minimal
#But for sector wise (Information Technology sector) it's result are not so accuarate.
