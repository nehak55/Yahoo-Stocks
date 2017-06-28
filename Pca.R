library(corrplot)
library(sqldf)
data_csv <- read.csv('/Users/neha/Documents/Predictive/hw2/data_file_ARQ.csv')
#View(data_csv)
indicators<- c("revenue","cor","gp","sps","eps","netinc","ncff","ncf","ncfdebt",
               "ncfcommon","assets","bvps","equity","debt","de","pe1","ps1","netmargin","fcfps",
               "ncfi","assetsc","assetsnc","capex","cashneq","cashnequsd","currentratio","debtusd",
               "ebit","ebitda","ebitdamargin","ebitusd","ebt","epsdil","epsusd","equityusd","ev",
               "evebit","evebitda","fcf","fcfps","grossmargin","invcap","inventory",
               "liabilitiesc","marketcap","ncfo","ncfx","netinc","netinccmn","pb","pe","ps",
               "retearn","sgna","sharesbas","shareswa","taxexp","tbvps","workingcapital")

#***** Removing multicollinearity using co-relation matrix ***********

newdata <- data_csv[indicators]
#boxplot(data_csv$marketcap)
newdata <-na.omit(newdata)
dim(newdata)
res<-cor(newdata, use="complete.obs", method="pearson") 

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

ind1 <- c("price","marketcap","sps","eps","ncff","ncf","accoci","ncfcommon","bvps","de",
          "pe1","ps1","netmargin","ncfi","capex","currentratio", "epsdil",
          "fcfps", "ncfx","pb","tbvps")
data_ind1<- data_csv[ind1]
data_ind1 <-na.omit(data_ind1)

res1<-cor(data_ind1, use="complete.obs", method="pearson") 
corrplot(res1, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

ind1 <- c("ticker", "calendardate","price","marketcap","sps","eps","ncff","ncf","accoci","ncfcommon","bvps","de",
          "pe1","ps1","netmargin","ncfi","capex","currentratio", "epsdil",
          "fcfps", "ncfx","pb","tbvps")

data_ind1<- data_csv[ind1]
data_ind1 <-na.omit(data_ind1)
dim(data_ind1)
# *******Computing Price ratio**********************
data_ind1<-data_ind1[order(data_ind1$ticker, data_ind1$calendardate),]
# adding a new cloumn for log price ratio
data_ind1["pratio"] <- NA

records<-nrow(data_ind1)
for (i in 2:records) {
  if(identical(data_ind1$ticker[i],data_ind1$ticker[i-1])){ # if ticker is same
    
    data_ind1$pratio[i]<-(data_ind1$price[i]/data_ind1$price[i-1])# calculate the log returns
    
  } else {
    data_ind1$pratio[i]<-0
  }
  
  
}
data_ind1$pratio[1]<-0

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
dfNormZ<-cbind(dfNormZ,data_ind1$pratio)

#naming the normalized variable
colnames(dfNormZ)[22] <- "ticker"
colnames(dfNormZ)[23] <- "calenderdate"
colnames(dfNormZ)[24] <- "pratio"

#***************** regression model****************************

dfNormZ<-subset(dfNormZ,pratio!=0)
dfNormZ<-subset(dfNormZ,pratio!=Inf)
dfNormZ<-dfNormZ[complete.cases(dfNormZ), ]
dfNormZ <-na.omit(dfNormZ)

# split the data
smp_size <-floor(0.75* nrow(dfNormZ))
train_ind <-sample(seq_len(nrow(dfNormZ)), size = smp_size)
train_set <- dfNormZ[train_ind,]
test_set  <-dfNormZ[-train_ind,]

cal_date<-unique(train_set$calenderdate)
l<-length(cal_date)
reg_model <- list()
reg_summary <- list()
for (i in 1:l){
  data_set<-subset(train_set,calenderdate==cal_date[i])
  
  #Building a linear regression model
  f<-lm(log(data_set$pratio)~marketcap+sps+eps+ncff+ncf+accoci
        +ncfcommon+bvps+de+pe1+ps1+netmargin+ncfx+pb
        +ncfi+capex+currentratio+epsdil+fcfps+tbvps,data=data_set)
  
  reg_model[[i]] <- f
  reg_summary[[i]] <- summary(f)
  cat("Summary for Model", i)
  print(summary(f))
}
#*********************************************************************
#********************** HW 4 use PCA**********************************
#*********************************************************************
pca_dfnorm <-princomp(dfNormZ[,2:21])
summary(pca_dfnorm)
#screeplot(pca_dfnorm, type="lines")
#biplot(pca_dfnorm)
pca_data<-data.frame(price = dfNormZ[, "price"], ticker = dfNormZ[, "ticker"],
                     calenderdate = dfNormZ[, "calenderdate"],
                     pratio = dfNormZ[, "pratio"],pca_dfnorm$scores)

#interested in first 16 componrnts
pca_data<- pca_data[,1:20]

# split the data
smp_size1 <-floor(0.75* nrow(pca_data))
train_ind1 <-sample(seq_len(nrow(pca_data)), size = smp_size1)
train_set1 <- pca_data[train_ind1,]
test_set1  <-pca_data[-train_ind1,]

cal_date<-unique(train_set1$calenderdate)
l<-length(cal_date)
reg_model1 <- list()
reg_summary1 <- list()
# regression model #
for (i in 1:l){
  data_set1<-subset(train_set1,calenderdate==cal_date[i])
  f1<-lm(log(data_set1$pratio)~Comp.1+Comp.2+Comp.3+Comp.4+Comp.5
        +Comp.6+Comp.7+Comp.8+Comp.9+Comp.10+Comp.11+Comp.12+Comp.13
        +Comp.14+Comp.15+Comp.16,data=data_set1)
  
  reg_model1[[i]] <- f
  reg_summary1[[i]] <- summary(f1)
  cat("Summary for Model", i)
  print(summary(f1))
}


#Conclusion:1. The betas values do change significantly, PCA ensures removing of 
# multicollinearity 
#2. Betas value are more signficant (p values)
#3. Model looks consistent in 20 runs of pca comapred to 20 runs of without pca
###############################################################  
###***************** HW 4 part 2 ************************* ###
##############################################################
tic <- read.csv('/Users/neha/Desktop/tic.csv')
tic<-tic[order(tic$Sector),]
#View(tic)

t = sqldf("select ticker, sector from tic where sector = 'Information Technology'")
#View(t)

# get all the data in information technology sector 
colnames(t)[1] <- "ticker"
sect_wise <-merge(dfNormZ,t, by = "ticker")
#View(sect_wise)

# Run a linear regression model for Information Technology sector

f_sect <- lm(log(sect_wise$pratio)~marketcap+sps+eps+ncff+ncf+accoci
               +ncfcommon+bvps+de+pe1+ps1+netmargin+ncfx+pb
               +ncfi+capex+currentratio+epsdil+fcfps+tbvps ,data=sect_wise)

summary(f_sect)


# Run a regression model using pca for Information Technology sector
#View(pca_data)
pca_sect_wise <-merge(pca_data,t, by = "ticker")

f_pca<-lm(log(pca_sect_wise$pratio)~Comp.1+Comp.2+Comp.3+Comp.4+Comp.5
       +Comp.6+Comp.7+Comp.8+Comp.9+Comp.10+Comp.11+Comp.12+Comp.13
       +Comp.14+Comp.15+Comp.16,data=pca_sect_wise)

summary(f_pca)
