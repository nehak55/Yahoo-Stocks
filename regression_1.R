#*********Loading the data **************************
data_csv <- read.csv('/Users/neha/Documents/Predictive/hw2/data_file_ARQ.csv')
#View(data_csv)

#subset the data, take your chosen 20 indicators
indicators<- c("ticker", "calendardate","revenue","cor","gp","sps","eps","netinc",
               "ncff","ncf","ncfdebt","ncfcommon","assets","bvps","equity","debt","de",
               "pe1","ps1","netmargin","fcfps","ncfi","price")

newdata <- data_csv[indicators]
#View(newdata)
newdata <-na.omit(newdata)
dim(newdata)



# *******Computing Price ratio**********************
newdata<-newdata[order(newdata$ticker, newdata$calendardate),]
# adding a new cloumn for log price ratio
newdata["pratio"] <- NA

records<-nrow(newdata)
for (i in 2:records) {
  if(identical(newdata$ticker[i],newdata$ticker[i-1])){ # if ticker is same
    
    newdata$pratio[i]<-(newdata$price[i]/newdata$price[i-1])# calculate the log returns
    
  } else {
    newdata$pratio[i]<-0
  }
  
  
}
newdata$pratio[1]<-0
#View(newdata)


#****************Data cleaning********************
# Removing the outlier
data1<-newdata
#1. Revenue
data1 <-subset(data1, revenue < 5000000000)
data1 <-subset(data1, revenue > -1000000000)
boxplot(data1$revenue)

# 2.Cor
data1 <-subset(data1, cor < 4000000000)
data1 <-subset(data1, cor > -100000000)
boxplot(data1$cor)

# 3.gp
data1 <-subset(data1, gp < 4000000000)
#boxplot(data1$gp)

# 4.sps
data1 <-subset(data1, sps < 700)
data1 <-subset(data1, sps > -100)
#boxplot(data1$sps)

# 5.eps
#boxplot(data1$eps)
data1 <-subset(data1, eps < 30)
data1 <-subset(data1, eps > -20)

# 6.netinc
#boxplot(data1$netinc)
data1 <-subset(data1, netinc < 3000000000)
data1 <-subset(data1, netinc > -2000000000)

#7.ncff
#boxplot(data1$ncff)
data1<-subset(data1, ncff > -10000000000)
data1<-subset(data1, ncff < 10000000000)

# 8.ncfdebt
#boxplot(data1$ncfdebt)
data1<-subset(data1, ncfdebt < 6000000000)
data1<-subset(data1, ncfdebt > -5000000000)

# 9.ncfcommon
#boxplot(data1$ncfcommon)
data1<-subset(data1, ncfcommon > -2000000000)
data1<-subset(data1, ncfcommon < 2000000000)

# 10.assets
#boxplot(data1$assets)
data1<-subset(data1, assets < 20000000000)

#11.equity
#boxplot(data1$equity)

#12.debt
#boxplot(data1$debt)
data1<-subset(data1, debt < 11000000000)

#13.de
#boxplot(data1$de)
data1<-subset(data1, de < 500)
data1<-subset(data1, de > -500)

#14.pe1
data1<-subset(data1, pe1 < 2500)
data1<-subset(data1, pe1 > -2500)
#boxplot(data1$pe1)

#15.ps1
data1<-subset(data1, ps1 < 150)
data1<-subset(data1, ps1 > -100)
#boxplot(data1$ps1)

#16.netmargin

data1<-subset(data1, netmargin < 200)
data1<-subset(data1, netmargin > -300)
#boxplot(data1$netmargin)

#17.fcfps
data1<-subset(data1, fcfps < 30)
data1<-subset(data1, fcfps > -30)
#boxplot(data1$fcfps)

#18.ncfi
data1<-subset(data1, ncfi < 2000000000)
data1<-subset(data1, ncfi > -3000000000)
#boxplot(data1$ncfi)
#View(data1)


#************ Normalize data***************

#We need regression for each indicator indivdually
#Column 1 and Column 2 are ticker and calendar date,so not considered
#for (k in 3:22){
  #normalizing each independent var
#  x<-(data1[,k]-mean(data1[,k]))/sd(data1[,k])
  #merging the normalized var to the original dataset
 # data1<-cbind(data1,x)
  #naming the normalized variable
 # names(data1)[NCOL(data1)]<-paste(colnames(data1[k]),"_n",sep="")

#}
#norm_data<-cbind(norm_data,data1$ticker, data1$calendardate,data1$price)


dfNormZ <- as.data.frame( scale(data1[3:23] ))

dfNormZ<-cbind(dfNormZ,data1$ticker)
dfNormZ<-cbind(dfNormZ,data1$calendardate)
dfNormZ<-cbind(dfNormZ,data1$pratio)
#naming the normalized variable
colnames(dfNormZ)[22] <- "ticker"
colnames(dfNormZ)[23] <- "calenderdate"
colnames(dfNormZ)[24] <- "pratio"
#View(dfNormZ)



#***************** regression model****************************

#dfNormZ<-dfNormZ[,-(1:19-1)]
dfNormZ<-subset(dfNormZ,pratio!=0)
dfNormZ<-subset(dfNormZ,pratio!=Inf)
dfNormZ<-dfNormZ[complete.cases(dfNormZ), ]
dfNormZ <-na.omit(dfNormZ)

# split the data
smp_size <-floor(0.75* nrow(dfNormZ))
train_ind <-sample(seq_len(nrow(dfNormZ)), size = smp_size)
train_set <- dfNormZ[train_ind,]
test_set  <-dfNormZ[-train_ind,]

#data1<-subset(data1,pratio!=0)
#data1<-subset(data1,pratio!=Inf)
#data1<-data1[complete.cases(data1), ]
#data1 <-na.omit(data1)
cal_date<-unique(train_set$calenderdate)
l<-length(cal_date)
reg_model <- list()
reg_summary <- list()
for (i in 1:l){
  data_set<-subset(train_set,calenderdate==cal_date[i])
  
  #Building a linear regression model
  f<-lm(log(data_set$pratio)~revenue+cor+gp+sps+eps+netinc+ncff+ncf
      +ncfdebt+ncfcommon+assets+equity+debt+de+pe1+ps1+netmargin
      +fcfps+ncfi+bvps,data=data_set)
  
  reg_model[[i]] <- f
  reg_summary[[i]] <- summary(f)
  cat("Summary for Model", i)
  print(summary(f))
}

#*************Visualize the data ****************************

# Scatterplot Matrices from the glus Package 
library(gclus)
dta <- data_set[,1:3] # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )

