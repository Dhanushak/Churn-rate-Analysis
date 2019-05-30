  setwd("C:\\Jig18638")
ds<-read.csv('telecomfinal.csv')
head(ds)
summary(ds)
dim(ds)

#Preparing Data Quality report using DataQualityR package

library(dataQualityR)
num.file<-paste("C:\\Jig18638\\dq_num",sep="")
cat.file<-paste("C:\\Jig18638\\dq_cat",sep="")
checkDataQuality(data=ds,out.file.num=num.file, out.file.cat=cat.file)

#Data prepartion
#1mou_mean#mean no. of monthly minutes of use
boxplot(ds$mou_Mean)
summary(ds$mou_Mean)
quantile(ds$mou_Mean,p=c(1:100)/100,na.rm = TRUE)
quantile(ds$mou_Mean,p=c(990:1000)/1000,na.rm = TRUE)
quantile(ds$mou_Mean,p=c(9990:10000)/10000,na.rm = TRUE)
quantile(ds$mou_Mean,p=c(99990:100000)/100000,na.rm = TRUE)
ds$mou_Mean<-ifelse(ds$mou_Mean>=7568.320,NA,ds$mou_Mean)
summary(ds$mou_Mean)
sum(is.na(ds$mou_Mean))
ds$mou_Mean[is.na(ds$mou_Mean)]<-529.1
sum(is.na(ds$mou_Mean))
hist(ds$mou_Mean)

#2Months#Total no. of months in service
boxplot(ds$months)
summary(ds$months)
quantile(ds$months,p=c(1:100)/100,na.rm = TRUE)
quantile(ds$months,p=c(990:1000)/1000,na.rm = TRUE)
ds$months<-ifelse(ds$months>=57,NA,ds$months)
sum(is.na(ds$months))
ds$months[is.na(ds$months)]<-16
summary(ds$months)

#3totcalls
#total no. of calls over the life of a customer
boxplot(ds$totcalls)
summary(ds$totcalls)
quantile(ds$totcalls,p=c(1:100)/100)
quantile(ds$totcalls,p=c(990:1000)/1000)
#IQR
1.5*(3508-860)
ds$totcalls<-ifelse(ds$totcalls>=3972,NA,ds$totcalls)
sum(is.na(ds$totcalls))
ds$totcalls[is.na(ds$totcalls)]<-1796
boxplot(ds$totcalls)
summary(ds$totcalls)


#4Ovrrev_mean
#mean average revenue
summary(ds$ovrrev_Mean)
boxplot(ds$ovrrev_Mean)
quantile(ds$ovrrev_Mean,p=c(1:100)/100, na.rm= TRUE)
quantile(ds$ovrrev_Mean,p=c(990:1000)/1000, na.rm= TRUE)
quantile(ds$ovrrev_Mean,p=c(9990:10000)/10000, na.rm= TRUE)
#IQR
1.5*(13.94-0)
ds$ovrrev_Mean<-ifelse(ds$ovrrev_Mean>=20.91,NA,ds$ovrrev_Mean)
sum(is.na(ds$ovrrev_Mean))
ds$ovrrev_Mean[is.na(ds$ovrrev_Mean)]<-0.90
summary(ds$ovrrev_Mean)
boxplot(ds$ovrrev_Mean)

#5dwelltype
#dwelling type unit
summary(ds$dwlltype)
ds$dwlltype[is.na(ds$dwlltype)]<-"S"
sum(is.na(ds$dwlltype))

#6Martial Status.
summary(ds$marital)
ds$marital[is.na(ds$marital)]<-'U'
sum(is.na(ds$marital))

#7Income
#Estimated income
summary(ds$income)
ds$income<-as.factor(ds$income)
ds$income[is.na(ds$income)]<-6
sum(is.na(ds$income))

#8CRCLSCOD
#Credit class code
summary(ds$crclscod)
#no treatment required

#9HND_Price
#current hand price
summary(ds$hnd_price)
boxplot(ds$hnd_price)
ds$hnd_price[is.na(ds$hnd_price)]<-149.9899
sum(is.na(ds$hnd_price))

#10PRIZM_SOCIAL_ONE
#Social group letter only
summary(ds$prizm_social_one)
ds$prizm_social_one[is.na(ds$prizm_social_one)]<-'S'
sum(is.na(ds$prizm_social_one))

#11AVGREV
#avg monthly revenue
summary(ds$avgrev)
ds$avgrev<-ifelse(ds$avgrev>=600,58.11,ds$avgrev)
boxplot(ds$avgrev)

#12rev_mean
#mean monthly revenue
summary(ds$rev_Mean)
boxplot(ds$rev_Mean)
ds$rev_Mean[is.na(ds$rev_Mean)]<-717.9008

#13total_mrc_mean
#mean total monthly recurring charge
summary(ds$totmrc_Mean)
boxplot(ds$totmrc_Mean)
ds$totmrc_Mean<-ifelse(is.na(ds$totmrc_Mean),47,ds$totmrc_Mean)
sum(is.na(ds$totmrc_Mean))

#dropping the variables with high missing values percentage
ds<-subset(ds,select = -c(dwllsize,mailordr,occu1,wrkwoman,solflag,proptype,mailresp,cartype,children,div_type,numbcars))

#rev_range
#Range of revenue (charge amount)
summary(ds$rev_Range)
boxplot(ds$rev_Range)
ds$rev_Range<-ifelse(ds$rev_Range>=81.375,NA, ds$rev_Range)
ds$rev_Range[is.na(ds$rev_Range)]<-18.80

#Age1
#Age of first household member
summary(ds$age1)
boxplot(ds$age1)
ds$age1[is.na(ds$age1)]<-36

#Age2 
#Age of second household member
summary(ds$age2)
boxplot(ds$age2)
ds$age2[is.na(ds$age2)]<-21.14

#dropping the variables which have range and mean
#Using the mean as its a better representation
ds<-subset(ds,select = -c(mou_Range,datovr_Range,da_Range))
ds<-within(ds,rm(drop_vce_Range))
#CHANGE_MOU
#Percentage change in monthly minutes of use vs previous three month average
summary(ds$change_mou)
boxplot(ds$change_mou)
ds$change_mou<-ifelse(ds$change_mou<=-504.750,NA,ds$change_mou)
ds$change_mou<-ifelse(ds$change_mou>=1556.3470,NA,ds$change_mou)
ds$change_mou[is.na(ds$change_mou)]<-13.41

#DROP_BLK_MEAN
#Number of dropped or blocked calls is equal to the sum of blocked data and voice calls and dropped data and voice calls.
summary(ds$drop_blk_Mean)
boxplot(ds$drop_blk_Mean)
ds$drop_blk_Mean<-ifelse(ds$drop_blk_Mean>=305.77,NA,ds$drop_blk_Mean)
ds$drop_blk_Mean[is.na(ds$drop_blk_Mean)]<-5.333

#drop_vce_mean
#Mean number of dropped (failed) voice calls
summary(ds$drop_vce_Mean)
boxplot(ds$drop_vce_Mean)
ds$drop_vce_Mean<-ifelse(ds$drop_vce_Mean>=10.50045,NA,ds$drop_vce_Mean)
ds$drop_vce_Mean[is.na(ds$drop_vce_Mean)]<-3.0000

#drop_dat_mean
#Mean number of dropped (failed) data calls
summary(ds$drop_dat_Mean)
boxplot(ds$drop_dat_Mean)
ds$drop_dat_Mean<-ifelse(ds$drop_dat_Mean>=22.82235,NA,ds$drop_dat_Mean)
ds$drop_dat_Mean[is.na(ds$drop_dat_Mean)]<-0.0

#datovr_mean
#mean revenue of data overage
summary(ds$datovr_Mean)
boxplot(ds$datovr_Mean)
ds$datovr_Mean<-ifelse(ds$datovr_Mean>=136.62034,NA,ds$datovr_Mean)
ds$datovr_Mean[is.na(ds$datovr_Mean)]<-0.0

#da_mean
#mean no. of directory assisted calls
summary((ds$da_Mean))
boxplot(ds$da_Mean)
ds$da_Mean=ifelse(ds$da_Mean>=1.485,NA,ds$da_Mean)
ds$da_Mean[is.na(ds$da_Mean)]<-0.2475

#mou_pead_Mean
#mean unrounded minutes of use of peaked data calls
summary(ds$mou_pead_Mean)
boxplot(ds$mou_pead_Mean)
ds$mou_pead_Mean<-ifelse(ds$mou_pead_Mean>=232.79742,NA,ds$mou_pead_Mean)
ds$mou_pead_Mean[is.na(ds$mou_pead_Mean)]<-0.0

#blck_dat_Mean
#mean no. of blocked data calls
summary(ds$blck_dat_Mean)
boxplot(ds$blck_dat_Mean)
ds$blck_dat_Mean<-ifelse(ds$blck_dat_Mean>=14.160533,NA,ds$blck_dat_Mean)
ds$blck_dat_Mean[is.na(ds$blck_dat_Mean)]<-0.0293

#recv_sms_mean
#mean no. of recieved sms calls
summary(ds$recv_sms_Mean)
boxplot(ds$recv_sms_Mean)
ds$recv_sms_Mean<-ifelse(ds$recv_sms_Mean>=53.580,NA,ds$recv_sms_Mean)
ds$recv_sms_Mean[is.na(ds$recv_sms_Mean)]<-0.0492

#roam_mean
#mean no. of roaming calls.
summary(ds$roam_Mean)
boxplot(ds$roam_Mean)
ds$roam_Mean<-ifelse(ds$roam_Mean>=0.387,NA,ds$roam_Mean)
ds$roam_Mean[is.na(ds$roam_Mean)]<-1.261
                         

#actvsubs and uniqsubs
#no. of active subscribers and no. unique subscriber in household
#no treatment required for the below variables
summary(ds$actvsubs)
summary(ds$uniqsubs)
                
#Truck
#truck indicator
summary(ds$truck)
ds$truck<-as.factor(ds$truck)                        
summary(ds$truck)                        
ds$truck[is.na(ds$truck)]<-0

#retdays
#no. of days since last retention calls
summary(ds$retdays)
boxplot(ds$retdays)
ds$retdays<-ifelse(ds$retdays>=886.492,NA,ds$retdays)
ds$retdays[is.na(ds$retdays)]<-162.0


#owylis_vce_Range
#range of no. of outbound wireless to wireless calls
summary(ds$owylis_vce_Range)
ds$owylis_vce_Range<-ifelse(ds$owylis_vce_Range>=227.704,NA,ds$owylis_vce_Range)
ds$owylis_vce_Range[is.na(ds$owylis_vce_Range)]<-8.00

#mou_opkv_range
#range of unrounded minutes of use of off peak voice calls
ds$mou_opkv_Range<-ifelse(ds$mou_opkv_Range>=1639.6503,NA,ds$mou_opkv_Range)
ds$mou_opkv_Range[is.na(ds$mou_opkv_Range)]<-55.24

#eqpdays
#no.  of days of current equipment
ds$eqpdays<-ifelse(ds$eqpdays>=1585.704,NA,ds$eqpdays)
ds$eqpdays[is.na(ds$eqpdays)]<-375.6

#custcare_Mean
#mean no. of customer care calls
ds$custcare_Mean<-ifelse(ds$custcare_Mean>=49.23467,NA,ds$eqpdays)
ds$custcare_Mean[is.na(ds$custcare_Mean)]<-1.803


#callwait_Mean
#mean no. of calls waiting calls
ds$callwait_Mean<-ifelse(ds$callwait_Mean>=63.33333,NA,ds$callwait_Mean)
ds$callwait_Mean[is.na(ds$callwait_Mean)]<-0.3333

#dropping callwait_Range-range of no of waiting calls
ds<-within(ds,rm(callwait_Range))
dim(ds)


#ccrndmou_Range
#range of rounded minutes of calls
ds$ccrndmou_Range<-ifelse(ds$ccrndmou_Range>=85,NA,ds$ccrndmou_Range)
ds$ccrndmou_Range[is.na(ds$ccrndmou_Range)]<-6.083

#adjqty #billing adjusted total no of calls over the life the customer
ds$adjqty<-ifelse(ds$adjqty>=44039.45,NA,ds$adjqty)
ds$adjqty[is.na(ds$adjqty)]<-1764

#ovrmou_Mean #mean overage minutes of use
ds$ovrmou_Mean<-ifelse(ds$ovrmou_Mean>=851.5600,NA,ds$ovrmou_Mean)
ds$ovrmou_Mean[is.na(ds$ovrmou_Mean)]<-40.18

#comp_vce_Mean #mean no of completed  voice calls
ds$comp_vce_Mean<-ifelse(ds$comp_vce_Mean>=951.3093,NA,ds$comp_vce_Mean)
ds$comp_vce_Mean[is.na(ds$comp_vce_Mean)]<-78.33

#plcd_vce_Mean #mean no of attempted voice calls placed
ds$plcd_vce_Mean<-ifelse(ds$plcd_vce_Mean>=243.045,NA,ds$plcd_vce_Mean)
ds$plcd_vce_Mean[is.na(ds$plcd_vce_Mean)]<-88.7


#avg3mou #average monthly mou over the orevious three months
ds$avg3mou<-ifelse(ds$avg3mou>=853.5,NA,ds$avg3mou)
ds$avg3mou[is.na(ds$avg3mou)]<-277.0

#avgmou # average monthly mou over the life of the customer
ds$avgmou<-ifelse(ds$avgmou>=726.3,NA,ds$avgmou)
ds$avgmou[is.na(ds$avgmou)]<-302.6


#avg3qty #average monthly no of calls over the previous three months
ds$avg3qty<-ifelse(ds$avg3qty>=280.5,NA,ds$avg3qty)
ds$avg3qty[is.na(ds$avg3qty)]<-128

#avg6mou #average monthly mou over previous 6 months
ds$avg6mou<-ifelse(ds$avg6mou>=816,NA,ds$avg6mou)
ds$avg6mou[is.na(ds$avg6mou)]<-280

#avg6qty #average monthly no of calls over the previous six months
ds$avg6qty<-ifelse(ds$avg6qty>=271.5,NA,ds$avg6qty)
ds$avg6qty[is.na(ds$avg6qty)]<-182.2
sum(is.na(ds$avg6qty))
#1.5*IQR
#inter quartile range 
#IQR=75%-25%

#models #no of models issued
ds$models<-as.factor(ds$models)
ds$models[is.na(ds$models)]<-1

#forgntvl
#foreign travel dummy variable
ds$forgntvl<-as.factor(ds$forgntvl)
ds$forgntvl[is.na(ds$forgntvl)]<-0

#opk_dat_Mean
#Mean no. of peak data calls
ds$opk_dat_Mean<-ifelse(ds$opk_dat_Mean>=62.23,NA, ds$opk_dat_Mean)
ds$opk_dat_Mean[is.na(ds$opk_dat_Mean)]<-0.0

#mtrcycle
#motor cycle indiactor
summary(ds$mtrcycle)
ds$mtrcycle<-as.factor(ds$mtrcycle)
ds$mtrcycle[is.na(ds$mtrcycle)]<-0

#Customer ID
#Treating it is not required


#Churn
summary(ds$churn)
ds$churn<-as.factor(ds$churn)

#Totrev
#total revenue
ds$totrev<-ifelse(ds$totrev>=1143.6,NA,ds$totrev)
ds$totrev[is.na(ds$totrev)]<-615.50

#Data Prepartion for categoric variables
#area
#Geographic area
summary(ds$area)
ds$area[is.na(ds$area)]<-'HOUSTON AREA'

#hnd_webcap
#Handset web capability
summary(ds$hnd_webcap)
ds$hnd_webcap[is.na(ds$hnd_webcap)]<-'WC'

#ethnic
#ethnicity roll up code
summary(ds$ethnic)
ds$ethnic[is.na(ds$ethnic)]<-'M'

#car_buy
#New or used car buyer
summary(ds$car_buy)
ds$car_buy[is.na(ds$car_buy)]<-'UNKNOWN'

#refurb_new
#handset refurbished or new
summary(ds$refurb_new)
ds$refurb_new[is.na(ds$refurb_new)]<-'N'

#CSA
#communication local service area
#removing this variable as it has 772 levels
ds<-subset(ds,select = -c(csa))

dim(ds)
ds
View(ds)

#numeric variable data prepartion
#plcd_dat_Mean
#mean no. of attempted data calls placed
ds$plcd_dat_Mean<-ifelse(ds$plcd_dat_Mean>=257.5189,NA,ds$plcd_dat_Mean)
ds$plcd_dat_Mean[is.na(ds$plcd_dat_Mean)]<-0.0

#comp_dat_Mean
#mean no. of completed data calls
ds$comp_dat_Mean<-ifelse(ds$comp_dat_Mean>=221.4693,NA,ds$comp_dat_Mean)
ds$comp_dat_Mean[is.na(ds$comp_dat_Mean)]<-0.0

#adjrev
#billing adjusted total revenue over the life of the customer
ds$adjrev<-ifelse(ds$adjrev>=1134.3,NA,ds$adjrev)
ds$adjrev[is.na(ds$adjrev)]<-728.2

#Dropping adjmou as it is related to adjrev
ds<-within(ds, rm(adjmou))



#iwylis_vce_Mean
#Range of number of inbound wireless to wireless voice calls
summary(ds$iwylis_vce_Mean)
unique(ds$iwylis_vce_Mean)
boxplot(ds$iwylis_vce_Mean)
quantile(ds$iwylis_vce_Mean,p=(1:100)/100)
quantile(ds$iwylis_vce_Mean,p=(990:1000)/1000)
#treating variable with inter-quantile range
#1.5*IQR
1.5*(9.333-0)
ds$iwylis_vce_Mean<-ifelse(ds$iwylis_vce_Mean>=13.9995,NA,ds$iwylis_vce_Mean)
ds$iwylis_vce_Mean[is.na(ds$iwylis_vce_Mean)]<-2
boxplot(ds$iwylis_vce_Mean)
hist(ds$iwylis_vce_Mean)

#avgqty
#Average monthly number of calls over the life of the customer
summary(ds$avgqty)
boxplot(ds$avgqty,horizontal = TRUE)
#treating variable with inter-quantile range
#1.5*IQR
1.5*(230.80-63.62)
ds$avgqty<-ifelse(ds$avgqty>=250.77,NA,ds$avgqty)
ds$avgqty[is.na(ds$avgqty)]<-127.60
boxplot(ds$avgqty)

#asl_flag
#Account spending limit
summary(ds$asl_flag)
#no. treatment required as there are no outliers.


#Dividing the data for test and test 
set.seed(5000)
numrow<-sample(nrow(ds),0.80*nrow(ds),replace=TRUE)
train<-ds[numrow,]
test<-ds[-numrow,]

#building model using all the variables
mod1<-glm(formula = churn~.,data = test,family = "binomial")
summary(mod1)

library(car)
vif(mod1)

train<-subset(train,select = -c(mou_Mean,months,totcalls,ovrrev_Mean,rev_Mean,eqpdays,custcare_Mean,adjqty,ovrmou_Mean,avg6qty,avg3qty,plcd_dat_Mean,comp_dat_Mean,adjrev))
mod2<-glm(formula = churn~.,data = train, family = 'binomial')
summary(mod2)

#CREATING DUMMIES FOR SIGNIFICANT VARIABLES
train$income1<-ifelse(train$income==1,1,0)
train<-within(train,rm(income1))
train$income2<-ifelse(train$income==2,1,0)
train$income3<-ifelse(train$income==3,1,0)
train$income4<-ifelse(train$income==4,1,0)
train$income5<-ifelse(train$income==5,1,0)
train$income6<-ifelse(train$income==6,1,0)
train$income9<-ifelse(train$income==9,1,0)

train$crclscodA2<-ifelse(train$crclscod=='A2',1,0)
train$crclscodB2<-ifelse(train$crclscod=='B2',1,0)
train$crclscodD4<-ifelse(train$crclscod=='D4',1,0)
train$crclscodD5<-ifelse(train$crclscod=='D5',1,0)
train$crclscodDA<-ifelse(train$crclscod=='DA',1,0)
train$crclscodE4<-ifelse(train$crclscod=='E4',1,0)
train$crclscodEA<-ifelse(train$crclscod=='EA',1,0)
train$crclscodGA<-ifelse(train$crclscod=='GA',1,0)
train$crclscodAA<-ifelse(train$crclscod=='AA',1,0)

train$asl_flagY<-ifelse(train$asl_flag=='Y',1,0)

train$prizm_social_oneR<-ifelse(train$prizm_social_one=='R',1,0)
train$prizm_social_oneT<-ifelse(train$prizm_social_one=='T',1,0)
train$prizm_social_oneS<-ifelse(train$prizm_social_one=='S',1,0)

summary(train$area)
train$areaDC<-ifelse(train$area=='DC/MARYLAND/VIRGINIA AREA',1,0)
train$areaMIDWEST<-ifelse(train$area=='MIDWEST AREA',1,0)
train$areaNORTHWEST<-ifelse(train$area=='NORTHWEST/ROCKY MOUNTAIN AREA',1,0)
train$areaOHIO<-ifelse(train$area=='OHIO AREA',1,0)
train$areaPHILADELPHIA<-ifelse(train$area=='PHILADELPHIA AREA',1,0)
train$areaSOUTHFLORIDA<-ifelse(train$area=='SOUTH FLORIDA AREA',1,0)
train$areaTENNESSEE<-ifelse(train$area=='TENNESSEE AREA',1,0)


train$refurb_newR<-ifelse(train$refurb_new=='R',1,0)

train$maritalS<-ifelse(train$marital=='S',1,0)

train$ethnicC<-ifelse(train$ethnic=='C',1,0)
train$ethnicO<-ifelse(train$ethnic=='O',1,0)
train$ethnicZ<-ifelse(train$ethnic=='Z',1,0)

train$models2<-ifelse(train$models==2,1,0)
train$models4<-ifelse(train$models==4,1,0)

train$truck1<-ifelse(train$truck==1,1,0)

mod3<-glm(formula = churn~totmrc_Mean+avgrev+rev_Range+change_mou+drop_blk_Mean+totrev+income+iwylis_vce_Mean+comp_vce_Mean+plcd_vce_Mean+avg3mou+avgmou+avgqty+crclscod+asl_flag+prizm_social_one+age1+hnd_price+actvsubs+uniqsubs+truck+models+ethnic+marital+refurb_new+area,data = train, family = 'binomial')
summary(mod3)

#mod4<-glm(formula = churn~totmrc_Mean+avgrev+rev_Range+change_mou+drop_blk_Mean+income2+totrev+income3+income4+income5+income6+income9+,data = train,family = "binomial")

mod4<-glm(formula = churn~totmrc_Mean+avgrev+rev_Range+change_mou+drop_blk_Mean+totrev+income2+income3+income3+income4+income5+income6+income9+iwylis_vce_Mean+comp_vce_Mean+plcd_vce_Mean+avg3mou+avgmou+avgqty+crclscodA2+crclscodAA+crclscodGA+crclscodEA+crclscodE4+crclscodDA+crclscodD5+crclscodD4+crclscodB2+asl_flagY+prizm_social_oneR+prizm_social_oneT+prizm_social_oneS+age1+hnd_price+actvsubs+uniqsubs+truck1+models2+models4+ethnicZ+ethnicO+ethnicC+maritalS+refurb_newR+areaTENNESSEE+areaSOUTHFLORIDA+areaOHIO+areaPHILADELPHIA+areaNORTHWEST+areaMIDWEST+areaDC,data = train, family = 'binomial')
summary(mod4)


mod5<-glm(formula = churn~totmrc_Mean+avgrev+rev_Range+change_mou+drop_blk_Mean+totrev+income3+income4+income6+iwylis_vce_Mean+comp_vce_Mean+plcd_vce_Mean+avg3mou+avgmou+avgqty+crclscodA2+crclscodAA+crclscodGA+crclscodEA+crclscodE4+asl_flagY+prizm_social_oneT+age1+hnd_price+actvsubs+uniqsubs+models2+ethnicZ+ethnicO+ethnicC+maritalS+refurb_newR+areaTENNESSEE+areaSOUTHFLORIDA+areaOHIO+areaNORTHWEST+areaMIDWEST+areaDC,data = train, family = 'binomial')
summary(mod5)



mod6<-glm(formula = churn~totmrc_Mean+avgrev+rev_Range+change_mou+drop_blk_Mean+totrev+income4+iwylis_vce_Mean+comp_vce_Mean+plcd_vce_Mean+avg3mou+avgmou+avgqty+crclscodA2+crclscodE4+asl_flagY+prizm_social_oneT+age1+hnd_price+actvsubs+uniqsubs+models2+ethnicZ+ethnicO+maritalS+refurb_newR+areaTENNESSEE+areaNORTHWEST+areaMIDWEST+areaDC,data = train, family = 'binomial')
summary(mod6)


mod7<-glm(formula = churn~totmrc_Mean+avgrev+rev_Range+change_mou+drop_blk_Mean+totrev+iwylis_vce_Mean+comp_vce_Mean+plcd_vce_Mean+avg3mou+avgmou+avgqty+crclscodA2+crclscodE4+asl_flagY+prizm_social_oneT+age1+hnd_price+actvsubs+uniqsubs+models2+ethnicZ+ethnicO+refurb_newR+areaTENNESSEE+areaNORTHWEST+areaMIDWEST+areaDC,data = train, family = 'binomial')
summary(mod7)



vif(mod7)

#creating dummy variables in test data
test$income1<-ifelse(test$income==1,1,0)
test<-within(test,rm(income1))
test$income2<-ifelse(test$income==2,1,0)
test$income3<-ifelse(test$income==3,1,0)
test$income4<-ifelse(test$income==4,1,0)
test$income5<-ifelse(test$income==5,1,0)
test$income6<-ifelse(test$income==6,1,0)
test$income9<-ifelse(test$income==9,1,0)

test$crclscodA2<-ifelse(test$crclscod=='A2',1,0)
test$crclscodB2<-ifelse(test$crclscod=='B2',1,0)
test$crclscodD4<-ifelse(test$crclscod=='D4',1,0)
test$crclscodD5<-ifelse(test$crclscod=='D5',1,0)
test$crclscodDA<-ifelse(test$crclscod=='DA',1,0)
test$crclscodE4<-ifelse(test$crclscod=='E4',1,0)
test$crclscodEA<-ifelse(test$crclscod=='EA',1,0)
test$crclscodGA<-ifelse(test$crclscod=='GA',1,0)
test$crclscodAA<-ifelse(test$crclscod=='AA',1,0)

test$asl_flagY<-ifelse(test$asl_flag=='Y',1,0)

test$prizm_social_oneR<-ifelse(test$prizm_social_one=='R',1,0)
test$prizm_social_oneT<-ifelse(test$prizm_social_one=='T',1,0)
test$prizm_social_oneS<-ifelse(test$prizm_social_one=='S',1,0)

summary(test$area)
test$areaDC<-ifelse(test$area=='DC/MARYLAND/VIRGINIA AREA',1,0)
test$areaMIDWEST<-ifelse(test$area=='MIDWEST AREA',1,0)
test$areaNORTHWEST<-ifelse(test$area=='NORTHWEST/ROCKY MOUNTAIN AREA',1,0)
test$areaOHIO<-ifelse(test$area=='OHIO AREA',1,0)
test$areaPHILADELPHIA<-ifelse(test$area=='PHILADELPHIA AREA',1,0)
test$areaSOUTHFLORIDA<-ifelse(test$area=='SOUTH FLORIDA AREA',1,0)
test$areaTENNESSEE<-ifelse(test$area=='TENNESSEE AREA',1,0)


test$refurb_newR<-ifelse(test$refurb_new=='R',1,0)

test$maritalS<-ifelse(test$marital=='S',1,0)

test$ethnicC<-ifelse(test$ethnic=='C',1,0)
test$ethnicO<-ifelse(test$ethnic=='O',1,0)
test$ethnicZ<-ifelse(test$ethnic=='Z',1,0)

test$models2<-ifelse(test$models==2,1,0)
test$models4<-ifelse(test$models==4,1,0)

test$truck1<-ifelse(test$truck==1,1,0)

test<-subset(test,select = -c(mou_Mean,months,totcalls,ovrrev_Mean,rev_Mean,eqpdays,custcare_Mean,adjqty,ovrmou_Mean,avg6qty,avg3qty,plcd_dat_Mean,comp_dat_Mean,adjrev))

mod8<-glm(formula = churn~totmrc_Mean+rev_Range+avgrev+change_mou+totrev+iwylis_vce_Mean+comp_vce_Mean+plcd_vce_Mean+avgqty+asl_flagY+age1+hnd_price+uniqsubs+ethnicZ+ethnicO+refurb_newR,data = train, family = 'binomial')
summary(mod8)

pred<-predict(mod8,type = "response",newdata = test)
head(pred)

table(ds$churn)/nrow(ds)
pred<-ifelse(pred>=0.23,1,0)
library(caret)
library(gains)
library(ROCR)

pred<-as.factor(pred)
confusionMatrix(pred,test$churn,positive='1')


pred<-prediction(predict(mod8,type = "response",newdata = test),test$churn)
perf<-performance(pred,"auc")
auc<-unlist(perf@y.values)
auc

roc<-performance(pred,"tpr","fpr")
plot(roc,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))



#gains chart
test$churn<-as.numeric(test$churn)
gains(test$churn, predict(mod8,type = 'response', newdata = test), groups = 10)
test$prob<-predict(mod8,type='response',newdata = test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
targeted<-test[test$prob>0.3264274&test$prob<=0.6728382,"Customer_ID"] 
length(targeted)
