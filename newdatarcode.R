rm(list=ls())

##====set the working directory/ working area
setwd("D:\\PURITY\\data malawi\\newdata")

##=====******* load the required libraries*************
library(xlsx)
library(epicalc)
#to be able to conver number in scientific fomat to integers
options(scipen=10) 
##***************************************************
##*******read in the datamalawi*********

datamalawi<-read.csv('datamalawi.csv',header=T,skip=1)

###====deleting the empty id numbers
datamalawi<-datamalawi[-c(1001:1499),]
datamalawi<-datamalawi[,-c(890:906)]
names(datamalawi)
##********* read in the data with out the empty id numbers
datamalawi<-subset(datamalawi,!is.na(ID.Number))
##verifying if the emty are deleted

check<-subset(datamalawi,is.na(ID.Number))


#****************************************************************************
# to check all the duplicated id numbers in the data malawi
check<-datamalawi[(duplicated(datamalawi[,c('ID.Number')])),]

#to remove all the duplicated in numbers
datamalawi<-datamalawi[!(duplicated(datamalawi[,c('ID.Number')])),]
#write.xlsx(check,'duplicated_id.xlsx',col.names=T,row.names=T,sheetName='duplicated_id_number')
dim(datamalawi)
names(datamalawi)

#*********************************************************************************
##================HOUSEHOLD DEMOGRAPHY====================
#==========================================================
hhdemog<-subset(datamalawi,select=c(Total.persons:Partially.Engaged3))
names(hhdemog)
#check<-hhdemog[anyDuplicated(hhdemog[c("ID.Number")]),]
names(hhdemog)
class(hhdemog$Total.persons)
hhdemog$Total.persons<-as.numeric(hhdemog$Total.persons)

library(epicalc)
library(plyr)
tab1(hhdemog$Total.persons)

#to get the household total/total number of persons in a household
rename.var(Total.persons,Household_Total,hhdemog)

#RENAMING THOSE FULLY DOING FARMING
rename.var(Fully.Engaged,Male_Farmer,hhdemog)
rename.var(Fully.Engaged2,Female_Farmer,hhdemog)
rename.var(Fully.Engaged3,Child_farmer,hhdemog)





#males over 15 years 
hhdemog$Male_Farmer <-as.numeric(hhdemog$Male_Farmer)
check<-subset(hhdemog,is.na(hhdemog$Male_Farmer))
hhdemog$Male_Farmer[hhdemog$Male_Farmer==0]<-NA
hhdemog$Male_Farmer[hhdemog$Male_Farmer==999]<-NA

#****************FEMALE FARMER****************************
#females over 15 years
class(hhdemog$Female_farmer ) 

hhdemog$Female_Farmer<-as.numeric(hhdemog$Female_Farmer)
check<-subset(hhdemog,hhdemog$Female_Farmer>=5)
check<-subset(hhdemog,is.na(hhdemog$Female_Farmer))
tab1(hhdemog$Female_Farmer)
hhdemog$Female_Farmer[hhdemog$Female_Farmer==0]<-NA
hhdemog$Female_Farmer[hhdemog$Female_Farmer==999]<-NA

#*****************************************************
#==============CHILD FARMER================================
#child doing farming
class(hhdemog$Child_farmer)
hhdemog$Child_farmer<-as.numeric(hhdemog$Child_farmer)
check<-subset(hhdemog,hhdemog$Child_farmer>1)

##DOING REPLACEMENT OF 999 AND 0'S WITH NA IN CHILD FARMER
hhdemog$Child_farmer[hhdemog$Child_farmer==999]<-NA
hhdemog$Child_farmer[hhdemog$Child_farmer==0]<-NA
demog<-subset(hhdemog,select=c(Household_Total,Male_Farmer,Female_Farmer))
names(data)

#*******************RUNNING BASIC DESCRPTION ON HOUSEHOLD DEMOGRAPHY
#*******************
ddply(demog,.(Household_Total,Male_farmer),summarise,mean=mean(Male_farmer),sd=sd(Household_Total),var=var(Male_farmer),lengthmale=length(Male_farmer))
summary(demog$Household_Total)
##===============================================================================
##=================BASICHOUSEHOLD INFORMATION=====================================
##================================================================================
## basic household information variables of interest
data<-subset(datamalawi,select=c(ID.Number:District,Relationship:Trees))
names(data)
tab1(data$District)
##relationship of the interviewee to the household head.
data$Relationship<-factor(data$Relationship,labels=c("Head","Spouse","Son","Daughter","Other relative","Other"),levels=c(1:6))
rename.var(Relationship,intervewiee_relation_HH,data)
#=====sex.1 of the intervieweee
data$Sex.1<-as.numeric(data$Sex.1)
class(data$Sex.1)
data$Sex.1  =2-data$Sex.1  
data$Sex.1  <-as.factor(data$Sex.1  )
class(data$Sex.1  )
data$Sex.1  <-factor(data$Sex.1  ,labels=c("Male","Female"),levels=c(0:1))
rename.var(Sex.1,sex_of_intervewee,data)
#========================================================
#============sex.2 sex of the house hold head to be a factor variable which is binary
#========================================================

data$Sex.2<-as.numeric(data$Sex.2)
class(data$Sex.2)
data$Sex.2=2-data$Sex.2
data$Sex.2<-as.factor(data$Sex.2)
class(data$Sex.2)
data$Sex.2<-factor(data$Sex.2,labels=c("Male","Female"),levels=c(0:1))
tab1(data$Sex.2)
rename.var(Sex.2,Sex_Household_Head,data)
#================================================================
#dealing with the====== age========== of the household
#====================================================
#=================================================
##====HOUSEHOLD HEADS ABOVE 20 years==============
#=================================================
#data$Age1<-as.numeric(data$Age)

#data$Age1<-ifelse(data$Age>20,1,0)
##data$age_use
#class(data$Age1)
#data<-subset(data,select=c(ID.Number:District,Sex.2,Age1,Education,Occupation,Participation,Farm.Size,Trees))
#data$Age1<-factor(data$Age1,levels=c(0,1),labels=c("less than 20 years","Greater than 20 years"))
#tab1(data$Age1)
#=============================MARITAL STATUS OF THE HEAD OF THE HOUSEHOLD===============================================
#=========marital status of the head of the house
data$Marital.Status<-factor(data$Marital.Status,levels=c(1:4),labels=c("Single","Married","Divorced","Widowed"))
tab1(data$Marital.Status)

#=====================================================
#==dealing with the ======education level==== of the household head
#=============================================================

class(data$Education)
data$Education<-as.numeric(data$Education)
names(data)
data$Education[data$Education==12]<=NA
data$Education<-factor(data$Education,
                       labels=c("None","Primary school(not completed)",
                                "primary school(completed)",
                                "Secondary school",
                                "Post-secondary","Others(specify)")
                       ,levels=c(1:6))

tab1(data$Education)
rename.var(Education,Education_HH_Head,data)
data<-subset(data,select=c(Sex_Household_Head,Education_HH_Head,Constraint.1,Constraint.2))
data$Constraint.1<-factor(data$Constraint.1,levels=c(1:8),labels=c("No market for farm produce","Rainfall/erosion","Poor soil fertility","Pests/disease damage","Sickness/HIV","Inadequate labor","Lack of food/hunger","Others"))
data$Constraint.2<-factor(data$Constraint.2,levels=c(1:8),labels=c("No market for farm produce","Rainfall/erosion","Poor soil fertility","Pests/disease damage","Sickness/HIV","Inadequate labor","Lack of food/hunger","Others"))
rename.var(Constraint.1,Critical_Constraint_one,data)
rename.var(Constraint.2,Critical_Constraint_two,data)
data<-cbind(demog,data)
names(data)
#=====================================================
#===EDUCATION VARIABLE===============================
##householdhead with=== secondary education=== and above
#======================================================
#data$Education<-ifelse(data$Education==1,1,0)
#class(data$Education)
#data$Education<-factor(data$Education,levels=c(0,1),labels=c("below secondary education","above secondary education"))
#tab1(data$Education)

#==================Occupation=====================================
#=======Occupation=========== of the head of the household
#class(data$Occupation)

##check for any missing occupation values/ if athe occupation of the house hold head is farming
#data$Occupation<-factor(data$Occupation,levels=c(1:4),labels=c("Farming","Regular non-farm employment","Small-scale business","Others(specify)"))

#=============================
#==============farmsizes variable===============================
#=====================================================
#dealing with the farm sizes where we will 
#pick a farmer who had  more than one acre

class(data$Farm.Size ) 


#=====farm size===============



#data$Farm.Size_use<-as.factor(data$Farm.Size_use)
#=====================================================
#=================participation in the AFSP============
#=======================================================
#class(data$Participation)
#data$Participation=2-data$Participation
#data$Participation<-factor(data$Participation,levels=c(0:1),labels=c("No","Yes"))
#data<-subset(data,select=c(ID.Number:District,Sex.2,Age1,Occupation,Participation,Trees,Education_use,Farm.Size_use))
#tab1(data$Participation)
#===================Engagement========================
#data$Engagement[data$Engagement==999]<-NA
#==================================================
#==============Trees==============================
#===================================================
#class(data$Trees)
#data$Trees<-as.numeric(data$Trees)
#class(data$Trees)
#data$Trees[data$Trees==999]<-NA
#to check for any missing trees
#check<-subset(data,is.na(Trees))

data$Trees_use<-as.factor(data$Trees_use)

names(data)
data$Wetland[data$Wetland==999]<-NA
data$Acquired.1[data$Acquired.1==999]<-NA
basichouseholdinfo<-data.frame(summ(data)$table)
write.xlsx(basichouseholdinfo,"aggregate.xlsx",col.names=T,row.names=T,sheetName="Basichhinfo")


##***************************************************************************
##==============FOOD SECURITY===============================================

#maize deficit months
maizedeficit<-subset(datamalawi,select=c(MaizeDeficit_From:MaizeDeficit_To))
maizedeficit$MaizeDeficit_From[maizedeficit$MaizeDeficit_From==999]<-NA
maizedeficit$MaizeDeficit_To[maizedeficit$MaizeDeficit_To==999]<-NA
maizedeficit$maize_deficit_months<-abs(maizedeficit$MaizeDeficit_From-maizedeficit$MaizeDeficit_To)
dataa<-subset(maizedeficit,select=c(maize_deficit_months))
data<-cbind(data,dataa)
names(data)

##legumes deficit months
legumedeficit<-subset(datamalawi,select=c(LegumeDeficit_From:LegumeDeficit_To))
legumedeficit$LegumeDeficit_From[legumedeficit$LegumeDeficit_From==999]<-NA
legumedeficit$LegumeDeficit_To[legumedeficit$LegumeDeficit_To==999]<-NA
legumedeficit$LegumeDeficit_months<-abs(legumedeficit$LegumeDeficit_From-legumedeficit$LegumeDeficit_To)
dataa<-subset(legumedeficit,select=c(LegumeDeficit_months))
data<-cbind(data,dataa)
names(data)

##do you purchase vegetables
vegetables<-subset(datamalawi,select=c(Vegetables:Purchase))
rename.var(Vegetables,Months_purchase_Vegetables,vegetables)
vegetables$Purchase<-factor(vegetables$Purchase,levels=c(1:2),labels=c("Yes","No"))
rename.var(Purchase,Purchase_vegetables,vegetables)
dataa<-subset(vegetables,select=c(Purchase_vegetables,Months_purchase_Vegetables))
#data<-cbind(data,dataa)
names(data)
###how to survive during the shortage/coping mechanism

survive<-subset(datamalawi,select=c(survive.food.shortage.1:survive..food.shortage.4))
survive$survive.food.shortage.1<-factor(survive$survive.food.shortage.1,levels=c(1:15)
                                        ,labels=c("Skip meals","change composition of meals","Sell fruits","Eat fruits"
                                                  ,"Seek ganyu","Do seasonal contracts","Borrow"
                                                  ,"Beg for food","Timber sales","Sell firewood",
                                                  "Sell charcoal","Exchange","Sell assets","Sell livestock","Others(specify"))
survive$survive.food.shortage.2  <-factor(survive$survive.food.shortage.2  ,levels=c(1:15)
                                          ,labels=c("Skip meals","change composition of meals","Sell fruits","Eat fruits"
                                                    ,"Seek ganyu","Do seasonal contracts","Borrow"
                                                    ,"Beg for food","Timber sales","Sell firewood",
                                                    "Sell charcoal","Exchange","Sell assets","Sell livestock","Others(specify"))
survive$survive.food.shortage.3  <-factor(survive$survive.food.shortage.3  ,levels=c(1:15)
                                          ,labels=c("Skip meals","change composition of meals","Sell fruits","Eat fruits"
                                                    ,"Seek ganyu","Do seasonal contracts","Borrow"
                                                    ,"Beg for food","Timber sales","Sell firewood",
                                                    "Sell charcoal","Exchange","Sell assets","Sell livestock","Others(specify"))
survive$survive..food.shortage.4<-factor(survive$survive..food.shortage.4,levels=c(1:15)
                                         ,labels=c("Skip meals","change composition of meals","Sell fruits","Eat fruits"
                                                   ,"Seek ganyu","Do seasonal contracts","Borrow"
                                                   ,"Beg for food","Timber sales","Sell firewood",
                                                   "Sell charcoal","Exchange","Sell assets","Sell livestock","Others(specify"))
###causes of food shortage in the house holds adn coping mechanisms
foodshortage<-subset(datamalawi,select=c(Food.Shortage.Cause.1:Food.Shortage.Cause.4))
foodshortage$Food.Shortage.Cause.1<-factor(foodshortage$Food.Shortage.Cause.1,levels=c(1:11)
                                           ,labels=c("Shortage of land to cultivate","Poor soil fertility","Lack of labour"
                                                     ,"Fertilizer not available  timely","Poor rainfall","Sickness","Just laziness","Improved seeds not available","High cost of food","Lack of income","Others(specify)"))
foodshortage$Food.Shortage.Cause.2  <-factor(foodshortage$Food.Shortage.Cause.2  ,levels=c(1:11)
                                             ,labels=c("Shortage of land to cultivate","Poor soil fertility","Lack of labour"
                                                       ,"Fertilizer not available  timely","Poor rainfall","Sickness","Just laziness","Improved seeds not available","High cost of food","Lack of income","Others(specify)"))
foodshortage$Food.Shortage.Cause.3    <-factor(foodshortage$Food.Shortage.Cause.3    ,levels=c(1:11)
                                               ,labels=c("Shortage of land to cultivate","Poor soil fertility","Lack of labour"
                                                         ,"Fertilizer not available  timely","Poor rainfall","Sickness","Just laziness","Improved seeds not available","High cost of food","Lack of income","Others(specify)"))
foodshortage$Food.Shortage.Cause.4<-factor(foodshortage$Food.Shortage.Cause.4,levels=c(1:11)
                                           ,labels=c("Shortage of land to cultivate","Poor soil fertility","Lack of labour"
                                                     ,"Fertilizer not available  timely","Poor rainfall","Sickness","Just laziness","Improved seeds not available","High cost of food","Lack of income","Others(specify)"))
dataa<-subset(survive,select=c(survive.food.shortage.1,survive.food.shortage.2,survive.food.shortage.3,survive..food.shortage.4))
data<-cbind(data,dataa)
names(dataa)

dataa<-subset(foodshortage,select=c(Food.Shortage.Cause.1,Food.Shortage.Cause.2,Food.Shortage.Cause.3,Food.Shortage.Cause.4))
names(dataa)
data<-cbind(data,dataa)
names(data)



##======================NUTRITION=================================================

meals<-subset(datamalawi,select=c(Meal_LP:Meal_HP))
meals$Meal_LP<-factor(meals$Meal_LP,levels=c(1:3),labels=c("Once","Twice","Thrice"))
meals$Meal_HP<-factor(meals$Meal_HP,levels=c(1:3),labels=c("Once","Twice","Thrice"))
tab1(meals$Meal_LP)
tab1(meals$Meal_HP)
dataa<-subset(meals,select=c(Meal_LP,Meal_HP))
data<-cbind(data,dataa)
names(data)
##apart form maize the other foods fed to the family
other<-subset(datamalawi,select=c(Other_food1:Other_food3))
other$Other_food1<-factor(other$Other_food1,levels=c(1:12),labels=c("None","Beans","Plantain","Banana","Sweet potato",
                                                                    "Rice","Cassava","Sorghum","Millet","Irish potato","Pumpkin","Others"))
other$Other_food2<-factor(other$Other_food2,levels=c(1:12),labels=c("None","Beans","Plantain","Banana","Sweet potato",
                                                                    "Rice","Cassava","Sorghum","Millet","Irish potato","Pumpkin","Others"))
other$Other_food3<-factor(other$Other_food3,levels=c(1:12),labels=c("None","Beans","Plantain","Banana","Sweet potato",
                                                                    "Rice","Cassava","Sorghum","Millet","Irish potato","Pumpkin","Others"))
dataa<-subset(other,select=c(Other_food1,Other_food2,Other_food3))                                                               
data<-cbind(data,dataa)

##======milk consumption
milkconsumption<-subset(datamalawi,select=c(Milk_adultFreq:Milk.Sales))
names(milkconsumption)
## frequency in which the adult members in the household consume milk per month
milkconsumption$Milk_adultFreq[milkconsumption$Milk_adultFreq==999]<-NA
milkconsumption$Milk_adultFreq[milkconsumption$Milk_adultFreq==0]<-NA
##frequency in which children in the households consume milk per month
milkconsumption$Milk_childFreq[milkconsumption$Milk_childFreq==999]<-NA
milkconsumption$Milk_childFreq[milkconsumption$Milk_childFreq==0]<-NA
rename.var(Milk_childFreq,MilkFreq_Child,milkconsumption)
rename.var(Milk_adultFreq,MilkFreq_Adult,milkconsumption)
#quantity of milk consumed in the houshold by children per day when milk is available
milkconsumption$Quantity.of.milk.per.child.per.day[milkconsumption$Quantity.of.milk.per.child.per.day==999]<-NA
milkconsumption$Quantity.of.milk.per.child.per.day[milkconsumption$Quantity.of.milk.per.child.per.day==0]<-NA
##milk type consumed
milkconsumption$Milk_Type[milkconsumption$Milk_Type==999]<-NA
milkconsumption$Milk_Type[milkconsumption$Milk_Type==0]<-NA
milkconsumption$Milk_Type<-factor(milkconsumption$Milk_Type,levels=c(1:5),labels=c("Cow milk","Goat milk","Both","Powdered","All"))
tab1(milkconsumption$Milk_Type)
##no of milk producing cows in each of the households
class(milkconsumption$No..Of.milk.Cows)
milkconsumption$No..Of.milk.Cows[milkconsumption$No..Of.milk.Cows==999]<-NA
milkconsumption$No..Of.milk.Cows[milkconsumption$No..Of.milk.Cows==0]<-NA
summ(milkconsumption$No..Of.milk.Cows)
boxplot(milkconsumption$No..Of.milk.Cows,col=2)
##milk produced by  each of the cows per day
class(milkconsumption$Cow.Produces)
rename.var(Cow.Produces,Cow_Milk_Production_Daily,milkconsumption)
milkconsumption$Cow_Milk_Production_Daily[milkconsumption$Cow_Milk_Production_Daily==999]<-NA
milkconsumption$Cow_Milk_Production_Daily[milkconsumption$Cow_Milk_Production_Daily==0]<-NA
summ(milkconsumption$Cow_Milk_Production_Daily)
table(milkconsumption$Cow_Milk_Production_Daily)
##milk producing goats in each of the households
milkconsumption$No..of.milk.Goats[milkconsumption$No..of.milk.Goats==999] <-NA
milkconsumption$No..of.milk.Goats[milkconsumption$No..of.milk.Goats==0] <-NA
summ(milkconsumption$No..of.milk.Goats)
table(milkconsumption$No..of.milk.Goats)
boxplot(milkconsumption$No..of.milk.Goats,col=2)
class(milkconsumption$No..of.milk.Goats)
##milk production by each of the  goats per dayin each of the households
milkconsumption$Goat.Production [milkconsumption$Goat.Production==999]<-NA
milkconsumption$Goat.Production [milkconsumption$Goat.Production==0]<-NA
summ(milkconsumption$Goat.Production )
table(milkconsumption$Goat.Production )
summary(milkconsumption$Goat.Production )
##milk purchsed  in each of the households per week
milkconsumption$Milk.Purchase<-as.numeric(milkconsumption$Milk.Purchase)
milkconsumption$Milk.Purchase[milkconsumption$Milk.Purchase==999]<-NA
milkconsumption$Milk.Purchase[milkconsumption$Milk.Purchase==0]<-NA
milkconsumption$Milk.Purchase[milkconsumption$Milk.Purchase==""]<-NA
summ(milkconsumption$Milk.Purchase)
boxplot(milkconsumption$Milk.Purchase)
class(milkconsumption$Milk.Purchase)
##milk sales per week
milkconsumption$Milk.Sales[milkconsumption$Milk.Sales==999]<-NA
milkconsumption$Milk.Sales[milkconsumption$Milk.Sales==0]<-NA
milkconsumption$Milk.Sales[milkconsumption$Milk.Sales==""]<-NA
summ(milkconsumption$Milk.Sales)
table(milkconsumption$Milk.Sales)

dataa<-subset(milkconsumption,select=c(MilkFreq_Adult,MilkFreq_Child,Milk_Type))
data<-cbind(data,dataa)
names(data)
##meat consumption
meatconsumption<-subset(datamalawi,select=c(Meat_adultFreq,Meat_childFreq,Fish_adultFreq,Fish_childFreq,Type.of.Meat_1,Type.of.Meat_2,Meat_source1,Meat_source2))
names(meatconsumption)

##type of meat normally consumed by the households
class(meatconsumption$Type.of.Meat_1)
##gives the summary of the subset dataframe data
library(pastecs)
stat.desc(meatconsumption)

meatconsumption$Type.of.Meat_1[meatconsumption$Type.of.Meat_1==999]<-NA
meatconsumption$Type.of.Meat_1[meatconsumption$Type.of.Meat_1==0]<-NA
meatconsumption$Type.of.Meat_1[meatconsumption$Type.of.Meat_1==""]<-NA
meatconsumption$Type.of.Meat_1<-factor(meatconsumption$Type.of.Meat_1,levels=c(1:6),labels=c("Beef","Chicken","Goat","Fish","Pork","Others"))
tab1(meatconsumption$Type.of.Meat_1)

meatconsumption$Type.of.Meat_2[meatconsumption$Type.of.Meat_2==999]<-NA
meatconsumption$Type.of.Meat_2[meatconsumption$Type.of.Meat_2==0]<-NA
meatconsumption$Type.of.Meat_2[meatconsumption$Type.of.Meat_2  ==""]<-NA
meatconsumption$Type.of.Meat_2<-factor(meatconsumption$Type.of.Meat_2,levels=c(1:6),labels=c("Beef","Chicken","Goat","Fish","Pork","Others"))
tab1(meatconsumption$Type.of.Meat_2)

##frequency of adult members of the household eating meat per month in the year
meatconsumption$Meat_adultFreq[meatconsumption$Meat_adultFreq==999]<-NA
meatconsumption$Meat_adultFreq[meatconsumption$Meat_adultFreq==0]<-NA
meatconsumption$Meat_adultFreq[meatconsumption$Meat_adultFreq==""]<-NA

##Meat_childFreq
meatconsumption$Meat_childFreq[meatconsumption$Meat_childFreq==999]<-NA
meatconsumption$Meat_childFreq[meatconsumption$Meat_childFreq==0]<-NA

meatconsumption$Fish_adultFreq[meatconsumption$Fish_adultFreq==999]<-NA
meatconsumption$Fish_adultFreq[meatconsumption$Fish_adultFreq==0]<-NA

meatconsumption$Fish_childFreq[meatconsumption$Fish_childFreq==999]<-NA
meatconsumption$Fish_childFreq[meatconsumption$Fish_childFreq==0]<-NA
##sources of meat 
meatconsumption$Meat_source1[meatconsumption$Meat_source1==999]<-NA
meatconsumption$Meat_source1[meatconsumption$Meat_source1==0]<-NA
meatconsumption$Meat_source1<-factor(meatconsumption$Meat_source1,levels=c(1:6),labels=c("Own animal","Buy in village","Buy in town","Got from fishing","Hunting","Others"))
tab1(meatconsumption$Meat_source1)

meatconsumption$Meat_source2[meatconsumption$Meat_source2==999]<-NA
meatconsumption$Meat_source2[meatconsumption$Meat_source2==0]<-NA
meatconsumption$Meat_source2<-factor(meatconsumption$Meat_source2,levels=c(1:6),labels=c("Own animal","Buy in village","Buy in town","Got from fishing","Hunting","Others"))
tab1(meatconsumption$Meat_source2)
dataa<-subset(meatconsumption,select=c(Type.of.Meat_1:Meat_source2))
rename.var(Meat_adultFreq,MeatFreq_Adult,meatconsumption)
rename.var(Meat_childFreq,MeatFreq_Child,meatconsumption)
rename.var(Fish_adultFreq,FishFreq_Adult,meatconsumption)
rename.var(Fish_childFreq,FishFreq_Child,meatconsumption)
rename.var(Type.of.Meat_1,Meat_type1,meatconsumption)
rename.var(Type.of.Meat_2,Meat_Type2,meatconsumption)
rename.var(Meat_source1,Meat_source1,meatconsumption)
rename.var(Meat_source2,Meat_source2,meatconsumption)
meatconsumption<-subset(meatconsumption,select=c(MeatFreq_Adult,MeatFreq_Child,FishFreq_Adult,FishFreq_Child,Meat_type1,Meat_Type2,Meat_source1,Meat_source2))

data<-cbind(data,meatconsumption)
names(data)
#*************************************************************************
##****************house hold fruit consumption

householdfruitconsumption<-subset(datamalawi,select=c(Mango:Own.Fruit))
names(householdfruitconsumption)
##how often adult members eat fruits per week DURING TH EFRUIT SEASON
householdfruitconsumption$Adult.Fruit.per.week[householdfruitconsumption$Adult.Fruit.per.week==999]<-NA
householdfruitconsumption$Adult.Fruit.per.week[householdfruitconsumption$Adult.Fruit.per.week==0]<-NA

##how often CHILDREN eat fruits per week DURING THE FRUIT SEASON
householdfruitconsumption$Child.fruit.per.week[householdfruitconsumption$Child.fruit.per.week==999]<-NA
householdfruitconsumption$Child.fruit.per.week[householdfruitconsumption$Child.fruit.per.week==0]<-NA

##OWN FRUT GARDEN
householdfruitconsumption$Own.Fruit[householdfruitconsumption$Own.Fruit==999]<-NA
householdfruitconsumption$Own.Fruit[householdfruitconsumption$Own.Fruit==0]<-NA
dataa<-subset(householdfruitconsumption,select=c(Adult.Fruit.per.week,Child.fruit.per.week))
rename.var(Adult.Fruit.per.week,FruitFreq_Adult,householdfruitconsumption)
rename.var(Child.fruit.per.week,FruitFreq_Child,householdfruitconsumption)
householdfruitconsumption<-subset(householdfruitconsumption,select=c(FruitFreq_Adult,FruitFreq_Child))
data<-cbind(data,householdfruitconsumption)
names(data)
###===============================================================================
##========================FARMING PRACTICES=======================================
##================================================================================
farmingpractices<-subset(datamalawi,select=c(Crop.1.Upland:seed15))
names(farmingpractices)

farmingpractices$Crop.1.Upland[farmingpractices$Crop.1.Upland==999]<-NA
farmingpractices$Crop.1.Upland<- ifelse (farmingpractices$Crop.1.Upland>=1,"Yes","No")

farmingpractices$Crop.1.Wetland[farmingpractices$Crop.1.Wetland  ==999]<-NA
farmingpractices$Crop.1.Wetland<- ifelse (farmingpractices$Crop.1.Wetland>=1,"Yes","No")

farmingpractices$Crop.2.Upland[farmingpractices$Crop.2.Upland  ==999]<-NA
farmingpractices$Crop.2.Upland<- ifelse (farmingpractices$Crop.2.Upland>=1,"Yes","No")

farmingpractices$Crop.2.Wetland  [farmingpractices$Crop.2.Wetland    ==999]<-NA
farmingpractices$Crop.2.Wetland  <- ifelse (farmingpractices$Crop.2.Wetland  >=1,"Yes","No")

farmingpractices$Crop.2.Wetland[farmingpractices$Crop.2.Wetland==999]<-NA
farmingpractices$Crop.2.Wetland<- ifelse (farmingpractices$Crop.2.Wetland>=1,"Yes","No")

farmingpractices$Crop.3.Upland[farmingpractices$Crop.3.Upland==999]<-NA
farmingpractices$Crop.3.Upland  <- ifelse (farmingpractices$Crop.3.Upland>=1,"Yes","No")

farmingpractices$Crop.3.Wetland[farmingpractices$Crop.3.Wetland  ==999]<-NA
farmingpractices$Crop.3.Wetland<- ifelse (farmingpractices$Crop.3.Wetland  >=1,"Yes","No")

farmingpractices$Crop.4.Upland  [farmingpractices$Crop.4.Upland    ==999]<-NA
farmingpractices$Crop.4.Upland  <- ifelse (farmingpractices$Crop.4.Upland    >=1,"Yes","No")

farmingpractices$Crop.4.Wetland[farmingpractices$Crop.4.Wetland ==999]<-NA
farmingpractices$Crop.4.Wetland<- ifelse (farmingpractices$Crop.4.Wetland>=1,"Yes","No")
farmingpractices<-subset(farmingpractices,select=c(Crop.1.Upland,Crop.1.Wetland,Crop.2.Upland,Crop.2.Wetland,Crop.3.Upland,Crop.3.Wetland,Crop.4.Upland,Crop.4.Wetland  ))
#data<-cbind(data,dataa)
names(data)
##===========soil fertility
soilfertility<-subset(datamalawi,select=c(Indicator:Fertilizer.Trend.Reason4))
names(soilfertility)
#soil fertility indicators
soilfertility$Indicator<-factor(soilfertility$Indicator,levels=c(1:8),labels=c("Crop Yield","Crop Growth","Presence of striga","Presence  of weeds","Color of soil","Physical texture","Color of maize leaves","Others"))
soilfertility$Indicator2<-factor(soilfertility$Indicator2,levels=c(1:8),labels=c("Crop Yield","Crop Growth","Presence of striga","Presence  of weeds","Color of soil","Physical texture","Color of maize leaves","Others"))
soilfertility$Indicator3[soilfertility$Indicator3==999]<-NA
soilfertility$Indicator3<-factor(soilfertility$Indicator3,levels=c(1:8),labels=c("Crop Yield","Crop Growth","Presence of striga","Presence  of weeds","Color of soil","Physical texture","Color of maize leaves","Others"))
#soil fertility status
soilfertility$Status<-factor(soilfertility$Status,levels=c(1:3),labels=c("Poor","Average","Good"))
#soil type
soilfertility$Soil.Type<-factor(soilfertility$Soil.Type,levels=c(1:4),labels=c("Sandy soil(Mchenga)","Red soil(Katondo)","Dark clayey soil(Makande)","Others")) 


#responsible for  the soil fertility trend
soilfertility$Trend.Reason<-factor(soilfertility$Trend.Reason,levels=c(1:12),labels=c("Lack or cost of fertilizer","Illness","Too much use of fertilizer","Bad agricultural practices","Erosion","Good use of fertilizer","Continuous cultivation","Use of beneficial plants","Use of compost/dung","Soil and water conservation practices","Improved fallows","Others"))
soilfertility$Trend.Reason2<-factor(soilfertility$Trend.Reason2,levels=c(1:12),labels=c("Lack or cost of fertilizer","Illness","Too much use of fertilizer","Bad agricultural practices","Erosion","Good use of fertilizer","Continuous cultivation","Use of beneficial plants","Use of compost/dung","Soil and water conservation practices","Improved fallows","Others"))
soilfertility$Trend.Reason3[soilfertility$Trend.Reason3 ==999]<-NA
soilfertility$Trend.Reason3 <-factor(soilfertility$Trend.Reason3,levels=c(1:12),labels=c("Lack or cost of fertilizer","Illness","Too much use of fertilizer","Bad agricultural practices","Erosion","Good use of fertilizer","Continuous cultivation","Use of beneficial plants","Use of compost/dung","Soil and water conservation practices","Improved fallows","Others"))

##trend of the soil in the past five years
soilfertility$Trend<-factor(soilfertility$Trend,levels=c(1:3),labels=c("Getting better","Same/no change","Getting worse"))
tab1(soilfertility$Trend)
#What is responsible for the trend in the soil fertility
soilfertility$Trend.Reason<-factor(soilfertility$Trend.Reason,levels=c(1:12),labels=c("Lack or cost of fertilizer","Illness","Too much use of fertilizer","Bad agricultural practices","Erosion","Good ude if fertilizer","Continuous culitvation","Use of beneficial plants","Use of compost/dung","Soil and water conservation practices","Improved fallows","Others"))

##what is the  trend of fertilizer application/use in the last five years
soilfertility$Fertilzer.Trend<-factor(soilfertility$Fertilzer.Trend,levels=c(1:3),labels=c("Decreasing","Same","Increasing"))
tab1(soilfertility$Fertilzer.Trend)

#what is responsible for the fertilizer trend use 
soilfertility$Fertilizer.Trend.Reason<-factor(soilfertility$Fertilizer.Trend.Reason,levels=c(1:13),labels=c("Needs less labour","Invigorates crops","Has immediate action","For bummper harvest","It spoils the soil","very expensive","Easility leached by rain","Waste if rainfall poor","No fertlizer coupoun","prices are lower","received subsidy","yearly subsidy","other"))  
soilfertility$Fertilizer.Trend.Reason2<-factor(soilfertility$Fertilizer.Trend.Reason2  ,levels=c(1:13),labels=c("Needs less labour","Invigorates crops","Has immediate action","For bummper harvest","It spoils the soil","very expensive","Easility leached by rain","Waste if rainfall poor","No fertlizer coupoun","prices are lower","received subsidy","yearly subsidy","other"))  
soilfertility$Fertilizer.Trend.Reason3<-factor(soilfertility$Fertilizer.Trend.Reason3    ,levels=c(1:13),labels=c("Needs less labour","Invigorates crops","Has immediate action","For bummper harvest","It spoils the soil","very expensive","Easility leached by rain","Waste if rainfall poor","No fertlizer coupoun","prices are lower","received subsidy","yearly subsidy","other"))  
soilfertility$Fertilizer.Trend.Reason4<-factor(soilfertility$Fertilizer.Trend.Reason4,levels=c(1:13),labels=c("Needs less labour","Invigorates crops","Has immediate action","For bummper harvest","It spoils the soil","very expensive","Easility leached by rain","Waste if rainfall poor","No fertlizer coupoun","prices are lower","received subsidy","yearly subsidy","other"))  




dataa<-subset(soilfertility,select=c(Status, Soil.Type,Trend.Reason,Trend.Reason2,Trend.Reason3, Trend,Indicator,	Indicator2,	Indicator3,Fertilzer.Trend,Fertilizer.Trend.Reason,Fertilizer.Trend.Reason2,	Fertilizer.Trend.Reason3,	Fertilizer.Trend.Reason4))
#soilfertility<-subset(soilfertility,select=c(soil_Fertilitystatus,Soil.Type,responsible_for_soil_fertility_status1,SoilFert_why2,SoilFert_why3,soil_fertilitytrendinthepast10years,SoilStatus_indicator1,SoilStatus_indicator2,SoilStatus_indicator3,FertilizerTrend,Fertilizertrend_why1,Fertilizertrend_why2,Fertilizertrend_why3,Fertilizertrend_why4))
rename.var(Status,soil_Fertilitystatus,dataa)
rename.var(Trend.Reason,responsible_for_soil_fertility_status1,dataa)
rename.var(Trend.Reason2,responsible_for_soil_fertility_status2,dataa)
rename.var(Trend.Reason3,responsible_for_soil_fertility_status3,dataa)
rename.var(Trend,soil_fertilitytrendinthepast10years,dataa)
rename.var(Indicator,indicatorsforassessingstatusofsoil1,dataa)
rename.var(Indicator2,indicatorsforassessingstatusofsoil2,dataa)
rename.var(Indicator3,indicatorsforassessingstatusofsoil3,dataa)

data<-cbind(data, dataa)
names(data)

####=====KNOWLEDGE AND USE OF AGROFORESTRY TECHNOLOGIES
knowledgeee<-subset(datamalawi,select=c(Chemical.Fertilizer:Reason.for.not11))
knowledgeee<-knowledgeee[,-c(6,12,18,24,30,36,42,54,60,66)]
names(knowledgeee)
#chemical_fertilizer
#chemical_fertilizer_know  chemical_Use	chemical_Used.before	chemical_Never.used	chemical_Constraints
#chemical.Fertilizer  Use	Stopped	Reason	Never.Use	Reason.for.n
rename.var(Chemical.Fertilizer,chemical_fertilizer_know,knowledgeee)
rename.var(Use,chemical_Use,knowledgeee)
rename.var(Stopped,chemical_Used.before,knowledgeee)
rename.var(Never.Use,chemical_Never.used,knowledgeee)
rename.var(Reason,chemical_Constraints,knowledgeee)

knowledgeee$chemical_fertilizer_know<-factor(knowledgeee$chemical_fertilizer_know,levels=c(1:2),labels=c("Yes","No"))
#chemical fertilizer use
knowledgeee$chemical_Use<-factor(knowledgeee$chemical_Use,levels=c(1:2),labels=c("Yes","No"))
#chemical sued before
knowledgeee$chemical_Used.before<-factor(knowledgeee$chemical_Used.before  ,levels=c(1:2),labels=c("Yes","No"))
#never used chemical
knowledgeee$chemical_Never.used<-factor(knowledgeee$chemical_Never.used,levels=c(1:2),labels=c("Yes","No"))
#chemical constraints
knowledgeee$chemical_Constraints<-factor(knowledgeee$chemical_Constraints,labels=c("I dont know about it","No visit from development agencies","Lack of training","Land area too small","Insecure land tenure","Too much labor","Fire and browsing","Long maturity period","Lack of seeds","Pests and diseases","Trees die off","Lack of rain","No water supply","Lack of money","Not practising irrigation farming/no wetland ","Have no livestock " ,"Other constraints"),levels=c(1:17))

##fertilizer tree
#Fert.trees_know  Fert.trees_use	fertilizer_Used.before.1	fertilizer_Never.used.1	fertilizer_Constraints.
#Fertilizer.Tree  Use2	Stopped2	Reason2	Never.Use2	Reason.for.not2
rename.var(Fertilizer.Tree,Fert.trees_know,knowledgeee)
rename.var(Use2,Fert.trees_use,knowledgeee)
rename.var(Stopped2,fertilizer_Used.before.1,knowledgeee)
rename.var(Never.Use2,fertilizer_Never.used.1 ,knowledgeee)
rename.var(Reason2,fertilizer_Constraints.1,knowledgeee)
#Fert.trees_know   know
knowledgeee$Fert.trees_know  <-factor(knowledgeee$Fert.trees_know,levels=c(1:2),labels=c("Yes","No"))
# fertilizer_Used.before.1use
knowledgeee$fertilizer_Used.before.1    <-factor(knowledgeee$fertilizer_Used.before.1    ,levels=c(1:2),labels=c("Yes","No"))
#never used fertilizer_Never.used.1  
knowledgeee$fertilizer_Never.used.1  <-factor(knowledgeee$fertilizer_Never.used.1  ,levels=c(1:2),labels=c("Yes","No"))
#fertilizer_Constraints.1  constraints
knowledgeee$fertilizer_Constraints.1  <-factor(knowledgeee$fertilizer_Constraints.1  ,labels=c("I dont know about it","No visit from development agencies","Lack of training","Land area too small","Insecure land tenure","Too much labor","Fire and browsing","Long maturity period","Lack of seeds","Pests and diseases","Trees die off","Lack of rain","No water supply","Lack of money","Not practising irrigation farming/no wetland ","Have no livestock " ,"Other constraints"),levels=c(1:17))

##fodder_trees
#Fodder_know  Fodder_use	fodder_trees_Used.before.2	fodder_trees_Constraints.2
#Fodder.Tree  Use6	Stopped6	Reason6	Never.Use6	Reason.for.not6	
rename.var(Fodder.Tree,Fodder_know,knowledgeee)
rename.var(Use6,Fodder_use,knowledgeee)
rename.var(Stopped6,fodder_trees_Used.before.2,knowledgeee)
rename.var(Never.Use6,fodder_trees_Never.used.2,knowledgeee)
rename.var(Reason6,fodder_trees_Constraints.2 ,knowledgeee)
#Fodder l know
knowledgeee$Fodder_know<-factor(knowledgeee$Fodder_know,levels=c(1:2),labels=c("Yes","No"))
#Fodder_use  use
knowledgeee$Fodder_use  <-factor(knowledgeee$Fodder_use  ,levels=c(1:2),labels=c("Yes","No"))
#fodder_trees_Used.before.2  
knowledgeee$fodder_trees_Used.before.2<-factor(knowledgeee$fodder_trees_Used.before.2,levels=c(1:2),labels=c("Yes","No"))
#nevefodder_trees_Never.used.2  
knowledgeee$fodder_trees_Never.used.2  <-factor(knowledgeee$fodder_trees_Never.used.2  ,levels=c(1:2),labels=c("Yes","No"))
#fodder_trees_Constraints.2   constraints
knowledgeee$fodder_trees_Constraints.2  <-factor(knowledgeee$fodder_trees_Constraints.2  ,labels=c("I dont know about it","No visit from development agencies","Lack of training","Land area too small","Insecure land tenure","Too much labor","Fire and browsing","Long maturity period","Lack of seeds","Pests and diseases","Trees die off","Lack of rain","No water supply","Lack of money","Not practising irrigation farming/no wetland ","Have no livestock " ,"Other constraints"),levels=c(1:17))


#fruit_trees
#Fruit_know  Fruit_use	fruit_trees_Used.before.3	fruit_trees_Never.used.3	fruit_trees_Constraints.3	
#Fruit.Tree  Use8	Stopped8	Reason8	Never.Use8	Reason.for.not8	
rename.var(Fruit.Tree ,Fruit_know,knowledgeee)
rename.var(Use8,Fruit_use,knowledgeee)
rename.var(Stopped8,fruit_trees_Used.before.3,knowledgeee)
rename.var(Never.Use8,fruit_trees_Never.used.3,knowledgeee)
rename.var(Reason8,fruit_trees_Constraints.3,knowledgeee)

#Fruit_know  
knowledgeee$Fruit_know <-factor(knowledgeee$Fruit_know  ,levels=c(1:2),labels=c("Yes","No"))
#Fruit_use
knowledgeee$Fruit_use<-factor(knowledgeee$Fruit_use  ,levels=c(1:2),labels=c("Yes","No"))
#fruit_trees_Used.before.3  
knowledgeee$fruit_trees_Used.before.3<-factor(knowledgeee$fruit_trees_Used.before.3  ,levels=c(1:2),labels=c("Yes","No"))
#fruit_trees_Never.used.3  
knowledgeee$fruit_trees_Never.used.3<-factor(knowledgeee$fruit_trees_Never.used.3  ,levels=c(1:2),labels=c("Yes","No"))
#ffruit_trees_Constraints.3  
knowledgeee$fruit_trees_Constraints.3<-factor(knowledgeee$fruit_trees_Constraints.3    ,labels=c("I dont know about it","No visit from development agencies","Lack of training","Land area too small","Insecure land tenure","Too much labor","Fire and browsing","Long maturity period","Lack of seeds","Pests and diseases","Trees die off","Lack of rain","No water supply","Lack of money","Not practising irrigation farming/no wetland ","Have no livestock " ,"Other constraints"),levels=c(1:17))


#communial_woodlot
#Communal_know  Communal_use	communial_woodlot_Used.before.4	communial_woodlot_Never.used.4	communial_woodlot_Constraints.4
#Woodlot.communal  Use9	Stopped9	Reason9	Never.Use9	Reason.for.not9

rename.var(Woodlot.communal,Communal_know,knowledgeee)
rename.var(Use9,Communal_use,knowledgeee)
rename.var(Stopped9,communial_woodlot_Used.before.4,knowledgeee)
rename.var(Never.Use9,communial_woodlot_Never.used.4,knowledgeee)
rename.var(Reason9,communial_woodlot_Constraints.4,knowledgeee)
#Communal_know    
knowledgeee$Communal_know<-factor(knowledgeee$Communal_know,levels=c(1:2),labels=c("Yes","No"))
#Communal_use
knowledgeee$Communal_use<-factor(knowledgeee$Communal_use ,levels=c(1:2),labels=c("Yes","No"))
#communial_woodlot_Used.before.4   
knowledgeee$communial_woodlot_Used.before.4  <-factor(knowledgeee$communial_woodlot_Used.before.4    ,levels=c(1:2),labels=c("Yes","No"))
#communial_woodlot_Never.used.4   
knowledgeee$communial_woodlot_Never.used.4  <-factor(knowledgeee$communial_woodlot_Never.used.4    ,levels=c(1:2),labels=c("Yes","No"))
#communial_woodlot_Constraints.4  
knowledgeee$communial_woodlot_Constraints.4  <-factor(knowledgeee$communial_woodlot_Constraints.4   ,labels=c("I dont know about it","No visit from development agencies","Lack of training","Land area too small","Insecure land tenure","Too much labor","Fire and browsing","Long maturity period","Lack of seeds","Pests and diseases","Trees die off","Lack of rain","No water supply","Lack of money","Not practising irrigation farming/no wetland ","Have no livestock " ,"Other constraints"),levels=c(1:17))

##individual woodlot
#Individual_know  Individual_use	individual_woodlot_Used.before.5	individual_woodlot_Never.used.5	individual_woodlot_Constraints.5	soil_water_conservation_know
#Woodlot.individual  Use10	Stopped10	Reason10	Never.Use10	Reason.for.not10
rename.var(Woodlot.individual,Individual_know,knowledgeee)
rename.var(Use10,Individual_use,knowledgeee)
rename.var(Stopped10,individual_woodlot_Used.before.5,knowledgeee)
rename.var(Never.Use10,individual_woodlot_Never.used.5,knowledgeee)
rename.var(Reason10,individual_woodlot_Constraints.5,knowledgeee)
#Individual_know  
knowledgeee$Individual_know  <-factor(knowledgeee$Individual_know,levels=c(1:2),labels=c("Yes","No"))
#Individual_use  
knowledgeee$Individual_use  <-factor(knowledgeee$Individual_use,levels=c(1:2),labels=c("Yes","No"))
#individual_woodlot_Used.before.5    
knowledgeee$individual_woodlot_Used.before.5<-factor(knowledgeee$individual_woodlot_Used.before.5,levels=c(1:2),labels=c("Yes","No"))
#individual_woodlot_Never.used.5     
knowledgeee$individual_woodlot_Never.used.5<-factor(knowledgeee$individual_woodlot_Never.used.5,levels=c(1:2),labels=c("Yes","No"))
#individual_woodlot_Constraints.5  
knowledgeee$individual_woodlot_Constraints.5<-factor(knowledgeee$individual_woodlot_Constraints.5,labels=c("I dont know about it","No visit from development agencies","Lack of training","Land area too small","Insecure land tenure","Too much labor","Fire and browsing","Long maturity period","Lack of seeds","Pests and diseases","Trees die off","Lack of rain","No water supply","Lack of money","Not practising irrigation farming/no wetland ","Have no livestock " ,"Other constraints"),levels=c(1:17))


#soil_water_conservation
#soil_water_conservation_know  soil_water_conservation_use	soil_water_conservation_Used.before.6	soil_water_conservation_Never.used.6	soil_water_conservation_Constraints.6
#Soil.water.Conservation  Use5	Stopped5	Reason5	Never.Use5	Reason.for.not5
rename.var(Soil.water.Conservation,soil_water_conservation_know ,knowledgeee)
rename.var(Use5,soil_water_conservation_use,knowledgeee)
rename.var(Stopped5,soil_water_conservation_Used.before.6,knowledgeee)
rename.var(Never.Use5,soil_water_conservation_Never.used.6,knowledgeee)
rename.var(Reason5,soil_water_conservation_Constraints.6,knowledgeee)
#soil_water_conservation_know   
knowledgeee$soil_water_conservation_know <-factor(knowledgeee$soil_water_conservation_know,levels=c(1:2),labels=c("Yes","No"))
#soil_water_conservation_use   
knowledgeee$soil_water_conservation_use<-factor(knowledgeee$soil_water_conservation_use,levels=c(1:2),labels=c("Yes","No"))
#soil_water_conservation_Used.before.6      
knowledgeee$soil_water_conservation_Used.before.6  <-factor(knowledgeee$soil_water_conservation_Used.before.6  ,levels=c(1:2),labels=c("Yes","No"))
#soil_water_conservation_Never.used.6      
knowledgeee$soil_water_conservation_Never.used.6  <-factor(knowledgeee$soil_water_conservation_Never.used.6  ,levels=c(1:2),labels=c("Yes","No"))
#soil_water_conservation_Constraints.6    
knowledgeee$soil_water_conservation_Constraints.6  <-factor(knowledgeee$soil_water_conservation_Constraints.6  ,labels=c("I dont know about it","No visit from development agencies","Lack of training","Land area too small","Insecure land tenure","Too much labor","Fire and browsing","Long maturity period","Lack of seeds","Pests and diseases","Trees die off","Lack of rain","No water supply","Lack of money","Not practising irrigation farming/no wetland ","Have no livestock " ,"Other constraints"),levels=c(1:17))

##farm_yard_manure
#Manure_know  Manure_use	farm_yard_manure_Used.before.7	farm_yard_manure_Never.used.7	farm_yard_manure_Constraints.7	
#Manure  Use3	Stopped3	Reason3	Never.Use3	Reason.for.not3
rename.var(Manure,Manure_know,knowledgeee)
rename.var(Use3,Manure_use,knowledgeee)
rename.var(Stopped3,farm_yard_manure_Used.before.7,knowledgeee)
rename.var(Never.Use3,farm_yard_manure_Never.used.7,knowledgeee)
rename.var(Reason3,farm_yard_manure_Constraints.7,knowledgeee)
#Manure_know    
knowledgeee$Manure_know<-factor(knowledgeee$Manure_know,levels=c(1:2),labels=c("Yes","No"))
#Manure_use 
knowledgeee$Manure_use<-factor(knowledgeee$Manure_use,levels=c(1:2),labels=c("Yes","No"))
#farm_yard_manure_Used.before.7       
knowledgeee$farm_yard_manure_Used.before.7<-factor(knowledgeee$farm_yard_manure_Used.before.7,levels=c(1:2),labels=c("Yes","No"))
#farm_yard_manure_Never.used.7      
knowledgeee$farm_yard_manure_Never.used.7<-factor(knowledgeee$farm_yard_manure_Never.used.7   ,levels=c(1:2),labels=c("Yes","No"))
#farm_yard_manure_Constraints.7     
knowledgeee$farm_yard_manure_Constraints.7<-factor(knowledgeee$farm_yard_manure_Constraints.7,labels=c("I dont know about it","No visit from development agencies","Lack of training","Land area too small","Insecure land tenure","Too much labor","Fire and browsing","Long maturity period","Lack of seeds","Pests and diseases","Trees die off","Lack of rain","No water supply","Lack of money","Not practising irrigation farming/no wetland ","Have no livestock " ,"Other constraints"),levels=c(1:17))

##organic_compost
#Compost_know  Compost_use	organic_compost_Used.before.8	organic_compost_Never.used.8	organic_compost_Constraints.8
#Organic.Manure  Use4	Stopped4	Reason4	Never.Use4	Reason.for.not4
rename.var(Organic.Manure,Compost_know,knowledgeee)
rename.var(Use4,Compost_use,knowledgeee)
rename.var(Stopped4,organic_compost_Used.before.8,knowledgeee)
rename.var(Never.Use4,organic_compost_Never.used.8,knowledgeee)
rename.var(Reason4,organic_compost_Constraints.8,knowledgeee)
#Compost_know   
knowledgeee$Compost_know<-factor(knowledgeee$Compost_know,levels=c(1:2),labels=c("Yes","No"))
#Compost_use  
knowledgeee$Compost_use<-factor(knowledgeee$Compost_use,levels=c(1:2),labels=c("Yes","No"))
#organic_compost_Used.before.8         
knowledgeee$organic_compost_Used.before.8<-factor(knowledgeee$organic_compost_Used.before.8,levels=c(1:2),labels=c("Yes","No"))
#organic_compost_Never.used.8        
knowledgeee$organic_compost_Never.used.8<-factor(knowledgeee$organic_compost_Never.used.8,levels=c(1:2),labels=c("Yes","No"))
#organic_compost_Constraints.8      
knowledgeee$organic_compost_Constraints.8  <-factor(knowledgeee$organic_compost_Constraints.8  ,labels=c("I dont know about it","No visit from development agencies","Lack of training","Land area too small","Insecure land tenure","Too much labor","Fire and browsing","Long maturity period","Lack of seeds","Pests and diseases","Trees die off","Lack of rain","No water supply","Lack of money","Not practising irrigation farming/no wetland ","Have no livestock " ,"Other constraints"),levels=c(1:17))

#irrigation_tread_pump
#Treadlepump_know  Treadlepumple_use	irrigation_tread_pump_Used.before.9	irrigation_tread_pump_Never.used.9	irrigation_tread_pump_Constraints.9
#Irrigation..treadle.pump.  Use11	Stopped11	Reason11	Never.Use11	Reason.for.not11
rename.var(Irrigation..treadle.pump.,Treadlepump_know,knowledgeee)
rename.var(Use11,Treadlepumple_use,knowledgeee)
rename.var(Stopped11,irrigation_tread_pump_Used.before.9,knowledgeee)
rename.var(Never.Use11,irrigation_tread_pump_Never.used.9,knowledgeee)
rename.var(Reason11,irrigation_tread_pump_Constraints.9,knowledgeee)
#Treadlepump_know  
knowledgeee$Treadlepump_know<-factor(knowledgeee$Treadlepump_know,levels=c(1:2),labels=c("Yes","No"))
#Treadlepumple_use   
knowledgeee$Treadlepumple_use<-factor(knowledgeee$Treadlepumple_use  ,levels=c(1:2),labels=c("Yes","No"))
#irrigation_tread_pump_Used.before.9         
knowledgeee$irrigation_tread_pump_Used.before.9<-factor(knowledgeee$irrigation_tread_pump_Used.before.9  ,levels=c(1:2),labels=c("Yes","No"))
#irrigation_tread_pump_Never.used.9          
knowledgeee$irrigation_tread_pump_Never.used.9<-factor(knowledgeee$irrigation_tread_pump_Never.used.9,levels=c(1:2),labels=c("Yes","No"))
#irrigation_tread_pump_Constraints      
knowledgeee$irrigation_tread_pump_Constraints.9<-factor(knowledgeee$irrigation_tread_pump_Constraints.9    ,labels=c("I dont know about it","No visit from development agencies","Lack of training","Land area too small","Insecure land tenure","Too much labor","Fire and browsing","Long maturity period","Lack of seeds","Pests and diseases","Trees die off","Lack of rain","No water supply","Lack of money","Not practising irrigation farming/no wetland ","Have no livestock " ,"Other constraints"),levels=c(1:17))
names(knowledgeee)
#merge the knowledge dataframe to data and arranged well
knowledgeee<-subset(knowledgeee,select=c(chemical_fertilizer_know,chemical_Use,chemical_Used.before,chemical_Never.used,chemical_Constraints,Fert.trees_know,Fert.trees_use,fertilizer_Used.before.1,fertilizer_Never.used.1,fertilizer_Constraints.1,
                                         Fodder_know,Fodder_use,fodder_trees_Used.before.2,fodder_trees_Constraints.2,Fruit_know, Fruit_use,fruit_trees_Used.before.3,fruit_trees_Never.used.3,fruit_trees_Constraints.3,Communal_know,Communal_use,communial_woodlot_Used.before.4,communial_woodlot_Never.used.4,communial_woodlot_Constraints.4,Individual_know,Individual_use,individual_woodlot_Used.before.5,individual_woodlot_Never.used.5,individual_woodlot_Constraints.5,
                                         soil_water_conservation_know,soil_water_conservation_use,soil_water_conservation_Used.before.6,soil_water_conservation_Never.used.6,soil_water_conservation_Constraints.6,Manure_know,Manure_use,farm_yard_manure_Used.before.7,farm_yard_manure_Never.used.7,farm_yard_manure_Constraints.7,Compost_know,Compost_use,organic_compost_Used.before.8,organic_compost_Never.used.8,organic_compost_Constraints.8,Treadlepump_know,Treadlepumple_use,irrigation_tread_pump_Used.before.9,irrigation_tread_pump_Never.used.9,irrigation_tread_pump_Constraints.9))
data<-cbind(data,knowledgeee)
names(knowledgeee)



# agroforestry knowledge (a) reading in the data into r 
#from a comma delimited format
data1<-subset(datamalawi,select=c(Husband.Knowledge.before:Wife.Knowledge.now))
names(data1)
data1$Husband.Knowledge.before[data1$Husband.Knowledge.before==999]<-NA
data1$Wife.Knowledge.before [data1$Wife.Knowledge.before  ==999]<-NA
data1$Husband.Knowledge.now   [data1$Husband.Knowledge.now    ==999]<-NA
data1$Wife.Knowledge.now[data1$Wife.Knowledge.now ==999]<-NA
data1<-subset(data1,select=c(Husband.Knowledge.before:Wife.Knowledge.now))
data1$Husband.Knowledge.before<-factor(data1$Husband.Knowledge.before,labels=c("None","Low","Intermediate","High"),levels=(1:4))
data1$Wife.Knowledge.before<-factor(data1$Wife.Knowledge.before ,labels=c("None","Low","Intermediate","High"),levels=(1:4))
data1$Husband.Knowledge.now <-factor(data1$Husband.Knowledge.now,labels=c("None","Low","Intermediate","High"),levels=(1:4))
data1$Wife.Knowledge.now<-factor(data1$Wife.Knowledge.now,labels=c("None","Low","Intermediate","High"),levels=(1:4))


#data<-cbind(data,data1)
names(data)
##defining an adopter
data1<-subset(datamalawi,select=c(Fertilizer.Tree,Use2))



#data<-cbind(data,data1)


#====================================================================
## if currently using the technologies list the  two most 
#numerous speciesplantedone
numerousspecies<-subset(datamalawi,select=c(Practiced:Surviving6))
names(numerousspecies)
#practised agroforestry b2wn 2008-2013 yes or no
numerousspecies$Practiced<-factor(numerousspecies$Practiced,levels=c(1:2),labels=c("Yes","No"))

#names of the first tree species

numerousspecies$X1st.Species<-factor(numerousspecies$X1st.Species,levels=c(1:19),
                                     labels=c("Eucalyptus","Lucaenea spp","Faiderbia Albida" ,"Senna app",
                                              "Gliricidia", 
                                              "Sesbania", 
                                              "Cajanus cajan",
                                              "Tephrosia spp", 
                                              "Khayn spp ",
                                              "Acacia ssp", 
                                              "  Albiza Lebbeck ",
                                              "Mango", 
                                              "Papaya", 
                                              "Caliander", 
                                              "Avacado pear ",
                                              "Citrus",
                                              "Guava",  
                                              "Other", 
                                              "not entred"
                                     ))
numerousspecies$X2nd.Species  <-factor(numerousspecies$X2nd.Species  ,levels=c(1:19),
                                       labels=c("Eucalyptus","Lucaenea spp","Faiderbia Albida" ,"Senna app",
                                                "Gliricidia", 
                                                "Sesbania", 
                                                "Cajanus cajan",
                                                "Tephrosia spp", 
                                                "Khayn spp ",
                                                "Acacia ssp", 
                                                "  Albiza Lebbeck ",
                                                "Mango", 
                                                "Papaya", 
                                                "Caliander", 
                                                "Avacado pear ",
                                                "Citrus",
                                                "Guava",  
                                                "Other", 
                                                "not entred"
                                       ))
#number of trees in 2008
numerousspecies$Number[numerousspecies$Number==999]<-NA
numerousspecies$Started.growing[numerousspecies$Started.growing==999]<-NA
numerousspecies$Planted[numerousspecies$Planted==999]<-NA
numerousspecies$Planted[numerousspecies$Planted==""]<-NA
numerousspecies$Surviving[numerousspecies$Surviving==999]<-NA
numerousspecies$Practiced2<-factor(numerousspecies$Practiced2,levels=c(1:2),labels=c("Yes","No"))
numerousspecies$X1st.Species3  <-factor(numerousspecies$X1st.Species3  ,levels=c(1:19),
                                        labels=c("Eucalyptus","Lucaenea spp","Faiderbia Albida" ,"Senna app",
                                                 "Gliricidia", 
                                                 "Sesbania", 
                                                 "Cajanus cajan",
                                                 "Tephrosia spp", 
                                                 "Khayn spp ",
                                                 "Acacia ssp", 
                                                 "  Albiza Lebbeck ",
                                                 "Mango", 
                                                 "Papaya", 
                                                 "Caliander", 
                                                 "Avacado pear ",
                                                 "Citrus",
                                                 "Guava",  
                                                 "Other", 
                                                 "not entred"
                                        ))
numerousspecies$X2nd.Species4    <-factor(numerousspecies$X2nd.Species4    ,levels=c(1:19),
                                          labels=c("Eucalyptus","Lucaenea spp","Faiderbia Albida" ,"Senna app",
                                                   "Gliricidia", 
                                                   "Sesbania", 
                                                   "Cajanus cajan",
                                                   "Tephrosia spp", 
                                                   "Khayn spp ",
                                                   "Acacia ssp", 
                                                   "  Albiza Lebbeck ",
                                                   "Mango", 
                                                   "Papaya", 
                                                   "Caliander", 
                                                   "Avacado pear ",
                                                   "Citrus",
                                                   "Guava",  
                                                   "Other", 
                                                   "not entred"
                                          ))

numerousspecies$Practiced2<-factor(numerousspecies$Practiced2,levels=c(1:2),labels=c("Yes","No"))
numerousspecies$X1st.Species3  <-factor(numerousspecies$X1st.Species3  ,levels=c(1:19),
                                        labels=c("Eucalyptus","Lucaenea spp","Faiderbia Albida" ,"Senna app",
                                                 "Gliricidia", 
                                                 "Sesbania", 
                                                 "Cajanus cajan",
                                                 "Tephrosia spp", 
                                                 "Khayn spp ",
                                                 "Acacia ssp", 
                                                 "  Albiza Lebbeck ",
                                                 "Mango", 
                                                 "Papaya", 
                                                 "Caliander", 
                                                 "Avacado pear ",
                                                 "Citrus",
                                                 "Guava",  
                                                 "Other", 
                                                 "not entred"
                                        ))
numerousspecies$X2nd.Species4    <-factor(numerousspecies$X2nd.Species4    ,levels=c(1:19),
                                          labels=c("Eucalyptus","Lucaenea spp","Faiderbia Albida" ,"Senna app",
                                                   "Gliricidia", 
                                                   "Sesbania", 
                                                   "Cajanus cajan",
                                                   "Tephrosia spp", 
                                                   "Khayn spp ",
                                                   "Acacia ssp", 
                                                   "  Albiza Lebbeck ",
                                                   "Mango", 
                                                   "Papaya", 
                                                   "Caliander", 
                                                   "Avacado pear ",
                                                   "Citrus",
                                                   "Guava",  
                                                   "Other", 
                                                   "not entred"
                                          ))

numerousspecies$Practiced3<-factor(numerousspecies$Practiced3,levels=c(1:2),labels=c("Yes","No"))
numerousspecies$X1st.Species4    <-factor(numerousspecies$X1st.Species4    ,levels=c(1:19),
                                          labels=c("Eucalyptus","Lucaenea spp","Faiderbia Albida" ,"Senna app",
                                                   "Gliricidia", 
                                                   "Sesbania", 
                                                   "Cajanus cajan",
                                                   "Tephrosia spp", 
                                                   "Khayn spp ",
                                                   "Acacia ssp", 
                                                   "  Albiza Lebbeck ",
                                                   "Mango", 
                                                   "Papaya", 
                                                   "Caliander", 
                                                   "Avacado pear ",
                                                   "Citrus",
                                                   "Guava",  
                                                   "Other", 
                                                   "not entred"
                                          ))
numerousspecies$X2nd.Species5      <-factor(numerousspecies$X2nd.Species5      ,levels=c(1:19),
                                            labels=c("Eucalyptus","Lucaenea spp","Faiderbia Albida" ,"Senna app",
                                                     "Gliricidia", 
                                                     "Sesbania", 
                                                     "Cajanus cajan",
                                                     "Tephrosia spp", 
                                                     "Khayn spp ",
                                                     "Acacia ssp", 
                                                     "  Albiza Lebbeck ",
                                                     "Mango", 
                                                     "Papaya", 
                                                     "Caliander", 
                                                     "Avacado pear ",
                                                     "Citrus",
                                                     "Guava",  
                                                     "Other", 
                                                     "not entred"
                                            ))
numerousspecies$Practiced4  <-factor(numerousspecies$Practiced4  ,levels=c(1:2),labels=c("Yes","No"))
numerousspecies$X1st.Species5      <-factor(numerousspecies$X1st.Species5      ,levels=c(1:19),
                                            labels=c("Eucalyptus","Lucaenea spp","Faiderbia Albida" ,"Senna app",
                                                     "Gliricidia", 
                                                     "Sesbania", 
                                                     "Cajanus cajan",
                                                     "Tephrosia spp", 
                                                     "Khayn spp ",
                                                     "Acacia ssp", 
                                                     "  Albiza Lebbeck ",
                                                     "Mango", 
                                                     "Papaya", 
                                                     "Caliander", 
                                                     "Avacado pear ",
                                                     "Citrus",
                                                     "Guava",  
                                                     "Other", 
                                                     "not entred"
                                            ))
numerousspecies$X2nd.Species6        <-factor(numerousspecies$X2nd.Species6        ,levels=c(1:19),
                                              labels=c("Eucalyptus","Lucaenea spp","Faiderbia Albida" ,"Senna app",
                                                       "Gliricidia", 
                                                       "Sesbania", 
                                                       "Cajanus cajan",
                                                       "Tephrosia spp", 
                                                       "Khayn spp ",
                                                       "Acacia ssp", 
                                                       "  Albiza Lebbeck ",
                                                       "Mango", 
                                                       "Papaya", 
                                                       "Caliander", 
                                                       "Avacado pear ",
                                                       "Citrus",
                                                       "Guava",  
                                                       "Other", 
                                                       "not entred"
                                              ))
numerousspecies$Practiced5  <-factor(numerousspecies$Practiced5  ,levels=c(1:2),labels=c("Yes","No"))
numerousspecies$X1st.Species6        <-factor(numerousspecies$X1st.Species6        ,levels=c(1:19),
                                              labels=c("Eucalyptus","Lucaenea spp","Faiderbia Albida" ,"Senna app",
                                                       "Gliricidia", 
                                                       "Sesbania", 
                                                       "Cajanus cajan",
                                                       "Tephrosia spp", 
                                                       "Khayn spp ",
                                                       "Acacia ssp", 
                                                       "  Albiza Lebbeck ",
                                                       "Mango", 
                                                       "Papaya", 
                                                       "Caliander", 
                                                       "Avacado pear ",
                                                       "Citrus",
                                                       "Guava",  
                                                       "Other", 
                                                       "not entred"
                                              ))
numerousspecies$X2nd.Species7          <-factor(numerousspecies$X2nd.Species7          ,levels=c(1:19),
                                                labels=c("Eucalyptus","Lucaenea spp","Faiderbia Albida" ,"Senna app",
                                                         "Gliricidia", 
                                                         "Sesbania", 
                                                         "Cajanus cajan",
                                                         "Tephrosia spp", 
                                                         "Khayn spp ",
                                                         "Acacia ssp", 
                                                         "  Albiza Lebbeck ",
                                                         "Mango", 
                                                         "Papaya", 
                                                         "Caliander", 
                                                         "Avacado pear ",
                                                         "Citrus",
                                                         "Guava",  
                                                         "Other", 
                                                         "not entred"
                                                ))
numerousspecies$Practiced6  <-factor(numerousspecies$Practiced6  ,levels=c(1:2),labels=c("Yes","No"))
numerousspecies$X1st.Species7          <-factor(numerousspecies$X1st.Species7          ,levels=c(1:19),
                                                labels=c("Eucalyptus","Lucaenea spp","Faiderbia Albida" ,"Senna app",
                                                         "Gliricidia", 
                                                         "Sesbania", 
                                                         "Cajanus cajan",
                                                         "Tephrosia spp", 
                                                         "Khayn spp ",
                                                         "Acacia ssp", 
                                                         "  Albiza Lebbeck ",
                                                         "Mango", 
                                                         "Papaya", 
                                                         "Caliander", 
                                                         "Avacado pear ",
                                                         "Citrus",
                                                         "Guava",  
                                                         "Other", 
                                                         "not entred"
                                                ))
numerousspecies$X2nd.Species8            <-factor(numerousspecies$X2nd.Species8            ,levels=c(1:19),
                                                  labels=c("Eucalyptus","Lucaenea spp","Faiderbia Albida" ,"Senna app",
                                                           "Gliricidia", 
                                                           "Sesbania", 
                                                           "Cajanus cajan",
                                                           "Tephrosia spp", 
                                                           "Khayn spp ",
                                                           "Acacia ssp", 
                                                           "  Albiza Lebbeck ",
                                                           "Mango", 
                                                           "Papaya", 
                                                           "Caliander", 
                                                           "Avacado pear ",
                                                           "Citrus",
                                                           "Guava",  
                                                           "Other", 
                                                           "not entred"
                                                  ))
numerousspecies$Practiced7  <-factor(numerousspecies$Practiced7  ,levels=c(1:2),labels=c("Yes","No"))
numerousspecies$X1st.Species8            <-factor(numerousspecies$X1st.Species8            ,levels=c(1:19),
                                                  labels=c("Eucalyptus","Lucaenea spp","Faiderbia Albida" ,"Senna app",
                                                           "Gliricidia", 
                                                           "Sesbania", 
                                                           "Cajanus cajan",
                                                           "Tephrosia spp", 
                                                           "Khayn spp ",
                                                           "Acacia ssp", 
                                                           "  Albiza Lebbeck ",
                                                           "Mango", 
                                                           "Papaya", 
                                                           "Caliander", 
                                                           "Avacado pear ",
                                                           "Citrus",
                                                           "Guava",  
                                                           "Other", 
                                                           "not entred"
                                                  ))
numerousspecies$X1st.Species8              <-factor(numerousspecies$X1st.Species8              ,levels=c(1:19),
                                                    labels=c("Eucalyptus","Lucaenea spp","Faiderbia Albida" ,"Senna app",
                                                             "Gliricidia", 
                                                             "Sesbania", 
                                                             "Cajanus cajan",
                                                             "Tephrosia spp", 
                                                             "Khayn spp ",
                                                             "Acacia ssp", 
                                                             "  Albiza Lebbeck ",
                                                             "Mango", 
                                                             "Papaya", 
                                                             "Caliander", 
                                                             "Avacado pear ",
                                                             "Citrus",
                                                             "Guava",  
                                                             "Other", 
                                                             "not entred"
                                                    ))
numerousspecies<-subset(numerousspecies,select=c(Practiced,X1st.Species,X2nd.Species,Practiced3,  X1st.Species4,	X2nd.Species5,Practiced4 , X1st.Species5,	X2nd.Species6))
rename.var(Practiced,Practiced_fertilizer_tree,numerousspecies)
rename.var(X1st.Species,X1st.Species_fert,numerousspecies)
rename.var(X2nd.Species,X2nd.Species_fert,numerousspecies)
rename.var(Practiced3,Practiced3_fodder,numerousspecies)
rename.var(X1st.Species4,X1st.Species4_fodder,numerousspecies)
rename.var(X2nd.Species5,X2nd.Species5_fodder,numerousspecies)
rename.var(Practiced4,Practiced4_fruit,numerousspecies)
rename.var(X1st.Species5,X1st.Species5_fruit,numerousspecies)
rename.var(X2nd.Species6,X2nd.Species6_fruit,numerousspecies)
data<-cbind(data,numerousspecies)
names(data)
#4.2.4b if you establshed an agroforestry technology since 2008 
#info on the early benefits
benefitsfromAG<-subset(datamalawi,select=c(Planted.Maize:AF.Impact5))
names(benefitsfromAG)
##have you plANTED MAIZE FOLLOWING FERTILIZER TREE ESTABLISHMENT
benefitsfromAG$Planted.Maize<-factor(benefitsfromAG$Planted.Maize,levels=c(1:2),labels=c("Yes","No")) 
tab1(benefitsfromAG$Planted.Maize,missing=T)


#maize yields
benefitsfromAG$Yields<-factor(benefitsfromAG$Yields,levels=c(1:5),labels=c("Much worse","Slightly worse","Same","Slightly better","Much better"))
tab1(benefitsfromAG$Yields)

#4.2.4c those with ttrees on the farm who is involved in tree management
#activities

managementactivities<-subset(datamalawi,select=c(Decision:Sells5))

#decision on whether and how muchto plant
rename.var(Decision,decidesfertilizerpermanentintercrop,managementactivities)
managementactivities$decidesfertilizerpermanentintercrop<-factor(managementactivities$decidesfertilizerpermanentintercrop,levels=c(1:4),labels=c("Head only","Spouse only","Head and spouse","Neither"))
#who provides labour
managementactivities$Labor<-factor(managementactivities$Labor,levels=c(1:4),labels=c("Head only","Spouse only","Head and spouse","Neither"))
#harvest
managementactivities$Harvest<-factor(managementactivities$Harvest,levels=c(1:4),labels=c("Head only","Spouse only","Head and spouse","Neither"))
#sells the product who?
managementactivities$Sells<-factor(managementactivities$Sells,levels=c(1:4),labels=c("Head only","Spouse only","Head and spouse","Neither"))
#===================NURSERIES=================================================
##if engaged in a group nursery didi you receive income from sale of the seedlings
nurseries<-subset(datamalawi,select=c(Individual:Buyer8))
names(nurseries)

##diseases and pests
Encounter.Disease.Pest:Trend.1
#=================================================================================
#==================ACCESS TO INFORMATION,FINANCIAL AND TECHNICAL ASSISTANCE
informationaccess<-subset(datamalawi,select=c(ICRAF:Listen.to.Needs6.5))
names(informationaccess)


#========================ICRAF==========================================
informationaccess$ICRAF_Assistance<-factor(informationaccess$ICRAF_AssistanceCRAF,levels=c(1:2),labels=c("Yes","No"))
informationaccess$Frequency.2[informationaccess$Frequency.2==999]<-NA
informationaccess<-subset(informationaccess,select=c(ICRAF))
data<-cbind(data,informationaccess)
names(data)


#SEEDLINGS
informationaccess$Frequency.22[informationaccess$Frequency.22==999]<-NA
informationaccess$Last.Assistance3[informationaccess$Last.Assistance3 ==999]<-NA
informationaccess$Needs.Addressed4[informationaccess$Needs.Addressed4==999]<-NA
informationaccess$Listen.to.Needs3[informationaccess$Listen.to.Needs3==999]<-NA

data1<-subset(informationaccess,select=c(ICRAF,Frequency.2,Last.Assistance2,Needs.addressed,Listen.to.Needs2,Frequency.22,Needs.Addressed4,Listen.to.Needs3))
#data<-cbind(data,data1)
names(data)
#Pesticide.Chemical  
informationaccess$Frequency.222[informationaccess$Frequency.222==999]<-NA
informationaccess$Last.Assistance4[informationaccess$Last.Assistance4==999]<-NA
informationaccess$Needs.Addressed2[informationaccess$Needs.Addressed2==999]<-NA
informationaccess$Listen.to.Needs4[informationaccess$Listen.to.Needs4==999]<-NA

#FARMEQUIPMENT
informationaccess$Frequency.2222[informationaccess$Frequency.2222==999]<-NA
informationaccess$Last.Assistance5[informationaccess$Last.Assistance5==999]<-NA
informationaccess$Needs.Addressed5[informationaccess$Needs.Addressed5==999]<-NA
informationaccess$Listen.to.Needs5[informationaccess$Listen.to.Needs5==999]<-NA
#Product.Processor 
informationaccess$Frequency32[informationaccess$Frequency32==999]<-NA
informationaccess$Last.Assistance6[informationaccess$Last.Assistance6==999]<-NA
informationaccess$Needs.Addressed52[informationaccess$Needs.Addressed52 ==999]<-NA
informationaccess$Listen.to.Needs52[informationaccess$Listen.to.Needs52==999]<-NA
#Cash.Money  
informationaccess$Frequency33[informationaccess$Frequency33==999]<-NA
informationaccess$Last.Assistance7  [informationaccess$Last.Assistance7  ==999]<-NA
informationaccess$Needs.Addressed53  [informationaccess$Needs.Addressed53   ==999]<-NA
informationaccess$Listen.to.Needs7  [informationaccess$Frequency332  ==999]<-NA
#Markets
informationaccess$Frequency332[informationaccess$Frequency332==999]<-NA
informationaccess$Last.Assistance8[informationaccess$Last.Assistance8==999]<-NA
informationaccess$Needs.Addressed532[informationaccess$Needs.Addressed532==999]<-NA
informationaccess$Listen.to.Needs6[informationaccess$Listen.to.Needs6==999]<-NA
data1<-subset(informationaccess,select=c(ICRAF,Frequency.2Last.Assistance2,Frequency33,Last.Assistance7,Frequency332,Frequency332,Last.Assistance8,Needs.Addressed532,Listen.to.Needs6))
#data<-cbind(data,data1)


#===================Forestry.Service  
informationaccess$Forestry.Service  [informationaccess$Forestry.Service  ==999]<-NA
informationaccess$Training.1[informationaccess$Training.1==999]<-NA
informationaccess$Frequency4[informationaccess$Frequency4==999]<-NA
informationaccess$Last.Assistance25[informationaccess$Last.Assistance25==999]<-NA
informationaccess$Needs.addressed6[informationaccess$Needs.addressed6  ==999]<-NA
informationaccess$Listen.to.Needs27[informationaccess$Listen.to.Needs27    ==999]<-NA
#Seed.Seedling.1  
informationaccess$Freuency2[informationaccess$Freuency2==999]<-NA
informationaccess$Last.Assistance3.1[informationaccess$Last.Assistance3.1 ==999]<-NA
informationaccess$Needs.Addressed4.1[informationaccess$Needs.Addressed4.1==999]<-NA
informationaccess$Listen.to.Needs3.1[informationaccess$Listen.to.Needs3.1==999]<-NA

#Pesticide.Chemical.1  
informationaccess$Frequency2[informationaccess$Frequency2==999]<-NA
informationaccess$Last.Assistance4.1[informationaccess$Last.Assistance4.1 ==999]<-NA
informationaccess$Needs.Addressed2.1  [informationaccess$Needs.Addressed2.1==999]<-NA
informationaccess$Listen.to.Needs4.1  [informationaccess$Listen.to.Needs4.1  ==999]<-NA

#Farm.Equipment.1  
informationaccess$Frequency3[informationaccess$Frequency3==999]<-NA
informationaccess$Last.Assistance5.1[informationaccess$Last.Assistance5.1==999]<-NA
informationaccess$Needs.Addressed5.1[informationaccess$Needs.Addressed5.1==999]<-NA
informationaccess$Listen.to.Needs5.1[informationaccess$Listen.to.Needs5.1==999]<-NA

#Product.Processor.1  
informationaccess$Frequency32.1[informationaccess$Frequency32.1==999]<-NA
informationaccess$Last.Assistance6.1[informationaccess$Last.Assistance6.1==999]<-NA
informationaccess$Needs.Addressed52.1[informationaccess$Needs.Addressed52.1==999]<-NA
informationaccess$Listen.to.Needs52.1[informationaccess$Listen.to.Needs52.1==999]<-NA

#Cash.Money.1  
informationaccess$Frequency33.1[informationaccess$Frequency33.1==999]<-NA
informationaccess$Last.Assistance7.1[informationaccess$Last.Assistance7.1==999]<-NA
informationaccess$Needs.Addressed53.1[informationaccess$Needs.Addressed53.1==999]<-NA
informationaccess$Listen.to.Needs7.1[informationaccess$Listen.to.Needs7.1==999]<-NA
#Cash.Money.2  
informationaccess$Frequency332.1  [informationaccess$Frequency332.1  ==999]<-NA
informationaccess$Assistance8.1[informationaccess$Assistance8.1==999]<-NA
informationaccess$Needs.Addressed532.1[informationaccess$Needs.Addressed532.1==999]<-NA
informationaccess$Listen.to.Needs6.1[informationaccess$Listen.to.Needs6.1==999]<-NA



#====================Agriculture.Dept  
#Training.2 
informationaccess$Frequency  [informationaccess$Frequency  ==999]<-NA
informationaccess$Last.Assistance2.1[informationaccess$Last.Assistance2.1==999]<-NA
informationaccess$Needs.addressed.1[informationaccess$Needs.addressed.1==999]<-NA
informationaccess$Listen.to.Needs2.1[informationaccess$Listen.to.Needs2.1==999]<-NA

#Seed.Seedling.2  
informationaccess$Freuency2.1  [informationaccess$Freuency2.1  ==999]<-NA
informationaccess$Last.Assistance3.2  [informationaccess$Last.Assistance3.2  ==999]<-NA
informationaccess$Needs.Addressed4.2  [informationaccess$Needs.Addressed4.2  ==999]<-NA
informationaccess$Listen.to.Needs3.2  [informationaccess$Listen.to.Needs3.2  ==999]<-NA
#Pesticide.Chemical.2  
informationaccess$Frequency2.1[informationaccess$Frequency2.1==999]<-NA
informationaccess$Last.Assistance4.2[informationaccess$Last.Assistance4.2==999]<-NA
informationaccess$Needs.Addressed2.2[informationaccess$Needs.Addressed2.2==999]<-NA
informationaccess$Listen.to.Needs4.2[informationaccess$Listen.to.Needs4.2==999]<-NA

#Farm.Equipment.2  
informationaccess$Frequency3.1[informationaccess$Frequency3.1==999]<-NA
informationaccess$Last.Assistance5.2[informationaccess$Last.Assistance5.2==999]<-NA
informationaccess$Needs.Addressed5.2[informationaccess$Needs.Addressed5.2==999]<-NA
informationaccess$Listen.to.Needs5.2[informationaccess$Listen.to.Needs5.2==999]<-NA

#Product.Processor.2    
informationaccess$Frequency32.2  [informationaccess$Frequency32.2  ==999]<-NA
informationaccess$Last.Assistance6.2  [informationaccess$Last.Assistance6.2  ==999]<-NA
informationaccess$Needs.Addressed52.2  [informationaccess$Needs.Addressed52.2  ==999]<-NA
informationaccess$Listen.to.Needs52.2  [informationaccess$Listen.to.Needs52.2  ==999]<-NA

#Cash.Money.2


#============================================================
##=================access to finance and credit

accesscredit<-subset(datamalawi,select=c(Taken_loan:Past.12months))
accesscredit$Taken_loan[accesscredit$Taken_loan==999]<-NA
check<-subset(accesscredit,(accesscredit>=1))
accesscredit$accesscredit
#ckeck<-ifelse(accesscredit$Taken_loan!="",1,0)
#MONTHS LOANS WERE TAKEN
accesscredit$Month.1<-factor(accesscredit$Month.1,levels=c(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))  
accesscredit$Month.2<-factor(accesscredit$Month.2,levels=c(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))  
accesscredit$Month.3<-factor(accesscredit$Month.3,levels=c(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))  

##reasons for taking a loan
accesscredit$Reason.1<-factor(accesscredit$Reason.1,levels=c(1:9),labels=c("pay school fees","buy food","pay for health care/ drugs","funeral expenses","pay farm hired labourers","buy household assets","buy agricultural inputs","start a nonagricultural business","others")) 
accesscredit$Reason.2<-factor(accesscredit$Reason.2,levels=c(1:9),labels=c("pay school fees","buy food","pay for health care/ drugs","funeral expenses","pay farm hired labourers","buy household assets","buy agricultural inputs","start a nonagricultural business","others")) 
accesscredit$Reason.3<-factor(accesscredit$Reason.3,levels=c(1:9),labels=c("pay school fees","buy food","pay for health care/ drugs","funeral expenses","pay farm hired labourers","buy household assets","buy agricultural inputs","start a nonagricultural business","others")) 

##loan source
accesscredit$Source.1<-factor(accesscredit$Source.1,levels=c(1:10),labels=c("relatives nad family members","friends in the village","friends outside the village","farmers' organization/club","NGOs","church/mosque","Banks","Micro-finance","Agricultural companies","Government"))
accesscredit$Source.2.1  <-factor(accesscredit$Source.2.1  ,levels=c(1:10),labels=c("relatives nad family members","friends in the village","friends outside the village","farmers' organization/club","NGOs","church/mosque","Banks","Micro-finance","Agricultural companies","Government"))
accesscredit$Source.3.1    <-factor(accesscredit$Source.3.1    ,levels=c(1:10),labels=c("relatives nad family members","friends in the village","friends outside the village","farmers' organization/club","NGOs","church/mosque","Banks","Micro-finance","Agricultural companies","Government"))
tab1(accesscredit$Source.3.1 )
#Amount_from
accesscredit$Amount_from[accesscredit$Amount_from==999]<-NA
accesscredit$Amount_to[accesscredit$Amount_to==999]<-NA
accesscredit$Interest[accesscredit$Interest==999]<-NA
accesscredit$Past.12months[accesscredit$Past.12months==999]<-NA
table(accesscredit$Past.12months)
summ(accesscredit$Amount_from)
data1<-subset(accesscredit,select=c(Taken_loan:Past.12months))
#data<-cbind(data,data1)
names(data)
###########================================================================================
##==============================farmers social network and coorperation groups
#====================================================================================================
socialnetwork<-subset(datamalawi,select=c(Farmers..club.group:Benefit3))
socialnetwork$Farmers..club.group<-factor(socialnetwork$Farmers..club.group,labels=c("Yes","No"),levels=c(1:2))
names(socialnetwork)
socialnetwork$Reason.1.1<- factor(socialnetwork$Reason.1.1,levels=c(1:10),labels=c("people uncooperative","don't see any need","lack of eqiuty","no one to take lead","previous attempt failed","my spouse is jealous","farmerss clubs do not exist here","lak of transparency","not profitable","other"))
socialnetwork$Reason.2.1  <- factor(socialnetwork$Reason.2.1  ,levels=c(1:10),labels=c("people uncooperative","don't see any need","lack of eqiuty","no one to take lead","previous attempt failed","my spouse is jealous","farmerss clubs do not exist here","lak of transparency","not profitable","other"))
socialnetwork$Reason.3.1  <- factor(socialnetwork$Reason.3.1  ,levels=c(1:10),labels=c("people uncooperative","don't see any need","lack of eqiuty","no one to take lead","previous attempt failed","my spouse is jealous","farmerss clubs do not exist here","lak of transparency","not profitable","other"))
socialnetwork$Objective.1 <-factor(socialnetwork$Objective.1,levels=c(1:14),labels=c("tree planting","livestock","credit","agricultural training","fertilizer distribution","diversified income","food for work","input distribution","hiv/aids support","funeral group","processing","marketing","radio listening","orphan care"))
socialnetwork$Objective.2   <-factor(socialnetwork$Objective.2  ,levels=c(1:14),labels=c("tree planting","livestock","credit","agricultural training","fertilizer distribution","diversified income","food for work","input distribution","hiv/aids support","funeral group","processing","marketing","radio listening","orphan care"))
socialnetwork$Objective.3     <-factor(socialnetwork$Objective.3    ,levels=c(1:14),labels=c("tree planting","livestock","credit","agricultural training","fertilizer distribution","diversified income","food for work","input distribution","hiv/aids support","funeral group","processing","marketing","radio listening","orphan care"))
socialnetwork$Benefit<-factor(socialnetwork$Benefit,levels=c(1:4),labels=c("sharing information","sharing resources","source of credit","risk sharing"))
socialnetwork$Benefit2  <-factor(socialnetwork$Benefit2  ,levels=c(1:4),labels=c("sharing information","sharing resources","source of credit","risk sharing"))
socialnetwork$Benefit3  <-factor(socialnetwork$Benefit3,levels=c(1:4),labels=c("sharing information","sharing resources","source of credit","risk sharing"))
socialnetwork<-subset(socialnetwork,select=c(Farmers..club.group:Objective.3))

#data<-cbind(data,socialnetwork)
names(data)
##agricultural related group dynamics
agriculturalrelated<-subset(datamalawi,select=c(Communication:Information))

#=========================================================================================================

##============================ENERGY AND COOKING=========================================================
#===========================================================================================================
enegyandcooking<-subset(datamalawi,select=c(Source1:To..which.months.are.there.problems.with.fuelwood.))
names(enegyandcooking)
#main source of eenrgy
enegyandcooking$Source1<-factor(enegyandcooking$Source1,levels=c(1:10),labels=c("Crop residual","Wood","Charcoal","Saw dust","Gas","Electricity","Solar","Biogas","Paraffin","Others"))
tab1(enegyandcooking$Source1)
enegyandcooking$Source2.1<-factor(enegyandcooking$Source2.1,levels=c(1:10),labels=c("Crop residual","Wood","Charcoal","Saw dust","Gas","Electricity","Solar","Biogas","Paraffin","Others"))
tab1(enegyandcooking$Source2.1)
#do you grow trees meant for use a fuelwood
enegyandcooking$Fuelwood<-factor(enegyandcooking$Fuelwood,levels=c(1:2),labels=c("Yes","No"))
tab1(enegyandcooking$Fuelwood)
enegyandcooking$Obtain.Fuelwood1<-factor(enegyandcooking$Obtain.Fuelwood1,levels=c(1:5),labels=c("Own field","Common forest/community woodlot","Estate farms","Bought from wood sellers","State forest"))
enegyandcooking$Obtain.Fuelwood2  <-factor(enegyandcooking$Obtain.Fuelwood2  ,levels=c(1:5),labels=c("Own field","Common forest/community woodlot","Estate farms","Bought from wood sellers","State forest"))
tab1(enegyandcooking$Fuelwood)

#renaming the variables
rename.var(Source1,Source1_energycooking,enegyandcooking)
rename.var(Source2.1,Source2.1_energycooking,enegyandcooking)
rename.var(Fuelwood,grow_trees_Fuelwood,enegyandcooking)
rename.var(Time.taken,Time.taken_fetchwood_bush_forest,enegyandcooking)
rename.var(Household1,Household1_units_fuelwooduse_weekly,enegyandcooking)
rename.var(Household2,Household2_unitsown_trees_fuelwood_weekly,enegyandcooking)
#removing Nas from energy and cooking
enegyandcooking$Time.taken_fetchwood_bush_forest[enegyandcooking$Time.taken_fetchwood_bush_forest==999]<-NA  


#problems obtaining wood
enegyandcooking$Problems...Fuelwood<-factor(enegyandcooking$Problems...Fuelwood,levels=c(1:2),labels=c("Yes","No"))

##months when there problems getting fuel
enegyandcooking$From..which.months.are.there.problems.with.fuelwood[enegyandcooking$From..which.months.are.there.problems.with.fuelwood==999]<-NA
enegyandcooking$To..which.months.are.there.problems.with.fuelwood.[enegyandcooking$To..which.months.are.there.problems.with.fuelwood.==999]<-NA

rename.var(From..which.months.are.there.problems.with.fuelwood,fromwhichmonthfuelproblems,enegyandcooking)
rename.var(To..which.months.are.there.problems.with.fuelwood.,towhichmonthfuelproblems,enegyandcooking)

enegyandcooking$fueldeficitmonths<-abs(enegyandcooking$fromwhichmonthfuelproblems-enegyandcooking$towhichmonthfuelproblems)

enegyandcooking$Household1_units_fuelwooduse_weekly [enegyandcooking$Household1_units_fuelwooduse_weekly==999]<-NA 
enegyandcooking$Household2_unitsown_trees_fuelwood_weekly[enegyandcooking$Household2_unitsown_trees_fuelwood_weekly==999]<-NA

#*******************************************
#variables used in energy and cooking
#**********************************************
enegyandcooking<-subset(enegyandcooking,select=c(Source1_energycooking,Source2.1_energycooking,Obtain.Fuelwood1,Obtain.Fuelwood2,Time.taken_fetchwood_bush_forest))

#data<-cbind(data,enegyandcooking)
names(data)
#********************************************************==========================================
####========================HOUSEHOLD ASSETS AND RESOURCES 
#*******************************************************==========================================
##========farm implements
farmimplements<-subset(datamalawi,select=c(Ox.Cart:Wheel.barrow))
names(farmimplements)
farmimplements$Ox.Cart<-factor(farmimplements$Ox.Cart,levels=c(1:2),labels=c("Yes","No"))
farmimplements$Axe<-factor(farmimplements$Axe,levels=c(1:2),labels=c("Yes","No"))
farmimplements$Hoes<-factor(farmimplements$Hoes,levels=c(1:2),labels=c("Yes","No"))
farmimplements$Sprayers<-factor(farmimplements$Sprayers,levels=c(1:2),labels=c("Yes","No"))
farmimplements$Treadle.pump  <-factor(farmimplements$Treadle.pump  ,levels=c(1:2),labels=c("Yes","No"))
farmimplements$Plough  <-factor(farmimplements$Plough  ,levels=c(1:2),labels=c("Yes","No"))
farmimplements$Motorized.Irrigation.pump<-factor(farmimplements$Motorized.Irrigation.pump    ,levels=c(1:2),labels=c("Yes","No"))
farmimplements$Slasher.cutlass <-factor(farmimplements$Slasher.cutlass,levels=c(1:2),labels=c("Yes","No"))
farmimplements$Wheel.barrow <-factor(farmimplements$Wheel.barrow ,levels=c(1:2),labels=c("Yes","No"))
farmimplements<-subset(farmimplements,select=c(Ox.Cart:Wheel.barrow))

data<-cbind(data,farmimplements)
names(data)

###==== house hold assets
householdassets<-subset(datamalawi,select=c(Bicycle:Vehicle))
names(householdassets)
householdassets$Bicycle<-factor(householdassets$Bicycle,levels=c(1:2),labels=c("Yes","No"))
householdassets$Radio<-factor(householdassets$Radio,levels=c(1:2),labels=c("Yes","No"))
householdassets$Motorcycle<-factor(householdassets$Motorcycle,levels=c(1:2),labels=c("Yes","No"))
householdassets$TV<-factor(householdassets$TV,levels=c(1:2),labels=c("Yes","No"))
householdassets$Mobile.Phone<-factor(householdassets$Mobile.Phone  ,levels=c(1:2),labels=c("Yes","No"))
householdassets$Vehicle<-factor(householdassets$Vehicle,levels=c(1:2),labels=c("Yes","No"))
householdassets<-subset(householdassets,select=c(Bicycle:Vehicle))

data<-cbind(data,householdassets)
names(data)


##livestock assets
livestockassets<-subset(datamalawi,select=c(Beef.Cattle:Dairy.Cattle))
names(livestockassets)
livestockassets$Beef.Cattle<-factor(livestockassets$Beef.Cattle,levels=c(1:2),labels=c("Yes","No"))
livestockassets$Goats<-factor(livestockassets$Goats,levels=c(1:2),labels=c("Yes","No"))
livestockassets$Pig<-factor(livestockassets$Pig,levels=c(1:2),labels=c("Yes","No"))
livestockassets$Sheep<-factor(livestockassets$Sheep,levels=c(1:2),labels=c("Yes","No"))
livestockassets$Chicken<-factor(livestockassets$Chicken,levels=c(1:2),labels=c("Yes","No"))
livestockassets$Rabbit<-factor(livestockassets$Rabbit,levels=c(1:2),labels=c("Yes","No"))                                                                          
livestockassets$Dairy.Cattle<-factor(livestockassets$Dairy.Cattle,levels=c(1:2),labels=c("Yes","No"))
livestockassets<-subset(livestockassets,select=c(Beef.Cattle,Goats,Pig,Dairy.Cattle,Sheep,Chicken,Rabbit))
names(livestockassets)
data<-cbind(data,livestockassets)
names(data)
##getting the means of the assets
assets<-cbind(livestockassets,householdassets,farmimplements)
summ(assets)
assets<-data.frame(summ(assets)$table)
write.xlsx(assets,"aggregate.xlsx",col.names=T,row.names=T,sheetName="asset",append=T)

###======livelihood and revenue from livestock
livelihood<-subset(datamalawi,select=c(Salaried.Male:Egg.Sales..Kwacha.))
names(livelihood) 
#salaried male
livelihood$Salaried.Male<-factor(livelihood$Salaried.Male,labels=c("Yes","No"),levels=c(1,2))
#salried female
livelihood$Salaried.Female<-factor(livelihood$Salaried.Female,labels=c("Yes","No"),levels=c(1,2))
livelihood$Business<-factor(livelihood$Business,labels=c("Yes","No"),levels=c(1,2))
#type of work
livelihood$Work<-factor(livelihood$Work,labels=c("Piecework","Civil servant","Self employed","Brick making","Other"),levels=c(1:5))
#length of working in  months
livelihood$Length[livelihood$Length==999]<-NA
rename.var(Length,Length_of_working,livelihood)
#TYPE OF BUSNESS 
livelihood$Type[livelihood$Type==999]<-NA
livelihood$Type<-factor(livelihood$Type,levels=c(1:6),labels=c("Craftman","Sell fruit and vegetables","Sell livestock","Sell fish","Sell fuel","Other"))
#length of business operation in months
livelihood$Length2[livelihood$Length2==999]<-NA
rename.var(Length2,length_of_business_operation,livelihood)

##compare revenue from non-farm to farm activities
livelihood$Revenue<-factor(livelihood$Revenue,labels=c("Much greater","Slightly greater","About the same","Slightly less","Much less","Do not engage"),levels=c(1:6))
dataa<-subset(livelihood,select=c(Salaried.Male,Salaried.Female,Business,Revenue))
tab1(livelihood$Revenue)
#data<-cbind(data,dataa)
names(data)
##===========livestock sales
livelihood$Livestock.Sales..Kwacha.[livelihood$Livestock.Sales..Kwacha.==0]<-NA
livelihood$Milk.Sales..Kwacha.  [livelihood$Milk.Sales..Kwacha.==0]<-NA
livelihood$Egg.Sales..Kwacha.[livelihood$Egg.Sales..Kwacha.==0]<-NA


##=========LIVESTOCK REVENUE
livestockrevenue<-subset(livelihood,select=c(Livestock.Sales..Kwacha.:Egg.Sales..Kwacha.))
class(livestockrevenue$Livestock.Sales..Kwacha.)
livestockrevenue$Livestock.Sales..Kwacha.<-as.numeric(livestockrevenue$Livestock.Sales..Kwacha.)
livestockrevenue$Milk.Sales..Kwacha.  <-as.numeric(livestockrevenue$Milk.Sales..Kwacha.  )
livestockrevenue$Egg.Sales..Kwacha.<-as.numeric(livestockrevenue$Egg.Sales..Kwacha.
)

livestockrevenue$Livestock_Total<-rowSums(livestockrevenue,na.rm=T)
dataa<-subset(livestockrevenue,select=c(Livestock_Total))
data.frame(summ(livestockrevenue))
write.xlsx(assets,"aggregate.xlsx",col.names=T,row.names=T,sheetName="asset",append=T)

#data<-cbind(data,dataa)
names(data)
###quality of housing
housingquality<-subset(datamalawi,select=c(Description:Description2))
rename.var(Description,Housewalls,housingquality)
rename.var(Description2,House_Roof,housingquality)

housingquality$Housewalls<-factor(housingquality$Housewalls,levels=c(1:4),labels=c("Poles and Mud","Mud/Mud-brick","Brick","Cement"))
housingquality$House_Roof<-factor(housingquality$House_Roof,levels=c(1:4),labels=c("Thatch","Iron sheet","Asbsetos","Tile"))
housingquality<-subset(housingquality,select=c(Housewalls,House_Roof))
names(housingquality)
data<-cbind(data,housingquality)
names(data)
write.xlsx(data,"followup_variables.xlsx",col.names=T,row.names=F)

##=====incentive/disentive

incentive<-subset(datamalawi,select=c(Financial.Benefit:Environmental.Benefits.Costs))
##======future
futureplans<-subset(datamalawi,select=c(Future:Constraints5))
dataaa<-data.frame(summ(data)$table)
write.xlsx(dataaa,"aggregate.xlsx",col.names=T,row.names=T,sheetName="datai",append=T)


################END END END END END END END#######################################################

#fit1<-glm(fertilizer~Education+Age1+Sex.2+Wife.Knowledge.before+Trend+Farm.Size_use+Roof_quality+wall_quality+Own.Fruit,data,family="binomial")
#summary(fit1)

#exp(coef(fit1))
names(data)




demographicfactors<-subset(data,select=c(intervewiee_relation_HH:Marital.Status,Education,Occupation,Participation,Male_farmer,Female_farmer,Child_farmer))
demogfactors<-data.frame(summ(demographicfactors)$table)
write.xlsx(demogfactors,"aggregate.xlsx",col.names=T,row.names=T,sheetName="demoduse",append=T)

socialeconomicfactors<-subset(data,select=c(Farm.Size,Totalfarmimplements,Total_household_assets,Total_livestock_assets,wall_quality,Roof_quality,Taken_loan,Farmers..club.group,Revenue))
socialeconomic<-data.frame(summ(socialeconomicfactors)$table)
write.xlsx(socialeconomic,"aggregate.xlsx",col.names=T,row.names=T,sheetName="socialecon",append=T)


ecologicalfactors<-subset(data,select=c(Indicator:Indicator3,Soil.Type,Status,Trend,Trend.Reason3,Fertilizer.Quantity:Fertilizer.Trend.Reason3,Trend))
rename.var(Status,soilfertilitystatus,ecologicalfactors)
ecofactors<-data.frame(summ(ecologicalfactors)$table)
write.xlsx(ecofactors,"aggregate.xlsx",col.names=T,row.names=T,sheetName="ecofacts",append=T)





















































































































































































































































































































































































































































































































































































































