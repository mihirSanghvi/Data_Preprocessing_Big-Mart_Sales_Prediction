#####################################################################################################
#########                READ DATA, DATA EXPLORATION                                        ######### 
#####################################################################################################

# Read CSV File
Train_Dateset <- read.csv(file = "F:\\OPIM-5604-Predictive Modeling-SECB14-1163\\datasets\\Project_Data\\Source_Data1\\Train_Data.csv")

# Get the feel of the data by using head, summary, names and str functions 
head(Train_Dateset)
summary(Train_Dateset$Item_Fat_Content)
str(Train_Dateset)
names(Train_Dateset)
View(Train_Dateset)

summary(Train_Dateset$Item_Type)

# Attached Train_Dataset dataframe into R search path
attach(Train_Dateset)

#####################################################################################################
#########                DEALING WITH MISSING VALUES                                        ######### 
#####################################################################################################

# Total Number of records 
nRows = nrow(Train_Dateset)

# Total Number of records without any mmissing values 
nComplete = sum(complete.cases(Train_Dateset))

# If we remove all the rows with missing values then, we are losing about 17.18% of the data 
dataLoss = (1- nComplete/nRows)*100 
dataLoss

#####################################################################################################
#########                DEALING WITH MISSING VALUES                                        ######### 
#####################################################################################################

# Item_Visibility has 0's and it is impracital to have zero visiblity in any store for any product
# Replace 0's with NA's 

Train_Dateset$Item_Visibility.Corrected = Train_Dateset$Item_Visibility
Train_Dateset$Item_Visibility.Corrected[Train_Dateset$Item_Visibility.Corrected == 0 ] = NA

summary(Train_Dateset$Item_Visibility.Corrected)
length(Train_Dateset$Item_Visibility[Train_Dateset$Item_Visibility==0])

# Outlet_Size contains empty values (it is not NA/NAN/NULL but it's empty)
# replace Empty values with NA's
Train_Dateset$Outlet_Size.Corrected = Train_Dateset$Outlet_Size
Train_Dateset$Outlet_Size.Corrected[Train_Dateset$Outlet_Size.Corrected==""] = NA

summary(Train_Dateset$Outlet_Size.Corrected)
length(Train_Dateset$Outlet_Size[Train_Dateset$Outlet_Size==""])


#####################################################################################################
#########                       ITEM_VISIBLITY                                              ######### 
#####################################################################################################

# Need to convert visiblity to scale of 100% from normalized scale of 1 
Train_Dateset$Item_Visibility.Corrected = Train_Dateset$Item_Visibility.Corrected * 100

#####################################################################################################
#########               MISSING DATA PATTERN VISUALIZATION                                  ######### 
#####################################################################################################


# Check missing data pattern
# install.packages("mice")
library(mice)

md.pattern(Train_Dateset)

# Visulize the missing data pattern 
# install.packages("VIM")
library(VIM)
mice_plot <- aggr(Train_Dateset[,c("Item_Weight","Outlet_Size.Corrected","Item_Visibility.Corrected")], col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=c("Item_Weight","Outlet_Size","Item_Visibility"), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))



#####################################################################################################
#########                 Cleaning up Factor levels : Item_Fat_Content                      ######### 
#####################################################################################################

summary(Item_Fat_Content)

# Create new variable Item_Fat_Content.f 
Train_Dateset$Item_Fat_Content.f = factor(Train_Dateset$Item_Fat_Content)
levels(Train_Dateset$Item_Fat_Content.f) = list(Regular = c("Regular","reg"), LowFat = c("Low Fat","LF","low fat"))

summary(Train_Dateset$Item_Fat_Content.f)

#####################################################################################################
#########             MISSING VALUE TREATMENT : ITEM_WEIGHT                             ############# 
#########           IMPUTATION TECHNIQUE : HOT DECK IMPUTATION                          #############
#####################################################################################################

# Item_Weight Column has missing values (Total 976 Missing Values)
summary(Item_Weight)


# STEP - I 
# Add new Column which indecates Missing Value (MI = Missing-Value Indicator)
Train_Dateset$Item_Weight_MI[is.na(Item_Weight)]=1
Train_Dateset$Item_Weight_MI[!is.na(Item_Weight)]=0

# Convert that Column form numeric to factor 
Train_Dateset$Item_Weight_MI = factor(Train_Dateset$Item_Weight_MI)


# Confirm missing values using summary (Total 976 Missing Values)
summary(Train_Dateset$Item_Weight_MI)

# install.packages("plyr")
library(plyr)
# Same Item is present in various stores, check if same item identifier has few missing values 

# Group by observations using count_ function from plyr package 
Missing_Value_Dependency_Check = data.frame(count(Train_Dateset,vars = c("Item_Identifier","Item_Weight_MI","Item_Weight")))

# Weight associated with an item id will be same 
head(Missing_Value_Dependency_Check)


# Get Item_Weight and Item_Weight cols in a new data frame (ommit rows with NA values within item_weight column)
Frame1 = Train_Dateset[!is.na(Item_Weight),c("Item_Weight","Item_Identifier")]
colnames(Frame1) = c("Item_Weight_Imputed","Item_Identifier")

# Join Frame1 and Train_Dataset into Train_Dataset
Train_Dateset = merge(x=Train_Dateset,y=Frame1,by.x="Item_Identifier",by.y = "Item_Identifier")
names(Train_Dateset)

# HOT DECK IMPUTATION COMPLETE 
head(Train_Dateset[,c("Item_Weight_MI","Item_Weight","Item_Weight_Imputed","Item_Identifier")],n = 50)
summary(Train_Dateset$Item_Weight_Imputed)

#####################################################################################################
#########               MISSING VALUE TREATMENT : OUTLET_SIZE                               ######### 
#####################################################################################################
# Missing Completely at random (MACR)
# Missing not at random (MNAR)

# Check total percentage of missing data 
sum(is.na(Train_Dateset$Outlet_Size.Corrected))/length(Train_Dateset$Outlet_Size)*100


library(plyr)
# By only looking at the summary data based on Outlet_Type and Outlet_Location_Type combination we can impute NA's 
count(Train_Dateset,vars = c("Outlet_Type","Outlet_Location_Type","Outlet_Establishment_Year","Outlet_Size.Corrected"))

# Create new column Outlet_Size_Imputed

Train_Dateset$Outlet_Size_Imputed = Train_Dateset$Outlet_Size.Corrected
Train_Dateset$Outlet_Size_Imputed = factor(Train_Dateset$Outlet_Size_Imputed)

# impute NA's with Small (Based on the Observed pattern (Hot Decking Imputation))

Train_Dateset$Outlet_Size_Imputed[is.na(Train_Dateset$Outlet_Size_Imputed)] = "Small" 
# summary(Train_Dateset$Outlet_Size_Imputed)



#####################################################################################################
#########               DUMMY /TREATMENT CODING fOR CATEGORICAL VARIABLES                   ######### 
#####################################################################################################

# Create new variable Item_Fat_Content_Recoded
expression1 = ifelse(Train_Dateset$Item_Fat_Content.f=="Regular",0,1)
Train_Dateset$Item_Fat_Content_Recoded = factor(expression1)
summary(Train_Dateset$Item_Fat_Content_Recoded)


# Create new variable Outlet_Size_Recoded
summary(Train_Dateset$Outlet_Size_Imputed)
expression2 = ifelse(Train_Dateset$Outlet_Size_Imputed == "Small",1,ifelse(Train_Dateset$Outlet_Size_Imputed == "Medium",2,3))
Train_Dateset$Outlet_Size_Recoded = ordered(expression2)
# Since Outlet_Size is an Ordinal Variable we have to make it ordered factor 
contrasts(Train_Dateset$Outlet_Size_Recoded) = contr.treatment(3)
#Train_Dateset$Outlet_Size_Recoded
summary(Train_Dateset$Outlet_Size_Recoded)


# Create new variable Outlet_Location_Type_Recoded
summary(Train_Dateset$Outlet_Location_Type)
expression3 = ifelse(Train_Dateset$Outlet_Location_Type=="Tier 1",1,ifelse(Train_Dateset$Outlet_Location_Type=="Tier 2",2,3))
Train_Dateset$Outlet_Location_Type_Recoded = factor(expression3)
summary(Train_Dateset$Outlet_Location_Type_Recoded)

# Create new variable Outlet_Type_Recoded
summary(Train_Dateset$Outlet_Type)
expression4 = ifelse(Train_Dateset$Outlet_Type=="Grocery Store",0,ifelse(Train_Dateset$Outlet_Type=="Supermarket Type1",1,ifelse(Train_Dateset$Outlet_Type=="Supermarket Type2",2,3)))
Train_Dateset$Outlet_Type_Recoded = factor(expression4)
summary(Train_Dateset$Outlet_Type_Recoded)

View(Train_Dateset)
str(Train_Dateset)




#################################################################################
########              Save data to new DataFrame                          ######
#################################################################################

Train_Dateset_New <- Train_Dateset
Train_Dateset_New <- Train_Dateset_New[-c(2,3,4,9,10,11,13,14,16,17)]

colnames(Train_Dateset_New) <- c("Item_Identifier","Item_Type","Item_MRP","Outlet_Identifier","Outlet_Establishment_Year",
                                 "Item_Visibility","Item_Weight","Item_Fat_Content","Outlet_Size",
                                 "Outlet_Location_Type","Outlet_Type")

summary(Train_Dateset_New)
str(Train_Dateset_New)

#######################################################################################
######                       Feature Engineering                               ########
#######################################################################################

# Create new variable Outlet_Age 
Train_Dateset_New$Outlet_Age <- (2016 - Train_Dateset_New$ Outlet_Establishment_Year)

# Create new variable Item_Id
Train_Dateset_New$Item_Id = factor(substr(Train_Dateset_New$Item_Identifier,1,2))


str(Train_Dateset_New)


#########################################################################################
###########             Item Visiblity Imputation                             ###########
#########################################################################################

# Creae new dataframe and remove Item_Identifier 
Train_Dateset_New1 <- Train_Dateset_New
Train_Dateset_New1 = Train_Dateset_New1[-c(1)]

#install.packages("missForest")
library(missForest)
Train_Dateset_New1.imp <- missForest(Train_Dateset_New1)

# Check normalized mean squared error in imputation
Train_Dateset_New1.imp$OOBerror

head(Train_Dateset_New1.imp$ximp$Item_Visibility)

#######################################################################################
####            Add Imputed Visiblity to Train_Dateset_New Datafrane           ########
#######################################################################################

Train_Dateset_New$Item_Visibility = Train_Dateset_New1.imp$ximp$Item_Visibility
str(Train_Dateset_New)
summary(Train_Dateset_New)
