#Analytics Vidhya Hackathon
#Lord Of the Machines
#Email Campaign Classification Problem
#https://datahack.analyticsvidhya.com/contest/lord-of-the-machines/

#Setting Required Options
rm(list = ls())
options(warn = -1)
setwd("/Users/ejain/Documents/Learning/AV_Hack/AVHACK4_LORD OF THE MACHINES/")

#Call Required Packages
library(data.table)
library(lubridate)
detach("package:h2o",  unload = TRUE)
library(dummies)
library(dplyr)


#Data Exploration
Train_Raw <- fread("train.csv", data.table = FALSE, header = TRUE) # 1023191 X 6
Test_Raw <- fread("test_BDIfz5B.csv", data.table = FALSE, header = TRUE) #773858 X 4
Campaign_Raw <- fread("campaign_data.csv", data.table = FALSE, header = TRUE) #52 X 9

#Merge User and Campaign Data
Train_Raw <- merge(Train_Raw, Campaign_Raw, by = c("campaign_id"), all.x = TRUE)
Test_Raw <- merge(Test_Raw, Campaign_Raw, by = c("campaign_id"), all.x = TRUE)

#Exploratory
#sum(!(unique(Test$user_id) %in% unique(Train$user_id)))


#Create New Variables
Train_Raw$Date <- parse_date_time(Train_Raw$send_date, orders="dmy hm") #Without h2o package
Train_Raw$DOW <- weekdays(Train_Raw$Date)
Train_Raw$Hour <- hour(Train_Raw$Date) + minute(Train_Raw$Date)/60
Train_Raw$MorningMail <- (Train_Raw$Hour < 12)
Train_Raw$AfternoonMail <- (Train_Raw$Hour > 12 & Train_Raw$Hour < 18)
Train_Raw$Hour_Rnd <- round((hour(Train_Raw$Date) + minute(Train_Raw$Date)/60), digits = 0)

Test_Raw$Date <- parse_date_time(Test_Raw$send_date, orders="dmy hm")
Test_Raw$DOW <- weekdays(Test_Raw$Date)
Test_Raw$Hour <- hour(Test_Raw$Date) + minute(Test_Raw$Date)/60
Test_Raw$MorningMail <- (Test_Raw$Hour < 12)
Test_Raw$AfternoonMail <- (Test_Raw$Hour > 12 & Test_Raw$Hour < 18)
Test_Raw$Hour_Rnd <- round((hour(Test_Raw$Date) + minute(Test_Raw$Date)/60), digits = 0)


#Existing Users Open Rate and Click Rate 
Open_Rates <- Train_Raw %>% group_by(user_id) %>% summarize(Avg_Open = mean(is_open))
Click_Rates <- Train_Raw %>% group_by(user_id) %>% summarize(Avg_Click = mean(is_click))

Train_Raw <- merge(Train_Raw, Open_Rates, by = c("user_id"), all.x = TRUE)
Train_Raw <- merge(Train_Raw, Click_Rates, by = c("user_id"), all.x = TRUE)

Test_Raw <- merge(Test_Raw, Open_Rates, by = c("user_id"), all.x = TRUE)
Test_Raw <- merge(Test_Raw, Click_Rates, by = c("user_id"), all.x = TRUE)

Test_Raw$Avg_Click[is.na(Test_Raw$Avg_Click)] <- mean(Train_Raw$Avg_Click, na.rm = TRUE)
Test_Raw$Avg_Open[is.na(Test_Raw$Avg_Open)] <- mean(Train_Raw$Avg_Open, na.rm = TRUE)

#Avg Hour Round - Open Rates and Click Rates
Hour_Open_Click_Rate <- Train_Raw %>% group_by(Hour_Rnd) %>% summarize(Hour_Mean_Open = mean(is_open) , Hour_Mean_Click = mean(is_click))

Train_Raw <- merge(Train_Raw, Hour_Open_Click_Rate, by = c("Hour_Rnd"), all.x = TRUE)
Test_Raw  <- merge(Test_Raw, Hour_Open_Click_Rate, by = c("Hour_Rnd"), all.x = TRUE)

Test_Raw$Hour_Mean_Open[which(Test_Raw$Hour_Rnd %in% c(6,8))] <- mean(Train_Raw$Hour_Mean_Open[which(Train_Raw$Hour_Rnd <= 12)])
Test_Raw$Hour_Mean_Click[which(Test_Raw$Hour_Rnd %in% c(6,8))] <- mean(Train_Raw$Hour_Mean_Click[which(Train_Raw$Hour_Rnd <= 12)])

Test_Raw$Hour_Mean_Open[which(Test_Raw$Hour_Rnd %in% c(19,21))] <- mean(Train_Raw$Hour_Mean_Open[which(Train_Raw$Hour_Rnd > 18)])
Test_Raw$Hour_Mean_Click[which(Test_Raw$Hour_Rnd %in% c(19,21))] <- mean(Train_Raw$Hour_Mean_Click[which(Train_Raw$Hour_Rnd > 18)])


#Communication Type and DOW
Train_Raw[,c("DOW","communication_type","is_click")] <- lapply(Train_Raw[,c("DOW","communication_type","is_click")], as.factor)
Test_Raw[,c("DOW","communication_type")] <- lapply(Test_Raw[,c("DOW","communication_type")], as.factor)


Train <- dummy.data.frame(Train_Raw, names = c("DOW"), sep = "_")
Train <- dummy.data.frame(Train, names = c("communication_type"), sep = "_")

Test <- dummy.data.frame(Test_Raw, names = c("DOW"), sep = "_")
Test <- dummy.data.frame(Test, names = c("communication_type"), sep = "_")

#names(Train)[! names(Train) %in% names(Test)]
Test$communication_type_Conference <- 0
Test$communication_type_Others <- 0
Test$communication_type_Webinar <- 0
Test$DOW_Saturday <- 0
Test$DOW_Sunday <- 0

#Number of mails sent to user and Time Since Last Mail Click
Total_Mails <- as.data.frame(rbind(Train[,c("user_id", "Date")], Test[,c("user_id", "Date")]))

Mails_Number <- Total_Mails %>% arrange(user_id, Date) %>%  group_by(user_id) %>% mutate(Mails_No = dense_rank(Date))

Min_Date <- Total_Mails %>% arrange(user_id, Date) %>%  group_by(user_id) %>% summarize(Min_Date = min(Date))


Last_Mail <- Total_Mails %>% arrange(user_id, Date) %>%  group_by(user_id) %>% mutate(diff = (Date - lag(Date, default=first(Date)))/(60*60*24))
#Set default difference as 500 days
Last_Mail$diff[Last_Mail$diff == 0] <- 500
Last_Mail$diff <- as.numeric(Last_Mail$diff)


#Append Total Mails Sent So far
Train <- merge(Train, Mails_Number, by = c("user_id", "Date"), all.x = TRUE)
Test <- merge(Test, Mails_Number, by = c("user_id", "Date"), all.x = TRUE)

#Append Min Date and Take The Difference
Train <- merge(Train, Min_Date, by = c("user_id"), all.x = TRUE)
Train$TimeSinceFirstEmail <- as.numeric(Train$Date - Train$Min_Date)/(60*60*24)

Test <- merge(Test, Min_Date, by = c("user_id"), all.x = TRUE)
Test$TimeSinceFirstEmail <- as.numeric(Test$Date - Test$Min_Date)/(60*60*24)


#Append Last Mail
Train <- merge(Train, Last_Mail, by = c("user_id", "Date"), all.x = TRUE)
Test <- merge(Test, Last_Mail, by = c("user_id", "Date"), all.x = TRUE)

#Number of mails sent for certain campaign
Campaign_Mails <- unique(as.data.frame(rbind(Train[,c("campaign_id", "Date")], Test[,c("campaign_id", "Date")])))

Campaign_Mails_Total <- Campaign_Mails %>% arrange(campaign_id, Date) %>%  group_by(campaign_id) %>% mutate(Camp_Tot_Mails = dense_rank(Date))

Campaign_Min_Date <- Campaign_Mails %>% arrange(campaign_id, Date) %>% group_by(campaign_id) %>% summarize(Campaign_Min_Date = min(Date))


#Append Total Mails Sent So far
Train <- merge(Train, Campaign_Mails_Total, by = c("campaign_id", "Date"), all.x = TRUE)
Test <- merge(Test, Campaign_Mails_Total, by = c("campaign_id", "Date"), all.x = TRUE)

#Append Min Date and Take The Difference
Train <- merge(Train, Campaign_Min_Date, by = c("campaign_id"), all.x = TRUE)
Train$TimeSinceCampaignFrstEmail <- as.numeric(Train$Date - Train$Campaign_Min_Date)/(60*60*24)

Test <- merge(Test, Campaign_Min_Date, by = c("campaign_id"), all.x = TRUE)
Test$TimeSinceCampaignFrstEmail <- as.numeric(Test$Date - Test$Campaign_Min_Date)/(60*60*24)



#____________________________________________________________________________________________________
# Using h20 package to build GBM
library(h2o)
h2o.init()


#Validation Set Creation for hyperparameter tuning
set.seed(123)
#smp_size <- floor(0.8 * nrow(Train))
smp_size <- floor(nrow(Train))
train_ind <- sample(seq_len(nrow(Train)), size = smp_size)
Train <- Train[train_ind, ]
Valid <- Train[-train_ind,]

Vars_Rmv <- c("is_open","user_id","campaign_id","id","email_body","subject","email_url","Date", "send_date", "Min_Date", "Campaign_Min_Date" )

# "TimeSinceCampaignFrstEmail",, "Camp_Tot_Mails"

trainhex <- as.h2o(Train[,!names(Train) %in% Vars_Rmv ])


valid <- as.h2o(Valid[,!names(Train) %in% Vars_Rmv])
test <- as.h2o(Test[,!names(Test) %in% Vars_Rmv])


RF_Model <- h2o.randomForest(
  training_frame = trainhex,       
  #validation_frame = valid,     
  x=c(1,3:ncol(trainhex)),                       
  y= 2, 
  ntrees = 150,
  max_depth = 4,
  min_rows = 1000,
  balance_classes = TRUE,
  model_id = "RF1",
  stopping_metric = "logloss"
)
h2o.shutdown(prompt = TRUE)

summary(RF_Model)
Predictions <- as.data.frame(h2o.predict(object = RF_Model, newdata = test ))

Results <- as.data.frame(cbind(Test[,c("id")], Predictions$p1))
colnames(Results) <- c("id", "is_click")



#___________________________________________________________________________________________
#GBM Model

GBM.Model <- h2o.gbm(training_frame = train,       
                     validation_frame = valid,     
                     x=c(1,3:ncol(train)),                       
                     y= 2, 
                     ntrees = 200,
                     max_depth = 4,
                     learn_rate = 0.005,
                     seed = 1122, 
                     stopping_metric = "logloss")

summary(GBM.Model)
Predictions <- as.data.frame(h2o.predict(object = GBM.Model, newdata = test ))

Results <- as.data.frame(cbind(Test[,c("id")], Predictions$p1))
colnames(Results) <- c("id", "is_click")


#_______________________________________________________________________________________________________________
#Write Submission
write.csv(Results,"/Users/ejain/Documents/Learning/AV_Hack/AVHACK4_LORD OF THE MACHINES/Submission/Test_RF_All_Campaign_Cum_3.csv", row.names = FALSE)


#
#______________________________________________________________________________________________________________
#XGBboost
xgb <- xgboost(data = data.matrix(train[,-1]), 
               label = y, 
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 12,
               nthread = 3
)

#_______________________________________________________________________________________________________________
#Creating model equation
selection<-colnames(Train[,!names(Train) %in% c("is_click")])
Indeps <- paste(selection, collapse = "+")
eqn <- as.formula(paste("is_click", Indeps, sep = "~"))

eqn <- as.formula(is_click ~ DOW + MorningMail + AfternoonMail + communication_type + 
                    total_links + no_of_internal_links + no_of_images + no_of_sections)

#Model
model <- glm(eqn,family=binomial(link='logit'),data=Train)

fitted.results <- data.frame(Pred = predict(model,newdata=Test,type='response'), row.names = FALSE)
Results <- as.data.frame(cbind(Test[,c(1)], fitted.results), row.names = FALSE)
colnames(Results) <- c("id", "is_click")
