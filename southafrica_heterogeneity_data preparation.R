####################################################
#title: "southafrica_heterogeneity_data preparation"
#author:  "Sun Yeop Lee"
#date:  "5/26/2020"
#output:
#  html_document: default
####################################################

#packages
library(tidyverse)
library(mice)
library(dplyr)
library(tibble)
library(ggplot2)
library(skimr)
library(gtools)

### data manipulation ###

#import data
cov_data <- read.csv("C:\\Users\\USER\\Desktop\\Backup\\Harvard\\1_Harvard School Works\\Research\\Vulnerability-variability Project\\southafrica\\southafrica data\\Christy_dataset_2_sun_12.24.19.csv") # covariate dataset
nc_data <- read.csv("C:\\Users\\USER\\Desktop\\Backup\\Harvard\\1_Harvard School Works\\Research\\Vulnerability-variability Project\\southafrica\\southafrica data\\Dataset_Christy_Sun_2_27_20_edited by Sun.csv") # neurocognitive outcome dataset

#merge data
all_data <- merge(cov_data, nc_data, all.x=T)

#check for missing values in covariate dataset
na_count <-sapply(cov_data[,c("Age", "HIV_Status", "Education_2", "Maritalstatus", "Income", "Employed", "CESD_total", "DTS_Total")], function(y) sum(is.na(y)))
na_count <- data.frame(na_count)
#view(na_count)

#check for missing values in neurocognitive outcome dataset
na_count <-sapply(nc_data, function(y) sum(is.na(y)))
na_count <- data.frame(na_count)
#view(na_count)

#rename variable names
colnames(all_data)[1] <- "id"

#remove outliers
all_data <- all_data %>% 
  filter(id!="NM157-12")

all_data$Colortrails_1[all_data$id=="NM244-14"] <- NA

#drop unused factor levels
all_data$id <- factor(all_data$id)

# all_data$Pegs_dominant[all_data$id=="NM157-12"] <- NA
# all_data$Pegs_nondominant[all_data$id=="NM157-12" | all_data$id=="TS291-15" ] <- NA
# all_data$TrailsA_time[all_data$id=="NV221-13"] <- NA

#assign unique IDs to each subject
#all_data$ID <- rownames(all_data)





#convert variables to factor
cat_var <- c('Childhood_Trauma','Employed', 'HIV_Status', 'Education_2',
             "CTS_Negotiation_Total", "CTS_Psychological_Agression_Total", 
             "CTS_Physical_Assault_Total", "CTS_Sexual_Coercion_Total", "CTS_Injury_Total")
all_data[,cat_var] <- lapply(all_data[,cat_var], factor)





# #continuouse variables to categorical variables by quantiles
# all_data$CTQ_Total_2 <- quantcut(all_data$CTQ_Total, q = 2)
# all_data$CTQ_Total_2_test[all_data$CTQ_Total_2 == "(41,116]"] <- 1
# all_data$CTQ_Total_2_test[all_data$CTQ_Total_2 == "[25,41]"] <- 0
# all_data$CTQ_Total_3 <- quantcut(all_data$CTQ_Total, q = 3)

#center continuous variables at the mean
all_data$Age_c <- all_data$Age - mean(all_data$Age, na.rm=T)
all_data$CTQ_Total_c <- all_data$CTQ_Total - mean(all_data$CTQ_Total, na.rm=T)
all_data$LE_Total_c <- all_data$LE_Total - mean(all_data$LE_Total, na.rm=T)
all_data$CESD_total_c <- all_data$CESD_total - mean(all_data$CESD_total, na.rm=T)
all_data$DTS_Total_c <- all_data$DTS_Total - mean(all_data$DTS_Total, na.rm=T)


# #create squared terms for continous variables
# all_data$CTQ_Total_centered_squared <- all_data$CTQ_Total_centered^2

#create indicator variables
all_data$Age_3 <- NA
all_data$Age_3[all_data$Age<=20] <- 1
all_data$Age_3[all_data$Age>20 & all_data$Age<=30] <- 2
all_data$Age_3[all_data$Age>30 & all_data$Age<=40] <- 3
all_data$Age_3[all_data$Age>40] <- 4



all_data$Income_2 <- ifelse(as.numeric(all_data$Income) != 0, 0, 1)

all_data$Maritalstatus_3 <- NA
all_data$Maritalstatus_3[all_data$Maritalstatus == 1] <- 1
all_data$Maritalstatus_3[all_data$Maritalstatus == 2 | all_data$Maritalstatus == 3] <- 2
all_data$Maritalstatus_3[all_data$Maritalstatus == 4 | all_data$Maritalstatus == 5 | all_data$Maritalstatus == 6] <- 3

cat_var <- c('Income_2','Maritalstatus_3', "HIV_Status", "Education_2", "Employed","Age_3")
all_data[,cat_var] <- lapply(all_data[,cat_var], factor)

lapply(all_data[,cat_var], summary)

#separate coding
all_data$ct_yes <- ifelse(all_data$Childhood_Trauma == 1, 1, 0)
all_data$ct_no <- ifelse(all_data$Childhood_Trauma == 0, 1, 0)

all_data$CTS_Negotiation_yes <- ifelse(all_data$CTS_Negotiation_Total == 1, 1, 0)
all_data$CTS_Negotiation_no <- ifelse(all_data$CTS_Negotiation_Total == 0, 1, 0)

all_data$CTS_Psychological_Agression_yes <- ifelse(all_data$CTS_Psychological_Agression_Total == 1, 1, 0)
all_data$CTS_Psychological_Agression_no <- ifelse(all_data$CTS_Psychological_Agression_Total == 0, 1, 0)

all_data$CTS_Physical_Assault_yes <- ifelse(all_data$CTS_Physical_Assault_Total == 1, 1, 0)
all_data$CTS_Physical_Assault_no <- ifelse(all_data$CTS_Physical_Assault_Total == 0, 1, 0)

all_data$CTS_Sexual_Coercion_yes <- ifelse(all_data$CTS_Sexual_Coercion_Total == 1, 1, 0)
all_data$CTS_Sexual_Coercion_no <- ifelse(all_data$CTS_Sexual_Coercion_Total == 0, 1, 0)

all_data$CTS_Injury_yes <- ifelse(all_data$CTS_Injury_Total == 1, 1, 0)
all_data$CTS_Injury_no <- ifelse(all_data$CTS_Injury_Total == 0, 1, 0)

cat_var <- c('ct_yes','ct_no', "CTS_Negotiation_yes", "CTS_Negotiation_no",
             "CTS_Psychological_Agression_yes",'CTS_Psychological_Agression_no',
             'CTS_Physical_Assault_yes', "CTS_Physical_Assault_no",
             'CTS_Sexual_Coercion_yes', 'CTS_Sexual_Coercion_no',
             'CTS_Injury_yes', 'CTS_Injury_no')
all_data[,cat_var] <- lapply(all_data[,cat_var], factor)

lapply(all_data[,cat_var], summary)

# outcome variables
outcomes <- colnames(nc_data[5:6]) #for running shorter codes
outcomes <- colnames(nc_data[3:23])
outcomes

#skim
skim(select(all_data, outcomes))

#check normality
normal_variable <- all_data$Pegs_dominant                         
ggplot(all_data, aes(x=normal_variable)) + geom_histogram() # historgram
ggplot(all_data, aes(x=normal_variable)) + geom_density() # density curve
shapiro.test(normal_variable)

ggplot(all_data, aes(x = normal_variable)) +
  geom_histogram(aes(color = Childhood_Trauma, fill = Childhood_Trauma), 
                 position = "identity", bins = 30, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

# lapply(nc_data[3:23], shapiro.test)
# do.call(rbind, lapply(nc_data[3:23], function(x) shapiro.test(x)[c("statistic", "p.value")]))





# exposure variables
exposures <- c("Childhood_Trauma", "CTQ_PA", "CTQ_SA", "CTQ_EA", "CTQ_EN", "CTQ_PN", "CTQ_Total", "CTQ_Total_c", 
               "LE_Total", "LE_Total_c", "CTS_Negotiation_Total", "CTS_Psychological_Agression_Total", 
               "CTS_Physical_Assault_Total", "CTS_Sexual_Coercion_Total", "CTS_Injury_Total")
lapply(all_data[,exposures], summary)
exposures

# covariates
covariates <- c("Age", "Age_c","HIV_Status", "Education_2", "Maritalstatus", "Income", "Employed",
                'Income_2','Maritalstatus_3', 'CESD_total','CESD_total_c','DTS_Total','DTS_Total_c',
                'ct_yes','ct_no', "CTS_Negotiation_yes", "CTS_Negotiation_no",
                "CTS_Psychological_Agression_yes",'CTS_Psychological_Agression_no',
                'CTS_Physical_Assault_yes', "CTS_Physical_Assault_no",
                'CTS_Sexual_Coercion_yes', 'CTS_Sexual_Coercion_no',
                'CTS_Injury_yes', 'CTS_Injury_no')
lapply(all_data[,covariates], summary)
covariates

# subset the data for only necessary variables
var_list <- c(covariates, exposures, outcomes, "id","Education",'Ethnicity','Homelang','Children','Breadwinner','Age_3')
all_data <- all_data %>% 
  select(var_list)

# check again for missing data
#check for missing values in covariate dataset
na_count <-sapply(all_data, function(y) sum(is.na(y)))
na_count <- data.frame(na_count)
# view(na_count)

# delete observations with missing data
data_subset <- all_data[ , var_list]
all_data <- all_data[complete.cases(data_subset), ]
# all_data2 <- na.omit(all_data)

# save the final dataset
write.csv(all_data,"C:\\Users\\USER\\Desktop\\Backup\\Harvard\\1_Harvard School Works\\Research\\Vulnerability-variability Project\\southafrica\\southafrica data\\final_dataset_8_26_2020.csv")






