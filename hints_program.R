library(haven)
my_data <- read_dta("C:/Users/arjun/Desktop/UIC/Summer Semester/Health care/Assignment 2/hints5_cycle2_public.dta")

manually_selected = c("Education",
"Age",
"Race_Cat2", 
"IncomeRanges",
"FreqGoProvider",
"Electronic_TestResults",
"Electronic_TalkDoctor",
"ConfidentInfoSafe",
"EverOfferedAccessRec",
"AccessOnlineRecord",
"UseInternet",
"ProviderMaintainEMR2",
"OtherDevTrackHealth",
"Tablet_DiscussionsHCP",
"Tablet_MakeDecision",
"Tablet_AchieveGoal",
"BMI",
"medconditions_Arthritis",
"medconditions_LungDisease",
"medconditions_HeartCondition",
"medconditions_Diabetes",
"medconditions_highBP",
"HaveDevice_Tablet",
"HaveDevice_SmartPh",
"HaveDevice_CellPh",
"HaveDevice_None")

manually_selected = tolower(manually_selected)

manually_selected_df = my_data[,c(manually_selected)]

factors <- c("race_cat2",
            "incomeranges",
            "freqgoprovider",
            "electronic_testresults",
            "electronic_talkdoctor",
            "everofferedaccessrec",
            "accessonlinerecord",
            "useinternet",
            "providermaintainemr2",
            "otherdevtrackhealth",
            "tablet_discussionshcp",
            "tablet_makedecision",
            "tablet_achievegoal",
            "medconditions_arthritis",
            "medconditions_lungdisease",
            "medconditions_heartcondition",
            "medconditions_diabetes",
            "medconditions_highbp",
            "havedevice_tablet",
            "havedevice_smartph",
            "havedevice_cellph",
            "havedevice_none"
)

ordinal <- c("education", "confidentinfosafe")

numeric <- c("age", "bmi")

numeric_df <- manually_selected_df[,numeric]
factor_df  <- manually_selected_df[,factors] 
ordinal_df <- manually_selected_df[,ordinal]


combined_df <- cbind(numeric_df, factor_df, ordinal_df)

combined_df[combined_df == -1] <- NA
combined_df[combined_df == -2] <- NA
combined_df[combined_df == -5] <- NA
combined_df[combined_df == -6] <- NA
combined_df[combined_df == -9] <- NA

combined_df <- na.omit(combined_df)

df_numeric <- combined_df[,numeric]
df_factor  <- combined_df[,factors]
df_ordinal <- combined_df[,ordinal]

#Converting to Numeric Variables
for (i in 1:ncol(df_numeric)){
  df_numeric[,i] <- as.numeric(df_numeric[,i])  
}

#Converting to Factors
for (i in 1:ncol(df_factor)){
  df_factor[,i] <- as.factor(df_factor[,i])  
}

#Converting to Factors
for (i in 1:ncol(df_ordinal)){
  df_ordinal[,i] <- as.factor(df_ordinal[,i])  
}

#Converting the ordinal variables to Ordinal type
df_ordinal[[1]] <- factor(df_ordinal[[1]], ordered = TRUE, levels = c(1:7))
df_ordinal[[2]] <- factor(df_ordinal[[2]], ordered = TRUE, levels = c(1:3))


#Pearson's Correlation Test
#===========================
cor(df_numeric)
#ANALYSIS:
#age and bmi are not correlated to each other


#Checking for variables which has got only 1 level
#These are the SCREENING variables
scr_var = c()
n = 1
for (i in 1:ncol(df_factor)){
  l = levels(df_factor[,i])
  len = length(l)
  if (len < 2){
    print(len)
    print(colnames(df_factor[i]))
    scr_var[[n]] <- colnames(df_factor[i])  
    n <- n + 1
  }
}

names(df_factor)

#Remove the Screening Variables
df_factor <- df_factor[ , !(names(df_factor) %in% scr_var)]

#Chi-Square Test
#================
#Checking the relation

target_no <- which(colnames(df_factor) == "accessonlinerecord")
pval_thr = 0.05

sig_factors <- c()
n <- 1
for (i in 1:ncol(df_factor)){
  chi <- chisq.test(df_factor[,target_no],df_factor[,i], correct = TRUE)
  if (chi[3] < pval_thr){
    print(colnames(df_factor[i]))
    print("P Value is - ")
    print(chi[3])
    sig_factors[n] <- colnames(df_factor[i])
    n <- n + 1
  }    
}

#t-Test
#=======

t.test(df_numeric$age, df_numeric$bmi)
#Since p-value is less than 0.05 thus both variables are INDEPENDENT


combined_df1 <- cbind(df_numeric, df_factor, df_ordinal)
target_no    <- which(colnames(df_factor) == "accessonlinerecord")

combined_df1 <- combined_df1[,-target_no]

log = glm(df_factor$accessonlinerecord ~ .,data = combined_df1, family = binomial)

summary(log)

View(df_factor)  
  
log = glm(df_factor$accessonlinerecord ~ .,data = combined_df1, family = binomial)  
summary(log)  
tabulate(df_factor$race_cat2,labels())
test<-combined_df1[1,1:22]

ncol(combined_df1)
test_pred <- predict(log,test, type="response")
test_pred

combined_df1 <- within(combined_df1, race_cat2 <- relevel(race_cat2, ref = '31'))
table(combined_df1$race_cat2)
    
View(df_factor)

combined_df1 <- within(combined_df1, race_cat2 <- relevel(race_cat2, ref = '31'))

electronic_testresult

d = df_factor$electronic_testresults

dl <- within(d, electronic_testresults <- relevel(electronic_testresults, ref = '2'))

levels(df_factor$race_cat2)


#1
df_factor <- within(df_factor, electronic_testresults <- relevel(electronic_testresults, ref = '1'))
l1 = glm(df_factor$accessonlinerecord ~ df_factor$electronic_testresults, data = combined_df1, family = binomial)  

summary(l1)

test_pred1 <- predict(l1,test, type="response")
test_pred1[1]

#2
df_factor <- within(df_factor, electronic_testresults <- relevel(electronic_testresults, ref = '2'))
l2 = glm(df_factor$accessonlinerecord ~ df_factor$electronic_testresults, data = combined_df1, family = binomial)  

summary(l2)

test_pred2 <- predict(l2,test, type="response")
test_pred2[5]


table(df_factor$electronic_testresults)
