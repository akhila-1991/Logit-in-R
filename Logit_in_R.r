#connecting to hermes to read the data

require(RODBC)
install.packages("data.table")
library(data.table)
conn <- odbcConnect("CarmelODBCDSII" , believeNRows = FALSE , rows_at_time = 1 )
sqlQuery(conn,"use p_dxiaoyun")
res <- sqlQuery(conn, "show tables")
names(res)
nrow(res)
data_base <- sqlQuery(conn, "select * from watches_txns_2019_v14")

#loading required packages

install.packages("dplyr")
library(dplyr)       

install.packages("broom")
library(broom)  

install.packages("visreg")   
library(visreg)      

install.packages("margins")
library(margins)    

install.packages("rcompanion")
library(rcompanion) 

install.packages("ROCR")
library(ROCR)    


#Exploratory Data Analysis

head(data_base)    

#Dropping not helpful columns

drop_columns <- c("auct_start_dt","brand","model","list_1st_dt","fa_lstg_auct_start_dt","sell_1st_dt","ftl_ind","lpsd_lstr_ind","cy_lstd_ind","transaction_id","buyer_id","created_dt","price_tranche","gmv","tot_txns","over_10k_txns","seller_status","css_segment","item_id","seller_id","AUCT_TITL","auct_over_10k_perc","quantity","mq_yn_ind","auct_type")

data_base <- data_base[ , !(names(data_base) %in% drop_columns)]

data_base$sps_slr_level_cd <- NULL 
data_base$listing_month <- NULL 
data_base$cndtn_rollup_id <- NULL 
data_base$return_window <- NULL 
data_base$fnf_flag <- NULL 

glimpse(data_base)

nrow(data_base)

data_base <- na.omit(data_base) #removing NA values

nrow(data_base)

#converting categorical variables into factors 


data_base$best_offer_ind <- as.factor(data_base$best_offer_ind)
data_base$free_shpng_yn_ind <- as.factor(data_base$free_shpng_yn_ind)
data_base$paypal_acptd_ind <- as.factor(data_base$paypal_acptd_ind)
data_base$listed_with_autopay_ind <- as.factor(data_base$listed_with_autopay_ind)
data_base$ebay_ctlg_cvrd_ind <- as.factor(data_base$ebay_ctlg_cvrd_ind)
data_base$cust_sgmntn_desc <- as.factor(data_base$cust_sgmntn_desc)
data_base$b2c_c2c <- as.factor(data_base$b2c_c2c)
data_base$store_owner_flag <- as.factor(data_base$store_owner_flag)
data_base$retruns_accpeted_ind <- as.factor(data_base$retruns_accpeted_ind) 
data_base$return_shipping <- as.factor(data_base$return_shipping)
data_base$item_condition <- as.factor(data_base$item_condition)
data_base$first_listed_item <- as.factor(data_base$first_listed_item) 
data_base$listed_current_year <- as.factor(data_base$listed_current_year)
data_base$brand_mod <- as.factor(data_base$brand_mod)
data_base$model_mod <- as.factor(data_base$model_mod)

data_base$Dependent_Var <- as.factor(data_base$Dependent_Var)

sum(is.na.data.frame(data_base))

#checking relationship

> xtabs(~ Dependent_Var + listing_month, data=data_base)
             listing_month
Dependent_Var     1     2     3     4     5     6     7     8     9    10    11    12
            0 66033 67677 84438 63037 59329 73072 57130 56178 70335 57361 61273 75173
            1    92    97   120   123   125   124    75    84   116    89    92   108
> xtabs(~ Dependent_Var + best_offer_ind, data=data_base)
             best_offer_ind
Dependent_Var      0      1
            0 619417 171619
            1    767    478
> xtabs(~ Dependent_Var + fnf_flag, data=data_base)
             fnf_flag
Dependent_Var      0      1
            0 735917  55119
            1   1139    106
> xtabs(~ Dependent_Var + free_shpng_yn_ind, data=data_base)
             free_shpng_yn_ind
Dependent_Var      0      1
            0 656274 134762
            1    705    540
> xtabs(~ Dependent_Var + paypal_acptd_ind, data=data_base)
             paypal_acptd_ind
Dependent_Var      0      1
            0  16058 774978
            1     20   1225
> xtabs(~ Dependent_Var + listed_with_autopay_ind, data=data_base)
             listed_with_autopay_ind
Dependent_Var      0      1
            0 733638  57398
            1   1203     42
> xtabs(~ Dependent_Var + ebay_ctlg_cvrd_ind, data=data_base)
             ebay_ctlg_cvrd_ind
Dependent_Var      0      1
            0 617399 173637
            1    806    439
> xtabs(~ Dependent_Var + cust_sgmntn_desc, data=data_base)
             cust_sgmntn_desc
Dependent_Var Entrepreneur Large Merchant Merchant Occasional Regular un_identified
            0       212331          59012   168295     131338  181410         38650
            1           48            306      133        401     125           232
> xtabs(~ Dependent_Var + b2c_c2c, data=data_base)
             b2c_c2c
Dependent_Var    B2C    C2C
            0 439638 351398
            1    487    758
> xtabs(~ Dependent_Var + sps_slr_level_cd, data=data_base)
             sps_slr_level_cd
Dependent_Var      1      2      3      4  99999
            0 326220 413843      2  15891  35080
            1    232    798      0     17    198
> xtabs(~ Dependent_Var + store_owner_flag, data=data_base)
             store_owner_flag
Dependent_Var      0      1
            0 455614 335422
            1    692    553
> xtabs(~ Dependent_Var + return_window, data=data_base)
             return_window
Dependent_Var 14 days 30 days 60 days no returns
            0   60279  299312    4841     426604
            1     124     314       2        805
> xtabs(~ Dependent_Var + retruns_accpeted_ind, data=data_base)
             retruns_accpeted_ind
Dependent_Var      n      y
            0 426604 364432
            1    805    440
> xtabs(~ Dependent_Var + return_shipping, data=data_base)
             return_shipping
Dependent_Var no returns others
            0     426604 364432
            1        805    440
> xtabs(~ Dependent_Var + item_condition, data=data_base)
             item_condition
Dependent_Var manudacture_refurbished new_with_defects new_with_tags new_without_tags non_specified
            0                    5872             7121        100842            55737          1837
            1                       3                0           233               88            12
             item_condition
Dependent_Var pre-owned seller_refurbished
            0    614994               4633
            1       903                  6
> xtabs(~ Dependent_Var + first_listed_item, data=data_base)
             first_listed_item
Dependent_Var      0      1
            0 783037   7999
            1   1177     68
> xtabs(~ Dependent_Var + listed_current_year, data=data_base)
             listed_current_year
Dependent_Var      0      1
            0 741452  49584
            1   1076    169
> xtabs(~ Dependent_Var + brand_mod, data=data_base)
             brand_mod
Dependent_Var Audemars Piguet Breitling Cartier Maitres du Temps  OMEGA others patek philippe  rolex
            0             454      4036    3881                0  12644 722516              3     19
            1              75        23      29                7      9   1098              0      1
             brand_mod
Dependent_Var  Seiko TAG Heuer UNknown
            0  37748      9734       1
            1      0         3       0
> xtabs(~ Dependent_Var + model_mod, data=data_base)
             model_mod
Dependent_Var Casio G-Shock Omega Seamaster others Rolex Datejust Rolex Day-Date Rolex Daytona
            0          7870            1818 775239           1581            187            96
            1             0               1   1049             21             36            58
             model_mod
Dependent_Var Rolex GMT-Master II Rolex Oyster Perpetual Rolex President TAG Heuer Aquaracer
            0                  77                    875             169                1405
            1                  64                     10               6                   0
             model_mod
Dependent_Var TAG Heuer Formula 1
            0                1719
            1                   0
			
#deleting sps_slr_level_cd column since its similar to seller segment and also does not have enough representation in data
data_base$sps_slr_level_cd <- NULL

#checking the structure one last time


# Train and Test Split

# Total number of rows in the data_base data frame
n <- nrow(data_base)
# Number of rows for the training set (80% of the dataset)
n_train <- round(0.80 * n)
# Create a vector of indices which is an 80% random sample
set.seed(123)
train_indices <- sample(1:n, n_train)
# Subset the data_base data frame to training indices only
train <- data_base[train_indices, ]
# Exclude the training indices to create the test set
test <- data_base[-train_indices, ]

paste("train sample size: ", dim(train)[1])
paste("test sample size: ", dim(test)[1])

#Fitting a binary logistic regression
model_logi <- glm(Dependent_Var~., data = train, family = "binomial")
#Model summary
summary(model_logi)

# Pseudo R_squared values and Likelyhood ratio test
nagelkerke(model_logi)

#obtainting log odds of the model
(exp(coef(model_logi)))

tidy(model_logi, exponentiate = TRUE, conf.level = 0.95) #odds ratio

# Calculate average marginal effect
effects_logit_dia = margins(model_logi)
# Summary of marginal effect
summary(effects_logit_dia)


# predict the test dataset
pred <- predict(model_logi, test, type="response") 
predicted <- round(pred) # round of the value; >0.5 will convert to 
                         # 1 else 0
# Creating a contigency table
tab <- table(Predicted = predicted, Reference = test$Dependent_Var)
tab

# Creating a dataframe of observed and predicted data
act_pred <- data.frame(observed = test$Dependent_Var, predicted = 
                      factor(predicted))
# Calculating Accuracy
accuracy_est <- accuracy(act_pred, observed, predicted)
print(accuracy_est)


pred.rocr <- prediction(pred, test$Dependent_Var)
eval <- performance(pred.rocr, "acc")
plot(eval)


# Identifying the best cutoff value that maximizes accuracy
max <- which.max(slot(eval, "y.values")[[1]])
acc <- slot(eval, "y.values")[[1]][max] #y.values are accuracy 
                                        #measures
cut <- slot(eval, "x.values")[[1]][max] #x.values are cutoff 
                                        #measures
print(c(Accuracy = acc, Cutoff = cut))



library(yardstick)
# Creating a actual/observed vs predicted dataframe
act_pred <- data.frame(observed = test$Dependent_Var, predicted =  
                       factor(predicted))
# Calculating precision, recall and F1_score
prec <- precision(act_pred, observed, predicted)
rec <- recall(act_pred, observed, predicted)
F1_score <- f_meas(act_pred, observed, predicted) #called f_measure
print(prec)
print(rec)
print(F1_score)



perf_rocr <- performance(pred.rocr, measure = "auc",
                         x.measure = "cutoff")
perf_rocr@y.values[[1]] <- round(perf_rocr@y.values[[1]], digits = 
                                 4)
perf.tpr.fpr.rocr <- performance(pred.rocr, "tpr", "fpr")
plot(perf.tpr.fpr.rocr, colorize=T, 
     main = paste("AUC:", (perf_rocr@y.values)))
	 
abline(a = 0, b = 1)