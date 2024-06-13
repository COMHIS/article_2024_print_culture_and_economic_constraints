# Info about the last time the script was run:

### R version 4.4.0 (2024-04-24)
### Platform: x86_64-pc-linux-gnu
### Running under: Ubuntu 20.04.6 LTS

### Matrix products: default
### BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
### LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/liblapack.so.3;  LAPACK version 3.9.0

### locale:
### [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C               LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8     LC_MONETARY=en_GB.UTF-8   
### [6] LC_MESSAGES=en_GB.UTF-8    LC_PAPER=en_GB.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C            
### [11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       

### time zone: Europe/Helsinki
### tzcode source: system (glibc)

### attached base packages:
###  [1] stats     graphics  grDevices utils     datasets  methods   base     

### other attached packages:
###  [1] xgboost_1.7.7.1    eccor_1.0.1        estcr_1.5.3        xtable_1.8-4       gridExtra_2.3      rstan_2.32.6       StanHeaders_2.32.9
### [8] lubridate_1.9.3    forcats_1.0.0      stringr_1.5.1      dplyr_1.1.4        purrr_1.0.2        readr_2.1.5        tidyr_1.3.1       
### [15] tibble_3.2.1       tidyverse_2.0.0    gghsci_1.2.2       caret_6.0-94       lattice_0.22-5     ggplot2_3.5.1 


# Set the path and call relevant functions and libraries
setwd("~/Git_root")
source("article_2024_print_culture_and_economic_constraints/code/final/functions.R")

library(ggplot2)
library(gghsci)
library(caret)
library(tidyverse)
library(xgboost)
library(gridExtra)
## Download the the data set
initial_price_data_set <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/price_analysis_initial_data.csv",stringsAsFactors = TRUE)

## Create basic summary statistics about the scale and variation in the price per sheet

initial_price_data_set_mean_price <- mean(initial_price_data_set$relative_price_per_sheet,na.rm=TRUE)
initial_price_data_set_sd_price <- sd(initial_price_data_set$relative_price_per_sheet,na.rm=TRUE)
### We will exclude observations beyond certain range from the training data
### As their price determination is beyond our model capabilities. For example, some of the very cheapest items
### found from our data were subsidised (e.g. cheap repository) and the cost of some the very
### expensive items (e.g. short letters costing half a guinea) can not be deduced from metadata.
### Such cases could, however, cause problems of overfitting if the model tried to adjust its predictions to account for these instances
### and we deemed that the models behaviour would me more predictable if it did
outlier_price_per_sheet_high <- initial_price_data_set_mean_price*6
outlier_price_per_sheet_low <- initial_price_data_set_mean_price/6

## Set the train and test data ratio and fix the seed for replication purposes

train_test_ratio <- 0.8
set.seed(101)

## Create the test and train data sets. Test and train data only includes eighteenth century (plus year 1800) ESTC records.

### Create the test and train data sets for the model with variables from ECCO. THIS NOW COVERS ALL PREDICTIONS AS GENRE SCHEME COVERS ALL RECORDS

initial_price_data_with_ecco_variables <- initial_price_data_set %>% distinct(estc_id,gatherings,pagecount_total,area,publication_year,is_first_year_edition,special_page_proportion,publication_place_category,sheets,main_category,relative_price_per_sheet) %>% filter(!is.na(main_category))
initial_price_data_with_ecco_variables_train <- initial_price_data_with_ecco_variables %>% filter(.,!is.na(relative_price_per_sheet)) %>% sample_n(.,size=as.integer(train_test_ratio*nrow(.))) 
initial_price_data_with_ecco_variables_test <- initial_price_data_with_ecco_variables %>% filter(.,!is.na(relative_price_per_sheet)) %>% anti_join(initial_price_data_with_ecco_variables_train)
initial_price_data_with_ecco_variables_train <- initial_price_data_with_ecco_variables_train %>% filter(relative_price_per_sheet<=outlier_price_per_sheet_high & relative_price_per_sheet>=outlier_price_per_sheet_low)
### Create the test and train data sets for the model without variables from ECCO

# Model training and evaluation

## Define the set of hyperparameter values over which the performance of the Extreme Gradient Boosted Decision Tree models
## Is evaluated with cross validation before fitting the model with the best performing hyperparameters

xgb_grid = expand.grid(nrounds = c(100,500,1000),eta = c(0.1,0.01,0.001),max_depth=c(3,6,9),gamma=c(0.0001,0.001,0.1),colsample_bytree=c(0.25,0.5,1),subsample=c(0.5,0.75,1),min_child_weight=c(1,3,5))
xgb_trcontrol = trainControl(method = "cv",
                             number = 5,
                             verboseIter = TRUE,
                             returnData = FALSE,
                             returnResamp = "all", 
                             allowParallel = TRUE)

## Train and analyse the performance of the model with variables from ECCO

### Train the model
xgb_train = train(x = initial_price_data_with_ecco_variables_train %>% select(-relative_price_per_sheet,-estc_id) %>% model.matrix(~ .-1,.),
                    y = initial_price_data_with_ecco_variables_train$relative_price_per_sheet,
                    trControl = xgb_trcontrol,
                    tuneGrid = xgb_grid,
                    method = "xgbTree")
saveRDS(xgb_train,"article_2024_print_culture_and_economic_constraints/data/final/xgb_price_predictor_with_ECCO_variables.RDS")
xgb_train <- readRDS("article_2024_print_culture_and_economic_constraints/data/final/xgb_price_predictor_with_ECCO_variables.RDS")

##Feature importance
xgb_feature_ipmportance <- xgb.importance( model = xgb_train$finalModel)
xgb_feature_ipmportance
## How much does the top 6 of physical attributes explain of all of the gains?
sum(xgb_feature_ipmportance$Gain[1:6])/sum(xgb_feature_ipmportance$Gain)
## Print the answer
sum(xgb_feature_ipmportance$Gain[1:14])/sum(xgb_feature_ipmportance$Gain)

## How about everything not related to topics?
1-sum(xgb_feature_ipmportance$Gain[grep(xgb_feature_ipmportance$Feature,pattern="sub")])/sum(xgb_feature_ipmportance$Gain)
### Make predictions the test data both at per sheet and edition level (price per sheet X number of sheets), and evaluate performance
pred <- predict(xgb_train,initial_price_data_with_ecco_variables_test %>% select(-relative_price_per_sheet,-estc_id) %>% model.matrix(~ .-1,.))
pred[pred<0.4] <- 0.4
pred_total_price <- pred*initial_price_data_with_ecco_variables_test$sheets
test_total_price <- initial_price_data_with_ecco_variables_test$sheets*initial_price_data_with_ecco_variables_test$relative_price_per_sheet
mse_total_price <- 1- mean((pred_total_price-test_total_price)^2)/mean((test_total_price-mean(test_total_price))^2)
mae_total_price <- 1- mean(abs(pred_total_price-test_total_price))/mean(abs(test_total_price-mean(test_total_price)))
mse <- 1-mean((pred-initial_price_data_with_ecco_variables_test$relative_price_per_sheet)^2)/mean((initial_price_data_with_ecco_variables_test$relative_price_per_sheet-mean(initial_price_data_with_ecco_variables_test$relative_price_per_sheet))^2)
mae <- 1-mean(abs(pred-initial_price_data_with_ecco_variables_test$relative_price_per_sheet))/mean(abs(initial_price_data_with_ecco_variables_test$relative_price_per_sheet-median(initial_price_data_with_ecco_variables_test$relative_price_per_sheet)))
log_mse <- 1-mean((log(pred_total_price)-log(test_total_price))^2)/mean((log(test_total_price)-mean(log(test_total_price)))^2)

### Visualise accuracy of predictions (Fig 2.)
quickplot(pred_total_price,pred_total_price-test_total_price)+xlim(0,500)+ylim(-250,250)
quickplot(log(pred_total_price),log(test_total_price))
pred_plot_with_ecco <- cbind.data.frame(pred_total_price,test_total_price) %>% name_data_frame(.,c("predicted_price","price")) %>% ggplot(data=.)+geom_point(aes(x=predicted_price,y=price))+theme_hsci_continuous_grayscale(base_size = 25)+xlab("Predicted Price (Pence)")+ylab("Price (Pence)")+scale_x_log10() + scale_y_log10()+geom_abline(a=0,b=1,linewidth=1,color="grey")

pred_plot_with_ecco

### Save visualisations of the predicted prices vs. known test prices.
ggsave(filename = "article_2024_print_culture_and_economic_constraints/output/figures/final/fig_2.pdf", plot=pred_plot_with_ecco)
embedFonts("article_2024_print_culture_and_economic_constraints/output/figures/final/fig_2.pdf")


### Ordinal scale classification of predicted prices for the test data
price_classified_predicted_ordinal <- cut(log(pred_total_price),breaks = c(-5,2.5,3.5,10),labels = c("cheap","middle","expensive"))
price_classified_test_ordinal <-  cut(log(test_total_price),breaks = c(-5,2.5,3.5,10),labels = c("cheap","middle","expensive"))

### Confusion matrix of prices classified on ordinal scale for the test data
confusionMatrix(price_classified_predicted_ordinal,price_classified_test_ordinal)

### Save the confusion matrix into a vector
confusion_matrix_1_vec <- confusionMatrix(price_classified_predicted_ordinal,price_classified_test_ordinal)$table %>% as.vector(.)
saveRDS(confusion_matrix_1_vec,"article_2024_print_culture_and_economic_constraints/data/final/confusion_matrix_1_vector.RDS")

#Predictions for the ESTC records without price information

## Make predictions for prices for the ESTC records with the ECCO variables
### Give each record a row number to be able to map the predictions - which the model does not generate for a few observations
### to the estc record id's after the predictions have been made. The data also needs to be formatted into a matrix.
initial_price_data_with_ecco_variables_with_row_number <- initial_price_data_with_ecco_variables  %>% mutate(.,row_number=seq(from=1,to=nrow(.),by=1))  
initial_price_data_with_ecco_variables_with_row_number_model_matrix_format <- initial_price_data_with_ecco_variables_with_row_number %>% select(-relative_price_per_sheet,-estc_id) %>% model.matrix(~ .-1,.)
predicted_price_with_ecco_variables <- predict(xgb_train,initial_price_data_with_ecco_variables_with_row_number_model_matrix_format[,-ncol(initial_price_data_with_ecco_variables_with_row_number_model_matrix_format)]) %>% mutate(as.data.frame(initial_price_data_with_ecco_variables_with_row_number_model_matrix_format),relative_price_per_sheet=.) %>% distinct(row_number,relative_price_per_sheet) %>% left_join(initial_price_data_with_ecco_variables_with_row_number %>% select(estc_id,row_number,sheets),.) %>% mutate(.,relative_price_per_sheet=ifelse(relative_price_per_sheet>=0.4,relative_price_per_sheet,0.4)) %>% distinct(estc_id,relative_price_per_sheet,sheets) %>% mutate(.,relative_price=sheets*relative_price_per_sheet) %>% mutate(price_class=cut(log(relative_price),breaks = c(-5,2.5,3.5,10),labels = c("cheap","middle","expensive")))

## Compile the final data set of prices with predictions included

### Take the original price data sepratately, as we prefer this information to the predictions
original_price_data <-  initial_price_data_set %>% filter(!is.na(relative_price)) %>% distinct(estc_id,sheets,relative_price_per_sheet,relative_price) %>% mutate(price_class=cut(log(relative_price),breaks = c(-5,2.5,3.5,10),labels = c("cheap","middle","expensive")))


### Combine all predictions and original price data, with the order of preference being to use the original data if available,
### Predictions utilising the ESTC variables as the second option, and predictions without the ECCO variables as the third option.
full_price_data_compiled <- original_price_data %>% full_join(anti_join(predicted_price_with_ecco_variables,.,by="estc_id"))

### Save the predictions for downstream analyses
write.csv(full_price_data_compiled,"article_2024_print_culture_and_economic_constraints/data/final/predicted_prices.csv")

