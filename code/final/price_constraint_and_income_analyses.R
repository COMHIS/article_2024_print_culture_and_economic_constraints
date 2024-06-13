# Inforrmation about the last run of the script:

##R version 4.4.0 (2024-04-24)

##Platform: x86_64-pc-linux-gnu

##Running under: Ubuntu 20.04.6 LTS

## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/liblapack.so.3;  LAPACK version 3.9.0

##locale:
###  [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C               LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8     LC_MONETARY=en_GB.UTF-8   
###[6] LC_MESSAGES=en_GB.UTF-8    LC_PAPER=en_GB.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C            
###[11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       

##time zone: Europe/Helsinki
###tzcode source: system (glibc)

##attached base packages:
###  [1] stats     graphics  grDevices utils     datasets  methods   base     

##other attached packages:
###  [1] xtable_1.8-4       gridExtra_2.3      rstan_2.32.6       StanHeaders_2.32.9 lubridate_1.9.3    forcats_1.0.0      stringr_1.5.1     
###[8] dplyr_1.1.4        purrr_1.0.2        readr_2.1.5        tidyr_1.3.1        tibble_3.2.1       tidyverse_2.0.0    gghsci_1.2.2      
###[15] caret_6.0-94       lattice_0.22-5     ggplot2_3.5.1 


#Initialising the script
##Set the working directory
setwd("~/Git_root")
source("~/Git_root/article_2024_print_culture_and_economic_constraints/code/final/functions.R")
##Download required R packages
library(tidyverse)
library(gghsci)
library(rstan)
library(gridExtra)
library(xtable)

#Downloading, creating and preprocessing data

##Map the years of the social tables to the period for which the table of the year is used as a source of socio-economic data
period_to_year_mapping <- cbind.data.frame(period=c("early_18th","mid_18th","late_18th"),year=c(1688,1759,1798))

##Download the table of monetary conversations to correct for changing real value of the pound
monetary_conversions <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/pound_conversions.csv",stringsAsFactors = FALSE)

##Load the budget data used to derive the proportion of basic expenses as a function of the total expenses. Normalise households to 4.5 family members
early_modern_budgets <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/early_modern_budgets.csv",stringsAsFactors = FALSE) %>% mutate(.,income_pounds=(as.numeric(income_pounds)/as.numeric(n_household_without_servants))*4.5) %>% mutate(.,proportion_of_bacic_expenses=1-as.numeric(proportion_other)) %>% mutate(.,income_in_1688_pounds=as.numeric(income_pounds)*as.numeric(conversion_factor)) %>% mutate(.,income_in_1688_pounds_to_basic=proportion_of_bacic_expenses*income_in_1688_pounds) %>% mutate(.,income_in_1688_pounds_to_not_basic=as.numeric(proportion_other)*income_in_1688_pounds)

## Conversion between relative incomes by using the calculator from https://www.measuringworth.com. How much is one pound of 1688 in 1760 terms?
pound_1688_conversion <- 1
## 1688 to 1760: £1   3s 6d
pound_1759_conversion <- 240/(240+3*12+6)
##1688 to 1798: £2   7s   2d
pound_1798_conversion  <- 240/(240+14*12+2)

## Create the list of data needed by STAN to fit the proportional model
data_list = list(y=early_modern_budgets$income_in_1688_pounds_to_basic,x=log(early_modern_budgets$income_in_1688_pounds),n=nrow(early_modern_budgets),n_new=length(as.numeric(1:500)),x_new=log(as.numeric(1:500)))

# Fitting and evaluation of the proportional model

## Define the model
proportional_model <-"
data {
  int n; // number of observations
  int n_new; // number of predictions
  vector[n] x; // log income
  vector[n_new] x_new; // log income for predictions
  vector[n] y; // expenditure
  
  
}
parameters {
  
  real<lower=0> sigma; // variance parameter
  
  real beta; // coefficient 
  
  real alpha; // intercept
}
model{
  vector[n] mu;
  alpha ~ normal(0,1); 
  beta ~ inv_gamma(1,1);
  sigma ~ inv_gamma(1,1);
  mu = x * beta+alpha;
  y ~ lognormal(mu,sigma);
}  
generated quantities{
  vector[n_new] y_new; // predicted expenditure
for (i in 1:n_new){
    y_new[i] = lognormal_rng(x_new[i]*beta+alpha,sigma);
}
}  
"

## Fit the Bayesian proportional model of income and expenditure on basic expenses.  We call the difference
##between income and basic expenses disposable income (omitting savings, debts and taxes.)
fit_proportional_model <- stan(model_code=proportional_model,data=data_list,warmup=500,iter=2000,chains=4,seed=101)
###Extract the posterior sample and predictions
fit_proportional_model_posterior <- as.data.frame(fit_proportional_model) %>% select(sigma,beta,alpha) 
fit_proportional_model_predictions <- as.data.frame(fit_proportional_model)[,4:503]  

### Save the fitted price constraint model
saveRDS(fit_proportional_model,"article_2024_print_culture_and_economic_constraints/data/final/price_constraint_model.RDS")

### Summarise the posterior
posterior_summary_table <- fit_proportional_model_posterior %>% summarise(across(where(is.numeric), quantile,probs=c(0.025,0.5,0.975))) %>% as.matrix(.) %>% t(.) %>% as.data.frame(.) %>% name_data_frame(.,c("2_5_th_quantile","median","97_5_th_quantile"))
print.xtable(xtable(posterior_summary_table))

### Summarise the predictions
pred_interval <- fit_proportional_model_predictions  %>% summarise(across(where(is.numeric), quantile,probs=c(0.95,0.5,0.05))) %>% as.matrix(.) %>% t(.) %>% as.data.frame(.) %>% name_data_frame(.,c("pred_upper","pred_median","pred_lower")) %>% mutate(.,income=as.numeric(1:500),pred_lower=pred_lower,pred_median=pred_median,pred_upper=pred_upper)

##Save the median parameters of the model
income_to_expenditure_coefficients <-  c(posterior_summary_table$median[3],posterior_summary_table$median[2],posterior_summary_table$median[1])

##Create predictions using the median parameter estimates from the fited model. Calculate the proportion of daily income-basic expenses 
##print products of given price would have cost. The outputs of these calculations are used in table 1.
predictions_data <- cbind.data.frame(income_in_1688_pounds=seq(from=12,to=500,by=1)) %>% mutate(.,predictions=exp(log(income_in_1688_pounds)*income_to_expenditure_coefficients[2]+income_to_expenditure_coefficients[1])+1/2*income_to_expenditure_coefficients[3]^2) %>% mutate(.,cheap_threshold_prop_of_remaining_budget=exp(2.5)/(240*(income_in_1688_pounds-predictions)/365)) %>% mutate(.,expensive_threshold_prop_of_remaining_budget=exp(3.5)/(240*(income_in_1688_pounds-predictions)/365))

### Create table 2 of the article
predictions_data_chosen_income_levels <- predictions_data %>% filter(income_in_1688_pounds==30|income_in_1688_pounds==50|income_in_1688_pounds==200)
colnames(predictions_data_chosen_income_levels) <- c("Income","BE","C(Income,exp(2.5)","C(Income,exp(3.5))")
print.xtable(xtable(predictions_data_chosen_income_levels[,c("Income","C(Income,exp(2.5)","C(Income,exp(3.5))")]))

## Make a data frame containing both original observations and the predictions for the observations
early_modern_budgets_with_predictions <- mutate(early_modern_budgets,pred_other_expenses=get_disposable_income(income_in_1688_pounds,1688)) %>% mutate(.,cheap_threshold_prop_of_remaining_budget=exp(2.5)/(240*pred_other_expenses/365)) %>% mutate(.,expensive_threshold_prop_of_remaining_budget=exp(3.5)/(240*pred_other_expenses/365))

### compute R^2 with median parameters of the posterior
#+1/2*income_to_expenditure_coefficients[3]^2
pred_basic_expenses <- exp(log(early_modern_budgets_with_predictions$income_in_1688_pounds)*income_to_expenditure_coefficients[2]+income_to_expenditure_coefficients[1]+1/2*income_to_expenditure_coefficients[3]^2)
rsq_of_prop_model <- 1-sum((early_modern_budgets_with_predictions$income_in_1688_pounds*early_modern_budgets_with_predictions$proportion_of_bacic_expenses-pred_basic_expenses)^2)/sum((early_modern_budgets_with_predictions$income_in_1688_pounds*early_modern_budgets_with_predictions$proportion_of_bacic_expenses-mean(early_modern_budgets_with_predictions$income_in_1688_pounds*early_modern_budgets_with_predictions$proportion_of_bacic_expenses))^2)

## Fig 1 of the article (prop_plots joint) and the two panels produced separately.
prop_plot <- ggplot()+geom_point(data=early_modern_budgets_with_predictions,aes(x=income_in_1688_pounds,y=income_in_1688_pounds_to_basic),size=3)+geom_linerange(data=pred_interval,aes(x=income,y=pred_median,ymin=pred_lower,ymax=pred_upper),alpha=0.5)+geom_line(data=pred_interval,aes(x=income,y=pred_median))+xlab("Income/Expenditure")+ylab("Basic Expenditure")+theme_hsci_continuous_grayscale(base_size = 20)+xlim(0,300)+ylim(0,300)

prop_plot_reverse <- ggplot()+geom_point(data=early_modern_budgets_with_predictions,aes(x=income_in_1688_pounds,y=income_in_1688_pounds_to_not_basic),size=3)+geom_linerange(data=pred_interval,aes(x=income,y=income-pred_median,ymin=income-pred_lower,ymax=income-pred_upper),alpha=0.5)+geom_line(data=pred_interval,aes(x=income,y=income-pred_median))+xlab("Income/Expenditure")+ylab("Not Basic Expenditure")+theme_hsci_continuous_grayscale(base_size = 20)+xlim(0,300)+ylim(0,300)

# Old version of Fig 1: prop_plots_joint <- ggplot()+geom_point(data=early_modern_budgets_with_predictions,aes(x=income_in_1688_pounds,y=income_in_1688_pounds_to_basic),size=3)+geom_linerange(data=pred_interval,aes(x=income,y=pred_median,ymin=pred_lower,ymax=pred_upper),alpha=0.5)+geom_line(data=pred_interval,aes(x=income,y=pred_median))+xlab("Income/Expenditure (Pounds)")+ylab("Income/Expenditure (Pounds)")+theme_hsci_continuous_grayscale(base_size = 20)+xlim(0,300)+geom_point(shape=15,data=early_modern_budgets_with_predictions,aes(x=income_in_1688_pounds,y=income_in_1688_pounds_to_not_basic),size=3)+geom_linerange(linetype="dotted",data=pred_interval,aes(x=income,y=income-pred_median,ymin=income-pred_lower,ymax=income-pred_upper),alpha=0.5)+geom_line(linetyppe="dotted",data=pred_interval,aes(x=income,y=income-pred_median))+ylim(-50,300)
prop_plots_joint <- ggplot()+geom_point(data=early_modern_budgets_with_predictions,aes(x=income_in_1688_pounds,y=income_in_1688_pounds_to_basic),size=3)+geom_linerange(data=pred_interval,aes(x=income,y=pred_median,ymin=pred_lower,ymax=pred_upper),alpha=0.5,colour="#CCCCCC")+geom_line(data=pred_interval,aes(x=income,y=pred_median))+xlab("Income/Expenditure (Pounds)")+ylab("Money Spent (Pounds)")+theme_hsci_continuous_grayscale(base_size = 20)+xlim(0,300)+geom_point(shape=15,data=early_modern_budgets_with_predictions,aes(x=income_in_1688_pounds,y=income_in_1688_pounds_to_not_basic),size=3)+geom_linerange(data=pred_interval,aes(x=income,y=income-pred_median,ymin=income-pred_lower,ymax=income-pred_upper),alpha=0.5,colour="#333333")+geom_line(data=pred_interval,aes(x=income,y=income-pred_median))+ylim(-50,300)
prop_plots_joint

# Save Fig 1 as a pdf and embed fonts
ggsave(filename = "article_2024_print_culture_and_economic_constraints/output/figures/final/fig_1.pdf",prop_plots_joint)
embedFonts("article_2024_print_culture_and_economic_constraints/output/figures/final/fig_1.pdf")


# Create the income groups used in the article and analyse the price constraint

## Download table on social classes and define a table of the total number of families per year
allen_social_table <- read.csv("article_2024_print_culture_and_economic_constraints_post_revision/data/final/allen_2019_income_per_earner_table.csv",stringsAsFactors = FALSE)
households_per_year <- cbind.data.frame(c(1688,1759,1798),c(1751331,2000635,2951687),c(pound_1688_conversion,pound_1759_conversion,pound_1798_conversion)) %>% name_data_frame(.,c("year","n_families_total","correction_factor"))

#Create a table that combines the two wealthiest social classes, derive the disposable income for each year and class, count the price constraint for books of a given price
social_tables_chosen_quantiles <- allen_social_table %>% left_join(households_per_year) %>% mutate(.,n_families=proportion_of_families*n_families_total) %>% mutate(.,income=income_per_earner_pounds*earners) %>% mutate(.,income_in_1688_pounds=correction_factor*income) %>% mutate(class=ifelse(class=="landed"|class=="bourgeoisie","large_property_owners",class)) %>% group_by(class,year) %>% summarise(.,n_families=sum(proportion_of_families*n_families_total),income_in_1688_pounds=sum(proportion_of_families*income_in_1688_pounds)/(sum(proportion_of_families))) %>% mutate(.,disposable_income_in_1688_pounds=get_disposable_income(income_in_1688_pounds,1688)) %>% mutate(.,prop_of_cheap_item=exp(2.5)*365/(disposable_income_in_1688_pounds*240),prop_of_middle_item=exp(3.5)*365/(disposable_income_in_1688_pounds*240)) %>% filter(class=="large_property_owners"|class=="lower_middle_class"|class=="workers"|class=="farmers")


