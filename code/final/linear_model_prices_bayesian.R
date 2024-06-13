# Info about the last time the script was run:

### R version 4.4.0 (2024-04-24)
### Platform: x86_64-pc-linux-gnu
### Running under: Ubuntu 20.04.6 LTS

### Matrix products: default
### BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
### LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/liblapack.so.3;  LAPACK version 3.9.0

### locale:
###  [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C               LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8     LC_MONETARY=en_GB.UTF-8   
### [6] LC_MESSAGES=en_GB.UTF-8    LC_PAPER=en_GB.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C            
### [11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       

### time zone: Europe/Helsinki
### tzcode source: system (glibc)

### attached base packages:
###  [1] stats     graphics  grDevices utils     datasets  methods   base     

### other attached packages:
###  [1] patchwork_1.2.0    data.table_1.15.4  xgboost_1.7.7.1    eccor_1.0.1        estcr_1.5.3        xtable_1.8-4       gridExtra_2.3     
###[8] rstan_2.32.6       StanHeaders_2.32.9 lubridate_1.9.3    forcats_1.0.0      stringr_1.5.1      dplyr_1.1.4        purrr_1.0.2       
### [15] readr_2.1.5        tidyr_1.3.1        tibble_3.2.1       tidyverse_2.0.0    gghsci_1.2.2       caret_6.0-94       lattice_0.22-5    
### [22] ggplot2_3.5.1     



#Set path and call functions and libraries
setwd("~/Git_root")

source("article_2024_print_culture_and_economic_constraints/code/final/functions.R")
library(ggplot2)
library(gghsci)
library(tidyverse)
library(rstan)
library(xtable)
library(gghsci)
library(gridExtra)
library(patchwork)
#Define the Bayesian regression model used in the article

bayesian_model_per_sheet_simple <- "
data {
  int n; // number of observations
  int d; // number pf columns
  matrix[n,d] X; // matrix of predictor variables
  vector[n] y; // price

}
parameters {
	
  real<lower=0> sigma; // variance parameter
  
  vector[d] beta; // coefficient prior 

  real<lower=0> alpha; // intercept
}
model{
  vector[n] mu;
  alpha ~ normal(2,1); 
  beta ~ double_exponential(0,0.1);  

  sigma ~ inv_gamma(1,1);
   mu = X * beta+alpha;
   	
 	y ~ student_t(2.5,mu,sigma);  
   
}                             
"
### Bring the model to a STAN model object

stan_model_linear <- stan_model(model_code=bayesian_model_per_sheet_simple)

# Data initialisation specific for the regression analysis

## Download the data sets used in the analysis

### Download a table of editions of the selected publishers, labeled to  pre (including 1774) and post 1774 editions

top_publishers_editions <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/top_publisher_editions.csv",stringsAsFactors = FALSE)

initial_price_data_set <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/price_analysis_initial_data.csv",stringsAsFactors = TRUE) %>% mutate(.,main_category=as.character(main_category))

## Create tables of variables specific to the regression analysis

### Take reprints of books and label them to pre (including 1774) and post 1774 literature
reprint_shift <- initial_price_data_set %>% filter(is_first_year_edition=="no") %>% mutate(.,reprint_premium=ifelse(publication_year<=1774,"pre_1774_reprint","post_1774_reprint")) %>% distinct(estc_id,reprint_premium)

### Define the ratio of data used to train and evaluate the models.
train_test_ratio <- 0.9

### Set a seed so that the result can be replicated.
set.seed(101)

## For the version of the analysis with variables from ECCO (now the only version):

### Bring all the variables used in the analysis to the same table, derive variables like potential luxury and five year interval from pre-existing variables, turn character variables to factors

initial_price_data_set_reformatted <- initial_price_data_set %>% left_join(top_publishers_editions) %>% mutate(.,top_publisher_pre_1774=as.factor(ifelse(!is.na(top_publisher_pre_1774),top_publisher_pre_1774,"other"))) %>% left_join(reprint_shift) %>% mutate(.,reprint_premium=as.factor(ifelse(!is.na(reprint_premium),reprint_premium,"other"))) %>% mutate(.,five_year_interval=as.factor(floor(publication_year/10)*10+floor((publication_year-floor(publication_year/10)*10)/5)*5)) %>% mutate(.,potential_luxury=as.factor(ifelse(pagecount_total>=128 & (gatherings=="2fo" | gatherings=="4to") & special_page_proportion>0,1,0))) %>% mutate(.,price_per_sheet_relative=relative_price/sheets) %>%  mutate(.,pamphlet_sized=ifelse(sheets<=4,1,0)) %>% mutate(.,book_sized=ifelse(sheets>=8 & sheets<24,1,0)) %>% mutate(.,main_category=ifelse(main_category=="literature" & publication_year<=1774,"literature_pre_1774",main_category)) %>% mutate(.,main_category=ifelse(main_category=="literature" & publication_year>1774,"literature_post_1774",main_category)) %>% mutate(.,long_book_sized=ifelse(sheets>=24,1,0)) %>% mutate(.,main_category=as.factor(main_category)) %>% distinct(estc_id,publication_place_category,pamphlet_sized,book_sized,long_book_sized,gatherings,potential_luxury,five_year_interval,special_page_proportion,reprint_premium,top_publisher_pre_1774,characters_per_sheet,main_category,price_per_sheet_relative) %>% filter(!is.na(price_per_sheet_relative) & !is.na(characters_per_sheet) & main_category!="unknown") 

### Manually set the 0-level for factors

initial_price_data_set_reformatted$gatherings <- relevel(initial_price_data_set_reformatted$gatherings,"8vo")
initial_price_data_set_reformatted$publication_place_category <- relevel(initial_price_data_set_reformatted$publication_place_category,"London")
initial_price_data_set_reformatted$reprint_premium <- relevel(initial_price_data_set_reformatted$reprint_premium,"other")
initial_price_data_set_reformatted$five_year_interval <- relevel(initial_price_data_set_reformatted$five_year_interval,"1770")
initial_price_data_set_reformatted$potential_luxury <- relevel(initial_price_data_set_reformatted$potential_luxury,"0")
initial_price_data_set_reformatted$top_publisher_pre_1774 <- relevel(initial_price_data_set_reformatted$top_publisher_pre_1774,"other")
initial_price_data_set_reformatted$main_category <- relevel(initial_price_data_set_reformatted$main_category,"politics")

### Split the data to train and test data 

initial_price_data_set_reformatted_fit <- sample_n(initial_price_data_set_reformatted,size=as.integer(train_test_ratio*nrow(initial_price_data_set_reformatted)))
initial_price_data_set_reformatted_test <- anti_join(initial_price_data_set_reformatted,initial_price_data_set_reformatted_fit)

### Reformat training data to matrix form that can be used with STAN

#remove the gatherings column of octavos which for some reason was not removed bt releveling
X <- initial_price_data_set_reformatted_fit %>% mutate(.,estc_id=NULL,price_per_sheet_relative=NULL,characters_per_sheet=scale(characters_per_sheet),special_page_proportion=scale(special_page_proportion)) %>% model.matrix(~ .-1,.) %>% as.data.frame(.) %>% .[,-(which(colSums(.)==0))] %>% as.matrix(.)
X <- X[,-1]

y <- initial_price_data_set_reformatted_fit$price_per_sheet_relative
n <- nrow(X)
d <- ncol(X)

### Map the parameters of the matrix form to parameter names

parameter_number_to_name <- colnames(X) %>% as.data.frame(.) %>% mutate(.,parameter_name=.) %>% mutate(.,parameter=paste0("beta","[",1:ncol(X),"]")) %>% distinct(parameter,parameter_name)

### Reformat test data to the same format and extract the right prices of test data to a vector

X_test <- initial_price_data_set_reformatted_test %>% mutate(.,estc_id=NULL,price_per_sheet_relative=NULL,characters_per_sheet=scale(characters_per_sheet),special_page_proportion=scale(special_page_proportion)) %>% model.matrix(~ .-1,.) %>% as.data.frame(.) %>% .[,colnames(X)] %>% as.matrix(.)
y_test <- initial_price_data_set_reformatted_test$price_per_sheet_relative

# Analysis starts from here

## Fit and analyse the model with variables from ECCO

### Model fitting
bm_mc <- stan(model_code=bayesian_model_per_sheet_simple,data=list(X=X,y=y,n=n,d=d),warmup=500,iter=2000,chains=4,seed=101)
bm_mc_parameters <- as.data.frame(bm_mc)
### Save the model
saveRDS(bm_mc,"article_2024_print_culture_and_economic_constraints/data/final/stan_model_linear.RDS")
bm_mc <- readRDS("article_2024_print_culture_and_economic_constraints/data/final/stan_model_linear.RDS")
bm_mc_parameters <- as.data.frame(bm_mc)

###Parameter extractions
beta <- bm_mc_parameters[grepl(x=colnames(bm_mc_parameters),pattern = "beta")] %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% as.matrix(.) %>% t(.)
alpha <- bm_mc_parameters[grepl(x=colnames(bm_mc_parameters),pattern = "alpha")]  %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% as.matrix(.) %>% as.vector(.)

### Model performance and residuals evaluated with test data
ms_explained <- 1-sum((y_test-(X_test %*% beta+alpha))^2)/sum((y_test-mean(y_test))^2)
mae_explained <- 1-sum(abs(y_test-(X_test %*% beta+alpha)))/sum(abs(y_test-median(y_test)))
residuals <- (y_test-(X_test %*% beta+alpha))
quickplot(residuals,geom="histogram")+xlim(-5,5)
quickplot(X_test %*% beta+alpha,residuals)+ylim(-3,3)
residuals_plot <- residuals %>% as.data.frame(.) %>% name_data_frame(.,"residual") %>% ggplot(aes(residual))+geom_histogram(binwidth = 0.1)+xlim(-2.5,2.5)+theme_hsci_continuous_grayscale(base_size=20)+xlab("Residual (Standardised Pence)")+ylab("Editions (N)")
residuals_plot

### Save The residual analysis plot (Fig. 6)
ggsave(filename = "article_2024_print_culture_and_economic_constraints/output/figures/final/fig_6.pdf", plot=residuals_plot)
embedFonts("article_2024_print_culture_and_economic_constraints/output/figures/final/fig_6.pdf")

### Bring the parameters and summary of their approximated posterior distributions to the same table
beta_parameter_table_2 <- distribution_summary_2(bm_mc_parameters,1:(ncol(bm_mc_parameters)-1))
beta_parameter_table_2  <- mutate(beta_parameter_table_2,parameter=rownames(beta_parameter_table_2))
beta_parameter_table_2 <- mutate(beta_parameter_table_2,parameter=rownames(beta_parameter_table_2)) %>% left_join(.,parameter_number_to_name) %>% mutate(.,parameter_name=ifelse(!is.na(parameter_name),parameter_name,parameter))
beta_parameter_table_2 %>% filter(sign(dist_2_5_interval)==sign(dist_97_5_interval)) %>% distinct(parameter_name,median,dist_2_5_interval,dist_97_5_interval) %>% .[,c("parameter_name","dist_2_5_interval","median","dist_97_5_interval")] %>% xtable(.)

## Subset the most remarkable effects for visualisation. Include effects that have an absolute effect size that rounds up to 0.15 on on both tails (and both tails have the same sign)
beta_parameter_table_2_remarkable_effects <- beta_parameter_table_2 %>% filter(sign(dist_2_5_interval)==sign(dist_97_5_interval) & abs(dist_2_5_interval)>=0.145 & abs(dist_97_5_interval)>=0.145) %>% distinct(parameter_name,median,dist_2_5_interval,dist_97_5_interval) %>% .[,c("parameter_name","dist_2_5_interval","median","dist_97_5_interval")] 

## Split the effects by their type (e.. related to place or physical attributes). 

place_effects <- beta_parameter_table_2_remarkable_effects[2:5,]
place_effects$parameter_name[1:4] <- c("Dublin","Edinburgh","Ireland-other","Scotland-other")

physical_effects <- beta_parameter_table_2_remarkable_effects[c(6:10,19),]
physical_effects$parameter_name[1:6] <- c("pamphlet","long book","12mo format","16mo format","2fo format","printing density")

temporal_effects <- beta_parameter_table_2_remarkable_effects[c(11:18),]
temporal_effects$parameter_name[] <- c("1700-1704","1705-1709","1710-1714","1715-1719","1730-1734","1760-1764","1795-1799","1800")

subject_topic_effects <- beta_parameter_table_2_remarkable_effects[c(20:23),]
subject_topic_effects$parameter_name[1:4] <- c("ephemera-entertainment","literature post-1774","literature pre-1774","religion")

## Create visualisations for different effect types (panels of Fig. 3).

place_plot <- ggplot(data=place_effects)+geom_pointrange(aes(x=parameter_name,ymin=dist_2_5_interval,y=median,ymax=dist_97_5_interval))+theme_hsci_continuous_grayscale(base_size = 20)+ylab("Per Sheet Effect (Pence)")+xlab("Location")+ylim(c(-0.85,0.85))+geom_hline(yintercept = 0,aes(linewidth=2),color="grey")+coord_flip()
place_plot

physical_plot <- ggplot(data=physical_effects)+geom_pointrange(aes(x=parameter_name,ymin=dist_2_5_interval,y=median,ymax=dist_97_5_interval))+theme_hsci_continuous_grayscale(base_size = 20)+ylab("Per Sheet Effect (Pence)")+xlab("Physical Attribute")+ylim(c(-0.85,0.85))+geom_hline(yintercept = 0,aes(linewidth=2),color="grey")+coord_flip()
physical_plot

temporal_plot <- ggplot(data=temporal_effects)+geom_pointrange(aes(x=parameter_name,ymin=dist_2_5_interval,y=median,ymax=dist_97_5_interval))+theme_hsci_continuous_grayscale(base_size = 20)+ylab("Per Sheet Effect (Pence)")+xlab("Period")+ylim(c(-0.85,0.85))+geom_hline(yintercept = 0,aes(linewidth=2),color="grey")+coord_flip()
temporal_plot

subject_plot <- ggplot(data=subject_topic_effects)+geom_pointrange(aes(x=parameter_name,ymin=dist_2_5_interval,y=median,ymax=dist_97_5_interval))+theme_hsci_continuous_grayscale(base_size = 20)+ylab("Per Sheet Effect (Pence)")+xlab("Subject Topic")+ylim(c(-0.85,0.85))+geom_hline(yintercept = 0,aes(linewidth=2),color="grey")+coord_flip()
subject_plot

## Create a grid table of visualisations

## complete_grid_plot <- grid.arrange(place_plot,physical_plot,temporal_plot,subject_plot,nrow=2)
complete_grid_plot <- place_plot+physical_plot+temporal_plot+subject_plot+plot_layout(2, 2)
complete_grid_plot

## Save the plot about the effects of different variables (Fig. 3).
ggsave(filename = "article_2024_print_culture_and_economic_constraints/output/figures/final/fig_3.pdf", plot=complete_grid_plot)
embedFonts("article_2024_print_culture_and_economic_constraints/output/figures/final/fig_3.pdf")



