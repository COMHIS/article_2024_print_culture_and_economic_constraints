# Info about the last time the script was run from sessionInfo() function:

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



# Call relevant packages and set the path. estcr and eccor not needed in the version found from this repo.
setwd("~/Git_root")
library(tidyverse)
library(tidytext)
#library(eccor)
#library(estcr)
library(data.table)
library(xtable)

#Convert certain combinations of main and type category to other values for main and sub _category
update_categories <- function(data,type_category,main_category,main_category_new=NA,sub_category_new=NA){
  right_index <- data$type_category==type_category & data$main_category==main_category
  data$main_category[right_index] <- main_category_new
  data$sub_category[right_index] <- sub_category_new
  return(data)
  
}
# More detailed data updating function (e.g. convert one label to another)
update_categories_comprehensive <- function(data,type_category,main_category,sub_category,main_category_new=NA,sub_category_new=NA){
  right_index <- data$type_category==type_category & data$main_category==main_category & data$sub_category==sub_category
  data$main_category[right_index] <- main_category_new
  data$sub_category[right_index] <- sub_category_new
  return(data)
  
}

## Download full label data and analyse the coverage

type_labels <-  read.csv("article_2024_print_culture_and_economic_constraints/data/final/genre_labels.csv",stringsAsFactors = FALSE,sep=",",header = TRUE) %>% count(main_category)  
### the estc_n originally from the calling the estcr-package and counting the number of rows. Here we used a fixed number that is the result of that instead.
### estc_n_total <- load_estc_core() %>% nrow()
estc_n_total <- 483925
n_missing_type_label <- estc_n_total-sum(type_labels$n)
n_with_type_label <- sum(type_labels$n)

## Download a set of estc records without a topic/genre/type etc. label, that were manually annotated.

without_labels_sample <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/estc_without_topic_harmonised.csv",stringsAsFactors = FALSE,sep=",",header = TRUE) %>% mutate(.,sub_category="") 
### We change the names of two columns. type_physical to type category and main_category_iiro to main_category. This is done, because
### the function we use to convert some of the labels to the final label scheme takes type_category and main_category columns as expected input
### As we do the same conversion to labels of another annotator (Kira), the name of the 5th column will be changed later to main_category
### and 4th back to main_category_iiro.
colnames(without_labels_sample)[3] <- "type_category"
colnames(without_labels_sample)[4] <- "main_category"

## Convert some of the labels of the evaluation phase into more aggregated categories. If anything, this might mean that we report slightly worse
## results than what is the reality under these labels, as the evaluation phase labels were more granular

without_labels_sample <- without_labels_sample %>% mutate(.,type_category=ifelse(is.na(type_category),"unknown",type_category))
without_labels_sample <- update_categories(without_labels_sample,"book","agriculture and husbandry","science","agriculture and husbandry")
without_labels_sample <- update_categories(without_labels_sample,"in-between","agriculture and husbandry","science","agriculture and husbandry")
without_labels_sample <- update_categories(without_labels_sample,"ephemera","agriculture and husbandry","practical",NA)
without_labels_sample <- update_categories(without_labels_sample,"ephemera","news","politics",NA)
without_labels_sample <- update_categories(without_labels_sample,"unknown","periodical","literature","periodical")
without_labels_sample <- update_categories_comprehensive(without_labels_sample,"ephemera","literature","poetry","entertainment","oral")
without_labels_sample <- update_categories(without_labels_sample,"ephemera","literature","entertainment",NA)
without_labels_sample <- update_categories(without_labels_sample,"ephemera","educational","practical",NA)
without_labels_sample <- update_categories(without_labels_sample,"unknown","agriculture and husbandry","science","agriculture and husbandry")
without_labels_sample <- update_categories(without_labels_sample,"ephemera","oral","entertainment","oral")
without_labels_sample <- update_categories(without_labels_sample,"ephemera","science","practical",NA)
without_labels_sample <- update_categories(without_labels_sample,"unknown","oral","entertainment","oral")
without_labels_sample <- update_categories(without_labels_sample,"book","educational","education",NA)


### Change names of columns, as noe we start converting labels from Kira to accord with the rules of the final labeling scheme, hence
### the columns of her labels needs to be called main_category (former main_category column is now main_category_iiro again.)
colnames(without_labels_sample)[4] <- "main_category_iiro"
colnames(without_labels_sample)[5] <- "main_category"

## Implement conversion to final labelling scheme to the records labelled by Kira

without_labels_sample <- without_labels_sample %>% mutate(.,type_category=ifelse(is.na(type_category),"unknown",type_category)) %>% mutate(.,main_category=ifelse(main_category=="political","politics",main_category)) %>% mutate(.,main_category=ifelse(main_category=="news","politics",main_category))
without_labels_sample <- update_categories(without_labels_sample,"book","agriculture and husbandry","science","agriculture and husbandry")
without_labels_sample <- update_categories(without_labels_sample,"in-between","agriculture and husbandry","science","agriculture and husbandry")
without_labels_sample <- update_categories(without_labels_sample,"ephemera","agriculture and husbandry","practical",NA)
without_labels_sample <- update_categories(without_labels_sample,"ephemera","news","politics",NA)
without_labels_sample <- update_categories(without_labels_sample,"unknown","periodical","literature","periodical")
without_labels_sample <- update_categories_comprehensive(without_labels_sample,"ephemera","literature","poetry","entertainment","oral")
without_labels_sample <- update_categories(without_labels_sample,"ephemera","literature","entertainment",NA)
without_labels_sample <- update_categories(without_labels_sample,"ephemera","educational","practical",NA)
without_labels_sample <- update_categories(without_labels_sample,"unknown","agriculture and husbandry","science","agriculture and husbandry")
without_labels_sample <- update_categories(without_labels_sample,"ephemera","oral","entertainment","oral")
without_labels_sample <- update_categories(without_labels_sample,"ephemera","science","practical",NA)
without_labels_sample <- update_categories(without_labels_sample,"unknown","oral","entertainment","oral")
without_labels_sample <- update_categories(without_labels_sample,"book","educational","education",NA)

### Change the column that stores the labels given by Kira back to main_category_Kira.
colnames(without_labels_sample)[5] <- "main_category_kira"

### Summarise how the physical size of editions is distributed among the sample of records without a label from the labelling scheme
### (but labelled by Kira and Iiro).
without_labels_sample_by_physical_size <- without_labels_sample %>% count(type_category,name="n_type_category") %>% mutate(.,proportion_type_category=n_type_category/sum(n_type_category))

## A summary table of how many records in the sample without label from the labelling scheme the two annotators labelled with different labels
without_labels_sample_by_category_iiro <- without_labels_sample %>% count(main_category_iiro,name="n_type_category_missing_iiro")
colnames(without_labels_sample_by_category_iiro)[1] <- "main_category"
without_labels_sample_by_category_kira <- without_labels_sample %>% count(main_category_kira,name="n_type_category_missing_kira")
colnames(without_labels_sample_by_category_kira)[1] <- "main_category"

## Averaged estimates over the two annotators of how many records with missing labels there were, and point estimates of how many such
## records there are among those records with missing labels in the ESTC as a whole (e.g. given the number of records without label and the
## number of records with label history in the sample, how many records belonging to history we expect there to be among the unclassified).
without_labels_sample_by_category <- left_join(without_labels_sample_by_category_iiro,without_labels_sample_by_category_kira) %>% mutate(.,n_type_category_missing=(n_type_category_missing_iiro+n_type_category_missing_kira)/2) %>% mutate(.,proportion_type_category_in_missing=n_type_category_missing/sum(n_type_category_missing)) %>% mutate(.,point_estimate_missing_projected=proportion_type_category_in_missing*n_missing_type_label) %>% mutate(.,main_category=ifelse(main_category=="art","arts",main_category)) %>% distinct(main_category,n_type_category_missing,point_estimate_missing_projected)

## Download the sample of ESTC records that do have a label in the labelling scheme, and were evaluated by Kira and Iiro (e.g. both scored the label with 0, 0.5 or 1)
## The label is deemed 'correct' (1) if the two annotators gave it more than 0.5 points in total
with_labels_sample <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/estc_topics_comprehensive_sample_harmonised.csv",stringsAsFactors = FALSE,sep=",",header = TRUE) %>% filter(as.numeric(main_category_iiro)>=0 & as.numeric(main_category_kira)>=0) %>% mutate(.,main_category_joint=(as.numeric(main_category_iiro)+as.numeric(main_category_kira))) %>% mutate(.,main_category_joint_discrete=ifelse(main_category_joint>0.5,1,0))

## Convert some of the labels of the evaluation phase into more aggregated categories. If anything, this might mean that we report slightly worse
## results than what is the reality under these labels, as the evaluation phase labels were more granular

with_labels_sample <- update_categories(with_labels_sample,"book","agriculture and husbandry","science","agriculture and husbandry")
with_labels_sample <- update_categories(with_labels_sample,"in-between","agriculture and husbandry","science","agriculture and husbandry")
with_labels_sample <- update_categories(with_labels_sample,"ephemera","agriculture and husbandry","practical",NA)
with_labels_sample <- update_categories(with_labels_sample,"ephemera","news","politics",NA)
with_labels_sample <- update_categories(with_labels_sample,"unknown","periodical","literature","periodical")
with_labels_sample <- update_categories_comprehensive(with_labels_sample,"ephemera","literature","poetry","entertainment","oral")
with_labels_sample <- update_categories(with_labels_sample,"ephemera","literature","entertainment",NA)
with_labels_sample <- update_categories(with_labels_sample,"ephemera","education","practical",NA)
with_labels_sample <- update_categories(with_labels_sample,"unknown","agriculture and husbandry","science","agriculture and husbandry")
with_labels_sample <- update_categories(with_labels_sample,"ephemera","oral","entertainment","oral")
with_labels_sample <- update_categories(with_labels_sample,"ephemera","science","practical",NA)

## Calculate how often a record claimed to have a label actually (according to annotators) has the label
with_labels_sample_precision_by_main_category <- with_labels_sample %>% group_by(main_category) %>% summarise(.,precision=sum(main_category_joint_discrete)/length(main_category_joint_discrete),n_samples=length(main_category_joint_discrete),n_correctly_classified=sum(main_category_joint_discrete))

## Create tables that summarise for both annotators the number of missclassified documents by label (we filter out properly classified records from the sample that don't have the real_category_kira/iiro values) 
with_labels_sample_proportion_of_main_category_in_missclassified_iiro <- with_labels_sample %>% filter(real_category_iiro!="unknown" & real_category_iiro!="") %>% group_by(real_category_iiro) %>% summarise(.,n_misclassified_iiro=length(real_category_iiro)) 
colnames(with_labels_sample_proportion_of_main_category_in_missclassified_iiro)[1] <- "main_category"
with_labels_sample_proportion_of_main_category_in_missclassified_kira <- with_labels_sample %>% filter(real_category_kira!="unknown" & real_category_kira!="") %>% group_by(real_category_kira) %>% summarise(.,n_misclassified_kira=length(real_category_kira))
colnames(with_labels_sample_proportion_of_main_category_in_missclassified_kira)[1] <- "main_category"

## Average the misclassifications of the annotators by label. Use this averaged number of falsely classified records in the follow-up analyses of precission and coverage
with_labels_sample_proportion_of_main_category_in_missclassified_averaged <- left_join(with_labels_sample_proportion_of_main_category_in_missclassified_iiro,with_labels_sample_proportion_of_main_category_in_missclassified_kira) %>% mutate(.,n_misclassified_averaged=(n_misclassified_iiro+n_misclassified_kira)/2) %>% mutate(.,main_category=ifelse(main_category=="art","arts",main_category))


## Based on preceding calculations, calculate the point estimates for precision and coverage of different labels as defined in the article.
## 7 observations of the original 1000 have been dropped at this point, due to not being able to assign a label.
with_labels_sample_precission_recall_and_coverage <- left_join(with_labels_sample_precision_by_main_category,with_labels_sample_proportion_of_main_category_in_missclassified_averaged) %>% mutate(.,recall=n_correctly_classified/(n_correctly_classified+n_misclassified_averaged)) %>% left_join(without_labels_sample_by_category) %>% mutate(.,point_estimate_correctly_detected=(n_correctly_classified/993)*n_with_type_label) %>% mutate(.,point_estimate_all_in_labeled=point_estimate_correctly_detected/recall) %>% mutate(.,coverage=point_estimate_correctly_detected/(point_estimate_all_in_labeled+point_estimate_missing_projected)) %>% distinct(main_category,precision,recall,coverage)

## Bootstrap estimates of precision and coverage (the quantiles from the table in the article) from here on

### Create a list of samples and define seed for replication
sample_list = list()
set.seed(101)

for(i in 1:1000){

## Draw samples with replacements from the data sets of records with and without labels in the labelling scheme
## Other than that, the calculations are as they were in the preceding parts.
## We just reproduce them with different permutations of the data sets and 
## save these permutations to the sample_list. In total, we reproduce the calculations with 1000 permutations of the original data sets. 
  
without_labels_sample_draw <- without_labels_sample %>% sample_n(.,size=nrow(.),replace=TRUE)
with_labels_sample_draw <- with_labels_sample %>% sample_n(.,size=nrow(.),replace=TRUE)


without_labels_sample_draw_by_category_iiro <- without_labels_sample_draw %>% count(main_category_iiro,name="n_type_category_missing_iiro")
colnames(without_labels_sample_draw_by_category_iiro)[1] <- "main_category"
without_labels_sample_draw_by_category_kira <- without_labels_sample_draw %>% count(main_category_kira,name="n_type_category_missing_kira")
colnames(without_labels_sample_draw_by_category_kira)[1] <- "main_category"

without_labels_sample_draw_by_category <- left_join(without_labels_sample_draw_by_category_iiro,without_labels_sample_draw_by_category_kira) %>% mutate(.,n_type_category_missing=(n_type_category_missing_iiro+n_type_category_missing_kira)/2) %>% mutate(.,proportion_type_category_in_missing=n_type_category_missing/sum(n_type_category_missing)) %>% mutate(.,point_estimate_missing_projected=proportion_type_category_in_missing*n_missing_type_label) %>% mutate(.,main_category=ifelse(main_category=="art","arts",main_category)) %>% distinct(main_category,n_type_category_missing,point_estimate_missing_projected)

with_labels_sample_draw_precision_by_type_category <- with_labels_sample_draw %>% group_by(type_category) %>% summarise(.,precision=sum(main_category_joint_discrete)/length(main_category_joint_discrete),n_samples=length(main_category_joint_discrete))

with_labels_sample_draw_precision_by_main_category <- with_labels_sample_draw %>% group_by(main_category) %>% summarise(.,precision=sum(main_category_joint_discrete)/length(main_category_joint_discrete),n_samples=length(main_category_joint_discrete),n_correctly_classified=sum(main_category_joint_discrete))

with_labels_sample_draw_proportion_of_main_category_in_missclassified_iiro <- with_labels_sample_draw %>% filter(real_category_iiro!="unknown" & real_category_iiro!="") %>% group_by(real_category_iiro) %>% summarise(.,n_misclassified_iiro=length(real_category_iiro)) 
colnames(with_labels_sample_draw_proportion_of_main_category_in_missclassified_iiro)[1] <- "main_category"
with_labels_sample_draw_proportion_of_main_category_in_missclassified_kira <- with_labels_sample_draw %>% filter(real_category_kira!="unknown" & real_category_kira!="") %>% group_by(real_category_kira) %>% summarise(.,n_misclassified_kira=length(real_category_kira))
colnames(with_labels_sample_draw_proportion_of_main_category_in_missclassified_kira)[1] <- "main_category"

with_labels_sample_draw_proportion_of_main_category_in_missclassified_averaged <- left_join(with_labels_sample_draw_proportion_of_main_category_in_missclassified_iiro,with_labels_sample_draw_proportion_of_main_category_in_missclassified_kira) %>% mutate(.,n_misclassified_averaged=(n_misclassified_iiro+n_misclassified_kira)/2) %>% mutate(.,main_category=ifelse(main_category=="art","arts",main_category))

with_labels_sample_draw_precission_recall_and_coverage <- left_join(with_labels_sample_draw_precision_by_main_category,with_labels_sample_draw_proportion_of_main_category_in_missclassified_averaged) %>% mutate(.,recall=n_correctly_classified/(n_correctly_classified+n_misclassified_averaged)) %>% mutate(.,recall=ifelse(is.na(recall),1,recall)) %>% left_join(without_labels_sample_draw_by_category) %>% mutate(.,point_estimate_missing_projected=ifelse(is.na(point_estimate_missing_projected),0,point_estimate_missing_projected)) %>% mutate(.,point_estimate_correctly_detected=(n_correctly_classified/993)*n_with_type_label) %>% mutate(.,point_estimate_all_in_labeled=point_estimate_correctly_detected/recall) %>% mutate(.,coverage=point_estimate_correctly_detected/(point_estimate_all_in_labeled+point_estimate_missing_projected)) %>% distinct(main_category,precision,recall,coverage)

sample_list[[i]] <- with_labels_sample_draw_precission_recall_and_coverage

}

## Create a table of all of the bootstrapped samples of precission and coverage estimates.
bootstram_saples <- rbindlist(sample_list)

## Summary table of main_category in the bootstrapped samples
bootstram_saples_final <- bootstram_saples
bootstram_saples_final_by_category <- bootstram_saples_final %>% count(main_category)

## The main line: calculate confidence intervals of precision and coverage for different labels. This tells us how much the estimates vary between different permutations, and
## can point out uncertainty related to some point estimates
bootstram_saples_final_confidence_intervals <- bootstram_saples_final %>% group_by(main_category) %>% summarise(.,precission_97_5=quantile(precision,0.975),precission_02_5=quantile(precision,0.025),recall_97_5=quantile(recall,0.975),recall_02_5=quantile(recall,0.025),coverage_97_5=quantile(coverage,0.975),coverage_02_5=quantile(coverage,0.025))

## Create a table with both the point estimates and bootsrapped confidence intervals of coverage and precision. Create a latex table
## that is used in the article (table B.7.)
final_evaluation_table <- left_join(with_labels_sample_draw_precission_recall_and_coverage,bootstram_saples_final_confidence_intervals) %>% .[,c(1,6,2,5,8,7,3,10,4,9)]
xtable(final_evaluation_table)
