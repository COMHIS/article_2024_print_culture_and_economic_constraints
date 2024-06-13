# Info about the last time the script was run (from sessionInfo())

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
###  [1] tidytext_0.4.2     patchwork_1.2.0    data.table_1.15.4  xgboost_1.7.7.1    eccor_1.0.1        estcr_1.5.3        xtable_1.8-4      
### [8] gridExtra_2.3      rstan_2.32.6       StanHeaders_2.32.9 lubridate_1.9.3    forcats_1.0.0      stringr_1.5.1      dplyr_1.1.4       
### [15] purrr_1.0.2        readr_2.1.5        tidyr_1.3.1        tibble_3.2.1       tidyverse_2.0.0    gghsci_1.2.2       caret_6.0-94      
### [22] lattice_0.22-5     ggplot2_3.5.1   


# Set the path and call relevant scripts, functions and libraries
setwd("~/Git_root")
source("article_2024_print_culture_and_economic_constraints/code/final/functions.R")
source("article_2024_print_culture_and_economic_constraints/code/final/price_constraint_and_income_analyses.R")
library(tidyverse)
library(ggplot2)
library(gghsci)
library(xtable)
library(gridExtra)
library(data.table)

# Define the correction factor that results from considering
correction_for_lost_data <- 1/(0.9*0.978*0.946)

## Download the data sets on number of copies printe

n_of_copies <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/harmonised_number_of_copies.csv",stringsAsFactors = FALSE) %>% distinct(estc_id,copies_total)
colnames(n_of_copies) <- c("estc_id","n_copies")

## Download the data set of ESTC records in general

initial_price_data_set <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/price_analysis_initial_data.csv",stringsAsFactors = TRUE) 
initial_price_data_set$publication_place_category <-  recode(initial_price_data_set$publication_place_category,England_other="England (other)",Scotland_other="Scotland (Other)",Ireland_other="Ireland (other)")

## Download the data set of ESTC records with prices

estc_predicted_prices <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/predicted_prices.csv",stringsAsFactors = FALSE)
estc_predicted_prices$price_class <- ordered(estc_predicted_prices$price_class, levels = c("cheap", "middle", "expensive"))

# Analysis

# overall number of price predictions for all records longer than 1 sheet and books 
n_of_predicted_prices_longer_than_1_sheet <- initial_price_data_set %>% distinct(estc_id,sheets,publication_year) %>% filter(sheets>1 & publication_year>=1700 & publication_year<=1800) %>% left_join(estc_predicted_prices) %>% filter(!is.na(price_class)) %>% nrow(.)
n_of_predicted_prices_longer_than_1_sheet <- initial_price_data_set %>% distinct(estc_id,sheets,publication_year,pagecount_total) %>% filter(sheets>1 & publication_year>=1700 & publication_year<=1800 & pagecount_total>=128) %>% left_join(estc_predicted_prices) %>% filter(!is.na(price_class)) %>% nrow(.)

## Plot the overall trends for eighteenth century (panels of the Fig 4. of the article). Limit the analysis to records longer than 1 sheet

### For all records more than 1 sheet
estc_overall_price_plot <- initial_price_data_set %>% distinct(estc_id,sheets,publication_year) %>% filter(sheets>1 & publication_year>=1700 & publication_year<=1800) %>% left_join(estc_predicted_prices) %>% filter(!is.na(price_class)) %>% count(publication_year,price_class) %>% mutate(.,Price=price_class) %>% ggplot(data=.)+geom_col(aes(x=publication_year,y=n*correction_for_lost_data,group=Price,colour=Price,fill=Price))+xlab("Year")+ylab("Editions (N)")+theme_hsci_discrete_grayscale(palette="bright",base_size = 18)
estc_overall_price_plot

### For books (more than 128 pages)
estc_overall_price_plot_books <- initial_price_data_set %>% distinct(estc_id,pagecount_total,publication_year,sheets) %>% filter(pagecount_total>=128 & publication_year>=1700 & publication_year<=1800 & sheets>1) %>% left_join(estc_predicted_prices) %>% filter(!is.na(price_class)) %>% count(publication_year,price_class) %>% mutate(.,Price=price_class) %>% ggplot(data=.)+geom_col(aes(x=publication_year,y=n*correction_for_lost_data,group=Price,colour=Price,fill=Price))+xlab("Year")+ylab("Editions (N)")+theme_hsci_discrete_grayscale(palette="bright",base_size = 18)
estc_overall_price_plot_books

### Create a plot with the relative proportions of the price classes by year
more_than_1_sheet_proportions_by_year<- initial_price_data_set %>% left_join(estc_predicted_prices %>% distinct(estc_id,price_class),by="estc_id") %>% distinct(estc_id,sheets,publication_year,price_class) %>% filter(sheets>1 & publication_year>=1700 & publication_year<=1800) %>% group_by(publication_year,price_class) %>% summarise(.,n=length(price_class)) %>% filter(!is.na(price_class))
more_than_1_sheet_proportions_by_year_n_total <- more_than_1_sheet_proportions_by_year %>% group_by(publication_year) %>% summarise(.,n_total=sum(n))
more_than_1_sheet_proportions_by_year <- left_join(more_than_1_sheet_proportions_by_year,more_than_1_sheet_proportions_by_year_n_total) %>% mutate(.,prop=n/n_total) %>% distinct(publication_year,price_class,n,prop,n_total) 
more_than_1_sheet_proportions_by_year_plot <- more_than_1_sheet_proportions_by_year %>% count(publication_year,price_class,wt=prop) %>% mutate(.,Price=price_class) %>% ggplot(data=.)+geom_col(aes(x=publication_year,y=n*100,group=Price,colour=Price,fill=Price))+xlab("Year")+ylab("Proportion (%)")+theme_hsci_discrete_grayscale(palette="bright",base_size = 18)
more_than_1_sheet_proportions_by_year_plot 

### Create a plot with the relative proportions of the price classes by year for book_sized_items

more_than_128_pages_proportions_by_year<- initial_price_data_set %>% left_join(estc_predicted_prices %>% distinct(estc_id,price_class),by="estc_id") %>% distinct(estc_id,pagecount_total,publication_year,price_class,sheets) %>% filter(pagecount_total>=128 & publication_year>=1700 & publication_year<=1800 & sheets>1) %>% group_by(publication_year,price_class) %>% summarise(.,n=length(price_class)) %>% filter(!is.na(price_class))
more_than_128_pages_proportions_by_year_n_total <- more_than_128_pages_proportions_by_year %>% group_by(publication_year) %>% summarise(.,n_total=sum(n))
more_than_128_pages_proportions_by_year <- left_join(more_than_128_pages_proportions_by_year,more_than_128_pages_proportions_by_year_n_total) %>% mutate(.,prop=n/n_total) %>% distinct(publication_year,price_class,n,prop,n_total) 
more_than_128_pages_proportions_by_year_plot <- more_than_128_pages_proportions_by_year %>% count(publication_year,price_class,wt=prop) %>% mutate(.,Price=price_class) %>% ggplot(data=.)+geom_col(aes(x=publication_year,y=n*100,group=Price,colour=Price,fill=Price))+xlab("Year")+ylab("Proportion (%)")+theme_hsci_discrete_grayscale(palette="bright",base_size = 18)
more_than_128_pages_proportions_by_year_plot 

## Create plots that illustrate the distribution of price segments in different topics/genres etc. (panels of Fig.5).

estc_overall_price_plot_categories <- initial_price_data_set %>% mutate(.,main_category=as.character(main_category)) %>% distinct(estc_id,sheets,publication_year,main_category) %>% filter(sheets>1 & publication_year>=1700 & publication_year<=1800) %>% left_join(estc_predicted_prices) %>% filter(!is.na(price_class)) %>% mutate(.,main_category=ifelse(main_category=="entertainment","ephemera-entertainment",main_category)) %>% mutate(.,main_category=ifelse(main_category=="practical","ephemera-practical",main_category)) %>% count(main_category,price_class) %>% filter(!is.na(main_category))  %>% mutate(.,Price=price_class) %>% ggplot(data=.)+geom_col(aes(x=reorder(main_category,n),y=n*correction_for_lost_data,group=Price,colour=Price,fill=Price))+xlab("Type of Publication")+ylab("Editions (N)")+theme_hsci_discrete_grayscale(palette="bright",base_size = 18)+coord_flip()
estc_overall_price_plot_categories

estc_overall_price_plot_categories_proportional <- initial_price_data_set %>% mutate(.,main_category=as.character(main_category)) %>% distinct(estc_id,sheets,publication_year,main_category) %>% filter(sheets>1 & publication_year>=1700 & publication_year<=1800 & !is.na(main_category)) %>% left_join(estc_predicted_prices) %>% filter(!is.na(price_class)) %>%  mutate(.,main_category=ifelse(main_category=="entertainment","ephemera-entertainment",main_category)) %>% mutate(.,main_category=ifelse(main_category=="practical","ephemera-practical",main_category)) %>% count(main_category,price_class) %>% filter(!is.na(main_category)) %>% left_join(.,count(.,main_category,wt=n) %>% name_data_frame(.,c("main_category","n_total"))) %>% mutate(.,Price=price_class) %>% ggplot(data=.)+geom_col(aes(x=reorder(main_category,n),y=n/n_total*100,group=Price,colour=Price,fill=Price))+xlab("Type of Publication")+ylab("Proportion (%)")+theme_hsci_discrete_grayscale(palette="bright",base_size = 18)+coord_flip()
estc_overall_price_plot_categories_proportional


## Merge the different plots (panels) of Fig.4 and Fig.5 of the article together.
plot_temporal <- arrangeGrob(estc_overall_price_plot,estc_overall_price_plot_books,more_than_1_sheet_proportions_by_year_plot,more_than_128_pages_proportions_by_year_plot 
,nrow=4)
#plot_spatial <- arrangeGrob(estc_overall_price_plot_place,estc_overall_price_plot_place_proportional,nrow=2)
plot_categorical <- arrangeGrob(estc_overall_price_plot_categories,estc_overall_price_plot_categories_proportional,nrow=2)

## Save Fig.4 and Fig.5 of the article

### Save Fig. 4
ggsave(filename = "article_2024_print_culture_and_economic_constraints/output/figures/final/fig_4.pdf",plot=plot_temporal)
embedFonts("article_2024_print_culture_and_economic_constraints/output/figures/final/fig_4.pdf")

### Save Fig. 5
ggsave(filename = "article_2024_print_culture_and_economic_constraints/output/figures/final/fig_5.pdf",plot=plot_categorical)
embedFonts("article_2024_print_culture_and_economic_constraints/output/figures/final/fig_5.pdf")

## Create a table that includes the relative proportions of the three price classes in early, middle and late eighteenth century
## together with information about the size and income of the social classes Allen (2019) derives from the social tables
## for the years 1688, 1759 and 1798.

### Create a table with the relative proportions of the price classes by period, limit to England as Social tables are for England and Wales
more_than_1_sheet_proportions_in_three_times <- initial_price_data_set %>% left_join(estc_predicted_prices %>% distinct(estc_id,price_class),by="estc_id") %>% distinct(estc_id,sheets,publication_year,price_class,publication_country) %>% filter(sheets>1 & publication_year>=1700 & publication_year<=1800 & publication_country=="England") %>% mutate(.,period=ifelse(publication_year>=1700 & publication_year<1705,"early_18th",NA)) %>% mutate(.,period=ifelse(publication_year>=1757 & publication_year<=1761,"mid_18th",period)) %>% mutate(.,period=ifelse(publication_year>=1796 & publication_year<=1800,"late_18th",period)) %>% filter(!is.na(period)) %>% group_by(period,price_class) %>% summarise(.,n=length(price_class)*correction_for_lost_data) %>% filter(!is.na(price_class))
more_than_1_sheet_proportions_in_three_times_n_total <- more_than_1_sheet_proportions_in_three_times %>% group_by(period) %>% summarise(.,n_total=sum(n))
more_than_1_sheet_proportions_in_three_times <- left_join(more_than_1_sheet_proportions_in_three_times,more_than_1_sheet_proportions_in_three_times_n_total) %>% mutate(.,prop=n/n_total) %>% distinct(period,price_class,n,prop,n_total) 

### Combine the table of social classes to the table of the relative proportions of print products in different price classes
social_tables_chosen_quantiles_to_output_numbers <- social_tables_chosen_quantiles %>% left_join(period_to_year_mapping) %>% left_join(more_than_1_sheet_proportions_in_three_times) %>% mutate(.,n_per_household_per_year=n_total/n_families*1000/5) %>% distinct(period,class,prop_of_cheap_item,prop_of_middle_item,n_per_household_per_year) %>% left_join(pivot_wider(id_cols=period,data=more_than_1_sheet_proportions_in_three_times,names_from = price_class,names_expand = TRUE,values_from = prop)) %>% mutate(.,cheap_per_household=n_per_household_per_year*cheap,middle_per_household=n_per_household_per_year*middle,expensive_per_household=n_per_household_per_year*expensive) %>% distinct(class,period,prop_of_cheap_item,prop_of_middle_item,cheap_per_household,middle_per_household,expensive_per_household)

### Print the joint table of proportions of records in different classes with income quantiles 
print.xtable(xtable(social_tables_chosen_quantiles_to_output_numbers))

## Appendix material: The prices connected to the number of copies
copies_with_predicted_prices <- left_join(n_of_copies,estc_predicted_prices)
copies_with_predicted_prices_plot <- copies_with_predicted_prices %>% filter(!is.na(n_copies) & n_copies>0 & !is.na(relative_price)) %>% ggplot(data=.)+geom_point(aes(x=log(relative_price),y=log(n_copies)))

### Visualisation of dependence on log scale
copies_with_predicted_prices_plot

## Summary statistics on the relationship between price and the number of copies used in the article
copies_with_predicted_prices_summary <- copies_with_predicted_prices %>% filter(!is.na(n_copies) & n_copies>0 & !is.na(relative_price) & sheets>1) %>% group_by(price_class) %>% summarise(.,median=median(n_copies),mean=mean(n_copies),std=sd(n_copies),max=max(n_copies),n=length(n_copies))
print.xtable(xtable(copies_with_predicted_prices_summary))