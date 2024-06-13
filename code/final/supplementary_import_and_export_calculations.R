# Information from sessionInfo() about the last run of the script:
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
###   [1] stats     graphics  grDevices utils     datasets  methods   base     

### other attached packages:
### [1] data.table_1.15.4  xgboost_1.7.7.1    eccor_1.0.1        estcr_1.5.3        xtable_1.8-4       gridExtra_2.3      rstan_2.32.6      
### [8] StanHeaders_2.32.9 lubridate_1.9.3    forcats_1.0.0      stringr_1.5.1      dplyr_1.1.4        purrr_1.0.2        readr_2.1.5       
### [15] tidyr_1.3.1        tibble_3.2.1       tidyverse_2.0.0    gghsci_1.2.2       caret_6.0-94       lattice_0.22-5     ggplot2_3.5.1     

# Set the path and call relevant scripts, functions and libraries
setwd("~/Git_root")
source("article_2024_print_culture_and_economic_constraints/code/final/functions.R")
library(tidyverse)
library(ggplot2)
library(gghsci)
library(xtable)
library(gridExtra)
library(data.table)
# For reference, the number of property owning families in different periods in our calculations:
### year:1688
### n: 91069.21

### year:1759
### n:114036.19

### year: 1798
### n:132825.91

# Additionally, private library estcr was used to construct some of the tables used. As the library overgoes changes during updates
# the repo contains saved versions of the relevant derivative tables used for easier replicability.

##pound to kg ration
pound_to_kg <- 0.4536
ounce_to_kg <- 0.02835

# Define the correction factor that results from considering survival and items missing from the analyses.
# As we are here analysing only books, we make the generous assumption that only 5 percent would have
# been lost unlike in the main analyses in which the parameter is set to 10 percent
correction_for_lost_data <- 1/(0.95*0.978*0.946)
correction_for_lost_data_original <- 1/(0.9*0.978*0.946)


# Download the data sets on number of copies printed, estc records in general and the prices.

n_of_copies <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/harmonised_number_of_copies.csv",stringsAsFactors = FALSE) %>% distinct(estc_id,copies_total)
colnames(n_of_copies) <- c("estc_id","n_copies")

initial_price_data_set <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/price_analysis_initial_data.csv",stringsAsFactors = TRUE) 
initial_price_data_set$publication_place_category <-  recode(initial_price_data_set$publication_place_category,England_other="England (other)",Scotland_other="Scotland (Other)",Ireland_other="Ireland (other)")

estc_predicted_prices <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/predicted_prices.csv",stringsAsFactors = FALSE)
estc_predicted_prices$price_class <- ordered(estc_predicted_prices$price_class, levels = c("cheap", "middle", "expensive"))

#Estc items in English
#Stoplist of Anglosphere locations
stoplist_anglosphere <- c("England","Scotland","Ireland","USA","Wales","Canada","India","Jamaica","Barbados","Barbados","Haiti","Dominica","Grenada","Saint Kitts and Nevis","St Vincent","Antigua","Guernsey","Bahamas","Antiqua and Barbuda","Isle of Man","Channel Islands","Guadeloupe","Malta","	
Martinique","Saint Vincent","Trinidad and Tobago","Northern Ireland","South Africa","Bermuda","Cuba","Netherlands Antilles","Antigua and Barbuda","Saint Lucia","Sri Lanka","Surinamee","Great Britain","Guadaloupe","Jersey","Mexico","Saint-Domingue","Scoland") %>% as.data.frame(.)
colnames(stoplist_anglosphere) <- "publication_country"


## Books printed in English in other European locations. The silenced code is here for documentation purposes
#estc_english_printed_not_in_gb_18th_c <- load_estc_core() %>% filter(publication_year>=1700 & publication_year<=1800) %>% anti_join(stoplist_anglosphere) %>% filter(!is.na(publication_country)) %>% filter(language_primary=="English") %>% filter(is_periodical=="FALSE") %>% mutate(.,gatherings_multiplier=as.numeric(str_extract(gatherings,pattern="[0-9]+"))) %>% mutate(.,sheets=(pagecount/2)/gatherings_multiplier) %>% distinct(estc_id,pagecount,sheets,gatherings.original,pagecount.orig,document_type,publication_country) 
#write.csv(estc_english_printed_not_in_gb_18th_c,"article_2024_print_culture_and_economic_constraints/data/final/editions_in_english_from_outside_anglosphere.csv",row.names = FALSE)
estc_english_printed_not_in_gb_18th_c <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/editions_in_english_from_outside_anglosphere.csv",stringsAsFactors = FALSE)
estc_english_printed_not_in_gb_18th_c_comprehensive_data <- estc_english_printed_not_in_gb_18th_c %>% filter(!is.na(gatherings.original) & !is.na(pagecount.orig))
estc_english_printed_not_in_gb_18th_c_comprehensive_data_longer_than_sheet_size_stat <- estc_english_printed_not_in_gb_18th_c_comprehensive_data %>% filter(sheets>1) %>% count(document_type)


#Raven's table of shipment to Knox with and without pamphlets
formats_knox <- c("2fo_bound","4to_bound","8vo_bound","12mo_bound","18_24_mo_bound","2fo_unbound","12mo_unbound","pamphlets")
weights_knox <- c(15*pound_to_kg,5*pound_to_kg+8*ounce_to_kg,pound_to_kg+8*ounce_to_kg,10*ounce_to_kg,4*ounce_to_kg,12*pound_to_kg,8*ounce_to_kg,1.5*ounce_to_kg)
n_volumes_knox <- c(28,129,192,1154,158,1,14,594)
table_knox <- cbind.data.frame(formats_knox,weights_knox,n_volumes_knox)

table_knox_no_pamphlets <- table_knox %>% filter(formats_knox!="pamphlets") %>% mutate(.,prop_of_weight=(n_volumes_knox*weights_knox)/(sum(n_volumes_knox*weights_knox)))
knox_weight_per_volume_no_pamphlets  <- sum(table_knox_no_pamphlets$weights_knox*table_knox_no_pamphlets$n_volumes_knox)/sum(table_knox_no_pamphlets$n_volumes_knox)

# Based on Ravens table, we reduce 20 percent of the weights so as to account for boards. We assumed a similar
# weight for 16mo as for 12mo
formats <- c("2fo","4to","8vo","12mo","16mo","18mo","24mo","32mo","48mo","64mo")
weight_per_volume_without_boards <- c(pound_to_kg*15,5*pound_to_kg+8*ounce_to_kg,pound_to_kg+8*ounce_to_kg,10*ounce_to_kg,10*ounce_to_kg,4*ounce_to_kg,4*ounce_to_kg,1.5*ounce_to_kg,1.5*ounce_to_kg,1*ounce_to_kg)*0.8
weight_per_volume_conversion_table <- cbind.data.frame(formats,weight_per_volume_without_boards)
colnames(weight_per_volume_conversion_table) <- c("gatherings","weight_per_volume")

#Based on the table, calculate the approximated annual weight of book-sized items. For items missing the volume estimate, use the estimate of 1 volume, which should steer as towards a lower bound estimate. Weight of volumes high, that does not assume one volume per edition
#and adjusts for survival (although less than standard calculations) and unqualified records is the final estimate.
initial_price_data_set_england_longer_than_one_sheet_book_sized_with_weight_estimates_simple <- initial_price_data_set %>% filter(publication_country=="England" & publication_year>=1700 & publication_year<=1800 & sheets>1 & pagecount_total>=128) %>% left_join(weight_per_volume_conversion_table) %>% mutate(.,volcount_total=ifelse(is.na(volcount_total),1,volcount_total))
initial_price_data_set_england_longer_than_one_sheet_book_sized_with_weight_estimates_simple_yearly_total <- initial_price_data_set_england_longer_than_one_sheet_book_sized_with_weight_estimates_simple %>% group_by(publication_year) %>% summarise(.,weight_of_volumes_lowest=sum(weight_per_volume)*1000,weight_of_volumes_standard=1000*sum(weight_per_volume*volcount_total),weight_of_volumes_high=1000*sum(weight_per_volume*volcount_total)*correction_for_lost_data,n_editions=length(estc_id))
initial_price_data_set_england_longer_than_one_sheet_book_sized_with_weight_estimates_simple_five_year_summaries <- initial_price_data_set_england_longer_than_one_sheet_book_sized_with_weight_estimates_simple_yearly_total %>% mutate(.,five_year_interval=floor(publication_year/5)*5) %>% group_by(five_year_interval) %>% summarise(.,weight_of_volumes_lowest=mean(weight_of_volumes_lowest),weight_of_volumes_standard=mean(weight_of_volumes_standard),weight_of_volumes_high=mean(weight_of_volumes_high),n_copies=mean(n_editions)*1000*correction_for_lost_data_original)

#This will be used later to transform volumes to editions
volume_per_edition <-  mean(initial_price_data_set_england_longer_than_one_sheet_book_sized_with_weight_estimates_simple$volcount_total)

#Download the import and export data summarised from Barber1976 and transform to kilograms of imports and exports
imports_and_exports <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/imports_and_exports.csv",stringsAsFactors = FALSE,header = TRUE)
colnames(imports_and_exports) <- c("publication_year","import_bound","import_unbound","export_east","export_west")
imports_and_exports <- imports_and_exports %>% mutate(.,total_weight_imports=import_bound*50.8+(import_unbound/8)*50.8,total_weight_exports=((export_east+export_west)/4)*50.8) %>% mutate(.,net_difference=total_weight_imports-total_weight_exports) %>% mutate(.,five_year_group=5*floor(publication_year/5))
imports_and_exports_five_year_means <- imports_and_exports %>% group_by(five_year_group) %>% summarise(total_weight_imports=mean(total_weight_imports,na.rm=TRUE),total_weight_exports=mean(total_weight_exports,na.rm = TRUE),net_difference=mean(net_difference,na.rm=TRUE))

#Re-save the table of weight estimates for book production on five year basis (unnecessary step, remove later)
weight_estimates_for_british_books <- initial_price_data_set_england_longer_than_one_sheet_book_sized_with_weight_estimates_simple_five_year_summaries %>% mutate(.,total_weight=weight_of_volumes_high,five_year_group=five_year_interval)

#Produce five-year comparisons between weight of imports and exports and domestic production
imports_and_exports_five_year_means_with_english_domestic_books <- left_join(imports_and_exports_five_year_means,weight_estimates_for_british_books) %>% mutate(.,imports_prop_of_domestic=total_weight_imports/total_weight,exports_prop_of_domestic_total=total_weight_exports/total_weight,prop_of_net_difference=net_difference/total_weight)

plot_of_imports_exports_and_net_impact <- ggplot(data=imports_and_exports_five_year_means_with_english_domestic_books)+geom_line(aes(x=five_year_group,y=imports_prop_of_domestic*100),linetype="dotted",lwd=1.2)+geom_line(aes(x=five_year_group,y=exports_prop_of_domestic_total*100),linetype="dashed",lwd=1.2)+geom_line(aes(x=five_year_group,y=prop_of_net_difference*100),lwd=1.2)+ylab("% of Domestic Book Production")+xlab("Five-Year Average")+geom_point(aes(x=five_year_group,y=imports_prop_of_domestic*100,size=1),shape=17)+geom_point(aes(x=five_year_group,y=exports_prop_of_domestic_total*100,size=1),shape=15)+geom_point(aes(x=five_year_group,y=prop_of_net_difference*100,size=1))+gghsci::theme_hsci_continuous_grayscale(base_size = 20)+theme(legend.position = "none")
plot_of_imports_exports_and_net_impact

plot_of_imports_exports_and_net_impact_in_editions <- ggplot(data=imports_and_exports_five_year_means_with_english_domestic_books)+geom_line(aes(x=five_year_group,y=imports_prop_of_domestic*n_copies),linetype="dotted",lwd=1.2)+geom_line(aes(x=five_year_group,y=exports_prop_of_domestic_total*n_copies),linetype="dashed",lwd=1.2)+geom_line(aes(x=five_year_group,y=prop_of_net_difference*n_copies),lwd=1.2)+ylab("N of Copies")+xlab("Five-Year Average")+geom_point(aes(x=five_year_group,y=imports_prop_of_domestic*n_copies,size=1),shape=17)+geom_point(aes(x=five_year_group,y=exports_prop_of_domestic_total*n_copies,size=1),shape=15)+geom_point(aes(x=five_year_group,y=prop_of_net_difference*n_copies,size=1))+gghsci::theme_hsci_continuous_grayscale(base_size = 20)+theme(legend.position = "none")
plot_of_imports_exports_and_net_impact_in_editions

#Estimate how many volumes the max year of exports could have corresponded to with Knox weighting without pamphlets. About 31 percent.
volcount_knox_weighting <- sum((max(imports_and_exports_five_year_means_with_english_domestic_books$total_weight_exports)*table_knox_no_pamphlets$prop_of_weight)/table_knox_no_pamphlets$weights_knox)
volcount_knox_weighting_to_editions <- volcount_knox_weighting/volume_per_edition
prop_of_annual_copies <- volcount_knox_weighting_to_editions/(imports_and_exports_five_year_means_with_english_domestic_books$n_copies[which.max(imports_and_exports_five_year_means_with_english_domestic_books$total_weight_exports)])


# Create and save Fig 7 of the article.
joint_exports_and_imports_plot <- grid.arrange(plot_of_imports_exports_and_net_impact,plot_of_imports_exports_and_net_impact_in_editions)
joint_exports_and_imports_plot 

ggsave(filename = "article_2024_print_culture_and_economic_constraints/output/figures/final/fig_7.pdf", plot=joint_exports_and_imports_plot)
embedFonts("article_2024_print_culture_and_economic_constraints/output/figures/final/fig_7.pdf")

# Calculate the impact of exports and imports relative to the number of large property-owning families

### How many books per property-owning household were imported in peak periods?
imported_books_per_large_property_owning_family_late_1720s <- (imports_and_exports_five_year_means_with_english_domestic_books$imports_prop_of_domestic[6]*imports_and_exports_five_year_means_with_english_domestic_books$n_copies[6])/91069.21
imported_books_per_large_property_owning_family_late_1760s <- (imports_and_exports_five_year_means_with_english_domestic_books$imports_prop_of_domestic[14]*imports_and_exports_five_year_means_with_english_domestic_books$n_copies[14])/114036.19

### How many books per property-owning household were exported during early 1770's
books_per_large_property_owning_household_domestic_taken_by_exports_in_early_1770s <- (prop_of_annual_copies*imports_and_exports_five_year_means_with_english_domestic_books$n_copies[15])/114036.19
books_per_large_property_owning_household_domestic_without_removing_exports_in_early_1770s <- (imports_and_exports_five_year_means_with_english_domestic_books$n_copies[15])/114036.19

###