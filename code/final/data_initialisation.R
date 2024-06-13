# Information from the function sessionInfo() of the previous run:
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
###  [1] eccor_1.0.1        estcr_1.5.3        xtable_1.8-4       gridExtra_2.3      rstan_2.32.6       StanHeaders_2.32.9 lubridate_1.9.3   
### [8] forcats_1.0.0      stringr_1.5.1      dplyr_1.1.4        purrr_1.0.2        readr_2.1.5        tidyr_1.3.1        tibble_3.2.1      
### [15] tidyverse_2.0.0    gghsci_1.2.2       caret_6.0-94       lattice_0.22-5     ggplot2_3.5.1  


#Set paths, call functions and libraries used in the script
setwd("~/Git_root")
source("article_2024_print_culture_and_economic_constraints/code/final/functions.R")
library(estcr)
library(eccor)
library(tidyverse)

#Download and create data from which the data used in the analysis will be constructed
# For replicating the final data used in the article, the ESTC and ECCO data should be used as it was in 1.4.2023
#Other than for the genre/supject topic data, which is the version from 17.10.2023.

##Construct conversion table - the fraction of sheets per leaf for different gatherings
formats <- c("8vo","12mo","2fo","4to","32mo","16mo","18mo","24mo","48mo","64mo")
sheet_multipliers <- c(1/8,1/12,1/2,1/4,1/32,1/16,1/18,1/24,1/48,1/64)
format_conversion_table <- cbind.data.frame(formats,sheet_multipliers)
colnames(format_conversion_table)[1] <- "gatherings"

##Download the table of monetary conversations to correct for changing real value of the pound
monetary_conversions <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/pound_conversions.csv",stringsAsFactors = FALSE)
#Create a table for converting prices to 1688 real price/wage values
monetary_conversions_to_1688_pounds <- monetary_conversions %>% mutate(.,conversion_rate_to_1688_real_wage_value=240/(pound*240+shilling*12+pence)) %>% distinct(year,conversion_rate_to_1688_real_wage_value)
colnames(monetary_conversions_to_1688_pounds)[1] <- "publication_year"

##Load the price table of Fielding and Robertson
fielding_and_robertson_table <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/fielding_and_robertson_table_parsed.csv",stringsAsFactors = FALSE) %>% distinct(estc_id,total_price) %>% rename(.,total_price_source_2=total_price)

##Load price data from the ESTC
estc_prices <- read.csv("data-raw/estc-data-unified/estc-prices/estc_prices.tsv",sep="\t") %>% filter(!is.na(total_price))
###Load corrections table
estc_prices_correction_table <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/price_correction_table.csv",stringsAsFactors = FALSE)
###Correct prices found to be erroneous during the article writing process
estc_prices$total_price[match(estc_prices_correction_table$estc_id,estc_prices$estc_id)] <- estc_prices_correction_table$total_price

###Add the price data of Fielding and Robertson to the price data used in this article
estc_prices <- full_join(estc_prices,fielding_and_robertson_table) %>% mutate(.,total_price=ifelse(!is.na(total_price),total_price,total_price_source_2)) %>% mutate(.,total_price_source_2=NULL)

##Load relevant ecco and estc data frames
load_ecco()
load_estc()

###Standardise variable names
colnames(ecco_graphics)[2] <- "document_id_octavo"


## Load estc-ecco links. Versions last modified on 29.6.2022 (e.g. loaded as they were from github then)
estc_links <- read.csv("data-raw/estc-data-unified/estc-eebo-ecco-links/idpairs_ecco_eebo_estc.csv",colClasses=c("document_id_octavo"="character"),stringsAsFactors = FALSE) %>% .[,c("document_id_octavo","estc_id_student_edition")] %>% name_data_frame(.,c("document_id_octavo","estc_id"))
estc_links_ecco <- read.csv("data-raw/estc-data-unified/estc-eebo-ecco-links/idpairs_ecco_eebo_estc.csv",colClasses=c("document_id_octavo"="character"),stringsAsFactors = FALSE) %>% .[,c("document_id_octavo","estc_id_student_edition","collection")] %>% name_data_frame(.,c("document_id_octavo","estc_id","collection")) %>% subset(.,collection=="ecco") %>% distinct(document_id_octavo)

## Get pagecount and number of volume estimates from ESTC and ECCO and process further

###Get the number of pages per ecco document that have graphical elements, the number of such pages is saved to ecco_pagecount_plate.
###This is not to say that all graphical elements were produced with plates
ecco_graphics_engraving_pages <- ecco_graphics  %>% distinct(document_id_octavo,page_number) %>% count(document_id_octavo) %>% name_data_frame(.,c("document_id_octavo","ecco_pagecount_plate"))

### Based on the estc plate page and total page estimates, derive an estimate of the number of normal pages. Here we
###Assume that the pages with plates were illustrated on one side only, hence we divide the pagecount.plate by 2
estc_core_pages <- estc_core %>% distinct(estc_id,pagecount,pagecount.plate) %>% mutate(.,pagecount.plate=pagecount.plate/2) %>% mutate(.,pagecount_normal=pagecount-pagecount.plate) %>% subset(.,!is.na(pagecount))
estc_core_volume <- estc_core %>% distinct(estc_id,volcount)

### Based on the ecco plate page and total page estimates, derive an estimate of the number of normal pages
ecco_core_pages <- ecco_core %>% distinct(ecco_id,ecco_pages) %>% name_data_frame(.,c("document_id_octavo","ecco_pagecount"))
ecco_core_pages <- left_join(ecco_core_pages,ecco_graphics_engraving_pages)
ecco_core_pages$ecco_pagecount_plate[is.na(ecco_core_pages$ecco_pagecount_plate)] <- 0
ecco_core_pages <- ecco_core_pages %>% mutate(.,ecco_pagecount_normal=ecco_pagecount-ecco_pagecount_plate)
ecco_core_pages <- ecco_core_pages %>% name_data_frame(.,c("document_id_octavo","ecco_pagecount","ecco_pagecount_plate","ecco_pagecount_normal"))

ecco_core_normal_pages_to_estc <- ecco_core_pages %>% left_join(.,estc_links) %>% count(estc_id,wt=ecco_pagecount_normal) %>% name_data_frame(.,c("estc_id","ecco_pagecount"))
ecco_core_plate_pages_to_estc <- ecco_core_pages %>% left_join(.,estc_links) %>% count(estc_id,wt=ecco_pagecount_plate) %>% name_data_frame(.,c("estc_id","ecco_pagecount_plate"))
ecco_core_volumes_to_estc <- ecco_core_pages %>% left_join(.,estc_links) %>% count(estc_id) %>% name_data_frame(.,c("estc_id","ecco_volcount"))
ecco_core_pages_to_estc <- left_join(ecco_core_normal_pages_to_estc,ecco_core_plate_pages_to_estc) %>% name_data_frame(.,c("estc_id","ecco_pagecount_normal","ecco_pagecount_plate")) %>% mutate(.,ecco_pagecount=ecco_pagecount_normal+ecco_pagecount_plate)

##Take the ids of book-sized items to a table. These will be used to filter edition information
estc_books <- estc_core %>% distinct(estc_id,document_type) %>% filter(document_type=="Book") %>% distinct(estc_id)

##Subset to the records that are not periodicals - for this first select all periodicals to a separate data frame
periodical_publication_ids <- estc_core %>% subset(.,!is.na(freq) | is_periodical==TRUE) %>% distinct(estc_id)


##Download,filter (remove duplicates), and map to editions the genre information used in the article

estc_main_categories_all_no_duplicates <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/genre_labels.csv",stringsAsFactors = FALSE) %>% distinct(estc_id,main_category) %>% left_join((estc_core %>% distinct(estc_id)),.) %>% mutate(.,main_category=ifelse(is.na(main_category),"unknown",main_category))

##Get edition information

### Get first year editions for such editions that have a work-id
### Remove the editions without a work id (staring with X) and limit the consideration of edition to book sized items (here the assumption is, that copyrights would have mostly been relevant for works
### of highly profilic authors that would have been named as authors of the work at hand.)
### As we do not have hypothesis that pamphlets, proclamations etc. would vary in their price as a function of the 'edition'
estc_works <- estc_core[-grep(x=estc_core$work_id,pattern = "^X"),] %>% inner_join(estc_books) %>% distinct(work_id,estc_id,publication_year)
###Create a binary variable is_first_year_edition that distinguishes first year editions of works from other editions
estc_works_first_editions <- aggregate(estc_works$publication_year,by=list(estc_works$work_id),FUN=min) %>% name_data_frame(.,c("work_id","publication_year"))
estc_first_year_editions <- left_join(estc_works_first_editions,estc_works) %>% distinct(estc_id) %>% mutate(.,is_first_year_edition = "yes")
estc_latter_editions <- estc_works %>% distinct(estc_id) %>% anti_join(estc_first_year_editions) %>%  mutate(.,is_first_year_edition = "no")
estc_editions <- anti_join(estc_core,estc_first_year_editions) %>% anti_join(estc_latter_editions) %>% distinct(estc_id) %>% mutate(.,is_first_year_edition = "other") %>% rbind.data.frame(.,estc_first_year_editions,estc_latter_editions)


##Get ecco character numbers per page
ecco_core_characters <- ecco_core %>% distinct(ecco_id,ecco_nr_characters)
estc_characters_per_page_groups <-  ecco_core_characters %>% name_data_frame(.,c("document_id_octavo","ecco_nr_characters")) %>% left_join(.,estc_links) %>% count(estc_id,wt=ecco_nr_characters,name = "estc_nr_characters") %>% left_join(.,ecco_core_pages_to_estc) %>% mutate(.,estc_characters_per_page=estc_nr_characters/ecco_pagecount) %>% distinct(estc_id,estc_characters_per_page) 

## Create and save a table of publishers involved in the DB case used in the analyses

top_publishers <- read.csv("article_2024_print_culture_and_economic_constraints/data/final/top_publishers.csv",stringsAsFactors = FALSE)
top_publishers_with_metadata <- left_join(top_publishers,load_estc_actors())
top_publishers_editions <- load_estc_actor_links() %>% subset(.,actor_role_publisher=="TRUE") %>% inner_join(top_publishers) %>% distinct(estc_id) %>% left_join(load_estc_core()) %>% mutate(.,top_publisher_pre_1774=as.factor(ifelse(publication_year<=1774,"top_publisher_pre_1774","top_publisher_post_1774"))) %>% distinct(estc_id,top_publisher_pre_1774)

write.csv(top_publishers_editions,"article_2024_print_culture_and_economic_constraints/data/final/top_publisher_editions.csv",row.names = FALSE)


#The compilation and filtering of the final data set.
### 1) Keep only records that have place, publication year and original page count and original gatherings in the ESTC
### 2) Keep only records that have normal pages (e.g. to ensure that they are print products and not wholly visual pieces). Two checks for this:
### require at least one 'normal' page in the ESTC data, and that the proportion of special pages in less than 1 in final estimates.
### 3) Prices of multi-volume works are only accepted if they cost more than 36 pence, as this is the extent they have been manually
###inspected. We also accept higher prices coming from other sources without inspection. The title page price 
###is sometimes unclear on whether it covers the entire edition or only one volume, hence the need for manual inspection.
### 4) Remove periodicals from the data set

estc_selected_variables <- estc_core %>% subset(.,!is.na(pagecount.orig) & !is.na(gatherings.original) & !is.na(publication_year) & !is.na(publication_place) & !is.na(publication_country)) %>% distinct(estc_id,area,publication_place,publication_country,publication_year,publication_decade,gatherings,work_id) %>% left_join(.,estc_core_pages) %>% left_join(.,ecco_core_pages_to_estc) %>% subset(.,pagecount_normal>0)  %>% left_join(.,estc_core_volume) %>% left_join(.,ecco_core_volumes_to_estc) %>% left_join(.,estc_characters_per_page_groups) %>% left_join(.,estc_main_categories_all_no_duplicates) %>% left_join(.,estc_editions) %>% anti_join(periodical_publication_ids) %>% mutate(.,pagecount_total=pmax(pagecount,ecco_pagecount,na.rm = TRUE)) %>% mutate(,pagecount_total=ifelse(pagecount_total %% 2 == 1,pagecount_total+1,pagecount_total)) %>% mutate(.,pagecount_plate_total=pmax(pagecount.plate,ecco_pagecount_plate,na.rm=TRUE)) %>% mutate(.,volcount_total=pmax(volcount,ecco_volcount,na.rm = TRUE)) %>% mutate(.,special_page_proportion=pagecount_plate_total/(pagecount_total)) %>% filter(special_page_proportion<1) %>% mutate(normal_page_proportion=1-special_page_proportion) %>% distinct(estc_id,pagecount_total,volcount_total,gatherings,area,publication_year,publication_place,publication_country,estc_characters_per_page,special_page_proportion,main_category,work_id,is_first_year_edition) %>% subset(.,publication_country=="England" | publication_country=="Scotland" | publication_country=="Ireland") %>% mutate(.,publication_place_category=character(nrow(.)))

### Convert the publication place and country information in the ESTC to the categorisation used in this article

estc_selected_variables$publication_place_category[estc_selected_variables$publication_place=="London"&estc_selected_variables$publication_country=="England"] <- "London"
estc_selected_variables$publication_place_category[(estc_selected_variables$publication_place=="Cambridge"|estc_selected_variables$publication_place=="Oxford")&estc_selected_variables$publication_country=="England"] <- "Oxbridge"
estc_selected_variables$publication_place_category[estc_selected_variables$publication_place_category==""&estc_selected_variables$publication_country=="England"] <- "England_other"

estc_selected_variables$publication_place_category[estc_selected_variables$publication_place=="Edinburgh"&estc_selected_variables$publication_country=="Scotland"] <- "Edinburgh"
estc_selected_variables$publication_place_category[estc_selected_variables$publication_place_category==""&estc_selected_variables$publication_country=="Scotland"] <- "Scotland_other"

estc_selected_variables$publication_place_category[estc_selected_variables$publication_place=="Dublin"&estc_selected_variables$publication_country=="Ireland"] <- "Dublin"
estc_selected_variables$publication_place_category[estc_selected_variables$publication_place_category==""&estc_selected_variables$publication_country=="Ireland"] <- "Ireland_other"


### Map ESTC gatherings to the corresponding notation we use in this article
estc_selected_variables$gatherings <- gsub(x=estc_selected_variables$gatherings,pattern = "(small)|(long)",replacement = "")
estc_selected_variables$gatherings <- gsub(x=estc_selected_variables$gatherings,pattern = "^2$",replacement = "2fo")
estc_selected_variables$gatherings <- gsub(x=estc_selected_variables$gatherings,pattern = "^4$",replacement = "4to")
estc_selected_variables$gatherings <- gsub(x=estc_selected_variables$gatherings,pattern = "^8$",replacement = "8vo")
estc_selected_variables$gatherings <- gsub(x=estc_selected_variables$gatherings,pattern = "^12$",replacement = "12mo")
estc_selected_variables$gatherings <- gsub(x=estc_selected_variables$gatherings,pattern = "^16$",replacement = "16mo")
estc_selected_variables$gatherings <- gsub(x=estc_selected_variables$gatherings,pattern = "^18$",replacement = "18mo")
estc_selected_variables$gatherings <- gsub(x=estc_selected_variables$gatherings,pattern = "^24$",replacement = "24mo")
estc_selected_variables$gatherings <- gsub(x=estc_selected_variables$gatherings,pattern = "^32$",replacement = "32mo")
estc_selected_variables$gatherings <- gsub(x=estc_selected_variables$gatherings,pattern = "^48$",replacement = "48mo")

### Create the variable sheets and attach real price information to the table, derive price and real price per sheet estimates
#### Note: We discard by default all multi-volume items from the training data due to disambiguities of what the price refers
#### to (volume or edition). However, as this would have led to removal of many of the most expensive editions, prices of multi-volume editions up
#### to the price of 36 pence were handchecked and included. This should not be significantly detrimental to our analyses.
#### Duhaime2019 observed only a modest correlation between volume number and price (2 percent for length normalised price) with a very similar data set derived from the ESTC, and
#### in our case the effect should be significantly smaller, as we control for other indicators of size of the edition like
#### the number of sheets and gatherings. 

estc_selected_variables <- estc_selected_variables %>% left_join(.,format_conversion_table) %>% mutate(.,sheets=sheet_multipliers*pagecount_total/2) %>% left_join(.,monetary_conversions_to_1688_pounds) %>% left_join(.,estc_prices) %>% mutate(.,total_price=ifelse(total_price>=36 | volcount_total<=1,total_price,NA)) %>% mutate(.,total_price=as.numeric(total_price)) %>% mutate(.,price_per_sheet=total_price/sheets) %>% mutate(.,relative_price=total_price*conversion_rate_to_1688_real_wage_value) %>% mutate(.,relative_price_per_sheet=price_per_sheet*conversion_rate_to_1688_real_wage_value) %>% mutate(.,characters_per_sheet=estc_characters_per_page*pagecount_total/sheets/1000)
### Proportion of records accepted into the data set out of all eighteenth century records from the British Isles. 0.946 percent
prop_of_eighteenth_century_records_from_british_isles <- (estc_selected_variables %>% filter(publication_year>=1700  & (publication_country=="England"|publication_country=="Scotland")|publication_country=="Ireland") %>% nrow(.))/(estc_core %>% filter(publication_year>=1700 & (publication_country=="England"|publication_country=="Scotland")|publication_country=="Ireland") %>% nrow(.))
### Proportion of ESTC records missing either country or time information. 0.022 percent
prop_of_time_or_place_mssing <- (estc_core %>% filter(is.na(publication_place)|is.na(publication_year)) %>% nrow(.))/nrow(estc_core)

write.csv(estc_selected_variables,"article_2024_print_culture_and_economic_constraints/data/final/price_analysis_initial_data.csv",row.names=FALSE)

# Some additional sanity checkins: in most instances, our page counts match with the ESTC to a very high degree (less than 5 percent difference in 96.8 percent of instances)
# suggesting that ECCO rarely adds significantly to the number of page estimates.

estc_pagecounts_compared <- left_join((estc_selected_variables %>% distinct(estc_id,pagecount_total)),(load_estc_core() %>% distinct(estc_id,pagecount))) %>% mutate(,page_dif=pagecount-pagecount_total) %>% mutate(.,page_diff_proportion=pagecount/pagecount_total)

strong_agreeance_proportion <- estc_pagecounts_compared %>% filter(page_diff_proportion>=0.95) %>% nrow(.)/nrow(estc_selected_variables)
