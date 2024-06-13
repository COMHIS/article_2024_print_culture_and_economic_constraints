# data/final/
Data sets used in the article. The ESTC (and ECCO) data sets are available upon request for replication purposes. Please contact iiro.tiihonen@helsinki.fi to access the ESTC and ECCO data sets needed for full replication.

## Other than ESTC/ECCO data sets
Data sets that are not derived from the ESTC and ECCO.

### social_tables_X
Average income per family and number of families from the social tables of King, Massie and Coulquhoun as revised and presented by Allen in the appendix of (Allen 2019).

### allen_2019_income_per_earner_table.csv
The incomes of the six social classes derived from the social tables by Allen. With derivative calculations. Original table in (Allen 2019).

### early\_modern\_budgets.csv
The budget data for early modern households used in the article, gathered from research literature and primary sources described in detail in the appendix of the article and the source column of the table. The table references the sources like they are named in the bibliography file.

### pound_conversions.csv
The table used to convert prices and incomes to the corresponding real wage value of the year 1688. For example, one row maps the value of a pound in the year 1688 to a sum of money in 1712 that has the same real wage/price value, another row does the same for the year 1751 etc. The table is the result of querying the service measuringworth.com. The sum corresponding to the real wage value of a 1688 pound in the target year is split to pounds, shillings and pences in separate columns in the table. Date of accessing the data: 13.9.2022.

### fielding\_and\_robertson\_table\_parsed.csv
The table of early modern book prices used by Fielding and Robertson in (Fielding 2015) and (Fielding 2017), as presented in the appendix of (Fielding 2015), and with prices converted to pences. The prices in the table are added to the price information in the ESTC.  

### readership\_experiences\_by\_class.csv
The number of eighteenth century readership experiences (records) for books/broadhseets/pamphlets and periodicals (in separate columns) for different socio-economic classes, obtained by querying the [Reading Experience Database](http://www.open.ac.uk/Arts/reading/UK/search.php). The databases was queried in 10.5.2024 

### imports_and_exports.csv
Data about books imported to and exported from Britain. Originally from (Barber 1976). 
## ESTC and ECCO Data
Data sets based on the ESTC and ECCO. These data sets are not part of the publicly accessible version of the repo. If you would like to access these data sets for replication purposes, please contact Iiro Tiihonen (iiro.tiihonen@helsinki.fi). 

### price\_analysis\_initial\_data.csv
The processed data set of ESTC records used in article. The variables used in the analyses are:
* **estc\_id** - the identifier of the edition     
* **work\_id** - the identifier of the work (can map to multiple editions) 
* **gatherings** - the format of the edition
* **area** - the area estimate of the edition (size of individual page)
* **publication\_year** - the publication year of the edition
* **publication\_place\_category** - the publication place of the edition. London, Dublin, Edinburgh. other locations in England, Scotland or Ireland grouped together.
* **characters\_per\_sheet** - characters per sheet based on ECCO metadata on number
of characters per documents
* **sheets** - Number of sheets based on format and page count. 
* **pagecount_total** - The number of pages per estc\_id, maximum of ESTC and ECCO page count.
* **special\_page\_proportion** - The proportion of pages with non-standard elements.
Maximum of the proportion of pages with graphical elements based on metadata from ECCO
and the number of plates reported in the ESTC.
 * **main_category** - The genre/topic/type label of the record.
 * **relative\_price\_per\_sheet** - Price per sheet converted to its real wage/price
 value in 1688 pounds.
 
### predicted\_prices.csv
The original and predicted prices for the records in
price\_analysis\_initial\_data.csv. Price (converted to corresponding real
value in 1688) and price on the ordinal scale used in the article. For those editions with known price,
the price comes from the original price data.

### price\_correction\_table.csv
Table of manually corrected prices. Result of manually examining price information and checking it for mistakes (e.g. original price information was only for one volume of an edition.) Either replaces the old price with a new one or replaces it with a missing value. Note field explains the reason for the change.

### top\_publishers.csv
The actor\_id's (identifiers) of the publishers who were involved in the court case against Donaldson as respondents.

### top\_publisher\_editions.csv
The ESTC record id's of the editions that have at least one of the publishers in top\_publishers.csv in their imprint, with the information whether the editions was published before (including 1774) or after 1774.

### estc\_editions.csv
The ESTC records classified to reprints of books (longer than 128 page editions) before (including) and after 1774 and those that don't
belong to either category.

### harmonised_number_of_copies.csv
Information about number of copies per edition printed from the ESTC field 500a and originally from the Bowyer ledgers. Also lists the number of atypical premium copies known.

### genre\_labels.csv
The ESTC records classified to different publication types/categories/topics/genres. Each record has one label. Some categories depend on the length of the publication. Records with an initial category of literature or arts shorter than 2 sheets were merged to category ephemera-entertainment. Records with an initial category of science or education shorter than 2 sheets were merged to the category ephemera-practical.

### estc\_topics\_comprehensive\_sample\_harmonised.csv
A manually evaluated sample of the genre/category/topic/genre information used in the article. Used to analyse the precision of the assigned
types/categories/topics/genres (e.g. how often is a record classified as history actually history).

### estc\_without\_topic\_harmonised.csv
A manually evaluated sample of documents that do not have a type/category/topic/genre in our classification scheme, with
labels assigned by human annotators. Used to evaluate the recall of the the assigned
types/categories/topics/genres (e.g. how much of the records that should belong to history category are we missing from the category).

### editions\_in\_english\_from\_outside\_anglosphere.csv
Collection of ESTC records printed in English but not in the British empire. Used in the analysis if imports and exports.

## Model objects
Statistical models and summaries that are the outputs of analysing the data.

### xgb\_price\_predictor\_with\_ECCO\_variables.RDS
Saved xgboost model used in prediction.

### confusion\_matrix\_1\_vector.RDS
Saved confusion matrix from the evaluation of the xgboost model.

### stan\_model\_linear.RDS
Saved Bayesian model used in the analysis of price variation.

## bibliography

### bibliography.bib
Bibliography file.
