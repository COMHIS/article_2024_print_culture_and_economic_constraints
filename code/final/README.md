# code/final
The scripts for data analysis and preprocessing of data related to the article. Each script
produces the analyses/figures etc. by running the entire script. The ESTC (and ECCO) data sets required to run many of
the scripts can be accessed for replication purposes by contacting iiro.tiihonen@helsinki.fi

## Supporting scripts

### functions.R
Contains self-defined R functions used in the article. Called in some of the other scripts.

## Preprocessing of data
Code used in the preprocessing of data prior to the analyses conducted in the article.

### data\_initialisation.R
Script used to preprocess ECCO and ESTC data to produce
price\_analysis\_initial\_data.csv, used in the downstream analyses.
Running it requires access to the ESTC and ECCO metadata and the tidyverse library. Included mainly for the sake of transparency, as there is no need to run this script to replicate the results (price\_analysis\_initial\_data.csv is enough).

## Model training and analysis
Scripts used to produce figures, tables and other outputs discussed in
the article

### predictor\_model\_training.R
Script used to train the Extreme Gradient Boosted Decision Tree model discussed in the 
article. It also includes the evaluation of the model and predicting prices for those ESTC records that don't have them. Requires the ESTC data sets in the repository to run, and the R packages ggplot2, gghsci, caret, tidyverse
and xgboost. Produces the predicted\_prices.csv.

### analysis\_of\_predictions.R
Script that produces the analyses of the predicted prices (supply of books in different price segments, comparison to socio-economic data etc.) Requires the ESTC data sets of the repository to run and the R packages tidyverse, ggplot2, gghsci and xtable.   

### price_constraint_and_income_analyses.R
Script that is used to fit and analyse the price constraint model introduced in the article. Only requires the publicly accessible data sets of the repository to run and the R packages XLConnect, tidyverse, gghsci and rstan.

### linear\_model\_prices\_bayesian.R
Script that produces the the Bayesian regression analyses of the article. Requires the ESTC data sets in the repository to run, and the R packages ggplot2, gghsci, tidyverse, rstan and xtable.

### supplementary\_import\_and\_export\_calculations
Produces the analyses related to importing and exporting of books discussed in the appendix of the article. Some analyses require the ESTC
data sets to run.

### type\_label\_evaluation.R
Produces the table about the precision and coverage of the different topic/genre/type labels for ESTC records discussed in the appendix of the article.