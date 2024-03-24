# Installation

``` r{}
install.packages(c("lime", "MBMethPred", "gridExtra")
```

# Loading packages
``` r{}
require(caret)
require(lime)
require(MBMethPred)
require(ggplot2)
require(gridExtra)
require(caTools)
```
# Loading your data
``` r{}
load("Data1.RData")
```
## Running the function
``` r{}
explainer(Data = Data1, 
          SplitRatio = 0.8, 
          CV = 5, # Cross validation folds
          N_test = 10, # number of test set to explain
          Top = 20) # Top features
```
