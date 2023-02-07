# SER
An R package **SER** for calulating _Short-period Environmental Regime (SER)_

**NOTES**: SER is extremely useful in linking time-series environmental variables to discrete biological responses. Specifically, SER function summaries time-series environmental variables into indices covering different facets i.e., magnitude, frequency, and rate of change of the data in any focused period, which are typically masked by using simple average or median values in conventional way. In total, 11 elementary indices were developed, and users can developed their own environmental regimes by changing the argument **days_bf**. Besides, it is recommended that users carefully read the vignette and check published studies (Guo et al. 2020, 2021; Wu et al. 2022) that used SER to make the most out of their data set with this package.

# Install the package

```R  
# install package devtools if necessary
if(!require(devtools)) install.packages("devtools")

# build_vignettes = T, so the vigenette will be availiable
devtools::install_github("https://github.com/kun-ecology/SER", build_vignettes=	TRUE)
```
# Tutorial
A tutorial (vignette) is availiable for this package, it provides basic information of how short-period environmental regime is developed and how the package can be used with an example. 

To access it:
```R
vignette("SER")
```

We only use hydrological data as an example in the tutorial, but short-period environmental regime is case specific, see the vignette for details. 

There are two datasets embeded in the package: _hydro_df_, a dataframe, containing daily discharge data in a stream; _sample_date_, a vector, containing 13 elements (in date fromate).  

Below is how the main function _SER_ works with the demo data:

`SER(hydro_df$Date, hydro_df$Discharge, sample_date)`

We highly recommend you read through the tutorial and then define your own short-period environmental indices.

# Citation
If you use this package, you can cite it as:

_Kun Guo, Naicheng Wu and Tenna Riis (2019). SER: Short-period Environmental Regime. R package version 0.1._

# Reference
+ Kun Guo, Naicheng Wu, Paraskevi Manolaki, Annette Baattrup-Pedersen, and Tenna Riis. "Short-period hydrological regimes override physico-chemical variables in shaping stream diatom traits, biomass and biofilm community functions." Science of The Total Environment 743 (2020): 140720.

+ Kun Guo, Naicheng Wu, Wei Li, Annette Baattrup-Pedersen, and Tenna Riis. "Microbial biofilm community dynamics in five lowland streams." Science of The Total Environment 798 (2021): 149169.

+ Naicheng Wu, Yaochun Wang, Yixia Wang, Xiuming Sun, Claas Faber, and Nicola Fohrer. "Environment regimes play an important role in structuring trait‐and taxonomy‐based temporal beta diversity of riverine diatoms." Journal of Ecology 110, no. 6 (2022): 1442-1454.

