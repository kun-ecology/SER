# SER
A R package **SER** for calulating _Short-period Environmental Regime (SER)_

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

`SER(hydro_df,sample_date)`

We highly recommend you read through the tutorial and then define your own short-period environmental indices.

**NOTES**
+ This is the first version of the package, there might be some bugs. 

+ In addition, more features are under development.

+ We are more than welcome to your feedbacks/suggestions.


# Citation
If you use this package, you can cite it as:

_Kun Guo, Naicheng Wu and Tenna Riis (2019). SER: Short-period Environmental Regime. R package version 0.0._

