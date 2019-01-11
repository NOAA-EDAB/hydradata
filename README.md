# mshydradata

Support package for MS_Hydra model.

Details of the original model can be found in:

Gaichas et al. (2017). Combining stock, multispecies, and ecosystem level fishery objectives within an operational management procedure: simulation to start the conversation. ICES 74:2, 552-565. doi:10.1093/icesjms/fsw119

This multispecies model is data intensive. It is coded in ADMB and it requires two data files (.dat and .pin) to run. Both are text files. 

## Features

* Easy manipulation of underlying data
* Easy creation of .dat and .pin files based on user options
* Complete documentation of underlying data in tabular and graphic form

Intended use in conjuction with model running and scenario testing.
Data are lazily loaded with the package

## Usage

Installation
devtools::install_github("andybeet/mshydradata",build_vignettes = TRUE)

If you don't have devtools installed you will see an error "there is no package called 'devtools'"; if that happens install devtools with install.packages("devtools").

## Help
browseVignettes("mshydradata")

