# hydradata

Support package for hydra_sim (and eventually hydra_est) model(s).

Details of the original model can be found in:

Gaichas et al. (2017). Combining stock, multispecies, and ecosystem level fishery objectives within an operational management procedure: simulation to start the conversation. ICES 74:2, 552-565. doi: [10.1093/icesjms/fsw119](https://doi.org/10.1093/icesjms/fsw119)

This multispecies model is data intensive. It is coded in ADMB and it requires two data files (.dat and .pin) to run. Both are text files. 

## Features

* Easy manipulation of underlying data
* Easy creation of .dat and .pin files based on user options
* Complete documentation of underlying data in tabular and graphic form

Intended use in conjuction with model running and scenario testing.
Data are lazily loaded with the package

## Usage

Installation

`remotes::install_github("noaa-edab/hydradata", build_vignettes = TRUE)`

## Help
browseVignettes("hydradata")

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
