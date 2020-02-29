# Overlooked Benefits of Nutrient Reductions in the Mississippi River Basin


This repo contains data and code for reproducing Parthum and Ando (2020), "[Overlooked Benefits of Nutrient Reductions in the Mississippi River Basin](http://dx.doi.org/)". 

> **Abstract:** Improvements in local surface water quality in the Mississippi River Basin (MRB) can contribute to the regional environmental goals of reducing hypoxia in the Gulf of Mexico. To inform estimates of the benefits of water quality policy, we use a choice experiment survey in a typical sub-watershed of the MRB to estimate willingness to pay for local environmental improvements and helping to reduce hypoxia far downstream. We find that residents place large values on reduced local algal blooms, improved local fish populations and diversity, and meeting local commitments to help with the regional environmental problem. 

Click on the "fork" button at the very top right of the page to create an independent copy of the repo within your own GitHub account. Alternatively, click on the green "clone or download" button just below that to download the repo to your local computer. You will need to set the working directory of the `.do` and `.R` files to the location of your download (this is noted at the top of each script). 

The main file for conducting the analysis is `1_analyze_wtpspace.do`. This will provide the primary findings in the paper. Each file is stand-alone and should be self-explanatory.

## Requirements

The analysis is written in *R*. Data is stored as *.rds*.

*R* is free, open-source and available for download [here](https://www.r-project.org/).

## Performance

The analyses in this paper involves many computationally intensive random parameter logit estimations. The computation time for each regression ranges from 5 to 45 minutes using 4 cores with 64GB RAM.

## Problems

If any errors are discovered, please inform the owner of this repository at bparthum@gmail.com. Thank you, and enjoy!

## License

The software code contained within this repository is made available under the [MIT license](http://opensource.org/licenses/mit-license.php). The data and figures are made available under the [Creative Commons Attribution 4.0](https://creativecommons.org/licenses/by/4.0/) license.
