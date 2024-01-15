[![DOI](https://zenodo.org/badge/508804988.svg)](https://zenodo.org/badge/latestdoi/508804988)

## Sampedro-etal_ERL

**Residential energy demand, emissions, and expenditures at regional and income-decile level for alternative futures**

Jon Sampedro<sup>1</sup>, Stephanie Waldhoff<sup>2\*</sup>, Jae Edmonds<sup>2</sup>, Gokul Iyer<sup>2</sup>, Siwa Msangi<sup>3</sup>, Kanishka Narayan<sup>2</sup>, Pralit Patel<sup>2</sup>, and Marshall Wise<sup>2</sup>, 

<sup>1 </sup> Basque Center for Climate Change, Leioa, Spain

<sup>2 </sup> Joint Global Change Research Institute, Pacific Northwest National Laboratory, College Park, United States of America

<sup>3 </sup> U.S. Department of Agriculture, Economic Research Service, Washington D.C., United States of America

\* corresponding author:  Stephanie.Waldhoff@pnnl.gov

## Abstract
Income and its distribution profile are important determinants of residential energy demand and carry direct implications for human well-being and climate. We explore the sensitivity of residential energy systems to income growth and distribution across SSP-RCP scenarios using a global, integrated, multisector dynamics model, GCAM, which tracks national/regional household energy services and fuel choice by income decile. Nation/region energy use patterns across deciles tend to converge over time with aggregate income growth, as higher-income consumers approach satiation levels in floorspace and energy services. However, in some regions, existing within-region inequalities in energy consumption persist over time due to slow income growth in lower income groups. Due to continued differences in fuel types, lower income groups will have higher exposure to household air pollution, despite lower contributions to greenhouse gas emissions. We also find that the share of income dedicated to energy is higher for lower deciles, with strong regional differences.

## Code reference
Available at Zenodo: [https://zenodo.org/record/6780818#.YrzrFXbMKUk](https://zenodo.org/record/8116637)(https://zenodo.org/record/8116637)

jonsampedro. (2023). jonsampedro/Sampedro-etal_OneEarth: v-1.2.0 (v-1.2.0). Zenodo. https://doi.org/10.5281/zenodo.8116637

## Data reference
Available at Zenodo: https://zenodo.org/record/6780424#.YrznmXbMKUk

Jon Sampedro, Stephanie Waldhoff, Jae Edmonds, Gokul Iyer, Siwa Msangi, Kanishka Narayan, Pralit Patel, & Marshall Wise. (2022). Input data for Sampedro et al [Data set]. Zenodo. https://doi.org/10.5281/zenodo.6780424

## Contributing modeling software
| Model | Version | Repository Link 
|-------|---------|-----------------
| Global Change Analysis Model (GCAM) | Enhanced version of v-6.0| https://github.com/jonsampedro/gcam-core | 

| Component| Version | Repository Link 
|-------|---------|-----------------
| gcamdata | 1.0| https://github.com/JGCRI/gcamdata | 
| rgcam | 1.2.0| https://github.com/JGCRI/rgcam | 
| rmap| 1.0.0| https://github.com/JGCRI/rmap | 
| jgcricolors| 1.0.0| https://github.com/JGCRI/jgcricolors| 

## Reproduce my experiment
To reproduce the results and figures shown in Sampedro et al.,

1. Install `R` here - https://www.r-project.org/
2. Install `R studio` from here - https://www.rstudio.com/
3. Download input data from Zenodo (https://zenodo.org/record/6780424#.YrznmXbMKUk) and place it in the corresponding subfolders in folder `workflow` (keep the same structure)
4. Run the script called `Results_processing` chunk by chunk to generate the figures.  

Note that figures are generated for a suite of representative regions. The user could easily genearte figures for any of the 32 GCAM regions (Figure 2, https://github.com/JGCRI/gcam-doc/blob/gh-pages/overview.md)
Similarly the user can easily change some additional settings (e.g., gases or palettes) in the first lines of the script (Lines 30-40)
