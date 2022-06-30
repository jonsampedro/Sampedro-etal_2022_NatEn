[![DOI](https://zenodo.org/badge/508804988.svg)](https://zenodo.org/badge/latestdoi/508804988)

## Sampedro-etal_2022_NatEn

**Residential energy demand, emissions, and expenditures at region and income-decile level for alternative SSP scenarios**

Jon Sampedro<sup>1\*</sup>, Stephanie Waldhoff<sup>1</sup>, Jae Edmonds<sup>1</sup>, Gokul Iyer<sup>1</sup>, Siwa Msangi<sup>1</sup>, Kanishka Narayan<sup>1</sup>, Pralit Patel<sup>1</sup>, and Marshall Wise<sup>1</sup>, 

<sup>1 </sup> Joint Global Change Research Institute, Pacific Northwest National Laboratory, College Park, United States of America

\* corresponding author:  jon.sampedro@pnnl.gov

## Abstract
The evolution of residential energy demand will have several multisector and multiscale implications in terms of energy access, emissions, and expenditures. Using an enhanced version of the Global Change Analysis Model (GCAM), we find that, residential energy service demand increases over time as per capita income and floorspace rises, with large differences across scenarios, regions and income groups. In all regions, differences between lower and upper deciles decrease over time, particularly for thermal services, as richer consumers get closer to their energy comfort levels. 
However, we show that current within-region inequalities in energy access can persist over time if no additional mechanisms are implemented. In terms of emissions, lower deciles will be more impacted by household air pollution, while their contribution to direct GHG emissions is smaller. We also find that the share of income dedicated to energy is higher for lower deciles, with some regional differences.  

## Code reference
Available at Zenodo: Add Zenodo Repo release (Link + citation)

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