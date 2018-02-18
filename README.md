EucFACE\_understorey\_vegetation: accepted by Journal of Ecology
================
Shun Hasegawa

This repository stores R scripts to reproduce the results presented in the manuscript below. The raw data are publicly available (<https://doi.org/10.6084/m9.figshare.5835216.v3>).

-   Article title: Elevated CO<sub>2</sub> concentrations reduce C<sub>4</sub> cover and decrease diversity of understorey plant community in a *Eucalyptus* woodland
-   Authors: Shun Hasegawa, Juan Piñeiro, Raúl Ochoa-Hueso, Kirk L. Barnett, Anthony M. Haigh, Paul D. Rymer, Sally A. Power
-   Article acceptance date: 26<sup>th</sup> January 2018

File description
----------------

### R scripts

-   **analysis.R**: This loads required packages and data.
-   **analysis\_diversity.R**: This analyses diversity indices for graminoid and forb species
-   **analysis\_cover.R**: This analyses cover of dominant and subordinate C<sub>4</sub> and C<sub>3</sub> graminoids, C<sub>4</sub>:C<sub>3</sub> ratios and subordinate:dominant ratios.
-   **analysis\_C43r\_soilNP.R**: This analyses relationship between C<sub>4</sub>:C<sub>3</sub> ratios soil N and P availability using multiple regression analysis with annual soil moisture (Moist), understorey temperature (Temp) and photosynthetic active radiation(PAR).

### Data

-   **FACE\_P0029\_RA\_UCOMGM\_R\_201209-201602.csv**: Cover data of graminoid species
-   **FACE\_P0029\_RA\_UCOMFB\_R\_201209-201602.csv**: Cover data of forb species
-   **FACE\_P0029\_RA\_ENVVARS\_L1\_201301-201512.csv**: Environmental variables (soil moisture, temperature and PAR)
-   **FACE\_P0029\_RA\_SOILVARS\_L1\_201311-201602.csv**: Plant accessible nutrients (ammonium, nitrate and phosphate)
-   **graminoid\_pfg.csv**: Table of graminoid species and corresponding plant functional types
-   **metadata.csv**: Description of the variables in the data above

Session infomation
------------------

The results were generated under the R session as detailed below.

    ## R version 3.3.3 (2017-03-06)
    ## Platform: x86_64-apple-darwin13.4.0 (64-bit)
    ## Running under: macOS  10.13.3
    ## 
    ## locale:
    ## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] MuMIn_1.40.0    visreg_2.4-1    lsmeans_2.27-61 tidyr_0.6.0    
    ##  [5] vegan_2.4-4     lattice_0.20-34 permute_0.9-0   lmerTest_2.0-33
    ##  [9] car_2.1-5       lme4_1.1-14     Matrix_1.2-8    ggplot2_2.1.0  
    ## [13] dplyr_0.5.0     plyr_1.8.4     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_0.12.13        mvtnorm_1.0-5       zoo_1.7-13         
    ##  [4] assertthat_0.2.0    rprojroot_1.2       digest_0.6.12      
    ##  [7] R6_2.2.2            backports_1.0.5     acepack_1.3-3.3    
    ## [10] MatrixModels_0.4-1  stats4_3.3.3        evaluate_0.10      
    ## [13] coda_0.18-1         rlang_0.1.4         multcomp_1.4-5     
    ## [16] minqa_1.2.4         data.table_1.10.4-3 SparseM_1.7        
    ## [19] nloptr_1.0.4        rpart_4.1-10        rmarkdown_1.6      
    ## [22] splines_3.3.3       stringr_1.2.0       foreign_0.8-67     
    ## [25] munsell_0.4.3       mgcv_1.8-17         htmltools_0.3.5    
    ## [28] nnet_7.3-12         tibble_1.3.4        gridExtra_2.2.1    
    ## [31] Hmisc_3.17-4        codetools_0.2-15    MASS_7.3-45        
    ## [34] grid_3.3.3          nlme_3.1-131        xtable_1.8-2       
    ## [37] gtable_0.2.0        DBI_0.7             magrittr_1.5       
    ## [40] scales_0.5.0        estimability_1.1-1  stringi_1.1.5      
    ## [43] latticeExtra_0.6-28 sandwich_2.3-4      Formula_1.2-1      
    ## [46] TH.data_1.0-7       RColorBrewer_1.1-2  tools_3.3.3        
    ## [49] parallel_3.3.3      pbkrtest_0.4-7      survival_2.40-1    
    ## [52] yaml_2.1.13         colorspace_1.2-6    cluster_2.0.5      
    ## [55] knitr_1.15          quantreg_5.24
