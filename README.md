Supporting information for Hasegawa et al.
================
Shun Hasegawa

This repository stores R scripts to reproduce the results presented in the manuscript below. The raw data are available in supporting information (Tables S2-S5).

-   Article title: Elevated CO<sub>2</sub> concentrations reduce C<sub>4</sub> abundance and decrease diversity of understorey plant community in a grassy-*Eucalyptus* woodland
-   Authors: Shun Hasegawa, Juan Piñeiro, Raúl Ochoa-Hueso, Kirk L. Barnett, Anthony M. Haigh, Paul D. Rymer, Sally A. Power
-   Article acceptance date:

File description
----------------

### R scripts

-   **analysis.R**: This loads required packages and data.
-   **analysis\_diversity.R**: This analyses diversity indices for graminoid and forb species
-   **analysis\_abundance.R**: This analyses cover abundance of *Microlaena stipoides* and *Cynodon dactylon* and total of C<sub>3</sub> and C<sub>4</sub> graminoids.
-   **analysis\_LAR.R**: This analyses log annual change ratio (LAR) of C<sub>3</sub> (LAR<sub>C3</sub>) and C<sub>4</sub> (LAR<sub>C4</sub>) using multiple regression with annual soil moisture (Moist), understorey temperature (Temp) and photosynthetic active radiation(PAR).
-   **analysis\_PRC.R**: This performs principal response curve analysis. The structure of permutation for repeated measurements of nested plots is also defined here.

### Data

-   **Table\_S2\_graminoid\_data.csv**: Cover abundance data of graminoid species
-   **Table\_S3\_forb\_data.csv**: Cover abundance data of forb species
-   **Table\_S4\_env\_data.csv**: Environmental variables (soil moisture, temperature and PAR)
-   **Table\_S5\_graminoid\_pfg.csv**: Table of graminoid species and corresponding plant functional types
-   **metadata.csv**: Description of the variables in the data above

Session infomation
------------------

The results were generated under the R session as detailed below.

    ## R version 3.3.2 (2016-10-31)
    ## Platform: x86_64-apple-darwin13.4.0 (64-bit)
    ## Running under: macOS Sierra 10.12.4
    ## 
    ## locale:
    ## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] LMERConvenienceFunctions_2.10 MuMIn_1.15.6                 
    ##  [3] visreg_2.3-0                  lsmeans_2.23                 
    ##  [5] estimability_1.1-1            tidyr_0.6.0                  
    ##  [7] vegan_2.4-1                   lattice_0.20-34              
    ##  [9] permute_0.9-0                 lmerTest_2.0-32              
    ## [11] car_2.1-3                     lme4_1.1-12                  
    ## [13] Matrix_1.2-7.1                ggplot2_2.1.0                
    ## [15] dplyr_0.5.0                   plyr_1.8.4                   
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] rgl_0.96.0          Rcpp_0.12.7         mvtnorm_1.0-5      
    ##  [4] zoo_1.7-13          assertthat_0.1      rprojroot_1.2      
    ##  [7] digest_0.6.9        mime_0.4            R6_2.1.2           
    ## [10] backports_1.0.5     acepack_1.3-3.3     MatrixModels_0.4-1 
    ## [13] stats4_3.3.2        spam_1.4-0          evaluate_0.10      
    ## [16] coda_0.18-1         multcomp_1.4-5      minqa_1.2.4        
    ## [19] data.table_1.9.7    SparseM_1.7         nloptr_1.0.4       
    ## [22] rpart_4.1-10        rmarkdown_1.4       splines_3.3.2      
    ## [25] stringr_1.1.0       foreign_0.8-67      htmlwidgets_0.7    
    ## [28] munsell_0.4.3       shiny_0.13.2        httpuv_1.3.3       
    ## [31] mgcv_1.8-17         htmltools_0.3.5     nnet_7.3-12        
    ## [34] tibble_1.1          gridExtra_2.2.1     Hmisc_3.17-4       
    ## [37] codetools_0.2-15    MASS_7.3-45         grid_3.3.2         
    ## [40] jsonlite_0.9.20     nlme_3.1-128        xtable_1.8-2       
    ## [43] gtable_0.2.0        DBI_0.4-1           magrittr_1.5       
    ## [46] scales_0.4.0        stringi_1.1.1       latticeExtra_0.6-28
    ## [49] sandwich_2.3-4      Formula_1.2-1       TH.data_1.0-7      
    ## [52] RColorBrewer_1.1-2  tools_3.3.2         LCFdata_2.0        
    ## [55] maps_3.1.1          fields_8.10         parallel_3.3.2     
    ## [58] pbkrtest_0.4-6      survival_2.39-5     yaml_2.1.13        
    ## [61] colorspace_1.2-6    cluster_2.0.5       knitr_1.15         
    ## [64] quantreg_5.24
