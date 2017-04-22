Supporting information for Hasegawa et al.
================
Shun Hasegawa

This repository stores R scripts to reproduce the result presented in the manuscript below.

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

-   **graminoid\_data.csv**: Cover abundance data of graminoid species
-   **forb\_data.csv**: Cover abundance data of forb species
-   **env\_data.csv**: Environmental variables (soil moisture, temperature and PAR)
-   **graminoid\_pfg.csv**: Table of graminoid species and corresponding plant functional types
-   **MetaData.csv**: Description of the variables in the data above
