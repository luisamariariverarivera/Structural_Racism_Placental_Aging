# Structural_Racism_Placental_Aging


**Making Index of Concentration at the Extremes Variables for Shelby County**
The script **ICE_Processing.R** uses the tidycensus package to download data from the American Community Survey and then make the ICE variables as per Krieger et al 2016. The ACS data I used were the 2014 5 year estimates, to generally approximate the period in which CANDLE mothers were pregnant.

The script also contains code to make a Shelby County-wide map of ICE variables.


**Matching ICE to the CANDLE dataset**

The Shelby ICE variables created above are indexed to GEOID at the census tract level. In order to protect participant identities, I sent this file to Maureen Sorrells, who then used participant address data to match ICD variables to study participants and remove the GEOID. This file, along with the other variables I requested for my analysis are under data_raw/mapp.csv.

**Joining the raw data***

The **Processing.R** script links the various raw data files 
