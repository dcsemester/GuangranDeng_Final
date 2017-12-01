Project purpose: to identify the relationships between resumption of cyclicity after calving and its potential factors

To rerun script that generate clean data files, figures, and analysis, install necessary R packages and run “GuangranDeng_Final.R” in script folder

R packages:
car
MASS
ggplot2
dplyr

Files in order to use:
1. raw_data/final_raw.csv - raw data file with reponse variable and explanatory variables
2. raw_data/metadata.docx - file describing final_raw.csv

3. script/GuangranDeng_Final.R - code to clean up raw data, produce one cleaned data file, one prediction result data file, and two figures
4. clean_data/final_clean.csv - reponse variable CLA is converted from character to numeric data type 0 and 1, spaces in DISEASE variable are removed, then DISEASE variable is converted to factor data type 
5. clean_data/final_predictions.csv - final analysis result, prediction values with compared to observation values

6. figures/High Leverages Figure.jpg - figure produced from analysis GuangranDeng_Final.R script
7. figures/Influential Points Figure.jpg - figure produced from analysis GuangranDeng_Final.R script

8. documents/manuscript.docx - draft of manuscript describing project and findings