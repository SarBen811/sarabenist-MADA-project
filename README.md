# Overview

This repository is for Sara Benist's final project for Modern Applied Data Analysis (EPID 8060E) class.

The template is supplied by Dr. Andreas Handel and can be found in this [Github repository](https://github.com/andreashandel/dataanalysis-template).

# Programs required

This project uses R Studio, Quarto, Github, and Zotero programs.

# Repository structure

-   All data can be found in the subfolders inside the `data` folder.
-   All code can be found in the `code` folder or subfolders.
-   All results (figures, tables, computed values) can be found in `results` folder or subfolders.
-   All products (manuscripts, supplement, presentation slides, web apps, etc.) can be found in `products` subfolders.
-   Each folder contains a `readme.md` file with more information.

# Project content

See the `readme` files in each folder for more details.

-   Raw data can be found in the `raw_data` folder.
-   The `processing_code` folder contains files that load the raw data, mostly cleans the data, and save the result in the `processed_data` folder.
-   The `analysis_code` folder contains files that load the processed data and conduct several exploratory analyses. These files produce figures and some numeric output (tables), which are saved to the `results` folder.
-   The `products` folder contains an example `bibtex` and CSL style files for references (to be updated with project references).
-   The `manuscript` folder contains the project report written as Quarto file. There is also a sub-folder  for any supplementary material file (to be added and updated).
-   The `slides` folder will contain a slide presentation for the project made with Quarto (to be added and updated).
-   The files should be run in this order to complete the analysis: `processingfile.qmd` > `exploratory_analysis.qmd` > `statistical_analysis.qmd`

# Data variables

| Indicator                                    | Inequality Dimensions                                                                                   |
|----------------------------------------------|---------------------------------------------------------------------------------------------------------|
| TB incidence                                 | Sex                                                                                                     |
| TB mortality                                 | Sex                                                                                                     |
| TB prevalence                                | Place of Residence                                                                                      |
| People with MDR/RR-TB                        | Sex                                                                                                     |
| Prevalence to notification ratio             | Sex                                                                                                     |
| Case detection rate                          | Age (2 groups), Sex                                                                                     |
| BCG immunization coverage \<1 year old       | Economic status (wealth quintile), Education (3 groups), Place of Residence, Sex                        |
| Report of TB spread through coughing         | Age groups (3 groups), Economic status (wealth quintile), Education (3 groups), Place of Residence, Sex |
| Desire to keep family member's TB secret     | Age (3 groups), Economic status (wealth qunitile), Education (3 groups), Place of Residence, Sex        |
| Families facing catastrophic costs due to TB | Economic status (wealth quintile), TB drug resistance                                                   |
