This folder contains code to load, clean, process, and analyze my data.

The `processing_code` folder contains R script and Quarto files to load and clean the data. Render the `processingfile.qmd` Quarto file in this folder first. This pulls coding script from the `processingcode.r` script. 

The `analysis_code` folder contains R script and Quarto files to load, explore, and analyze the processed data. Render the `exploratory_analysis.qmd` Quarto file after running `processingfile.qmd`. Render the `statistical_analysis.qmd` Quarto file after running the `processingfile.qmd`. Render the `boosted_statistical_analysis.qmd` Quarto file after running the `statistical_analysis.qmd`. Re