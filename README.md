# MS Statistics Final Project

The final project code is available in *Analysis.R*, which reads files from *data/* and *info/*. Below are two options to reproduce the analysis.

### Option 1. Clone Git Repository

1. Clone Git Repository
2. Open *Project.Rproj* to launch the project in an RStudio session.
3. Install `renv` package: `install.packages("renv")`. Version 1.0.2 or higher may be required.
4. Execute the command `renv::restore()`. This will install the same package versions used by our team during the development of our analysis.
5. Run `Analysis.R`

### Option 2. Download Essential Files Only

1. Open R Studio and create a new project.
2. Download *Analysis.R*, *data/*, and *info/* into the folder of your new R Project.
3. Run *Analysis.R*

*Note: You may need to install packages used in the code. However, this option may not install the exact package versions utilized by our team, unlike option #1.*
 
## R Session Info

R and R Studio Version used during the development of our analysis:

- R version 4.3.3 (2024-02-29)
- R Studio Version 2023.12.1+402

## Note

*Analysis.R* is a cleaned-up version of our Quarto Markdown file responsible for generating the PDF report. The Quarto Markdown file is not provided here for two reasons: 1) it is messy and considerably longer, and 2) it relies on the [quarto titlepages](https://nmfs-opensci.github.io/quarto_titlepages/) extension, which might necessitate an extra step to reproduce the analysis. Hence, *Analysis.R* was created for simplicity.

Please reach out to our team if you require the Quarto Markdown file.