# MS Statistics Final Project

The final project code is available in *Analysis.R*, which reads files from *data/* and *info/*. Below are two options to reproduce the analysis.

## Two Options to Reproduce Analysis

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
    3a. Note: You may need to install packages used in the code.
    3b. Note: This option may not install the same package versions that our team used. 
    
## R Session Info

R and R Studio Version used during the development of our analysis:

- R version 4.3.3 (2024-02-29)
- R Studio Version 2023.12.1+402