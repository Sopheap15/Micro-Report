# Load data (Shortcut CTR|Command + A + Enter)----
setwd(getwd())
rm(list = ls())
source("code/Load_data.R",knitr::knit_global())

# Render report
rmarkdown::render("code/Microbiology Report.Rmd",
                    output_file = str_glue("outputs/{dic$short_name}_Microbiology_Report_{Sys.Date()}.html"), quiet = T)



