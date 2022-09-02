# Load data (Shortcut CTR|Command + A + Enter)----
rm(list = ls())
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
source("code/Load_data.R",knitr::knit_global())

# Render report
rmarkdown::render("code/Microbiology_Report.Rmd",
                    output_file = str_glue("outputs/{dic$short_name}_Microbiology_Report_{Sys.Date()}.html"), quiet = T)



