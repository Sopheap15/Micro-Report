# Load data (Shortcut CTR|Command + A + Enter)----
rm(list = ls())
source("code/Load_data.R",knitr::knit_global())

# Render report
rmarkdown::render("code/Microbiology_Report.Rmd",
                  output_dir = "outputs",
                  output_file = str_glue("{dic$short_name}_Microbiology_Report_{Sys.Date()}.html"), 
                    quiet = T)

# Done
# ដំណើរការបញ្ចាប់ដោយជោគជ័យ

