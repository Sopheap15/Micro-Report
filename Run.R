# Load data (Shortcut CTR|Command + A + Enter)----
source("code/Load_data.R",knitr::knit_global())

# Render report
rmarkdown::render("code/Microbiology Report.Rmd",
                    output_file = paste0("Outputs/Microbiology_Report_",{Sys.Date()},".html"), quiet = T)




