# report.

library(rmarkdown)

# Render the RMarkdown file to PDF
rmarkdown::render("risk_model_report.Rmd",
                  output_format = "pdf_document",
                  output_file = "risk_model_report.pdf",
                  output_dir = "reports/",
                  clean = TRUE)

