# report.

# Create output directory if it doesn't exist
if (!dir.exists("reports")) {
  dir.create("reports")
}

# Load required packages
required_packages <- c("rmarkdown", "quantmod", "PerformanceAnalytics", "MASS", "kableExtra", "ggplot2", "corrplot")
new_pkgs <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs)
lapply(required_packages, require, character.only = TRUE)

# Render Market Risk Report
rmarkdown::render(
  input = "reports_src/market_risk_report.Rmd",
  output_format = "pdf_document",
  output_file = "market_risk_report.pdf",
  output_dir = "reports",
  clean = TRUE
)

# Render Credit Risk Report
rmarkdown::render(
  input = "reports_src/credit_risk_report.Rmd",
  output_format = "pdf_document",
  output_file = "credit_risk_report.pdf",
  output_dir = "reports",
  clean = TRUE
)

# Render Correlation Report
rmarkdown::render(
  input = "reports_src/correlation_report.Rmd",
  output_format = "pdf_document",
  output_file = "correlation_report.pdf",
  output_dir = "reports",
  clean = TRUE
)

cat("\n✅ All reports successfully generated in /reports\n")

