# Shiny Interactive Dashboard

library(shiny)
library(plotly)
library(corrplot)
library(rugarch)
source("utils/kupiec_test.R")





ui <- fluidPage(
  tags$head(tags$base(target = "_blank")),
  titlePanel("Interactive Risk Simulator"),
  tabsetPanel(
  tabPanel("Market Risk Simulator",
           sidebarLayout(
             sidebarPanel(
               sliderInput("volatility", "Annualized Volatility:", min = 0.01, max = 0.5, value = 0.2, step = 0.01),
               numericInput("mu", "Expected Return:", value = 0.0005),
               numericInput("sim_days", "Days:", value = 252, min = 30),
               numericInput("sim_mc", "Simulations:", value = 10000, min = 1000),
               actionButton("go_market", "Simulate Market Risk"),
               fileInput("actual_file", "Upload CSV of Actual Returns", accept = ".csv"),
               downloadButton("download_market", "Download Market Risk Report"),
              
             ),
             mainPanel(
               plotlyOutput("lossPlot_market"),
               verbatimTextOutput("summaryStats_market")
             )
           )
      
  ),
  
  tabPanel("Credit Risk Simulator",
           sidebarLayout(
             sidebarPanel(
               sliderInput("pd", "Probability of Default (PD):", min = 0.01, max = 0.5, value = 0.05, step = 0.01),
               sliderInput("lgd", "Loss Given Default (LGD):", min = 0.1, max = 1, value = 0.6, step = 0.05),
               numericInput("ead", "Exposure at Default (EAD):", value = 100000),
               numericInput("numObligors", "Number of Obligors per Simulation:", value = 100),
               numericInput("numCreditSim", "Number of Simulations:", value = 10000),
               actionButton("go_credit", "Simulate Credit Risk"),
               downloadButton("download_credit", "Download Credit Risk Report")
             ),
             mainPanel(
               plotOutput("credit_plot"),
               verbatimTextOutput("credit_summary")
             )
           )
  ),
    
    tabPanel("Correlation Matrix",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("tickers", "Select Stocks:", 
                                    choices = c("AAPL", "MSFT", "GOOGL", "JPM"),
                                    selected = c("AAPL", "MSFT", "GOOGL", "JPM")),
                 actionButton("go_corr", "Show Correlation"),
                 downloadButton("download_correlation", "Download Correlation Analysis")
               ),
               mainPanel(
                 plotOutput("corrPlot")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # CREDIT RISK
  observeEvent(input$go_credit, {
    req(input$pd, input$lgd, input$ead, input$numObligors, input$numCreditSim)
    
    set.seed(123)
    pd <- input$pd
    lgd <- input$lgd
    ead <- input$ead
    n_sim <- input$numCreditSim
    n_obl <- input$numObligors
    
    default_matrix <- matrix(rbinom(n_sim * n_obl, 1, pd), nrow = n_sim)
    loss_per_default <- lgd * ead
    total_losses <- rowSums(default_matrix) * loss_per_default
    
    # Plot
    output$credit_plot <- renderPlot({
      hist(total_losses, breaks = 50, col = "steelblue",
           main = "Portfolio-Level Credit Losses",
           xlab = "Total Portfolio Loss ($)")
    })
    })
  
  # Validation Summary
  output$download_credit <- downloadHandler(
    filename = function() {
      paste0("credit_risk_report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "credit_risk_report.Rmd")
      file.copy("reports_src/credit_risk_report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        pd = input$pd,
        lgd = input$lgd,
        ead = input$ead,
        n_sim = input$numCreditSim,
        n_obl = input$numObligors
      )
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
  
  # MARKET RISK
 observeEvent(input$go_market, {
  set.seed(123)
   sim_returns <- rt(input$sim_mc, df = 3) * (input$volatility / sqrt(252)) + input$mu
   
  output$lossPlot_market <- renderPlotly({
    plot_ly(x = ~sim_returns, type = "histogram", marker = list(color = 'darkgreen')) %>%
      layout(title = "Simulated Daily Market Returns", 
             xaxis = list(title = "Return"), 
             yaxis = list(title = "Frequency"))
    
})
  
  var_95 <- quantile(sim_returns, 0.05)
  es_95 <- mean(sim_returns[sim_returns < var_95])
  
  output$summaryStats_market <- renderPrint({
    list(
      `VaR (95%)` = var_95,
      `Expected Shortfall` = es_95
    )
  })
  
  # Kupiec Test
  observeEvent(input$actual_file, {
    actual_data <- read.csv(input$actual_file$datapath)
    actual_returns <- actual_data[[1]]
    test_result <- kupiec_test(actual_returns, rep(abs(var_95), length(actual_returns)), alpha = 0.05)
    
    showModal(modalDialog(
      title = "Kupiec VaR Backtest",
      paste("Breaches:", test_result$breaches),
      paste("LR_pof:", test_result$LR_pof),
      paste("p-value:", test_result$p_value),
      paste("Result:", test_result$result)
    ))
  })
  
})
 
 output$download_market <- downloadHandler(
   filename = function() {
     paste("market_risk_report_", Sys.Date(), ".pdf", sep = "")
   },
   content = function(file) {
     rmarkdown::render("reports_src/market_risk_report.Rmd",
                       output_file = file,
                       output_format = "pdf_document")
   }
 )

 
 
  # CORRELATION MATRIX
  observeEvent(input$go_corr, {
    req(input$tickers)
    getSymbols(input$tickers, from = "2022-01-01", auto.assign = TRUE)
    prices <- do.call(merge, lapply(input$tickers, function(t)Cl(get(t))))
    returns <- na.omit(Return.calculate(prices))
    corr_matrix <- cor(returns)
    output$corrPlot <- renderPlot({
      corrplot(corr_matrix, method = "color", type = "upper", 
               tl.col = "black", addCoef.col = "black")
    })
  })
  
  output$download_correlation <- downloadHandler(
    filename = function() {
      paste("correlation_risk_report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      rmarkdown::render("reports_src/correlation_risk_report.Rmd",
                        output_file = file,
                        output_format = "pdf_document")
    }
  )
}

shinyApp(ui = ui, server = server)
