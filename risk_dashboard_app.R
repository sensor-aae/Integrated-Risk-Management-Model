# Shiny Interactive Dashboard

library(shiny)
library(plotly)
library(corrplot)
library(rugarch)
source("utils/kupiec_test.R")



ui <- fluidPage(
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
               fileInput("actual_file", "Upload CSV of Actual Returns", accept = ".csv")
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
                 numericInput("ead", "Exposure at Default (EAD):", value = 100000, min = 1000, step = 1000),
                 numericInput("n", "Number of Simulations:", value = 10000, min = 1000, step = 1000),
                 actionButton("go_credit", "Simulate Credit Risk"),
                 
               ),
               mainPanel(
                 plotOutput("lossPlot"),
                 verbatimTextOutput("summaryStats")
               )
             )
    ),
    
    tabPanel("Correlation Matrix",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("tickers", "Select Stocks:", 
                                    choices = c("AAPL", "MSFT", "GOOGL", "JPM"),
                                    selected = c("AAPL", "MSFT", "GOOGL", "JPM")),
                 actionButton("go_corr", "Show Correlation")
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
    sim_loss <- rbinom(input$n, 1, input$pd) * input$lgd * input$ead
    output$lossPlot <- renderPlot({
      hist(sim_loss, breaks = 40, col = "steelblue", main = "Simulated Credit Losses", xlab = "Loss")
    })
    output$summaryStats <- renderPrint({
      list(
        `VaR (95%)` = quantile(sim_loss, 0.95),
        `Expected Shortfall` = mean(sim_loss[sim_loss > quantile(sim_loss, 0.95)])
      )
    })
  })
  
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
}

shinyApp(ui = ui, server = server)
