
library(shiny)
library(ggplot2)
library(quantmod)

# BDA400 Assignment 6 - Technical Analysis Dashboard
# Data source: Yahoo Finance

get_stock_data <- function(symbol) {
  getSymbols(
    symbol, src = "yahoo",
    from = "2025-01-01",
    auto.assign = FALSE
  )
}

ui <- fluidPage(
  titlePanel("Apple Stock Portfolio Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "symbol", "Stock Symbol:",
        choices = c("AAPL", "MSFT", "GOOG", "TSLA")
      ),
      
      dateRangeInput(
        "date_range", "Select Date Range:",
        start = "2026-05-01",
        end = "2026-10-30"
      ),
      
      selectInput(
        "time_frame", "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly")
      ),
      
      checkboxGroupInput(
        "indicators", "Technical Indicators:",
        choices = c(
          "SMA", "EMA", "RSI", "MACD"
        ),
        selected = c("SMA", "EMA")
      ),
      
      checkboxInput(
        "signals", "Show Trading Signals",
        value = TRUE
      )
    ),
    
    mainPanel(
      h4("Stock Price and Moving Averages"),
      plotOutput("stock_chart", height = "400px"),
      
      conditionalPanel(
        condition = "input.indicators.includes('RSI')",
        h4("Relative Strength Index (RSI)"),
        plotOutput("rsi_chart", height = "230px")
      ),
      
      conditionalPanel(
        condition = "input.indicators.includes('MACD')",
        h4("Moving Average Convergence Divergence"),
        plotOutput("macd_chart", height = "230px")
      ),
      
      h4("Trading Signals"),
      tableOutput("signals_table")
    )
  )
)

server <- function(input, output) {
  
  # Fetch data when the stock symbol changes
  stock_data <- reactive({
    tryCatch(
      get_stock_data(input$symbol),
      error = function(e) {
        validate(need(FALSE, paste(
          "Unable to fetch stock data:", e$message
        )))
      }
    )
  })
  
  # Calculate indicators using historical prices
  daily_data <- reactive({
    raw <- stock_data()
    
    data <- data.frame(
      Date = as.Date(index(raw)),
      Close = as.numeric(Cl(raw))
    )
    
    data$SMA20 <- as.numeric(SMA(data$Close, n = 20))
    data$SMA50 <- as.numeric(SMA(data$Close, n = 50))
    data$EMA20 <- as.numeric(EMA(data$Close, n = 20))
    data$RSI <- as.numeric(RSI(data$Close, n = 14))
    
    macd_values <- MACD(
      data$Close,
      nFast = 12,
      nSlow = 26,
      nSig = 9,
      maType = "EMA",
      percent = FALSE
    )
    
    data$MACD <- as.numeric(macd_values[, 1])
    data$MACDSignal <- as.numeric(macd_values[, 2])
    data$MACDHistogram <- data$MACD - data$MACDSignal
    
    # Signals occur when SMA20 crosses SMA50
    previous_short <- c(NA, head(data$SMA20, -1))
    previous_long <- c(NA, head(data$SMA50, -1))
    
    buy <- data$SMA20 > data$SMA50 &
      previous_short <= previous_long
    
    sell <- data$SMA20 < data$SMA50 &
      previous_short >= previous_long
    
    data$Signal <- ifelse(
      !is.na(buy) & buy, "Buy",
      ifelse(!is.na(sell) & sell, "Sell", "Hold")
    )
    
    data
  })
  
  # Filter dates and select last close per week/month
  filtered_data <- reactive({
    data <- daily_data()
    
    data <- subset(
      data,
      Date >= input$date_range[1] &
        Date <= input$date_range[2]
    )
    
    validate(need(nrow(data) > 0,
                  "No stock data in this date range."))
    
    if (input$time_frame == "Weekly") {
      data$Period <- as.Date(cut(data$Date, "week"))
    } else if (input$time_frame == "Monthly") {
      data$Period <- as.Date(cut(data$Date, "month"))
    } else {
      data$Period <- data$Date
    }
    
    # Retain the last trading day of each period
    data <- data[
      !duplicated(data$Period, fromLast = TRUE),
    ]
    
    data
  })
  
  # Price chart with optional moving averages
  output$stock_chart <- renderPlot({
    data <- filtered_data()
    
    p <- ggplot(data, aes(x = Date, y = Close)) +
      geom_line(color = "steelblue", linewidth = 1) +
      labs(
        title = paste(input$symbol, "Stock Prices"),
        x = "Date", y = "Price (USD)"
      ) +
      theme_minimal()
    
    if ("SMA" %in% input$indicators) {
      p <- p +
        geom_line(
          aes(y = SMA20, color = "SMA 20"),
          linewidth = 0.8, na.rm = TRUE
        ) +
        geom_line(
          aes(y = SMA50, color = "SMA 50"),
          linewidth = 0.8, na.rm = TRUE
        )
    }
    
    if ("EMA" %in% input$indicators) {
      p <- p +
        geom_line(
          aes(y = EMA20, color = "EMA 20"),
          linewidth = 0.8, na.rm = TRUE
        )
    }
    
    if (input$signals) {
      signal_points <- subset(
        data, Signal %in% c("Buy", "Sell")
      )
      
      p <- p +
        geom_point(
          data = signal_points,
          aes(color = Signal),
          size = 3
        ) +
        geom_text(
          data = signal_points,
          aes(label = Signal, color = Signal),
          vjust = -1, check_overlap = TRUE
        )
    }
    
    p + scale_color_manual(
      values = c(
        "SMA 20" = "orange",
        "SMA 50" = "purple",
        "EMA 20" = "darkgreen",
        "Buy" = "green3",
        "Sell" = "red"
      )
    )
  })
  
  # RSI chart
  output$rsi_chart <- renderPlot({
    data <- filtered_data()
    
    ggplot(data, aes(x = Date, y = RSI)) +
      geom_line(color = "purple", linewidth = 1) +
      geom_hline(
        yintercept = c(30, 70),
        linetype = "dashed",
        color = "red"
      ) +
      coord_cartesian(ylim = c(0, 100)) +
      labs(x = "Date", y = "RSI") +
      theme_minimal()
  })
  
  # MACD chart
  output$macd_chart <- renderPlot({
    data <- filtered_data()
    
    ggplot(data, aes(x = Date)) +
      geom_col(
        aes(y = MACDHistogram),
        fill = "gray70"
      ) +
      geom_line(
        aes(y = MACD, color = "MACD"),
        linewidth = 0.9
      ) +
      geom_line(
        aes(y = MACDSignal, color = "Signal"),
        linewidth = 0.9
      ) +
      scale_color_manual(
        values = c(
          "MACD" = "blue",
          "Signal" = "red"
        )
      ) +
      labs(x = "Date", y = "MACD") +
      theme_minimal()
  })
  
  # Recent trading signals
  output$signals_table <- renderTable({
    data <- filtered_data()
    data <- subset(data, Signal != "Hold")
    data$Date <- format(data$Date, "%Y-%m-%d")
    
    if (nrow(data) == 0) {
      return(data.frame(
        Message = "No buy/sell crossovers in the selected period"
      ))
    }
    
    tail(
      data[, c("Date", "Close", "Signal")],
      10
    )
  })
}

shinyApp(ui = ui, server = server)
