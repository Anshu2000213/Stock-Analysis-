library(shiny)
library(shinydashboard)
library(quantmod)
library(plotly)
library(DT)

# For Calculating DataFrame
get_stock_data <- function(stock_name, start_date, end_date) 
{
  stock_data <- quantmod::getSymbols(stock_name, from = start_date, to = end_date, auto.assign = FALSE)
  stock_df <- data.frame(Date = as.Date(index(stock_data)),
                         Open = as.numeric(quantmod::Op(stock_data)),
                         High = as.numeric(quantmod::Hi(stock_data)),
                         Low = as.numeric(quantmod::Lo(stock_data)),
                         Close = as.numeric(quantmod::Cl(stock_data)),
                         Volume = as.numeric(quantmod::Vo(stock_data)),
                         Adjusted = as.numeric(quantmod::Ad(stock_data)))
  
  return(stock_df)
}

#Important Information or Matrix
calculate_metrics <- function(stock_df) 
{
  last_two_rows <- tail(stock_df, n = 2)
  last_closing_price <- last_two_rows[1, "Close"]
  second_last_closing_price <- last_two_rows[2, "Close"]
  last_date <- last_two_rows[1, "Date"]
  
  daily_returns <- diff(stock_df$Close) / lag(stock_df$Close, default = stock_df$Close[1])
  expected_return <- mean(daily_returns)
  volatility <- sd(diff(log(stock_df$Close)))
  
  percentage_return <- ((last_closing_price - second_last_closing_price) / second_last_closing_price) * 100
  
  metrics <- list(last_closing_price = last_closing_price,
                  last_date = last_date,
                  expected_return = expected_return,
                  volatility = volatility,
                  percentage_return = percentage_return)
  
  return(metrics)
}



#moving average
calculate_ma <- function(data, ma_window) 
{
  data$Moving_Average <- stats::filter(data$Close, rep(1/ma_window, ma_window), sides=2)
  return(data)
}

calculate_bb <- function(data, ma_window, sd_multiplier) 
{
  data <- calculate_ma(data, ma_window)
  data$Upper_Band <- data$Moving_Average + sd_multiplier * stats::sd(data$Close)
  data$Lower_Band <- data$Moving_Average - sd_multiplier * stats::sd(data$Close)
  return(data)
}

#  RSI
calculate_rsi <- function(data, rsi_window) 
{
  rsi <- numeric(length(data$Close))
  for (i in seq_along(data$Close)) 
  {
    if (i >= rsi_window) 
    {
      deltas <- diff(data$Close[(i - rsi_window + 1):i])
      gain <- sum(ifelse(deltas > 0, deltas, 0))
      loss <- sum(ifelse(deltas < 0, -deltas, 0))
      
      avg_gain <- gain / rsi_window
      avg_loss <- loss / rsi_window
      
      rs <- avg_gain / avg_loss
      rsi[i] <- 100 - (100 / (1 + rs))
    } 
    else 
    {
      rsi[i] <- NA
    }
  }
  data$RSI <- rsi
  return(data)
}

# MACD
calculate_macd <- function(data, n_fast = 12, n_slow = 26, n_signal = 9) 
{
  ema_fast <- EMA(data$Close, n_fast)
  ema_slow <- EMA(data$Close, n_slow)
  macd <- ema_fast - ema_slow
  signal <- EMA(macd, n_signal)
  histogram <- macd - signal
  
  return(data.frame(Date = data$Date, MACD = macd, Signal = signal, Histogram = histogram))
}


calculate_adl <- function(data, smooth_volume = FALSE, period = 14) 
{
  adl <- rep(0, nrow(data))
  for (i in 2:nrow(data)) 
  {
    if (smooth_volume) 
    {
      vol_sum <- sum(data$Volume[max(1, i - period):i])
    } 
    else 
    {
      vol_sum <- data$Volume[i]
    }
    adl[i] <- adl[i - 1] + ((data$Close[i] - data$Low[i]) - (data$High[i] - data$Close[i])) * vol_sum / (data$High[i] - data$Low[i])
  }
  return(adl)
}


# UI
ui <- dashboardPage(
  dashboardHeader(title = "Stock Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Indicators", tabName = "indi", icon = icon("th")),
      menuItem("History", tabName = "widgets", icon = icon("th")),
      menuItem("Code", tabName = "code", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 6,
                       box(selectInput("symb", label = h3("Select a Stock"), choices = c("ADANIPORTS.NS", "RELIANCE.NS", "TATASTEEL.NS"), selected = "RELIANCE.NS")),
                       box(width = 6,uiOutput('DATEE')),
                       box(title='Important Information', width = 12, height = NULL, uiOutput("closing_and_volatility")),
                       box(title='Graph Type', width = 12, height = NULL, 
                           radioButtons("graph_type", label = "Select Graph Type:",choices = list("Line" = "line", "Area" = "area", "Candlestick" = "candlestick"), selected = "line")),
                ),
                column(width = 6,box(title='Stock Prices', width = 12, height = NULL, plotlyOutput("plot"))
                )
              )
      ),
      tabItem(tabName = "widgets",
              fluidRow(
                box(
                  width = 6,
                  title = "Select Stock",
                  selectInput("symb_search", label = NULL, choices = c("ADANIPORTS.NS", "RELIANCE.NS", "TATASTEEL.NS"), selected = "RELIANCE.NS")
                ),
                box(
                  width = 6,
                  title = "Select Dates",
                  dateInput("start_date", label = "Start Date", value = Sys.Date() - 365),
                  dateInput("end_date", label = "End Date", value = Sys.Date())
                )
              ),
              DTOutput("table_data")
      ),
      tabItem(tabName = 'code',
              box(
                title = "Code Contents",
                solidHeader = TRUE,
                width = 12,
                uiOutput('file_contents')
              ), 
                verbatimTextOutput("Code")
      ),
      tabItem(tabName = 'indi',
              fluidPage(
                titlePanel("Indicator Analyzer"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("stock_name", label = h3("Select a Stock"), 
                                choices = c("ADANIPORTS.NS", "RELIANCE.NS", "TATASTEEL.NS"), selected = "RELIANCE.NS"),
                    dateInput("start_date", "Start Date:", value = Sys.Date() - 365 * 3),
                    dateInput("end_date", "End Date:", value = Sys.Date()),
                    selectInput("indicator", "Select Indicator:", c("Moving Average" = "MA", "Bollinger Bands" = "BB", "Relative Strength Index (RSI)" = "RSI", "MACD" = "MACD", "Accumulation Distribution Line (ADL)" = "ADL")),
                    conditionalPanel(
                      condition = "input.indicator == 'MA'",
                      numericInput("ma_window", "Moving Average Window:", value = 10, min = 1)
                    ),
                    conditionalPanel(
                      condition = "input.indicator == 'BB'",
                      numericInput("moving_average", "Moving Average Window:", value = 20, min = 1),
                      numericInput("sd_multiplier", "Standard Deviation Multiplier (Bollinger Bands):", value = 2, min = 0.1, max = 10)
                    ),
                    conditionalPanel(
                      condition = "input.indicator == 'RSI'",
                      numericInput("rsi_window", "RSI Window:", value = 14, min = 2)
                    ),
                    conditionalPanel(
                      condition = "input.indicator == 'MACD'",
                      numericInput("n_fast", "MACD Fast EMA Period:", value = 12, min = 1),
                      numericInput("n_slow", "MACD Slow EMA Period:", value = 26, min = 1),
                      numericInput("n_signal", "MACD Signal Line Period:", value = 9, min = 1)
                    ),
                    conditionalPanel(
                      condition = "input.indicator == 'ADL'",
                      checkboxInput("smooth_volume", "Smooth Volume? (ADL)", value = FALSE),
                      numericInput("adl_period", "ADL Period:", value = 14, min = 1)
                    )
                  ),
                  mainPanel(
                    plotlyOutput("indicator_plot")
                  )
                )
              )
      )
              
      )
    )
  
)

# Server
server <- function(input, output, session) 
{
  #DATA FRAME
  dataInput1 <- reactive({
    symbol <- input$symb
    end_date <- Sys.Date()
    start_date <- end_date - 365 * 3
    stock_data <- get_stock_data(symbol, start_date, end_date)
    stock_data <- na.omit(stock_data)
    stock_data
  }) 
 
  #Important-INfo
  output$closing_and_volatility <- renderUI({
    stock_data <- dataInput1()
    metrics <- calculate_metrics(stock_data)
    result <- HTML(
      paste(
        "Closing Price: ", "<span style='color:red;'>", round(metrics$last_closing_price, 4), "</span>", " <br>",
        "Previous Day Return: ", "<span style='color:red;'>", round(metrics$percentage_return, 4), "</span>", " <br>",
        "Volatility: ", "<span style='color:red;'>", round(metrics$volatility, 4), "</span>", " <br>",
        "Expected Return: ", "<span style='color:red;'>", round(metrics$expected_return, 4), "</span>", " <br>"
      )
    )
    result
  })
  
  #LAst date
  output$DATEE <- renderUI({
    stock_data <- dataInput()
    metrics <- calculate_metrics(stock_data)
    result <- HTML(paste("Latest Date: ", metrics$last_date))
    result
  })
  
  #LINE,AREA,CANDLESTICK plot
  output$plot <- renderPlotly({
    stock_data <- dataInput1()
    
    if (input$graph_type == "line") {
      p <- plot_ly(stock_data, x = ~Date, y = ~Close, type = "scatter", mode = "lines")
    } else if (input$graph_type == "area") {
      p <- plot_ly(stock_data, x = ~Date, y = ~Close, type = "scatter", mode = "lines", fill = "tozeroy")
    } else if (input$graph_type == "candlestick") {
      p <- plot_ly(stock_data, x = ~Date) %>%
        add_trace(type = "candlestick",
                  open = ~Open, high = ~High,
                  low = ~Low, close = ~Close)
    }
    
    p <- p %>% layout(title = "Stock Prices")
    
    p <- p %>% layout(xaxis = list(rangeslider = list(visible = TRUE)))
    
    p
  })
  
  #For History data section
  dataInput <- reactive({
    symbol <- input$symb_search
    start_date <- input$start_date
    end_date <- input$end_date
    stock_data <- get_stock_data(symbol, start_date, end_date)
    stock_data <- na.omit(stock_data)
    stock_data
  }) 
  

  
  output$table_data <- renderDT({
    return(dataInput())
  })
  
  
  #For Code printing
  output$file_contents <- renderUI({
    file_path <- "project3.R"
    if (file.exists(file_path)) 
    {
      file_text <- readLines(file_path)
      return(tags$pre(HTML(paste(file_text, collapse = "\n"))))
    } 
    else 
    {
      return("File not found.")
    }
  })
  

  #For Indicators 
  stock_data <- reactive({
    get_stock_data(input$stock_name, input$start_date, input$end_date)
  })
  

  output$indicator_plot <- renderPlotly({
    df <- stock_data()
    
    # selected indicator
    if (input$indicator == "MA") {
      df <- calculate_ma(df, input$ma_window)
      p <- plot_ly(df, x = ~Date) %>%
        add_lines(y = ~Close, name = "Stock Price") %>%
        add_lines(y = ~Moving_Average, name = "Moving Average") %>%
        layout(title = "Moving Average Analysis",
               yaxis = list(title = "Price"),
               legend = list(title = "Legend"))
    } 
    else if (input$indicator == "BB") 
      {
      # Calculate moving average
      ma_window <- input$moving_average
      df$Moving_Average <- stats::filter(df$Close, rep(1/ma_window, ma_window), sides=2)
      
      # Calculate standard deviation
      sd_multiplier <- input$sd_multiplier
      df$Upper_Band <- df$Moving_Average + sd_multiplier * stats::sd(df$Close)
      df$Lower_Band <- df$Moving_Average - sd_multiplier * stats::sd(df$Close)
      
   
      p <- plot_ly(df, x = ~Date, y = ~Close) %>%
        add_lines() %>%
        add_lines(y = ~Moving_Average, name = "Moving Average", color = I("blue")) %>%
        add_ribbons(ymin = ~Lower_Band, ymax = ~Upper_Band, fill = "grey", alpha = 0.5) %>%
        layout(title = "Bollinger Bands",
               yaxis = list(title = "Price"))
    } 
    else if (input$indicator == "RSI") 
      {
      df <- calculate_rsi(df, input$rsi_window)
      p <- plot_ly(df, x = ~Date, y = ~RSI) %>%
        add_lines(color = I("blue")) %>%
        layout(title = "Relative Strength Index (RSI) Analysis",
               yaxis = list(title = "RSI"))
    } 
    else if (input$indicator == "MACD") 
    {
      df <- calculate_macd(df, input$n_fast, input$n_slow, input$n_signal)
      p <- plot_ly(df, x = ~Date) %>%
        add_lines(y = ~MACD, name = "MACD") %>%
        add_lines(y = ~Signal, name = "Signal") %>%
        add_bars(y = ~Histogram, name = "Histogram", marker = list(color = "gray")) %>%
        layout(title = "MACD Analysis",
               yaxis = list(title = "Value"),
               legend = list(orientation = "h"))
    } 
    else if (input$indicator == "ADL") 
    {
      adl <- calculate_adl(df, input$smooth_volume, input$adl_period)
      df$ADL <- adl
      p <- plot_ly(df, x = ~Date) %>%
        add_lines(y = ~Close, name = "Close", line = list(color = "black")) %>%
        add_lines(y = ~ADL, name = "ADL", line = list(color = "blue")) %>%
        layout(title = "Accumulation Distribution Line (ADL) Analysis",
               yaxis = list(title = "Price", zeroline = FALSE),
               xaxis = list(title = "Date"),
               legend = list(orientation = "h"))
    }
    
    p <- p %>% layout(xaxis = list(rangeslider = list(visible = TRUE)))
    
    return(p)
  })
  
}

shinyApp(ui, server)

