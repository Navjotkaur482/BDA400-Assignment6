##############################################################################
# BDA400 - Assignment 6
# Technical Analysis using R - Visualization Phase
#
# Portfolio Visualization Dashboard (R Shiny)
# Fetches stock data from Yahoo Finance, visualizes it with multiple chart
# types, overlays technical indicators (Moving Averages, RSI, MACD), and
# implements a Moving-Average-Crossover trading rule with chart annotations.
#
# NOTE ON STYLE: Per submission requirements, ALL charts use black & white
# only. Series/signals are distinguished using linetype, shape, and fill
# (not colour).
##############################################################################

## ---------------------------------------------------------------------
## STEP 1: DATA COLLECTION AND SETUP
## ---------------------------------------------------------------------

# Install packages (run once, then comment out)
# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("quantmod")
# install.packages("TTR")
# install.packages("dplyr")
# install.packages("patchwork")

library(shiny)     # web app framework
library(ggplot2)   # plotting
library(quantmod)  # fetching stock data + technical indicator helpers
library(TTR)        # SMA / EMA / RSI / MACD calculations
library(dplyr)      # data wrangling
library(patchwork)  # stacking price / RSI / MACD panels

# Black & white ggplot theme reused everywhere so nothing but black ink is used
bw_theme <- theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey85"),
    axis.text  = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    plot.title = element_text(colour = "black", face = "bold"),
    legend.position = "bottom",
    legend.text  = element_text(colour = "black"),
    legend.title = element_text(colour = "black"),
    text = element_text(colour = "black")
  )

# Helper: safely fetch historical data for a symbol from Yahoo Finance.
# Handles bad symbols / missing data gracefully instead of crashing the app.
fetch_stock_data <- function(symbol, from, to) {
  tryCatch({
    data <- getSymbols(symbol, src = "yahoo", from = from, to = to,
                        auto.assign = FALSE)
    if (is.null(data) || nrow(data) == 0) return(NULL)
    colnames(data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    data
  }, error = function(e) {
    NULL
  }, warning = function(w) {
    NULL
  })
}

# Helper: resample OHLC data to Daily / Weekly / Monthly
resample_data <- function(data, time_frame) {
  if (is.null(data)) return(NULL)
  switch(time_frame,
    "Daily"   = data,
    "Weekly"  = to.weekly(data, indexAt = "lastof", drop.time = TRUE, name = NULL),
    "Monthly" = to.monthly(data, indexAt = "lastof", drop.time = TRUE, name = NULL),
    data
  )
}

## ---------------------------------------------------------------------
## STEP 2: VISUALIZING STOCK DATA - UI
## ---------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Portfolio Visualization Dashboard"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # --- Data selection widgets ---
      textInput("stock_symbol", "Stock Symbol:", value = "AAPL"),
      dateRangeInput("date_range", "Select Date Range:",
                      start = Sys.Date() - 365, end = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly"), selected = "Daily"),
      selectInput("chart_type", "Chart Type:",
                  choices = c("Line", "Candlestick", "Area"), selected = "Candlestick"),

      hr(),

      # --- Technical indicator toggles ---
      h4("Technical Indicators"),
      checkboxGroupInput("technical_indicators", NULL,
                          choices = c("Moving Averages", "RSI", "MACD")),
      conditionalPanel(
        condition = "input.technical_indicators.includes('Moving Averages')",
        numericInput("ma_short_n", "Short MA period:", value = 20, min = 2, max = 200),
        numericInput("ma_long_n",  "Long MA period:",  value = 50, min = 2, max = 400)
      ),

      hr(),

      # --- Trading rule toggle ---
      h4("Trading Rule"),
      checkboxInput("show_signals", "Show Buy/Sell/Hold signals (MA crossover)", value = TRUE),

      actionButton("refresh", "Fetch / Refresh Data", class = "btn-block")
    ),

    mainPanel(
      width = 9,
      plotOutput("stock_chart", height = "700px"),
      br(),
      tableOutput("signal_table")
    )
  )
)

## ---------------------------------------------------------------------
## STEP 2-4: SERVER LOGIC
## ---------------------------------------------------------------------

server <- function(input, output, session) {

  # ---- STEP 1 (server side): reactive data fetch, triggered by button ----
  raw_data <- eventReactive(input$refresh, {
    fetch_stock_data(toupper(input$stock_symbol),
                      input$date_range[1], input$date_range[2])
  }, ignoreNULL = FALSE)

  # Fetch once automatically on app start with default inputs
  observeEvent(TRUE, { }, once = TRUE)

  # ---- STEP 2 (server side): filter + resample stock data ----
  filtered_data <- reactive({
    data <- raw_data()
    validate(need(!is.null(data), paste0(
      "Could not fetch data for '", input$stock_symbol,
      "'. Check the symbol or your internet connection.")))

    # Filter by date range (extra safety on top of getSymbols' from/to)
    data <- data[paste0(input$date_range[1], "/", input$date_range[2])]
    validate(need(nrow(data) > 1, "No data available for the selected date range."))

    # Resample by chosen time frame
    resample_data(data, input$time_frame)
  })

  # ---- STEP 4: trading rule (Moving Average Crossover) ----
  signal_data <- reactive({
    data <- filtered_data()
    req(input$ma_short_n, input$ma_long_n)

    short_ma <- SMA(Cl(data), n = min(input$ma_short_n, nrow(data) - 1))
    long_ma  <- SMA(Cl(data), n = min(input$ma_long_n,  nrow(data) - 1))

    # Buy  = short MA crosses above long MA
    # Sell = short MA crosses below long MA
    # Hold = otherwise
    raw_signal <- ifelse(short_ma > long_ma, "Buy",
                   ifelse(short_ma < long_ma, "Sell", "Hold"))

    df <- data.frame(
      Date     = index(data),
      Close    = as.numeric(Cl(data)),
      ShortMA  = as.numeric(short_ma),
      LongMA   = as.numeric(long_ma),
      Signal   = raw_signal,
      stringsAsFactors = FALSE
    )

    # Only flag the BAR where the signal actually changes (a true crossover),
    # so annotations mark real trade points rather than every single bar.
    df$SignalChange <- c(FALSE, diff(as.numeric(factor(df$Signal))) != 0)
    df$SignalChange[is.na(df$SignalChange)] <- FALSE
    df
  })

  # ---- STEP 2/3/4: build the combined chart ----
  output$stock_chart <- renderPlot({

    data <- filtered_data()
    df   <- signal_data()
    df_ohlc <- data.frame(
      Date  = index(data),
      Open  = as.numeric(Op(data)),
      High  = as.numeric(Hi(data)),
      Low   = as.numeric(Lo(data)),
      Close = as.numeric(Cl(data))
    )

    ## --- Base price panel, by chart type ---
    if (input$chart_type == "Line") {

      p <- ggplot(df_ohlc, aes(x = Date, y = Close)) +
        geom_line(colour = "black", linewidth = 0.6)

    } else if (input$chart_type == "Area") {

      p <- ggplot(df_ohlc, aes(x = Date, y = Close)) +
        geom_area(fill = "grey80", colour = "black", linewidth = 0.5)

    } else { # Candlestick
      df_ohlc$Direction <- ifelse(df_ohlc$Close >= df_ohlc$Open, "Up", "Down")
      # width of candle body scaled to number of bars
      w <- as.numeric(diff(range(df_ohlc$Date))) / nrow(df_ohlc) * 0.6
      if (!is.finite(w) || w <= 0) w <- 0.6

      p <- ggplot(df_ohlc, aes(x = Date)) +
        geom_segment(aes(xend = Date, y = Low, yend = High), colour = "black") +
        geom_rect(aes(xmin = Date - w/2, xmax = Date + w/2,
                      ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                      fill = Direction), colour = "black") +
        scale_fill_manual(values = c("Up" = "white", "Down" = "black"),
                           guide = "none")
    }

    p <- p +
      labs(title = paste(toupper(input$stock_symbol), "-", input$chart_type, "Chart"),
           x = NULL, y = "Price") +
      bw_theme

    ## --- STEP 3: overlay Moving Averages on the price panel ---
    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p +
        geom_line(data = df, aes(x = Date, y = ShortMA, linetype = "Short MA"),
                   colour = "black", linewidth = 0.5, na.rm = TRUE) +
        geom_line(data = df, aes(x = Date, y = LongMA, linetype = "Long MA"),
                   colour = "black", linewidth = 0.5, na.rm = TRUE) +
        scale_linetype_manual(name = NULL, values = c("Short MA" = "dashed",
                                                        "Long MA"  = "dotted"))
    }

    ## --- STEP 4: annotate Buy/Sell signals on the price chart ---
    if (input$show_signals) {
      changes <- df[df$SignalChange & df$Signal != "Hold", ]
      if (nrow(changes) > 0) {
        p <- p +
          geom_point(data = changes, aes(x = Date, y = Close, shape = Signal),
                     size = 3, colour = "black", fill = "black") +
          geom_text(data = changes, aes(x = Date, y = Close, label = Signal),
                     vjust = -1, size = 3, colour = "black") +
          scale_shape_manual(name = "Signal",
                              values = c("Buy" = 24, "Sell" = 25))
      }
    }

    panels <- list(p)

    ## --- STEP 3: RSI sub-panel (toggle on/off) ---
    if ("RSI" %in% input$technical_indicators) {
      rsi_val <- RSI(Cl(data), n = 14)
      rsi_df <- data.frame(Date = index(data), RSI = as.numeric(rsi_val))

      p_rsi <- ggplot(rsi_df, aes(x = Date, y = RSI)) +
        geom_line(colour = "black", linewidth = 0.5, na.rm = TRUE) +
        geom_hline(yintercept = c(30, 70), linetype = "dotted", colour = "black") +
        labs(y = "RSI (14)", x = NULL) +
        ylim(0, 100) +
        bw_theme

      panels <- c(panels, list(p_rsi))
    }

    ## --- STEP 3: MACD sub-panel (toggle on/off) ---
    if ("MACD" %in% input$technical_indicators) {
      macd_val <- MACD(Cl(data), nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
      macd_df <- data.frame(
        Date      = index(data),
        MACD      = as.numeric(macd_val[, "macd"]),
        Signal    = as.numeric(macd_val[, "signal"])
      )
      macd_df$Hist <- macd_df$MACD - macd_df$Signal

      p_macd <- ggplot(macd_df, aes(x = Date)) +
        geom_col(aes(y = Hist), fill = "grey70", colour = NA, na.rm = TRUE) +
        geom_line(aes(y = MACD, linetype = "MACD"), colour = "black", na.rm = TRUE) +
        geom_line(aes(y = Signal, linetype = "Signal"), colour = "black", na.rm = TRUE) +
        scale_linetype_manual(name = NULL, values = c("MACD" = "solid", "Signal" = "dashed")) +
        labs(y = "MACD", x = "Date") +
        bw_theme

      panels <- c(panels, list(p_macd))
    }

    ## --- Combine price panel + any indicator panels vertically ---
    if (length(panels) == 1) {
      panels[[1]]
    } else {
      heights <- c(3, rep(1, length(panels) - 1))
      Reduce(`/`, panels) + plot_layout(heights = heights)
    }
  })

  # ---- Small table showing the most recent trading signals ----
  output$signal_table <- renderTable({
    req(input$show_signals)
    df <- signal_data()
    tail(df[, c("Date", "Close", "ShortMA", "LongMA", "Signal")], 10)
  })
}

## ---------------------------------------------------------------------
## RUN APP
## ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)
