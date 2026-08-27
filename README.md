# BDA400 - Assignment 6: Technical Analysis using R, Visualization Phase

**Name:** Navjot Kaur
**Course:** BDA400 - Data Science Tools and Techniques
**Assignment:** Assignment 6 - Technical Analysis using R, Visualization Phase (15%)

## Project Description

An interactive R Shiny portfolio dashboard that fetches historical stock data
(default: **AAPL**) from Yahoo Finance using the `quantmod` package and
visualizes it with three chart types: Line, Candlestick, and Area.

The dashboard overlays technical indicators that can be toggled on and off:
- **Moving Averages** (short/long SMA, adjustable periods)
- **RSI** (14-period)
- **MACD** (12/26/9)

It also implements a **Moving Average Crossover trading rule**: when the
short-term MA crosses above the long-term MA a **Buy** signal is generated;
when it crosses below, a **Sell** signal is generated; otherwise the signal
is **Hold**. Crossover points are annotated directly on the price chart with
black triangle markers, and the 10 most recent signals are shown in a table
below the chart.

All charts use black & white styling only (linetype/shape/fill distinguish
series instead of colour).

## Files

- `app.R` — full Shiny application (UI + server), organized into the four
  assignment steps: Data Collection & Setup, Visualizing Stock Data, Overlay
  Technical Indicators, and Implement Trading Rules & Annotations.

## How to Run

```r
install.packages(c("shiny", "ggplot2", "quantmod", "TTR", "dplyr", "patchwork"))
shiny::runApp("app.R")
```

## Repository Link

https://github.com/Navjotkaur482/BDA400-Assignment6
