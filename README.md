# Project Proposal — Forecasting Daily Gold Futures Prices

---

## Table of Contents

1. [Motivation & Problem](#1-motivation--problem)
2. [Data](#2-data)
3. [Models and Evaluation Metrics](#3-models-and-evaluation-metrics)
4. [Implementation Plan](#4-implementation-plan)
5. [Repository Structure](#5-repository-structure)
6. [Getting Started — Setup](#6-getting-started--setup)
7. [Contributing via Pull Requests](#7-contributing-via-pull-requests)

---

## 1. Motivation & Problem

Gold holds a unique position in global finance, often acting as a currency hedge, a safe-haven asset, and an inflation hedge. Classical forecasting models historically rely purely on the intrinsic temporal dynamics of gold prices. However, in highly integrated global markets, gold prices are heavily influenced by broader macroeconomic conditions. 

This project sets out to build a robust, reproducible analytical pipeline to forecast **daily gold futures prices (GC=F)**. By comparing pure univariate modeling against multivariate systems, we aim to answer:

1. Does the inclusion of systemic asset class correlations (equities, bond yields, oil, and the US dollar) provide meaningful predictive lift over pure univariate historical autoregression?
2. Are macroeconomic indicators effective in creating more accurate forecasting confidence intervals during unseen holdout periods?
3. Can complex state-space and integrated models strictly beat random walk baselines in an efficient financial market?

---

## 2. Data

All data are sourced from **Yahoo Finance** and the **St. Louis Federal Reserve (FRED)**, harmonized to a **daily frequency**.

| Variable | Source / Ticker | Description |
|---|---|---|
| **Gold Futures (Target)** | [Yahoo Finance (GC=F)](https://finance.yahoo.com/quote/GC=F) | Target variable representing immediate market sentiment |
| **U.S. Dollar Index** | [FRED (DTWEXBGS)](https://fred.stlouisfed.org/series/DTWEXBGS) | Measures dollar strength; gold is priced globally in USD |
| **S&P 500 Index** | [FRED (SP500)](https://fred.stlouisfed.org/series/SP500) | Proxy for global risk appetite and stock market performance |
| **WTI Crude Oil Prices** | [FRED (DCOILWTICO)](https://fred.stlouisfed.org/series/DCOILWTICO) | Proxy for broad commodity market strength and inflation |
| **10-Year Treasury Yield** | [FRED (DGS10)](https://fred.stlouisfed.org/series/DGS10) | Represents the risk-free real yield and opportunity cost |

**Pre-processing steps:**

- **Date Alignment & Imputation**: Forward-fill imputation resolves missing values induced by mismatched market holidays across variables.
- **Data Lagging**: All independent macroeconomic variables are lagged by exactly 1 day. A forecast for time $t$ can strictly only utilize data available up to time $t-1$.
- **Data Splitting**: Strict chronological sequence split into training and holdout subsets to prevent data leakage.
- **EDA & Transformations**: Exploratory data analysis (time-series plots, ACF/PACF) and logarithmic returns conversion for stationarity testing.

---

## 3. Models and Evaluation Metrics

### Benchmark rules

| Rule | Description |
|---|---|
| Average forecast | Forecast equals the historical mean of the training data |
| Naïve forecast (Random Walk) | Forecast equals the most recent observed value ($y_{t \mid t-1} = y_{t-1}$) |

### Candidate models

We fit models from both the purely endogenous and exogenous time-series families:

- **ETS (Exponential Smoothing State Space)**: Deconstructs the timeline into Error, Trend, and Seasonal components based on exponential decay weights.
- **ARIMA (AutoRegressive Integrated Moving Average)**: Maps predictions using lagged autoregressive terms and residual moving averages.
- **ARIMAX**: Extends ARIMA to include the lagged macro-financial variables (USD, Equities, Oil, Yields) as exogenous regressors.

### Evaluation Metrics

Forecasts are evaluated on a strictly unseen holdout validation set:

**1. Root Mean Square Error (RMSE):** Used as the primary accuracy metric to heavily penalize large predictive mistakes out-of-sample.
$$\mathrm{RMSE} = \sqrt{\frac{1}{N}\sum_{i=1}^{N}\!\left(Actual_i - Forecast_i\right)^2}$$

**2. Prediction Interval Coverage:** Point forecasts in financial markets are inherently fragile. We rigorously test the empirical coverage of the **80% and 95% predictive intervals** to ensure the models accurately capture realized volatility.

**3. Residual Diagnostics (Ljung-Box Test):** Assesses whether the model residuals significantly deviate from white noise. A successful model should have extracted all available predictive signal.

---

## 4. Implementation Plan

The project is developed in **R** and **RStudio** with version control managed through **GitHub**.

- All data ingestion, cleaning, modeling, and comparison steps are implemented sequentially as reproducible R scripts (`01` through `07`).
- Common project operations (plotting themes, directory scaffolding, metric calculations) are centralized in a `utils.R` helper script.
- The pipeline yields automated final evaluation tables directly out to `data/processed/` and renders stylized forecast plots into the `figures/` directory.

---

## 5. Repository Structure

```text
gold-futures-forecasting/
├── data/
│   ├── raw/               # Downloaded CSVs from FRED/Yahoo (Git ignored)
│   └── processed/         # Cleaned datasets, fitted metrics, and summaries
├── figures/               # Output directory for ACF, forecast grids, & error plots
├── scripts/
│   ├── 01_get_and_wrangle_data.R
│   ├── 02_split_and_EDA.R
│   ├── 03_baselines.R
│   ├── 04_ets.R
│   ├── 05_arima.R
│   ├── 06_arimax.R
│   ├── 07_compare_models.R
│   └── utils.R            # Utility functions for scaffolding and analysis
├── gold-futures-forecasting.Rproj
├── .gitignore
└── README.md
```

---

## 6. Getting Started — Setup

This repository contains built-in environment checking to ensure you have the required analytical R packages cleanly installed.

### Prerequisites

- R ≥ 4.1.0
- RStudio (recommended for viewing `.Rproj`)

### Steps

1. **Clone the repository**

   ```bash
   git clone https://github.com/saumyajain1/gold-futures-forecasting.git
   cd gold-futures-forecasting
   ```

2. **Open the project in RStudio**

   Double-click `gold-futures-forecasting.Rproj`.

3. **Run the sequential pipeline**

   The project utilizes a built-in function `setup_project()` defined within `utils.R`. Before executing any scripts, standard dependencies (like `dplyr`, `ggplot2`, `forecast`, etc.) will be gracefully checked and demanded.

   Run the scripts in numerical order starting with `scripts/01_get_and_wrangle_data.R`. This initial script creates the internal `data/` directories and pulls directly from external APIs.

---

## 7. Contributing via Pull Requests

We follow a **feature-branch workflow**. Please do **not** push directly to `main`.

### Workflow

1. **Sync your local `main` with the remote**

   ```bash
   git checkout main
   git pull origin main
   ```

2. **Create a feature branch**

   Use a descriptive name, e.g.:

   ```bash
   git checkout -b feature/arimax-volatility-regressors
   ```

3. **Make your changes**

   - Write clean, well-commented R code.
   - Keep each commit focused on a single logical change.
   - If introducing new libraries, be sure to update the `required_packages` vectors located at the top of the relevant numbered pipeline scripts.

4. **Push your branch**

   ```bash
   git push origin feature/arimax-volatility-regressors
   ```

5. **Open a Pull Request on GitHub**

   - Navigate to the repository on GitHub and click **"Compare & pull request"**.
   - Give the PR a clear title and a short description of what was changed and why.

6. **Address review feedback**

   Push additional commits to the same branch; the PR updates automatically.

7. **Merge**

   Once approved, merge using **"Squash and merge"** to keep the history clean, then delete the feature branch.

### Commit message style

```text
<type>: <short summary>

Optional longer explanation.
```

Common types: `feat`, `fix`, `data`, `docs`, `refactor`, `test`.

Example: `feat: add GARCH volatility layer to arimax exogenous vectors`
