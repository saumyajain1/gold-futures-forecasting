# Project Proposal: Forecasting Daily Gold Futures Prices

## 1. Executive Summary
This project proposes a robust, reproducible analytical pipeline to forecast daily gold futures prices (GC=F) using classical univariate time-series models and multivariate modeling supplemented with systemic macro-financial indicators. 

Gold holds a unique position in global finance—often acting as a currency hedge, a safe-haven asset, and an inflation hedge. The objective of this analysis is to evaluate whether systemic asset class correlations (equities, bond yields, oil, and fiat currencies) provide meaningful predictive lift for gold prices over pure univariate historical autoregression.

## 2. Data Acquisition & Exogenous Variables
The modeling framework uses daily gold futures closing prices alongside a curated set of macroeconomic and systemic financial predictors.

### 2.1 Target Variable
* **Gold Futures (GC=F)**: Sourced via Yahoo Finance. This serves as our prediction target representing the immediate market sentiment and pricing of gold.

### 2.2 Macro-Financial Predictors
We utilize four core exogenous variables sourced from the St. Louis Federal Reserve (FRED) to capture broader market conditions.
1. **Trade Weighted U.S. Dollar Index (DTWEXBGS)**: Gold is globally priced in U.S. Dollars. An appreciating dollar intuitively makes gold more expensive in other currencies, historically enforcing a strong inverse relationship.
2. **S&P 500 Index (SP500)**: Serves as a primary proxy for global risk appetite and stock market performance. Gold often experiences heightened demand as a "safe haven" during equity market distressed periods.
3. **WTI Crude Oil Prices (DCOILWTICO)**: Acts as a dual proxy for broad commodity market strength and global inflation expectations. Gold is traditionally utilized as an inflation hedge.
4. **10-Year Treasury Constant Maturity Rate (DGS10)**: Represents the "risk-free" real yield over time. Because physical gold yields no interest or dividend, rising treasury yields increase the opportunity cost of holding gold, typically placing downward pressure on gold prices.

## 3. Data Preprocessing & Exploratory Analysis
Financial data is intrinsically noisy and subject to structural breaks and varying holiday schedules across asset classes. Our pipeline enforces strict preprocessing regimens.

* **Date Alignment & Imputation**: Merging asset datasets frequently introduces NA values due to mismatched market holidays. We utilize forward-fill imputation to resolve these gaps, maintaining chronological integrity based on the assumption that the most recently traded price represents current market consensus in the absence of a new session.
* **Lagging Mechanism**: To accurately represent a predictive operational environment, all independent macroeconomic variables are lagged by exactly 1 day. A forecast for time $t$ can strictly only utilize data available up to time $t-1$.
* **Train / Holdout Splitting**: Time-series models are heavily prone to overfitting random walks. We perform a strict chronological sequence split, keeping a concluding holdout subset entirely "unseen" during model training. Exploratory Data Analysis (EDA)—including ACF/PACF autocorrelation checks and seasonality evaluations—is performed strictly on the training set to prevent data leakage.

## 4. Modeling Methodology
Financial markets are highly efficient. Hence, any proposed model must prove structural superiority over simple rules of thumb. 

### 4.1 Benchmark Models
Before employing complex models, we establish lower-bound performance using baselines:
* **Mean Benchmark**: Forecasts future values as the historical mean.
* **Naïve Benchmark (Random Walk)**: Assumes that the best prediction of tomorrow's price is today's price. Financial assets often resemble random walks, making this a notoriously difficult baseline to definitively beat.

### 4.2 Univariate Time-Series Models
We deploy standard internal-memory models that rely solely on historical gold price dynamics:
* **ETS (Exponential Smoothing State Space)**: Deconstructs the timeline into Error, Trend, and Seasonal components. It provides excellent responsive predictions based on recent trajectory-level exponential decay weights.
* **ARIMA (AutoRegressive Integrated Moving Average)**: Standardizes non-stationary prices via differencing, and maps predictions using lagged autoregressive terms (AR) and residual moving averages (MA).

### 4.3 Multivariate Time-Series Models
* **ARIMAX**: We extend our optimal ARIMA topologies to include our preprocessed macro-financial indicators as exogenous regressors ($X$). This tests the central hypothesis: Does feeding the model the lagged state of the Dollar, Equities, Oil, and Yields structurally improve our forecasting accuracy or error intervals?

## 5. Evaluation & Diagnostics
We evaluate each model using stringent, robust financial data science principles.

* **Root Mean Square Error (RMSE)**: Used to heavily penalize large predictive mistakes on the holdout data points. We evaluate absolute RMSE as well as relatively via its delta against the Naïve Benchmark.
* **Residual Diagnostics (Ljung-Box Test)**: Performed on the "winning" topologies. If a model has fully extracted the predictive signal from the data, its resulting residuals should structurally resemble white noise.
* **Prediction Interval Coverage**: Point forecasts in financial markets are structurally fragile. We rigorously test the 80% and 95% predictive interval output topologies to observe empirical coverage on the unseen holdout data. A functionally robust model must maintain confidence intervals that accurately capture the asset's realized volatility without being overly broad.

## 6. System Architecture (Pipeline Scripts)
The overarching code infrastructure is fully automated and modular, numbered deliberately for complete chronological reproducibility across seven distinct phases from fetching raw datasets, to fitting models, to rendering final statistical ranking figures and validation curves.
