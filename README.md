# Forecasting Gold Futures Under Macroeconomic Pressures

**Authors**: Saumya Jain, Arav Deewan, Kashish Joshipura 

---

### Project Overview

The primary objective of this project is to construct moving 1-step ahead forecasts for daily Gold Futures closing prices (`GC=F`) over a specified holdout period. We aim to empirically demonstrate statistical proficiency in classical time-series analysis by comparing four distinct predictive approaches: 
1. Simple baselines (Persistence/Naïve and Average models in the training set)
2. Exponential Smoothing methods (ETS)
3. Rigorous diagnostic-driven auto-regressive integrated moving average models (ARIMA) purely evaluating intrinsic asset momentum.
4. Multivariate ARIMAX algorithms utilizing lagged macroeconomic trends (Equities, US Dollar strength, Bonds, and Oil). 

---

### Data Variables and Exploratory Analysis

Our forecasting pipeline is built on daily-frequency data queried from Yahoo Finance and the St. Louis Federal Reserve (FRED), utilizing forward-fill imputation for mismatched market holidays.
* **Target ($y_t$): Gold Futures (GC=F)**. Measured in USD per Troy Ounce. Used to represent daily market consensus.
* **Exogenous ($X_1$): U.S. Dollar Index (DTWEXBGS)**. Index value. Strongly inversely correlated with gold.
* **Exogenous ($X_2$): S&P 500 Index (SP500)**. Index value. Represents broad equity and risk-on sentiment.
* **Exogenous ($X_3$): 10-Year Treasury Yield (DGS10)**. Measured in percentages (%). Represents the risk-free rate and opportunity cost of holding non-yielding gold.
* **Exogenous ($X_4$): WTI Crude Oil (DCOILWTICO)**. USD per barrel. Acts as a core inflation and commodity-wide signal.

All exogenous explanatory variables are strictly lagged by 1-day ($X_{t}^{(lag)} = X_{t-1}$) to strictly prevent data leakage, guaranteeing that moving 1-step ahead forecasts use purely historical records. To enforce stationarity during formulation, the target asset was modeled using logarithmic returns: $r_t = \ln(y_t) - \ln(y_{t-1})$. 

---

### Model Candidates and Forecasting Methodologies

**Theoretical Baselines**
We fit a naïve persistence rule ($\hat{y}_{t+h \mid t} = y_{t}$) and a historical average rule. Assuming efficient financial markets frequently behave as random walks, the persistence rule establishes a notoriously difficult baseline to definitively beat over continuously trending holdout blocks.

**Exponential Smoothing (ETS)**
We fitted parameterized exponential decay configurations treating Error, Trend, and Seasonality as dynamic weights optimized against the training dataset.

**Diagnostic ARIMA Selection**
Utilizing `auto.arima()` acting on the original training series, we extracted algorithmic parameter recommendations for $p, d, q$. To account for the reality that the mathematically optimal AIC model is not necessarily the superior model for out-of-sample forward forecasting, we purposefully perturbed the $p$ and $q$ lags around the native recommendation, evaluating residuals visually (ACF/PACF) and mathematically (Ljung-Box Q-Statistic).

**ARIMAX (With Explanatory Variables)**
To evaluate global market constraints, we fitted multivariate regressions by feeding the structural elements from `auto.arima()` with our active vector of 1-day lagged macroeconomic components. 

---

### Out-of-Sample Holdout Forecast Evaluation

Determining the topologically superior model relies on strict evaluation architectures. Each model candidate was scored dynamically across the hidden sequence using **moving 1-step ahead forecasts**. The competing methods were evaluated exclusively using their generated **Root Mean Square Error (RMSE)**. 



---

### Reproducible Codebase Infrastructure

Our analysis adheres to strict computational reproducibility standards. The foundational pipeline is purposefully uncoupled logically into sequentially numbered `.R` scripts.

#### R Execution Sequence
* `01_get_and_wrangle_data.R` (Wrangling)
* `02_split_and_EDA.R` (Exploration)
* `03_baselines.R` & `04_ets.R` (Exponential Smoothing)
* `05_arima.R` (ARMA/ARIMA diagnostics)
* `06_arimax.R` (ARMAX variables)
* `07_compare_models.R` (Comparisons of best models)

At every intermediate stage, the codebase rigorously outputs `.csv` files representing raw downloads, wrangled states, diagnostic tables, and model residual metrics directly to the `data/processed/` environments. This cleanly uncouples extraction steps and cleanly insulates the predictive topologies when train/holdout structures or targets are altered in the future.
