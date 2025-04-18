
# STOP: Studying Time-Series of Preeclamptic Emergencies

This repository contains the code and resources for the project **STOP** (Studying Time-Series of Preeclamptic Emergencies), which applies a comprehensive data analytics pipeline (Descriptive, Diagnostic, Predictive, and Prescriptive - DDPP) to analyze emergency room arrivals of preeclampsia patients at the *IESS Hospital del Día Sur Valdivia* in Guayaquil, Ecuador.

## 📋 Project Overview

Between 2019 and 2023, anonymized electronic medical records (EMRs) were collected and processed to:
- Understand the patterns in monthly preeclamptic emergency arrivals
- Forecast future demand using machine learning and statistical models
- Provide actionable insights through scenario-based prescriptive analytics

This research supports hospital resource planning and emergency preparedness, especially for maternal health management.

## 📈 Methodology

The project followed a DDPP (Descriptive, Diagnostic, Predictive, Prescriptive) analytics framework:

1. **Descriptive Analytics**: 
   - Summary statistics (mean, median, quartiles)
   - Visualizations: line plots, boxplots

2. **Diagnostic Analytics**:
   - Trend, seasonality, stationarity, autocorrelation, and anomaly detection using STL decomposition, ACF/PACF, Durbin-Watson test, and others

3. **Predictive Analytics**:
   - Time series forecasting using:
     - ARIMA
     - ETS
     - Extreme Learning Machine (ELM)
     - Multilayer Perceptron (MLP)
   - Best model (MLP) achieved MAPE ≈ 17.21%

4. **Prescriptive Analytics**:
   - Scenario 1: Prioritization of emergency cases by survival probability using the Knapsack optimization problem
   - Scenario 2: Emergency room closure impact analyzed using Bayesian Structural Time Series (BSTS) modeling

## 🧪 Technologies Used

- **Language**: R
- **Libraries**: `forecast`, `nnfor`, `bsts`, `dplyr`, `ggplot2`, `tseries`, `caret`, etc.
- **Models**: ARIMA, ETS, MLP, ELM
- **Optimization**: Knapsack (Integer Programming)

## 📁 Repository Structure

```
.
├── CAMAS DE HOSPITALIZACION Y URGENCIAS-VALDIVIA-SOLO EMERGENCIASv2.csv #data
├── STOPEmergenciesPreeclampsia.R #code
└── README.md
```

## 📊 Key Findings

- An average of **13.3** preeclamptic emergencies occurred per month between 2019–2023.
- The **MLP model** produced the best forecast accuracy.
- Prescriptive scenarios highlight the need for:
  - Allocating emergency resources by survival probability
  - Planning for patient redistribution in case of hospital closure

## 📄 Citation

If you use this code or dataset, please cite:

> Parrales-Bravo, F., Caicedo-Quiroz, R., Tolozano-Benites, E., Vasquez-Cevallos, L., & Cevallos-Torres, L. (2025). STOP: Studying Time-Series of Preeclamptic Emergencies. *IEEE Access*, 13, 65672–65687. https://doi.org/10.1109/ACCESS.2025.3558888

## 🔗 Links

- 📄 [Full Paper (IEEE Access)](https://doi.org/10.1109/ACCESS.2025.3558888)
- 💻 [Author's GitHub Repo](https://github.com/fparrale/STOP-CODE)

## 📬 Contact

For questions or collaboration, reach out to:

**Franklin Parrales-Bravo**  
Senior Member, IEEE  

---

> This project was supported by the Universidad de Guayaquil under Grant FCI-008-2021.
