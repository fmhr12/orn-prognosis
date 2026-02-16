# Osteoradionecrosis (ORN) Prognosis Tool

A compiled R Shiny application designed to predict the individualized risk of Osteoradionecrosis (ORN) in Head and Neck Cancer (HNC) patients treated with Intensity-Modulated Radiation Therapy (IMRT).

This tool utilizes a survival analysis model to generate Cumulative Incidence Functions (CIF) and employs SHAP (SHapley Additive exPlanations) values to interpret specific risk factors for individual patients.

## Features

* **Individualized Risk Prediction:** Calculates the Cumulative Incidence of ORN (ClinRad grade 1 or higher) while accounting for death as a competing risk.
* **Interactive Visualizations:**
    * **CIF Curve:** Dynamic Plotly graph showing risk trajectory from 0 to 114 months.
    * **Reference Overlay:** Option to compare the patient against the institutional average (mean CIF).
* **Explainable AI (XAI):** Uses a SHAP Force Plot to visualize how specific patient features (e.g., Smoking history, D10cc) push the risk higher or lower compared to the baseline.
* **Fast Computation:** Implements a Gower-distance nearest-neighbor algorithm to approximate SHAP values from a precomputed grid, ensuring instant results without heavy real-time processing.

## Prerequisites

To run this application, you need **R** installed. The following R packages are required:

```r
install.packages(c(
  "shiny",
  "shinythemes",
  "survival",
  "riskRegression",
  "fastshap",
  "shapviz",
  "ggplot2",
  "prodlim",
  "gower",
  "plotly"
))
