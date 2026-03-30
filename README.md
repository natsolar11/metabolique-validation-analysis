# metabolique-validation-analysis
Reproducible R workflow for the validation analysis of the METAbolique dietary assessment app.
# METAbolique Validation Analysis

This repository contains a reproducible R analysis template based on my Master's thesis project:

**“Accuracy and Precision of a Mobile Application Compared with 24-hour Recall for Dietary Intake Estimation Using a Standardized Meal as Reference.”**

## Project overview
This study compared a mobile dietary assessment tool (**METAbolique**) against a traditional **24-hour recall (R24)** using a **standardized meal with known nutritional composition** as the reference.

The main outcomes were:
- Energy (kcal)
- Carbohydrates (g)
- Protein (g)
- Lipids (g)

The analysis included:
- Descriptive statistics
- Error calculation against the standardized meal
- Bland–Altman agreement analysis
- RMSE estimation
- Exploratory regression models for absolute caloric error
- SUS usability score visualization

## Sample
The original study included **95 adults**.

## Reproducibility note
This repository provides a **clean analysis template** derived from the original thesis workflow.  
The original dataset is **not publicly shared** due to confidentiality and institutional restrictions.

A mock dataset can be used to reproduce the workflow structure.

## Files
- `analysis_template.R`: Main R script for the analysis workflow
- `mock_data.csv`: Example dataset with fictional values
- `data_dictionary.md`: Description of required variables

## Software
Analyses were conducted in **R**.

## Contact
For academic correspondence related to this project:  
**Natalia Soto, MSc**  
natalia.sotol@incmnsz.mx
