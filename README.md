# editing-and-imputation-air-quality
R workflow for statistical data editing and imputation, following the approach used in official statistics: validation rules, error localisation, hot-deck imputation and rule-based correction, applied to the UCI Air Quality dataset.

## Goal
Show how raw data can be checked and cleaned before the main analysis, using a
transparent and reproducible sequence of steps.

## Workflow
1. **Loading**: the dataset is downloaded directly from the UCI repository. The value
   `-200` is recoded as missing, empty rows and columns are removed and variable names
   are cleaned.
2. **Inspection**: structure, summary statistics, ranges and boxplots of the variables.
3. **Validation rules**: rules based on physical and chemical constraints, for example
   concentrations within plausible ranges and
   limits for temperature, relative humidity and sensor responses.
4. **Validation**: the rules are checked against the data (`validate`) to see which
   records violate which rules, with a summary and a plot of the failures.
5. **Error localisation**: the fields to correct are located with `errorlocate`.
6. **Missing data patterns**: visualised with `VIM`, to look at item and unit
   non-response.
7. **Imputation**: hot-deck (donor-based) imputation, using `Time` as the domain
   variable to find similar time points.
8. **Correction**: deterministic, rule-based corrections of out-of-range values.
9. **Final check**: the cleaned data are checked again against the rules.

## Requirements
R with the packages `readr`, `validate`, `errorlocate`, `VIM`, `dplyr` and `ggplot2`.
The script also loads `deducorrect`, but the deductive corrections are written by hand
with `ifelse`.

## Data
The data are downloaded automatically by the script from the UCI Machine Learning
Repository (Air Quality dataset), so no data file is included here.

## Notes and limitations
- Missing values are **simulated**: six variables are randomly deleted to have something to impute.
  They are not part of the original data.
- The limits used to correct values are more cautious than the limits in the validation
  rules, so the final check showing no failures does not mean the two sets are identical.
- This is a learning project and the thresholds reflect my own choices for the
  purpose of the demonstration.

## Files
- `projectscript.R`: the full script
