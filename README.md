This repository contains code to assess the performance of multiple risk prediction models.

The analysis code requires an input dataset from which the risk factors outcomes have been derived together with meta data specifying the model risk factors and their levels (including weights).  The format of the meta data file allows for the input and analysis of multiple risk prediction models simultaneously.  

The analysis uses logistic and coxph (time to event) models.  The models assess the performance based on the use of a linear predictor constructed with risk factor levels and their weights, together with a version that uses model risk factors only and re-calibrates the weights.

All models derive a predicted risk for each individual. The predicted risk is compared to the observed outcome to derive model discrimination (C-index), Briers score (accuracy) and calibration.

The calibration code will be updated to include a score.

The time to event data will be updated to determine absolute risks at both 5 and 10 year periods.
