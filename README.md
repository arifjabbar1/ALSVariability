# ALSVariability
Repository Containing Code Used in 'Describing and Characterising Variability in ALS Disease Progression' Published in ALS-FTD. PROACT database files are available at https://ncri1.partners.org/ProACT.

'clean_and_merge_data.ipynb' loads the raw data from the PROACT database, cleans, encodes and merges all the data.  

'impute_and_window.ipynb' further cleans the data, split the data into train and test sets, and does data imputation.

The file 'Data Aggregation.R' takes in the adverse events file from the PRO-ACT database and 'ALSdataimputedknn_GL_cv0.csv' obtained from 'impute_andwindow.ipynb'. It can be used to generate these files: 'Var_6mo_6mo.csv', 'Var_9mo_9mo.csv', 'Var_12mo_12mo.csv' and 'Var_Tot.csv' for the 6, 9, 12 months and total trial duration period windows described in the paper. 

The file 'ML Training.R' takes 'Var_6mo_6mo.csv', 'Var_9mo_9mo.csv' and 'Var_12mo_12mo.csv' as input and can be used to generate the classification models with observation windows, whereas 'Regression Model.R' generates regression models. 'ML Training_Total.R' generates a classification model for the total trial period model, 'Var_Tot.csv'. 

'Data Aggregation (Single Time Point).R' aggregates data from 'ALSdataimputedknn_GL_cv0.csv' obtained from 'impute_andwindow.ipynb' and generates 'SingleTimePoints.csv'. ‘Single Time Point Predictions.R' generate classification models which predict variability without utilising an observation window. ‘Reduced model.R’ generates a similar classification model from this datafile but with a reduced number of parameters.

Required packages in R are ROSE, XGBoost, ggplot2 and dplyr.
