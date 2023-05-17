# Blog Notebooks Repository

This branch contains the notebooks for the data minining project. 

In this branch, we will explore the data and build models to predict the number of injuries and fatalities caused by tornadoes in the United States.

Links: 


### Data sources
https://www.kaggle.com/datasets/danbraswell/us-tornado-dataset-1950-2021

## Models:
- [Polynomial w/ backwards elimination](polynomial.R)
- [Support Vector Machine](svr.R)
- [Random Forest Regression](randomforest.R)


Data description:

- yr: Year of tornado occurrence
- mo: Month of tornado occurrence
- dy: Day of tornado occurrence
- date: Date of tornado occurrence (yyyy-mm-dd)
- st: State of tornado occurrence
- mag: F-scale magnitude of tornado (0-5) (-9 = unknown)
- inj: Number of injuries
- fat: Number of fatalities
- slat: Starting latitude of tornado
- slon: Starting longitude of tornado
- elat: Ending latitude of tornado (0 = unknwon)
- elon: Ending longitude of tornado (0 = unknown)
- len: Length of tornado in miles (track length)
- wid: Width of tornado in yards
