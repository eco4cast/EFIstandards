# Methods Description

**Forecast timestep**
1 day

**Forecast time horizon**

16 days

**Data assimilation**

Data Assimilation used: Yes
If, DA used - type of method: EnKF
If, DA used - Number of parameters calibrated: 3
If, DA used - Sources of training data (DOI, GitHub): https://github.com/CareyLabVT/SCCData/tree/carina-data

**Model Description**

Type of model (Empirical, process-based, machine learning): Process-based
Model name: General Lake Model-AED V3
Location of repository with model code: 
	GLM: https://github.com/AquaticEcoDynamics/GLM
	AED: https://github.com/AquaticEcoDynamics/libaed2
Model citation: Hipsey et al. 2019 GMD
Total number of model process parameters: Hard to count

**Model Covariates**

Type (i.e., meteorology): meteorology
Source (i.e., NOAA GEFS): NOAA GEFS 16-day

Type (i.e., meteorology): Stream Inflow
Source (i.e., NOAA GEFS): https://github.com/CareyLabVT/SCCData/tree/diana-data

**Uncertainty**

Answers: No, Derived from data, Propagates, Assimilates

Initial conditions: Assimilates
Parameter: Propagates, Assimilates
Parameter Random Effects: No
Process (within model): Propagates, Assimilates
Multi-model: No
Driver: Derived from data
Scenario: Yes - oxygen system on and off

Method for propagating uncertainty (Analytic, ensemble numeric): ensemble numeric
If Analytic, specific method: NA
If ensemble numeric, number of ensembles: 210



