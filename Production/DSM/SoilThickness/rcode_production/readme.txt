# Organisation of R coded workflow for national mapping of soil thickness

## Data Wrangling
1. Data Wrangling

* Processing of the national site collation observation data to derive estimates of the soil thickness. Determination of whether observations were censored or not. [TERN_sitedata folder]
* Determination of soil thickness from the bore log data [boreLog_queries.R]
* Covariate data extraction [site_data_covariate_extraction.R]
* Setting up of model frames for the spatial modelling (for all variables) [model_data_preparation.R]

** Note that there were a few non R steps used in data wrangling where instead JMP was used. Some things included:
** Determination of observations to get rid of. Some criteria around removal of censored data where censoring was less than 1.35m. Removal of inordinately deep soils ie >15m (i think was the threshold). [[I will think of others]] 

*** There are some older bits of R code from Ross with initial workflow for processing the bore log data. 


## Model Fitting
* Variogram modelling of soil thickness [residual_variogram_modelling_rangermodels.R] 
* Ranger model fitting of the categorical prediction for distinguishing between deep and not deep soils [rangerModelling_soilDepth_categorical_deep_vs_notdeep.R] 
* Ranger model fitting of the categorical prediction for distinguishing between rock outcrops from not rock outcrops [rangerModelling_soilDepth_categorical_rock_vs_notrock.R] 
* Ranger model fit of soil thickness as a continuous variable [rangerModelling_soilDepth_continuous.R]


## Spatialisation of Models [folder: modelSpatialisation]
* Applying (kriging) globally fitted variogram [residual_modelling_rangermodels_spatialise.R] 
* Applying the categorical model for distinguishing between deep and not deep soils [spatialise_categorical_deep_vs_notdeep_SD_model.R] 
* Applying the categorical model for distinguishing between rock outcrops and not rock outcrops [spatialise_categorical_rock_vs_notrock_outcrop_model.R] 
* Applying the continuous model of soil thickness prediction [spatialise_continuous_SD_model.R]


## Pulling it all together
* Integration of continuous and categorical map outputs to derive products (13 products derived).[spatialise_SD_model__statisticalmoments.R (in modelspatialisation folder]
* Tile mosaicing of all outputs [folder: tileMosaic contains 13 seperate R scripts for each statistical moment.]




