# Available Phosphorus
Methodology used to produce a map of inherent soil Colwell P for Australia which approximates available P

## Summary
The map was made using soil point data and environmental covariates developed by TERN. The soil 
data comes from state and territory agencies and was collated using the TERN Soil Data Federator 
(See TERNDataPrep.R). Soil data was filtered using *disturbance of site* coding if available or *land 
use* data (ACLUMP). A *random forests* (RF) model was fitted using the *Ranger* implementation 
of RF. Prediction uncertainty was determined using a bootstrapping method implemented by 
Malone (2017). 50 simulations were run to determine the 90% prediction interval. The final map was 
predicted using the *predict* function in the *Raster* package. Predictions were made using the 
TERN covariate tiles in batch mode on the DES HPC.

## Workflow
1. Extract all avialable Colwell P data from state agency soil databases.
2. Apply a land use filter to the extracted data to capature just soil data from non-cropping lands.
3. Add in other data not in state agency data bases.
4. Standardise soil data to global soil map standard depths using a spline.
5. Format standardised data for modell fitting by random forest.
6. Select TERN covariates.
7. Fit a random forests model using the Ranger package.
8. Anaylse model fit, repeat above steps if necessary.
9. Map out fitted model using raster interpolation function *predict* using TERN standard tiles
10. Mosaic tile predictions and back transform map

**Installation:** To download all scripts and files in this repository, click green Clone or Download button, select Download ZIP, save zip file to C://Temp, extract files. Always source files from this respository so that you have the latest version. You will need SQL Developer (linked to SALI), RStudio, R and Pyhon installed.

**Usage:** This repository is provided for reviewers of the TERN Available P map to assist with understanding how the product was made.

**Contributing:** If you can see improvements that can be made, please do a pull request or raise an issue.

**Credits:** The Soil Data Federator API used in the first script was created by Ross Searle, TERN Landscapes & CSIRO. The second script contains ideas and code present in Malone BP, Minasny B, McBratney AB (2017) ‘Using R for Digital Soil Mapping.’ (Spinger International Publishing: Switzerland) as well as orginal script by Peter Zund, who is also the author of this repository.

**License:** Free to use with acknowledgement.

## Further detail 
1. Using the TERNDataPrep.R, P data has been extracted from national, state and terittory databases. Both Colwell P & Olsen P were extracted (Lab method code 9B1, 9B2, 9C1, 9C2). After extraction. Olsen P was converted to the equivellent Colwell P using this function (ColwellPEQUI = (OlsenP X 2.869) - 2.93 (Moody etal 2013)) except for Calcaresols. The extraction is done using the TERN Soil Data Federator Web API (Searle) using the following settings - Profile samples (not bulk samples). Quality of sample collection process, sample management and quality of location coordinates was not considered.
2. Disturbance of site (NCST 2009) or landuse (ACLUMP) were used to exclude sites if currently cropping or other landuses with potentially unnatural P concentrations were practiced. Disturbance of site was the first filter and if unavilable, then landuse using the current 50m raster product available on https://www.agriculture.gov.au/abares/aclump/land-use/catchment-scale-land-use-of-australia-update-december-2018 . See this file ALUMClassV8.csv in this repo for which landuses were used to exclude sites.
3. The link between the Soil Data Federator Web API and the Queensland Government SALI database is live for anaylical data but static for all other data (morphological data). Hence new sites entered into SALI since Novermber 2019 are not caputured by the Web API. Without the corresponding site & morphological data the anaylical data will not be exported by the Web API. Because of this, the author has done a manual query of all current SALI, this data is read by the TERNDataPrep.R script and samples compared with data comming from the Web API. Any missing Qld data is added in. Also valuable data (D. Lawrence) not in SALI has been manually added by the same script.
During the review of maps in WA, it was found that there were large errors with some of the data coming from the WA soils database. The author was supplied with corrected and incorrect samples (See two excel files in this repo). This data is read in by the TERNDataPrep.R script and correct samples added and incorrect samples removed.
4. Brendan Malones ithir::ea_spline function was used to spline data to the six GSM depths. Sites/Profiles with only one sample (which cannot be splined) were added back into the resulting attribute data after splining. The midpoint of the sample depth was used to determine which GSM depth single profile samples belong to.
5. The distribution of the final P data was skewed to low P concentrations. RF models expect normally distributed data. The final data was transformed using the *natural log*.
6. Only available TERN covariates were used for this project. 46 covariates were selected. Covariates were as much as possible, artifact free as it relates to Colwell P as well as not autocorrelated.   
7. The Ranger implementation of RF within the *Caret package* was used to fit the model. By using the *Caret* implementation of the *Ranger package* the author was able to interpolate the fitted model using the *predict* function of the *Raster package*. Model fitting is done using the first half of the *Ranger implementation.R* script. Interpolation is done using the second half of the script. 30% of all input data was held back for internal validation. Model fitting was performed using leave-one-out- cross-validation with ten folds. The prediction interval was determined using simple bootstrapping (50 runs). A 90% prediction interval was used to determine the lower and upper prediction limits.
8. Model fit was assessed using *RMSE*, *R2*, *MAE* and *MSE*
9. Model interpolation was done tile by tile using the 2172 TERN covariate tiles. 
10. Predicted maps were mosaiced using *rgdal::mosaic_rasters* function and then back transformed. 
