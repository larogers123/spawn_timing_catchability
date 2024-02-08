## Data readme

**CompiledMaturityData_HaulsinAssessment_Females.csv**  
Walleye pollock specimen data from acoustic-trawl surveys conducted by the Midwater Acoustics and Conservation Engineering (MACE) Program at NOAA Alaska Fisheries Science Center. Data include surveys were conducted from 1983-2021, and subset to only those survey dates that were used to derive estimates of biomass. Specimens were randomly sampled from midwater trawls targetted on acoustic sign.  

*maturity:* maturity text code, depends on whether 5 or 8 stage maturity key was used.  
*maturity table:* "3" indicates 5 stage key was used; "11" indicates 8 stage key was used  
*matcode:* numeric, corresponds to maturity  
*mat5stage:* all maturity levels recoded to 5 stage key  
*is_spsp:* 0 when not spawning or spent (immature, developing, or prespawning), 1 when spawning or spent  
*is_mature:* 0 when not mature (immature, developing), 1 when mature (prespawning, spawning, spent)  
*yday:* day of year  
*fork_length:* fork length of fish, in cm  

**MeanVarHatchSpawn_01Sep20_NewAgeLengthREwStn_extendeddates_thru19.csv**  
Estimates of the start, midpoint, and end of spawning of walleye pollock as estimated from larvae collected during NOAA AFSC EcoFOCI spring larval surveys (1979-2019). Estimates are from Rogers, L.A., and Dougherty, A.B. 2019. Effects of climate and demography on reproductive phenology of a harvested marine fish population. Global Change Biology 25(2): 708–720. doi:10.1111/gcb.14483, with updates described in Rogers et al. 2024 ICES J. Mar. Sci.

*MeanSpawnLR:* Day of year of estimated mean date of spawning, as derived from larval otolith data.  
*VarSpawnLR:* Variance of the estimated day of year of spawning  
*MeanHatchLR:* Estimated mean day of larval hatching  
*MedSpawnLR:* Median day of spawning  
*DurSpawnLR:* Duration of the spawning season (95% of spawning in this window)  
*StartSpawnLR:* Day of 2.5th percentile of spawning distribution  
*EndSpawnLR:* Day of 97.5th percentile of spawning distribution  
*Spawn10LR:* Day of 10th percentile of spawning distribution  
*Spawn20LR:* Day of 20th percentile of spawning distribution  

**NCEP_reanalysisGOAmonthlySST_thru2021.csv**  
Monthly average sea surface temperature in the Gulf of Alaska.  
Produced at NOAA/ESRL PSD at http://www.esrl.noaa.gov/psd/data/timeseries/ with the following parameters:  
Surface Gauss SST(C)  
Latitude Range used: 56.2 to 54.3  
Longitude Range used: 202.5 to 208.1   
Area weighted grid

**ShelikofStartEndDates_to2021.csv**  
Dates for the start and end of the winter Shelikof acoustic-trawl survey conducted by the Midwater Acoustics and Conservation Engineering (MACE) Program at NOAA Alaska Fisheries Science Center.

**expected_shelikof_indices_Jan2022.csv**  
Predicted (model-estimated) survey biomass estimates for the winter Shelikof AT survey, as estimated by the ADMB stock assessment model used for providing management advice (specifically, the 2021 model; Monnahan, C. C., Dorn, M. W., Deary, A. L., Ferriss, B. E., Fissel, B. E., Jones, D. T., Levine, M., et al. 2021. Chapter 1: Assessment of the Walleye Pollock Stock in the Gulf of Alaska. In Stock assessment and fishery evaluation report for the groundfish resources of the Gulf of Alaska for 2021, pp. 1–126. North Pacific Fishery Management Council, Anchorage, AK. https://www.fisheries.noaa.gov/resource/data/2021-assessment-walleye-pollock-stock-gulf-alaska )  
Values are estimated biomass (ages 3+) in millions of tons.  
*Full_data_pred:* All survey data were used in assessment model  
*No_indices_pred:* Biomass indices for the Shelikof survey were removed when fitting the model  
*No_survey_pred:* Biomass indices and age-comp data were removed when fitting the model  
*Observed3p:* Actual survey-estimated biomass (age 3+) in millions of tons. Matches values in "obs_index1.txt"  

**obs_index1.txt**  
Estimates of pollock biomass (ages 3+) from the winter Shelikof acoustic-trawl survey conducted by the Midwater Acoustics and Conservation Engineering (MACE) Program at NOAA Alaska Fisheries Science Center.  
Columns are:  
*Year*  
*Survey-estimated biomass (age 3+) in tons*  
*CV used in assessment model*  
