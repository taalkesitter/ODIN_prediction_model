# ODIN prediction model
This repository includes information on the development, internal validation and external evaluation of the ODIN prediction model. All included files are R-codes for R-version 4.5.2..

### Data Source
Data for both the development sample, as well as the external evlauation sample, comes from national registires in the respective countires. The sweidsh and the norwegian legislation prohibits sharing individual-level data publicly. Anybody, who is interested in replicating the study, can apply for data access thorugh the different registry holders.

### Data Sample
The model was developed in Swedish Data and externally evaluated in Norwegian Data. Details on the sample and the inclusion and exclusion criteria can be found in the published study. 

### Development  & Internal Validation
The files 1.1 and 1.2 contain code for data preparation and sample description of the Swedish sample 
The filed 2.1 to 2.4 contain the code for development and internal validation in the Swedish sample for the outcomes 1-year all-cause mortality (2.1), 30-days all-cause mortality (2.2), 1-year overdose mortality (2.3) and 30-days overdose-mortality(2.4). The developed models are Elastic Net models which are tuned and internally validated with a nested 10-fold cross-validation

### External Evaluation
File 3.1 and 4 contain code for the data preparation for the external evaluation of the Norwegian Sample and its description. 
File 5.1 to 5.4 contain the code for the external evaluation of the models for 1-year all-cause mortality (5.1), 30-days all-cause mortality (5.2), 1-year overdose mortality (5.3) and 30-days overdose-mortality (5.4) in the Norwegian sample. Every file shows hereby first the simple evaluation, the the recalibration of the model to the new setting, and finally the external evaluation of the recalibrated model.

### Subgroup Anlysis
File 3.2 contains code for the data preparation for the model evaluation in the subgroups of interest. 
File 6.1 to 6.4 contains code for the evaluation of the recalibrated models by place of birth (born inside/outside Norway)
File 7 contains code for the evalcuation of the recalibrated models by place of release (released form remand/released from a different place than remand)
File 8 contains code for the evaluation of the recalibrated models in the time prior to the first Covid-19 outbreak in Norway. 


