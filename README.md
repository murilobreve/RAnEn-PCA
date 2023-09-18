# Weather Prediction Using Analog Ensemble (AnEn) Methods

This repository contains a script that facilitates weather prediction using Analog Ensemble (AnEn) methods.

## Overview

Written by **Murilo Montanini Breve**, the primary function in the script manages several tasks:

- **Data Loading**: Fetch data from `/data/predictor` or `/data/predicted` directories.
- **Data Organization**: Reformulate this data into a sequence of vectors.
- **Prediction**: Use either the AnEn or ClustAnEn methods to generate predictions.

## Prerequisites

Before executing the script, ensure that:

1. Required libraries are installed. These can be found in the `/R/settings/` directory.
2. Desired parameters are verified. Again, look in the `/R/settings/` directory.
3. Run the function `AnEn_methods.R`.

## Results

On successful execution, the script will return predictions based on the provided data and also compare the predicted values to real values, giving a measure of error.

## Additional Notes

The main function is the primary concern, and other secondary functions are utilized to support its operation but aren't described in detail here.

For more details, reach out to:
**Murilo Montanini Breve**
📧 [murilobreve@alunos.utfpr.edu.br](mailto:murilobreve@alunos.utfpr.edu.br)
📅 Last modification: 11/11/2022

## Usage

To use the main function, `AnEnMethods`, the following parameters are required:

- `params_list`: List of parameters to be used.
- `method`: Prediction method, default is "monache".
- `Pca`: Boolean value determining the use of Principal Component Analysis, default is TRUE.
- `predictors`: Number of predictors, default is 2.
- `variables`: Variables for prediction, default includes "WSPD" and "GST".
- `avail_threshold`: Threshold for data availability, default is 0.85.

```R
AnEnMethods(
  params_list = your_params_list,
  method = "monache",
  Pca = TRUE,
  predictors = 2,
  variables = c("WSPD", "GST"),
  avail_threshold = 0.85
)

