# NeuroBridges 2024 project on Drift Diffusion Modeling 

This repository contains code and data for modeling the effect of rumination on performance in the Stop-Signal Task, created for the NeuroBridges 2024 summer school in Cluny, France.


## Repository Structure

- `R/`: R scripts for data preprocessing, analysis, and modeling
  - `simple_ddm.R`: Implementation of a simple drift diffusion model using the RWiener package in R
  - `rumination_correlations.R`: Computes correlations between rumination sub-scores and behavioural measures

- `data/`: Processed data files
  - `Data_rumination_CF.csv`: Rumination data at the group level (i.e., aggregrated for each participant).

- `output/img/`: Output images and plots
  - Various plots related to model parameters, correlations, and distributions

- `processed_data/`: Intermediate processed data files
  - `stop_signal_preprocessed_int_rumination_ssrt.csv`: Preprocessed SST data with rumination and SSRT

- `raw_data/`: Raw data files
  - Questionnaire data (ERQ, ERRI, RRS)
  - Task data (NBack, StopSignal)
  - Demographic data

## Usage

1. Run the drift diffusion model using `run_sst_model.R`
2. Generate plots and analyze results using the R scripts in the `output/img` directory

## Requirements

- R (Version 2024.04.2+76 or higher)
- Required R packages: dplyr, tidyr, rtdists, ggplot2, RWiener, purrr, tidyverse

## License

This project is licensed under the terms of the LICENSE file in the repository root.

## Acknowledgements

Original code provided by Gaya Aran (Hebrew university of Jerusalem). Data and model code written by Aamir Sohail (University of Birmingham).