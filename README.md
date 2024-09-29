# **HCP Structure Analysis**
### *Contributors*: Yi Wei @ University of Maryland  
#### *Maintained by*: Stephen Shin @ University of Maryland & Nate Mearns-Escobar @ University of Maryland

---

## **Table of Contents**
- [**HCP Structure Analysis**](#hcp-structure-analysis)
    - [*Contributors*: Yi Wei @ University of Maryland](#contributors-yi-wei--university-of-maryland)
      - [*Maintained by*: Stephen Shin @ University of Maryland \& Nate Mearns-Escobar @ University of Maryland](#maintained-by-stephen-shin--university-of-maryland--nate-mearns-escobar--university-of-maryland)
  - [**Table of Contents**](#table-of-contents)
- [Project Directory Structure](#project-directory-structure)
  - [01-master-csv](#01-master-csv)
  - [02-transform-sh](#02-transform-sh)
  - [03-transformed-csv](#03-transformed-csv)
  - [04-models-sh](#04-models-sh)
  - [05-zaratan-sh](#05-zaratan-sh)
  - [06-bayes-results](#06-bayes-results)
  - [07-imagegen-sh](#07-imagegen-sh)
  - [08-tablegen-sh](#08-tablegen-sh)
  - [09-assets](#09-assets)
  - [Description of Tasks](#description-of-tasks)
      - [RAVLT](#ravlt)
      - [FLANKER](#flanker)
- [Resources](#resources)

---

# Project Directory Structure

This project consists of several directories that contain data, scripts, models, and results. Below is a brief description of the purpose of each numbered folder. You can click on each folder name to navigate to it.

## [01-master-csv](./01-master-csv)
Contains the master CSV file that serves as the raw data source for this project.

- `Master_CSV_aging_neural.csv`: Main dataset containing aging and neural data.

## [02-transform-sh](./02-transform-sh)
Contains scripts for transforming the raw data into a format suitable for analysis.

- `main_0_prepare_masterdata.R`: Prepares [data_brms_log_normalized.RDS](./03-transformed-csv/data_brms_log_normalized.RDS) from [Master_CSV_aging_neural.csv](./01-master-csv/Master_CSV_aging_neural.csv)
- `main_data.R`: Prepares [data_brms_log.RDS](./03-transformed-csv/data_brms_log.RDS) from [Master_CSV_aging_neural.csv](./01-master-csv/Master_CSV_aging_neural.csv)

## [03-transformed-csv](./03-transformed-csv)
Stores transformed and cleaned CSV files, ready for modeling.

- `data_brms_log.RDS`: Transformed data for use with the `brms` package.
- `data_brms_log_normalized.RDS`: Normalized version of the log transformed data.

## [04-models-sh](./04-models-sh)
Contains the model scripts for various analyses.

- `main.R`: Main script for running models.
- `md_2min.R`: Model for 2-minute data analysis.
- `md_FnR.R`: Model for Flanker and RAVLT analysis.
- `md_cog.R`: Cognitive model
- `md_flanker.R`: Flanker model
- `md_ravlt.R`: RAVLT model

## [05-zaratan-sh](./05-zaratan-sh)
Contains SLURM scripts for job submission on the Zaratan cluster.

- `sbatch.sh`: SLURM batch script for submitting jobs that can be run as `sbatch.sh $nameofRmodel.R`
- `slurm.sh`: Runs `sbatch.sh` with an easier interface in the command line.

## [06-bayes-results](./06-bayes-results)
Contains the results from Bayesian models.

## [07-imagegen-sh](./07-imagegen-sh)
Contains scripts and files related to generating images from the results.

- `get_pvalue_rgb.R`: Script to generate p-value-based RGB images.
- `lh.aparc.annot`: Left hemisphere annotation file.
- `rh.aparc.annot`: Right hemisphere annotation file.

## [08-tablegen-sh](./08-tablegen-sh)
Contains scripts for generating tables from the results.

- `make_value_table.R`: Script to create value tables from the results.

## [09-assets](./09-assets)
A general folder for storing final assets related to the project.

## Description of Tasks

Some of the data vectors are based on the [Human Connectome Project Standards](https://www.humanconnectome.org/storage/app/media/documentation/LS2.0/LS_2.0_Release_Appendix_2.pdf)

These are some of the tasks involved in the study:

#### RAVLT
The Rey Auditory Verbal Learning Test (RAVLT) measures how well subjects can remember and learn words. It looks at different aspects of memory, like short-term memory, learning strategies, and how well people can recall information after a delay. The RAVLT uses two unrelated world lists (List A and B). The first 5 trials prompt the subject with words from List A, with the subject then being tasked with recalling as many words from the list as possible. The 6th trial prompts the subject with words from List B, with the subject then tasked with recalling as many words from the list as possible. Lastly, in the 7th trial, the subject is asked to recall as many words from list A without hearing the list again. The interfering list (List A for trial 6, List B for trial 7) is tracked and scored alongside the primary list of interest, in case the subject recalls any interfering words. This test was administered through RAVLT using an iPad via the Q-Interactive platform.

#### FLANKER
The Flanker task measures the subject’s attentive ability and inhibitory control. The subject is tasked with focusing on a primary stimuli while simultaneously ignoring additional flanking stimuli. Ages 8-85 are given 20 trials with arrows, while ages 3-7 receive 20 additional trials with arrows if they score ≥90% on the fish stimuli (with no more than one congruent and one incongruent trial incorrect). The Flanker is a good measure of executive function. The skills assessed in this task are considered to be “fluid abilities,” with performance peaking in early adulthood and declining throughout the rest of the lifespan. Higher scores indicate higher levels of ability to pay attention while omitting distractions.

# Resources
[Bids Documentation](https://bids-specification.readthedocs.io/en/stable/introduction.html)

[Bids Overview](https://oesteban.github.io/ohbm19/#1)
[Bids Checker](https://bids-standard.github.io/bids-validator/)

BIDS was originally designed to describe and apply consistent naming conventions to raw (unprocessed or minimally processed due to file format conversion) data.