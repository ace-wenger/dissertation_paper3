# Working title: *Explaining Heterogeneity in Science Education Research - Comparing machine learning models with a priori meta-regression models*

This project was completed as part of my dissertation for a Ph.D. in Science Education at Western Michigan University. 
My committee chair is Dr. William Cobern with Dr. Betty Adams and Dr. Ya Zhang serving as committee members.

> [!NOTE]
> This project is currently underway and all materials should be considered preliminary.
> If you have any questions or interest in this project please submit a new issue under the issues tab and I will do my best to respond.

## Purpose
As established in prior work, considerable heterogeneity exists in the research on concept mapping (CM) instructional interventions.
This heterogeneity is mostly unexplained and severely limits the recommendations that can be supported for teachers interested in applying CM. 
This project attempts to explain heterogeneity by building on prior work in three ways: 1) expanded scope of eligibility to increase the amount of available data, 2) multiple imputation of missing data to increase the amount of available data, and 3) application of machine learning to create robust data-driven models.

## Research Questions
1. What is the mean effect size and heterogeneity of CM instructional interventions for different school science subjects (e.g., biology/life sciences)? 
2. What is the extent of heterogeneity and how much is associated with each EMSI category?
3. How much heterogeneity can be explained:
   1. using a MUTOS-based, meta-regression model informed by previous findings?
   2. using a meta-regression model constructed through a machine learning algorithm?
4. What sample and intervention factors are associated with differences in CM efficacy?

## Ancillary Goals
This project aims to be computationally reproducible in that the data analysis is explicit and entirely scripted with no undocumented external dependencies or user inputs. 
This project also aims to be extensible in that analysis scripts are written as a series of functions that reliably perform one operation with no side effects (changes to the computational environment which may impact further operations).

In order to achieve these goals, the `renv` and `targets` packages are implemented.
`renv` records the R environment and validates future environments.
`targets` defines a data analysis pipeline and monitors the pipeline for changes.
Implicitly, the structure of a `targets` pipeline requires functions to be pure, a la functional programming.
Pure functions return identical values for identical arguments with no side effects.
Additionally, by using git and github, the project is version controlled with a consistent state that exists as the main branch in this repository.

The goal of reproducibility is not limited to the generation of results as contained in machine-read only objects, such as csv spreadsheets and `metafor` model objects. 
It is intended that the production of figures and tables will also be reproducible and integrated into the pipeline.

# Overview of repository structure (WIP)

- `_targets/` and `_targets.R`: pipeline script and subdirectory which contains pipeline objects and other files produced by `targets`
- `renv/` and `renv.lock`: file and subdirectory that record the R computatonal environment
- `protocol/`: subdirectory containing Quarto documents and associated files which document R scripts used outside of the pipeline and protocols for distinct project steps
- `data/`: subdirectory containing raw and processed data grouped in further subdirectories
- `R/`: subdirectory containing all defined functions used in this project
- `writing/`: subdirectory containing Quarto documents for preprint and journal manuscripts as well as intermediate reports

# Data Files (WIP)
- `data_rec_compiled` either one compiled file or the raw inputs for deduplication and compilation
- `data_rec_raw`
- `data_es_raw`

# Analysis Scripts and Pipeline (WIP)
Below, each major section of the data analysis pipeline is explained and every target is listed.

## Data Collection: Protocols and Scripts

### Protocols
- [ ] 01_criteria_search_strategy
- [ ] 02_lit_search_abstract_screening
- [ ] 03_fulltext_retrieval_screening
- [ ] 04_fulltext_coding

### Scripts
- [ ] 11_litsearch_phase1: database searches
- [ ] 12_litsearch_phase2: conference proceedings
- [ ] 13_litsearch_phase3: citation chasing

## Pipeline 1: Data Processing and Description

### Search and Screen Results
- [ ] `data_rec_raw`: tabulation of search results
- [ ] `figure_PRISMA`: production of PRISMA diagram
- [ ] `report_search`: render of parameterized basic search analysis

### Full-text Coding Results
- [ ] `data_es_raw`:
- [ ] `data_es_proc`:
- [ ] `table_coding`:
- [ ] `report_coding`: descriptive analysis of coding results
- [ ] `report_missing`: missingness analysis

## Pipeline 2: Multiple Imputation and Diagnostics

### Multiple Imputation

### Influential Cases, Checking Assumptions

## Pipeline 3: Meta-Regression Modeling

## Pipeline 4: Random Forest Machine Learning Modeling
