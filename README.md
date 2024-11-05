# TriNetX & PEDSnet Data Quality Collaboration
This repository contains the code base for the data quality assessments developed in collaboration between TriNetX and PEDSnet. Four checks, two proposed by each team, were developed. The versions of the check in this repository are built
to run against an OMOP CDM instance. Please see below for instructions on how to execute each check.

## Executing the Data Quality Checks
### Setting up the environment
#### Relevant files: 
- [driver_setup.R](https://github.com/PEDSnet/trinetx_collab/blob/main/code/driver_setup.R)

The first step in executing any of the checks is setting up the environment. In `driver_setup.R`, first establish your top-level working directory. Then, we have provided a helper function called initialize_session to assist with connecting to your database. You will need to provide your database connection information, either by pointing to a json file containing your credentials or by providing a connection object established by DBI (or a suitable equivalent). Once the connection is established, and the additional parameters have been filled out to your liking, you can proceed to executing the data quality analyses!

### Case-Mix / Signatures
#### Relevant files:
- [cohort_case_mix.R](https://github.com/PEDSnet/trinetx_collab/blob/main/code/cohort_case_mix.R)
- [driver_case_mix.R](https://github.com/PEDSnet/trinetx_collab/blob/main/code/driver_case_mix.R)
- [case_mix_signatures.Rmd](https://github.com/PEDSnet/trinetx_collab/blob/main/reporting/case_mix_signatures.Rmd)
- [Visualization functions](https://github.com/PEDSnet/trinetx_collab/blob/main/code/cohort_viz.R#L169-L319)

The Case-Mix or Signatures check evaluates the distribution of ICD10CM headers for diagnoses within a group of patients. A specific cohort can be provided to target the analysis, but if no cohort is provided, the check will evaluate all available patients in the `condition_occurrence` table. 

The execution of the check plus some additional post processing can be found in `driver_case_mix.R`. See the [Anomaly Detection Methods](https://github.com/PEDSnet/trinetx_collab/blob/main/README.md#anomaly-detection-methods) section below for information on how outliers were detected.

### Couplets
#### Relevant files:
- [cohort_dcon.R](https://github.com/PEDSnet/trinetx_collab/blob/main/code/cohort_dcon.R)
- [dcon_execute.R](https://github.com/PEDSnet/trinetx_collab/blob/main/code/dcon_execute.R)
- [driver_couplets.R](https://github.com/PEDSnet/trinetx_collab/blob/main/code/driver_couplets.R)
- [couplets.Rmd](https://github.com/PEDSnet/trinetx_collab/blob/main/reporting/couplets.Rmd)
- [Visualization functions](https://github.com/PEDSnet/trinetx_collab/blob/main/code/cohort_viz.R#L75-L166)

The Couplets check evaluates the concordance between two user-defined cohorts. Our selected cohorts can be found in the `dcon_execute.R` file, and by following the same list structure, users can customize the inputs and include different cohort pairs. The check and subsequent post-processing will compute counts & proportions for 5 cohort member types: patients that only belong to cohort 1, patients that only belong to cohort 2, patients belonging to both cohorts, patients belonging to cohort 1 that also belong to cohort 2 (cohort 2 used as the denominator rather than the total number of patients), and patients belonging to cohort 2 that also belong to cohort 1 (cohort 1 used as the denominator rather than the total number of patients).

The execution of the check plus some additional post processing can be found in `driver_couplets.R`. See the 
[Anomaly Detection Methods](https://github.com/PEDSnet/trinetx_collab/blob/main/README.md#anomaly-detection-methods) section below for information on how outliers were detected.

### Stability Over Time
#### Relevant files:
- [cohort_fot.R](https://github.com/PEDSnet/trinetx_collab/blob/main/code/cohort_fot.R)
- [fot_execute.R](https://github.com/PEDSnet/trinetx_collab/blob/main/code/fot_execute.R)
- [driver_fot.R](https://github.com/PEDSnet/trinetx_collab/blob/main/code/driver_fot.R)
- [stability_over_time.Rmd](https://github.com/PEDSnet/trinetx_collab/blob/main/reporting/stability_over_time.Rmd)
- [Visualization functions](https://github.com/PEDSnet/trinetx_collab/blob/main/code/cohort_viz.R#L321-L478)

The Stability Over Time check evaluates the number of patients with a given fact for each month across a time span. Our selected fact types can be found in the `fot_execute.R` file, and by following the same list structure, users can customize the inputs and include different facts. The time span can also be adjusted as needed. 

The execution of the check plus some additional post processing can be found in the `driver_fot.R` file. See the 
[Anomaly Detection Methods](https://github.com/PEDSnet/trinetx_collab/blob/main/README.md#anomaly-detection-methods) section below for information on how outliers were detected.

### Coverage Overlap
#### Relevant files:
- [cohort_coverage_overlap.R](https://github.com/PEDSnet/trinetx_collab/blob/main/code/cohort_coverage_overlap.R)
- [coverage_overlap_execute.R](https://github.com/PEDSnet/trinetx_collab/blob/main/code/coverage_overlap_execute.R)
- [driver_coverage_overlap.R](https://github.com/PEDSnet/trinetx_collab/blob/main/code/driver_coverage_overlap.R)
- [coverage_overlap.Rmd](https://github.com/PEDSnet/trinetx_collab/blob/main/reporting/coverage_overlap.Rmd)
- [Visualization functions](https://github.com/PEDSnet/trinetx_collab/blob/main/code/cohort_viz.R#L481-L576)

The Coverage Overlap check evaluates the distribution and overlap of patient representation in each user-provided domain. Each patient is only counted one time in the appropriate intersection of fact types. Our execution (found in `coverage_overlap_execute.R`) looked at four domains (diagnoses, medications, procedures, and labs), and by following the same list structure, users can customize the inputs and include different domains.

The execution of the check plus some additional post processing can be found in the `driver_coverage_overlap.R` file. See the [Anomaly Detection Methods](https://github.com/PEDSnet/trinetx_collab/blob/main/README.md#anomaly-detection-methods) section below for information on how outliers were detected.

## Anomaly Detection Methods

### TriNetX Method
#### Relevant Code
- [Anomaly Computation](https://github.com/PEDSnet/trinetx_collab/blob/main/code/cohort_trinetx_anom.R)
- [Visualization](https://github.com/PEDSnet/trinetx_collab/blob/main/code/cohort_viz.R#L2-L71)

For each check aside from Stability Over Time, an IQR-based anomaly detection method developed by TriNetX is used to identify outliers. There are 3 steps which identify the outlier, grade it's severity, and summarize all outliers within an institution to develop a summary score. The steps are as follows:

1. The IQR is used to compute upper (`Q3 + (1.5*IQR)`) and lower (`Q3 + (1.5*IQR)`) limits. Any value that is greater than the upper limit or lesser than the lower limit
is considered an outlier.

2. The severity of an outlier is determined by dividing the value's distance from the nearest limit by the range between the upper and lower limits `severity_score = (value - upper/lower limit) / (upper limit - lower limit)`

3. An overall site severity score is also computed to provide a single value for each site in the input data. To compute this, each outlier severity score is multiplied by the mean for the group - `severity_score * (mean / 100)`.
All these scores are then summed to determine the overall `site_score`

### Euclidean Distance
#### Relevant Code
- [Anomaly Computation](https://github.com/PEDSnet/trinetx_collab/blob/main/code/cohorts.R#L363-L407)
- [Visualization](https://github.com/PEDSnet/trinetx_collab/blob/main/code/cohort_viz.R#L358-L478)

For Stability Over Time, a Euclidean Distance measure is used to identify outliers. In this computation, the average distance between the overall, all-site mean and the individual site value (with Loess regression applied) is computed 
across the time period. This outputs a single number per site that helps to identify if one institution's time series is very different from the aggregate or follows a similar trend to the other institutions in the computation.

## Concept Sets
All concept sets used to support our executions of the checks can be found in the [specs](https://github.com/PEDSnet/trinetx_collab/blob/main/specs/) directory. If you choose to include additional executions of a check that requires a new concept set, we recommend adding it to the specs directory.

