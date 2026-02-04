# Skeleton concept

## A structured approach to registry data integration

Creating analysis-ready datasets from health and social registries
requires systematic data engineering that addresses the complexity of
real-world data structures, evolving research questions, and changing
operational definitions. This vignette presents a structured, modular
framework that keeps things simpler and easier to reproduce, whether
you’re doing cross-sectional summaries or high-frequency longitudinal
analyses.

### The challenge

Real-world epidemiological analyses rarely involve simple two-variable
relationships. Consider a typical research question:

> “Estimate the effect of X on Y, adjusted for education, income, sex,
> birth country, comorbidity, and time since diagnosis.”

While conceptually straightforward, implementation presents significant
challenges:

- *Education* is recorded yearly in administrative databases, but you
  want the level “on the date of diagnosis.”
- *Comorbidity* might require scanning all inpatient and outpatient ICD
  codes across 10+ years to build a Charlson index.
- *Time since diagnosis* requires identifying the first occurrence of a
  disease and aligning everything to that timeline.
- *Income* needs to be inflation-adjusted and household-weighted.

Each variable requires complex data operations including multiple joins,
filters, date comparisons, and aggregation procedures.

A common approach involves constructing a wide person-level dataset with
pre-calculated variables (one record per person). This method functions
adequately for static analyses but becomes problematic when research
requirements evolve—such as stratification by calendar year,
incorporation of time-varying covariates, or implementation of exposure
lags.

Such modifications often require substantial dataset reconstruction.

### The skeleton framework

Rather than constructing the complete dataset simultaneously, this
approach begins with a structural *skeleton*.

The skeleton is a long-format table that defines the analytical unit:
one observation per person per time period (e.g., one row per person per
week). This provides the temporal foundation for subsequent data
integration. For example:

| id     | isoyearweek | isoyear |
|--------|-------------|---------|
| 100001 | 2010-01     | 2010    |
| 100001 | 2010-02     | 2010    |
| 100001 | 2010-03     | 2010    |
| 100002 | 2009-52     | 2009    |
| 100002 | 2010-01     | 2010    |

This structure contains:

- One observation per person-time combination within the analytical
  window
- No exposure, outcome, or covariate data initially
- Both weekly (isoyearweek) and yearly (isoyear) time units for flexible
  temporal aggregation

The temporal resolution (weekly, monthly, or daily) depends on
analytical requirements. For most registry-based epidemiological
studies, weekly resolution provides an optimal balance between precision
and computational efficiency.

Following skeleton construction, data are systematically integrated
through sequential operations. This includes:

- **Outcomes**: Binary indicators for events of interest (e.g.,
  myocardial infarction occurrence)
- **Exposures**: Treatment or intervention status (e.g., vaccination,
  hospitalization, benefit receipt)
- **Covariates**: Time-fixed (e.g., sex), semi-time-varying (e.g.,
  annual income), or high-resolution (e.g., new diagnoses)

Each data component is integrated through separate pipeline operations
using standardized joins and transformations. This modular approach
provides several advantages:

- Individual steps can be executed, debugged, or modified independently
- Data provenance remains transparent throughout the process
- The original temporal structure is preserved

When person-level aggregation is required for specific analyses (e.g.,
logistic regression, baseline tables), the skeleton can be collapsed at
the final stage.

### Temporal data classification

The skeleton framework accommodates three distinct temporal patterns in
registry data:

#### 1. Time-invariant data (demographics, baseline characteristics)

Variables that remain constant are propagated to **all temporal
observations** for each individual:

- Sex assigned at birth, birth country, genetic markers

#### 2. Periodically updated data (socioeconomic status, family structure)

Variables with regular update cycles are applied to **all observations
within the relevant period**:

- Annual income from tax records
- Family composition, marital status
- Education level (with potential temporal variation)
- Employment status

#### 3. Event-based data (diagnoses, prescriptions, deaths)

Variables tied to specific occurrences are assigned to **temporal
periods when events occurred**:

- Hospital admissions and diagnoses
- Prescription dispensing dates
- Surgical procedures
- Death dates and causes

These three categories handle the different ways registry data changes
over time.

### Methodological advantages

This approach has practical benefits:

- **Enhanced data quality verification**: Follow-up discontinuation is
  immediately apparent through missing temporal observations.
- **Flexible analytical modifications**: Exposure redefinition or
  temporal lags require modification of only the relevant processing
  layer.
- **Native time-varying covariate support**: Variables with different
  temporal resolutions (annual income, daily prescriptions) can be
  integrated through appropriate temporal joins.
- **Multiple outcome compatibility**: The skeleton structure supports
  simultaneous analysis of multiple endpoints.

**Application example:** Consider modeling sickness absence following
COVID-19 infection. The skeleton would span person-weeks from March 2020
to December 2022, with sequential integration of:

- Positive COVID-19 test dates from laboratory databases
- Inpatient diagnoses from hospital registers
- Sickness absence benefits from social insurance records
- Income and education from administrative data
- Age calculated dynamically by follow-up date
- Outcome variable: weekly sickness absence status (binary)

The resulting structure supports multiple analytical approaches
including time-to-event models, generalized estimating equations,
fixed-effects regressions, or conditional logistic regression—depending
on temporal aggregation and variable encoding strategies.

### The three-stage swereg workflow

swereg implements this skeleton concept through three distinct stages:

#### Stage 1: skeleton1_create (data integration)

- Create the time-structured skeleton
- Add raw registry data sequentially (demographics, diagnoses,
  prescriptions, etc.)
- Apply data standardization and medical code pattern matching
- Result: Raw data integrated into time structure

#### Stage 2: skeleton2_clean (data cleaning)

- Load skeleton1_create output
- Clean variables and create derived indicators
- Apply quality filters and validation rules
- Create composite variables and person-level summaries
- Result: Clean, analysis-ready variables

#### Stage 3: skeleton3_analyze (analysis preparation)

- Load skeleton2_clean output
- Aggregate weekly data to yearly (or other time units) as needed
- Create final analysis datasets optimized for specific research
  questions
- Implement memory-efficient processing for large populations
- Result: Final analysis datasets

Each stage is self-contained and can be debugged, modified, or rerun
independently.

### Software implementation

The `swereg` package provides functions for:

- Defining temporal skeletons for populations with specified follow-up
  periods
- Integrating and aggregating registry data within the skeleton
  framework
- Calculating exposures and outcomes from ICD, ATC, and other medical
  classification codes
- Performing temporal alignment (e.g., identifying income or education
  closest to diagnosis dates)
- Managing temporal relationships and calendar-year linkages

The package ensures:

- Consistent temporal handling (ISO week standardization, partial
  overlap logic)
- Transparent operational definitions (e.g., “hospital admission”
  criteria)
- Methodological reproducibility across research projects and teams

This standardized approach reduces repetitive data processing tasks, so
you can focus on analysis instead of data plumbing.

### Getting started

To learn swereg, follow the vignettes in order:

1.  **“Building the data skeleton (skeleton1_create)”** - Learn to
    create the time structure and integrate raw data
2.  **“Cleaning and deriving variables (skeleton2_clean)”** - Learn to
    clean data and create analysis variables  
3.  **“Production analysis workflows (skeleton3_analyze)”** - Learn to
    create final datasets and handle large-scale processing

Each vignette builds on the previous one, showing you how to implement
the skeleton concept in practice.

### Summary

High-quality analytical datasets require systematic construction rather
than ad-hoc processing. Registry-based variables are typically derived
through deliberate, transparent transformations rather than direct
extraction. The skeleton framework provides a structure that’s easy to
maintain and debug. The `swereg` package standardizes this workflow with
functions for common data processing tasks. By establishing temporal
foundations before data integration, you can focus on analysis instead
of data plumbing.
