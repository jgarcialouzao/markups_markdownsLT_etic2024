This readme explains the logical sequence to replicate the main results in "The Dynamics of Product and Labor Market Power: Evidence from Lithuania" by Ziran Ding, Jose Garcia-Louzao and Valentin Jouvanceau


## Data access
The primary data used in the analysis are confidential and therefore are not provided.
The data are kept at the facilities of the Bank of Lithuania. Researchers can apply for the visit program and obtain access to the dataset (https://www.lb.lt/en/ca-visiting-researcherprogramme).


## Replication files
The full set of results can be obtained running the `Master_File.do` program. This program includes the folliwing sub-programs:

* `data_extraction.do`      - opens annual files and set up a panel of firms between 2004 and 2018 with key variables, requires associated ado files: `xlsfirms_new.ado` and `xlsfirms.ado` 
* `prog_translog.do`        - sets up estimation sample and performs translog production function estimation, requires associated ado file: `dlw_TL_LL.ado`
* `prog_cobbdoublas.do`     - sets up estimation sample and performs Cobb-Douglas production function estimation, requires associated ado file: `dlw_CD_LL.ado`
* `prog_descriptives.do`    - produces basic descriptive statistics 
* `prog_mu_micro.do`        - creates markup specific summary statistics
* `prog_md_micro.do`        - creates markdown specific summary statistics
* `prog_het_regression.do`  - estimates firm-level regressions of markups and markdowns on selected firm-level characteristics
* `prog_mumd_agg.do`        - produces aggregate values of markups and markdowns and creates figures [`prog_mumd_agg_cd` for the Cobb-Douglas version]
* `prog_OP.do`              - performs Olley and Pakes decomposition along with additional sector-level measures of concentration 
* `prog_FHK.do`             - performs Foster, Haltiwanger, and Krizan decomposition







