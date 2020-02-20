This directory contains the raw data that are used to construct the data.frames
in the package. Here is a brief rundown of the files and folders in this 
directory.

## Folders

- _raw/        raw data including department affiliations
- _tidy/       for intermediate files constructed by scripts
- _delete/     files to be deleted

- departments/ contains raw department data
- salaries/    contains raw salary data
- affiliation/ contains department affiliation

## Files

- 00_clean-ISU-dir-data.R        clean department affiliation data
- 01_get-raw-sals.R              download raw salary data from the Des Moines Register
- 02_make_combined_data.R        combine salary and department affiliation
- 03_calc_gender_fracs_by_dept.R ?
- 03_calc-gender-pcts-by-dept.R  ?
- 03_run_stats.R                 ?


- run_all.R         sources all other scripts in the proper order
  - departments.R   constructs departments data set
  - salaries.R      constructs salaries data set   
  - affiliation.R   constructions affiliation data set

