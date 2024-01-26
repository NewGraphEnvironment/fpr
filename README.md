# Fish Passage Reporting
And much more... Package used to clean data, QA, and for building [interactive reports](https://github.com/NewGraphEnvironment/dff-2022/blob/master/docs/Aquatic_restoration_and_fish_passage_resources.pdf) usually related to fish passage planning, implementation and monitoring.  

<br>

Currently under active development. You will want to disect each function to understand things like the folder structure of the repo used to build the actual report. Use at your own risk and consider keeping track of which version of the package you are using through the date formatted tags. 

<br>

Install with `pak::pkg_install("NewGraphEnvironment/fpr")`

<br>

If looking for backwards comparability because you used the package in the past and there are issues now with a newer release of the package, install with:

`pak::pkg_install("NewGraphEnvironment/fpr@v1.1.0")` (tag coinciding with the one you used to report)

<br>

The reporting that this package serves to help line up is generated with `bookdown` from `Rmarkdown` so there are many dependencies such as `knitr` and `KableExtra`. Additionally, of key importance workflows rely on provincial British Columbia data input spreadsheets with version updates.  As of 2022-04-13 the provincial templates included:

 + Populated British Columbia standardized [Fish Data Submission Spreadsheet Template](https://www2.gov.bc.ca/gov/content/environment/plants-animals-ecosystems/fish/fish-and-fish-habitat-data-information/fish-data-submission/submit-fish-data#submitfish) 

 + Populated British Columbia standardized [Provincial Stream Crossing Inventory System submission template](https://www2.gov.bc.ca/gov/content/environment/plants-animals-ecosystems/fish/aquatic-habitat-management/fish-passage/fish-passage-technical/assessment-projects)
 
 <br>
 
 
Additionally many outputs manipulated by these functions rely on a tables generated from a `postgresql` database built with [`bcfishpass`](https://github.com/smnorris/bcfishpass). Some tables are unstable with column names likely to change in the future.

<br>

Custom inputs include:

 + Custom CSV file [example here](https://github.com/NewGraphEnvironment/fish_passage_skeena_2021_reporting/blob/master/data/habitat_confirmations_priorities.csv) detailing Phase 2 site:
     - priority level for proceeding to design for replacement
     - length of survey upstream and downstream
     - a conservative estimate of the linear length of mainstem habitat potentially available upstream of the crossing 
     - fish species confirmed as present upstream of the crossing following assessments.


 + Photos within structured folders and naming conventions as per the Provincial Stream Crossing Information System and project specific protocols [example here](https://github.com/NewGraphEnvironment/fish_passage_skeena_2021_reporting/tree/master/data/photos) 
