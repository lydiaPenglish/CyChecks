#' Departments
#' 
#' Information on departments including name, codes, address, phone number, etc.
#' 
#' @format A data.frame with 261 rows and the following columns:
#' \describe{
#'   \item{NUMERIC CODE}{integer, numeric code for department}
#'   \item{ALPHA CODE}{character, alpha code for department}
#'   \item{DIRECTORY NAME}{character, department name as used in the directory}
#'   \item{FULL NAME}{character, full department name}
#'   \item{ADDRESS}{character, address}
#'   \item{PHONE}{character, phone number}
#'   \item{RMM RESOURCE UNIT NUMBER}{integer, ??}
#'   \item{PARENT DEPT NUMBER}{integer, code for parental department}
#' }
#' @source \url{https://www.ir.iastate.edu/deptcodes}
#' 
"departments"


#' Salaries
#' 
#' Iowa State University salaries as downloaded from the Des Moines Register.
#' Base_salary has been cleaned to deal with hourly pay, hyphens, and commas.
#' A new column `pay_period` has been introduced to distinguish hourly pay from
#' yearly pay. No differentiation is made concerning A-base (12-month) and 
#' B-base (9-month) pay in the database. 
#' 
#' @format A data.frame with the following columns:
#' \describe{
#'   \item{year}{integer, fiscal year for this salary}
#'   \item{name}{character, individual's name}
#'   \item{gender}{character, [M]ale, [F]emale, or [*]}
#'   \item{place_of_residence}{character, county}
#'   \item{position}{character, name of position}
#'   \item{base_salary_date}{date-time, it seems to always be 1 July of that fiscal_year}
#'   \item{total_salary_paid}{numeric, total salary including summer pay, bonuses etc}
#'   \item{travel_subsistence}{numeric, reimbursement (?) for travel}
#'   \item{base_salary}{numeric, base pay}
#'   \item{pay_period}{character, 'hourly' or 'yearly'}
#' }
#' @source \url{https://db.desmoinesregister.com/state-salaries-for-iowa}
#' 
"salaries"


#' Department affiliation
#' 
#' Provides the link between individuals (by name) and their department 
#' (by numeric code) from 2012-2019.
#' 
#' @format A data.frame with the following columns:
#' \describe{
#'   \item{year}{integer, fiscal year}
#'   \item{NUMERIC CODE}{integer, department code}
#'   \item{name}{character, last_name first_name}
#' }
#' 
"affiliation"