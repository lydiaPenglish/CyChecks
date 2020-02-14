#' Get Iowa State University salaries
#' 
#' This function will obtain Iowa State University salaries from the Des Moines
#' Register data base. It uses an API token which you will need to request from
#' here.
#' 
#' Each row contains a person-year. 
#' 
#' @param token API token
#' @param limit max number of entries
#' @param offset where to start gathering (useful if you need to get the data in batches)
#' @return A data.frame containing the following columns:
#' \describe{
#'   \item{fiscal_year}{numeric, fiscal year for this observation}
#'   \item{name}{character, individual's name}
#'   \item{gender}{character, Male, Female, or *}
#'   \item{position}{character, position}
#'   \item{base_salary_date}{Date}
#'   \item{total_salary_paid}{numeric, total salary including bonuses and summer salary}
#'   \item{travel_subsistence}{numeric, payment for travel}
#'   \item{base_salary}{numeric, base salary}
#' }
#' 
#' @source <https://db.desmoinesregister.com/state-salaries-for-iowa/>
#' @export
#' 
cyf_getsals <- function(token, limit = 200000, offset = 0) {
  url <- sprintf("https://data.iowa.gov/resource/s3p7-wy6w.json?%s&$limit=%d&$offset=%d&$order=:id&department=Iowa%%20State%%20University", 
                 token = token, 
                 limit = limit, 
                 offset = offset)
  
  
  jsonlite::fromJSON(url)
}
