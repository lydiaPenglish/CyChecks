#' Simplify professor titles for easy interpretation
#'
#' @param data A dataframe with professor titles. 
#'
#' @return A dataframe without adjunct, affiliation, emeritus, visiting, collaborating or department chairs, 
#' as their salaries tend to be weird, and a new column named `prof_simp` that has simplified names, see below. 
#' 
#' @details This function filters all titles to only include `PROF`, `ASSOC PROF`, and `ASST PROF`, and creates
#' a new category called `AWARDED PROF` which includes distinguished, morrill or university professors. 
#' 
#' @export
#'
#' @examples
#' data(salaries)
#' simplify_profs(data = salaries)
#' 

simplify_profs <- function(data = data){
  
  assertthat::see_if("title" %in% names(data), msg = "There needs to be a `title` column to simplify profs")
  
  awardprof <- stringr::str_to_upper(c("distg prof", "univ prof", "morrill prof"))
  
  df <- data %>%
  
  # eliminate department chairs, they are weird
  # eliminate adjuncts, things with 'adj' or 'affil' or 'emer' or 'vstg', they are paid strangely
  
  dplyr::filter(!grepl(stringr::str_to_upper("chair|adj|affil|emer|vstg|chr|clin|collab|res"), title)) %>%
  dplyr::filter(grepl("PROF", title)) %>%
  
  dplyr::mutate(prof_simp = ifelse(title %in% awardprof, "AWARDED PROF", title))
  
  return(df)
  
}
