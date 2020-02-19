#' Simplify professor titles
#'
#' @param data A dataframe that can be simplified. 
#'
#' @return A dataframe without adjunct, affiliation, emeritus, visiting, collaborating or department chairs, 
#' as their salaries tend to be weird. 
#' @export
#'
#' @examples
#' 
#' 

simplify_profs <- function(data = cyd_saldept){
  
  awardprof <- c("distg prof", "univ prof", "morrill prof")
  
  df <- data %>%
  
  # eliminate department chairs, they are weird
  # eliminate adjuncts, things with 'adj' or 'affil' or 'emer' or 'vstg', they are paid strangely
  
  filter(!grepl("chair|adj|affil|emer|vstg|chr|clin|collab|res", title)) %>%
  filter(grepl("prof", title)) %>%
  
  mutate(prof_simp = ifelse(position %in% awardprof, "awarded prof", title)) %>% 
  filter(!is.na(dept))
  
  return(df)
  
}