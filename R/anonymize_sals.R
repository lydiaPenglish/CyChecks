#' Option to anonymize salary data
#'
#' @param df A dataframe that needs to be anonymized
#' @param col_to_anon The column name that should be anonymized, usually "name" or other identifying feature
#' @param algo The algorithm that will be used to anonymize. 
#'
#' @return A dataframe that replaces the identifying column with an anonymized one  
#' @export
#'
#' @examples
#' 

anonymize_sals <- function(df, col_to_anon = "name", algo = "crc32"){
  
  assertthat::not_empty(df)
  assertthat::see_if(col_to_anon %in% names(df), msg = "The selected column isn't in the dataframe")
  assertthat::assert_that(is.data.frame(df),
                          is.character(algo),
                          nrow(df) > 0)
  
  to_anon <- dplyr::select(df, col_to_anon)
  
  ids <- unname(apply(to_anon, 1, digest::digest, algo = algo))
  
  df2 <- df %>%
    dplyr::mutate(id = ids) %>%
    # get rid of identifying column
    dplyr::select(-col_to_anon)
  
  return(df2)
}
