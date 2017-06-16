#' Report whether a drug appears between start and end date
#
#' @param df dataframe of pis data in common format
#' @param bnf_code bnf code for drug to the level of interest (e.g. '01', '0101', '0101010G0BNAABY', etc)
#' @start_date start date for period of interest
#' @param end_date end date for period of interest
#' @return data_frame (prochi, ???)


ever_used <- function(df, bnf_code, start_date, end_date){

  # Pre-conditions
  stopifnot(is.data.frame(df))
  stopifnot(end_date >= start_date)
  stopifnot(is.character(bnf_code))
  stopifnot(str_length(bnf_code) <= 15) # Note: allow 0 length string for has any prescription


  return (
    df %>%
      filter(disp_date > start_date & disp_date < end_date) %>%
      select(prochi, disp_date, bnf_item_code) %>%
      mutate(is_match = ifelse(str_sub(bnf_item_code, 1, str_length(bnf_code)) == bnf_code, 1, NA)) %>%
      group_by(prochi) %>%
      summarise(has_drug=ifelse(sum(!is.na(is_match))>0, 1, 0))
  )


}
