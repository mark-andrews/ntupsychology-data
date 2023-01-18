get_dummy_code <- function(Df, variable){
  tmp_df <- fastDummies::dummy_cols(Df, remove_first_dummy = TRUE)
  tmp_df <- dplyr::select(tmp_df, dplyr::starts_with(variable))
  dplyr::arrange(dplyr::distinct(tmp_df), !!variable)
}
