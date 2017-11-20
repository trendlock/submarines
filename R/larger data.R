




knots_df <- tibble(
  kts = seq(0.5, 20, 0.5),
  eff.prop = NA
)

devtools::use_data(knots_df, overwrite = T)
