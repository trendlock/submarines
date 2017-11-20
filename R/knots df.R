
knots_df <- tibble(
  knots = seq(1, 20, 0.25),
  eff.prop = NA
)

devtools::use_data(knots_df, overwrite = T)
