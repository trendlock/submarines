

get_out <- read_rds("extdata/sample_jet_eff.rds")

get_out <- get_out %>%
  map_dbl( ~ .x + 0.1)
write_rds(get_out, "extdata/sample_jet_eff.rds")
