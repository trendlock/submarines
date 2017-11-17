

df3 <- tibble(ran.vbl = runif(50)) %>%
  mutate(row.id = row_number())
devtools::use_data(df3, overwrite = T)

