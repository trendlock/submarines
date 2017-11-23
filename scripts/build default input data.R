


# build defauts input data


df <- tibble(kts = seq(0.5, 20, 0.5))

# add user input jet eff

default_prop <- list.files("/Users/rosseji/dev/apps/subsApp", full.names = T, pattern = ".rds")[5] %>%
  read_rds()



default_jet <- list.files("/Users/rosseji/dev/apps/subsApp", full.names = T, pattern = ".rds")[5] %>%
  read_rds()

df <- df %>%
  mutate(eff.jet = default_jet,
         eff.prop = default_prop)

fit_poly_mod <- lm( pull(df, eff.jet)~ poly( pull(df, kts),3))
fit_poly_mod2 <- lm( pull(df, eff.prop)~ poly( pull(df, kts),3))

df <- df %>%
  mutate(eff.jet = fit_poly_mod$fitted.values,
         eff.prop = fit_poly_mod2$fitted.values)




