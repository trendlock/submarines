
df <- tibble(kts = seq(0.5, 20, 0.5))



default_jet <- read_rds("/Users/rosseji/dev/packages/submarines/extdata/saved_jet_input.rds")
default_prop <- read_rds("/Users/rosseji/dev/packages/submarines/extdata/saved_prop_input.rds")

df <- df %>%
  mutate(eff.jet = default_jet,
         eff.prop = default_prop)

fit_poly_mod <- lm( pull(df, eff.jet)~ poly( pull(df, kts),3))
fit_poly_mod2 <- lm( pull(df, eff.prop)~ poly( pull(df, kts),3))

df <- df %>%
  mutate(eff.jet = fit_poly_mod$fitted.values,
         eff.prop = fit_poly_mod2$fitted.values)


write_rds(df, "/Users/rosseji/dev/apps/subsApp/extdata/default_subs_df.rds")

