

model <- lm(pull(df, kts) ~ poly(pull(df, eff.jet),3))

mod_df <- model$model

mod2_df <- pull(mod_df,2)

x <- mod2_df %>% as_tibble()

check2 <- tibble(y = pull(x, 3)) %>%
  mutate(kts = df$kts)
ggplot(check2, aes(x = kts, y = y))+
  geom_line()

model$residuals
check <- tibble(y = pull(x, 3)) %>%
  mutate(kts = df$kts)

ggplot(check, aes(x = kts, y = y))+
  geom_line()

w
