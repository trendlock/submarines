
library(tidyverse)
df <- read_rds("/Users/rosseji/dev/packages/submarines/extdata/default_subs_df.rds")

ggplot(df, aes(x = kts, y = eff.jet))+
  geom_line()

model <- lm( pull(df, eff.jet)~ poly( pull(df, kts),3))


df2 <- bind_cols(df, tibble(poly.fit = model$fitted.values))
df_gath <- df2 %>%
  gather("cat", "val", 2:ncol(.))
ggplot(df_gath, aes(x = kts, y = val, col = cat)) +
  geom_line()

model$fitted.values
model$

mod_df <- model$model

mod2_df <- pull(mod_df,2)

x <- mod2_df %>% as_tibble()

check2 <- tibble(y = pull(x, 2)) %>%
  mutate(kts = df$kts)
ggplot(check2, aes(x = kts, y = y))+
  geom_line()
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
