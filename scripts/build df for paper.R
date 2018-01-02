
df <- read_rds("extdata/default_subs_df.rds")

x <- list.files("extdata", pattern = "_input", full.names = T)

# 1514864275.63078 %>% lubridate::as_datetime()
# 1514865059.98424 %>% lubridate::as_datetime()
# 1514865327.90148 %>% lubridate::as_datetime()
# prop
n <-  6
x <- list.files("extdata", pattern = "_input_", full.names = T)  %>%
  map2(letters[1:n], ~ read_rds(.x) %>%
         as_tibble() %>%
         mutate(id = .y)) %>%
  bind_rows()

x <- x %>%
  mutate(kts = rep(df$kts, n))

ggplot(x, aes(x = kts, y= value, col = id)) +
  geom_line() +
  ylim(0, 1)



a <- lm(x %>% filter(id == "a") %>% pull(value) ~ poly(pull(df, kts), 4))$fitted.values
b <- lm(x %>% filter(id == "b") %>% pull(value) ~ poly(pull(df, kts), 4))$fitted.values
c <- lm(x %>% filter(id == "c") %>% pull(value) ~ poly(pull(df, kts), 3))$fitted.values
e <- lm(x %>% filter(id == "d") %>% pull(value) ~ poly(pull(df, kts), 3))$fitted.values
d <- lm(x %>% filter(id == "e") %>% pull(value) ~ poly(pull(df, kts), 4))$fitted.values
f <- lm(x %>% filter(id == "f") %>% pull(value) ~ poly(pull(df, kts), 3))$fitted.values



y <- x %>%
  mutate(eff = c(a,
                 b,
                 c,
                 d,
                 e,
                 f
  )) %>%
  mutate(System = case_when(
    id == "a" ~ "Jet 1",
    id == "b" ~ "Jet 2",
    id == "c" ~ "Jet 3",
    id == "d" ~ "Prop 1",
    id == "e" ~ "Prop 2",
    id == "f" ~ "Prop 3"
  ))

ggplot(y, aes(x = kts, y= eff, col = System)) +
  geom_line() +
  ylim(0, 1)

plotly::ggplotly()

y %>% write_rds("extdata/comb_eff_df.rds")
