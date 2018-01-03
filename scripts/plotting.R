library(submarines)
library(dev)
library(plotly)


vec <- seq(0, 2, 0.05)
df <- tibble(vec)
df <- df %>%
  mutate(eta = submarines::efficiency(deltaVonV = vec))
colnames(df) <- c("deltaVonV", "eta")

plot <- df %>%
  ggplot(aes(x = deltaVonV, y = eta))+
  geom_line()
ggplotly(plot)

speed <- seq(0,20,0.5)
df2<- tibble(speed)
df2<- df2 %>%
  mutate(thrust.req = speed^2,
         A1 = 5,
         WT = 0.025*speed,
         speed.advance =speed - WT*speed,
         Q = A1*speed.advance,
         deltaV = thrust.req/Q,
         deltaVprop = deltaV/speed.advance,
         eta = submarines::efficiency(deltaVonV = deltaVprop))


