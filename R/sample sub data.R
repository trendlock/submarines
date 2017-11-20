
lenght_in <- 20

speeds <- c(1:lenght_in)  ## knots
hotel.1 <- c(rep(150,lenght_in))  ## kw
eff.jet <- c(0.08,0.125,0.2, 0.3, 0.35, 0.42, 0.46, 0.5, 0.56, 0.6, 0.62, 0.58, 0.57, 0.55, 0.54, 0.54, 0.54, 0.54, 0.54, 0.53)
#eff.jet <- c(0.05,0.07,0.1, 0.15, 0.2, 0.25, 0.28, 0.3, 0.33, 0.35, 0.37, 0.4, 0.42, 0.43, 0.45, 0.48, 0.52, 0.54, 0.55, 0.57)
total.batt <- 500 #Tonnes,
batt.dens <- 0.15

patrol <- 2
power.mob.drawn.jet.patrol <- hotel.1[1]
power.mob.req.patrol <- power.mob.drawn.jet.patrol*eff.jet[patrol]
const <- power.mob.req.patrol/(patrol^3)

df <- tibble(speeds, hotel.1, eff.jet)
colnames(df) <- c("speed", "hotel.1", "eff.jet")

df <- df %>%
  mutate(eff.prop = 1-log(speed+3)/6 +0.10 - speeds/500 ) %>%
  mutate(power.mob.req = const*speed^3,
         power.mob.drawn.jet = power.mob.req/eff.jet,
         power.mob.drawn.prop = power.mob.req/eff.prop,
         power.tot.jet = power.mob.drawn.jet + hotel.1,
         power.tot.prop = power.mob.drawn.prop + hotel.1,
         total.energy = total.batt*batt.dens*1000,  ##MJ
         endurance.prop.hour = total.energy*1000/power.tot.prop/3600,
         endurance.jet.hour = total.energy*1000/power.tot.jet/3600,
         range.prop = endurance.prop.hour*speed,
         range.jet = endurance.jet.hour*speed)

df2 <- df %>%
  gather(key = "tail", value = "value", endurance.prop.hour, endurance.jet.hour)

df2 <- df2 %>%
  head(20)

devtools::use_data(df2, overwrite = T)

