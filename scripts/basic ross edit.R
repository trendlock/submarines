
library(plotly)

df <- tibble(kts = seq(0.5, 20, 0.5))

# add user input jet eff

df <- df %>%
  mutate(eff.jet = c(read_rds("extdata/sample_jet_eff.rds"), read_rds("extdata/sample_jet_eff.rds") %>% tail(1)))

fit_poly_mod <- lm( pull(df, eff.jet)~ poly( pull(df, kts),3))

df <- df %>%
  mutate(eff.jet = fit_poly_mod$fitted.values,
         eff.prop = 1 - log(kts + 3)/6 + 0.10 - (kts/500))




# save default

write_rds(df, "extdata/default_subs_df.rds")

ggplot(df, aes(x = kts, y = eff.jet)) +
  geom_line()

# Params ====

hotel <- 150
total.batt <- 500 #Tonnes,
batt.dens <- 0.15

patrol <- 2.5


max.speed = 20
max.power = 7000
speed = 10
power = 1000
system = "jet"
method = "max power"


df <- df %>%
  mutate(hotel)

const <- calibrator(df,
                    system = system,
                    method = method,
                    hotel = hotel,
                    patrol = patrol,
                    max.speed = max.speed,
                    max.power = max.power,
                    speed = speed,
                    power = power)


df <- df %>%
  mutate(power.mob.req = const * kts^3,
         power.mob.drawn.jet = power.mob.req / eff.jet,
         power.mob.drawn.prop = power.mob.req / eff.prop,
         power.tot.jet = power.mob.drawn.jet + hotel,
         power.tot.prop = power.mob.drawn.prop + hotel,
         total.energy = total.batt * batt.dens * 1000,  ##MJ
         endurance.prop.hour = total.energy*1000/power.tot.prop/3600,
         endurance.jet.hour = total.energy*1000/power.tot.jet/3600,
         range.prop = endurance.prop.hour * kts,
         range.jet = endurance.jet.hour * kts)

df <- df %>%
  gather("cat", "val", 2:ncol(.))

# Eff  ====

main_plot_df <- df %>%
  filter(cat %in% c("eff.jet", "eff.prop"))


ggplot(main_plot_df, aes(x = kts, y = val, col = cat))+
  geom_line()+
  labs(title = "Efficiency Assumption") +
  theme_classic()


ggplotly()%>%
  layout(legend = list(orientation = 'v', y = 0.9, x = 0.7))


# Endurance ====

end_plot_df <- df %>%
  filter(cat %in% c("endurance.prop.hour", "endurance.jet.hour"))

ggplot(end_plot_df, aes(x = kts, y = val, col = cat))+
  geom_line()+
  #geom_text(aes(label = round(val, digits = 1)), nudge_y = 2, angle = 45)+
  labs(title = "Endurance") +
  theme_classic()


ggplotly()%>%
  layout(legend = list(orientation = 'v', y = 0.9, x = 0.7))


# Range ====

range_plot_df <- df %>%
  filter(cat %in% c("range.prop", "range.jet"))

ggplot(range_plot_df, aes(x = kts, y = val, col = cat))+
  geom_line()+
  #geom_text(aes(label = round(val, digits = 1)), nudge_y = 2, angle = 45)+
  labs(title = "Range") +
  theme_classic()


ggplotly()%>%
  layout(legend = list(orientation = 'v', y = 0.9, x = 0.7))



# Pwer ====

power_plot_df <- df %>%
  filter(cat %in% c("hotel","power.mob.drawn.jet", "power.mob.req", "power.mob.drawn.prop"))

ggplot(power_plot_df, aes(x = kts, y = val, col = cat))+
  geom_line()+
  labs(title = "Power") +
  theme_classic()


ggplotly()%>%
  layout(legend = list(orientation = 'v', y = 0.8, x = 0.1))

