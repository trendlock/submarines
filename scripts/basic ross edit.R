library(dev)
library(plotly)


######### SCENARIO 1   ##############


df <- tibble(kts = seq(0.5, 20, 0.5))

# add user input jet eff

df <- df %>%
  mutate(eff.jet = c(0, read_rds("extdata/sample_jet_eff.rds")))



model <- lm(pull(df, kts) ~ poly(pull(df, eff.jet),3))

check <- tibble(model$residuals)

check <- p
ggplot(check, aes(x = kts, y = eff.jet))+
  geom_line()
#######  NUMBERS TO ADJUST BY USE IN APP ##########
hotel <- 150
total.batt <- 500 #Tonnes,
batt.dens <- 0.15

#######
patrol <- 2.5

# There are a few different ways of providing a reference point for the power consumption for a given propulsion type.
# 1. Nominate a speed at which power consumption for propulsion is equal to hotel load, as per Andrew Davies article
# 2. Nominate a max speed and a maximum motor power output
# 3. Nominate a speed and power consumption, irrespective from hotel load.

###################



df <- df %>%
  mutate(hotel)

df <- df %>%
  mutate(eff.prop = 1 - log(kts + 3) / 6 + 0.10 - kts / 500)

### find the constant here

const <- calibrator(df, system = "prop", method = "max power",
                    hotel = hotel,
                    patrol = patrol,
                    max.speed = 20,
                    max.power = 7000,
                    speed = 10,
                    power = 1000
                    )

df <- df %>%
  mutate(power.mob.req = const * kts^3,
         power.mob.drawn.jet = power.mob.req / eff.jet,
         power.mob.drawn.prop = power.mob.req / eff.prop,
         power.tot.jet = power.mob.drawn.jet + hotel,
         power.tot.prop = power.mob.drawn.prop + hotel,
         total.energy = total.batt * batt.dens * 1000,  ##MJ
         endurance.prop.hour = total.energy * 1000 / power.tot.prop / 3600,
         endurance.jet.hour = total.energy * 1000 / power.tot.jet / 3600,
         range.prop = endurance.prop.hour * kts,
         range.jet = endurance.jet.hour * kts)

df <- df %>%
  gather("cat", "val", 2:ncol(.))

cats <- df %>% pull(cat) %>% unique()

main_plot_df <- df %>%
  filter(cat %in% c("eff.jet", "eff.prop"))



ggplot(main_plot_df, aes(x = kts, y = val, col = cat))+
  geom_line()

ggplotly()



  labs(title = "Efficiency Assumption")+
  theme(axis.title.y = element_blank())
ggplotly(plot)%>%
  layout(legend = list(orientation = 'v', y = 0.9, x = 0.7))

  ## - speeds/120

plot <- df2 %>%
  ggplot(aes(x = speed, y = value, col = tail))+
  geom_line()+
  geom_point()+
  geom_text(aes(label = round(value, digits = 1)), nudge_y = 2, angle = 45)+
  #geom_text(aes(label = round(value, digits = 1)), position = position_dodge(4))+
  labs(title = "Endurance")+
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(legend.title=element_blank())
ggplotly(plot) %>%
  layout(legend = list(orientation = 'v',y = 0.8, x = 0.7))

# plot <- df %>%
#   ggplot(aes(x = speed))+
#   geom_line(aes(y = endurance.prop.hour, col = "Propeller Endurance"))+
#   geom_text()+
#   geom_line(aes(y = endurance.jet.hour, col = "Jet Endurance"))+
#   labs(title = "Endurance Comparison")+
#   #theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
#   theme(legend.title=element_blank())
# ggplotly(plot) %>%
#   layout(legend = list(orientation = 'v',y = 0.8, x = 0.7))

plot <- df %>%
  ggplot(aes(x = speed))+
  geom_line(aes(y = range.prop, col = "Propeller Range nm"))+
  geom_line(aes(y = range.jet, col = "Jet Range nm"))+
  labs(title = "Range")
ggplotly(plot) %>%
  layout(legend = list(orientation = 'v', y = 0.8, x = 0.6))

plot <- df %>%
  ggplot(aes(x = speed))+
  geom_line(aes(y = hotel, col = "hotel")) +
  geom_line(aes(y = power.mob.drawn.jet, col = "power drawn by jet"))+
  geom_line(aes(y = power.mob.req, col = "power thrust required"))+
  geom_line(aes(y = power.mob.drawn.prop, col = "power drawn by prop"))+
  labs(title = "Power")
ggplotly(plot) %>%
  layout(legend = list(orientation = 'v', y = 0.8, x = 0.1))


############# SCENARIO 2 #############

speeds <- c(1:20)  ## knots

detailed <- speeds %>% tibble()
colnames(detailed) <- c("kts")
detailed <- detailed %>%
  mutate(halves = kts - 0.5)

eff <- c(detailed$kts, detailed$halves) %>% tibble()
colnames(eff) <- c("speed")
eff <- eff %>%
  arrange(speed) %>%
  filter(speed > 0)
speeds <- eff$speed

hotel <- c(rep(150,40))  ## kw
#eff.jet <- c(0.08,0.125,0.2, 0.3, 0.35, 0.42, 0.46, 0.5, 0.56, 0.6, 0.62, 0.58, 0.57, 0.55, 0.54, 0.54, 0.54, 0.54, 0.54, 0.53)
eff.jet <- c(0.05,0.07,0.1, 0.15, 0.2, 0.25, 0.28, 0.3, 0.33, 0.35, 0.37, 0.4, 0.42, 0.43, 0.45, 0.48, 0.52, 0.54, 0.55, 0.57)
eff.jet.2 <- eff.jet + c(rep(0.01, 20))
new.eff.jet <- tibble(eff.jet, eff.jet.2) %>%
  gather() %>%
  arrange(value)
eff.jet <- c(new.eff.jet$value)

total.batt <- 500 #Tonnes,
batt.dens <- 0.15

patrol <- 3
power.mob.drawn.jet.patrol <- hotel[1]
power.mob.req.patrol <- power.mob.drawn.jet.patrol*eff.jet[patrol]
const <- power.mob.req.patrol/(patrol^3)

df <- tibble(speeds, hotel, eff.jet)
colnames(df) <- c("speed", "hotel", "eff.jet")

df <- df %>%
  mutate(eff.prop = 1-log(speed+3)/6 +0.10 - speeds/200 ) %>%
  mutate(power.mob.req = const*speed^3,
         power.mob.drawn.jet = power.mob.req/eff.jet,
         power.mob.drawn.prop = power.mob.req/eff.prop,
         power.tot.jet = power.mob.drawn.jet + hotel,
         power.tot.prop = power.mob.drawn.prop + hotel,
         total.energy = total.batt*batt.dens*1000,  ##MJ
         endurance.prop.hour = total.energy*1000/power.tot.prop/3600,
         endurance.jet.hour = total.energy*1000/power.tot.jet/3600,
         range.prop = endurance.prop.hour*speed,
         range.jet = endurance.jet.hour*speed)

df2 <- df %>%
  gather(key = "tail", value = "value", endurance.prop.hour, endurance.jet.hour)

plot <- df %>%
  ggplot(aes(x = speed))+
  geom_line(aes(y = eff.prop, col = "efficiency prop")) +
  geom_line(aes(y = eff.jet, col = "efficiency jet"))+
  labs(title = "Efficiency Assumption Scenario 2 (patrol 3kt, transit 12kt)")+
  theme(axis.title.y = element_blank())
ggplotly(plot)%>%
  layout(legend = list(orientation = 'v', y = 0.9, x = 0.7))

## - speeds/120

plot <- df2 %>%
  ggplot(aes(x = speed, y = value, col = tail))+
  geom_line()+
  geom_point()+
  geom_text(aes(label = round(value, digits = 1)), nudge_y = 2, angle = 45)+
  #geom_text(aes(label = round(value, digits = 1)), position = position_dodge(4))+
  labs(title = "Endurance Scenario 2 (patrol 3kt, transit 12kt)")+
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(legend.title=element_blank())
ggplotly(plot) %>%
  layout(legend = list(orientation = 'v',y = 0.8, x = 0.7))

# plot <- df %>%
#   ggplot(aes(x = speed))+
#   geom_line(aes(y = endurance.prop.hour, col = "Propeller Endurance"))+
#   geom_text()+
#   geom_line(aes(y = endurance.jet.hour, col = "Jet Endurance"))+
#   labs(title = "Endurance Comparison")+
#   #theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
#   theme(legend.title=element_blank())
# ggplotly(plot) %>%
#   layout(legend = list(orientation = 'v',y = 0.8, x = 0.7))

plot <- df %>%
  ggplot(aes(x = speeds))+
  geom_line(aes(y = range.prop, col = "Propeller Range nm"))+
  geom_line(aes(y = range.jet, col = "Jet Range nm"))+
  labs(title = "Range Scenario 2 (patrol 3kt, transit 12kt)")
ggplotly(plot) %>%
  layout(legend = list(orientation = 'v', y = 0.8, x = 0.6))

plot <- df %>%
  ggplot(aes(x = speeds))+
  geom_line(aes(y = hotel, col = "hotel")) +
  geom_line(aes(y = power.mob.drawn.jet, col = "power drawn by jet"))+
  geom_line(aes(y = power.mob.req, col = "power thrust required"))+
  geom_line(aes(y = power.mob.drawn.prop, col = "power drawn by prop"))+
  labs(title = "Power Scenario 2 (patrol 3kt, transit 12kt)")
ggplotly(plot) %>%
  layout(legend = list(orientation = 'v', y = 0.8, x = 0.1))






