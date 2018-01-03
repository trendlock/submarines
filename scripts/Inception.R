library(dev)
library(plotly)


speed <- rep(seq(0.5,20,0.5),90)
VP. <- c(rep(1.228*1000,40), rep(2.339*1000,40), rep(4.245*1000,40))
VP <- rep(VP., 30)
temp. <- c(rep(10,40), rep(20,40), rep(30,40))
temp <- rep(temp., 30)
depth <- c(rep(10,120), rep(20,120), rep(30,120), rep(40,120), rep(50,120), rep(60,120), rep(70,120), rep(80,120), rep(90,120), rep(100,120),
           rep(110,120), rep(120,120), rep(130,120), rep(140,120), rep(150,120), rep(160,120), rep(170,120), rep(180,120), rep(190,120), rep(200,120),
           rep(210,120), rep(220,120), rep(230,120), rep(240,120), rep(250,120), rep(260,120), rep(270,120), rep(280,120), rep(290,120), rep(300,120))

df<- tibble(speed, depth, temp, VP)
df2 <- df %>%
  mutate(thrust = (speed^2)*35/40*1000,
         pressure = (100 + depth*10)*1000,
         disc.pressure = thrust/5,
         peak.pressure.drop = disc.pressure*10,
         min.stat.pressure = pressure - peak.pressure.drop,
         min.stat.rel.VP = (min.stat.pressure - VP)/100000)

plot <- df2 %>%
  filter(temp == 20 & depth <= 100) %>%
  ggplot(aes(x = speed, y = depth, fill = min.stat.rel.VP))+
  geom_tile()+
  scale_fill_gradientn(colours = rainbow(5))+
  scale_x_continuous(name ="speed (kt)")+
  scale_y_continuous(name ="depth (m)")+
  labs(title = "Cavitation Inception Equivalence Contours", fill = "Indicative \nExcess \nStatic Pressure \n(Atmospheres)")+
  stat_contour(aes(z = min.stat.rel.VP), col = "black")+
  stat_contour(data = df2 %>% filter(temp == 30 & depth <= 100), aes(z = min.stat.rel.VP), col = "red", linetype = 2)


ggplotly(plot)
