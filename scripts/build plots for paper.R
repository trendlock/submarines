
library(submarines)
library(plotly)

df <- read_csv(find::this("/Submarines/eff-data-2018-01-03.csv"))

 # df <- read_rds("extdata/default_subs_df.rds")

df_temp <- df %>%
  mutate(system = case_when(str_detect(cat, "jet") ~ "Pumpjet",
                   str_detect(cat, "prop") ~ "Propeller"),
         level = case_when(str_detect(cat, "1") ~ "Central",
                            str_detect(cat, "2") ~ "High",
                            str_detect(cat, "3") ~ "Low"))

ggplot(df_temp, aes(x = kts, y = val, col = cat)) +
  geom_line(size = 1)

ggplot(df_temp, aes(x = kts, y = val, col = level, linetype = system)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("black", "blue", "red") )+
  ylim(0, 1)+
  labs(title = "Efficiency Curve Assumptions")+
  scale_x_continuous(name ="speed (kt)")+
  scale_y_continuous(name ="efficiency")

plotly::ggplotly()

df <- df %>%
  mutate(set = case_when(
    cat %in% c("input_prop_1", "input_jet_1") ~ "A",
    cat %in% c("input_prop_3", "input_jet_2") ~ "B",
    cat %in% c("input_prop_2", "input_jet_3") ~ "C"
  ))

ls <- df %>%
  split(.$set) %>%
  map( ~ .x %>%
         select(-set) %>%
         mutate(cat = case_when(
           str_detect(cat, "jet") ~ "eff.jet",
           str_detect(cat, "prop") ~ "eff.prop"
         )) %>%
         spread(cat, val))






ls_comp <- ls %>%
  map( ~ .x %>%
         select(kts, eff.jet, eff.prop) %>%
         run_subs(hotel = 100,
                  total.batt = 500,
                  batt.dens = 0.14,
                  patrol = 2.5,
                  max.speed = 18,
                  max.power = 7,
                  speed = 20,
                  power = 6500,
                  system = "jet",
                  method = "other reference")) %>%
  set_names(c("Set 1", "Set 2", "Set 3"))




produce_all_plots <- function(df) {

  eff_plot_df <- df %>%
    filter(cat %in% c("eff.jet", "eff.prop")) %>%
    mutate(cat = case_when(
      cat ==  "eff.prop" ~ "Propeller",
      cat ==  "eff.jet" ~ "Pumpjet"
    ))

  end_plot_df <- df %>%
    filter(cat %in% c("endurance.prop.hour", "endurance.jet.hour")) %>%
    mutate(cat = case_when(
      cat ==  "endurance.prop.hour" ~ "Propeller",
      cat ==  "endurance.jet.hour" ~ "Pumpjet"
    ))

  y_max_e <- roundUpNice(max(pull(end_plot_df, val)))

  comp_plot_df <- df %>%
    filter(cat %in% c("eff.jet", "eff.prop", "range.jet", "range.prop", "endurance.prop.hour", "endurance.jet.hour")) %>%
    mutate(sys = case_when(
      str_detect(cat,  "prop") ~ "Propeller",
      str_detect(cat,  "jet") ~ "Pumpjet"),
      cat = case_when(
        str_detect(cat,  "end") ~ "end",
        str_detect(cat,  "eff") ~ "eff",
        str_detect(cat, "range") ~ "range"))

  diff_plot_df <- comp_plot_df %>%
    spread(sys, val) %>%
    mutate(prop = (Propeller/Pumpjet -1) * 100,
           diff = (Propeller - Pumpjet))

  end_prop_plot_df <- end_plot_df %>%
    spread(cat, val) %>%
    mutate(diff = (Propeller/Pumpjet -1) * 100)

  y_max_e_p <- roundUpNice(max(pull(end_prop_plot_df, diff)))




  range_plot_df <- df %>%
    filter(cat %in% c("range.prop", "range.jet")) %>%
    mutate(cat = case_when(
      cat ==  "range.prop" ~ "Propeller",
      cat ==  "range.jet" ~ "Pumpjet"
    ))

  y_max_r <- roundUpNice(max(pull(range_plot_df, val)))

  range_prop_plot_df <- range_plot_df %>%
    spread(cat, val) %>%
    mutate(diff = (Propeller/Pumpjet -1) * 100)




  power_plot_df <- df %>%
    filter(cat %in% c("hotel","power.mob.drawn.jet", "power.mob.req", "power.mob.drawn.prop")) %>%
    mutate(cat = case_when(
      cat == "hotel" ~ "Hotel Load",
      cat ==  "power.mob.drawn.prop" ~ "Propulsion Power Drawn Propeller",
      cat ==  "power.mob.drawn.jet" ~ "Propulsion Power Drawn Jet",
      cat ==  "power.mob.req" ~ "Effective Power Required"
    ))

  y_max_p <- roundUpNice(max(pull(power_plot_df, val)))

  eff <- ggplot(eff_plot_df, aes(x = kts, y = val, linetype = cat))+
    geom_line()+
    theme_minimal() +
    scale_linetype_manual(values = c(2, 1) )+
    scale_y_continuous(breaks=seq(0, 1, 0.2),
                       name = "Efficiency") +
    scale_x_continuous(breaks=seq(0, 20, 2))



  end <- ggplot(end_plot_df, aes(x = kts, y = val, col = cat))+
    geom_line() +
    theme_minimal() +
    scale_y_continuous(breaks=seq(0, y_max_e, (y_max_e/10)),
                       name = "Endurance (hours)") +
    scale_x_continuous(breaks=seq(0, 20, 2))

  rng <- ggplot(range_plot_df, aes(x = kts, y = val, col = cat))+
    geom_line()+
    theme_minimal()+
    scale_y_continuous(breaks=seq(0, 350, (350/10)),
                       name = "Range (nm) ") +
    scale_x_continuous(breaks=seq(0, 20, 2))

  end_p <- ggplot(end_prop_plot_df, aes(x = kts, y = diff))+
    geom_line() +
    theme_minimal() +
    scale_y_continuous(name = "% Advantage Propeller")

  pwr <- ggplot(power_plot_df, aes(x = kts, y = val, col = cat))+
    geom_line() +
    theme_minimal() +
    scale_y_continuous(breaks=seq(0, 32000, (32000/10)),
                       name = "kW") +
    scale_x_continuous(breaks=seq(0, 20, 2))

  #list(eff = eff, end = end, rng = rng, end_p = end_p,  pwr =pwr)
  #list(comp_plot_df = comp_plot_df, end_prop_plot_df = end_prop_plot_df, range_prop_plot_df = range_prop_plot_df, diff_plot_df = diff_plot_df)
  list(power_plot_df = power_plot_df, end_prop_plot_df = end_prop_plot_df, diff_plot_df=diff_plot_df)
}



# another plot
# proportional change to range?

ls_plots <- ls_comp %>%
  map( ~produce_all_plots(.x))

#hotl_x <- ls_plots$`Set 2`$range_plot_df

#ls_plots$`Set 2`$rng %>% write_rds("extdata/rng_ploy_hotlx.rds")


##############  New filling procedure for Difference plots ################

df_filler <- ls_plots$`Set 1`$diff_plot_df

df_filler <- df_filler %>%
  select(-Propeller, -Pumpjet)

df_filler <- df_filler %>%
  gather(key = "var", value = "val", prop, diff )

df_filler <- df_filler %>%
  filter(cat %in% c("end", "range"),
         !(cat == "range" & var == "prop"))

df_filler <- df_filler %>%
  spread(key = var, value = val) %>%
  spread(key = cat, value = diff)

df_filler <- df_filler %>%
  fill(range, .direction = "up")

df_filler <- df_filler %>%
  filter(end > 0)

df_filler <- df_filler %>%
  gather(key = "var", value = "val", -kts)

############### the mutate step ##############
df_filler <- df_filler %>%
  mutate(pair = "mid",
         test = "corners",
         hotel = 50,
         battery = 0.28)

df_filler %>%
  ggplot(aes(x = kts, y = val, col = pair))+
  geom_line()+
  facet_grid(var~., scales = "free")

df_full <- df_full %>%
  bind_rows(df_filler)
#df_full <- df_filler

df_full_cornered %>% write_rds("extdata/df_full_diff_cornered.rds")

########## Time to do some indiscretion ratios (IR)  #############

df_filler <- ls_plots$`Set 3`$power_plot_df

df_filler <- df_filler %>%
  mutate(pair = "wide",
         hotel = 100,
         battery = 0.14,
         total.batt = 500)

#df_full <- df_filler

df_full <- df_full %>%
  bind_rows(df_filler)

### checking df ###
df_full %>%
  ggplot(aes(x = kts, y = val, col = pair, linetype = cat))+
  geom_line()

### calculations to get IRs ###
df_IRs <- df_full %>%
  mutate(energy.kJ = total.batt*1000*battery*1000,
         charge.rate.kw = 5500)

df_IRs <- df_IRs %>%
  filter(cat %in% c("Propulsion Power Drawn Jet", "Propulsion Power Drawn Propeller"))

df_IRs <- df_IRs %>%
  mutate(endurance.hrs = energy.kJ/(val + hotel)/3600,
         charge.time.hrs = energy.kJ/charge.rate.kw/3600,
         IR = charge.time.hrs/endurance.hrs)

plot <- df_IRs %>%
  ggplot(aes(x = kts, y = IR, col = cat))+
  geom_line()+
  scale_y_continuous(breaks=seq(0, 1.3, 0.1),
                     name = "IR") +
  scale_x_continuous(breaks=seq(0, 20, 1))+
  facet_grid(pair~., scales = "free")


ggplotly(plot)

#### Starting some plots for IRs  ######




######  Let's tru some plots  ######

df_full_cornered$hotel <- factor(df_full_cornered$hotel, levels = c("50", "100", "200" ),
                                  labels = c( "Hotel 50kW", "Hotel 100kW", "Hotel 200kW" ))
df_full_cornered$battery <- factor(df_full_cornered$battery, levels = c("0.07", "0.14", "0.28" ),
                                 labels = c( "Battery 0.07MJ/kg", "Battery 0.14MJ/kg", "Battery 0.28MJ/kg" ))

df_full_cornered %>%
  filter(test == "hotel") %>%
  ggplot(aes(x = kts, y = val, col = pair))+
  geom_line()+
  labs(col = "Efficiency \nCurve Pair")+
  scale_y_continuous(name = element_blank())+
  facet_grid(var ~ as.factor(hotel), scales = "free")

df_full_cornered %>%
  filter(test == "battery") %>%
  ggplot(aes(x = kts, y = val, col = pair))+
  geom_line()+
  labs(col = "Efficiency \nCurve Pair")+
  scale_y_continuous(name = element_blank())+
  facet_grid(var ~ as.factor(battery), scales = "free")

df_full_cornered <- df_full %>%
  mutate(corner = paste0( hotel, " ", battery))

df_full_cornered <- read_rds("extdata/df_full_diff_cornered.rds")

df_full_cornered$var <- factor(df_full_cornered$var, levels =  c("end", "range", "prop"),
                                 labels = c("Endurance (hrs)", "Range (nm)", "% Change"))
df_full_cornered$corner <- factor(df_full_cornered$corner, levels = c("200 0.07", "200 0.28","50 0.07", "50 0.28", "100 0.14", "50 0.14",  "200 0.14", "100 0.07", "100 0.28" ),
                                  labels = c("Hotel 200kW, Battery 0.07MJ/kg", "Hotel 200kW, Battery 0.28MJ/kg", "Hotel 50kW, Battery 0.07MJ/kg", "Hotel 50kW, Battery 0.28MJ/kg",
                                             "100 0.14", "50 0.14",  "200 0.14", "100 0.07", "100 0.28" ))


plot <- df_full_cornered %>%
  filter(test == "battery") %>%
  filter(var == "Range (nm)") %>%
  #filter(var == "Endurance (hrs)") %>%
  #filter(var == "Endurance (hrs)") %>%
  ggplot(aes(x = kts, y = val, col = pair))+
  geom_line()+
  labs(col = "Efficiency \nCurve Pair")+
  #scale_y_continuous(name = element_blank())+
  #theme(legend.position="bottom")+
  #facet_grid(var ~ corner, scales = "free", switch = "y")+
  facet_grid(. ~ battery, scales = "free", switch = "y")

ggplotly(plot)

plot <- df_full_cornered %>%
  filter(test == "corners") %>%
  ggplot(aes(x = kts, y = val, col = pair))+
  geom_line()+
  labs(col = "Efficiency \nCurve Pair")+
  #scale_y_continuous(name = element_blank())+
  theme(legend.position="bottom")+
  facet_grid(var ~ corner, scales = "free", switch = "y")

################### Filling up the data frame manually  ##################
df_filler <- ls_plots$`Set 2`$power_plot_df %>%
  mutate(level = case_when(cat == "Propeller" ~ "Low",
                           cat == "Pumpjet" ~ "High"),
         var = "power",
         pair = "narrow")

#### for proportion case ####
df_filler <- ls_plots$`Set 2`$end_prop_plot_df %>%
  select(-Pumpjet, -Propeller) %>%
  mutate(cat = "diff",
         level = NA,
         var = "end_prop",
         pair = "narrow")
  colnames(df_filler)[2] <- "val"
df_filler <- df_filler %>%
  select(kts, cat, val, level, var, pair)

#df_full <- df_filler

ggplot(df_filler, aes(x = kts, y = val, linetype = cat))+
  geom_line()

df_full <- df_full %>%
  bind_rows(df_filler)

#df_full <- df_full_wide
df_full_wide$pair <- "wide"

# check them out

df_full_wide <- df_full
df_full_wide %>% write_rds("extdata/df_full_wide.rds")


df_full_complete <- df_full_bottom %>%
  bind_rows(df_full_jetdown, df_full_jetup, df_full_mid, df_full_narrow, df_full_propup, df_full_propdown, df_full_top, df_full_wide)


# df_full_complete. <- df_full_complete %>%
#   mutate(cat = str_replace_all(cat, "Propulsion Power Drawn Jet", "Pumpjet"),
#          cat = str_replace_all(cat, "Propulsion Power Drawn Propeller", "Propeller"))

df_full_complete_end_prop <- df_full_complete %>%
  filter(var == "end_prop") %>%
  mutate(level = "not applicable")

df_full_complete. <- df_full_complete
df_full_complete. <- df_full_complete. %>%
  filter(!var == "end_prop")
df_full_complete. <-df_full_complete. %>%
  bind_rows(df_full_complete_end_prop)

df_full_complete.$pair <- factor(df_full_complete.$pair, levels =  c("bottom", "jetdown", "propdown", "wide", "mid", "narrow", "propup", "jetup", "top"),
                                 labels = c("Lowest Lines", "Jet Lowered", "Propeller Lowered", "Widest Separation", "Central Selections", "Narrowest Separation", "Propeller Raised", "Jet Raised", "Highest Lines"))
df_full_complete.$var <- factor(df_full_complete.$var, levels = c("eff", "end", "range", "end_prop", "power"),
                                labels = c("Efficiency", "Endurance (hrs)", "Range (nm)", "Advantage Propeller %", "Power (kW)"))

df_full_complete <- df_full_complete.
df_full_complete. %>% write_rds("extdata/df_full_complete_fac.rds")

df <- read_rds("extdata/df_full_complete_fac.rds")
plot <- df %>%
  filter(var == "Advantage Propeller %", pair %in% c("Lowest Lines", "Jet Lowered", "Propeller Lowered", "Widest Separation", "Central Selections", "Narrowest Separation", "Propeller Raised", "Jet Raised", "Highest Lines") ) %>%
  ggplot(aes(x = as.numeric(kts), y = as.numeric(val), col = pair))+
  geom_line()+
  scale_x_continuous(name ="speed (kt)")+
  labs(col = "Curves Compared")+
  scale_y_continuous(name ="Percentage Propeller Advantage", breaks=seq(0, 120, 10))

ggplotly(plot)

df <- read_rds("extdata/df_full_complete_fac.rds")
plot <- df %>%
  filter(var == "Power (kW)", pair %in% c("Central Selections")) %>%
  ggplot(aes(x = as.numeric(kts), y = as.numeric(val), col = cat))+
  scale_x_continuous(name ="speed (kt)")+
  #scale_y_continuous(name = "Endurance (hrs)")+
  #labs(title = "Endurance")+
  geom_line(size = 0.8)
ggplotly(plot)


df <- read_rds("extdata/df_full_complete_fac.rds")

df %>%
  filter( pair %in% c("Widest Separation", "Narrowest Separation", "Central Selections")) %>%
  filter(var %in% c("Efficiency", "Endurance (hrs)", "Range (nm)")) %>%
  #filter(var == "end_prop", pair %in% c("top", "bottom", "propup", "propdown", "jetup", "jetdown", "wide", "narrow", "mid") ) %>%
  ggplot(aes(x = as.numeric(kts), y = as.numeric(val), linetype = as.factor(cat), col = level))+
  geom_line(aes(group = cat))+
  scale_linetype_manual(labels = c("Propeller", "Pumpjet" ), values = c(1, 2 ) )+
  scale_colour_manual(values = c("black", "blue", "red") )+
  scale_x_continuous(name ="speed (kt)")+
  scale_y_continuous(name =element_blank())+
  labs(linetype = "system")+
  facet_grid(var ~ pair, scales = "free", switch =  "y")




# plotly

# ls_plotly <- ls_plots %>%
#   map( ~ map(.x, ~ plotly::ggplotly(.x)))
#
#
# ls_plots$`Set 1`$end_p
#


 # if you want to plot them together...
library(gtable)
library(gridExtra)


obj <- grid.arrange(
  grobs = list(ls_plots$`Set 1`$eff,
               ls_plots$`Set 1`$end,
               ls_plots$`Set 1`$end_p,
               ls_plots$`Set 1`$rng,
               ls_plots$`Set 1`$pwr,
               ls_plots$`Set 2`$eff,
               ls_plots$`Set 2`$end,
               ls_plots$`Set 2`$end_p,
               ls_plots$`Set 2`$rng,
               ls_plots$`Set 2`$pwr,
               ls_plots$`Set 3`$eff,
               ls_plots$`Set 3`$end,
               ls_plots$`Set 3`$end_p,
               ls_plots$`Set 3`$rng,
               ls_plots$`Set 3`$pwr),
  widths = c(1, 1, 1),
  layout_matrix = rbind(c(1, 6, 11),
                        c(2, 7, 12),
                        c(4, 9, 14),
                        c(3, 8, 13),
                        c(5, 10, 15)))
