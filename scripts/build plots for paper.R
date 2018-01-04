
library(submarines)

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
    cat %in% c("input_prop_2", "input_jet_3") ~ "A",
    cat %in% c("input_prop_3", "input_jet_2") ~ "B",
    cat %in% c("input_prop_1", "input_jet_1") ~ "C"
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
  list(eff_plot_df = eff_plot_df, end_plot_df = end_plot_df, range_plot_df = range_plot_df, end_prop_plot_df = end_prop_plot_df, power_plot_df = power_plot_df)
}



# another plot
# proportional change to range?

ls_plots <- ls_comp %>%
  map( ~produce_all_plots(.x))

#hotl_x <- ls_plots$`Set 2`$range_plot_df

#ls_plots$`Set 2`$rng %>% write_rds("extdata/rng_ploy_hotlx.rds")


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

# df_full <- df_full %>%
#   filter(!cat == "diff")
# check them out

df_full_narrow <- df_full
df_full_narrow %>% write_rds("extdata/df_full_narrow.rds")


df_full_complete <- df_full_bottom %>%
  bind_rows(df_full_jetdown, df_full_jetup, df_full_mid, df_full_narrow, df_full_propup, df_full_propdown, df_full_top, df_full_wide)

df_full_complete %>% write_rds("extdata/df_full_complete.rds")

plot <- df_full_complete %>%
  #filter(var == "end", pair %in% c("wide", "narrow", "mid")) %>%
  filter(var == "end_prop", pair %in% c("top", "bottom", "propup", "propdown", "jetup", "jetdown", "wide", "narrow", "mid") ) %>%
  ggplot(aes(x = as.numeric(kts), y = as.numeric(val), linetype = as.factor(cat), col = pair))+
  geom_line(aes(group = cat))

ggplotly(plot)

df_full_complete %>%
  filter( pair %in% c("wide", "narrow", "mid")) %>%
  #filter(var == "end_prop", pair %in% c("top", "bottom", "propup", "propdown", "jetup", "jetdown", "wide", "narrow", "mid") ) %>%
  ggplot(aes(x = as.numeric(kts), y = as.numeric(val), linetype = as.factor(cat), col = cat))+
  geom_line(aes(group = cat))+
  facet_grid(var ~ pair, scales = "free")

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
