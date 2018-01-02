
library(stringr)

df <- read_rds("extdata/comb_eff_df.rds")

 # df <- read_rds("extdata/default_subs_df.rds")


ggplot(df, aes(x = kts, y = eff, col = System)) +
  geom_line() +
  ylim(0, 1)


df_1_j <- df %>%
  filter(System == "Jet 1") %>%
  rename(eff.jet = eff)
df_1_p <- df %>%
  filter(System == "Prop 1")
df_1 <-  df_1_j %>%
  mutate(eff.prop = pull(df_1_p, eff))

df_2_j <- df %>%
  filter(System == "Jet 2") %>%
  rename(eff.jet = eff)
df_2_p <- df %>%
  filter(System == "Prop 2")
df_2 <-  df_2_j %>%
  mutate(eff.prop = pull(df_2_p, eff))

df_3_j <- df %>%
  filter(System == "Jet 3") %>%
  rename(eff.jet = eff)
df_3_p <- df %>%
  filter(System == "Prop 3")
df_3 <-  df_3_j %>%
  mutate(eff.prop = pull(df_3_p, eff))


ls <- list(df_1, df_2, df_3) %>%
  map( ~ .x %>%
         select(kts, eff.jet, eff.prop) %>%
         run_subs(
                  hotel = 150,
                  total.batt = 500,
                  batt.dens = 0.14,
                  patrol = 2.5,
                  max.speed = 18,
                  max.power = 7,
                  speed = 8,
                  power = 1500,
                  system = "jet",
                  method = "hotel match")) %>%
  set_names(c("Set 1", "Set 2", "Set 3"))




produce_all_plots <- function(df) {
  eff_plot_df <- df %>%
    filter(cat %in% c("eff.jet", "eff.prop")) %>%
    mutate(cat = case_when(
      cat ==  "eff.prop" ~ "Propeller",
      cat ==  "eff.jet" ~ "Jet"
    ))


  end_plot_df <- df %>%
    filter(cat %in% c("endurance.prop.hour", "endurance.jet.hour")) %>%
    mutate(cat = case_when(
      cat ==  "endurance.prop.hour" ~ "Propeller",
      cat ==  "endurance.jet.hour" ~ "Jet"
    ))

  y_max_e <- roundUpNice(max(pull(end_plot_df, val)))

  range_plot_df <- df %>%
    filter(cat %in% c("range.prop", "range.jet")) %>%
    mutate(cat = case_when(
      cat ==  "range.prop" ~ "Propeller",
      cat ==  "range.jet" ~ "Jet"
    ))

  y_max_r <- roundUpNice(max(pull(range_plot_df, val)))


  power_plot_df <- df %>%
    filter(cat %in% c("hotel","power.mob.drawn.jet", "power.mob.req", "power.mob.drawn.prop")) %>%
    mutate(cat = case_when(
      cat == "hotel" ~ "Hotel Load",
      cat ==  "power.mob.drawn.prop" ~ "Propulsion Power Drawn Propeller",
      cat ==  "power.mob.drawn.jet" ~ "Propulsion Power Drawn Jet",
      cat ==  "power.mob.req" ~ "Thrust Power Required"
    ))

  y_max_p <- roundUpNice(max(pull(power_plot_df, val)))

  eff <- ggplot(eff_plot_df, aes(x = kts, y = val, col = cat))+
    geom_line()+
    theme_minimal() +
    scale_y_continuous(breaks=seq(0, 1, 0.2),
                       name = "Efficiency") +
    scale_x_continuous(breaks=seq(0, 20, 2))

  end <- ggplot(end_plot_df, aes(x = kts, y = val, col = cat))+
    geom_line() +
    theme_minimal() +
    scale_y_continuous(breaks=seq(0, y_max_e, (y_max_e/10)),
                       name = "hours") +
    scale_x_continuous(breaks=seq(0, 20, 2))

  rng <- ggplot(range_plot_df, aes(x = kts, y = val, col = cat))+
    geom_line()+
    theme_minimal()+
    scale_y_continuous(breaks=seq(0, y_max_r, (y_max_r/10)),
                       name = "nm") +
    scale_x_continuous(breaks=seq(0, 20, 2))

  pwr <- ggplot(power_plot_df, aes(x = kts, y = val, col = cat))+
    geom_line() +
    theme_minimal() +
    scale_y_continuous(breaks=seq(0, y_max_p, (y_max_p/10)),
                       name = "hours") +
    scale_x_continuous(breaks=seq(0, 20, 2))

  list(eff = eff, end = end, rng = rng, pwr =pwr)
}



ls_plots <- ls %>%
  map( ~produce_all_plots(.x))

ls_plots$`Set 1`

library(gtable)
library(gridExtra)

obj <- grid.arrange(
  grobs = list(ls_plots$`Set 1`$eff,
               ls_plots$`Set 1`$end,
               ls_plots$`Set 1`$rng,
               ls_plots$`Set 1`$pwr,
               ls_plots$`Set 2`$eff,
               ls_plots$`Set 2`$end,
               ls_plots$`Set 2`$rng,
               ls_plots$`Set 2`$pwr,
               ls_plots$`Set 3`$eff,
               ls_plots$`Set 3`$end,
               ls_plots$`Set 3`$rng,
               ls_plots$`Set 3`$pwr),
  widths = c(1, 1, 1, 1),
  layout_matrix = rbind(c(1, 1, 1),
                        c(1, 1, 1),
                        c(1, 1, 1)))



obj <- grid.arrange(
  grobs = list(ls_plots$`Set 1`$eff,
               ls_plots$`Set 1`$end,
               ls_plots$`Set 1`$rng,
               ls_plots$`Set 1`$pwr,
               ls_plots$`Set 2`$eff,
               ls_plots$`Set 2`$end,
               ls_plots$`Set 2`$rng,
               ls_plots$`Set 2`$pwr,
               ls_plots$`Set 3`$eff,
               ls_plots$`Set 3`$end,
               ls_plots$`Set 3`$rng,
               ls_plots$`Set 3`$pwr),
  widths = c(1, 1, 1),
  layout_matrix = rbind(c(1, 5, 9),
                        c(2, 6, 10),
                        c(3, 7, 11),
                        c(4, 8, 12)))
