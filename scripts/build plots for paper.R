
library(submarines)

df <- read_csv(find::this("/Submarines/eff-data-2018-01-03.csv"))

 # df <- read_rds("extdata/default_subs_df.rds")


ggplot(df, aes(x = kts, y = val, col = cat)) +
  geom_line() +
  ylim(0, 1)

plotly::ggplotly()

df <- df %>%
  mutate(set = case_when(
    cat %in% c("input_prop_1", "input_jet_1") ~ "Middle",
    cat %in% c("input_prop_2", "input_jet_3") ~ "Close",
    cat %in% c("input_prop_3", "input_jet_2") ~ "Far"
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
         run_subs(hotel = 150,
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



  end_prop_plot_df <- end_plot_df %>%
    spread(cat, val) %>%
    mutate(diff = (Propeller/Jet -1) * 100)

  y_max_e_p <- roundUpNice(max(pull(end_prop_plot_df, diff)))


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

  end_p <- ggplot(end_prop_plot_df, aes(x = kts, y = diff))+
    geom_line() +
    theme_minimal() +
    scale_y_continuous(name = "% Advantage")

  rng <- ggplot(range_plot_df, aes(x = kts, y = val, col = cat))+
    geom_line()+
    theme_minimal()+
    scale_y_continuous(breaks=seq(0, 350, (350/10)),
                       name = "nm") +
    scale_x_continuous(breaks=seq(0, 20, 2))



  pwr <- ggplot(power_plot_df, aes(x = kts, y = val, col = cat))+
    geom_line() +
    theme_minimal() +
    scale_y_continuous(breaks=seq(0, 32000, (32000/10)),
                       name = "hours") +
    scale_x_continuous(breaks=seq(0, 20, 2))

  list(eff = eff, end = end, end_p = end_p, rng = rng,  pwr =pwr)
}



# another plot
# proportional change to range?

ls_plots <- ls_comp %>%
  map( ~produce_all_plots(.x))



# check them out


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
                        c(3, 8, 13),
                        c(4, 9, 14),
                        c(5, 10, 15)))
