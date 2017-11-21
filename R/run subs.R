
#' @export

run_subs <- function(
  df,
  hotel,
  total.batt,
  batt.dens,
  patrol,
  max.speed,
  max.power,
  speed,
  power,
  system,
  method
) {

  # add hotel load
  df <- mutate(df, hotel)

  # run cal
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
           range.jet = endurance.jet.hour * kts) %>%
    gather("cat", "val", 2:ncol(.))


  df

}


