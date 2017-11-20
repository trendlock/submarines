#' @export

calibrator <- function(df, system = "jet", method = "hotel match", hotel = 150, patrol = 2.5, max.speed = 20, max.power = 7000, speed = 10, power = 1000) {

  if(method == "hotel match") {
    speed. <- patrol
    power. <- hotel
  }

  if(method == "max power") {
    speed. <- max.speed
    power. <- max.power
  }

  if(method == "other reference"){
    speed. <- speed
    power. <- power
  }

  if(system == "jet"){
    df. <- df %>%
      select(kts, hotel, eff.jet) %>%
      rename(eff = eff.jet)
  }

  if(system == "prop"){
    df. <- df %>%
      select(kts, hotel, eff.prop) %>%
      rename(eff = eff.prop)
  }

  index <- df. %>%
    mutate(id = row_number(),
           line = case_when(kts == speed. ~ id))
  index. <- index %>%
    filter(line > 0) %>%
    pull(line)

  eff. <- df.$eff[index.]

  const <- speed.^3 / (power. * eff.)

  const

}
