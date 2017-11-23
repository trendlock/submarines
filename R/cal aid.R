#' @export

calibrator_aid <- function(df, system = "jet", method = "hotel match", hotel = 150, patrol = 2.5, max.speed = 20, max.power = 7000, speed = 10, power = 1000) {



  print(df)

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


  message(glue::glue("speed. is {speed.}"))
  message(glue::glue("power. is {power.}"))

  if(system == "jet"){
    print(df)
    df. <- df %>%
      select(kts, hotel, eff.jet) %>%
      rename(eff = eff.jet)
  }

  if(system == "prop"){
    message("this df")
    print(df)
    df. <- df %>%
      select(kts, hotel, eff.prop) %>%
      rename(eff = eff.prop)
  }

  index <- df. %>%
    mutate(line = case_when(kts == speed. ~ row_number()))

  print(index)

  index. <- index %>%
    filter(line > 0) %>%
    pull(line)

  print(index.)

  eff. <- df.$eff[index.]

  const <- speed.^3/(power.*eff.)

  const

}
