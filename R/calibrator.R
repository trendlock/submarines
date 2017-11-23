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

  message(glue::glue("Aid speed. is {speed.}"))
  message(glue::glue("Aid power. is {power.}"))


  if(system == "jet"){
    df. <- df %>%
      select(speed, hotel.1, eff.jet)
    colnames(df.) <- c("speed", "hotel.1", "eff")
  }

  if(system == "prop"){
    df. <- df %>%
      select(speed, hotel.1, eff.prop)
    colnames(df.) <- c("speed", "hotel.1", "eff")
  }

  index <- df. %>%
    mutate(line = case_when(speed == speed. ~ row_number()))
  index. <- index %>%
    filter(line > 0) %>%
    pull(line)

  eff. <- df.$eff[index.]

  message(glue::glue("Aid index is {index.}"))
  message(glue::glue("Aid eff. is {eff.}"))

  const <- speed.^3/(power.*eff.)

  const

}
