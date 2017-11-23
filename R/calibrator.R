#' @export

calibrator <- function(df, system = "jet", method = "hotel match", hotel = 150, patrol = 2.5, max.speed = 20, max.power = 7000, speed = 10, power = 1000) {

  df <- df %>%
    mutate(hotel)

  if(method == "hotel match") {
    speed. <- patrol
    power. <- hotel
  }

  if(method == "max power") {
    speed. <- max.speed
    power. <- max.power
  }

  if(method == "other reference"){

    print(power)
    speed. <- speed
    power. <- power
  }

  if(system == "jet"){
    df. <- df %>%
      rename(eff = eff.jet) %>%
      select(kts, hotel, eff)
  }

  if(system == "prop"){
    df. <- df %>%
      rename(eff = eff.prop) %>%
      select(kts, hotel, eff)
  }

  index <- df. %>%
    mutate(id = row_number(),
           line = case_when(kts == speed. ~ id))


  df_outcoming <<- index
  #message(glue::glue("Calibrator: index is {index}"))
  index. <- index %>%
    filter(line > 0) %>%
    pull(line)

  eff. <- df.$eff[index.]

  speed.^3 / (power. * eff.)


}
