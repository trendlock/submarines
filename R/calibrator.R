#' @export

calibrator <- function(df, system, method, hotel, patrol, max.speed, max.power, speed, power) {

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

    speed. <- speed
    power. <- power
  }

  message(glue::glue("speed. is {speed.}"))
  message(glue::glue("power. is {power.}"))

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
  index. <- index %>%
    filter(line > 0) %>%
    pull(line)

  eff. <- df.$eff[index.]

  message(glue::glue(" index is {index.}"))
  message(glue::glue(" eff. is {eff.}"))

  speed.^3 / (power. * eff.)


}
