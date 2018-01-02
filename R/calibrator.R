#' @export

calibrator <- function(df, system, method, hotel, patrol, max.speed, max.power, speed, power) {

  df <- df %>%
    mutate(hotel)

  message("in df")
  print(head(df))

  if(method == "hotel match") {
    speed. <- patrol
    power. <- hotel
  }

  if(method == "max power") {
    speed. <- max.speed
    power. <- max.power * 1000
  }

  if(method == "other reference"){

    speed. <- speed
    power. <- power
  }




  message("speed.")
  print(speed.)
  message("power.")
  print(power.)


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

  message("df.")
  print(head(df.))


  index <- df. %>%
    mutate(id = row_number(),
           line = case_when(kts == speed. ~ id))


  index. <- index %>%
    filter(line > 0) %>%
    pull(line)

  message("index.")
  print(index.)


  eff. <- df.$eff[index.]


  message("eff.")
  print(eff.)

  (power.* eff.)/(speed.^3)



}
