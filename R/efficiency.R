
#' @export

efficiency <- function(KT = 0.1, K = 0.1, deltaVonV = 0.4){

  eta <- (1- (KT/2)/deltaVonV)/(1+deltaVonV/2+(K/2)/deltaVonV)

  eta
}
