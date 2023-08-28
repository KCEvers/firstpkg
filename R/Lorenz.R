
#' Chaos in the atmosphere
#'
#' @param t
#' @param state
#' @param parameters
#'
#' @return
#' @export
#'
#' @examples
Lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <-  a * X + Y * Z
    dY <-  b * (Y - Z)
    dZ <- -X * Y + c * Y - Z
    list(c(dX, dY, dZ))
  })
}


#' Generalized Lotka-Volterra Model
#'
#' @param t
#' @param state
#' @param parameters
#'
#' @return
#' @export
#'
#' @examples
GLV <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # dX <-  a * X + Y * Z
    # dY <-  b * (Y - Z)
    # dZ <- -X * Y + c * Y - Z
    C = C0 * s
    diag(C) = diag(C0)
    dX <- r * c(X1,X2,X3,X4) * (1 - C %*% c(X1,X2,X3,X4)) + mu
    # print(dX)
    list(dX)
  })
}
