#' @title "interest_rate_by_period": Converts mortgage annual interest rates to periodic rates
#'
#' @description Converts mortgage annual interest rates to periodic rates.
#' @description This function calculates mortgage estimates. You should seek professional advice before making financial decisions.
#'
#' @author Deepankar Datta <deepankardatta@nhs.net>
#'
#' @param annual_interest_rate The annual interest rate of the mortgage
#' @param period Optional and unused variable to change what period the annual interest rate is converted to. May be deprecated.
#'
#' @return periodic_interest_rate The interest rate for the period of note
#'
#' @references Formulas sourced from https://en.wikipedia.org/wiki/Mortgage_calculator
#'
#' @examples
#' # interest_rate_by_period( annual_interest_rate )
#' # interest_rate_by_period( annual_interest_rate , period = 12 )
#' # monthly_interest_rate <- interest_rate_by_period( annual_interest_rate )
#'
#' @export

interest_rate_by_period <- function( annual_interest_rate , period=12 ){

  warning('This function calculates mortgage estimates. You should
          seek professional advice before making financial decisions.')

  # Default to return monthly interest rates
  # Not really thought how to optimise for other types of periodic accounting

  periodic_interest_rate <- annual_interest_rate / ( period * 100 )

  return(periodic_interest_rate)

}
