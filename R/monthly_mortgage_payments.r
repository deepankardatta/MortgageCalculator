#' @title "monthly_mortgage_payments": Calculates monthly mortgage payments
#'
#' @description Calculates monthly mortgage payments from mortgage data.
#' @description This function calculates mortgage estimates. You should seek professional advice before making financial decisions.
#'
#' @author Deepankar Datta <deepankardatta@nhs.net>
#'
#' @param annual_interest_rate The annual interest rate of the mortgage
#' @param term The mortgage term in months
#' @param principal The principal of the mortgage (aka mortgage amount, or amount borrowed)
#'
#' @return monthly_payment A value with the estimated monthly mortgage payment
#'
#' @references Formulas sourced from https://en.wikipedia.org/wiki/Mortgage_calculator
#'
#' @examples
#' # monthly_mortgage_payments( annual_interest_rate , term , principal )
#' # payments <- monthly_mortgage_payments( annual_interest_rate , term , principal )
#'
#' @export

monthly_mortgage_payments <- function( annual_interest_rate , term , principal ){

  warning('This function calculates mortgage estimates. You should
          seek professional advice before making financial decisions.')

  if( annual_interest_rate == 0 ){
    monthly_payment <- principal / term
  }

  if( annual_interest_rate != 0 ){
    monthly_interest <- interest_rate_by_period( annual_interest_rate )
    monthly_payment <- ( principal * monthly_interest ) /
      (1-(1+monthly_interest)^(-term))
  }

  return(monthly_payment)

}
