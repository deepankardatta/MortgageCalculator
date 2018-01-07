#' @title "amount_owed_after_n_months": Calculates the outstanding mortgage principle after a set amount of months
#'
#' @description Calculates the outstanding mortgage principle after a set amount of months.
#' @description This function calculates mortgage estimates. You should seek professional advice before making financial decisions.
#'
#' @author Deepankar Datta <deepankardatta@nhs.net>
#'
#' @include monthly_mortgage_payments.r
#' @include interest_rate_by_period.r
#'
#' @param month_of_interest The value of the month that you wish to know the outstanding principle
#' @param alternate_payment (OPTIONAL) A value for an alternate monthly payment used to pay off the principal - this is used for overpayments
#'
#' @return outstanding_owed The outstanding principle at the end of the supplied month of interest
#'
#' @references Formulas sourced from https://en.wikipedia.org/wiki/Mortgage_calculator
#'
#' @examples
#' # amount_owed_after_n_months( annual_interest_rate , term , principal , month_of_interest )
#'
#' @export

amount_owed_after_n_months <- function( annual_interest_rate ,
                                        term ,
                                        principal ,
                                        month_of_interest ,
                                        alternate_payment=NULL ){

  warning('This function calculates mortgage estimates. You should
          seek professional advice before making financial decisions.')

  # Formulas sourced from https://en.wikipedia.org/wiki/Mortgage_calculator

  # Call the monthly_mortgage_payments function to calculate the
  # minimum monthly repayment
  if( is.null(alternate_payment) ) {
    monthly_payment <-
      monthly_mortgage_payments( annual_interest_rate , term , principal )
  } else {
    monthly_payment <- alternate_payment
  }

  # Calculate monthly interest rate
  monthly_interest <-
    interest_rate_by_period( annual_interest_rate , period = 12 )

  # The amount owed calculations apparently uses cyclotomic polynomials

  # "MIRE" is just an internal acronym for mortgage interest rate exponent
  # This is part of the calculation formula that is used a few times
  # For readability I have just calculated it separately as "MIRE"
  MIRE <- ( 1 + monthly_interest ) ^ month_of_interest

  # Does the calculation to work out the outstanding amount after the month of interest
  outstanding_owed <- MIRE * principal - ( (MIRE-1) / monthly_interest * monthly_payment)

  # If outstanding_owed is less then 0, make it 0. This is to
  # compensate if we use an alternate payment to overpay
  if( outstanding_owed < 0) { outstanding_owed = 0 }

  # Returns the data
  return( outstanding_owed )

}
