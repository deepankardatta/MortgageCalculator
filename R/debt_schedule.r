#' @title "debt_schedule": Calculates the mortgage debt schedule
#'
#' @description Calculates the mortgage debt schedule
#' @description This function calculates mortgage estimates. You should seek professional advice before making financial decisions.
#'
#' @author Deepankar Datta <deepankardatta@nhs.net>
#'
#' @import lubridate
#'
#' @param annual_interest_rate The annual interest rate of the mortgage
#' @param term The mortgage term in months
#' @param principal The principal of the mortgage
#' @param starting_date (OPTIONAL) If a starting date is supplied the schedule will autofill with years and dates
#'
#' @return debt_schedule A data frame containing the mortgage schedule
#'
#' @references Formulas sourced from https://en.wikipedia.org/wiki/Mortgage_calculator
#'
#' @examples
#' # debt_schedule( annual_interest_rate , term , principal , starting_date=NULL )
#'
#' @export

debt_schedule <- function( annual_interest_rate , term , principal , starting_date=NULL ){

  warning('This function calculates mortgage estimates. You should
          seek professional advice before making financial decisions.')

  # Error check
  # Need to check if starting_date is valid

  # Create lists
  month_list <- list()
  amount_owed_list <- list()

  # I created a separate function to work out the amount owed at any month
  # Then I have vectorised a call to the function to get the schedule for
  # all months

  month_list <-
    c(0:term)
  amount_owed_list <-
    amount_owed_after_n_months( annual_interest_rate , term , principal , c(0:term) )

  # Optional date lists
  if( !is.null(starting_date) ) {
    date_list <- list()
    year_list <- list()
    month_name <- list()
    date_list <- starting_date + months( c(0:term) )
    year_list <- year( date_list )
    month_name <- month( date_list , label = TRUE)
  }

  # Return data frame
  if( !is.null(starting_date) ) {
    debt_schedule <- data.frame( month_list , year_list , month_name , amount_owed_list )
  }
  else {
    debt_schedule <- data.frame( month_list , amount_owed_list )
  }

  return( debt_schedule )

}
