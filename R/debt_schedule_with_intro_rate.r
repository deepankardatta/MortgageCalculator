#' @title "debt_schedule_with_intro_rate": Calculates the mortgage debt schedule
#'
#' @description Calculates the mortgage debt schedule, when an introductory rate is available
#' @description This function calculates mortgage estimates. You should seek professional advice before making financial decisions.
#'
#' @author Deepankar Datta <deepankardatta@nhs.net>
#'
#' @import lubridate
#'
#' @include debt_schedule.r
#'
#' @param introductory_interest_rate The introductory interest rate
#' @param introductory_term The introductory term in months
#'
#' @return debt_schedule A data frame containing the mortgage schedule
#'
#' @references Formulas sourced from https://en.wikipedia.org/wiki/Mortgage_calculator
#'
#' @examples
#' # debt_schedule_with_intro_rate( annual_interest_rate , term , principal , introductory_interest_rate , introductory_term , starting_date=NULL )
#'
#' @export


debt_schedule_with_intro_rate <- function( annual_interest_rate ,
                                           term ,
                                           principal ,
                                           introductory_interest_rate ,
                                           introductory_term ,
                                           starting_date=NULL ,
                                           alternate_payment=NULL ){

  warning('This function calculates mortgage estimates. You should
          seek professional advice before making financial decisions.')

  # Error check
  # Need to check if starting_date is valid
  # Need to check if introductory_interest_rate and introductory_term are null

  # Calculate introductory rate
  intro_rate_schedule <-
    debt_schedule( introductory_interest_rate ,
                   term ,
                   principal ,
                   starting_date ,
                   alternate_payment )

  # Remove all the rows after the introductory term
  intro_rate_schedule <- intro_rate_schedule[ 1:introductory_term , ]

  ### Calculate rate once moves back to SVR ###

  # First bit gets some calculations out of the way to
  # take into account there was an introductory rate

  # Calculates how many months are left after the introductory rate
  SVR_term <- term - introductory_term
  # Calculates the starting date of the SVT interest rate
  if( !is.null(starting_date) ) {
    SVR_starting_date <- starting_date + months(introductory_term)
  }
  # Calculates how much money is left
  SVR_principal <- amount_owed_after_n_months( introductory_interest_rate ,
                                term ,
                                principal ,
                                introductory_term ,
                                alternate_payment )

  # This now does the actual calculation
  SVR_schedule <- debt_schedule( annual_interest_rate ,
                                 SVR_term ,
                                 SVR_principal ,
                                 SVR_starting_date ,
                                 alternate_payment)
  SVR_schedule$month_list <- SVR_schedule$month_list + introductory_term

  # Combine the 2 data frames
  debt_schedule <- rbind( intro_rate_schedule , SVR_schedule )
  debt_schedule$amount_owed_list <- round( debt_schedule$amount_owed_list , 2 )

  # Return data
  return( debt_schedule )

}
