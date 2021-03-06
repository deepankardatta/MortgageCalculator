% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monthly_mortgage_payments.r
\name{monthly_mortgage_payments}
\alias{monthly_mortgage_payments}
\title{"monthly_mortgage_payments": Calculates monthly mortgage payments}
\usage{
monthly_mortgage_payments(annual_interest_rate, term, principal)
}
\arguments{
\item{annual_interest_rate}{The annual interest rate of the mortgage}

\item{term}{The mortgage term in months}

\item{principal}{The principal of the mortgage}
}
\value{
monthly_payment A value with the estimated monthly mortgage payment
}
\description{
Calculates monthly mortgage payments from mortgage data.

This function calculates mortgage estimates. You should seek professional advice before making financial decisions.
}
\examples{
# monthly_mortgage_payments( annual_interest_rate , term , principal )
# payments <- monthly_mortgage_payments( annual_interest_rate , term , principal )

}
\references{
Formulas sourced from https://en.wikipedia.org/wiki/Mortgage_calculator
}
\author{
Deepankar Datta <deepankardatta@nhs.net>
}
