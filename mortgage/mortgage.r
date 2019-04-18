mortgage <- function(P=5000000, I=1, L=300, amort=T, plotData=T) {
	J <- I/(12 * 100)
	N <- L
	M <- P*J/(1-(1+J)^(-N))
	monthPay <<- M
	cat("\nThe payments for this loan are:\n\n\n",
			"\tMonthly payment: \t￥",format(M, big.mark=",", scientific=F)," (stored in monthPay)\n",
			"\tTotal cost: \t\t￥", format(M*N, big.mark=",", scientific=F), "\n\n", sep="")
	# Calculate Amortization for each Month
	if(amort==T) {
		Pt <- P # current principal or amount of the loan
		currP <- NULL
		while(Pt>=0) {
			H <- Pt * J # this is the current monthly interest
			C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
			Q <- Pt - C # this is the new balance of your principal of your loan
			Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
			currP <- c(currP, Pt)
		}
		monthP <- c(P, currP[1:(length(currP)-1)])-currP
		aDFmonth <<- data.frame(
					      Amortization=c(P, currP[1:(length(currP)-1)]),
					      Monthly_Payment=monthP+c((monthPay-monthP)[1:(length(monthP)-1)],0),
					      Monthly_Principal=monthP,
					      Monthly_Interest=c((monthPay-monthP)[1:(length(monthP)-1)],0),
					      Year=sort(rep(1:ceiling(N/12), 12))[1:length(monthP)]
				)
		aDFyear <- data.frame(
					     Amortization=tapply(aDFmonth$Amortization, aDFmonth$Year, max),
					     Annual_Payment=tapply(aDFmonth$Monthly_Payment, aDFmonth$Year, sum),
					     Annual_Principal=tapply(aDFmonth$Monthly_Principal, aDFmonth$Year, sum),
					     Annual_Interest=tapply(aDFmonth$Monthly_Interest, aDFmonth$Year, sum),
					     Year=as.vector(na.omit(unique(aDFmonth$Year)))
					     )
		aDFyear <<- aDFyear
		cat("The amortization data for each of the", N, "months are stored in \"aDFmonth\".\n\n")
		cat("The amortization data for each of the", L, "years are stored in \"aDFyear\".\n\n")
	}
	if(plotData==T) {
	barplot(t(aDFyear[,c(3,4)]),
		col=c("blue", "red"),
		main="Annual Interest and Principal Payments",
		sub="The data for this plot is stored in aDFyear.",
		xlab="Years", ylab=" Amount",
		legend.text=c("Principal", "Interest"),
		ylim=c(0, max(aDFyear$Annual_Payment)*1.3))
	}
}
cat("The monthly mortgage payments and amortization rates can be calculted with the mortgage() function like this: \n
	mortgage(P=500000, I=6, L=30, amort=T, plotData=T)
		P = principal (loan amount)
		I = annual interest rate
		L = length of the loan in months(300=25years,120=10years)


		\n")
