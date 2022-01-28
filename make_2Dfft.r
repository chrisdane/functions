##--------------------------------------------------------------------------------------------------
## R-Funktion: Detrending of 2D-Fields and determine Fourier coefficients
## Lotte Bierdel, May 2012
##--------------------------------------------------------------------------------------------------
## INPUT
## field: 2D Feld 
## OUTPUT
## cpq.small: 2D Fourier Coefficients
##-------------------------------------------------------------------------------------------------

make_2Dfft <- function(aij=field){

# Detrending following Errico (1985)

	Nx <- dim(aij)[1]
	Ny <- dim(aij)[2]
	i <- seq(1,Nx)
	j <- seq(1,Ny)
	sj <- rep(NA,length(j))
	si <- rep(NA,length(i))
	aij.detrend <- Re(aij[(1:Nx),(1:Ny)])


	sj <- (Re(aij[Nx,])-Re(aij[1,]))/(Nx-1)

	sj <- Re(sj)

	for(ii in 1:Nx){
	aij.detrend[ii,] <- aij[ii,] - (2*ii-Nx-1)*sj/2
	}

	si <- (Re(aij.detrend[,Ny])-Re(aij.detrend[,1]))/(Ny-1)
	si <- Re(si)

	for(jj in 1:Ny){
	aij.detrend[,jj] <- aij.detrend[,jj] - (2*jj-Ny-1)*si/2
	}

# Receive 2D-Fourier coefficients

	cpq.back <- fft(aij.detrend)/((Nx-1)*(Ny-1))
	l.back <- trunc(length(cpq.back[,1])/2)
	l1.back <- trunc(length(cpq.back[1,])/2)
	cpq.small <- cpq.back[(1:(l.back+1)),(1:(l1.back+1))]

return(list(aij.detrend=aij.detrend, #detrended field
	    cpq.small=cpq.small))   # field of 2D Fourier coefficients
}


