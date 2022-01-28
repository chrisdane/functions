##--------------------------------------------------------------------------------------------------
## R-Funktion to calculate 1D spectra
## Lotte Bierdel, May 2012
##-------------------------------------------------------------------------------------------------
## INPUT
## Nx: Number of Gridpoints in x-direction
## Ny: Number of gridpoints in y-direction
## delta_x: Horizontal grid spacing
## cpq.small: Field with 2D Fourier coefficients
## OUTPUT
## S: one-dimensional spectrum
## k : one-dimensional wavenumber
##-------------------------------------------------------------------------------------------------


make_1Dspectrum <- function(Nx=Nx,Ny=Ny,delta_x=delta_x,cpq.small=cpq.small){

	lx <- seq(0,trunc(Nx/2))
	ly <- seq(0,trunc(Ny/2))

	# be, 18.07.2014
	if(Nx>=Ny){ 
	  delta_k <- 2*pi/(delta_x*(Nx-1))
	  k <- lx*delta_k
	}else{
	  delta_k <- 2*pi/(delta_x*(Ny-1))
	  k <- ly*delta_k
	} # be

	S <- rep(NA,length(k))

# Define matrix mat with the entries sqrt(p_sqr + q_sqr)
 p <- 2*pi*lx/(delta_x*(Nx-1))
 q <- 2*pi*ly/(delta_x*(Ny-1))

 p.mat <-array(p,c(length(lx),length(ly))) 
 q.mat <-t(array(q,c(length(ly),length(lx)))) 
 mat <- sqrt(p.mat^2+q.mat^2)

	for(ik in k){
	S[which(k==ik)] <- 0
	k_1 <- ik - delta_k/2
	k_2 <- ik + delta_k/2

	root <- which(mat<k_2&mat>k_1,arr.ind=TRUE)
		# Determine Fourier coefficients which contribute to S[k==ik]
		for(j in 1:length(root[,1])){
		lx <- as.double(root[j,1])
		ly <- as.double(root[j,2])

		S[which(k==ik)] <- S[which(k==ik)] + cpq.small[lx,ly]*Conj(cpq.small[lx,ly])
		}

     }

S <- Re(S)
S <- S*sqrt(4)

return(list(S=S, # Spectrum S
	k=k)) # 1D Wavenumber k = sqrt(p^2+q^2)

}
