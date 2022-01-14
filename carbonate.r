# r

# air-sea CO2 flux = Fsurf 
# wanninkhof 2014:
#  = k (C_w - C_a)              | k    = gas transfer velocity (m s-1)
#                               | C    = concentrations (kg m-3 or mol m-3)
#  = k K0 (pCO2_w - pCO2_a)     | K0   = solubility (kg m-3 Pa-1 or mol m-3 Pa-1)
#                               | pCO2 = partial pressure CO2 in equilibrium between sea surface and air
# orr et al. 2017:
#  = kw ([A]_sat - [A])         | kw      = gas transfer velocity (m s-1)
#                               | [A]     = concentration of dissolved gas (mol m-3)
#                               | [A]_sat = corresponding saturation concentration in equilibrium between 
#                               |           sea surface and water-vapor-saturated air at total atmospheric pressure P_a 
# notation here:
#  = kw * alpha * DeltapCO2
# kw = gas transfer velocity = piston velocity
#    = (1-ice)*a(Sc/600)^(-0.5)*U^2
#   with a  = 6.97*1e-7 s m-1
#        Sc = Schmidt number = 2116.8 - 136.25*T_C + 4.7353*T_C^2 - 0.092307*T_C^3 + 0.0007555*T_C^4
# alpha = solubility of CO2 in seawater 
#       = exp(a1 + a2*100/T_K + a3*log(T_K/100) + a4*(T_K/100)^2 + S*(b1 + b2*T_K/100 + b3*(T_K/100)^2))
#  with a1  = -160.7333
#       a2  = 215.4152
#       a3  = 89.8920
#       a4  = -1.47759
#       b1  = 0.029941
#       b2  = -0.027455
#       b3  = 0.0053407
#       T_K = SST in Kelvin
#       S   = SSS in psu ?
# DeltapCO2 = difference of atmospheric and oceanic partial pressures = pCO2_ocean - pCO2_atm

# piston velocity kw
seacarb_piston_vel <- function(wind_speed_ms=10, sea_ice_frac=0, Sc, method="orr17") {
    if (method == "orr17") {
        const_a <- 6.97*1e-7 # s m-1
        (1-sea_ice_frac)*const_a*(Sc/600)^(-0.5)*wind_speed_ms^2
    } else {
        stop("method = ", method, " not implemented")
    }
} # seacarb_piston_vel

# Schmidt number Sc
# = ratio of kinematic viscosity of water ν to the diffusion coefficient of the gas D (Sc = ν/D)
seacarb_Sc <- function(temp_C=25, method="wanninkhof14") {
    if (method == "wanninkhof14") {
        2116.8 - 136.25*temp_C + 4.7353*temp_C^2 - 0.092307*temp_C^3 + 0.0007555*temp_C^4
    } else {
        stop("method = ", method, " not implemented")
    }
} # seacarb_Sc

# solubility alpha
seacarb_alpha_co2 <- function(temp_C=25, salt_psu=34, method="orr17") {
    if (method == "orr17") { # eq. 15
        a1 <- -160.7333
        a2 <- 215.4152
        a3 <- 89.8920
        a4 <- -1.47759
        b1 <- 0.029941
        b2 <- -0.027455
        b3 <- 0.0053407
        temp_K <- temp_C + 273.15
        exp(a1 + a2*100/temp_K + a3*log(temp_K/100) + a4*(temp_K/100)^2 + salt_psu*(b1 + b2*temp_K/100 + b3*(temp_K/100)^2)) 
    } else {
        stop("method = ", method, " not implemented")
    }
} # seacarb_alpha_co2

# air-sea CO2 flux
seacarb_Fsurf <- function(alpha, kw, DeltapCO2, method="orr17") {
    if (method == "orr17") {
        alpha*kw*DeltapCO2 # m s-1 µatm
    } else {
        stop("method = ", method, " not implemented")
    }
} # seacarb_Fsurf

seacarb_pCO2w_from_dCO2 <- function(dCO2=15, ...) {

    # dCO2 = dissolved CO2 in seawater in µmol kg-1
    # ... = args for seacarb::K0(); defaults: S=35, T=25, P=0, Patm=1, warn="y"
    library(seacarb)
    
    # from Follows et al. 2006 (eq. 5) or Williams and Follows (eq. 6.1): 
    pCO2 <- dCO2/seacarb::K0(...) # µmol kg-1 / (mol kg-1 atm-1)) = µatm
    attr(pCO2, "unit") <- "µatm"
    return(pCO2)

} # seacarb_pCO2w_from_dCO2

# copied from https://biocycle.atmos.colostate.edu/shiny/carbonate
pCO2w_from_alk_dic_tans98 <- function(SALT=34.78, TEMP=16., alk=2311, DIC=2002, pH=8.) {

    # Carbonate equilibrium system based on 
    # P. Tans, "Why Carbon Dioxide from Fossil Fuel Burning Won't Go Away" 
    # In: MacAladay, J. (ed),  "Environmental Chemistry," 1996. pp. 

    # Note that we are ignoring contributions of PO4 and SiO4 to alkalinity, which may be 
    # important regionally but probably lead to errors in pCO2 < 1 ppmv globally
    # See Follows et al (2006; https://www.sciencedirect.com/science/article/pii/S1463500305000533) for a discussion of this approximation

    # Default values if the user doesn't specify at runtime are for preindustrial global surface

    # Tans defines pCO2 via Henry's Law in atmospheres
    # I'm converting to ppmv for clarity 
    
    # Variable definitions

    # INPUT VARIABLES
    #       T      #  Temperature (Celsius)
    #       TA     #  titration alkalinity (equivalents/kg)
    #       DIC    #  total dissolved inorganic carbon (mol/kg)

    # OUTPUT VARIABLES
    #       pH     #  pH of the water (-log(H))
    #       pCO2    #  partial pressure of CO2 (Pascals)
    #       iter   iteration count

    # CHEMICAL & THERMODYNAMIC COEFFICIENTS  
    #       K0     #  Henry's Law constant
    #       K1     #  first dissociation coefficient for H2CO3
    #       K2     #  second dissociation coefficient for H2CO3
    #       Kb     #  dissociation constant for boric acid
    #       S      #  Salinity (g/kg)
    #       Boron  #  total boron (mol/kg)
    #       H0     #  "old" concentration of hydrogen ion (mol/kg)

    # INTERNAL VARIABLES USED TO CALCULATE pCO2 and pH
    #       H      #  current concentration of hydrogen ion (mol/kg)
    #       CA     #  carbonate alkalinity (eq / l)
    #       CO2aq  #  concentration of aqueous CO2
    #       diff.H  #  difference in successive estimates of H+ (mol/kg)
    #       tiny.diff.H   (1e-15) test for convergence 
    #       a      #  first term in quadratic for eq 12
    #       b      #  second term in quadratic for eq 12
    #       c      # third term in quadratic for eq 12

    if (F) {
        SALT=34.78
        TEMP=16.
        #alk=2311
        alk=2100:2500
        #DIC=2002
        DIC=1800:2200
        pH=8.
    }

    # check inputs
    nunique <- unique(c(length(SALT), length(TEMP), length(alk), length(DIC), length(pH)))
    if (length(nunique) > 2) stop("args are of ", length(nunique), " different lengths: ",
                                  paste(nunique, collapse=", "))
    if (length(nunique) > 1) { # args are of different length
        if (!any(nunique == 1)) stop("args are of ", length(nunique), " different lengths: ",
                                     paste(nunique, collapse=", "))
    }
    nmax <- max(nunique)
    if (length(SALT) != nmax) {
        if (length(SALT) == 1) {
            SALT <- rep(SALT, t=nmax)
        } else {
            stop("SALT is not of length 1 and not of length nmax = ", nmax)
        }
    }
    if (length(TEMP) != nmax) {
        if (length(TEMP) == 1) {
            TEMP <- rep(TEMP, t=nmax)
        } else {
            stop("TEMP is not of length 1 and not of length nmax = ", nmax)
        }
    }
    if (length(alk) != nmax) {
        if (length(alk) == 1) {
            alk <- rep(alk, t=nmax)
        } else {
            stop("alk is not of length 1 and not of length nmax = ", nmax)
        }
    }
    if (length(DIC) != nmax) {
        if (length(DIC) == 1) {
            DIC <- rep(DIC, t=nmax)
        } else {
        stop("DIC is not of length 1 and not of length nmax = ", nmax)
        }
    }
    if (length(pH) != nmax) {
        if (length(pH) == 1) {
            pH <- rep(pH, t=nmax)
        } else {
        stop("pH is not of length 1 and not of length nmax = ", nmax)
        }
    }

    # Convert input units to mks
    TEMP <- TEMP + 273.15 # temperature from Celsius to Kelvin
    alk <- alk * 1.e-6    # microequivalents to equivalents
    DIC <- DIC * 1.e-6    # micromoles to moles

    # Set values of prescribed constants
    S <- rep(34.78, t=nmax)    # Salinity in ppt
    Boron <- 1.179e-5 * S      # Total Boron mole/kg as a fraction of salinity

    # Carbonate and boric acid equilibrium constants as functions of temp and S
    K0 <- exp(-60.2409 + 9345.17/TEMP + 23.3585*log(TEMP/100) 
              + S * (0.023517 - 0.00023656*TEMP +0.0047036*(TEMP/100)^2) )

    K1 <- exp(2.18867 - 2275.036/TEMP - 1.468591 * log(TEMP) 
              + (-0.138681 - 9.33291/TEMP) * sqrt(S) + 0.0726483*S    
              - 0.00574938 * S ^1.5)

    K2 <- exp(-0.84226 - 3741.1288/TEMP -1.437139 * log(TEMP)
              + (-0.128417 - 24.41239/TEMP)*sqrt(S) + 0.1195308 * S   
              - 0.0091284 * S ^1.5 )

    Kb <- exp( (-8966.90 - 2890.51*sqrt(S) - 77.942*S 
                + 1.726 * S ^1.5 - 0.0993*S^2) / TEMP                
                + (148.0248 + 137.194 * sqrt(S) + 1.62247 * S)       
                + (-24.4344 - 25.085 * sqrt(S) - 0.2474 * S) * log(TEMP)
                + 0.053105 * sqrt(S) * TEMP)      

    # Iterate for H and CA by repeated solution of eqs 13 and 12
    H <- 10.^(-pH)                 # initial guess from arg list      
    diff.H <- H     
    tiny.diff.H <- 1.e-15 
    
    # loop through provided vector
    CA <- iter_all <- rep(NA, t=nmax)
    for (i in seq_along(H)) {

        iter <- 0
        while (diff.H[i] > tiny.diff.H) {     # iterate until H converges
        
            H.old <- H[i]                      # remember old value of H

            # solve Tans' equation 13 for carbonate alkalinity from TA
            CA[i] <- alk[i] - (Kb[i]/(Kb[i]+H[i])) * Boron[i]     

            # solve quadratic for H (Tans' equation 12)
            a <- CA[i]
            b <- K1[i] * (CA[i] - DIC[i])
            c <- K1[i] * K2[i] * (CA[i] - 2 * DIC[i])
            H[i] <- (-b + sqrt(b^2 - 4. * a * c) ) / (2. * a)  

            # How different is new estimate from previous one?
            diff.H[i] <- abs(H[i] - H.old)
            iter <- iter + 1

        } # while

        iter_all[i] <- iter

    } # for i
    
    iter <- iter_all

    # Now solve for CO2 from equation 11 and pCO2 from eq 4
    CO2aq <- CA / (K1/H + 2*K1*K2/H^2)  # Eq 11
    pCO2 <- CO2aq / K0 * 1.e6           # Eq 4 (converted to ppmv)
    pH <- -log10(H)

    return(list(SALT=SALT, TEMP=TEMP-273.15, alk=alk*1.e6, DIC=DIC*1.e6, pH=pH, pCO2=pCO2, iter=iter))

} # pCO2w_from_alk_dic_tans98

