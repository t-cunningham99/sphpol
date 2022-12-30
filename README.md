# sphpol: Spherical Polarization R function
See Cunningham (Working Paper 2022) Spherical Polarization: A Measurement Approach for Compositional Data

https://github.com/t-cunningham99/sphpol

29 December 2022

## Description
Measures polarization of systems of ordered compositional data

## Usage 
sphpol(l = left, c = center, r = right)

## Arguments
left: Lower extreme of the system (numeric, must be between 0 and 1)

center: Center category of the system (numeric, must be between 0 and 1)

right: Upper extreme of the system (numeric, must be between 0 and 1)

NOTE: left + center + right must be equal to 1

## Details
System polarization will be a number between 0 and pi/2, with pi/2 being the most polarized system 

A value of pi/2 is associated with an even concentration split between left and right

A value of 0 is associated with a complete concentration in either left, center, or right 

## References
Cunningham (Working Paper 2022) Spherical Polarization: A Measurement Approach for Compositional Data

https://github.com/t-cunningham99/sphpol


## Anticipated updates (As of 29 December 2022)
Addition of functionality to systems with > 3 ordered bins

Smoother applicability to dataframes without manual loops

Visualization optionality 

## Example (See polarization_test.R)
require(readxl)

require(pracma)

require(plotly)

require(readstata13)

require(dplyr)

source("polarization_function.R")

dfPol <- read_xlsx("pol_basefile.xlsx")

dfPol$lev <- as.numeric(rownames(dfPol))

dfPol$output <- NA

dfPol$l_norm <- NA

dfPol$c_norm <- NA

dfPol$r_norm <- NA

for (i in 1:5151) {

  dfTest <- dfPol[i,]
  test <- sphpol(l = dfTest$lpop_pct, 
                 c = dfTest$center_pct, 
                 r = dfTest$rpop_pct)

  dfPol$output[i] <- test[[1]]
  
  dfPol$l_norm[i] <- test[[2]]
  
  dfPol$c_norm[i] <- test[[3]]
  
  dfPol$r_norm[i] <- test[[4]]
  
  dfPol$sdist_pp[i] <- test[[5]]

}


ppol <- max(dfPol$output)

p <- plot_ly(data = dfPol, 
             x = dfPol$l_norm, 
             z = dfPol$c_norm, 
             y = dfPol$r_norm, 
             type = "mesh3d",
             cmin = 0, 
             cmax = ppol, 
             intensity = dfPol$output)

p
