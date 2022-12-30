gc()
rm(list = ls())
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

save.dta13(dfPol, file = "tempfiles/poltest.dta")

