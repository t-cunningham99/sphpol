sphpol <- function(l, 
                   c, 
                   r) {
  sl <- sqrt(l)
  sc <- sqrt(c)
  sr <- sqrt(r)
  mag <- sqrt(sl^2 + sc^2 + sr^2)
  l_norm <- sl/mag
  c_norm <- sc/mag
  r_norm <- sr/mag
  lpp_norm <- sqrt(0.5)
  cpp_norm <- 0
  rpp_norm <- sqrt(0.5)
  
  obs_vec <- as.vector(c(l_norm, 
                         c_norm, 
                         r_norm))
  pp_vec <- as.vector(c(lpp_norm, 
                        cpp_norm, 
                        rpp_norm))
  
  
  cross_pp <- cross(obs_vec, 
                 pp_vec)
  
  num_pp <- sqrt(cross_pp[1]^2 + cross_pp[2]^2 + cross_pp[3]^2)
  
  denom_pp <- as.numeric(obs_vec %*% pp_vec)
  
  sdist_pp <- atan(num_pp/denom_pp)
  
  pot <- pi/2
  
  output <- (pot - sdist_pp) * (1 - abs(l - r))
  
  
  sphere <- list(output, 
                 l_norm, 
                 c_norm, 
                 r_norm, 
                 sdist_pp)
  
  #return(output)
  return(sphere)
  
  
}