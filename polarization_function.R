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
  
  lcpp_norm <- sqrt(0.5)
  ccpp_norm <- sqrt(0.5)
  rcpp_norm <- 0
  
  lcpp_vec <- as.vector(c(lcpp_norm, 
                        ccpp_norm, 
                        rcpp_norm))
  
  
  cross_lcpp <- cross(obs_vec, 
                    lcpp_vec)
  
  num_lcpp <- sqrt(cross_lcpp[1]^2 + cross_lcpp[2]^2 + cross_lcpp[3]^2)
  
  denom_lcpp <- as.numeric(obs_vec %*% lcpp_vec)
  
  sdist_lcpp <- atan(num_lcpp/denom_lcpp)
  
  lrpp_norm <- 0
  crpp_norm <- sqrt(0.5)
  rrpp_norm <- sqrt(0.5)
  
  rcpp_vec <- as.vector(c(lrpp_norm, 
                        crpp_norm, 
                        rrpp_norm))
  
  
  cross_rcpp <- cross(obs_vec, 
                    rcpp_vec)
  
  num_rcpp <- sqrt(cross_rcpp[1]^2 + cross_rcpp[2]^2 + cross_rcpp[3]^2)
  
  denom_rcpp <- as.numeric(obs_vec %*% rcpp_vec)
  
  sdist_rcpp <- atan(num_rcpp/denom_rcpp)
  
  lextl_norm <- 1
  lextc_norm <- 0
  lextr_norm <- 0
  
  lext_vec <- as.vector(c(lextl_norm, 
                          lextc_norm, 
                          lextr_norm))
  
  cross_lext <- cross(obs_vec, 
                      lext_vec)
  
  
  num_lext <- sqrt(cross_lext[1]^2 + cross_lext[2]^2 + cross_lext[3]^2)
  
  denom_lext <- as.numeric(obs_vec %*% lext_vec)
  
  sdist_lext <- atan(num_lext/denom_lext)
  
  
  rextl_norm <- 0
  rextc_norm <- 0
  rextr_norm <- 1
  
  rext_vec <- as.vector(c(rextl_norm, 
                          rextc_norm, 
                          rextr_norm))
  
  cross_rext <- cross(obs_vec, 
                      rext_vec)
  
  
  num_rext <- sqrt(cross_rext[1]^2 + cross_rext[2]^2 + cross_rext[3]^2)
  
  denom_rext <- as.numeric(obs_vec %*% rext_vec)
  
  sdist_rext <- atan(num_rext/denom_rext)
  
  
  
  cextl_norm <- 0
  cextc_norm <- 1
  cextr_norm <- 0
  
  cext_vec <- as.vector(c(cextl_norm, 
                          cextc_norm, 
                          cextr_norm))
  
  cross_cext <- cross(obs_vec, 
                      cext_vec)
  
  
  num_cext <- sqrt(cross_cext[1]^2 + cross_cext[2]^2 + cross_cext[3]^2)
  
  denom_cext <- as.numeric(obs_vec %*% cext_vec)
  
  sdist_cext <- atan(num_cext/denom_cext)
  
  l_cline <- sqrt((1 - c)/2)
  c_cline <- sqrt(c)
  r_cline <- sqrt((1 - c)/2)
  
  mag_cline <- sqrt(l_cline^2 + c_cline^2 + r_cline^2)
  l_norm_cline <- l_cline/mag_cline
  c_norm_cline <- c_cline/mag_cline
  r_norm_cline <- r_cline/mag_cline

  cline_vec <- as.vector(c(l_norm_cline, 
                         c_norm_cline, 
                         r_norm_cline))

  cross_cline <- cross(obs_vec, 
                    cline_vec)
  
  num_cline <- sqrt(cross_cline[1]^2 + cross_cline[2]^2 + cross_cline[3]^2)
  
  denom_cline <- as.numeric(obs_vec %*% cline_vec)
  
  sdist_cline <- atan(num_cline/denom_cline)
  
  
  
  pot <- pi/2
  
  psos <- (pi^2/16)
  
  pcos <- (pi^3/64)
  
  pof <- pi/4
  
  pcot <- (pi^3/32)
  
  output <- (pot - sdist_pp) * ((sdist_lext * sdist_rext) / psos)
  
  pol_other <- (pot - sdist_pp) * ((pof - (sdist_cline))/pof) * ((sdist_lext * sdist_rext) / psos)
  
  pol_alt <- (pot - sdist_pp) * (pof - (sdist_cline))/pof
  
  pol_alternative <-  (pot - sdist_pp) * (1 - abs(l - r))
  
  
  sphere <- list(output, 
                 l_norm, 
                 c_norm, 
                 r_norm, 
                 sdist_pp, 
                 sdist_lext,
                 sdist_rext, 
                 sdist_cline,
                 pol_other, 
                 pol_alt, 
                 pol_alternative)
  
  #return(output)
  return(sphere)
  
  
}