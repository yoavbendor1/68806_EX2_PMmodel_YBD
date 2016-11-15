# this function modifies the gs value according to the Naomi's example

#' @param        Tair    (deg C) air temperature
#' @param     gs      (s/mm)  surface conductance
#' @author Yoav BD
#' @return new gs (s/mm)

modified_gs=
  function(gs, Tair){
  cond1=mean(Tair)
  maxgs=max(gs)
  # calculate ET
  newgs=ifelse (Tair >= cond1, maxgs, 0)
}

