# Modify gs values:
# gs is defined by its max value, multiplied with relative Tair scale factor and scaled vpd

#' @param    Tair    (deg C) air temperature
#' @param    vpd     (Pa)    vapour pressure deficit
#' @param    gs      (s/mm)  surface conductance
#' @author Yoav BD
#' @return Evapotranspiration (mm/day)


modified_gs2 =
  function(Tair, vpd, gs) {
    # find max value of gs 
    maxgs=max(gs)

    # parametrize a linear gs-t relationship
    xval=seq(from=min(Tair) , to=max(Tair), by=0.001 )
    yval=seq(from=0.8 , to=1, length.out =length(xval))
    gstfactor=rep(0,length(gs))
    for (ii in 1:length(gs)) {
      gstfactor[ii]=yval[match(Tair[ii],xval)]
    }

    # scale the vpd values
    scaledvpd=vpd/max(vpd)
    
    newgs=maxgs*scaledvpd*gstfactor
  }