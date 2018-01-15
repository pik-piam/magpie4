input_land<-function(gdx,file=NULL,level="cell",siclass="all") {
  output<-suppressWarnings(readGDX(gdx,"f10_land","f_land", format="first_found"))
  #bugfix2
  if(is.null(output)){
    t <- readGDX(gdx,"t")
    si0<-readGDX(gdx,"fm_land_si0","f_land_si0", format="first_found")[,t,]
    dimnames(si0)[[3]]<-paste(dimnames(si0)[[3]],"si0",sep=".")
    nsi0<-readGDX(gdx,"fm_land_nsi0","f_land_nsi0", format="first_found")[,t,]
    dimnames(nsi0)[[3]]<-paste(dimnames(nsi0)[[3]],"nsi0",sep=".")
    output<-mbind(si0,nsi0)
  }
  if(is.null(output)){
    warning("No element of f_land, fm_land_si0, f_land_si0 found in GDX file! NULL returned")
    return(NULL)
  }
  if(siclass=="si0") output<-output[,,"si0"]
  if(siclass=="nsi0") output<-output[,,"nsi0"]
  if(siclass=="sum") output<-dimSums(output,dim=3.2)
  if(level!="cell") output<-superAggregate(output,aggr_type="sum",level=level)
  out(as.magpie(output),file)
}