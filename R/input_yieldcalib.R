input_yieldcalib<-function(gdx,file=NULL) {
  output<-readGDX(gdx,"f14_yld_calib","f_yld_calib", format="first_found")
  out(output,file)  
}