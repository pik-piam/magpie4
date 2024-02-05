
level <- "grid"
variable <-"crop"
statistic <- c("R2") # , "MAE", "bias", "error"
dataset <-"MAPSPAM"
dir <- "/p/projects/landuse/users/mbacca/magpie_versions/develop-1/magpie/output/default_2024-01-25_09.04.15/"

for(s in statistic){
a<-cellularFit(gdx, file=NULL, level=level, statistic=s,variable=variable,dataset=dataset,water_aggr =FALSE,dir=dir)
}

