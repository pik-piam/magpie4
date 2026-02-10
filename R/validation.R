#' @title validation
#' @description Create Validation pdf from MAgPIE output and corresponding validation.mif
#'
#' @export
#'
#' @param gdx GDX file
#' @param hist  Validation data. All formats allowed which can be converted to quitte (including characters containing the path to a mif file)
#' @param file a file name the output pdf
#' @param runinfo (optional) Rdata object with run information
#' @param clusterinfo (optional) RDS file or vector containing mapping information on 0.5degree between regions and cluster
#' @param debug Switch to activate or deactivate debug mode
#' @param reportfile file name to which a backup of the magpie reporting should be written (file ending should be ".mif"). No report written if set to NULL or
#' if report is already provided via getReport!
#' @param scenario scenario name used inside reportfile. Not used if reportfile is NULL.
#' @param getReport the return value of the \code{getReport} function. Can be provided if available to reduce
#' overall runtime.
#' @param ... additional arguments supplied to the validationpdf function
#' @author Jan Philipp Dietrich
#' @examples
#'
#'   \dontrun{
#'     validation("fulldata.gdx","validation.mif",filter="Yield")
#'   }
#'
#' @importFrom mip validationpdf
#' @importFrom lusweave swopen swlatex swclose swR swtable swfigure
#' @importFrom magclass getYears getRegions
#' @importFrom utils methods
#' @importFrom mip plotstyle
#' @importFrom utils capture.output
#' @importFrom magclass write.report2

validation <- function(gdx,hist,file="validation.pdf",runinfo=NULL, clusterinfo=NULL, debug=FALSE, reportfile=NULL, scenario=NULL, getReport=NULL, ...) {

  template <-  c("\\documentclass[a4paper, portrait ]{article}",
                 "\\setlength{\\parindent}{0in}",
                 "\\usepackage{float}",
                 "\\usepackage[bookmarksopenlevel=section]{hyperref}",
                 "\\hypersetup{bookmarks=true,pdfauthor={PIK Landuse group}}",
                 "\\usepackage{graphicx}",
                 "\\usepackage{rotating}",
                 "\\usepackage[strings]{underscore}",
                 "\\usepackage[margin=2cm]{geometry}",
                 "\\usepackage{fancyhdr}",
                 "\\pagestyle{fancy}",
                 "\\begin{document}",
                 "<<echo=false>>=",
                 "options(width=90)",
                 "@")

  sw <- swopen(outfile = file, template = template)
  swlatex(sw,c("\\title{MAgPIE run analysis}","\\author{Aperture Science Enrichment Center}","\\maketitle","\\tableofcontents"))
  on.exit(swclose(sw, clean_output=!debug, engine="knitr"))


  #Warnings
  if(!is.null(runinfo)) {
    validation <- list(technical=list())
    load(runinfo)
    #########Warnings##############
    if(!is.null(validation$technical$last.warning)) {
      swlatex(sw,"\\part{Warnings}")
      swR(sw,"structure",validation$technical$last.warning, class = "warnings")
    }
  }

  swlatex(sw,"\\part{Basics}")

  #########Region map##############
  swlatex(sw,"\\subsection{World regions}")

  if(!is.null(clusterinfo)) {
    if(is.character(clusterinfo) && length(clusterinfo)==1) {
      clusterinfo <- readRDS(clusterinfo)$cluster
    }
    swfigure(sw,luplot::plotregionscluster, clusterinfo, fig.orientation="landscape")
  } else {
    i2iso <- readGDX(gdx,"i_to_iso", react="silent")
    if(!is.null(i2iso)) {
      map <- as.magpie(i2iso[2:1],spatial=1)
      col <- plotstyle(levels(as.factor(i2iso[[1]])))
      plotcountrymap<-function(x,hatching=FALSE,...) {
        namedim<-getNames(x)
        if(is.null(namedim)){namedim=1}
        year<-getYears(x)
        if(is.null(year)){year=1}
        if (hatching==FALSE) {
          if (length(namedim)>1) {stop("please provide only one name-column per plot")}
        } else {
          if (length(namedim)>2) {stop("please provide only one name-column per plot")}
        }
        if (length(year)>1) {stop("please provide only one year per plot")}
        countries <- getRegions(x)
        values <- as.vector(x[,year,namedim])

        if (hatching){
          DF <- data.frame(country = countries,namedim = values,hatching=as.vector(x[,year,2]))
          dimnames(DF)[[2]][[2]] <- paste(namedim[1],substr(year,2,5))
          dimnames(DF)[[2]][[3]] <- paste(namedim[2],substr(year,2,5))
          mapobject <- rworldmap::joinCountryData2Map(DF, joinCode = "ISO3",nameJoinColumn = "country")
          rworldmap::mapCountryData(mapobject, nameColumnToPlot = dimnames(DF)[[2]][[2]],nameColumnToHatch=dimnames(DF)[[2]][[3]],...)

        } else{
          DF <- data.frame(country = countries,namedim = values)
          dimnames(DF)[[2]][[2]] <- paste(namedim,substr(year,2,5))
          mapobject <- rworldmap::joinCountryData2Map(DF, joinCode = "ISO3",nameJoinColumn = "country")
          rworldmap::mapCountryData(mapobject, nameColumnToPlot = dimnames(DF)[[2]][[2]],...)

        }

      }
      tmpplot <- function(...)  {
        a <- capture.output(plotcountrymap(...))
      }
      swfigure(sw,tmpplot,map,colourPalette=col,catMethod = "categorical", mapTitle="", fig.orientation="landscape")
    } else {
      swlatex(sw,"Could not find mapping between countries and regions in gdx file!")
    }
  }

  #########Modelstat and goal function value##############
  swlatex(sw,"\\subsection{Modelstat}")
  #Modelstat
  modstat<-modelstat(gdx)
  if(!is.null(modstat)) {
    swtable(sw,modstat,table.placement="H",caption.placement="top",transpose=TRUE,caption="main",vert.lines=1,align="c")
  } else {
    swlatex(sw,"Could not find modelstat in gdx file!")
  }

  swlatex(sw,"\\subsection{Food Modelstat}")
  modstat<-foodmodelstat(gdx)
  if(!is.null(modstat)) {
    swtable(sw,modstat,table.placement="H",caption.placement="top",transpose=FALSE,caption="main",vert.lines=1,align="c")
  } else {
    swlatex(sw,"Could not find food model statistics in gdx file!")
  }

  #global costs in billion USD
  swlatex(sw,"\\subsection{Goal function value}")
  costs <- costs(gdx,level = "glo", sum=FALSE)
  if(!is.null(costs)) {
    costs_tot <- dimSums(costs, dim=3)
    swtable(sw,costs_tot/1000,table.placement="H",caption.placement="top",transpose=TRUE,caption="Global costs (billion USD)",vert.lines=1,align="c")

    # decomposition of costs (in billion USD and in ratios)
    swlatex(sw,"\\subsubsection{Total costs decomposition}")

    swfigure(sw, luplot::magpie2ggplot2, costs/1000, geom="bar", group="Data1", color="Data1",
             ylab="Total costs decompositino [bill. US$]", stack=TRUE, fill="Data1",
             stack_share=F, facet_x="Region", legend_position="bottom", legend_ncol=2)
    swfigure(sw, luplot::magpie2ggplot2, costs, geom="bar", group="Data1", color="Data1",
             ylab="Total costs decompositon [%]", stack=TRUE, fill="Data1",
             stack_share=T, facet_x="Region", legend_position="bottom", legend_ncol=2)
  } else {
    swlatex(sw,"Could not find goal function value in gdx file!")
  }

  ###################Validation output################################
  if(is.null(getReport)) {
    x <- getReport(gdx, scenario = scenario)
  } else{
    x <- getReport
  }


  if(!is.null(reportfile) && is.magpie(x)) write.report2(x,reportfile)
  validationpdf(x,hist=hist,file=sw,debug=debug,prefix="",...)


  ################Additional run information#########################
  swlatex(sw,"\\part{Run Information}")

  swlatex(sw,"\\section{Calibration}")

  #########yield calib factor##############
  swlatex(sw,"\\subsection{Yield calibration factors}")
  calib_factor<-readGDX(gdx, "f14_yld_calib", format="first_found", react="silent")
  if(!is.null(calib_factor)) {
    dimnames(calib_factor)[[3]]<-c("crops","pasture")
    swtable(sw,calib_factor[,1,],table.placement="H",include.rownames=TRUE,transpose=TRUE,vert.lines=1,align="c",hor.lines=1)
  } else {
    swlatex(sw,"Could not find calibration factors in gdx file!")
  }

  #########Land use change cropland 1995##############
  swlatex(sw,"\\subsection{Land use change in 1995 (reshuffling)}")
  land <- land(gdx,level="cell",sum=FALSE)
  croparea <- croparea(gdx,level="regglo",water_aggr=TRUE)
  if(!is.null(land) & !is.null(croparea)) {
    land <- land[,"y1995",]
    land_start <- readGDX(gdx,"pm_land_start", format="first_found")[,,getNames(land)]
    if(dim(land_start)[3] == 12) land_start <- dimSums(land_start,dim=3.2)
    dimnames(land_start)[[2]]<-"y1995"
    diff <- land - land_start

    contraction <- superAggregateX((diff < 0) * diff,level="regglo",aggr_type="sum")
    expansion   <- superAggregateX((diff > 0) * diff,level="regglo",aggr_type="sum")

    land <- superAggregateX(land,level="regglo",aggr_type="sum")
    land_start <- superAggregateX(land_start,level="regglo",aggr_type="sum")


    getNames(croparea) <- "crop"

    expansion_crop <- setNames(expansion[,,"crop"],"expansion")
    contraction_crop <- setNames(contraction[,,"crop"],"contraction")
    net_changes <- setNames(land[,,"crop"] - land_start[,,"crop"], "net changes")
    gross_changes <- setNames(abs(expansion_crop)+setNames(abs(contraction_crop),NULL), "gross changes")

    all<-mbind(expansion_crop,contraction_crop,net_changes,gross_changes)
    swtable(sw,all[,"y1995",],transpose=TRUE,caption.placement="top",caption="Land use change cropland 1995 (Mio. ha)",table.placement="H",vert.lines=1,align="r",hor.lines=1)
  } else {
    swlatex(sw,"Could not find required data in gdx file to calculate reshuffling in 1995!")
  }

  if(!is.null(runinfo)) {
    load(runinfo)
    swlatex(sw,"\\section{Model settings}")

    #########Model version##############
    if(!is.null(validation$technical$model_setup)) {
      swlatex(sw,"\\subsection{Code settings}")
      model_setup<-validation$technical$model_setup
      swR(sw,"cat",model_setup,sep='\n',fill=TRUE)
    }
    #########Input dataset##############
    if(!is.null(validation$technical$input_data)) {
      swlatex(sw,"\\subsection{Dataset}")
      input_data<-validation$technical$input_data
      swR(sw,"cat",input_data,sep='\n',fill=TRUE)
    }
    #########Module Interfaces##############
    if(!is.null(validation$technical$modules)) {
      swlatex(sw,"\\subsection{Module Interfaces}")
      swR(sw,print,validation$technical$modules)
    }

    #########R Informations##############
    if(!is.null(validation$technical$setup_info)) {
      swlatex(sw,"\\subsection{R Information}")
      for(n in names(validation$technical$setup_info)) {
        swlatex(sw,paste0("\\paragraph{",n,"}"))
        swR(sw,print,validation$technical$setup_info[[n]]$sessionInfo)
        swR(sw,print,validation$technical$setup_info[[n]]$libPaths)
        swR(sw,print,validation$technical$setup_info[[n]]$installedpackages[,"Version"])
      }
    }


    #########Runtime information##############
    swlatex(sw,"\\section{Runtime information}")
    runtime<-NULL
    for(d in c(lapply(validation$technical$time,as.numeric,units="hours"),recursive=TRUE)) {
      runtime <- c(runtime,paste(format(floor(d),width=2),'h ',
                                 format(floor((d-floor(d))*60),width=2),'m ',
                                 format(round(((d-floor(d))*60-floor((d-floor(d))*60))*60),width=2),'s',
                                 sep=''))
    }
    names(runtime) <- names(validation$technical$time)
    tmp <- NULL
    for(i in 1:length(runtime)) {
      runtime[i] <- paste(format(names(runtime[i]),width=20),': ',runtime[i],sep='')
    }
    swR(sw,"cat",runtime,sep='\n',fill=TRUE)
  }

}
