#' @title carbonHWP
#' @description reads carbon stored in harvested timber out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param unit element" or "gas"; "element": co2_c in Mt C/yr, n2o_n in Mt N/yr, ch4 in Mt CH4/yr; "gas": co2_c Mt CO2/yr, n2o_n in Mt NO2/yr, ch4 in Mt CH4/yr
#' @param half_life Half life in years for decay in wood products loosing half theor carbon content. (35 yrs is deafault)
#' @details Annual (and cumulative) Carbon stored in harvested wood products as well as slow emissions from half life deacy.
#' @param cumulative Logical; Determines if cHWP emissions are reported annually (FALSE) or cumulative (TRUE). The starting point for cumulative emissions is y1995.
#' @param baseyear Baseyear used for cumulative emissions (default = 1995)
#' @return carbon stocks in MtC from harvested timber
#' @author Abhijeet Mishra, Florian Humpenoeder
#' @importFrom gdx readGDX out
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @importFrom luscale superAggregate
#' @importFrom utils tail
#' @examples
#' 
#'   \dontrun{
#'     x <- carbonHWP(gdx)
#'   }

carbonHWP <- function(gdx, file=NULL, level="cell",unit="element", half_life=35, cumulative=FALSE, baseyear=1995){
  
  timber <- FALSE
  fore_red <- readGDX(gdx,"ov32_land_reduction","ov_forestry_reduction",select = list(type="level"),react = "silent", format="first_found")
  if (!is.null(fore_red)) {
    if (max(fore_red) > 1) {
      if(readGDX(gdx,"s73_timber_demand_switch")){
        timber <- TRUE
      }
    }
  }
   
  if (timber) {
    kforestry <- readGDX(gdx,"kforestry",react = "silent")
    
    ### Production of wood and woodfuel (tDM)
    prod <- collapseNames(readGDX(gdx,"ov_prod")[,,kforestry][,,"level"])
    first_yr <- as.numeric(gsub(x = head(getYears(prod),1),pattern = "y",replacement = ""))
    last_yr <- as.numeric(gsub(x = tail(getYears(prod),1),pattern = "y",replacement = ""))
    every_year <- new.magpie(getCells(prod),paste0("y",first_yr:last_yr),getNames(prod),0)
    
    ### Convert from annual values to total values
    timestep_length <- readGDX(gdx,"im_years",react="silent")
    if(is.null(timestep_length)) timestep_length <- timePeriods(gdx)

    #prod <- prod * timestep_length
    
    ### Conversion to (tC)
    prod <- prod * 0.5
    
    wood <- prod[,,"wood"] ## tC in wood
    woodfuel <- prod[,,"woodfuel"] ## tc in woodfuel
    
    ## Inflow of the carbon stock calculation is based on www.pnas.org/cgi/doi/10.1073/pnas.1904231116 equation 3
    ## Inflow of carbon is calculated and not tDM because IPCC describes inflow in Gg C yr-1 from eq. 2.8.5
    ## Only locally produced stuff has to be accounted
    domestically_produced_cHWP = prod * readGDX(gdx,"f21_self_suff")[,getYears(prod),kforestry]
    
    ## We know carbon stored in harvested wood products for now. We just need to redistribute the slow release over time
    # We will redistribute these emissions according to half life of wood products
    
    ## Initialize values
    remaining_stock_cumulative <- 0
    remaining_stock_annual <- NULL
    slowly_released_overall <- 0
    
    ## Passing calculations in two loops.
    ## First loop goes over every individual year cHWP and then the 2nd loop redistributes 
    ## the value of these individual cHWP over the rest of the years. 
    ## cHWP values from 1995 will be redistributed over next 105 years until 2100.
    ## cHWP values from 2050 will only be redistributed over next 50 years until 2100.
    
    for(i in getYears(domestically_produced_cHWP)){
      
      ## This identifies which step we are in
      year_identifier <- grep(pattern = i,x = getYears(domestically_produced_cHWP))
      
      ## If we are in 1995, we redistribute emissions over all the years
      if(i=="y1995"){
        emission_yrs <- getYears(domestically_produced_cHWP)
        annual_yrs <- getYears(every_year)
      } else {
        ## If we are not in 1995, then we skip the years already past and redistribute 
        ## emissions only from that particular year onwards until 2100.
        emission_yrs <- getYears(domestically_produced_cHWP)[-(2:year_identifier-1)]

        ## We also create empty magpie object for years already past.
        ## This is to keep magpie object's not throwing errors whe we exit second loop.
        ## More explanation when 2nd loop ends.
        first_yr <- as.numeric(gsub(x = head(emission_yrs,1),pattern = "y",replacement = ""))
        last_yr <- as.numeric(gsub(x = tail(emission_yrs,1),pattern = "y",replacement = ""))
        annual_yrs <- paste0("y",first_yr:last_yr) 
        empty_magpie <- every_year[,annual_yrs,,invert=TRUE]
        empty_magpie[empty_magpie!=0] <- 0
      }
      
      ## For every year of wood products produced, we need to spread it up over time.
      ## Initialize the value of cHWP from the year of 1st loop
      init <- setYears(domestically_produced_cHWP[,i,],NULL)
        
      ## initialize values
      remaining_stock <- NULL
      slowly_released <- NULL
      ## Start 2nd loop, go over remaining years until 2100
      for (y in annual_yrs) {
        ### IPCC method
        # ts_length = as.numeric(timestep_length[,y,])
        # if(ts_length != 5){
        #   k <- round(log(2)/(half_life/ts_length),4)
        # } else { 
        #   ts_length_factor <- ts_length/5
        #   k <- round(log(2)/(ts_length_factor*half_life/ts_length),4) 
        #   }
        k <- round(log(2)/half_life,4)
        ## k= decay constant of FOD for each HWP category (HWPj) 
        ## given in units yr-1(k= ln(2)/HL, where HL is half-life of the HWP pool in years (see Section 2.8.3.2).
        ## Assumed 35 years here
        ## As half life is annual, need to know what is the time step length because half lives are in yrs.
        ## Between 2060 and 20170, the half life decay has taken place 10 times.
        ## Slightly inconsistent but should be fine.
        #timestep_correction = grep(pattern = y,x = getYears(timestep_length))
      
        temp <- init - init*k
        
        ############################### SLOW RELEASE
        ### Uncomment this to see how halflife will slowly deprecate 100 gT to 50 gT in halflife yrs
        # init <- 100
        # for (i in 1:50) {
        #     temp <- init - init*k
        #     init <- temp
        #     print(paste0(i,"--",init))
        #     }
        
        ## This is the remaining stock from respective decays
        remaining_stock_temp <- temp ## Now we save the value of updated initial stock
        ## This is how much was released due to respective decay
        slowly_released_temp <- init-temp
        getYears(remaining_stock_temp) <- y ## Setting the right years for remaining stock
        getYears(slowly_released_temp) <- y ## Setting the right years for remaining stock
        
        ## Update how the stock will change over time
        remaining_stock <- mbind(remaining_stock, remaining_stock_temp)
        ## Update what will be released gradually
        slowly_released <- mbind(slowly_released, slowly_released_temp)
        
        ## Update the stock because this will be the starting value for next decay
        init <- remaining_stock_temp ## Exchange values to be used in next iteration of loop
      }
      
      ## Here we update how much the overall stock will look like
      ## Because more and more material is added to the product pool every step
      ## But this pool is added on top of remaining stock from the past.
      ## Additionally, Future decays from past add up to the decays from today.
      
      if(i=="y1995"){
        ## Cumulative stock accounts for past and present remaining stock! (therefore using PLUS)
        remaining_stock_cumulative <- remaining_stock_cumulative+remaining_stock
        ## Annual stock just accounts for what additional carbon is stored in THIS particulat year
        remaining_stock_annual <- remaining_stock[,i,]
        
        ## Slow release is tricky because slow release will take place in harvest year or production year
        ## But slow release also comes from timber already harvested earlier depending on half life of timber
        slowly_released_overall <- slowly_released_overall+slowly_released
      } else {
        ## mbind with empty object to keep the missing years 
        remaining_stock <- mbind(empty_magpie, remaining_stock)
        ## Cumulative stock accounts for past and present remaining stock! (therefore using PLUS)
        remaining_stock_cumulative <- remaining_stock_cumulative+remaining_stock
        ## Annual stock just accounts for what additional carbon is stored in THIS particulat year
        remaining_stock_annual <- mbind(remaining_stock_annual,remaining_stock[,i,])
        
        ## Slow release is tricky because slow release will take place in harvest year or production year
        ## But slow release also comes from timber already harvested earlier depending on half life of timber
        slowly_released <- mbind(empty_magpie, slowly_released)
        slowly_released_overall <- slowly_released_overall+slowly_released
      }
    }
    
    ## Some pool of slow release already exists before 1995 so we bumpup all of slow release pool by a value of 1995
    slowly_released_overall <- slowly_released_overall + dimSums(slowly_released_overall[,1:5,],dim=2)/5
    
    #ind_rw_pool <- setNames(remaining_stock_cumulative[,,"wood"],"ind_rw_cumulative")
    #released_overall <- setNames(slowly_released_overall[,,"wood"],"slow_release_pool")1
    #net_timber_pool <- setNames(ind_rw_pool - released_overall,"net_timber_pool")
    
    reporting_yrs <- getYears(prod)
    
    long_term_pool <- add_dimension(x = remaining_stock_annual ,dim = 3.1,nm = "storage",add = "type")[,reporting_yrs,]
    decay_pool     <- add_dimension(x = slowly_released_overall,dim = 3.1,nm = "decay"  ,add = "type")[,reporting_yrs,]
    
    a <- mbind(long_term_pool,decay_pool)
    #a <- mbind(ind_rw_pool,released_overall,net_timber_pool)[,reporting_yrs,]
    #ind_rw_pool_ann <- setNames(remaining_stock_annual[,,"wood"],"ind_rw_annual")
    #a <- mbind(a,ind_rw_pool_ann)
    #Division by time step length
    #a <- a/5
    
    # p <- as.ggplot(dimSums(a[,,]/1000,dim=1))
    # head(p)
    # ggplot(data = p,aes(x = Year,y = Value)) + geom_point(aes(color=Data2)) + facet_grid(.~Data1)
    
    ### Fire time step bugix
    #a[,1,] <- a[,2,]
    
    if (cumulative) {
      years <- getYears(a,as.integer = T)
      im_years <- new.magpie("GLO",years,NULL)
      im_years[,,] <- c(1,diff(years))
      a[,"y1995",] <- 0
      a <- a*im_years[,getYears(a),]
      a <- as.magpie(apply(a,c(1,3),cumsum))
      a <- a - setYears(a[,baseyear,],NULL)
    }
    

    if(unit=="gas") a <- a * 44 / 12

    if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)
    
   } else { 
     a <- NULL
     message("Disabled for magpie run without timber production.")
     }
  
  out(a,file)
}

