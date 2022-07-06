#' @title population
#' @description reads population out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx          GDX file
#' @param file         a file name the output should be written to using write.magpie
#' @param level        Level of regional aggregation; "reg" (regional),
#'                     "glo" (global), "regglo" (regional and global)
#'                     or any other aggregation level defined in superAggregate
#' @param age          if TRUE, population is split up by age groups
#' @param sex          if TRUE, population is split up by sex
#' @param bmi_groups   if TRUE, the population will be split up in body-mass-index groups.
#' @param magpie_input Available modes are "auto" (default), TRUE or FALSE.
#'                     This setting is only activated if argument "bmi_groups" is set to TRUE.
#'                     If set to TRUE, BMI distribution is estimated ex-post
#'                     such that it corresponds to the per-capita kcal intake values
#'                     finally driving MAgPIE dynamics.
#'                     In cases where exogenous diet scenarios (e.g. EAT Lancet diets)
#'                     are simulated, these ex-post BMI distribution can diverge from the (calibrated)
#'                     regression outputs from the food demand model.
#'                     If set to FALSE, the BMI distribution as calculated in the
#'                     food demand model is used, which might not be consistent with
#'                     the intake and calorie supply used in a MAgPIE simulation in
#'                     the case of exogenous diet scenarios (e.g. EAT Lancet diets).
#'                     The default setting "auto" detects automatically whether
#'                     an exogenous scenario for per-capita kcal intake is simulated by MAgPIE,
#'                     and uses the respective settings:
#'                     1) ex-post estimate in case of exogenous scenarios and
#'                     2) estimates from the food demand model in case of endogenous scenarios.
#' @param dir          for gridded outputs: magpie output directory which contains
#'                     a mapping file (rds or spam) disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#'
#' @return population as MAgPIE object (million people)
#'
#' @author Florian Humpenoeder, Benjamin Bodirsky, Isabelle Weindl
#'
#' @importFrom magclass colSums getYears dimSums
#' @importFrom gdx readGDX
#'
#' @seealso \code{\link{reportPopulation}}
#' @examples
#' \dontrun{
#' x <- population(gdx)
#' }
#'
population <- function(gdx, file = NULL, level = "reg", age = FALSE, sex = FALSE,
                       bmi_groups = FALSE, magpie_input = "auto",
                       dir = ".", spamfiledirectory = "") {

  dir <- getDirectory(dir, spamfiledirectory)

  if (magpie_input == "auto") {

    exoDiet     <- readGDX(gdx = gdx, "s15_exo_diet")
    magpie_input <- FALSE

    if (!is.null(exoDiet)) {
      if (exoDiet > 0) {
        magpie_input <- TRUE
      }
    }
  }

  pop <- readGDX(gdx, "im_demography", format = "first_found", react = "warning")
  pop <- pop + 0.000001

  # Check: sum of decomposition of population (im_demography) must be equal to population
  pop2 <- readGDX(gdx, "im_pop_iso", format = "first_found", react = "warning")
  # add one person to each country + age group to avoid division by zeros
  if (sum(abs(dimSums(pop, dim = 3) - pop2)) > 10) {
    warning(paste0("datasets for demogragphy and population diverge by: ",
                   round(sum(abs(dimSums(pop, dim = 3) - pop2)) / length(getYears(pop))),
                   " Mio people on average per time step"))
  }


  # subset years
  pop <- pop[, readGDX(gdx, "t"), ]

  underaged <- readGDX(gdx, "underaged15")
  working   <- readGDX(gdx, "working15")
  retired   <- readGDX(gdx, "retired15")
  adults    <- setdiff(readGDX(gdx, "age"), underaged)

  if (age == FALSE) {

    pop <- dimSums(pop, dim = "age")

  } else if (age == "adults") {

    pop <- pop[, , adults]
    pop <- dimSums(pop, dim = "age")

  } else if (age == "underaged") {

    pop <- pop[, , underaged]
    pop <- dimSums(pop, dim = "age")

  } else if (age == "working") {

    pop <- pop[, , working]
    pop <- dimSums(pop, dim = "age")

  } else if (age == "retired") {

    pop <- pop[, , retired]
    pop <- dimSums(pop, dim = "age")

  } else if (age != TRUE) {

    pop <- pop[, , age]
    pop <- dimSums(pop, dim = "age")

  }

  if (sex == FALSE) {

    pop <- dimSums(pop, dim = "sex")

  } else if (sex != TRUE) {

    pop <- pop[, , sex]
    pop <- dimSums(pop, dim = "sex")

  }

  if (bmi_groups == TRUE) {

    bmiShr <- anthropometrics(gdx = gdx, indicator = "bmi_shr", level = "iso",
                               sex = sex, age = age, bmi_groups = TRUE)

    if (magpie_input == TRUE) {
      # this implementation is depreciated, and shall only be used for an intermediate magpie version that was used for the Soergel paper
      p15_intake_detail = readGDX(gdx,"p15_intake_detail",react="silent")
      if (length(p15_intake_detail)>0){
        tmp     <- bmiShr
        fader   <- readGDX(gdx, "i15_exo_foodscen_fader")
        fader   <- gdxAggregate(gdx, fader, to = "iso", absolute = FALSE)
        bmiScen <- tmp * (1 - fader)
        bmiScen[, , "medium"] <- tmp[, , "medium"] * (1 - fader) + fader
        bmiShr  <- bmiScen
      }
    }

    pop <- pop * bmiShr

  } else if (bmi_groups != FALSE) {

    bmiShr <- anthropometrics(gdx = gdx, indicator = "bmi_shr", level = "iso",
                               sex = sex, age = age, bmi_groups = TRUE)
    pop     <- pop * bmiShr
    pop     <- pop[, , bmi_groups]
    pop     <- dimSums(pop, dim = "bmi_group15")
  }

  pop <- gdxAggregate(gdx, pop, to = level, absolute = TRUE,
                      dir = dir, weight = "land", subcategories = "urban")

  out(pop, file)
}
