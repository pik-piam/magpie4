#' @title getReportDietaryIndicators
#' @description reports dietary indicators on the country level. These are formatted as data.frames describing:
#' 1. population, anthropometrics, and intake
#' 2. caloric intake by food category (without food waste)
#'
#' @export
#'
#' @param gdx filepath of the GDX file
#' @param scenario character string describing the scenario configuration
#' @return list of data.frames for the dietary indicators
#' @author Michael Crawford, Felicitas Beier, Benjamin Bodirsky
#' @examples
#'
#'   \dontrun{
#'     getReportDietaryIndicators(gdx, scenario)
#'   }
#'
#' @importFrom magclass as.data.frame
#' @importFrom magpiesets reportingnames
#' @importFrom dplyr inner_join

getReportDietaryIndicators <- function(gdx, scenario) {


  # Population ------------------------------------------------------------------------------------------------------

  pop <- magpie4::population(gdx, level = "iso", age = TRUE, sex = TRUE, bmi_groups = TRUE)
  pop <- as.data.frame(pop)
  pop <- pop[names(pop) != "Cell"]
  colnames(pop) <- c("region", "year", "sex", "age", "bmi_group", "population")


  # Anthropometrics -------------------------------------------------------------------------------------------------

  # Some anthropometrics are partitionable by BMI_group, while others are not
  .calcAnthroprometrics <- function(.metric, .hasBMI_group) {
    .d <- magpie4::anthropometrics(gdx, level = "iso", age = TRUE, sex = TRUE,
                                   bmi_groups = .hasBMI_group, indicator = .metric)
    .d <- as.data.frame(.d)
    .d <- .d[names(.d) != "Cell"]
    if (.hasBMI_group) {
      colnames(.d) <- c("region", "year", "sex", "age", "bmi_group", .metric)
    } else {
      colnames(.d) <- c("region", "year", "sex", "age", .metric)
    }
    return(.d)
  }

  withBMI_groups <- .calcAnthroprometrics("bodyweight", .hasBMI_group = TRUE)

  withoutBMI_groups <- Reduce(x = Map(f = .calcAnthroprometrics,
                                      list("bodyheight", "PAL"),
                                      .hasBMI_group = FALSE),
                              f = function(a, b) dplyr::inner_join(a, b, by = c("region", "year", "sex", "age")))

  anthropometrics <- dplyr::inner_join(withBMI_groups, withoutBMI_groups, by = c("region", "year", "sex", "age"))


  # Intake ----------------------------------------------------------------------------------------------------------

  intake <- magpie4::Intake(gdx, level = "iso", age = TRUE, sex = TRUE, bmi_groups = TRUE)
  intake <- as.data.frame(intake)
  intake <- intake[names(intake) != "Cell"]
  colnames(intake) <- c("region", "year", "sex", "age", "bmi_group", "intake")


  # Combine population, anthropometrics, and intake and finalize dataset --------------------------------------------

  allAges <- Reduce(x = list(pop, anthropometrics, intake),
                    f = function(a, b) dplyr::inner_join(a, b, by = c("region", "year", "sex", "age", "bmi_group")))

  # Filter to the relevant timespan
  allAges$year <- as.numeric(as.character(allAges$year))
  allAges <- allAges[allAges$year <= 2050, ]

  # Re-code children, remove their empty factor set
  areKids <- c("0--4", "5--9", "10--14")
  kids <- subset(allAges, (allAges$age %in% areKids) & (allAges$bmi_group != "mediumhigh"))
  kids <- droplevels(kids)
  levels(kids$bmi_group) <- c(verylow = "<-2sd", low = "-2sd_-1sd", medium = "-1sd_1sd",
                              high = "1sd_2sd", veryhigh = ">2sd")

  # Re-code adults
  adults <- subset(allAges, !(allAges$age %in% areKids))
  levels(adults$bmi_group) <- c(verylow = "<BMI18p5", low = "BMI18p5_BMI20", medium = "BMI20_BMI25",
                                mediumhigh = "BMI25_BMI30", high = "BMI30_BMI35", veryhigh = ">BMI35")

  # Combine kids and adults
  allAges <- rbind(kids, adults)

  # Round columns
  allAges <- within(allAges,
                    expr = {
                      population <- round(population, 10)
                      bodyweight <- round(bodyweight, 1)
                      bodyheight <- round(bodyheight, 1)
                      PAL        <- round(PAL, 2)
                      intake     <- round(intake, 0)
                    })

  # Recode sex
  levels(allAges$sex) <- c(m = "male", `F` = "female")

  # Add scenario ID as the third column, to comply with read.magpie
  allAges["scenario"] <- scenario
  allAges <- allAges[, c(1, 2, 11, seq(3, 10))]

  # Sort
  allAges <- allAges[order(allAges$scenario, allAges$region, allAges$year,
                           allAges$sex, allAges$age, allAges$bmi_group), ]

  # Remove rownames
  rownames(allAges) <- NULL

  # Rename columns to reflect units
  colnames(allAges) <- c("region", "year", "scenario", "sex", "age", "bmi_group",
                         "population",  # millions capita
                         "bodyweight",  # kg per capita
                         "bodyheight",  # cm per capita
                         "PAL",         # active energy per basal metabolic rate
                         "intake")      # kcal per capita

  # Consumption -----------------------------------------------------------------------------------------------------
  # Per-capita kilocalorie consumption from the food demand model, which includes food waste

  intakeDetailed <- IntakeDetailed(gdx, level = "iso", product_aggr = FALSE)
  intakeDetailed <- as.data.frame(intakeDetailed)
  intakeDetailed <- intakeDetailed[names(intakeDetailed) != "Cell"]
  colnames(intakeDetailed) <- c("region", "year", "product", "kcal")

  # Filter to the relevant timespan
  intakeDetailed$year <- as.numeric(as.character(intakeDetailed$year))
  intakeDetailed <- intakeDetailed[intakeDetailed$year <= 2050, ]

  # Add more descriptive reporting names, tece -> temperate cereals, e.g.
  intakeDetailed$product <- magpiesets::reportingnames(as.character(intakeDetailed$product))

  # Round intakeDetailed
  intakeDetailed$kcal <- round(intakeDetailed$kcal, digits = 0)

  # Add scenario ID as the first column
  intakeDetailed["scenario"] <- scenario
  intakeDetailed <- intakeDetailed[, c(1, 2, 5, 3, 4)]

  # Sort
  intakeDetailed <- intakeDetailed[order(intakeDetailed$scenario,
                                         intakeDetailed$region,
                                         intakeDetailed$year,
                                         intakeDetailed$product), ]

  # Remove rownames
  rownames(intakeDetailed) <- NULL

  # Rename columns to reflect units
  colnames(intakeDetailed) <- c("region", "year", "scenario", "product", "kcal") # kcal per capita

  return(list(dietaryIndicators = allAges, caloricSupply = intakeDetailed))

}
