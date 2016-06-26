#' Subset panel data and find weights
#'
#' Subset panel data by outcome and covariates, model the attrition rates, impute data for attrited individuals, and find weights for analysis
#'
#' @param panelType Character vector with the name of the panel to be used: TAPS or ANES (2008-2010)? - default is TAPS
#' @param dataANES If ANES is selected, merged data from the 2008-2009 panel and re-contact panels 2010
#' @param outcome A character vector of the names of outcome variables of interest. It is highly suggested that the outcome variables be entered starting with the earliest wave.
#' @param covars A character vector of the names of the covariate variables of interest
#' @param weight A logical argument specifying whether to use TAPS base weights or not - default is FALSE
#' @param refusedasNA A logical argument specifying whether to conisder the response 'Refused' as a missing value - default and suggested value is TRUE
#' @param refusedCode A character vector with the label or labels that identify non-response options - default is ``Refused''
#' @param method A character object indicating type of imputation to be used. hotdeck for hotdeck imputation, multi (default) for mulitple imputation, none for no imputation
#' @param na.delete A logical argument specifying whether to eliminate rows with NAs before calculating weights if method chosen is none - default is TRUE. Only set to FALSE if planning to use NA observations.
#' @param m A numeric argument specifying number of imputed data sets to produce if using multiple imputation - default is 5
#' @param pop.base A numeric object specifying which CPS data to use as a baseline. For TAPS: 1 is Dec. 2011, 2 is Dec. 2012, 3 is Dec. 2013. For ANES: 1-7 from 2008 (used for original weighting) to 2014. Default is 1.
#' @param trunc_at A numeric object specifying where to truncate the weights (what should the max weight be?) - default is 5
#' @param stringsAsFactors A logical vector indicating whether non-numeric variables should be factors rather than strings - default is TRUE
#' @param psu Name of the primary sampling units (PSU) to set the survey design (for complex survey with cluster designs) - default is none
#' @param strata  Name of the strata to set the survey design (for stratified sampling designs)
#' 
#' @return An object of class weightTAPSoutput with the following slots:
#' \itemize{
#'  \item \code{df} List of dataframes (or single dataframe) of subsetted TAPS data with imputed data for missing values in covariates, with column for weights
#'  \item \code{attrit} A list of vectors of attrition rates for populations
#'  \item \code{stats} A list of population statistics from wave to wave
#'  }
#' @docType methods
#' @author David Carlson \email{carlson.david@@wustl.edu},  Michelle Torres \email{smtorres@@wustl.edu}
#' @examples
#' 
#' myOutcome <- c("APPRCONGS2","APPRCONGS6")
#' myCovars <- c("POLKNOW3S2","POLKNOW6S2") 
#' test<-weightPANEL(panelType="TAPS", outcome=myOutcome,covars=myCovars,weight=FALSE,refusedasNA=TRUE, refusedCode= "Refused",
#' method="hotdeck",m=5,pop.base=1,trunc_at=5,stringsAsFactors=TRUE)
#' 
#' myOutcomeANES <- "derw9b2"
#' myCovarsANES <- c("rqdm2", "rqdm2a1")
#' refusedCodesANES <- c("-9. Refused","-7. No answer","-6. Not asked, unit non-response",
#'        "-5. Not asked, terminated","-4. Error, see documentation","-3. Restricted access",
#'        "-2. Missing, miscellaneous nonresponse", "-2. Missing, misc nonresponse")
#' testANES<-weightPANEL(panelType="ANES", outcome=myOutcomeANES,covars=myCovarsANES,
#' weight=TRUE,refusedasNA=TRUE, refusedCode= refusedCodesANES,
#' method="hotdeck",m=5,pop.base=1,trunc_at=5,stringsAsFactors=TRUE)
#' 
#' @seealso \code{\link{weightTAPSPACK}} \code{\link{variablesTAPS}} \code{\link{subsetTAPS}} \code{\link{weightTAPSoutput}} \code{\link{simpleWeight}} \code{\link{attritTAPS}} \code{\link{multipleImp}} \code{\link{hotdeckImp}} \code{\link{wavesTAPS}}
#' @rdname weightTAPS
#' @aliases weightTAPS,ANY-method
#' @details
#' This package is meant to subset two panels (either The American Panel Survey (TAPS) or the American National Election Studies (ANES)) data by outcome and by covariate variables of interest through the function \code{\link{weightPANEL}}.
#' The subsetting process accounts for respondents attriting from at least one of the waves under analysis, as well as for outcome non-response.
#' The variables of interest must be entered exactly as named in the dataframe of interest. See \url{http://taps.wustl.edu/data-archive} or the corresponding codebook for ANES. Additionally you can use the \code{\link{variablesPANEL}} function
#' to explore the names of the variables by wave. It is important to revise the particular features of each of the variables of interest.
#' 
#' 
#' It is strongly suggested that the outcome variables be entered starting with the earliest wave for easier interpretation of the attrition rates.
#' Other arguments are listed in the help file of the \code{weightPANEL()} function, and must be considered based on the user's needs.
#' 
#' 
#' The function \code{weightPANEL()} should be assigned to an object in order to conduct the analysis of the panel.
#' \code{weightPANEL()} returns a subset of the complete selected panel dataset that includes only the outcome variable 
#' and covariates specified by the user, a set of standard demographics used for the post-stratification process as specified 
#' in the methodological design of each survey and a new variable with the corresponding weight for each respondent.
#' 
#' 
#' It also retains the respondents that gave an answer to the outcome variable of interest through the waves specified by the user.
#' Respondents that attritted or did not provide an answer (optional) to the outcome variable for any of the waves under analysis are removed from the subset data.
#' Missing values in sociodemographic variables are imputed for the respondents that remain in the sample, in order to compute their proper weight.
#' The missing values observed in the covariates of the remaining respondents are imputed through the method selected by the user.
#' Once the panel data is subset, the function calculates weights based on the demographic group membership of the respondents in the final subset.
#' These weights will be appended to the end of the data frame(s) with the column name \code{new.weights}.
#' 
#' 
#' The output (see \code{\link{weightPANELoutput}}) is of class weightPANELoutput. This class implies the existence of certain slots that save useful information.
#' These slots are \code{df}, \code{attrit} and \code{stats}.
#' 
#' 
#' The slot \code{df} contains the dataframe(s) that represent the final subset data.
#' The final subset data keeps only the outcome and covariates of interest specified as well as a set of demographics and the new dynamic weights.
#' It also accounts for non-response in the outcome variable and attrition across waves through the waves specified.  
#' Respondents with missing values in the outcome variable for any of the waves desired are removed from the final dataframe.
#' 
#' 
#' The missing covariate data can be left as is, by specifying \code{method="none"}.
#' If imputation is desired, the argument \code{method} can be set to 'multi' for multiple imputation, or 'hotdeck' for hotdeck imputation.
#' If multiple imputation is done, the argument \code{m} should be set to the number of imputed dataframes to be created.
#' Depending on the imputation method selected, \code{df} can be a list of \code{m} elements or a list containing a single element. Each element of \code{df} stores a dataframe.
#' If \code{method="multi"} is specified, \code{df} contains a list of \code{m} dataframes.
#' 
#' 
#' To access the dataframes, use \code{getdf(objectname)}. Objectname corresponds to the object where the value of \code{weightTAPS()} was originally stored.
#' If hotdeck or no imputation was used, the final dataset is the first element of the \code{df} list, and can be accessed with \code{getdf(objectname)[[1]]}.
#' 
#' 
#' The slot \code{attrit} is a list of attrition rates from the first wave specified in the outcome argument.
#' Each quantity represents the percentage of people (by demographic group) that attritted the panel under analysis through the waves specified.
#' It compares the initial composition of each demographic group (from the oldest wave specified) to the composition of the same demographic group in the final subset data delivered by \code{weightPANEL()}.
#' Large values, particularly large values relative to other values in the same sociodemographic category, indicate high rates of attrition.
#' It is important to highlight that high rates of attrition may cause problems in data analysis.
#' The slot \code{stats} lists each sociodemographic group's share of the overall population as represented in the final sample for each outcome.
#' 
#' 
#' The information contained in both the \code{attrit} and \code{stats} slots can be graphically illustrated using the \code{plot(objectname)} function.
#' Two different types of plots are displayed after running the plot function: a dot chart and a set of trend plots.
#' The dot chart shows the differences between the sociodemographic composition of the sample in the first wave specified and the final subset dataframe.
#' This information is disaggregated by the following sociodemographic groups: Age and Gender, Ethnicity, Education, Income, Region and Metropolitan status, and Internet use (for TAPS),
#' or Sex, Age, Region, Race and Education (for ANES)
#' The trend plots presented illustrate the changing composition of the sample by demographic group across the waves specified.
#' The lines shown in each plot correspond to the different categories within each of the groups mentioned.
#' The lines show the percentage of the final subset data belonging to each category by wave. 
#' The plots aim to show the variation in the composition of the sociodemographic groups through the waves specified.
#'  @export
setGeneric(name="weightPANEL",
           def=function(panelType = "TAPS",
                        dataANES = NULL,
                        outcome=NULL, covars=NULL,
                        weight=FALSE,refusedasNA=TRUE, refusedCode=NULL,
                        method="multi",na.delete=TRUE,m=5,
                        pop.base=1,trunc_at=5,stringsAsFactors=TRUE,
                        strata=NULL, psu=NULL)
           {standardGeneric("weightPANEL")}
)

setMethod(f="weightPANEL",
          definition=function(panelType = "TAPS",
                              dataANES = NULL,
                              outcome=NULL, covars=NULL,
                              weight=FALSE,refusedasNA=TRUE, refusedCode=NULL,
                              method="multi",na.delete=TRUE,m=5,
                              pop.base=1,trunc_at=5,stringsAsFactors=TRUE,
                              strata=NULL, psu=NULL){
            if(panelType== "TAPS"){
              temp.data <- TAPScum
              dems <- dd
              if(is.null(refusedCode)){
                refusedCode = "Refused"
              }
              pop.margins <- pop.margins
              caseID <- "wustlid"
              names<-c(~agegend,~ethm,~educat,~regmetro,~incomcat,~ppnet)
            }
            else if(panelType=="ANES"){
              temp.data <- dataANES
              dems <- ANESdemographics
              pop.margins <- pop.margins.ANES
              caseID <- "caseid"
              if(is.null(refusedCode)){
                refusedCode <- c("-9. Refused","-7. No answer","-6. Not asked, unit non-response",
                                 "-5. Not asked, terminated", "-4. Error, see documentation",
                                 "-3. Restricted access","-2. Missing, miscellaneous nonresponse",
                                 "-2. Missing, misc nonresponse")
              }
              names<-c(~sex,~age,~region,~race,~education)
            }
            else{
              stop("Sorry, the desired panel is not supported by the data.")
            }
            
            data<-subsetPANEL(outcome,covars=covars,data=temp.data, dems=dems,
                              weight=weight, caseID=caseID,
                              refusedasNA=TRUE, refusedCode=refusedCode, psu = psu, strata = strata)
            
            if(method == 'none' & na.delete) data<-na.omit(data)
            if(!is.null(covars)){
              test<-apply(data[covars],2,function(x){all(is.na(x))})
              continue<-"continue"
              if(any(test)) continue<-readline(paste("Covariates selected ",covars[test]," have all missing values.\n To ignore, type continue, to terminate function type terminate: "))
              if(continue=="terminate") stop("Function terminated")
            }
            
            if(weight){
              data<-data[!is.na(data$baseweights),]
              data$baseweights <- data$baseweights*(nrow(data)/sum(data$baseweights)) 
            }
            
            if(panelType=="ANES" & !is.null(strata)){
              data[is.na(data$strata),"strata"]<-"NA"
              data$strata<-factor(data$strata)
            }
            new.weights<-simpleWeightPANEL(data,pop.margins[pop.base][[1]],names,trunc_at, 
                                           psu=psu, strata=strata)
            data<-cbind(data,new.weights)
            attrit.data<-attritPANEL(outcome, panelType = panelType, dataANES = temp.data,
                                    refusedCode = refusedCode)
            stats.data<-wavesPANEL(outcome, panelType = panelType, dataANES = temp.data,
                                  refusedCode = refusedCode)
            if(method=="none") df.data<-data
            if(method=="hotdeck") df.data<-hotdeckImp(data,outcome,covars)
            if(method=="multi") df.data<-multipleImp(data,outcome,covars,m)
            if(!stringsAsFactors){
              if(class(df.data)=='data.frame'){
                i <- sapply(df.data, is.factor)
                df.data[i] <- lapply(df.data[i], as.character)
              }
              else{
                for(i in 1:length(df.data)){
                  k <- sapply(df.data[[i]], is.factor)
                  df.data[[i]][k] <- lapply(df.data[[i]][k], as.character)
                }
              }
            }
            
            return(new("weightPANELoutput",df=df.data,attrit=attrit.data,stats=stats.data,
                       type=panelType))
          }
)
