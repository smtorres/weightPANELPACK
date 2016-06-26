#' Find attrition stats for panel waves
#'
#' Find response attrition statistics for panel waves
#'
#' @param outcome A character vector of the names of outcome variables of interest
#' @param panelType The panel under analysis: TAPS or ANES - default is TAPS
#'
#' @return A list of attrition rates in population margins. The number in each category corresponds to the percentage of people within that category that left/attrited the panel
#' @docType methods
#' @author David G. Carlson \email{carlson.david@@wustl.edu} Michelle Torres: \email{smtorres@@wustl.edu}
#' @seealso \code{\link{weightPANELPACK}} \code{\link{weightPANEL}}
#' @rdname attritPANEL
#' @aliases attritPANEL,ANY-method
#' @export
setGeneric(name="attritPANEL",
           def=function(outcome, panelType="TAPS",
                        dataANES=NULL, refusedCode=NULL)
           {standardGeneric("attritPANEL")}
)

setMethod(f="attritPANEL",
          definition=function(outcome, panelType="TAPS",
                              dataANES=NULL, refusedCode=NULL){
            
            if(panelType== "TAPS"){
              temp.data <- TAPScum
              dems <- dd
              if(is.null(refusedCode)){
                refusedCode = "Refused"
              }
              pop.margins <- pop.margins
              caseID <- "wustlid"
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
            }
            else{
              stop("Sorry, the desired panel is not supported by the data.")
            }    
            baseline<-subsetPANEL(outcome[1],covars=NULL,data=temp.data, dems=dems,
                                  weight=FALSE, caseID=caseID,
                                  refusedasNA=TRUE, refusedCode=refusedCode, psu = NULL, strata = NULL)
            final<-subsetPANEL(outcome,covars=NULL,data=temp.data, dems=dems,
                               weight=FALSE, caseID=caseID,
                               refusedasNA=TRUE, refusedCode=refusedCode, psu = NULL, strata = NULL)
            if(panelType=="TAPS"){
              f.agegend.names<-names(table(baseline$agegend))
              f.agegend.vec<-as.numeric(table(baseline$agegend))
              names(f.agegend.vec)<-f.agegend.names
              f.ethm.names<-names(table(baseline$ethm))
              f.ethm.vec<-as.numeric(table(baseline$ethm))
              names(f.ethm.vec)<-f.ethm.names
              f.educat.names<-names(table(baseline$educat))
              f.educat.vec<-as.numeric(table(baseline$educat))
              names(f.educat.vec)<-f.educat.names
              f.regmetro.names<-names(table(baseline$regmetro))
              f.regmetro.vec<-as.numeric(table(baseline$regmetro))
              names(f.regmetro.vec)<-f.regmetro.names
              f.incomcat.names<-names(table(baseline$incomcat))
              f.incomcat.vec<-as.numeric(table(baseline$incomcat))
              names(f.incomcat.vec)<-f.incomcat.names
              f.ppnet.names<-names(table(baseline$ppnet))
              f.ppnet.vec<-as.numeric(table(baseline$ppnet))
              names(f.ppnet.vec)<-f.ppnet.names
              
              f.agegend.names<-names(table(final$agegend))
              f.agegend.vec.final<-as.numeric(table(final$agegend))
              names(f.agegend.vec.final)<-f.agegend.names
              f.ethm.names<-names(table(final$ethm))
              f.ethm.vec.final<-as.numeric(table(final$ethm))
              names(f.ethm.vec.final)<-f.ethm.names
              f.educat.names<-names(table(final$educat))
              f.educat.vec.final<-as.numeric(table(final$educat))
              names(f.educat.vec.final)<-f.educat.names
              f.regmetro.names<-names(table(final$regmetro))
              f.regmetro.vec.final<-as.numeric(table(final$regmetro))
              names(f.regmetro.vec.final)<-f.regmetro.names
              f.incomcat.names<-names(table(final$incomcat))
              f.incomcat.vec.final<-as.numeric(table(final$incomcat))
              names(f.incomcat.vec.final)<-f.incomcat.names
              f.ppnet.names<-names(table(final$ppnet))
              f.ppnet.vec.final<-as.numeric(table(final$ppnet))
              names(f.ppnet.vec.final)<-f.ppnet.names
              
              t.agegend<-round((f.agegend.vec-f.agegend.vec.final)/f.agegend.vec,3)
              t.ethm<-round((f.ethm.vec-f.ethm.vec.final)/f.ethm.vec,3)
              t.educat<-round((f.educat.vec-f.educat.vec.final)/f.educat.vec,3)
              t.regmetro<-round((f.regmetro.vec-f.regmetro.vec.final)/f.regmetro.vec,3)
              t.incomcat<-round((f.incomcat.vec-f.incomcat.vec.final)/f.incomcat.vec,3)
              t.ppnet<-round((f.ppnet.vec-f.ppnet.vec.final)/f.ppnet.vec,3)
              
              socio.diffs<-list(t.agegend, t.ethm, t.educat, t.regmetro, t.incomcat, t.ppnet)
              names(socio.diffs)<-c("AgeGender", "Ethnicity", "Schooling", "Region", "Income", "InternetUsers")
            }
            else if(panelType=="ANES"){
              # Sex
              f.gend.names<-names(table(baseline$sex))
              f.gend.vec<-as.numeric(table(baseline$sex))
              names(f.gend.vec)<-f.gend.names
              
              # Age
              f.age.names<-names(table(baseline$age))
              f.age.vec<-as.numeric(table(baseline$age))
              names(f.age.vec)<-f.age.names
              
              #Region
              f.regmetro.names<-names(table(baseline$region))
              f.regmetro.vec<-as.numeric(table(baseline$region))
              names(f.regmetro.vec)<-f.regmetro.names
              
              # Race
              f.ethm.names<-names(table(baseline$race))
              f.ethm.vec<-as.numeric(table(baseline$race))
              names(f.ethm.vec)<-f.ethm.names
              
              # Education
              f.educat.names<-names(table(baseline$education))
              f.educat.vec<-as.numeric(table(baseline$education))
              names(f.educat.vec)<-f.educat.names
              
              # Sex (final) 
              f.gend.names<-names(table(final$sex))
              f.gend.vec.final<-as.numeric(table(final$sex))
              names(f.gend.vec.final)<-f.gend.names
              
              # Age (final) 
              f.age.names<-names(table(final$age))
              f.age.vec.final<-as.numeric(table(final$age))
              names(f.age.vec.final)<-f.age.names
              
              # Region
              f.regmetro.names<-names(table(final$region))
              f.regmetro.vec.final<-as.numeric(table(final$region))
              names(f.regmetro.vec.final)<-f.regmetro.names
              
              # Race (final)
              f.ethm.names<-names(table(final$race))
              f.ethm.vec.final<-as.numeric(table(final$race))
              names(f.ethm.vec.final)<-f.ethm.names
              
              # Education
              f.educat.names<-names(table(final$education))
              f.educat.vec.final<-as.numeric(table(final$education))
              names(f.educat.vec.final)<-f.educat.names
              
              
              t.gend<-round((f.gend.vec-f.gend.vec.final)/f.gend.vec,3)
              t.age<-round((f.age.vec-f.age.vec.final)/f.age.vec,3)
              t.regmetro<-round((f.regmetro.vec-f.regmetro.vec.final)/f.regmetro.vec,3)
              t.ethm<-round((f.ethm.vec-f.ethm.vec.final)/f.ethm.vec,3)
              t.educat<-round((f.educat.vec-f.educat.vec.final)/f.educat.vec,3)
              
              socio.diffs<-list(t.gend, t.age, t.regmetro, t.ethm, t.educat)
              names(socio.diffs)<-c("Gender", "Age", "Region", "Ethnicity", "Schooling")
            }
            
            return(socio.diffs)
          }
)
