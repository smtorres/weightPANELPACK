#' Find population statistics for different waves
#'
#' Find the population statistics for each wave in the desired panel data to be analyzed. Useful for attrition information.
#'
#' @param outcome A character vector of the names of outcome variables of interest
#' @param panelType The panel under analysis: TAPS or ANES - default is TAPS
#' @param dataANES If ANES is selected, merged data from the 2008-2009 panel and re-contact panels 2010
#' @param refusedCode A character vector with the label or labels that identify non-response options - default is ``Refused''
#'
#' @return A list of population stats
#' @docType methods
#' @author David G. Carlson \email{carlson.david@@wustl.edu}  Michelle Torres: \email{smtorres@@wustl.edu}
#' @seealso \code{\link{weightPANEL}}
#' @rdname wavesPANEL
#' @aliases wavesPANEL,ANY-method
#' @export
setGeneric(name="wavesPANEL",
           def=function(outcome, panelType="TAPS",
                        dataANES=NULL, refusedCode = NULL, ...)
           {standardGeneric("wavesTAPS")}
)

setMethod(f="wavesPANEL",
          definition=function(outcome, panelType="TAPS",
                              dataANES=NULL, refusedCode = NULL, ...){
            if(panelType== "TAPS"){
              temp.data <- TAPScum
              dems <- dd
              pop.margins <- pop.margins
              caseID <- "wustlid"
            }
            else if(panelType=="ANES"){
              temp.data <- dataANES
              dems <- ANESdemographics
              pop.margins <- pop.margins.ANES
              caseID <- "caseid"
            }
            else{
              stop("Sorry, the desired panel is not supported by the data.")
            } 
            dataSets<-llply(outcome, function(x)(outcome=x,covars=NULL,data=temp.data, dems=dems,
                                                 weight=FALSE, caseID=caseID,
                                                 refusedasNA=TRUE, refusedCode=refusedCode))
            data.subset<-subsetTAPS(outcome=outcome, covars=NULL,data=temp.data, dems=dems,
                                    weight=FALSE, caseID=caseID,
                                    refusedasNA=TRUE, refusedCode=refusedCode)
            
            if (length(outcome)>1){
              data.waves<-Reduce(function(x, y) merge(x, y, all=TRUE), dataSets)
            }else{
              data.waves<-dataSets
            }
            
            names.waves<-character(0) #Waves names for legend (see loop)
            
            if(panelType=="TAPS"){
              t.agegend<-t.ethm<-t.educat<-t.regmetro<-t.incomcat<- t.ppnet<-list()
              for (i in 1:length(outcome)){
                names.waves[[i]]<-paste("Outcome",i, sep="")
                t.agegend[[i]]<-round((prop.table(table(dataSets[[i]]$agegend)))*100,2)
                t.ethm[[i]]<-round((prop.table(table(dataSets[[i]]$ethm)))*100,2)
                t.educat[[i]]<-round((prop.table(table(dataSets[[i]]$educat)))*100,2)
                t.regmetro[[i]]<-round((prop.table(table(dataSets[[i]]$regmetro)))*100,2)
                t.incomcat[[i]]<-round((prop.table(table(dataSets[[i]]$incomcat)))*100,2)
                t.ppnet[[i]]<-round((prop.table(table(dataSets[[i]]$ppnet)))*100,2) 
              }
              t.total<-list(t.agegend, t.ethm, t.educat, t.regmetro, t.incomcat, t.ppnet)
              statsWave<-llply(t.total, .fun=function(x) do.call(cbind, x))
            }
            else if(panelType=="ANES"){
              t.gend<-t.age<-t.regmentro <- t.ethm<-t.educat<-
              for (i in 1:length(outcome)){
                names.waves[[i]]<-paste("Outcome",i, sep="")
                t.gend[[i]]<-round((prop.table(table(dataSets[[i]]$sex)))*100,2)
                t.age[[i]]<-round((prop.table(table(dataSets[[i]]$age)))*100,2)
                t.regmetro[[i]]<-round((prop.table(table(dataSets[[i]]$region)))*100,2)
                t.ethm[[i]]<-round((prop.table(table(dataSets[[i]]$ethm)))*100,2)
                t.educat[[i]]<-round((prop.table(table(dataSets[[i]]$education)))*100,2)
              }
              t.total<-list(t.gend, t.age, t.regmetro t.ethm, t.educat)
              statsWave<-llply(t.total, .fun=function(x) do.call(cbind, x))
            }
            else {
              stop("Sorry, the desired panel is not supported by the data.")
            }
            
            for (i in 1:length(statsWave)){
              colnames(statsWave[[i]])<-names.waves 
            }
            
            return(statsWave)
          }
)
