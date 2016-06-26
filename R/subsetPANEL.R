#' Subset PANEL data
#'
#' Subset panel data by outcome and covariates specified.
#'
#' @param outcome A character vector of the names of outcome variables of interest
#' @param covars A character vector of the names of the covariate variables of interest
#' @param data The panel data to be weighted 
#' @param dems The dataset with demographics in which the base weight is contained (base weight should be named ``basewt'')
#' @param weight A logical argument specifying whether to use ANES base weights or not - default is FALSE
#' @param caseID A character vector indicating the name of the IDs in the dataset
#' @param refusedasNA A logical argument specifying whether to consider the response refused as an NA - default and suggested value is TRUE
#' @param refusedCode A character vector with the label or labels that identify non-response options - default is ``Refused''
#' @param psu Name of the primary sampling units (PSU) to set the survey design (for complex survey with cluster designs) - default is none
#' @param strata  Name of the strata to set the survey design (for stratified sampling designs)
#' 
#' @return A dataframe of subsetted panel data with post-stratification demographics appended
#' @docType methods
#' @author David G. Carlson \email{carlson.david@@wustl.edu}  Michelle Torres: \email{smtorres@@wustl.edu}
#' @seealso \code{\link{weightPANEL}}
#' @rdname subsetPANEL
#' @aliases subsetPANEL,ANY-method
#' @export
setGeneric(name="subsetPANEL",
           def=function(outcome,covars=NULL,data=NULL, dems=NULL,
                        weight=FALSE, caseID=NULL,
                        refusedasNA=TRUE, refusedCode=NULL, psu = NULL, strata = NULL)
           {standardGeneric("subsetPANEL")}
)

setMethod(f="subsetPANEL",
          definition=function(outcome,covars=NULL,data=NULL, dems=NULL,
                              weight=FALSE, caseID=NULL,
                              refusedasNA=TRUE, refusedCode="Refused", psu = NULL, strata = NULL){
            if(panelType== "TAPS"){
              if(is.null(refusedCode)){
                refusedCode = "Refused"
              }
            }
            else if(panelType=="ANES"){
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
            
            covars <- c(covars, psu, strata)
            if(weight) weighter<-as.numeric(dems$basewt) else weighter<-1
            if(is.null(covars)){
              q<-numeric()
              for(i in 1:length(outcome)){
                
                if(!outcome[i]%in%colnames(data)){
                  holder<-readline(paste("Outcome variable",outcome[i],"is not a valid variable name\n and is being dropped, press enter to continue."))
                  q<-c(q,i)
                }
              }
              if(length(q)!=0) outcome<-outcome[-q]
              if(is.na(outcome[1])) stop("No outcome variables, function terminated")
              fdata<-data.frame(ID=as.numeric(data[,caseID]),data[outcome],baseweights=weighter)
            }else{
              q<-numeric()
              for(i in 1:length(outcome)){
                
                if(!outcome[i]%in%colnames(data)){
                  holder<-readline(paste("Outcome variable",outcome[i],"is not a valid variable name\n and is being dropped, press enter to continue"))
                  q<-c(q,i)
                }
              }
              if(length(q)!=0) outcome<-outcome[-q]
              if(is.na(outcome[1])) stop("No outcome variables, function terminated")
              q<-numeric()
              for(i in 1:length(covars)){
                
                if(!covars[i]%in%colnames(data)){
                  holder<-readline(paste("Covariate variable",covars[i],"is not a valid variable name\n and is being dropped, press enter to continue"))
                  q<-c(q,i)
                }
              }
              if(length(q)!=0) covars<-covars[-q]
              if(!is.na(covars[1])){
                  fdata<-data.frame(caseID=as.numeric(data[,caseID]),data[outcome],data[covars],baseweights=weighter)
              }
              else{
                  fdata<-data.frame(caseID=as.numeric(data[,caseID]),data[outcome],baseweights=weighter)
              }
            }
            fdata[is.null(fdata)]<-NA
            if(refusedasNA){
              for (j in 1:length(refusedCode)){
                fdata[fdata==refusedCode[j]]<-NA 
              }
              if(!is.null(covars)){
                for(i in 1:length(covars)){
                  if(is.factor(fdata[,covars[i]])) fdata[,covars[i]]<-factor(fdata[,covars[i]])
                  
                }
              }
              
            }
            for(i in 1:length(outcome)){
              fdata<-as.data.frame(fdata[!is.na(fdata[outcome[i]]),])
            }
            names(fdata)[1] <- caseID
            if(!is.null(psu)){
              names(fdata)[grep(psu, colnames(fdata))] <- "psu"  
            }
            impdata <- merge(fdata, dems, by=caseID)   
            impdata[is.null(impdata)]<-NA
            
            return(impdata)
          }
)
