#' @title Converts a server-side numeric vector into a factor with intervals 
#' @description This function assigns a server-side numeric vector into a factor type 
#' with levels as intervals of the numeric values.
#' @details Divides the range of x into intervals and codes the values in x 
#' according to which interval they fall. 
#' The leftmost interval corresponds to level one, 
#' the next leftmost to level two and so on. 
#' The inclusion of the breaks in the lower or upper intervall 
#' can be changed withg \code{right}.
#' 
#' The function implements the base R function \code{cut}
#' 


#' Server functions called: \code{cutDS}
#' dsBaseClient functions used: \code{ds.quantileMean}
#' @param input.var.name a character string which provides 
#' the name of the variable to be converted to a factor. 
#' @param newobj.name a character string that provides the name for the output variable 
#' that is stored on the data servers. Default \code{cut.newobj}. 
#' @param breaks either a numeric vector of  unique cut points (ngroups=FALSE) or 
#' a single number (ngroups=TRUE) (2 or 4) giving the number of intervals 
#' into which x is to be cut. 
#' @param ngroups logical, default false. If true, breaks will be interpreted 
#' as the number of groups to be cut into
#' @param labels labels for the levels of the resulting category. 
#' By default (NULL), labels are constructed using "(a,b]" 
#' (brackets interchanged when right=FALSE) interval notation. 
#' If labels = FALSE, simple integer codes are returned instead of a factor.
#' @param right logical, indicating if the intervals should be closed on 
#' the right and open on the left (default,  \code{breaks}=TRUE) or vice versa.
#' @param dig.lab integer which demermines the numer of digits to be rounded to before categorization.
#' It is also used when labels are not given and then determines the number of 
#' digits used in formatting the break numbers.
#' Default is 0.
#' @param ordered_results logical, should the result be an ordered factor?
#' Default is FALSE
#' @param var.min minimal value of all categories. Default: -99999999
#' @param var.max maximal value of all categories. Default: 99999999
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return \code{ds.cut} returns the levels of the converted 
#' variable and a validity 
#' message with the name of the created object on the client-side and 
#' the vector in the server-side.  
#' 
#' @examples
#' \dontrun{
#'  
#'   ## Version 6
#'   # Connecting to the Opal servers
#'   
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')

#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'
#'   ds.cut(input.var.name = "D$PM_BMI_CONTINUOUS", 
#'               newobj.name = "BMI_cat1", 
#'               breaks = c(0,18.5,25,999), #a vector with all break points for the levels 
#'               labels = NULL, #use interval notation
#'               right = TRUE, #intervals should be closed on the right and opended on the left
#'               datasources = connections)#all the Opal servers are used, in this case 3 
#'                                         #(see above the connection to the servers) 
#'   ds.cut(input.var.name = "D$PM_BMI_CONTINUOUS", 
#'               newobj.name = "BMI_cat2", 
#'               breaks = c(0,18.5,25,999), #a vector with all break points for the levels 
#'               labels = NULL, #use interval notation
#'               right = FALSE, #intervals should be open on the right and closed on the left
#'               datasources = connections)#all the Opal servers are used, in this case 3 
#'                                         #(see above the connection to the servers) 
#'                                         
#'   ds.cut(input.var.name = "D$PM_BMI_CONTINUOUS", 
#'               newobj.name = "BMI_cat3", 
#'               breaks = 2, #cut numeric value into 2 groups at global median 
#'               labels = NULL, #use interval notation
#'               right = TRUE, #intervals should be closed on the right and opended on the left
#'               datasources = connections)#all the Opal servers are used, in this case 3 
#'                                         #(see above the connection to the servers) 
#'               
#'    # Clear the Datashield R sessions and logout  
#'    datashield.logout(connections) 
#'
#'
#' }
#' @author Daniela Zoeller
#' @export
ds.cut <- function(input.var.name = NULL, newobj.name = NULL,
                   breaks = NULL, ngroups = FALSE, labels = NULL,
                   right = TRUE, dig.lab = 0,
                   ordered_result = FALSE,
                   var.min = -99999999, var.max = 99999999, 
                   datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  numstudies <- length(datasources)
  
  # check if user has provided the name of the column that holds the input variable
  if(is.null(input.var.name)){
    stop("Please provide the name of the variable that is to be converted to a factor e.g. 'varname'", call.=FALSE)
  }
  
  # check if user has provided the name of the input variable in a correct character format
  if(!is.character(input.var.name)){
    stop("Please provide the name of the variable that is to be converted to a factor in character format e.g. 'varname'", call.=FALSE)
  }
  
  # if no output variable specified then provide a default name
  if(is.null(newobj.name)){
    newobj.name <- "cut.newobj"
  }
  
  # check if user has provided breaks
  if(is.null(breaks)){
    stop("Please provide the cut of values or the number of groups you want to cut in  e.g. 'c(5,6)' or '2'", call.=FALSE)
  }
  
  # CHECK IF breaks LIES WITHIN THE ALLOWED RANGE USING QUINTILES
  #quint.s <- ds.quantileMean(input.var.name,type="split",datasources)
  #quint.5 <- quint.95 <- rep(0,numstudies)
  #for(j in 1:numstudies){
  #  quint.5[j] <- quint.s[[j]][1]
  #  quint.95[j] <- quint.s[[j]][7]
  #}
  
  #if(any(min(breaks)<quint.5) | any(max(breaks)>quint.95)) {
  #  stop("The breaks need to be within the range of 5% and 95% quintiles for each study")
  #}
  

  
  # if only one value for breaks is given, numeric value will be split into 
  # breaks groups depending on quantiles
  
  if(ngroups){
    quint <- ds.quantileMean(input.var.name,type="combine",datasources)
    #if((length(breaks > 1))|(ngroups)){
    #  stop("ngroups=TRUE: breaks specifies the number of groups to be cut into, you only have the options to choose between 2 or 4. Either put breaks=2 or breaks=4, or but ngroups=FALSE and define the break points as vector.")
    #}
    
    if(breaks==2){
      breaks <- round(quint[4], dig.lab)
    } else{
      breaks <- round(quint[c(3,4,5)], dig.lab)
    }
  }
  
  
  
  #CALL THE SERVER SIDE FUNCTION (AN ASSIGN FUNCTION)
  #TO PERFORM THE CATEGORIZATION
  breaks.transmit <- paste0(breaks, collapse=",")
  labels.transmit <- paste0(labels, collapse = ",")
  calltext1 <- call("cutDS", input.var.name, breaks.transmit, labels.transmit,right, dig.lab, ordered_result,var.min, var.max)
  DSI::datashield.assign(datasources, newobj.name, calltext1)
  
  
  
  
  ##########################################################################################################
  #MODULE 5: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                                   #
  #
  #SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 #
  test.obj.name<-newobj.name                                                                               #
  #
  # CALL SEVERSIDE FUNCTION                                                                                #
  calltext <- call("testObjExistsDS", test.obj.name)													 #
  object.info<-DSI::datashield.aggregate(datasources, calltext)												 #
  #
  # CHECK IN EACH SOURCE WHETHER OBJECT NAME EXISTS														 #
  # AND WHETHER OBJECT PHYSICALLY EXISTS WITH A NON-NULL CLASS											 #
  num.datasources<-length(object.info)																	 #
  #
  #
  obj.name.exists.in.all.sources<-TRUE																	 #
  obj.non.null.in.all.sources<-TRUE																		 #
  #
  for(j in 1:num.datasources){																			 #
    if(!object.info[[j]]$test.obj.exists){																 #
      obj.name.exists.in.all.sources<-FALSE															 #
    }																								 #
    if(is.null(object.info[[j]]$test.obj.class) || object.info[[j]]$test.obj.class=="ABSENT"){														 #
      obj.non.null.in.all.sources<-FALSE																 #
    }																								 #
  }

  all.levels <- ds.levels(newobj.name,datasources)
  all.levels.all.studies <- NULL
  
  for(j in 1:numstudies){
    all.levels.all.studies <- c(all.levels.all.studies,all.levels[[j]])
  }
  
  all.unique.levels <- as.character(unique(all.levels.all.studies))
  #
  #
  
  if(obj.name.exists.in.all.sources && obj.non.null.in.all.sources){										 #
    #
    return.message<-																					 #
      paste0("Data object <", test.obj.name, "> correctly created in all specified data sources")		 	 #
    #
    return(list(all.levels=all.unique.levels,return.message=return.message))						 #
    #
  }else{																								 #
    #
    return.message.1<-																					 #
      paste0("Error: A valid data object <", test.obj.name, "> does NOT exist in ALL specified data sources. 
             You can re-run the function with disclosure.check = TRUE to see errors.")#
    #
    return.message.2<-																					 #
      paste0("It is either ABSENT and/or has no valid content/class, see return.info above")				 #
    #
    return.message<-list(return.message.1,return.message.2)												 #
    #
    return.info<-list(object.info = object.info)																			 #
    #
    return(list(all.levels=all.levels,return.info=return.info,return.message=return.message))	 #
    #
  }																									 #
  #END OF MODULE 5																						 #
  ##########################################################################################################
  
  
}
#ds.cut