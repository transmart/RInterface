# Copyright 2014, 2015, 2016 The Hyve B.V.
# Copyright 2014 Janssen Research & Development, LLC.
#
# This file is part of tranSMART R Client: R package allowing access to
# tranSMART's data via its RESTful API.
#
# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version, along with the following terms:
#
#   1. You may convey a work based on this program in accordance with
#      section 5, provided that you retain the above notices.
#   2. You may convey verbatim copies of this program code as you receive
#      it, in any medium, provided that you retain the above notices.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
# Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.


# Retrieve patient.set ID from tranSMART database, based on the constraints given by the user.
# Patient.set constraints are provided as an expression in the shape of, for example,
# (c1 | c2) & (c3|c4|c5) & c6 &... where c is either a constraint built up as {concept}{operator}{constraint_value} 
# (e.g. "age" < 60) or a reference to a concept (e.g. "age")
getPatientSetID <- function(study.name, patientset.constraints, returnXMLquery = F){
  if(missing(study.name)){stop("Provide study name")}
  if(missing(patientset.constraints)){stop("Provide patientset.constraints")}
  
  # retrieve the expression that defines the constraints
  if(!is.call(patientset.constraints)){patientset.constraints <- substitute(patientset.constraints)}
  patientset.constraints <- .checkPatientSetConstraints(patientset.constraints)
  
  message("Processing input...", "")
  xmlQuery <- .buildXMLquery(patientset.constraints, study.name)
  
  # do POST request, and store result
  message("\nCreating patient set...", "")
  serverResult <- .transmartGetJSON("/patient_sets", requestBody = xmlQuery,  
                                   post.content.type ="text/xml;charset=UTF-8", onlyContent = c(201))
  
  #return patient.set ID
  patientsetID <- serverResult$id
  
  hrConstraints <- .makeHumanReadableQuery(xmlQuery)
  
  result <- list(patientsetID = patientsetID, patientsetSize = serverResult$setSize, 
                 input_patientset.constraints = .expressionToText(patientset.constraints), 
                 finalQueryConstraints = hrConstraints)
  
  message(paste("\nBased on the input, the following constraints were defined and sent to the server:\n", 
              result$finalQueryConstraints, sep = ""), "")
  if(returnXMLquery){result[["xmlQuery"]] <- xmlQuery}
  return(result)
}


.checkPatientSetConstraints <- function(patientsetConstraints){
  #test if it is expression and not a string. If string: try to parse
  if(is.character(patientsetConstraints)){
    if(length(patientsetConstraints) > 1){
      stop("Incorrect input for patient set constraints. Found multiple strings for defining the patient set constraints. 
           The patient set constraints should be supplied in one single expression (or string).")}
    
    try({patientsetConstraintsParsed <- parse(text = patientsetConstraints)[[1]]
      
      if(length(patientsetConstraintsParsed) > 1){
        message(paste("Detecting a string as input for patient set constraints - expected is an expression,",  
                "such as: \"age\" > 65. \nWill attempt to parse the constraints out of the string, converting it",
                "into an expression..."))
        patientsetConstraints <- patientsetConstraintsParsed
      }
    }, silent = T
    )
    }
  return(patientsetConstraints)
}


# parse the constraints, and turn it into a query in XML format
.buildXMLquery <- function(patientset.constraints, study.name){
  # retrieve concept information for the given study, and only keep relevant columns.  
  # this will be used later to match the concepts supplied by the user as part of the constraint definition to concept 
  # paths.
  studyConcepts <- getConcepts(study.name)
  studyConcepts <- studyConcepts[, c("name", "fullName", "type", "api.link.self.href")]
  studyConcepts <- .findEndLeaves(studyConcepts)   
  
  ## parse the expression containing the constraints and translate this into a query definition in XML format
  parsedConstraintsXMLlist <- .parsePatientSetConstraints(patientset.constraints, studyConcepts)
  
  # parsePatientSetConstraints returns a list with XML trees, these trees all either have items as top XMLnodes or 
  # panels. If the top nodes of the trees are items, add these items to a panel node and add this new node to a list.
  if(xmlName(parsedConstraintsXMLlist[[1]]) == "item"){    
    parsedConstraintsXMLlist <- .makePanelList(parsedConstraintsXMLlist)
  }
  
  #add one panel with study.name, ensuring that only patients from the specified study are selected
  parsedConstraintsXMLlist <- .addStudyPanel(parsedConstraintsXMLlist, study.name, studyConcepts[1, "fullName"])
  
  # build XML formatted query
  xmlQuery <- xmlNode("qd:query_definition", namespaceDefinitions = c(qd="http://www.i2b2.org/xsd/cell/crc/psm/1.1/"))
  for(i in 1:length(parsedConstraintsXMLlist)){
    xmlQuery <- append.XMLNode(xmlQuery, parsedConstraintsXMLlist[[i]])
  }
  
  xmlQuery_asString <- saveXML(xmlQuery, prefix = '<?xml version="1.0" encoding="UTF-8"?>\n')
  
  if (getOption("verbose")) { message(xmlQuery_asString) }
  
  return(xmlQuery_asString)
}


# determine for each concept in the concept table whether a concept is an end leaf of the tree, ie. if it is a data node 
# (which can be either a numeric, categorical or highdim node) 
.findEndLeaves <- function(conceptListStudy){
  conceptTypes <- unique(conceptListStudy$type) 
  
  if( any(! conceptTypes %in% c("CATEGORICAL_OPTION", "NUMERIC", "UNKNOWN", "HIGH_DIMENSIONAL"))){
    warning("Unexpected concept type for one or more concepts in the selected study.
            Determination which concepts are end-leaves of the tree might not work correcty in all cases. 
            This only affects the patient selection query if concepts with undetermined type are included in the query. 
            In that case this message is followed by an accompanying error.
            You can help fix it by contacting us. Type ?transmartRClient for contact details.
            \n")
  }
  
  # concepts with type numeric and high_dimensional are end-leaves, 
  # concepts with type categorical_options are not end-leaves
  endLeaf <- ""
  conceptListStudy <- cbind(conceptListStudy, endLeaf, stringsAsFactors = F)
  conceptListStudy$endLeaf[conceptListStudy$type %in% c("NUMERIC", "HIGH_DIMENSIONAL")] <- "YES"
  conceptListStudy$endLeaf[conceptListStudy$type == "CATEGORICAL_OPTION"] <- "NO"
  
  #find categorical data nodes, and set type of categorical end-leave (data node) to "CATEGORICAL_NODE"
  # concepts with 'type' categorical_option are the concept values. Take the concept path of the concept values and 
  # remove the last part to retrieve a list of concept paths for categorical nodes.
  categoricalOptionsPaths <- conceptListStudy$fullName[conceptListStudy$type == "CATEGORICAL_OPTION"]
  categoricalNodes <- sub("\\\\[^\\]*\\\\$", "\\\\",categoricalOptionsPaths) # remove last part of concept path 
                                            # containing the categorical value, to obtain path to categorical node
  categoricalNodes <- unique(categoricalNodes)
  
  conceptListStudy$endLeaf[conceptListStudy$type == "UNKNOWN" & conceptListStudy$fullName %in% categoricalNodes] <- "YES"
  conceptListStudy$type[conceptListStudy$type == "UNKNOWN" & conceptListStudy$fullName %in% categoricalNodes] <- "CATEGORICAL_NODE"
  conceptListStudy$endLeaf[conceptListStudy$type == "UNKNOWN" & !conceptListStudy$fullName %in% categoricalNodes] <- "NO"
  
  return(conceptListStudy)
}  

# parsePatientSetConstraints takes an expression defining the constraints for the patientset and returns  
# either a list of item XMLtrees or list of panel XMLtrees 
.parsePatientSetConstraints <- function(patientsetConstraints, studyConcepts){
  relationalOperators <- c("<", ">", "<=",">=", "==", "!=")
  logicalOperators <- c("&","&&", "|", "||")
  allowedOperators <- c(relationalOperators, logicalOperators)
  
  verbose <- getOption("verbose")
  # construct a message that's used later on, when an error occurs. This message includes a listing of the different  
  # elements (sub units) of the constraint expression, if verbose == T 
  elementsMsg <- ""
  if(verbose){
    subUnits <- ""
    for(i in 1:length(patientsetConstraints)){
      subUnits <- paste(subUnits, paste("\n\tElement ", i,": ", .expressionToText(patientsetConstraints[[i]]), sep = ""))
    }
    elementsMsg <- paste("\nElements of the (sub)constraint after parsing", subUnits,sep = "")
  }
  
  errorMsg <- paste("Incorrect (sub)constraint definition, or failure to parse the (sub)constraint definition correctly.", 
                    "Check the format of the constraint. \nFor more details about how to specify patient set constraints,", 
                    "see the help/manual page of this function. \n(Sub)constraint: ", 
                    .expressionToText(patientsetConstraints), elementsMsg)

  # if length(patientsetConstraints) == 3, then the expression contains three elements, so it is either a low-level 
  # constraint of the form {concept}{constraint_operator}{constraint_value} or it is a concatenation of constraints
  # separated by either an AND or OR operator (of form {some constraint(s)}{ &, &&, | or || }{some constraint(s)} )
  if(length(patientsetConstraints) == 3){
    constraintOperator <- as.character(patientsetConstraints[[1]])
    
    if(!constraintOperator%in% allowedOperators){stop(errorMsg)}
    constraint <- list()
    
    # in case where the (sub)constraint is a concatation of subconstraints, combined by an AND or OR operator 
    # (e.g. "age" > 12 & "sex" = "Female"): element [[1]] contains the AND or OR operator, element [[2]] the
    # subconstraint to the left of the operator, element [[3]] is the subconstraint to the right of the operator 
    # in case that the (sub)constraint is not a concatenation of subconstraints, but holds a single criterium that a 
    # concept has to satisfy to: 
    # [[1]] contains a relational operator, [[2]] the concept, [[3]] the constraint value
    is.singleConstraint <- constraintOperator %in% relationalOperators
    if(is.singleConstraint){
      itemXMLlist <- list(.parseSingleConstraint(patientsetConstraints, studyConcepts))
      return(itemXMLlist)
    }else{
      # it's a concatenation of constraints: call function again on the subconstraints.
      # right now it only supports the format where the & operators are always the highest level operators and the | 
      # operators are only used as lowest level, forcing the format: (c1|c2)&c3&(c4|c5|c6|...)& ...
      
      treeBeforeOperator <- .parsePatientSetConstraints(patientsetConstraints[[2]], studyConcepts)
      treeAfterOperator  <- .parsePatientSetConstraints(patientsetConstraints[[3]], studyConcepts)
      
      #if there is an "OR" operation inbetween two subconstraints, the whole constraint cannot have an & anymore 
      # (this is for forcing the strict format for constraint definition described above)
      if(constraintOperator == "|" ) {
        if(grepl("&", .expressionToText(patientsetConstraints))){
          stop(paste("Wrong format of (sub)constraint definition. Found in (sub)constraint: ", 
                     .expressionToText(patientsetConstraints), 
                     "\nRight now the only format supported for defining patientset constraints is one where the & ",
                     "operator is on the highest level of the constraint definition \nand the | (or) operator on the ", 
                     "lowest (second) level, \nie. the format is 'x1' or (in case of multiple \'&\' operations) ", 
                     "'x1 & x2 & ...', \nwhere x1, x2, etc. can contain one or more subconstraints (called c here) ", 
                     "separated by an | (or) operator, ie. x = c1 or x = c1 | c2 | ...,", 
                     "\n where c is a single constraint such as \'\"age\" < 60\' or a reference to a concept.",
                     "\n Examples of valid constraints: c1, c1|c2, c1&c2&c3, (c1|c2)&c3&(c4|c5|c6)", sep = ""
          ))
        }
        itemXMLlist <- c(treeBeforeOperator,treeAfterOperator)
        return(itemXMLlist)
      }
      
      # treeBeforeOperator/treeAfterOperator can be either a list of items or a list of panels
      # if it contains a list of items: add the items of that list to a panel node
      if(constraintOperator == "&"){
        if(xmlName(treeBeforeOperator[[1]]) == "item"){    
          beforePanels <- .makePanelList(treeBeforeOperator)
        }
        if(xmlName(treeBeforeOperator[[1]]) == "panel"){ 
          beforePanels <- treeBeforeOperator 
        }
        if(xmlName(treeAfterOperator[[1]]) == "item"){    
          afterPanels <- .makePanelList(treeAfterOperator)
        }
        if(xmlName(treeAfterOperator[[1]]) == "panel"){ 
          afterPanels <- treeAfterOperator 
        }
        
        panelList <- c(beforePanels, afterPanels)
        return(panelList)
      }
    }
  }else if(class(patientsetConstraints) == "("){ 
    # expression is surrounded by brackets: take expression between brackets and call function again
    # element [[2]] contains the expression between the brackets, element [[1]] is '('
    xmlTreeList <- .parsePatientSetConstraints(patientsetConstraints[[2]], studyConcepts)
    return(xmlTreeList)
  }else if(length(patientsetConstraints) == 1) { 
    # Then the (sub)constraint should consist of only a specification of a concept. 
    # This will result in selection of all patients that have a value for this concept.
    # Concept specification can be a string containing a pattern to match to the concept name or a concept path or link, 
    # or an object (variable) that contains such a string (only single string).
    
    #retrieve concept path
    conceptPath <- .getConstraintConcept(patientsetConstraints, patientsetConstraints, studyConcepts, 
                                        identical.match = F, testIfEndLeave = F)[["conceptPath"]]
    
    # make itemTree for that concept
    itemXMLlist <- list(xmlNode("item", xmlNode("item_key", .makeItemKey(conceptPath))))
    return(itemXMLlist)
  }else{
    stop(errorMsg)
  }
}

# the deparse function converts expressions to strings. However it cuts the strings of at a certain bytelength, 
# so a long expression could result in a character vector with several portions of the original expression
# this function makes one string out of the vector again
.expressionToText <- function(expression){
  textExpression <- deparse(expression, width.cutoff = 500)
  
  if(length(textExpression)>1){
    textExpressionPasted <- gsub("^[[:blank:]]+", "", textExpression)
    textExpressionPasted <- paste(textExpressionPasted, collapse = "")
    
    #warnings are truncated, so it doesn't necessarily print all in case of long textExpression
    message(paste("While trying to convert an expression to text, the deparse function cut an expression in two.", 
                  "\nSeparate parts:\n", paste("\t", textExpression, collapse = "\n   AND \n"),
                  "\nThese are pasted again together. Result:\n    ", textExpressionPasted)) 
    textExpression <- textExpressionPasted
  }
  return(textExpression)
}


.makeHumanReadableQuery <- function(xmlQuery){
  parsedXML <- ""
  xmlQuery <- strsplit(xmlQuery, "\n")[[1]]
  panelCount <- 0
  itemCount <- 0
  invert <- "0"
  for(i in 1:length(xmlQuery)){
    oneLine <- xmlQuery[[i]]
    
    if(grepl("<panel>",oneLine)){ 
      if(panelCount > 0 ){parsedXML <- paste(parsedXML, " & \n(", sep = "")
      }else{parsedXML <- paste(parsedXML, "(", sep = "")}
      panelCount <- panelCount + 1
      itemCount <- 0
    }
    if(grepl("<invert>", oneLine)){
      invert <- .removeXMLmarkUp(oneLine, "invert")
      invert <- gsub("[[:blank:]]", "",invert)
      if(invert == "1"){ parsedXML <- paste(parsedXML, "!(", sep = "")}
    }
    if(grepl("</panel>",oneLine)){ 
      if(invert == "1"){ parsedXML <- paste(parsedXML, "))", sep = "")
      }else{parsedXML <- paste(parsedXML, ")", sep = "")}
    }
    if(grepl("<item>",oneLine)){ 
      if(itemCount > 0 ){parsedXML <- paste(parsedXML, " | ", sep = "")}
      itemCount <- itemCount + 1
    }
    if(grepl("<item_key>",oneLine)){
      #get concept path
      item_key <- .removeXMLmarkUp(oneLine, "item_key")
      concept_path <- gsub("\\\\\\\\Public Studies", "", item_key)
      concept_path <- gsub("\\\\\\\\Private Studies", "", concept_path)
      parsedXML <- paste(parsedXML, "\"", concept_path, "\"", sep = "") 
    } 
    if(grepl("<value_operator>",oneLine)){
      valueOperator <- .removeXMLmarkUp(oneLine, "value_operator")
      parsedXML <- paste(parsedXML, " ", valueOperator, " ", sep = "") 
    } 
    if(grepl("<value_constraint>",oneLine)){
      valueConstraint <- .removeXMLmarkUp(oneLine, "value_constraint")
      parsedXML <- paste(parsedXML, " ", valueConstraint, " ", sep = "") 
    } 
  }
  return(parsedXML)
}


.removeXMLmarkUp <- function(string, markUpString){
  string <- gsub(paste("^[[:blank:]]*<", markUpString,">", sep = ""), "", string)
  string <- gsub(paste("</", markUpString,">", sep = ""), "", string)
  return(string)
}


#just needs one conceptPath, can be of any of the concepts in the study. It can be any path in column 'fullName'
.addStudyPanel <- function (constraintXMLlist, study.name, conceptPath){
  # retrieve the path for the study concept, by taking only the first part of the supplied concept path up to and 
  # including the study.name.
  # e.g. take "\\Public Studies\\GSE8581\\" from "\\Public Studies\\GSE8581\\Subjects\\Ethnicity\\Afro American\\"
  splitPath <- strsplit(conceptPath, "\\\\")[[1]] 
  nameHit <- grep(study.name, splitPath, ignore.case = T)[1] # take the first, just in case the study.name is repeated 
                                                               # in later part of path
  studyPath <- paste(c(splitPath[1:nameHit], ""), collapse = "\\", sep = "")
  itemKey <- .makeItemKey(studyPath)
  
  panel <- xmlNode("panel", 
                   xmlNode("invert", 0), 
                   xmlNode("item",
                           xmlNode("item_key", itemKey)))
  constraintXMLlist <- c(constraintXMLlist, list(panel))
  return(constraintXMLlist)
}


# it expects a list of "item" XML trees. It will add all items to a panel XML node, 
# and returns that node as part of a list
.makePanelList <- function(itemXMLtreeList){
  panel <- xmlNode("panel", xmlNode("invert", 0))
  for(i in 1:length(itemXMLtreeList)){
    panel<- append.XMLNode(panel, itemXMLtreeList[[i]])
  }
  panel <- list(panel)
  return(panel)
}

# constraint is of format: {concept definition}{relational operator}{constraint_value}, e.g. "age" < 12.
.parseSingleConstraint <- function(patientsetConstraints, studyConcepts){
  constraint <- list()
  
  # grab the different elements of the constraint definition
  constraint$operator <- as.character(patientsetConstraints[[1]])
  constraint$concept <- patientsetConstraints[[2]]
  constraint$value   <- patientsetConstraints[[3]]
  
  if(class(constraint$value) == "name"){
    tmpValue <- try(eval(constraint$value, envir = globalenv()), silent = T)
    if(class(tmpValue) == "try-error"){
      try_error <- attr(tmpValue, "condition")$message
      err_message <- paste(try_error, ". Object was specified in (sub)constraint ", 
                           .expressionToText(patientsetConstraints) , ".\n", sep = "")
      stop(err_message)
    }
    if(length(tmpValue) >1){
      tryCatch(stop(paste("Incorrect input for constraint_value in (sub)constraint: ", 
                          .expressionToText(patientsetConstraints), ".\nObject length of \'", constraint$value ,
                          "\' is larger than 1.", 
                          "Only a single input value (string/number) is allowed as a constraint_value.", 
                          "\nInput for constraint_value: ", sep = "") , call. = F), finally = print(tmpValue))
    }
    constraint$value <- tmpValue
  }
  
  # a concept can be defined by a pattern matching the concept name (1), by concept.link(2), concept.path(3) or 
  # by giving a variable/object containing a string with one of those three  
  # find the concept path that corresponds to the concept, and determine the type of 
  # node (numerical, categorical or high dim)
  constraint <- c(constraint, .getConstraintConcept(constraint$concept, patientsetConstraints, studyConcepts, 
                                                   identical.match = F))
  constraint$value_operator <- NA
  constraint$value_type <- NA
  
  if(constraint$conceptType == "NUMERIC"){
    #check if the supplied constraint value is numeric
    if(!is.numeric(constraint$value)){
      stop(paste("The supplied constraint value ", deparse(constraint$value)," is not numerical, while concept ",
                constraint$conceptPath, " is a numerical concept. (This was the concept selected based on the input: \'", 
                constraint$concept, "\'). \nEncountered in (sub)constraint: ",.expressionToText(patientsetConstraints),  
                sep = "" )) 
    }
    
    # Each individual constraint is represented as an "item"  in the XML tree that holds the query definition for the 
    # patient.set
    # construct the "item" subtree for the current constraint
    constraint$item_key <- .makeItemKey(constraint$conceptPath)
    constraint$value_type <- "NUMBER"
        #translate relational operator from R to a value operator that can be recognized in the query
    constraint$value_operator <- .getValueOperator(constraint$operator, "NUMERIC") 
    constrain_by_value_tree <- xmlNode("constrain_by_value",
                                       xmlNode("value_operator", constraint$value_operator),
                                       xmlNode("value_constraint", constraint$value),
                                       xmlNode("value_type", constraint$value_type))
    itemXMLtree <- xmlNode("item",
                           xmlNode("item_key", constraint$item_key),
                           constrain_by_value_tree)
  }
  
  if(constraint$conceptType == "CATEGORICAL_NODE" ){
    
    #check if supplied constraint value is  character
    if(!is.character(constraint$value)){
      warning(paste("The supplied constraint value ", constraint$value," is not of class \'character\', while concept ",
                 constraint$conceptPath, " is a categorical concept (ie. containing text).", 
                 "\n(This was the concept selected based on the input: \'", 
                 constraint$concept, "\')", "\nWill convert the value to character, but unless there is actually a ",
                 "categorical value that matches the constraint value, this will result in an error later on.",
                 "\nEncountered in (sub)constraint: ",.expressionToText(patientsetConstraints),
                 sep = "" )) 
      constraint$value <- as.character(constraint$value)
    }
    
    #check if the given constraint value exists for the specified categorical concept
    constraintValuePath <- .getConstraintConcept(constraint$value, patientsetConstraints, studyConcepts, 
                                                identical.match  = T, testIfEndLeave = F)[["conceptPath"]]
    if(constraintValuePath != paste(constraint$conceptPath, constraint$value, "\\", sep = "")){
      stop(paste("Incorrect (sub)constraint definition for (sub)constraint:\'", .expressionToText(patientsetConstraints), 
                 "\'.", "\nThe constraint value \'", constraint$value,"\' does not seem to be an existing value ",
                 "of the categorical concept \'", constraint$concept, "\'.",
                 "\nConcept path: ", constraint$conceptPath,"\nPath to contstraint value: ", 
                 constraintValuePath, sep= ""))
    }
    
    #translate relational operator from R to a value operator that can be used in the query
      #only EQ and NE are possible for text variables. Only EQ is supported right now
    constraint$value_operator <- .getValueOperator(constraint$operator, "CATEGORICAL_NODE") 
    
    # construct the "item" subtree for the current constraint
    if(constraint$value_operator == "EQ"){
      itemXMLtree <- xmlNode("item", xmlNode("item_key", .makeItemKey(constraintValuePath)))
    }
    if(constraint$value_operator == "NE"){
      stop("For now the '!=' operation is not supported for categorical values")
      ##implement later? So that if you specify conceptX != A then it automatically selects  all  possible categorical
      # values in conceptX, except A. (you can't just use invert=1, for example trial_group != control | x < 1)
      # or  trial_group != control | lung_abnormal == "YES" should work too)
    }
  }
  
  if(constraint$conceptType == "HIGH_DIMENSIONAL"){
    # you cannot apply relational operations to the high dimensional node
    stop(paste("Incorrect use of a high dimensional data node in (sub)constraint: ",
               .expressionToText(patientsetConstraints),".",
               "\nYou can only use high dimensional nodes for defining patient sets by supplying the node name ", 
               "alone (e.g. \"mRNA day1\"); you cannot apply a relational operation (such as \"mRNA day1 < 0\")", 
               "to the node. \nIf you supply the high dimensional node name, ",
               "all patients that have data for that high dimensional node will be selected.", sep = ""))
  }
  
  if(is.na(constraint$value_operator)){ 
    stop(paste("Could not determine which value_operator to use in the query definition for the constraint \'", 
               .expressionToText(patientsetConstraints),  "\'. Operator supplied by user: ", constraint$operator, 
               sep = "" ))
  }
  
  return(itemXMLtree)
}


#construct item_key from concept path
# expected format item key:\\Dimension\concept_path. Examples:
#       <item_key>\\Public Studies\Public Studies\Cell-line\Demographics\Age\</item_key>
#       <item_key>\\Private Studies\Private Studies\Cell-line\Characteristics\Age\</item_key>
.makeItemKey <- function(conceptPath){
  dimension <- strsplit(conceptPath, "\\\\")[[1]][2] #get first part of the concept path, that is either public or private study
  
  if(!dimension %in% c("Public Studies", "Private Studies")){
    stop("Could not determine the dimension for the item_key, that is used for the XML query")}
  item_key <- paste("\\\\", dimension, conceptPath, sep = "")
  return(item_key)
}


#translate relational operators to a text representation as is expected for the query
.getValueOperator <- function(operator, type){
  if(type == "NUMERIC"){
    if(operator == "<"){return("LT")}
    if(operator == "<="){return("LE")}
    if(operator == ">"){return("GT")}
    if(operator == ">="){return("GE")}
    if(operator == "=="){return("EQ")}
    if(operator == "!="){return("NE")}
  }
  
  if(type == "CATEGORICAL_NODE"){
    if(operator %in% c("<", "<=", ">", ">=")){
      stop(paste("The operation \'", operator, "\' is not supported for text variables.", sep = ""))}
    if(operator == "=="){return("EQ")}
    if(operator == "!="){return("NE")}
  }
  
  #if the function did not return yet, something went wrong.
  stop(paste("Something went wrong with determining the value_operator to use for the query definition. Operator:", 
             operator,". Value type: ", type, sep = ""))
}


# find the concept path for a given concept definition. Concept can be specified as pattern matching a
# concept name, or as a partial/full concept path or link
.getConstraintConcept <- function(concept, subconstraint, studyConcepts, identical.match = F, testIfEndLeave = T){
  info <- "Correct way to supply a concept (as part of a (sub)constraint) is:
  either directly as a string, containing the concept name or path,
  or indirectly as an object (variable) that contains a string with the concept name or path. 
  Supplying a concept link as found in the column \'api.link.observations.href\' of the data.frame retrieved by 
  getConcepts() should also work.
  Example: if you want to select patients younger than 12, supply \"age\" directly as as string: \"age\" < 12  
  or indirectly: concepts[2] < 12, where concepts[2] contains the string \"age\"."
  
  subconstraint <- .expressionToText(subconstraint)
  
  #if not string: get the value of the variable/object. Value should be one string.
  if(class(concept) == "name"){
    result <- try(eval(concept, envir = globalenv()), silent = T)
    if(class(result) == "try-error"){
      try_error <- attr(result, "condition")$message
      err_message <- paste(try_error,  ". Object was specified in subconstraint ", subconstraint, ".\n", info, sep = "")
      stop(err_message)
    }
    if(length(result) >1){
      tryCatch(stop(paste("Incorrect input for concept specification in subconstraint: ", subconstraint,
                 ".\nObject length of \'", concept ,
                 "\' is larger than 1. Only a single string is allowed for specifying the concept.", 
                 "\nInput for concept: ", sep = "") , call. = F), finally = print(result))
    }
    concept <- result
  }
  #concept should be a string.
  if(!is.character(concept)){
    stop(paste("Incorrect input for concept specification in subconstraint: ", subconstraint, ".\n", info, sep = ""))
  }
  
  orig_concept <- concept 
  
  if(identical.match) { 
    concept <- paste("^", concept, "$", sep = "")
    concept <- gsub("^^", "^", concept, fixed = T)
    concept <- gsub("$$", "$", concept, fixed = T)
  }
  
  is.concept.path <- grepl("\\\\", concept) 
  conceptMatch <- character(0)
  if(!is.concept.path){
    #concept paths are in 'fullName' column of getConcepts result
    conceptMatch <- grep(concept, studyConcepts$name, ignore.case = !identical.match)
    
    if(length(conceptMatch) > 1){
      conceptMatch <- .selectMatch(concept = concept, matching_indices = conceptMatch, concept_list = studyConcepts$name)
    }
  }
  
  if(length(conceptMatch) == 0){
    # supplied concept migth be concept path or link.
    is.concept.link <- grepl("^/studies/.+/concepts/", concept) | grepl("^studies/.+/concepts/", concept)
    
    if(is.concept.path & is.concept.link){stop(
      paste( "Something went wrong with detecting whether the provided string \'", concept,
             "\' is a concept path or concept link. Please check if the provided string is correct.",
             "\nTo check this, you can look at the resulting data.frame of getConcepts(YOUR_STUDY_NAME).",
             "\nThe concept paths that can be used for this study can be found in the \'fullName\' column,", 
             "and the concept links in the \'api.link.self.href\' column",
             "If the string does have the correct format, you may have encountered a bug.",
             "\nYou can help fix it by contacting us. Type ?transmartRClient for contact details.", sep = ""))
      }
    
    if(is.concept.path){
      conceptMatch <- grep(concept, studyConcepts$fullName, fixed = T)
      if(length(conceptMatch) > 1){
        conceptMatch <- .selectMatch(concept = concept, matching_indices = conceptMatch, 
                                    concept_list = studyConcepts$fullName)
      }
    }
    
    if(is.concept.link){
      message("Detecting a concept.link. Will attempt to find matching concept path.")
      conceptMatch <- grep(concept, studyConcepts$api.link.self.href) 
      if(length(conceptMatch) > 1){
        conceptMatch <- .selectMatch(concept = concept, matching_indices = conceptMatch, 
                                    concept_list = studyConcepts$api.link.self.href)
      }
    }
  }
  
  identicalM <- ""
  if(identical.match){identicalM <- "identical(literal) "} 
  if(length(conceptMatch) == 0){
    stop(paste("No ", identicalM, "match found for concept or categorical value \'", orig_concept, 
               "\', found in subconstraint: ", subconstraint, 
               "\nNote: The supplied concept in the constraint definition can be a full or partial ",
               "match to the concept name (and can even contain regular expressions: pattern matching will be done as", 
               " done for the grep function, ignoring case), or it can be a concept.link or a concept.path.", 
               "\nIn case of a categorical concept; the value part of the constraint has to be a literal match to one",
               " of the possible categorical values for that concept." , sep = "")) 
  }

  #test if matches are endLeaves, ie. a data node.
  # If constraints are supplied in the form of {concept}{operator}{constraint_value}, the concept should be an end leave 
  # (ie. data node), either categorical or numerical, and if it's categorical it should be an end leave and not a 
  # categorical value. If only a concept is supplied as a constraint, it is possible to also use other concepts that 
  # are not end leaves - in that case testIfEndLeave should be FALSE.
  is.endLeaf <- studyConcepts$endLeaf[conceptMatch] == "YES"
  
  if(!is.endLeaf & testIfEndLeave){
    stop(paste("The supplied concept \'", concept, "\' is not a data node (ie. not an end leaf of the transmart tree).",
               "The supplied concept name/path/link must point to a single numerical, categorical or high dimensional",
               " data node (end leaf).", sep =  ""))
  }
    
  matched_concept = list(conceptPath = studyConcepts$fullName[conceptMatch], 
                         conceptType = studyConcepts$type[conceptMatch])
  message(paste("Matched the concept \'", orig_concept, "\' in subconstraint \'", subconstraint,
              "\'\n  to concept (full path): \'", matched_concept$conceptPath, "\'\n", sep = "") )
  return(matched_concept)
}


#called by .getConstraintConcept if there were initially multiple matches found for the concept, using the 'grep' function
.selectMatch <- function(concept, matching_indices, concept_list){
  #any literal, full length matches? (ignoring case)
  literalMatches <- tolower(concept_list[matching_indices]) == tolower(concept)
  if(any(literalMatches)){
    matching_indices <- matching_indices[literalMatches]
    if(length(matching_indices) > 1){
      stop(paste("There seem to be more than one concepts with the name \'", concept, "\'.",
                 "\nPlease use the concept path instead of the concept name to specify the concept.",
                 "(Hint: Concept paths can be found in the \'fullName\' column of the getConcepts() result).", sep = ""))
    }
    message(paste("Multiple matching concepts found for the string \'", concept,"\'. One identical match was found: \'",
                  concept_list[matching_indices], "\'.\nThis match is selected.", 
                  "\nFor more precise matching use full-length concept names, paths, or links,", 
                  " and/or include beginning/end of string symbols (^/$) - see ?regexp", sep = ""))
  }
  
  #if not literal match take the shortest match
  if(!any(literalMatches)){
    paths_tmp<- concept_list[matching_indices]
    shortest_match<- matching_indices[which.min(nchar(paths_tmp))]
    matching_indices<- shortest_match
    message(paste("Multiple matching concepts found for the string \'", concept,"\', selecting shortest match: \'",
                  paste(concept_list[shortest_match], collapse = ","), "\'.",
                  "\nFor more precise matching use full-length names or paths,", 
                  " and/or include beginning/end of string symbols (^/$) - see ?regexp", sep = ""))
    if(length(matching_indices) > 1){
      stop(paste("There are multiple shortest matches for \'", concept, "\'. Matches: ", 
                 paste(concept_list[shortest_match], collapse = ", "), ".",
                 "\nPlease use a more specific/longer string for specifying the concept name or path,", 
                 "or use the (full) concept path instead of the concept name to specify the concept.",
                 "(Hint: Concept paths can be found in the \'fullName\' column of the getConcepts() result).", sep = ""))
    } 
  }
  return(matching_indices)
}  
