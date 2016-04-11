\name{getPatientSetID}
\alias{getPatientSetID}

\title{
Define a patient set based on a series of constraints and retrieve the patient.set ID
}
\description{
This function can be used to create a patient.set in tranSMART, based on a set of constraints, and it returns the ID of the created patient.set. This ID can be used in other functions, such as  \code{\link{getObservations}} and  \code{\link{getHighDimData}} to retrieve the data for only the patients that belong to that specific patient.set.\cr
The function returns a list with the ID of the newly created patient.set, the size of the created patient.set, the original user input specifying the constraints and the interpretation of that input, containing the constraints that were sent to tranSMART(called "finalQueryConstraints"). Optionally, the body of the POST request can be returned as well. This body contains the query definition in XML format as it is sent to tranSMART.
}
\usage{
getPatientSetID(patientset.constraints, study.name, returnXMLquery = FALSE)
}

\arguments{
  \item{study.name}{a character string giving the name of a study}
  \item{patientset.constraints}{the definition of the patient.set constraints: an expression containing all criteria (constraints) that the patients in the patient.set have to meet, or an object containing one such an expression as created with function \code{\link{substitute}}. This expression can contain one or more constraints, concatenated by the "AND" (\code{&}) and OR (\code{|}) operators, and has to meet a strict format. \cr
A single constraint can either be: 
\enumerate{
\item a string with a reference to a concept; ie. a pattern/regular expression matching a concept name, or a partial/full concept path or link, as can be found in the \code{"fullName"} and \code{"api.link.self.href"} column of the concept table retrieved by \code{getConcepts("STUDY_NAME"))}. The pattern or string is used to match it to a concept name/path/link from the \code{getConcepts(study.name)} table and return the full concept path for the matching concept.
  If only single node is supplied as a constraint, without constraint operator and constraint value, then any study concept (node) can be used, and even the values of categorical concepts. All patients with a (non-missing) value for that concept will be selected.  If concept paths are used, then strings are taken literally, as in \code{\link{grep}} with \code{fixed = T}, meaning it is case sensitive and regular expressions cannot be used. 
\item  or a constraint of the format {reference to concept}{relational_operator}{constraint_value}, e.g. "age" < 65. All patients that meet the criterium are selected. In this case it is not possible to use just any concept: the concept has to be a numerical or categorical data node (end leaf). Selection on values of a high dimensional node is not supported.
  }
References to concepts, and constraint_values can also be passed as an object (variable) with one element, being a string or a string/number respectively. E.g. if \"code{concepts = c("age", "sex", "ethnicity")} supplying \code{concepts[[1]] < 65}  will also work. 

Multiple (sub)constraints can be combined into a larger constraint by use of the "AND" (\code{&}) and OR (\code{|}) operators. Note: this has to adhere to a strict format: if both "AND" and "OR" operators are used in a single patient.set constraints definition, the "AND" operator always has to be on the highest/outer level and the "OR" operator has to be on the lower/inner level. E.g. if constraints c1, c2, etc..., are concatenated with both "AND" and "OR" operators, then the whole constraint has to be of the format \code{X1 & X2 & X3}, with \code{Xi} being a concatenation of one or more (sub)constraints separated by the "OR" operator (and surrounded by brackets), eg. \code{Xi = (c1 | c2 | c3)} . A complete constraint, consisting of a concatenation of subconstraint, can for example be: \code{(c1 | c2) & (c3 | c4 | c5) & c6 & ...} or \code{c1}, \code{c1 | c2 | c3}, \code{c1 & c2}. \cr
Example: "sex"== "female" & ("age" > 65 | "blood_pressure" > 140) &  ("diagnosis" == "diabetes" | "diagnosis" == "prediabetic")

  See also \code{\link{substitute}} for creating expressions in case you want to store expressions in a variable before calling getPatientSetID. E.g. \code{getPatientSetID("SOME STUDY", "age" < 65)}, will have the same result as:  \code{my_expression <- substitute("age" < 65) ; getPatientSetID("SOME STUDY", my_expression)}.  Alternatively, the constraints can also be given as a string (\code{getPatientSetID("SOME STUDY", "\"age\" < 65")} or \code{age_concept<- "age"; getPatientSetID("SOME STUDY", "age_concept < 65") }), but this might not be fully supported. If the constraints are supplied as a string, things that should be interpreted as text should be quoted, this applies for example to the concept names/paths/links and the categorical values (e.g. "\"age\" > 65"), else this will be interpreted as a variable name and the function will try to find a variable with that name in the global environment and use the value stored in that variable.}
  \item{returnXMLquery}{If TRUE the body of the POST is request is part of the returned list. This contains the query definition in XML format.}

}

\details{
If a constraint is of the form \{reference to concept\}\{relational_operator\}\{constraint_value\}, the following operations are possible:
\itemize{
\item For numerical data nodes the relational_operations "<", ">", "<=",">=", "==" and "!=" can be used. 
\item For categorical data nodes, only the "==" operation is possible.
} 
some examples: \cr
correct format:  \code{( "age" < 65 |  "sex" == "Female") & "test" & ("test" == 1 | "test" == 2 | "test" == 3) } \cr
wrong format:   \code{( "age" < 65 |  "sex" == "female") & ("test" == 1 | ("test2" < 2 & "test3" == 3)) }  (the & after '"test2" < 2' is on a lower/more inner level than the |) \cr
wrong format:    \code{(( "age" < 65 |  "sex" == "female") & ("test" == 1)) | "test" == 4 } (the | before the '"test4" == 4' is on a higher/more outer level than the &)
}

\value{
A list with the ID of the newly created patient.set, the size of the created patient.set, the original user input specifying the constraints and the interpretation of that input, containing the constraints that were sent to tranSMART and optionally the body of the POST request.
\item{patientsetID}{a numerical value containing the ID of the newly created patient.set. This patient.set ID can be used in other functions, such as  \code{\link{getObservations}} and  \code{\link{getHighDimData}} to retrieve the data for only the patients that belong to that specific patient.set}
\item{patientsetSize}{a numerical value specifying the number of patients in the created patient.set}
\item{input_patientset.constraints}{a character string containing the input by the user}
\item{finalQueryConstraints}{a character string representing the interpretation of the user input, containing the the constraints that were sent to tranSMART. This part of the output can be used to check if indeed the right concepts were selected, based on the input. For concepts the full concept path is given and relational operators are represented by text: "<" is represented by "LT", ">" by "GT", "<=" by "LE",">=" by "GE", "==" by "EQ" and "!=" by "NE". \crNote: the query constraints will always include a study concept path as well; this is added to ensure only patients from the supplied study are selected. Note 2: if a constraint was supplied for a categorical node in the form of \{concept\}\{relational_operator\}\{categorical value\}, e.g.  "sex" == "female", only the path to the categorical value is represented (e.g. '\\Public Studies\\SOME STUDY\\Subjects\\Sex\\female').}
\item{xmlQuery}{a character string containing the body of the POST request that is sent to the tranSMART instance. This body contains the query definition in XML format as it is sent to tranSMART. This is only returned if returnXMLquery = T.}
}
\references{
}
\author{
Tim Dorscheidt, Jan Kanis, Rianne Jansen. 
Contact: development@thehyve.nl}
\note{To be able to access a transmart database, you need to be connected to the server the database is on. If you haven't connected to the server yet, establish a connection using the \code{\link{connectToTransmart}} function.}


\seealso{ \code{\link{getHighdimData}} and \code{\link{getObservations}} }
\examples{
\dontrun{
  # obtain a list of all studies in the database 
  studies <- getStudies()
  
  # the following call will give all concepts for GSE8581
  concepts <- getConcepts("GSE8581")
  
  ## create patient.set
  
  getPatientSetID("GSE8581", "age" < 65)
  
    #or:
    
  my_concepts <- c("Age", "Sex", "Lung Disease")
  constraint_value <- 65
  getPatientSetID("GSE8581", my_concepts[[1]] < constraint_value)
  
  #multiple constraints can be combined:
  getPatientSetID("GSE8581", "Age" < 65 & "Sex" == "female" & ("Lung Disease" == "chronic obstructive pulmonary disease" | 
  "Lung Disease" == "control"))
  
  
  # there are multiple ways the patient.set constraints can be supplied. The following will have the same result:
  # 1 as expression
  getPatientSetID("GSE8581", "age" < 65)
  
  # 2 as an object (variable) containing a single expression
  my_expression <- substitute("age"< 65)
  getPatientSetID("GSE8581", my_expression)
  
  #3 as string. 
    # supplying concept name as string:
  getPatientSetID("GSE8581", "\"age\" < 65")
   # or if concept name is stored in an object (variable):
  age_concept<- "age" 
  getPatientSetID("GSE8581", "age_concept < 65")

  
  #retrieve clinical observation data for patient.set
  
  #retrieve high dim data for patient.set
  
  }.


}

\keyword{ database }
\keyword{ transmart }
