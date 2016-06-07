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

# concepts table for GSE8581
gse8581conceptsLocation <- 
  system.file("unittests/resources/gse8581concepts.txt", package="transmartRClient")

gseconcepts <- read.table(gse8581conceptsLocation, header = T, stringsAsFactors = F, sep = "\t")

### unit tests for function .checkPatientSetConstraints ###
# this should convert a string to an expression, if the constraints are
# provided as a string. Also checks that it only contains one string
#both "\"age\"" and "age" as input should result in returning  "age" # suggest to use variable name without the quotes if variable with name age should be used
test.checkPatientSetConstraints.simpleString.1 <- function() {
  result<- transmartRClient:::.checkPatientSetConstraints("\"age\"")
  checkEquals("age", result)
}

test.checkPatientSetConstraints.simpleString.2 <- function() {
  result<- transmartRClient:::.checkPatientSetConstraints("age")
  checkEquals("age", result)
}

test.checkPatientSetConstraints.object <- function() {
  result<- transmartRClient:::.checkPatientSetConstraints("concepts[1]")
  expected <- substitute(concepts[1])
  checkEquals(expected, result)
}

test.checkPatientSetConstraints.constraintDefinitionString <- function() {
  result<- transmartRClient:::.checkPatientSetConstraints("\"age\" < 60")
  expected <- substitute("age" < 60)
  checkEquals(expected, result)
}

#should only work with a single string
test.checkPatientSetConstraints.constraintDefinitionMultipleStrings <- function() {
  input <- c("\"age\" < 60", "Male")
  checkException(transmartRClient:::.checkPatientSetConstraints(input))
}

test.checkPatientSetConstraints.constraintDefinitionExpression <- function() {
  result<- transmartRClient:::.checkPatientSetConstraints(substitute("age" < 60))
  expected <- substitute("age" < 60)
  checkEquals(expected, result)
}


### unit test for function .buildXMLquery(patientset.constraints, studyConcepts) ###
## supplying a single concept only, without constraint operator and constraint value"

# supplying a concept that is data node 
test.buildXMLquery.datanode <- function() {
  result <- transmartRClient:::.buildXMLquery("Age", gseconcepts, "GSE8581")
  xmlQueryText <- saveXML(result, prefix = '<?xml version="1.0" encoding="UTF-8"?>\n') #convert XML tree to string
  expected <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<qd:query_definition xmlns:qd=\"http://www.i2b2.org/xsd/cell/crc/psm/1.1/\">\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\Age (year)\\</item_key>\n  </item>\n </panel>\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\</item_key>\n  </item>\n </panel>\n</qd:query_definition>"
  checkIdentical(expected, xmlQueryText)
}

#concept that is not a data node
test.buildXMLquery.nonDataNode <- function() {
  result <- transmartRClient:::.buildXMLquery("Subjects", gseconcepts, "GSE8581")
  xmlQueryText <- saveXML(result, prefix = '<?xml version="1.0" encoding="UTF-8"?>\n') #convert XML tree to string
  expected <-  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<qd:query_definition xmlns:qd=\"http://www.i2b2.org/xsd/cell/crc/psm/1.1/\">\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\</item_key>\n  </item>\n </panel>\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\</item_key>\n  </item>\n </panel>\n</qd:query_definition>"
  checkIdentical(expected, xmlQueryText)
}

# categorical value (end leaf)
test.buildXMLquery.categoricalValue <- function() {
  result <- transmartRClient:::.buildXMLquery("control", gseconcepts, "GSE8581")
  xmlQueryText <- saveXML(result, prefix = '<?xml version="1.0" encoding="UTF-8"?>\n') #convert XML tree to string
  expected <-  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<qd:query_definition xmlns:qd=\"http://www.i2b2.org/xsd/cell/crc/psm/1.1/\">\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\Lung Disease\\control\\</item_key>\n  </item>\n </panel>\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\</item_key>\n  </item>\n </panel>\n</qd:query_definition>"
  checkIdentical(expected, xmlQueryText)
}

# non-existing data node (a string that does nat match any concepts)
test.buildXMLquery.notExistingConcept <- function() {
 checkException( transmartRClient:::.buildXMLquery("Nonsense", gseconcepts, "GSE8581"))
}

#concept link
test.buildXMLquery.conceptLink<- function() {
  input<-substitute("/studies/gse8581/concepts/Subjects/Age%20%28year%29")
  result <- transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581")
  xmlQueryText <- saveXML(result, prefix = '<?xml version="1.0" encoding="UTF-8"?>\n') #convert XML tree to string
  expected <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<qd:query_definition xmlns:qd=\"http://www.i2b2.org/xsd/cell/crc/psm/1.1/\">\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\Age (year)\\</item_key>\n  </item>\n </panel>\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\</item_key>\n  </item>\n </panel>\n</qd:query_definition>"
  checkIdentical(expected, xmlQueryText)
}

# concept path. 
test.buildXMLquery.conceptPath <- function() {
  input<-substitute("\\Public Studies\\GSE8581\\Subjects\\Age (year)\\")
  result <- transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581")
  xmlQueryText <- saveXML(result, prefix = '<?xml version="1.0" encoding="UTF-8"?>\n') #convert XML tree to string
  expected <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<qd:query_definition xmlns:qd=\"http://www.i2b2.org/xsd/cell/crc/psm/1.1/\">\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\Age (year)\\</item_key>\n  </item>\n </panel>\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\</item_key>\n  </item>\n </panel>\n</qd:query_definition>"
  checkIdentical(expected, xmlQueryText)
}

## constraints consisting of concept with constraint value
# simple constraint, consisting of only one concept plus constraint value
test.buildXMLquery.simpleConstraint<- function() {
  input  <- substitute("age" < 65)
  result <- transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581")
  xmlQueryText <- saveXML(result, prefix = '<?xml version="1.0" encoding="UTF-8"?>\n') #convert XML tree to string
  expected <-   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<qd:query_definition xmlns:qd=\"http://www.i2b2.org/xsd/cell/crc/psm/1.1/\">\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\Age (year)\\</item_key>\n   <constrain_by_value>\n    <value_operator>LT</value_operator>\n    <value_constraint>65</value_constraint>\n    <value_type>NUMBER</value_type>\n   </constrain_by_value>\n  </item>\n </panel>\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\</item_key>\n  </item>\n </panel>\n</qd:query_definition>"
  checkIdentical(expected, xmlQueryText)
}

# non-existing value for categorical concept 
test.buildXMLquery.nonExistingCategoricalValue <- function() {
  input  <- substitute("sex" == "unknown")
  checkException(transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581"))
}

#relational operation shouldn't work with high dim node
test.buildXMLquery.relationalOperationHighDimNode<- function() {
  input  <- substitute("lung" < 65)
  checkException(transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581"))
}

#relational operation shouldn't work with a concept that is not a datanode
test.buildXMLquery.relationalOperationNonDataNode<- function() {
  input  <- substitute("Subjects" < 65)
  checkException(transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581"))
}

#compound constraint |
test.buildXMLquery.compoundConstraintsORed<- function() {
  input  <- substitute("age" < 65 | "sex" == "female")
  result <- transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581")
  xmlQueryText <- saveXML(result, prefix = '<?xml version="1.0" encoding="UTF-8"?>\n') #convert XML tree to string
  expected <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<qd:query_definition xmlns:qd=\"http://www.i2b2.org/xsd/cell/crc/psm/1.1/\">\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\Age (year)\\</item_key>\n   <constrain_by_value>\n    <value_operator>LT</value_operator>\n    <value_constraint>65</value_constraint>\n    <value_type>NUMBER</value_type>\n   </constrain_by_value>\n  </item>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\Sex\\female\\</item_key>\n  </item>\n </panel>\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\</item_key>\n  </item>\n </panel>\n</qd:query_definition>" 
  checkIdentical(expected, xmlQueryText)
}

#compound constraint &
test.buildXMLquery.compoundConstraintsANDed<- function() {
  input  <- substitute("age" < 65 & "sex" == "female")
  result <- transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581")
  xmlQueryText <- saveXML(result, prefix = '<?xml version="1.0" encoding="UTF-8"?>\n') #convert XML tree to string
  expected <-   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<qd:query_definition xmlns:qd=\"http://www.i2b2.org/xsd/cell/crc/psm/1.1/\">\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\Age (year)\\</item_key>\n   <constrain_by_value>\n    <value_operator>LT</value_operator>\n    <value_constraint>65</value_constraint>\n    <value_type>NUMBER</value_type>\n   </constrain_by_value>\n  </item>\n </panel>\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\Sex\\female\\</item_key>\n  </item>\n </panel>\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\</item_key>\n  </item>\n </panel>\n</qd:query_definition>"
  checkIdentical(expected, xmlQueryText)
}

#compound constraint complex
test.buildXMLquery.compoundConstraintsComplex<- function() {
  input  <- substitute("age" < 65 & 
                         ("lung disease" == "control" | "lung disease" == "chronic obstructive pulmonary disease") & 
                         "Biomarker_Data")
  result <- transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581")
  xmlQueryText <- saveXML(result, prefix = '<?xml version="1.0" encoding="UTF-8"?>\n') #convert XML tree to string
  expected <-   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<qd:query_definition xmlns:qd=\"http://www.i2b2.org/xsd/cell/crc/psm/1.1/\">\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\Age (year)\\</item_key>\n   <constrain_by_value>\n    <value_operator>LT</value_operator>\n    <value_constraint>65</value_constraint>\n    <value_type>NUMBER</value_type>\n   </constrain_by_value>\n  </item>\n </panel>\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\Lung Disease\\control\\</item_key>\n  </item>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\Lung Disease\\chronic obstructive pulmonary disease\\</item_key>\n  </item>\n </panel>\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\MRNA\\Biomarker_Data\\</item_key>\n  </item>\n </panel>\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\</item_key>\n  </item>\n </panel>\n</qd:query_definition>"
  checkIdentical(expected, xmlQueryText)
}

#compound constraint complex, wrong format
test.buildXMLquery.compoundConstraintsComplexWrongFormat<- function() {
  input  <- substitute("sex"== "female" | ("age" < 65 & "Biomarker_Data"))
  checkException(transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581"))
}

#compound constraint complex, wrong format (no brackets around ORed part)
test.buildXMLquery.compoundConstraintsComplexWrongFormat2<- function() {
  input  <- substitute("sex"== "female" & "age" < 65 | "Biomarker_Data")
  checkException(transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581"))
}

# concept and constraint value should also be possible to be stored in an object
test.buildXMLquery.simpleConstraintContainingObjects<- function() {
  concepts <- c("age", "sex")
  some_value <- 65
  input  <- substitute(concepts[1] < some_value)
  result <- transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581")
  xmlQueryText <- saveXML(result, prefix = '<?xml version="1.0" encoding="UTF-8"?>\n') #convert XML tree to string
  expected <-   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<qd:query_definition xmlns:qd=\"http://www.i2b2.org/xsd/cell/crc/psm/1.1/\">\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\Age (year)\\</item_key>\n   <constrain_by_value>\n    <value_operator>LT</value_operator>\n    <value_constraint>65</value_constraint>\n    <value_type>NUMBER</value_type>\n   </constrain_by_value>\n  </item>\n </panel>\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\</item_key>\n  </item>\n </panel>\n</qd:query_definition>"
  checkIdentical(expected, xmlQueryText)
}

#constraints with double substitute (in getPatientSetID another substitute is performed on the input, so if input is 
# substitute("age"<65), then input for buildXMLquery is substitute(substitute("age"<65))
test.buildXMLquery.doubleSubstitute<- function() {
  concepts <- c("age", "sex")
  some_value <- 65
  input  <- substitute(substitute(concepts[1] < some_value[1]))
  result <- transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581")
  xmlQueryText <- saveXML(result, prefix = '<?xml version="1.0" encoding="UTF-8"?>\n') #convert XML tree to string
  expected <-   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<qd:query_definition xmlns:qd=\"http://www.i2b2.org/xsd/cell/crc/psm/1.1/\">\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\Age (year)\\</item_key>\n   <constrain_by_value>\n    <value_operator>LT</value_operator>\n    <value_constraint>65</value_constraint>\n    <value_type>NUMBER</value_type>\n   </constrain_by_value>\n  </item>\n </panel>\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\</item_key>\n  </item>\n </panel>\n</qd:query_definition>"
  checkIdentical(expected, xmlQueryText)
}

#constraints as string in object
test.buildXMLquery.stringInObject<- function() {
  concepts <- c("age", "sex")
  assign("concepts", concepts, envir = .GlobalEnv)
  constraint <- "concepts[1] < 65"
  input <- substitute(constraint[1])
  result <- transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581")
  xmlQueryText <- saveXML(result, prefix = '<?xml version="1.0" encoding="UTF-8"?>\n') #convert XML tree to string
  expected <-   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<qd:query_definition xmlns:qd=\"http://www.i2b2.org/xsd/cell/crc/psm/1.1/\">\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\Age (year)\\</item_key>\n   <constrain_by_value>\n    <value_operator>LT</value_operator>\n    <value_constraint>65</value_constraint>\n    <value_type>NUMBER</value_type>\n   </constrain_by_value>\n  </item>\n </panel>\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\</item_key>\n  </item>\n </panel>\n</qd:query_definition>"
  checkIdentical(expected, xmlQueryText)
}

#object name without index (this is handled differently than objects with index as the class differs)
test.buildXMLquery.objectWithoutIndex<- function() {
  constraint <- "age"
  input <- substitute(constraint)
  result <- transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581")
  xmlQueryText <- saveXML(result, prefix = '<?xml version="1.0" encoding="UTF-8"?>\n') #convert XML tree to string
  expected <-    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<qd:query_definition xmlns:qd=\"http://www.i2b2.org/xsd/cell/crc/psm/1.1/\">\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\Age (year)\\</item_key>\n  </item>\n </panel>\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\</item_key>\n  </item>\n </panel>\n</qd:query_definition>"
  checkIdentical(expected, xmlQueryText)
}

#NA instead of constraint
test.buildXMLquery.objectWithNA<- function() {
  constraints <- "age"
  input <- substitute(constraints[2])
  checkException(transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581"))
}

#NA concept
test.buildXMLquery.constraintWithNAConcept<- function() {
  constraints <- "age"
  input <- substitute(constraints[2] < 65)
  checkException(transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581"))
}

test.buildXMLquery.objectWithSubstitute<- function() {
  tmp <- c(substitute("age" <65), substitute("sex"== "female"))
  input <- substitute(tmp[1])
  result <- transmartRClient:::.buildXMLquery(input, gseconcepts, "GSE8581")
  xmlQueryText <- saveXML(result, prefix = '<?xml version="1.0" encoding="UTF-8"?>\n') #convert XML tree to string
  expected <-   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<qd:query_definition xmlns:qd=\"http://www.i2b2.org/xsd/cell/crc/psm/1.1/\">\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\Subjects\\Age (year)\\</item_key>\n   <constrain_by_value>\n    <value_operator>LT</value_operator>\n    <value_constraint>65</value_constraint>\n    <value_type>NUMBER</value_type>\n   </constrain_by_value>\n  </item>\n </panel>\n <panel>\n  <invert>0</invert>\n  <item>\n   <item_key>\\\\Public Studies\\Public Studies\\GSE8581\\</item_key>\n  </item>\n </panel>\n</qd:query_definition>"
  checkIdentical(expected, xmlQueryText)
}




#may also be useful for testing (need to connect to a database that has both the clinical and high dim data of gse8581):
# getPatientSetID("GSE8581", "Age") #works on transmart-dev.
# getPatientSetID("GSE8581", "Subjects") #works on transmart-dev.
# getPatientSetID("GSE8581", "control") #works on transmart-dev.
# getPatientSetID("GSE8581", "Nonsense") #isn't supposed to work
# getPatientSetID("GSE8581", "/studies/gse8581/concepts/Subjects/Age%20%28year%29") #works on transmart-dev
# getPatientSetID("GSE8581", "/studies/gse8581/concepts/Subjects/Age%20%28year%29") #works on transmart-dev
# getPatientSetID("GSE8581", "age" < 65) #works on transmart-dev.
# getPatientSetID("GSE8581", "sex" == "unknown") #isn't supposed to work
# getPatientSetID("GSE8581", "lung" < 65) #isn't supposed to work
# getPatientSetID("GSE8581", "Subjects" < 65) #isn't supposed to work
# getPatientSetID("GSE8581", "age" < 65 | "sex" == "female") #works on transmart-dev.
# getPatientSetID("GSE8581", "age" < 65 & "sex" == "female") #works on transmart-dev.
# getPatientSetID("GSE8581", "age" < 65 & ("lung disease" == "control" | "lung disease" == "chronic obstructive pulmonary disease") & "Biomarker_Data") 
# getPatientSetID("GSE8581","sex"== "female" | ("age" < 65 & "Biomarker_Data")) 
# getPatientSetID("GSE8581","sex"== "female" & "age" < 65 | "Biomarker_Data") 
# concepts <- c("age", "sex")
# getPatientSetID("GSE8581", concepts[1] < some_value) #works on transmart-dev.
# getPatientSetID("GSE8581", substitute(concepts[1] < some_value[1])) #works on transmart-dev.
# constraint <- "concepts[1] < 65"
# getPatientSetID("GSE8581",constraint[1]) #works on transmart-dev.
# constraint <- "age"
# getPatientSetID("GSE8581",constraint) #works on transmart-dev.
# constraints <- "age"
# getPatientSetID("GSE8581",constraints[2]) #shouldn't work
# getPatientSetID("GSE8581",constraints[2] < 65)  #shouldn't work
# tmp <- c(substitute("age" <65), substitute("sex"== "female"))
# getPatientSetID("GSE8581",tmp[1])  #works on transmart-dev
# concepts <- c("age", "sex")
# constraint <- "concepts[1] < 65"
# getPatientSetID("gse8581", "concepts[1] < 65")

