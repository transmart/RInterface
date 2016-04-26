\name{getTree}
\alias{getTree}

\title{
Obtain the transmart tree structure in the form of a nested list based on the study's concept paths
}
\description{
This function will retrieve a list containing all concepts of the study in a nested tree structure.
It will also contain attributes required for creating a shinyTree representation of the study.
}
\usage{
getTree(study, add_data = FALSE)
}

\arguments{
  \item{study}{a character string giving the study ID}
  \item{add_data}{logical (default setting is FALSE): should the returned list contain all the clinical data values? (could take a long time for large studies) If TRUE, visualizing data in shinyTree will take less time}
}


\references{}

\author{Tim Dorscheidt, Jan Kanis, Rianne Jansen, Stefan Payralbe. 
Contact: development@thehyve.nl}

\note{To be able to access a transmart database, you need to be connected to the server the database is on. If you haven't connected to the server yet, establish a connection using the \code{\link{connectToTransmart}} function.}

\seealso{\code{\link{showTree}, \link{getStudies}, \link{getConcepts}}.}

\examples{
\dontrun{
  # obtain a list of all studies in the database 
  studies <- getStudies()
  
  # select the name of one study from the list of studies
  study1 <- studies$id[[1]]
  
  # the following call will give you the tree structure of the study
  study1_tree <- getTree(study1)
  }

}

\keyword{ database }
\keyword{ transmart }
