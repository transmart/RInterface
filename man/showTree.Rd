\name{showTree}
\alias{showTree}

\title{
Visualize the tree structure obtained by the getTree function in a shinyTree representation
}
\description{
This function will open up a new window with a shinyTree representation of a tranSMART study. All concepts will be
shown in a tree in which both categorical and numerical clinical variables can be visualized. 
}
\usage{
showTree(study_as_list)
}

\arguments{
  \item{study_as_list}{a study tree object obtained by the getTree function}
}


\references{}

\author{Tim Dorscheidt, Jan Kanis, Rianne Jansen, Stefan Payralbe. 
Contact: development@thehyve.nl}

\note{To be able to access a transmart database, you need to be connected to the server the database is on. If you haven't connected to the server yet, establish a connection using the \code{\link{connectToTransmart}} function.}

\seealso{\code{\link{getTree}}}

\examples{
\dontrun{
  # obtain a list of all studies in the database 
  studies <- getStudies()
  
  # select the name of one study from the list of studies
  study1 <- studies$id[[1]]
  
  # the following call will give you the tree structure of the study
  study1_tree <- getTree(study1)
  
  # visualize the tree structure
  showTree(study1_tree)
  }
}

\keyword{ database }
\keyword{ transmart }
