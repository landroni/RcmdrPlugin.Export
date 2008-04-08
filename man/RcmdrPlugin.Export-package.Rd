\name{RcmdrPlugin.Export-package}
\alias{RcmdrPlugin.Export-package}
%\alias{}
\docType{package}
\title{
Graphically export objects to LaTeX or HTML
}
\description{
The package provides facilities to graphically export Rcmdr output to 
LaTeX or HTML code. 
}
% Please put two spaces after the end of a sentence in sections with 
% running text (\description, \details, ...).
\details{
\tabular{ll}{
Package: \tab RcmdrPlugin.Export\cr
Type: \tab Package\cr
Version: \tab 0.2-0\cr
Date: \tab 2008-04-08\cr
License: \tab GPL-3\cr
}

  The package is intended to (1) facilitate exporting Rcmdr output to 
  formats other than ASCII text and (2) provide R novices with an easy 
  to use, easy to access reference on exporting R objects to formats 
  suited for printed output.
  
  One may conveniently use the generated xtable commands in conjunction 
  with \code{\link{Sweave}}.  Learning and using \code{Sweave} itself is 
  somewhat trivial, but it does require understanding and using \code{LaTeX}, 
  implying the non-negligeable cost of learning \code{LaTeX}.  However, 
  one may use \code{LaTeX} the easy way through \code{LyX}, a "Document 
  Processor" that essentially is a graphical user interface to \code{LaTeX}. 
  
  On both Windows and Linux (currently I am not aware of MacOS), \code{LyX}
  can be configured to process Sweave files. While configuration on 
  Windows might prove cumbersome, on Linux it may well be straight-forward. 
  Several approaches are possible:  \newline{}
  - Gregor Gorjanc's \url{http://gregor.gorjanc.googlepages.com/lyx-sweave} \newline{}
  - Paul E. Johnson's \url{http://wiki.lyx.org/LyX/LyxWithRThroughSweave} and \newline{}
  \url{http://www.nabble.com/Converter-failure-with-Sweave-tt16180204.html} \newline{}
  - \url{http://www.nabble.com/Moving-graphics-from-R-into-LyX---best-format--tt14791163.html}
  
  The first one is a "complete approach", the second will be particularly 
  interesting for Windows users, while the latter is somewhat more 
  straight-forward (resembling a hack) and might well suit Linux users.  
  For more related reading, search for "Sweave" on the LyX Mailing 
  Lists archives. 

% \code{latex()} from Hmisc 
  Currently the plug-in is a graphical front-end to \code{\link[xtable]{xtable}}, 
  although support for \code{\link[Hmisc]{latex}} is planned.  Facilities 
  for exporting to other formats (e.g., Rich Text Format, Open Document 
  Text, etc.) could be integrated provided that someone is willing to 
  contribute the code (say, personal scripts that were never published), 
  or point to a sensible way of automating the process. 
  
  Developing the plug-in became possible after John Fox implemented the 
  \code{popOutput()} function in Rcmdr.  Starting with version 1.3-12, 
  Rcmdr "maintains a stack of the last several output objects.  These may 
  be retrieved via popOutput().  The length of the stack is controlled by 
  the Rcmdr option "lengthOutputStack", which defaults to 10."  
  
  Since objects printed in Rcmdr are retrieved from a stack, the last printed 
  object is the first one retrieved (the "First In, First Out" paradigm).  
  This also means that objects can be exported only one at a time; 
  the dialog must be (re-)initialized for each exported object.  
  It is worth noting that once the dialog is initialized, the plug-in 
  makes a copy of the retrieved object; pressing cancel means that 
  the object is no longer available neither in the stack nor as 
  a temporary copy. 
  
  On a final note, please let me know whether you know how to work around 
  the LIMITATIONS listed in the file with the same name. 
}

\author{
Liviu Andronic \email{landronimirc@gmail.com}
}
\keyword{ package }
\seealso{
\code{\link[Rcmdr]{Rcmdr}}, \code{\link[xtable]{xtable}}, 
\code{\link[Hmisc]{latex}}
}

\examples{
\dontrun{
   ## start R
   library(RcmdrPlugin.Export)  ## loads the package and Rcmdr
}
}
