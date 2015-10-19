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

%? note doc is beginner's guide to exporting
The package is designed to assist in exporting Rcmdr output 
(or other objects printed in Rcmdr) to
formats other than ASCII text, and provide R novices with an easy-to-use, 
easy-to-access reference on exporting R objects to formats
suitable for publication.  At the moment, it is a graphical 
front-end to \code{\link[xtable]{xtable}} and \code{\link[Hmisc]{latex}} 
and can export various R objects to \code{LaTeX} or \code{HTML}.  

In the case of \code{LaTeX}, the printed output can be used in two ways.  
Taking a static approach, one 
can copy the generated \code{LaTeX} code and paste directly into a \code{LaTeX} 
document, or within an ERT inset of a LyX [1] document.  If the code 
is exported to a \code{.tex} file, one may insert it as an external 
file in LyX. 

A more dynamic approach consists in creating \code{\link{Sweave}} 
documents, which combine \code{LaTeX} and \code{R} code.  In this case, 
one may copy the generated \code{R} code and paste into code chunks
in the \code{Sweave} document.  Learning \code{Sweave} is considerably trivial
compared to learning \code{R}, so don't let yourself intimidated!  
Although standard usage of \code{Sweave} requires knowledge of \code{LaTeX}, 
one can readily use LyX to create \code{Sweave} documents [2][3], 
thus avoiding the burden of (properly) learning \code{LaTeX}.  For this 
particular usage, the plug-in can be seen as a reference guide 
for retrieving (or learning) the correct syntax to exporting objects. 

Inserting the exported \code{HTML} code into conventional 
word-processing programmes (MS Word, OpenOffice Writer, AbiWord, etc.) 
can get trickier, but several approaches are possible.  
The \code{HTML} code can, of course, be readily copied and pasted 
into the source of an \code{HTML} document (for example, in 
Mozilla SeaMonkey Composer).  This also works for an \code{HTML} 
document developed in OpenOffice Writer/Web, which subsequently 
can be \emph{exported} to \code{.odt}.  

It is possible export the \code{R} object to an \code{.html} file. 
Then, one could \emph{insert} the \emph{file} in an OpenOffice Writer or MS Word
document, with a resulting table that could be easily formatted.   
When working with OpenOffice, MS Word, AbiWord or Gnumeric, one can also
\emph{open} the \code{.html} document, then copy and paste the table into 
the desired document.  This approach combined with the \code{append} 
argument is particularly useful when exporting many objects. % what about excel?

For more information concerning exporting R output please see 
this page [4] by Frank Harrell. 

%!! dynamic approach (HTML): R2HTML

%!! dynamic approach (word): odfWeave 

Developing the plug-in became possible when John Fox, the author of Rcmdr,  
implemented the \code{popOutput()} function.  The function keeps a stack 
of the last several objects printed in Rcmdr, and allows to retrieve them 
following a "First In, First Out" paradigm.  Thus, the last printed
object would also be the first one retrieved.  This also means that objects 
available for export can be displayed only one at a time; one should 
reinitialise the dialogue for each subsequent object.  Once the dialogue 
is initialised, pressing "Cancel" removes the object from the stack.
The length of the stack 
can be controlled via Rcmdr options. Please note that Rcmdr.Export 
is limited to handling objects printed within Rcmdr; consequently, the
plug-in will not retrieve objects printed in the underlying R console. 

On a final note, please contact me should you spot a bug, want to request 
a feature or know how to work around the issues listed in LIMITATIONS.  
I will also be happy to receive feedback on the present documentation.
}

\author{
  Liviu Andronic \email{landronimirc@gmail.com}
}
\keyword{ package }
\references{
[1]  \url{http://www.lyx.org/} -- A GUI for \code{LaTeX}, similar to 
conventional word-processing programmes \cr
[2]  \url{http://cran.r-project.org/doc/Rnews/Rnews_2008-1.pdf} -- 
\emph{Using Sweave with LyX}, Rnews article by Gregor Gorjanc \cr
[3]  \url{http://gregor.gorjanc.googlepages.com/lyx-sweave} -- 
Files needed to configure \code{Sweave} and LyX \cr
[4]  \url{http://biostat.mc.vanderbilt.edu/SweaveConvert} -- Converting 
Documents Produced by Sweave  \cr
}
\seealso{
\code{\link[Rcmdr]{Rcmdr}}, 
\code{\link[xtable]{xtable}},
\code{\link[Hmisc]{latex}},
\code{\link{Sweave}}
}

\examples{
\dontrun{
   ## start R
   library(RcmdrPlugin.Export)  ## loads Rcmdr and the plug-in
}
}
