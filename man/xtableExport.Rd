\name{xtableExport}
\alias{xtableExport}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Export objects using xtable
}
\description{
Rcmdr menu interface to export objects to LaTeX or HTML code. 
}
\usage{
xtableExport()
}
%- maybe also 'usage' for other objects documented here.
\details{
  The graphcal interface to \code{\link[xtable]{xtable}} is, naturally, 
  limited to exporting objects that \code{xtable} itself can convert.  
  The plug-in, however, attempts to work around and -- in certain cases -- 
  prior to exporting it converts the object to a data type that \code{xtable} 
  can handle.  In other cases, the plugin does not pass to \code{xtable} 
  the object itself, but the relevant structure element(s) (say, 
  \code{.numSummary$table}).  Objects of certain classes are simply 
  ignored and are not displayed in the dialog. 
  
  Objects of the following classes are ignored: \code{logical}, 
  \code{function}, \code{help_files_with_topic}, \code{packageIQR}, 
  \code{trellis}, \code{xtable}, \code{latex}, \code{htest}, 
  \code{stem.leaf}, \code{multinom}. 
  
  Objects of the following classes receive "special treatment": 
  \code{numSummary}, \code{summary.multinom}, \code{polr}, 
  \code{summary.polr}, \code{reliability}, \code{confint.glht}, 
  \code{factanal}, \code{outlier.test}, \code{array}, 
  \code{integer}, \code{character}, \code{numeric}, \code{rcorr}, 
  \code{by}.
  
  All other objects are passed directly to \code{xtable}. 
  
  If the object retrieved is on the ignored list, initializing the dialog 
  will fail "silently", display a warning message and attempt re-initialize.  
  If the stack is empty, or contains only objects that cannot 
  be exported, initializing the dialog will fail "silently" and display 
  an error message. 

  The \code{digits} optional argument defaults to "2". In the graphical 
  dialog, the user must specify a digit (say, "3") or a list of length 
  equal to the number of columns of the resulting table (say, "0,0,3,3,3" 
  for a 5 column table, as opposed to the more R-intuitive "c(0,0,3,3,3)" 
  list). 
  
  The \code{size} printing option (to the best of my knowledge) should be
  one of the following LaTeX sizes: tiny, small, big, huge. For normal 
  size leave the field blank. 
  
}
\value{
\code{NULL}
}
\author{Liviu Andronic \email{landronimirc@gmail.com}}
\seealso{\code{\link[xtable]{xtable}}}
\keyword{ print }
