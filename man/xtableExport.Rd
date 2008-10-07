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

  The \code{file} printing option lets you specify an output file. The user
  needs to input the name of the file only; the plug-in will add the extension
  automatically depending on the chosen 'Export format'. The behaviour is
  intended to prevent the user from making carelesss errors, such as exporting
  to \code{LaTeX} code but into a *.html file. This will also prevent from
  outputing on top of files different from \code{*.tex} or \code{*.html}.

  The \code{append} printing option is checked by default. This more conservative
  approach is due to the default behaviour of \code{xtable} to overwrite
  output files; it will be easier to recover files in case of a name clash.
  Even if checked, the option will be ignored if the \code{file} input field
  is empty.

}
\value{
\code{NULL}
}
\author{Liviu Andronic \email{landronimirc@gmail.com}}
\seealso{\code{\link[xtable]{xtable}}}
\keyword{ print }
