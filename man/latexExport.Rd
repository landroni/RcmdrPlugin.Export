\name{latexExport}
\alias{latexExport}
\title{
    Export objects using Hmisc::latex
}
\description{
    Rcmdr interface to export objects to LaTeX code.
}
\usage{
    latexExport()
}
\details{
The graphical interface is
limited to exporting objects supported by \code{\link[Hmisc]{latex}}.  
For some objects, however, the plug-in attempts to work around the 
limitations, either by converting the object to a supported class or 
by considering only the relevant element from its structure.   
Objects of certain classes are simply ignored and are not displayed 
in the dialogue.

If the retrieved object is on the ignored list, initialising the dialogue
will fail "silently", display a warning message and attempt re-initialize.
If the stack is empty, or contains only objects that cannot
be exported, initializing the dialogue will fail "silently" and display
an error message.

Depending on the class of the retrieved object, the plug-in will 
attempt to propose a vector of approapriate length for the
\code{digits} argument.  By default, the vector will set all columns 
to "2", a double-digit precision.  

The \code{size} printing option should be any valid LaTeX size (see [1]).  

The \code{file} option lets you specify an output file.  Only the name 
of the file should be entered; the plug-in will add the extension
automatically depending on the chosen export format.  This behaviour is
intended to prevent the user from making carelesss errors, such as exporting
\code{LaTeX} code to an \code{.html} file.  

Unlike in \code{xtable}, the \code{append} printing option defaults 
to \code{TRUE}.  The conservative approach was chosen since by default 
\code{xtable} overwrites existing files; in case of a name clash, it could 
be easier to recover the files.  The option is ignored when the 
\code{file} input field is empty, and doesn't affect the process of 
outputting to new files.

The plug-in supports previewing the \code{\link[Hmisc]{latex}} code 
using a \code{.dvi} viewer.  However, unlike in 
\code{latex}, by default the code will not be previewed; the user 
must check the relevant option.  Also, the 
\code{LaTeX} code will be printed on screen prior to calling 
the viewer.  To choose the programme used for previewing, for example, 
set \code{options(xdvicmd='evince')} to use Evince
 (see \code{\link{Startup}}).  


}
\value{
    \code{NULL}
}
\author{Liviu Andronic \email{landronimirc@gmail.com}}
\references{
[1]  \url{http://en.wikibooks.org/wiki/LaTeX/Formatting#Font_Styles_and_size}
}
\seealso{\code{\link[Hmisc]{latex}}}
\keyword{ print }
