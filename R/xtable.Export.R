# This file is part of the RcmdrPlugin.Export package. 
# The current code is based on code taken from Rcmdr. 
# file created: 02 Feb 2008
# last modified: 24 Mar 2008

xtableExport <- function(){
    justDoIt(paste("tmpObject <- popOutput()"))
    objectClass <- class(tmpObject)
    objectClass <- objectClass[[1]]
    if (is.null(objectClass) == "TRUE" | objectClass == "logical"){
        justDoIt(paste("remove(", "tmpObject", ")", sep=""))
        Message(message=paste("xtable() cannot export objects of class '", objectClass, 
          "'", sep=""), type="note")
        Message(message=paste("the stack is probably empty", sep=""), type="warning")
        return()
        }
    else if (objectClass == "function" | objectClass == "help_files_with_topic" | 
      objectClass == "packageIQR" | objectClass == "trellis" | objectClass == "xtable" | 
      objectClass == "latex" | objectClass == "htest" | objectClass == "stem.leaf" | 
      objectClass == "multinom"){
        justDoIt(paste("remove(", "tmpObject", ")", sep=""))
        Message(message=paste("xtable() cannot export objetcts of class '", objectClass, 
          "'", sep=""), type="note")
        return(xtableExport())
        }
    else {
        initializeDialog(title=gettextRcmdr("Export objects using xtable()"))
        }
    dataFrame <- tkframe(top)
    xBox <- variableListBox(dataFrame, paste(objectClass), title=gettextRcmdr("Object class"))
    radioButtons(window=dataFrame, name="type", buttons=c("latex", "html"), 
      values=c("LaTeX", "HTML"), labels=gettextRcmdr(c("LaTeX", "HTML")), 
      title=gettextRcmdr("Export format"))
    optionsFrame <- tkframe(top)
    captionInput <- tclVar("")
    captionField <- tkentry(optionsFrame, width="15", textvariable=captionInput)
    labelInput <- tclVar("")
    labelField <- tkentry(optionsFrame, width="15", textvariable=labelInput)
    alignInput <- tclVar("")
    alignField <- tkentry(optionsFrame, width="15", textvariable=alignInput)
    digitsInput <- tclVar("2")
    digitsField <- tkentry(optionsFrame, width="15", textvariable=digitsInput)
    displayInput <- tclVar("")
    displayField <- tkentry(optionsFrame, width="15", textvariable=displayInput)
    additionalFrame <- tkframe(top)
    sizeInput <- tclVar("")
    sizeField <- tkentry(additionalFrame, width="15", textvariable=sizeInput)
    naInput <- tclVar("")
    naField <- tkentry(additionalFrame, width="15", textvariable=naInput)
    require("xtable")
    onOK <- function(){
        type <- tclvalue(typeVariable)
        caption <- paste(tclvalue(captionInput))
        label <- paste(tclvalue(labelInput))
        align <- paste(tclvalue(alignInput))
        digits <- paste(tclvalue(digitsInput))
        if (digits == "2"){digits <- paste("")}
        display <- paste(tclvalue(displayInput))
        size <- paste(tclvalue(sizeInput))
        na <- paste(tclvalue(naInput))
        closeDialog()
        if (caption != ""){
            caption <- paste(", caption=", '"', paste(tclvalue(captionInput)), '"', sep="")
            }
        if (label != ""){
            label <- paste(", label=", '"', paste(tclvalue(labelInput)), '"', sep="")
            }
        if (align != ""){
            align <- paste(", align=", '"', paste(tclvalue(alignInput)), '"', sep="")
            }
        if (digits != ""){
            digits <- paste(", digits=", "c(", paste(tclvalue(digitsInput)), ")", sep="")
            }
        if (display != ""){
            display <- paste(", display=", '"', paste(tclvalue(displayInput)), '"', sep="")
            }
        if (size != ""){
            size <- paste(", size=", '"', paste(tclvalue(sizeInput)), '"', sep="")
            }
        if (na != ""){
            na <- paste(", NA.string=", '"', paste(tclvalue(naInput)), '"', sep="")
            }
        commandRepeat <- 1
        objectName <- paste(".", objectClass, sep="")
        if (objectClass == "numSummary"){
            objectCommandName <- paste("as.table(", objectName, "$table)", sep="")
            }
        else if (objectClass == "summary.multinom"){
            objectCommandName1 <- paste("as.data.frame(", objectName, "$coefficients)", sep="")
            objectCommandName2 <- paste("as.data.frame(", objectName, "$standard.errors)", sep="")
            objectCommandName <- c(objectCommandName1, objectCommandName2)
            commandRepeat <- 2
            }
        else if (objectClass == "polr"){
            objectCommandName1 <- paste("as.data.frame(", objectName, "$coefficients)", sep="")
            objectCommandName2 <- paste("as.data.frame(", objectName, "$zeta)", sep="")
            objectCommandName <- c(objectCommandName1, objectCommandName2)
            commandRepeat <- 2
            }
        else if (objectClass == "summary.polr"){
            objectCommandName <- paste(objectName, "$coefficients", sep="")
            }
        else if (objectClass == "reliability"){
            objectCommandName <- paste(objectName, "$rel.matrix", sep="")
            }
        else if (objectClass == "confint.glht"){
            objectCommandName <- paste(objectName, "$", "confint", sep="")
            }
        else if (objectClass == "factanal"){
            objectCommandName1 <- paste("as.data.frame(", objectName, "$uniquenesses)", sep="")
            objectCommandName2 <- paste("as.table(", objectName, "$loadings)", sep="")
            objectCommandName <- c(objectCommandName1, objectCommandName2)
            commandRepeat <- 2
            Message(message=paste("xtable() cannot export all output of objects of class '", 
              objectClass, "'", sep=""), type="note")
            }
        else if (objectClass == "outlier.test"){
            objectCommandName <- paste("as.data.frame(", objectName, "$test)", sep="")
            }
        else if (objectClass == "array" | objectClass == "integer" | 
          objectClass == "character" | objectClass == "numeric"){
            objectCommandName <- paste("as.data.frame(", objectName, ")", sep="")
            }
        else if (objectClass == "rcorr"){
            commandRepeat <- 3
            objectCommandList <- c("$r", "$n", "$P")
            objectCommandName <- NULL
            for (i in 1:commandRepeat){
                objectCommandName[i] <- paste(objectName, objectCommandList[i], sep="")
                }
            }
        else if (objectClass == "by"){
            commandRepeat <- dim(tmpObject)
            objectCommandName <- NULL
            for (i in 1:commandRepeat){
                objectCommandName[i] <- paste("as.table(", objectName, 
                  "$", '"', i, '"', ")", sep="")
                }
            }
        else {
            objectName <- paste(".object", sep="")            objectCommandName <- paste(objectName)
            }
        justDoIt(paste(objectName, " <- tmpObject", sep=""))
        logger(paste(objectName, " <- popOutput()", sep=""))
        if (type == "LaTeX"){
            if (size != "" | na != ""){
                usePrint <- paste("print(", sep="")
                printType <- paste(", type=", '"', "latex", '"', ")", sep="")
                }
            else {
                usePrint <- paste("", sep="")
                printType <- paste("", sep="")
                }
            }
        else if (type == "HTML"){
            usePrint <- paste("print(", sep="")
            printType <- paste(", type=", '"', "html", '"', ")", sep="")
            }
        for (i in 1:commandRepeat){
            doItAndPrint(paste(usePrint, "xtable(", objectCommandName[i], caption, label, 
              align, digits, display, ")", size, na, printType, sep=""))
            }
        justDoIt(paste("remove(tmpObject)", sep=""))
        logger(paste("remove(", objectName, ")", sep=""))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="xtable")
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(typeFrame, sticky="sw")
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Arguments"), fg="blue"), 
      tklabel(optionsFrame, text=gettextRcmdr("(optional)"), fg="blue"), sticky="w")   
    tkgrid(tklabel(additionalFrame, text=gettextRcmdr("Printing"), fg="blue"), 
      tklabel(additionalFrame, text=gettextRcmdr("options"), fg="blue"), sticky="w") 
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Caption:")), captionField, sticky="w")
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Label:")), labelField, sticky="w")
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Align:")), alignField, sticky="w")
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Digits:")), digitsField, sticky="w")
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Display:")), displayField, sticky="w")
    tkgrid(tklabel(additionalFrame, text=gettextRcmdr("Size:")), sizeField, sticky="w")
    tkgrid(tklabel(additionalFrame, text=gettextRcmdr("NA string:")), naField, sticky="w")
    tkgrid(dataFrame, tklabel(top, text=" "), optionsFrame, tklabel(top, text=" "), 
      additionalFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=3, sticky="w")
    dialogSuffix(rows=3, columns=3)
    }