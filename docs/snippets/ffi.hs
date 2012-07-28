print :: Foreign a => a -> Fay ()
print = foreignFay "window.console.log" ""

alert :: Foreign a => a -> Fay ()
alert = foreignFay "window.alert" ""

getInnerHtml :: Element -> Fay String
getInnerHtml = foreignMethodFay "innerHTML" FayString

setInnerHtml :: Element -> String -> Fay ()
setInnerHtml = foreignSetProp "innerHTML"

thedocument :: Element
thedocument = foreignValue "window.document" FayNone

jquery :: Element -> JQuery
jquery = foreignPure "window.jQuery" FayNone
