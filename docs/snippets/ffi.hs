print :: Foreign a => a -> Fay ()
print = foreignFay "window.console.log" ""

alert :: Foreign a => a -> Fay ()
alert = foreignFay "window.alert" ""

thebody :: Element
thebody = foreignPure "document.body" FayNone

getInnerHtml :: Element -> Fay String
getInnerHtml = foreignPropFay "innerHTML" FayString

setInnerHtml :: Element -> String -> Fay ()
setInnerHtml = foreignSetProp "innerHTML"
