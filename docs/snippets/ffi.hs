print :: Foreign a => a -> Fay ()
print = ffi "window.console.log(%1)" ""

alert :: Foreign a => a -> Fay ()
alert = ffi "window.alert(%1)" ""

getInnerHtml :: Element -> Fay String
getInnerHtml = ffi "%1.innerHTML(%2)" FayString

setInnerHtml :: Element -> String -> Fay ()
setInnerHtml = ffi "%1.innerHTML=%2"

thedocument :: Element
thedocument = ffi "window.document" FayNone

jquery :: Element -> JQuery
jquery = ffi "window.jQuery(%1)" FayNone
