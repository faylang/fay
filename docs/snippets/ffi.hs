print :: Foreign a => a -> Fay ()
print = foreignFay "window.console.log" ""

alert :: Foreign a => a -> Fay ()
alert = foreignFay "window.alert" ""
