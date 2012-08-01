var True = true;
var False = false;

/*******************************************************************************
 * Thunks.
 */

// Force a thunk (if it is a thunk) until WHNF.
function _(thunkish,nocache){
    while (thunkish instanceof $) {
        thunkish = thunkish.force(nocache);
    }
    return thunkish;
}

// Apply a function to arguments (see method2 in Fay.hs).
function __(){
    var f = arguments[0];
    for (var i = 1, len = arguments.length; i < len; i++) {
        f = (f instanceof $? _(f) : f)(arguments[i]);
    }
    return f;
}

// Thunk object.
function $(value){
    this.forced = false;
    this.value = value;
}

// Force the thunk.
$.prototype.force = function(nocache){
    return nocache
        ? this.value()
        : this.forced
        ? this.value
        : (this.forced = true, this.value = this.value());
};

/*******************************************************************************
 * Constructors.
 */

// A constructor.
function Fay$$Constructor(){
    this.name = arguments[0];
    this.fields = Array.prototype.slice.call(arguments,1);
}

// Eval in the context of the Haskell bindings.
function Fay$$eval(str){
    return eval(str);
}

/*******************************************************************************
 * Monad.
 */

function Fay$$Monad(value){
    this.value = value;
}

// >>
function Fay$$then(a){
    return function(b){
        return new $(function(){
            _(a,true);
            return b;
        });
    };
}

// >>=
function Fay$$bind(m){
    return function(f){
        return new $(function(){
            var monad = _(m,true);
            return f(monad.value);
        });
    };
}

var Fay$$unit = null;

// return
function Fay$$return(a){
    return new Fay$$Monad(a);
}

/*******************************************************************************
 * FFI.
 */

// Serialize a Fay object to JS.
function Fay$$serialize(type,fayObj){
    var base = type[0];
    var args = type[1];
    var jsObj;
    switch(base){
    case "action": {
        // A nullary monadic action. Should become a nullary JS function.
        // Fay () -> function(){ return ... }
        jsObj = function(){
            return Fay$$serialize(args[0],_(fayObj,true).value);
        };
        break;
    }
    case "function": {
        // A proper function.
        jsObj = function(){
            var return_type = args[args.length-1];
            var len = args.length;
            // If some arguments.
            if (len > 1) {
                // Apply to all the arguments.
                fayObj = _(fayObj,true);
                for (var i = 0, len = len; i < len - 1; i++) {
                    // Unserialize the JS values to Fay for the Fay callback.
                    fayObj = _(fayObj(Fay$$unserialize(args[i],arguments[i])),true);
                }
                // Finally, serialize the Fay return value back to JS.
                var return_base = return_type[0];
                var return_args = return_type[1];
                // If it's a monadic return value, get the value instead.
                if(return_base == "action") {
                    return Fay$$serialize(return_args[0],fayObj.value);
                }
                // Otherwise just serialize the value direct.
                else {
                    return Fay$$serialize(return_type,fayObj);
                }
            } else {
                throw new Error("Nullary function?");
            }
        };
        break;
    }
    case "string": {
        // Serialize Fay string to JavaScript string.
        var str = "";
        fayObj = _(fayObj);
        while(fayObj instanceof Fay$$Cons) {
            str += fayObj.car;
            fayObj = _(fayObj.cdr);
        }
        jsObj = str;
        break;
    }
    case "list": {
        // Serialize Fay list to JavaScript array.
        var arr = [];
        fayObj = _(fayObj);
        while(fayObj instanceof Fay$$Cons) {
            arr.push(Fay$$serialize(args[0],fayObj.car));
            fayObj = _(fayObj.cdr);
        }
        jsObj = arr;
        break;
    }
    case "double": {
        // Serialize double, just force the argument. Doubles are unboxed.
        jsObj = _(fayObj);
        break;
    }
    case "unknown": {
        // Just force unknown values to WHNF.
        jsObj = _(fayObj);
        break;
    }
    case "bool": {
        // Bools are unboxed.
        jsObj = fayObj;
        break;
    }
    default: throw new Error("Unhandled serialize type: " + base);
    }
    return jsObj;
}

// Unserialize an object from JS to Fay.
function Fay$$unserialize(type,jsObj){
    var base = type[0];
    var args = type[1];
    var fayObj;
    switch(base){
    case "action": {
        // Unserialize a "monadic" JavaScript return value into a monadic value.
        fayObj = Fay$$return(Fay$$unserialize(args[0],jsObj));
        break;
    }
    case "string": {
        // Unserialize a JS string into Fay list (String).
        fayObj = Fay$$list(jsObj);
        break;
    }
    case "list": {
        // Unserialize a JS array into a Fay list ([a]).
        var serializedList = [];
        for (var i = 0, len = jsObj.length; i < len; i++) {
            // Unserialize each JS value into a Fay value, too.
            serializedList.push(Fay$$unserialize(args[0],jsObj[i]));
        }
        // Pop it all in a Fay list.
        fayObj = Fay$$list(serializedList);
        break;
    }
    case "double": {
        // Doubles are unboxed, so there's nothing to do.
        fayObj = jsObj;
        break;
    }
    case "unknown": {
        // Any unknown values can be left as-is.
        fayObj = jsObj;
        break;
    }
    default: throw new Error("Unhandled unserialize type: " + base);
    }
    return fayObj;
}

// Encode a value to a Show representation
function Fay$$encodeShow(x){
    if (x instanceof $) x = _(x);
    if (x instanceof Array) {
        if (x.length == 0) {
            return "[]";
        } else {
            if (x[0] instanceof Fay$$Constructor) {
                if(x[0].fields.length > 0) {
                    var args = x.slice(1);
                    var fieldNames = x[0].fields;
                    return "(" + x[0].name + " { " + args.map(function(x,i){
                        return fieldNames[i] + ' = ' + Fay$$encodeShow(x);
                    }).join(", ") + " })";
                } else {
                    var args = x.slice(1);
                    return "(" + [x[0].name].concat(args.map(Fay$$encodeShow)).join(" ") + ")";
                }
            } else {
                return "[" + x.map(Fay$$encodeShow).join(",") + "]";
            }
        }
    } else if (typeof x == 'string') {
        return JSON.stringify(x);
    } else if(x instanceof Fay$$Cons) {
        return Fay$$encodeShow(Fay$$serialize(["list",[["unknown"]]],x));
    } else if(x == null) {
        return '[]';
    } else {
        return x.toString();
    }
}

/*******************************************************************************
 * Lists.
 */

// Cons object.
function Fay$$Cons(car,cdr){
    this.car = car;
    this.cdr = cdr;
}

// Make a list.
function Fay$$list(xs){
    var out = null;
    for(var i=xs.length-1; i>=0;i--)
        out = new Fay$$Cons(xs[i],out);
    return out;
}

// Built-in list cons.
function Fay$$cons(x){
    return function(y){
        return new Fay$$Cons(x,y);
    };
}

// List index.
function Fay$$index(index){
    return function(list){
        for(var i = 0; i < index; i++) {
            list = _(list).cdr;
        }
        return list.car;
    };
}

/*******************************************************************************
 * Numbers.
 */

// Built-in *.
function Fay$$mult(x){
    return function(y){
        return _(x) * _(y);
    };
}

// Built-in +.
function Fay$$add(x){
    return function(y){
        return _(x) + _(y);
    };
}

// Built-in -.
function Fay$$sub(x){
    return function(y){
        return _(x) - _(y);
    };
}

// Built-in /.
function Fay$$div(x){
    return function(y){
        return _(x) / _(y);
    };
}

/*******************************************************************************
 * Booleans.
 */

// Are two values equal?
function Fay$$equal(lit1,lit2){
    // Simple case
    lit1 = _(lit1);
    lit2 = _(lit2);
    if(lit1 === lit2) {
        return true;
    }
    // General case
    if(lit1 instanceof Array) {
        if(lit1.length!=lit2.length) return false;
        for(var len = lit1.length, i = 0; i < len; i++) {
            if(!Fay$$equal(lit1[i],lit2[i]))
                return false;
        }
        return true;
    } else if (lit1 instanceof Fay$$Cons) {
        while(lit1 instanceof Fay$$Cons && lit2 instanceof Fay$$Cons && Fay$$equal(lit1.car,lit2.car))
            lit1 = lit1.cdr, lit2 = lit2.cdr;
        return (lit1 === null && lit2 === null);
    } else return false;
}

// Built-in ==.
function Fay$$eq(x){
    return function(y){
        return Fay$$equal(x,y);
    };
}

// Built-in /=.
function Fay$$neq(x){
    return function(y){
        return !(Fay$$equal(x,y));
    };
}

// Built-in >.
function Fay$$gt(x){
    return function(y){
        return _(x) > _(y);
    };
}

// Built-in <.
function Fay$$lt(x){
    return function(y){
        return _(x) < _(y);
    };
}

// Built-in >=.
function Fay$$gte(x){
    return function(y){
        return _(x) >= _(y);
    };
}

// Built-in <=.
function Fay$$lte(x){
    return function(y){
        return _(x) <= _(y);
    };
}

// Built-in &&.
function Fay$$and(x){
    return function(y){
        return _(x) && _(y);
    };
}

// Built-in ||.
function Fay$$or(x){
    return function(y){
        return _(x) || _(y);
    };
}

/*******************************************************************************
 * Mutable references.
 */

// Make a new mutable reference.
function Fay$$Ref(x){
    this.value = x;
}

// Write to the ref.
function Fay$$writeRef(ref,x){
    ref.value = x;
}

// Get the value from the ref.
function Fay$$readRef(ref,x){
    return ref.value;
}

/*******************************************************************************
 * Dates.
 */
function Fay$$date(str){
    return window.Date.parse(str);
}

/*******************************************************************************
 * Application code.
 */
