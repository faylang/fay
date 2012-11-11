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
$.prototype.force = function(nocache) {
  return nocache ?
    this.value() :
    (this.forced ?
     this.value :
     (this.value = this.value(), this.forced = true, this.value));
};

/*******************************************************************************
 * Monad.
 */

function Fay$$Monad(value){
    this.value = value;
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then(a){
    return function(b){
      return Fay$$bind(a)(function(_){
        return b;
      });
    };
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind(m){
  return function(f){
    return new $(function(){
      var monad = _(m,true);
      if(monad.cont) {
        return _(monad.cont(f));
      }
      else {
        return f(monad.value);
      }
    });
  };
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$$_return(a){
    return new Fay$$Monad(a);
}

// Unit: ().
var Fay$$unit = null;

/*******************************************************************************
 * Serialization.
 * Fay <-> JS. Should be bijective.
 */

// Serialize a Fay object to JS.
function Fay$$fayToJs(type,fayObj){
    var base = type[0];
    var args = type[1];
    var jsObj;
    switch(base){
    case "action": {
        // A nullary monadic action. Should become a nullary JS function.
        // Fay () -> function(){ return ... }
        jsObj = function(){
            return Fay$$fayToJs(args[0],_(fayObj,true).value);
        };
        break;
    }
    case "function": {
        // A proper function.
        jsObj = function(){
            var fayFunc = fayObj;
            var return_type = args[args.length-1];
            var len = args.length;
            // If some arguments.
            if (len > 1) {
                // Apply to all the arguments.
                fayFunc = _(fayFunc,true);
                // TODO: Perhaps we should throw an error when JS
                // passes more arguments than Haskell accepts.
                for (var i = 0, len = len; i < len - 1 && fayFunc instanceof Function; i++) {
                    // Unserialize the JS values to Fay for the Fay callback.
                    fayFunc = _(fayFunc(Fay$$jsToFay(args[i],arguments[i])),true);
                }
                // Finally, serialize the Fay return value back to JS.
                var return_base = return_type[0];
                var return_args = return_type[1];
                // If it's a monadic return value, get the value instead.
                if(return_base == "action") {
                    return Fay$$fayToJs(return_args[0],fayFunc.value);
                }
                // Otherwise just serialize the value direct.
                else {
                    return Fay$$fayToJs(return_type,fayFunc);
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
    case "list": case "tuple": {
        // Serialize Fay list or tuple to JavaScript array.
        var arr = [];
        fayObj = _(fayObj);
        while(fayObj instanceof Fay$$Cons) {
            arr.push(Fay$$fayToJs(args[0],fayObj.car));
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
    case "int": {
        // Serialize int, just force the argument. Ints are unboxed.
        jsObj = _(fayObj);
        break;
    }
    case "bool": {
        // Bools are unboxed.
        jsObj = _(fayObj);
        break;
    }
    case "unknown":
    case "user": {
        if(fayObj instanceof $)
            fayObj = _(fayObj);
        jsObj = Fay$$fayToJsUserDefined(type,fayObj);
        break;
    }
    default: throw new Error("Unhandled Fay->JS translation type: " + base);
    }
    return jsObj;
}

// Unserialize an object from JS to Fay.
function Fay$$jsToFay(type,jsObj){
    var base = type[0];
    var args = type[1];
    var fayObj;
    switch(base){
    case "action": {
        // Unserialize a "monadic" JavaScript return value into a monadic value.
        fayObj = new Fay$$Monad(Fay$$jsToFay(args[0],jsObj));
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
            serializedList.push(Fay$$jsToFay(args[0],jsObj[i]));
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
    case "int": {
        // Int are unboxed, so there's no forcing to do.
        // But we can do validation that the int has no decimal places.
        // E.g. Math.round(x)!=x? throw "NOT AN INTEGER, GET OUT!"
        fayObj = Math.round(jsObj);
        if(fayObj!==jsObj) throw "Argument " + jsObj + " is not an integer!";
        break;
    }
    case "bool": {
        // Bools are unboxed.
        fayObj = jsObj;
        break;
    }
    case "unknown":
    case "user": {
        if (jsObj && jsObj['instance']) {
            fayObj = Fay$$jsToFayUserDefined(type,jsObj);
        }
        else
            fayObj = jsObj;
        break;
    }
    default: throw new Error("Unhandled JS->Fay translation type: " + base);
    }
    return fayObj;
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

// List length.
function Fay$$listLen(list,max){
  for(var i = 0; list !== null && i < max + 1; i++) {
    list = _(list).cdr;
  }
  return i == max;
}

/*******************************************************************************
 * Numbers.
 */

// Built-in *.
function Fay$$mult(x){
    return function(y){
        return new $(function(){
            return _(x) * _(y);
        });
    };
}

// Built-in +.
function Fay$$add(x){
    return function(y){
        return new $(function(){
            return _(x) + _(y);
        });
    };
}

// Built-in -.
function Fay$$sub(x){
    return function(y){
        return new $(function(){
            return _(x) - _(y);
        });
    };
}

// Built-in /.
function Fay$$div(x){
    return function(y){
        return new $(function(){
            return _(x) / _(y);
        });
    };
}

/*******************************************************************************
 * Booleans.
 */

// Are two values equal?
function Fay$$equal(lit1, lit2) {
    // Simple case
    lit1 = _(lit1);
    lit2 = _(lit2);
    if (lit1 === lit2) {
        return true;
    }
    // General case
    if (lit1 instanceof Array) {
        if (lit1.length != lit2.length) return false;
        for (var len = lit1.length, i = 0; i < len; i++) {
            if (!Fay$$equal(lit1[i], lit2[i])) return false;
        }
        return true;
    } else if (lit1 instanceof Fay$$Cons && lit2 instanceof Fay$$Cons) {
        do {
            if (!Fay$$equal(lit1.car,lit2.car))
                return false;
            lit1 = _(lit1.cdr), lit2 = _(lit2.cdr);
            if (lit1 === null || lit2 === null)
                return lit1 === lit2;
        } while (true);
    } else if (typeof lit1 == 'object' && typeof lit2 == 'object' && lit1 && lit2 &&
              lit1.constructor === lit2.constructor) {
      for(var x in lit1) {
        if(!(lit1.hasOwnProperty(x) && lit2.hasOwnProperty(x) &&
            Fay$$equal(lit1[x],lit2[x])))
          return false;
      }
      return true;
    } else {
      return false;
    }
}

// Built-in ==.
function Fay$$eq(x){
    return function(y){
        return new $(function(){
            return Fay$$equal(x,y);
        });
    };
}

// Built-in /=.
function Fay$$neq(x){
    return function(y){
        return new $(function(){
            return !(Fay$$equal(x,y));
        });
    };
}

// Built-in >.
function Fay$$gt(x){
    return function(y){
        return new $(function(){
            return _(x) > _(y);
        });
    };
}

// Built-in <.
function Fay$$lt(x){
    return function(y){
        return new $(function(){
            return _(x) < _(y);
        });
    };
}

// Built-in >=.
function Fay$$gte(x){
    return function(y){
        return new $(function(){
            return _(x) >= _(y);
        });
    };
}

// Built-in <=.
function Fay$$lte(x){
    return function(y){
        return new $(function(){
            return _(x) <= _(y);
        });
    };
}

// Built-in &&.
function Fay$$and(x){
    return function(y){
        return new $(function(){
            return _(x) && _(y);
        });
    };
}

// Built-in ||.
function Fay$$or(x){
    return function(y){
        return new $(function(){
            return _(x) || _(y);
        });
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
