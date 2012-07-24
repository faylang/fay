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
function Fay$$serialize(type,obj){
  type = _(type);
  if(type) type = type[0].name;
  if(type == "JsType"){
    return function(){
      return _(obj,true).value;
    };
  } else {
    obj = _(obj);
    if(type == "StringType" ||
       (obj instanceof Fay$$Cons && typeof obj.car == 'string')){
      var str = "";
      while(obj instanceof Fay$$Cons) {
        str += obj.car;
        obj = _(obj.cdr);
      }
      return str;      
    } else if(type == "FunctionType" || typeof obj == 'function'){
      return function(){
        var out = obj;
        for (var len = arguments.length, i = 0; i < len; i++){
          if(typeof out != 'function') {
            throw "Wrong number of arguments for callback: " + arguments.toString();
          }
          out = out(arguments[i]);
        }
        return _(out,true);
      };    
    } else if(type == "ListType" || (obj instanceof Fay$$Cons)){
      var arr = [];
      while(obj instanceof Fay$$Cons) {
        arr.push(Fay$$serialize(null,obj.car));
        obj = _(obj.cdr);
      }
      return arr;      
    } // else if(type == "BoolType || obj == _(True) || obj == _(False)) {
    //   return obj == _(True);
    // } 
    else {
      return obj;
    }      
  }
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
    return Fay$$encodeShow(Fay$$serialize(ListType,x));
  } else if(x == null) {
    return '[]';
  } else {
    return x.toString();
  }
}

// Unserialize an object from JS to Fay.
function Fay$$unserialize(typ,obj){
  if(typ == 'string' || typ == 'array')
    return Fay$$list(obj);
  else if(typ == 'bool')
    return obj? True : False;
  else if(typ == 'data') {
    alert('Time to unserialize a data record!');
  }
  else return obj;
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

