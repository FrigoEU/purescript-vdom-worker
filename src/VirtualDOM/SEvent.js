// module VirtualDOM.SEvent

// This is a terrible function that clones PureScript ADT's/records
// very unsafely and fills in the "val" property
exports.magic = function(action){
  return function(a){
    return cloneAndFillVal(action, a);
  };
};

function cloneAndFillVal(action, valToFill){
  var returnVal;
  if (action.constructor.value){
    returnVal = action.constructor.value;
  } else if(action == null || typeof action !== "object"){
    returnVal = action;
  } else {
    returnVal = createUntilDone(action);
    for (var prop in action) {
      if (action.hasOwnProperty(prop)) {
        returnVal[prop] = (prop === "val") ?
          valToFill : cloneAndFillVal(action[prop], valToFill);
      }
    }
  }
  return returnVal;
}
function createUntilDone(action){
  var returnVal = action.constructor.create(action.constructor);
  while (typeof returnVal === "function"){
    returnVal = returnVal();
  }
  return returnVal;
}

exports.makeHook = function makeHook(o){
  var Hook = function () {};
  Hook.prototype.hook = function (node) {
    if(!node.initialized){
      node.initialized = true;
      o.hook(node)();
    }
  };
  Hook.prototype.unhook = function (node) {
    o.unhook(node)();
  };
  return new Hook();
};
