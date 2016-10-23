// module VirtualDOM.SEvent

exports.magic = function(action){
  return function(a){
    if (action.constructor.value){
      return action.constructor.value;
    } else if (action.constructor.create){
      var v0 = {}, keys, l, i;
      if(action.value0){
        keys = Object.keys(action.value0);
        l = keys.length;
        for (i = 0; i < l; i++){
          v0[keys[i]] = action.value0[keys[i]];
        }
        if(v0.val !== undefined){
          v0.val = a;
        }
      }
      return action.constructor.create(v0);
    } else {
      throw new Error("Invalid Action provided to Client.SEvents.magic");
    }
  };
};
