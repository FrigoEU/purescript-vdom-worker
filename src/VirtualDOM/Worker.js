"use strict";

// module VirtualDOM.Worker
/* global exports */

exports.crossAndAtRegex = /#(\d+)@(.*)/;

// From onclick@xy to {prop: "onclick", val: "xy"}
function serializeProperty(str){
  var splitted = str.split("@");
  if (splitted.length === 2){
    return {prop: splitted[0], val: splitted[1]};
  } else {
    return {prop: str, val: str};
  }
}

exports.mkWorkerFunctionsForWEvents = function(){
  var functionMap = { nextIndex: 0, map: {} };

  function functionSerializer(key, func){
    var index;
    if (!func.vdomAsJsonfunctionMap){
      index = functionMap.nextIndex.toString();
      functionMap.map[index] = func; // Save into map so we can find function by index later
      func.vdomAsJsonfunctionMap = index; // Save index on function object so we don't make new indexes for the same function
      functionMap.nextIndex = functionMap.nextIndex + 1;
    } else {
      index = func.vdomAsJsonfunctionMap;
    }

    var r = serializeProperty(key), str;
    str = "#" + index + "@" + r.val;

    return [r.prop, str];
  }

  function handler(weventmessage){
    return functionMap.map[weventmessage.id](weventmessage.data);
  }

  return {
    functionSerializer: functionSerializer,
    handler: handler
  };
};

// TODO grab the correct WEventHandler here already and compose with the func
exports.localFunctionSerializer = function(key, func){
  var r = serializeProperty(key);
  return [r.prop, func];
};
