"use strict";

// module VirtualDOM.Worker
/* global require, exports, document */

exports.crossAndAtRegex = /#(\d+)@(.*)/;

exports.mkWorkerFunctionsForWEvents = function(){
  var functionIndex = { nextIndex: 0, map: {} };

  function serializeProperty(str){
    var splitted = str.split("@");
    if (splitted.length === 2){
      return {prop: str, val: str};
    } else {
      return {prop: splitted[0], val: splitted[1]};
    }
  }

  function functionSerializer(key, func){
    var index;
    if (!func.vdomAsJsonFunctionIndex){
      index = functionIndex.nextIndex.toString();
      functionIndex.map.set(index, func); // Save into map so we can find function by index later
      func.vdomAsJsonFunctionIndex = index; // Save index on function object so we don't make new indexes for the same function
      functionIndex.nextIndex = functionIndex.nextIndex + 1;
    } else {
      index = func.vdomAsJsonFunctionIndex;
    }

    var r = serializeProperty(key), str;
    str = "#" + index + "@" + r.val;

    return [r.prop, str];
  }

  function handler(m){
    var message = JSON.parse(m.data);
    functionIndex.map.get(message.id)(message.data);
  }

  return {
    functionSerializer: functionSerializer,
    handler: handler
  };
};

