"use strict";

// module VirtualDOM
/* global require, exports, document */

exports.vnode = function vnode(tag){
  var VNode = require("virtual-dom/vnode/vnode");
  return function(props){
    return function(children){
      return new VNode(tag, props, children);
    };
  };
};

exports.vtext = function vtext(str){
  var VText = require("virtual-dom/vnode/vtext");
  return new VText(str);
};

exports.createElement = function(vtree){
  var createElement = require("virtual-dom/create-element");
  return createElement(vtree);
};

exports.appendToBody = function(node){
  return function(){
    return document.body.appendChild(node);
  };
};

exports.diff = function(vtree1){
  var diff = require("virtual-dom/diff");
  return function(vtree2){
    return diff(vtree1, vtree2);
  };
};

exports.serializePatch = function(functionSerializer){
  var serializePatch = require("vdom-serialized-patch/serialize");
  return function(patches){
    return serializePatch(patches, functionSerializer);
  };
};

exports.toString = function(serializedPatches){
  return JSON.stringify(serializedPatches);
};
exports.fromString = function(string){
  return JSON.parse(string);
};

exports.fromJson = function(makeEventHandlerSendToWorker){
  var fj = require("vdom-as-json/fromJson");
  return function(serializedPatches){
    return fj(serializedPatches, makeEventHandlerSendToWorker);
  };
};

exports.applyPatch = function(node){
  var ap = require("vdom-serialized-patch/patch");
  return function(patches){
    return function(){
      return ap(node, patches);
    };
  };
};

// exports.exampleSerializeProp = function serializeProperty(prop){
//   if (prop === "onclickxy"){
//     return {prop: "onclick", val: "xy"};
//   }
//   return {prop: prop, val: prop};
// };

exports.mkWorkerFunctions = function(serializeProperty){
  var functionIndex = { nextIndex: 0, map: {} };

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

exports.crossAndAtRegex = /#(\d+)@(.*)/;

// exports.mkMakeHandler = function(worker, replaceWithEventHandler){
//   return function(str){
//     var matches = str.match(regex), nr = matches[1], serialized = matches[2];
//     var message = {id: nr, data: null};

//     // if (serialized === "xy"){
//     //   return function(ev){
//     //     message.data = {x: ev.pageX, y: ev.pageY};
//     //     worker.postMessage(JSON.stringify(message));
//     //   };
//     // }

//     return function(){
//       worker.postMessage(JSON.stringify(message));
//     };
//   };
// };
