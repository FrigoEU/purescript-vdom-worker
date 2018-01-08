"use strict";

// module VirtualDOM
/* global require, exports, document */

var VNode = require("virtual-dom").VNode;
exports.vnode = function vnode(tag){
  return function(props){
    return function(children){
      return new VNode(tag, props, children);
    };
  };
};

var VText = require("virtual-dom").VText;
exports.vtext = function vtext(str){
  return new VText(str);
};

var createElementForn = require("virtual-dom").create;
exports.createElement = function(vtree){
  return function(){
    return createElementForn(vtree);
  };
};

exports.appendToBody = function(node){
  return function(){
    return document.body.appendChild(node);
  };
};

var diffForn = require("virtual-dom").diff;
exports.diff = function(vtree1){
  return function(vtree2){
    return diffForn(vtree1, vtree2);
  };
};

var serializePatchForn = require("vdom-serialized-patch").serialize;
exports.serializePatchImpl = function(functionSerializer){
  return function(patches){
    return serializePatchForn(patches, functionSerializer);
  };
};

var patchForn = require("vdom-serialized-patch").patch;
exports.applyPatch = function(node){
  return function(patches){
    return function(deserializer){
      return function(){
        return patchForn(node, patches, deserializer);
      };
    };
  };
};

exports.prop = function(key){
  return function(val){
    return [key, val];
  };
};

exports.props = function(arr){
  var obj = {}, a;
  for (var i = 0; i < arr.length; i++){
    a = arr[i];
    obj[a[0]] = a[1];
  }
  return obj;
};

exports.stringify = JSON.stringify;
exports.parse = JSON.parse;
