"use strict";

// module VirtualDOM
/* global require, exports, document */

var VNode = require("virtual-dom/vnode/vnode");
exports.vnode = function vnode(tag){
  return function(props){
    return function(children){
      return new VNode(tag, props, children);
    };
  };
};

var VText = require("virtual-dom/vnode/vtext");
exports.vtext = function vtext(str){
  return new VText(str);
};

var createElement = require("virtual-dom/create-element");
exports.createElement = function(vtree){
  return function(){
    return createElement(vtree);
  };
};

exports.appendToBody = function(node){
  return function(){
    return document.body.appendChild(node);
  };
};

var diff = require("virtual-dom/diff");
exports.diff = function(vtree1){
  return function(vtree2){
    return diff(vtree1, vtree2);
  };
};

var serializePatch = require("vdom-serialized-patch/serialize");
exports.serializePatchImpl = function(functionSerializer){
  return function(patches){
    return serializePatch(patches, functionSerializer);
  };
};

var ap = require("vdom-serialized-patch/patch");
exports.applyPatch = function(node){
  return function(patches){
    return function(deserializer){
      return function(){
        return ap(node, patches, deserializer);
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
