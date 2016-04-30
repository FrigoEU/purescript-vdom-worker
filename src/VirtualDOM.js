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

exports.applyPatch = function(node){
  var ap = require("vdom-serialized-patch/patch");
  return function(patches){
    return function(makeDOMHandlers){
      return function(){
        return ap(node, patches, function(str){
          return function(ev){
            return makeDOMHandlers(str)(ev)();
          };
        });
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
