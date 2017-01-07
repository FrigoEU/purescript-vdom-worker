"use strict";

/* global window, MouseEvent, exports */

exports.innerHTML = function(el){
  return function(){
    return el.innerHTML;
  };
};

exports.sendClick = function (el){
  return function (screenX){
    return function(screenY){
      return function(){
           var ev = new MouseEvent("click", {
             "view": window,
             "bubbles": true,
             "cancelable": true,
             "screenX": screenX,
             "screenY": screenY
           });

           el.dispatchEvent(ev);
      };
    };
  };
};
