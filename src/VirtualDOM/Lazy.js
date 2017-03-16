"use strict";

/* global console */

/* Stolen from https://github.com/evancz/virtual-dom/blob/master/src/wrapper.js */
function Thunk(fn, args, thunk) {
  /* public (used by VirtualDom.js) */
  this.vnode = null;
  this.key = undefined;

  /* private */
  this.fn = fn;
  this.args = args;
  this.thunk = thunk;

  /* debugging */
  this.failedMemoizations = 0;
  this.failedMemoizationsFn = 0;
  this.succeededMemoizations = 0;
}

Thunk.prototype.type = "Thunk";
Thunk.prototype.render = renderThunk;

function shouldUpdate(current, previous) {
  if (current.fn !== previous.fn) {
    current.failedMemoizationsFn = (previous.failedMemoizationsFn || 0) + 1;
    checkFailingMemoization(current);
    return true;
  }

  // if it's the same function, we know the number of args must match
  var cargs = current.args;
  var pargs = previous.args;

  for (var i = cargs.length; i--; ) {
    if (cargs[i] !== pargs[i]) {
      current.failedMemoizations = (previous.failedMemoizations || 0) + 1;
      checkFailingMemoization(current);
      return true;
    }
  }

  current.succeededMemoizations = (previous.succeededMemoizations || 0) + 1;
  return false;
}

function checkFailingMemoization(current){
  if (current.failedMemoizationsFn > 5 && current.succeededMemoizations === 0){
    console.error("Highly likely lazyRef not working. Component function didn't match. Node: ");
    console.dir(current);
  }
  if (current.failedMemoizations > 5 && current.succeededMemoizations === 0){
    console.warn("Possible lazyRef not working. Arguments didn't match. Node: ");
    console.dir(current);
  }
}

function renderThunk(previous) {
  if (previous == null || shouldUpdate(this, previous)) {
    return this.thunk();
  } else {
    return previous.vnode;
  }
}
exports.lazyRef1Impl = function lazyRef1Impl(fn, a) {
  function thunk() {
    return fn(a);
  }
  return new Thunk(fn, [a], thunk);
};

exports.lazyRef2Impl = function lazyRef2Impl(fn, a, b) {
  function thunk() {
    return fn(a)(b);
  }
  return new Thunk(fn, [a,b], thunk);
};

exports.lazyRef3Impl = function lazyRef3Impl(fn, a, b, c) {
  function thunk() {
    return fn(a)(b)(c);
  }
  return new Thunk(fn, [a,b,c], thunk);
};
exports.lazyRef4Impl = function lazyRef4Impl(fn, a, b, c, d) {
  function thunk() {
    return fn(a)(b)(c)(d);
  }
  return new Thunk(fn, [a,b,c,d], thunk);
};
exports.lazyRef5Impl = function lazyRef5Impl(fn, a, b, c, d, e) {
  function thunk() {
    return fn(a)(b)(c)(d)(e);
  }
  return new Thunk(fn, [a,b,c,d,e], thunk);
};

exports.lazyRef6Impl = function lazyRef6Impl(fn, a, b, c, d, e, f) {
  function thunk() {
    return fn(a)(b)(c)(d)(e)(f);
  }
  return new Thunk(fn, [a,b,c,d,e,f], thunk);
};
exports.lazyRef7Impl = function lazyRef7Impl(fn, a, b, c, d, e, f, g) {
  function thunk() {
    return fn(a)(b)(c)(d)(e)(f)(g);
  }
  return new Thunk(fn, [a,b,c,d,e,f,g], thunk);
};
