// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract C { 
    D d;
    constructor(D addr) { d = addr; }
    function foo(address add) public { payable(add).call(10); }
    receive() external payable { d.g(); }
}

contract D { 
    uint public x;
    constructor() payable { } 
    function f(address a) public payable { payable(a).call(100); }
    function g() public { x = 1; }
    receive() external payable { x += 1; }
}