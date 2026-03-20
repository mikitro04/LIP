// SPDX-License-Identifier: MIT
pragma solidity ^ 0.8.0;

contract C {  
    function f() public payable { }
    function g(uint amt) public { payable(msg.sender).transfer(amt); } 
}

contract D { 
    C c; 
    uint x; 
    constructor(C add) payable { c = add; } 
    function dp(uint amt) public { c.f{value:amt}(); }
    function wd(uint amt) public { c.g(amt); }
}