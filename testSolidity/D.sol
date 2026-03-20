// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.8.1 < 0.9.0;

contract D { 
      constructor() payable { } 
      function f(address a) public { payable(a).transfer(1); }
}