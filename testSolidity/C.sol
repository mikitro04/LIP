// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.8.1 < 0.9.0;

contract C { 
      uint x; 
      receive() external payable { x = 5; }
}