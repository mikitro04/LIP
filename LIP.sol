// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract LIP {
    // le variabili hanno dei valori di default
    /*int x;
    uint y;
    bool b;*/
    address owner; // =>  0x5B38Da6a701c568545dCfcB03FcB875f56beddC4
    Oracle c;
    
    constructor(address o) {
        owner = msg.sender;
        c = Oracle(o);
    }

    function deposit() public payable {
        require(msg.value >= 10);       //msg.value = il valore che mi stanno passando
        
    }

    function withdraw(address payable a, uint amt) public {
        require(amt < address(this).balance - 100);
        int x = c.getX{value: 15}();        // {value: 15} sto pagando 15 unita' di CryptoValute
        require(x > 1000);      // se lo metto dopo la trasfer abortisce tutte le transazioni precedenti lo stesso  (effetto collaterale, chi richiama il contratto paga lo stesso)
        a.transfer(amt);
    }
}

contract Oracle {
    uint x;

    function setX(int n) public {
        x = uint(n);
    }

    function getX() public payable returns(int) {
        require(msg.value > 10);
        return int(x);
    }    
}