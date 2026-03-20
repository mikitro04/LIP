// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Salvadanaio {
    event FondiRicevuti(address mittente, uint256 quantita);

    // Si attiva quando mandi Ether direttamente al contratto
    receive() external payable {
        emit FondiRicevuti(msg.sender, msg.value);
    }

    function getSaldo() public pure {
        return;
    }
}