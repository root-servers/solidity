contract Other {
    constructor() payable {
    }
    function getAddress() public returns (address) {
        return address(this);
    }
}
contract ClientReceipt {
    Other other;
    constructor() payable {
        other = new Other{value:500}();
    }
    function getAddress() public returns (address) {
        return other.getAddress();
    }
}
// ====
// compileViaYul: also
// ----
// constructor(), 2000 wei ->
// contract.balance -> 1500
// getAddress() -> 0xF01F7809444BD9A93A854361C6FAE3F23D9E23DB
// contract.balance: 0xF01F7809444BD9A93A854361C6FAE3F23D9E23DB -> 500
