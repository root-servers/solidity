contract C {
    function add(uint, uint) public pure {}
    function g() public pure {
        assembly {
            let x := add(1, 2)
            mstore(0, x)
        }
    }
}
// ----
