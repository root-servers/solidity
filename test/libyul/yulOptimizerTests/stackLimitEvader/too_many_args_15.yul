{
    {
        mstore(0x40, memoryguard(128))
        sstore(g(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29, 30), 0)
    }
    function g(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, b22, b23, b24, b25, b26, b27, b28, b29, b30) -> v {
	sstore(0, b14)
	sstore(1, b15)
	sstore(2, b16)
	sstore(3, b17)
	sstore(4, b18)
	sstore(5, b19)
	sstore(6, b29)
	sstore(7, b30)
        v := b30
        sstore(b1, b30)
    }

}
// ----
// step: stackLimitEvader
//
// {
//     {
//         mstore(0x40, memoryguard(0x0280))
//         sstore(g(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30), 0)
//     }
//     function g(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, b22, b23, b24, b25, b26, b27, b28, b29, b30) -> v
//     {
//         mstore(0xa0, b16)
//         mstore(0xc0, b17)
//         mstore(0xe0, b18)
//         mstore(0x0100, b19)
//         mstore(0x0120, b20)
//         mstore(0x0140, b21)
//         mstore(0x0160, b22)
//         mstore(0x0180, b23)
//         mstore(0x01a0, b24)
//         mstore(0x01c0, b25)
//         mstore(0x01e0, b26)
//         mstore(0x0200, b27)
//         mstore(0x0220, b28)
//         mstore(0x0240, b29)
//         mstore(0x0260, b30)
//         mstore(0x80, 0)
//         sstore(0, b14)
//         sstore(1, b15)
//         sstore(2, mload(0xa0))
//         sstore(3, mload(0xc0))
//         sstore(4, mload(0xe0))
//         sstore(5, mload(0x0100))
//         sstore(6, mload(0x0240))
//         sstore(7, mload(0x0260))
//         mstore(0x80, mload(0x0260))
//         sstore(b1, mload(0x0260))
//         v := mload(0x80)
//     }
// }
