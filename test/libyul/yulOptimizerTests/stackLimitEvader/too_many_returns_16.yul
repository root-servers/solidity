{
    {
        mstore(0x40, memoryguard(128))
	let a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15 := g(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31)
        sstore(0, 1)
	a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15 := g(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31)
    }
    function g(b16, b17, b18, b19, b20, b21, b22, b23, b24, b25, b26, b27, b28, b29, b30, b31) -> b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15 {
	b1 := 1
	b2 := 2
	b15 := 15
    }

}
// ----
// step: stackLimitEvader
//
// {
//     {
//         mstore(0x40, memoryguard(0x0260))
//         g(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31)
//         let a1 := mload(0x80)
//         let a2 := mload(0xa0)
//         let a3 := mload(0xc0)
//         let a4 := mload(0xe0)
//         let a5 := mload(0x0100)
//         let a6 := mload(0x0120)
//         let a7 := mload(0x0140)
//         let a8 := mload(0x0160)
//         let a9 := mload(0x0180)
//         let a10 := mload(0x01a0)
//         let a11 := mload(0x01c0)
//         let a12 := mload(0x01e0)
//         let a13 := mload(0x0200)
//         let a14 := mload(0x0220)
//         let a15 := mload(0x0240)
//         sstore(0, 1)
//         g(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31)
//         a1 := mload(0x80)
//         a2 := mload(0xa0)
//         a3 := mload(0xc0)
//         a4 := mload(0xe0)
//         a5 := mload(0x0100)
//         a6 := mload(0x0120)
//         a7 := mload(0x0140)
//         a8 := mload(0x0160)
//         a9 := mload(0x0180)
//         a10 := mload(0x01a0)
//         a11 := mload(0x01c0)
//         a12 := mload(0x01e0)
//         a13 := mload(0x0200)
//         a14 := mload(0x0220)
//         a15 := mload(0x0240)
//     }
//     function g(b16, b17, b18, b19, b20, b21, b22, b23, b24, b25, b26, b27, b28, b29, b30, b31)
//     {
//         mstore(0x80, 0)
//         mstore(0xa0, 0)
//         mstore(0xc0, 0)
//         mstore(0xe0, 0)
//         mstore(0x0100, 0)
//         mstore(0x0120, 0)
//         mstore(0x0140, 0)
//         mstore(0x0160, 0)
//         mstore(0x0180, 0)
//         mstore(0x01a0, 0)
//         mstore(0x01c0, 0)
//         mstore(0x01e0, 0)
//         mstore(0x0200, 0)
//         mstore(0x0220, 0)
//         mstore(0x0240, 0)
//         mstore(0x80, 1)
//         mstore(0xa0, 2)
//         mstore(0x0240, 15)
//     }
// }
