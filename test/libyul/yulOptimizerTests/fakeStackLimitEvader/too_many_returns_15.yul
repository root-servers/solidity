{
    {
        mstore(0x40, memoryguard(128))
	let a1,a2,a3,$a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,$a15 := g(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
        sstore(0, 1)
	a1,a2,a3,$a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,$a15 := g(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
    }
    function g(b16, b17, b18, b19, b20, b21, b22, $b23, b24, b25, b26, b27, b28, b29, b30) -> $b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15 {
	$b1 := 1
	b2 := 2
	b15 := add($b23, 15)
    }

}
// ----
// step: fakeStackLimitEvader
//
// {
//     {
//         mstore(0x40, memoryguard(0x02c0))
//         let a1 := g(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
//         let a2 := mload(0x0100)
//         let a3 := mload(0x0120)
//         mstore(0x80, mload(0x0140))
//         let a5 := mload(0x0160)
//         let a6 := mload(0x0180)
//         let a7 := mload(0x01a0)
//         let a8 := mload(0x01c0)
//         let a9 := mload(0x01e0)
//         let a10 := mload(0x0200)
//         let a11 := mload(0x0220)
//         let a12 := mload(0x0240)
//         let a13 := mload(0x0260)
//         let a14 := mload(0x0280)
//         mstore(0xa0, mload(0x02a0))
//         sstore(0, 1)
//         a1 := g(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
//         a2 := mload(0x0100)
//         a3 := mload(0x0120)
//         mstore(0x80, mload(0x0140))
//         a5 := mload(0x0160)
//         a6 := mload(0x0180)
//         a7 := mload(0x01a0)
//         a8 := mload(0x01c0)
//         a9 := mload(0x01e0)
//         a10 := mload(0x0200)
//         a11 := mload(0x0220)
//         a12 := mload(0x0240)
//         a13 := mload(0x0260)
//         a14 := mload(0x0280)
//         mstore(0xa0, mload(0x02a0))
//     }
//     function g(b16, b17, b18, b19, b20, b21, b22, $b23, b24, b25, b26, b27, b28, b29, b30) -> $b1
//     {
//         mstore(0xe0, $b23)
//         mstore(0xc0, 0)
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
//         mstore(0x0260, 0)
//         mstore(0x0280, 0)
//         mstore(0x02a0, 0)
//         mstore(0xc0, 1)
//         mstore(0x0100, 2)
//         mstore(0x02a0, add(mload(0xe0), 15))
//         $b1 := mload(0xc0)
//     }
// }
