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
//         let a1_1 := g(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
//         let a2_2 := mload(0x0100)
//         let a3_3 := mload(0x0120)
//         let $a4_4 := mload(0x0140)
//         let a5_5 := mload(0x0160)
//         let a6_6 := mload(0x0180)
//         let a7_7 := mload(0x01a0)
//         let a8_8 := mload(0x01c0)
//         let a9_9 := mload(0x01e0)
//         let a10_10 := mload(0x0200)
//         let a11_11 := mload(0x0220)
//         let a12_12 := mload(0x0240)
//         let a13_13 := mload(0x0260)
//         let a14_14 := mload(0x0280)
//         let $a15_15 := mload(0x02a0)
//         mstore(0xa0, $a15_15)
//         mstore(0x80, $a4_4)
//         let a14 := a14_14
//         let a13 := a13_13
//         let a12 := a12_12
//         let a11 := a11_11
//         let a10 := a10_10
//         let a9 := a9_9
//         let a8 := a8_8
//         let a7 := a7_7
//         let a6 := a6_6
//         let a5 := a5_5
//         let a3 := a3_3
//         let a2 := a2_2
//         let a1 := a1_1
//         sstore(0, 1)
//         let a1_16 := g(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
//         let a2_17 := mload(0x0100)
//         let a3_18 := mload(0x0120)
//         let $a4_19 := mload(0x0140)
//         let a5_20 := mload(0x0160)
//         let a6_21 := mload(0x0180)
//         let a7_22 := mload(0x01a0)
//         let a8_23 := mload(0x01c0)
//         let a9_24 := mload(0x01e0)
//         let a10_25 := mload(0x0200)
//         let a11_26 := mload(0x0220)
//         let a12_27 := mload(0x0240)
//         let a13_28 := mload(0x0260)
//         let a14_29 := mload(0x0280)
//         let $a15_30 := mload(0x02a0)
//         mstore(0xa0, $a15_30)
//         mstore(0x80, $a4_19)
//         a14 := a14_29
//         a13 := a13_28
//         a12 := a12_27
//         a11 := a11_26
//         a10 := a10_25
//         a9 := a9_24
//         a8 := a8_23
//         a7 := a7_22
//         a6 := a6_21
//         a5 := a5_20
//         a3 := a3_18
//         a2 := a2_17
//         a1 := a1_16
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
