Donkey VM
========

This file describes an early draft of the Donkey VM.

It's stack based, so no general-purpose registers. However, internally the VM will store some internal state, like the instruction pointer, function base address pointer, stack pointer, and so on as necessary. They will be acessible in the bytecode and can be changed, though it's not so straightforward.

Endianness is machine dependent, bytecode produced on a little-endian machine does not run on a big-endian machine. 

Memory is seen as a uniform block of memory, operations that take data from addresses would work on addresses on either stack or heap.

The VM itself might have 2 dedicated allocations for each region, or do whatever it wants, but addresses are just addresses and shouldwork uniformly.

Pointer arithmetic should work.

We try to assume the least things about the OS or environment we are running with. It would be nice if Donkey ran on WASM, so we will make our own allocatos, and they might not be that efficient or fast.


Memory Management
=================

The VM will hold all the memory, we won`t use malloc or other OS-dependent calls.

But we can simulate it more or less!

We can break apart memory into pages, say 64kb pages of memory, so that we allocate memory in chunks as needed
Addresses are 4 bytes, 32 bits (u32).
The first 2 bytes (16 bits) are used to indicate the page, and the other 2 bytes are the offset into a page.

For instance:

    0x00000000 = very first byte of all memory, page 0
    0x0000ffff = very last byte of memory on page 0. This is byte #65536
    0x00010000 = byte #65537 on page 1

This allows pointer arithmetic and allows a scheme where we can choose the size of the page in powers of 2.
We have 4 bytes, 32 bits:

    0b 00000000 00000000 00000000 00000000
        0x00      0x00    0x00     0x00

To make it easier to picture, we can use each half-byte:

    0b 0000 0000 0000 0000 0000 0000 0000 0000
       0x0  0x0  0x0  0x0  0x0  0x0  0x0  0x0

Each half-byte can represent 16 (0xf = 15) possible states, 16 possible addresses (from 0).

So we can have the following page sizes, and total number of pages:

    2 ^ 4  = 16b   (268m pages)
    2 ^ 8  = 256b  (16m pages)
    2 ^ 12 = 4kb   (1m pages)
    2 ^ 16 = 64kb  (65k pages) 
    2 ^ 20 = 1mb   (4k pages)
    2 ^ 24 = 16mb  (256 pages)
    2 ^ 28 = 256mb (16 pages)
    2 ^ 32 = 4gb   (1 page)

So depending on how memory efficient we want to be, we could choose different page sizes for the VM.
I think for the modern era (circa 2022) it's fine to have 65535 64kb pages. In fact, WASM does the same.

This scheme allows us to even increase the memmory by using a 64 bit address space.

Stack vs Heap
-------------

In a real process running on the OS, the space is divided between stack and heap, and there are some allocation schemes to manage them differently. 

A process memory layout is more or less like this:

```
[data][code]{heap~~~||}[stack]
                    ˆˆ
                    brk
```

The OS provides virtual memory, i.e. you don't know where in actual physical memory something is being allocated. The OS translates addresses to the physical ones. On UNIX, the process can use mmap and brk syscalls to allocate more memory. AFAIK BRK is an older way to manage heap space (it`s like a bar you can move up and down), while mmap is used to allocate memory somewhere, which is then mapped onto the process address space.

So there is this idea of a "translation" from virtual to physical.... so it isn't too absurd if we do it too. Every memory access might require a branch though :( Lets just roll with it for now.

Therefore, we can have the following memory layout:

    [blank][data][code][stack][heap]

     Blank: 1 page "thrown away". Some special things could live here, ex: 
        - nullptr address (0x00000000)
        - read/write/exec for each sub-address space (code, data, stack, heap)
        - address of beginning of the code section
     Data: Fixed size determined during compilation, 1 to d+1 page (as needed, d is size of data section in pages)
     Code: Fixed size determined during compilation, d+2 to d+2+c pages (as needed, c is size of code section in pages)
    Stack: Pages d+2+c+1 to d+2+c+129 (128 pages, a nice 8mb chunk of data)
     Heap: pages c+3+d+128+1 to 65535 (all the rest)

Stack grows from a low address to a high address, and so does the heap and all other memory spaces.

All of this lives on the heap on the real underlying operating system. This is a virtual machine for a reason.

Of course, the bytecode file format would only have the data and code sections stored in it.

While this is machine-independent, we try to emulate what a computer would do to a certain extent.

So yes, it is entirely possible for the code to read its own bytecode and print it. We probably want to protect it from writes though... but it would be "fun" to have self-modifying code.


Calling functions
=================

Suppose we want to compile the following code:


    def half(num: i32)->i32:
        return num / 2

    def some_function(num: i32, s: i32) -> i32:
        half_val = half(num) + s
        return num * half_val

    def main():
        x : i32 = 15
        y : i32 = 3
        z : i32 = x + y
        result: i32 = 5 + some_function(z, x)
        result = result + y


Prior to calling a function, we have to do some bookkeeping. Stack pointer points to a position where
writes will happen. Therefore reads will read bytes prior to that value, i.e. a read of 4 bytes at 
position 0x04 will read from 0x00 to 0x04.

Some values stored in the stack are reused, so we cannot pop them 


Calling convention:
1 - push argument values to the stack before calling function
2 - call operation stores the return address in the stack as: ${address of the call operator + 4 bytes}
3 - values on the stack after a function call is considered the result


bp = base pointer
sp = stack pointer


Call {operand}:
    pushes current ip + 4 to the stack (so that the return falls on the next instruction)
    sets ip = operand
    sets bp = sp (observer will see sp +4 due to the IP push which moved the stack pointer)

Return:
    pops from stack
    uses popped value as ip
    jumps to that ip

When a call is made, the stack looks like: 
    - function return value (reserved) (pushed by the caller itself)
    - function arguments (pushed by the caller itself)
    - return bp (pushed by the caller itself)
    - return ip (pushed by the call instruction)

When a function gets called:
    - copies arguments to the current function scope (pass by value, copied)
    - reserves N bytes for function scope with push_imm instruction
    - then the rest of the bytes are used for temp values for binary operations

When a function finishes:
    - return value is assumed to be at the top of the stack
    - Pops this value, and stores it at the function return value (reserved)  address
    - destroy local function storage (stackoffset 0)
    - return (pops ip from stack and jumps to it)
    - caller pops bp from stack with pop_reg bp
    - pop all pushed arguments
    - continue execution



The resulting bytecode is:

```

    
main: <function stack: [], {ip = 0, bp = 0, sp = 0}>, 
;; no arguments and nowhere to return
00      stackoffset     16      ; move 16 bytes upwards in preparation to store values here
04      push_imm32      15      ; loads 15 in memory
08      storeaddr_rel32 bp+0    ; sets at variable 
12      push_imm32      3       ; loads 3 in memory
16      storeaddr_rel32 bp+4    ; sets at variable 
;; at this point we have stack: [x, y, z, result], sp=16
20      loadaddr_rel32  bp+0    ; load x
24      loadaddr_rel32  bp+4    ; load y
28      sums32                  ; x + y
32      storeaddr_rel32 bp+8    ; store result at z
;; at this point we have stack: [x, y, z, result], sp=16
36      push_imm32      5       ; pushes 5 in preparation for binary op +, sp = 20
40      push_imm32      0       ; reserve space for return argument, sp = 24
44      loadaddr_rel32  bp+8    ; loads argument z, sp = 28
48      loadaddr_rel32  bp+0    ; loads argument x, sp = 32
52      push_reg        bp      ; save our bp to recover later, sp = 36
56      call <some_function>    ; moves ip to some_function, sets bp = sp + 4 (40), pushes a return instruction pointer (60)
;;stack before entering: [x, y, z, result, 5, return space, z, x, bp(0), ip(60) ] {ip = 96, bp = 40}
;;stack after call: [x, y, z, result, 5, return space, z, x, bp(0)] {ip = 60, bp = 40, sp = 36}
60      pop_reg         bp      ; recovers the bp {sp = 32, bp = 0}
;;stack now: [x, y, z, result, 5, return space, z, x] {ip = 64 bp = 0, sp = 32}
64      pop32                   ; pops the x argument
68      pop32                   ; pops the z argument
;;stack now: [x, y, z, result, 5, return space] {ip = 72 bp = 0 sp = 24}
72      sums32                  ; sums 5 + some_function(z, x)
;;stack now: [x, y, z, result, (5 + some_function(z, x))] {ip = 76 bp = 0 sp = 20}
76      storeaddr_rel32 bp+12   ; stores it in the result variable
;;stack now: [x, y, z, result] {ip = 80 bp = 0 sp = 16}
80      loadaddr_rel32  bp+12   ; loads it again  sp = 20
84      loadaddr_rel32  bp+4    ; loads y sp = 24
88      sums32                  ; sums result + y sp = 20
92      storeaddr_rel32 bp+12   ; stores at result, sp = 16


some_function: <function stack: [..,-20:(reserved return storage), -16:z, -12:x, -8:0(return bp), -4:60(return ip)], {ip = 96, bp = 40, sp = 40}>
; copy values from previous function stack to this one
96       loadaddr_rel32  bp-16   ; z var loaded on stack by main, now pushed to this stack at num
100      loadaddr_rel32  bp-12   ; x var loaded on stack by main, now pushed to this stack at s
104      stackoffset     12      ; stack variable storage (num, s, half_num, 4 bytes each, total 12 bytes), new sp = 52 (40 + 12)
108      push_imm32       0      ; reserves storage space for <half> return value, new sp = 56.
112      loadaddr_rel32  bp+0    ; loads the argument num for the function <half>, new sp = 60
116      push_reg        bp      ; stores our base pointer on the stack (36), new sp = 64
120      call <half>             ; moves ip to half, sets bp = sp + 4 (68) and pushes a return instruction pointer 124
;; stack before call:  [.., (reserved return storage), (arg1), 40 (return bp), 124(return ip)] { ip = 168, bp = 68, sp = 68}
;; stack after  call:  [.., (half result), (arg1), 40 (return bp)] { ip = 124, bp = 68, sp = 64}
124      pop_reg         bp      ; restores the bp value to 40, pops stack
;; stack is now [.., (half result), (arg1)] { ip = 128, sp = 60}
128      pop32                   ; pops the arg1 argument
;; stack is now [.., (half result)] { ip = 132, sp = 56}
132      loadaddr_rel32  bp+4    ; loads s to the stack in preparation for half(num) + s
;; stack is now [.., (half result), (s)] { ip = 136, sp = 60}
136      sums32                  ; sums both 32bit values, signed, new sp = 56
;; stack is now [.., (half result + s)] sp=56
140      storeaddr_rel32 bp+8    ; stores the result of half(num) + s at half_val, new sp = 52 
;; stack is now [..,]
144      loadaddr_rel32 bp+0     ; loads the variable num on the stack new sp = 56
148      loadaddr_rel32 bp+8     ; loads the variable half_val on the stack, new sp = 60
152      muls32                  ; multiplies both values (num * half_val), new sp = 56
;; stack is now [..,-20:(reserved return storage), -16:x, -12:z, -8:0(return bp), -4:60(return ip), +0:(num), +4:(s), +8:(half_num), +12:(num * half_val)]
156      storeaddr_rel32 bp-20   ; pops the result value from the stack and store at the reserved return storage, new sp = 52
;; stack is now [..,-20:(now set return storage), -16:x, -12:z, -8:(return bp), -4:(return ip), +0:(num), +4:(s), +8:(half_num)]
160      stackoffset     0       ; destroy the function local storage, maybe unecessary, new sp = 40
164      return                  ; pops ip, returns to it, new sp = 36


half: <function stack[.., -16:(reserved return storage), -12:num, -8:36(return bp), -4:124(return ip)], {ip = 168, bp = 68, sp = 68}>
168      loadaddr_rel32  bp-12   ; num var loaded on stack by caller, now pushed to this stack at num, new sp = 72
172      stackoffset     4       ; storage for num variable, sp still 68 because stackoffset is relative on bp. Not really needed, can be optimized out
176      loadaddr_rel32  bp+0    ; load num in the left hand side, new sp = 76
180      divs_imm32      2       ; divides the number by 2, sp is still 76. We could have pushed 2 to the stack before bp+0 and used pure stack instruction, but this is faster
184      storeaddr_rel32 bp-16   ; pops the result value from the stack and store at the reserved return storage, new sp = 72
188      stackoffset     0       ; destroy the function local storage, maybe unecessary, new sp = 68
192      return                  ; pops ip, returns to it, new sp = 64

```


Opcodes
=======

All instructions are 32 bits in length.
All instructions increment the instruction pointer by 1 unless explicitly documented otherwise.

There are pseudo-ops, flags, and operands. 
At this point, the bit pattern of each pseudo-operation is not final. The bit layout should be final,
but whether push_imm is 00001 or 00010 is not final, as not all instructions are described so far.

First 5 bits indicate the pseudo-operation.
Each operation may have a number of flags.
[n] indicates what the bit range means when it doesn't fit the space in the bit layout, read the [n] comment for further info.
```

PUSH IMMMEDIATE
Pushes immediate value to stack. Does not pop any values.

Syntax:

    push_imm{num_bits} {<< left shift size} #{immediate lsb 16bit} 

push_imm{n}    0   0   0   0   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0| ...
              | opr               |nbits| lshift |immediate least significant bits (up to 16)  
    Pushes an immediate {n} bits to the stack.          
    nbits {n}: bit pattern
        0 0 = 8
        0 1 = 16
        1 0 = 32
        1 1 = 64
    lshift: bit pattern
        0 0 =  imm << 0 (no shift)
        0 1 =  imm << 16
        1 0 =  imm << 32
        1 1 =  imm << 48


By using the left shift, you can choose whether the push immediate sets the most or least significant bits.

To push values higher than 65535 (i.e. a 32-bit value 1248612943),
do the following:
    1 - break the 16 MSB and 16 LSB portions of the value apart
    2 - pushimm32 <<16 #{hsb 16 bits}
    3 - sumimm32 #{lsb 16 bits}


The equivalent C code of this entire process is:    

    int i = 1248612943;
    short hsb = (i >> 16); //crush 16 lower bits and get only those bits
    short lsb = (i << 16) >> 16; //crush 16 higher bits and return them to original position to get only those bits
    
    int reconstructed = 0;
    reconstructed = ((int)msb << 16);
    reconstructed += (int)lsb;

For 64 bit values, the process is essentially the same. You can push to the stack 2 32-bit values for the LSB and MSB of the 64 bits (in that order),
shift the top stack value by << 32, and then sum64 the 2 top stack values.
You can also store it in the data portion of the program (which you know the address), then use loadaddr64 to load it, but this might be a bit slower.



LOAD FROM ADDRESS

Pushes a value to the stack by loading it from a given address.

Syntax:

loadaddr_{mode}{num_bits} bp{+|-}#{operand}

Modes:
    -       = pops an address from the stack, and loads from it
    rel     = compute address relative (+/-) to function base pointer. See +/- notation
    imm     = (Immediate) load address given by an absolute address

Operand notation:

    +       = Computes address forward to function base pointer
    -       = Computes address backwards from function base pointer

loadaddr_{n}   0   0   0   1   0   0   0   0   0   0   0   0   0   0   0   0   ....       
              | opr               | nbits | mode | operand (23 bits)
    loads {n} bits from an adress and pushes to the stack.
    nbits {n}: bit pattern
        0 0 = 8
        0 1 = 16
        1 0 = 32
        1 1 = 64
    mode: bit pattern
        0 0 = pops an address from the stack, and loads from it
            operand is unused
        0 1 = (rel) compute address relative forward to function base pointer
        1 0 = (rel) compute address relative backward to function base pointer
            On both modes, operand is the offset from function base pointer (23 bits). Max value is 16777215 (16MB), which is larger than default stack size (8MB)
        1 1 = (imm) load address given by an absolute address
            operand is the raw address, 23 bits. Max value is 16777215 (16MB), which might cover most of the average program data section. 

STORE TO ADDRESS

Pops a value from the stack and stores it into an address.

Syntax:

storeaddr_{mode}{num_bytes} bp{+|-}#{operand}


Modes:
    -       = pops an address from the stack, and stores to it. Pops the value and then the address.
    rel     = compute address relative (+/-) to function base pointer. See +/- notation
    imm     = (Immediate) load address given by an absolute address

Operand notation:

    +       = Computes address forward to function base pointer
    -       = Computes address backwards from function base pointer


storeaddr_{n}  0   0   0   1   1   0   0   0   0   0   0   0   0   0   0   0   ....       
              | opr               | nbits | mode | operand
    loads {n} bits from an adress and pushes to the stack.
    nbits {n}: bit pattern
        0 0 = 8
        0 1 = 16
        1 0 = 32
        1 1 = 64
    mode: bit pattern
        0 0 = pops a value from the stack, then the address, and stores value in address
            operand is unused
        0 1 = (rel) compute address relative forward to function base pointer
        1 0 = (rel) compute address relative backward to function base pointer
            On both modes, operand is the offset from function base pointer (23 bits). Max value is 16777215 (16MB), which is larger than default stack size (8MB)
        1 1 = (imm) store at address given by an absolute address
            operand is the raw address, 23 bits. Max value is 16777215 (16MB), which might cover most of the average program data section. 

    On stack mode (0 0), the memory will look like this:
    ... | 0 1 0 1 0 1 0 1 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 | 1 0 1 0 1 0 0 0 |
          ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ   ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
                                    address                                     value


BIT SHIFT

Performs a bit shift operation, left or right
Syntax:

{l|r}shift{_imm?}{num_bits} #{shift size?}:
   l|r: left or right shift
   imm?: shift size will be given in the immediate {shift size} instead of popped from stack
   {num_bits}: 8, 16, 32 or 64
   {shift size}: the shift size immediate, optional

Examples:

    rshift_imm32 5
    lshift_imm64 63
    rshift32
    lshift8
    lshift16


{d}shift{n}     0   0   1   0   0   0   0   0   0   0   0   0   0   0   0 
              | opr               | nbits |[1]|[2]|[3]|shift size       | ...
    opr: bit pattern
    nbits {n}: bit pattern
        0 0 = 8
        0 1 = 16
        1 0 = 32
        1 1 = 64
    [1] Direction {d}
        0 = left
        1 = right
    [2] Mode of operation.
        0 = binary operation on stack:
            - pops the first value %1 as a right-hand side of the operator
            - pops the second value %2 as a left hand side of the operator
            - performs %1 << %2 and pushes to stack
        1 = shift operation with immediate 5 bit (max value)
            - pops the first value %1 as a right-hand side of the operator
            - uses 8 bit value from immediate as %2 
            - performs %1 (<< or >>) %2 and pushes to stack
            - shift size: Size of the shift, 5 bits (max shift size = 63)
    [3] Keep sign bit
        0 = Don't keep
        1 = Keep


For bit shifts with constant values (that coud be given by 2 immediates), maybe the compiler should perform constant folding on it instead of
executing it on the VM.


BITWISE OPERATORS

Performs a bitwise operation.
The sign bit has no meaning, every bit is taken as-is.
The right hand side of the operator is always popped from the stack.
The left hand side is optionally taken from the stack, but it can also be an immediate, max 24 bits.

Operations can be:

    - Bitwise AND
    - Bitwise OR
    - Bitwise XOR

Syntax:

    {opr}{k?}_{imm?}{nbits} {lhs?}

    andk_imm32
    xor64
    xor_imm32 0xfefefe



{opr}{nbits}    0   0   1   0   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
              | opr               | nbits | binop |[1]|[2] | left hand side (max 22 bits)
    opr: bit pattern
    
    nbits {n}: bit pattern
        0 0 = 8
        0 1 = 16
        1 0 = 32
        1 1 = 64
    binop: bit pattern
        0 0 = bitwise AND
        0 1 = bitwise OR
        1 0 = bitwise XOR

    [1] keep sign flag
        0 = not keep
        1 = keep

    [2] Mode of operation.
        0 = binary operation on stack:
            - pops the first value %1 as a right-hand side of the operator
            - pops the second value %2 as a left hand side of the operator
            - performs %1 {opt} %2 and pushes to stack
        1 = left shift operation with immediate 21 bit (max value)
            - pops the first value %1 as a right-hand side of the operator
            - uses 21 bit immediate value as %2.
            - performs %1 {opt} %2 and pushes to stack




ARITHMETIC INTEGER BINARY OPERATIONS

Performs an arithmetic binary operation on integers.

Operations can be:

    - sum: Sum
    - sub: Subtract
    - mul: Multiply
    - div: Divide
    - pow: Power
  
Both operands can be signed or unsigned, but they must match. You cannot do a signed-with-unsigned operation or vice-versa.

Therefore you can do a subtract operation by pushing a negative right-hand-side value and then using the sum signed operation.

Syntax:

{opr}{s|u}{m}{n} #{immediate}:

Examples:

sums_imm32: Sums signed values in immediate mode, 32 bits
mulu64: Sums unsigned values popped from stack, 64 bits


{opr}{n}       0   0   1   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
              | opr               |nbits |operation  |[1]|[2]|immediate unsigned (max 16 bits)
    
    opr: bit pattern
    
    nbits {n}: bit pattern
        0 0 = 8
        0 1 = 16
        1 0 = 32
        1 1 = 64

    operation: bit pattern
        0  0  0 = Sum
        0  0  1 = Subtract
        0  1  0 = Multiply
        0  1  1 = Divide
        1  0  0 = Power

    [1] Signed or unsigned
        0 = both sides of the operation are unsigned
        1 = both sides of the operation are signed
    [2] Mode of operation.
        0 = binary operation on stack:
            - pops the first value %2 as a right-hand side of the operator
            - pops the second value %1 as a left hand side of the operator
            - performs %1 {opt} %2 and pushes to stack
        1 = left shift operation with immediate LSB 16 bit
            - pops the first value %1 as a left-hand side of the operator
            - uses 16 bit immediate value as LSB %2 right hand side. If the number of bits > 16, all other bits are interpreted as 0. This does not include sign, and it's always > 0
            - performs %1 {opt} %2 and pushes to stack

For bit shifts with constant values (that coud be given by 2 immediates), maybe the compiler should perform constant folding on it.
The most you can do is use mode 1 and push %1 to the stack with a pushimm if it fits in the instruction.


BINARY INTEGER COMPARE FUNCTIONS

Performs a compare operation on integers. Pushes a byte to the stack 0x00 if false, 0x01 if true 

Operations can be:

    - eq: Equals
    - ne: Not Equals
    - lt: Less than
    - le: Less than or equals
    - gt: Greater than
    - ge: Greather than or equals
  

Both operands can be signed or unsigned, but they must match. You cannot do a signed-with-unsigned operation or vice-versa.

Syntax:

{opr}{s|u}{m}{n} #{immediate}:

Examples:

eqs_imm32: Compare if 2 signed integer values are equal, taking an immediate 16 bits
neu32: Compare if 2 unsigned integer values are NOT equal, taking both values from the stack.


{opr}{n}       0   0   1   1   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
              | opr               |nbits |operation  |[1] |[2]|immediate unsigned (max 16 bits)                             
    opr: bit pattern
    
    nbits {n}: bit pattern
        0 0 = 8
        0 1 = 16
        1 0 = 32
        1 1 = 64

    operation: bit pattern
        0  0  0 = Equals
        0  0  1 = Not equals
        0  1  0 = Less than
        0  1  1 = Less than or equals
        1  0  0 = Greater than
        1  0  1 = Greater than or equals

    [1] Signed or unsigned
        0 = both sides of the operation are unsigned
        1 = both sides of the operation are signed
    [2] Mode of operation.
        0 = binary operation on stack:
            - pops the first value %2 as a right-hand side of the operator
            - pops the second value %1 as a left hand side of the operator
            - performs %1 {opt} %2 and pushes to stack
        1 = left shift operation with immediate LSB 16 bit
            - pops the first value %1 as a left-hand side of the operator
            - uses 16 bit immediate value as LSB %2 right hand side. If the number of bits > 16, all other bits are interpreted as 0. This does not include sign, and it's always > 0
            - performs %1 {opt} %2 and pushes to stack

ARITHMETIC FLOAT BINARY OPERATIONS

Performs an arithmetic binary operation on floats.

Operations can be:

    - fsum: Sum
    - fsub: Subtract
    - fmul: Multiply
    - fdiv: Divide
    - fpow: Power
  

Syntax:

f{opr}{n}

These operations work as follows:
    - pops the first value %1 as a right-hand side of the operator
    - pops the second value %2 as a left hand side of the operator
    - performs %1 {opr} %2 and pushes to stack

This operation never works with immediate values. Values must be pushed to the stack.


{opr}{n}       0   1   0   0   0   0   0   0   0   0
              | opr              |nbits  | operation |unused                           
    opr: bit pattern

    nbits {n}: bit pattern
        0 = 32
        1 = 64

    operation: bit pattern
        0  0  0  = Float Sum
        0  0  1  = Float Subtract
        0  1  0  = Float Multiply
        0  1  1  = Float Divide
        1  0  0  = Float Power
        

BINARY FLOAT LOGIC FUNCTIONS

Performs an arithmetic binary operation on floats. Pushes a byte to the stack 0x00 if false, 0x01 if true 

Operations can be:

    - eq: Equals
    - ne: Not Equals
    - lt: Less than
    - le: Less than or equals
    - gt: Greater than
    - ge: Greather than or equals

Syntax:

f{opr}{n}:

Examples:

feq32: Compare if 2 floats are equal
fne64: Compare if 2 floats are NOT equal

Mode of operation:
    - pops the first value %2 as a right-hand side of the operator
    - pops the second value %1 as a left hand side of the operator
    - performs %1 {opt} %2 and pushes to stack

{opr}{n}       0   1   0   0   1   0   0   0   0   0  ...  
              | opr              |nbits |operation  | unused                         
    opr: bit pattern    
    nbits {n}: bit pattern
        1 0 = 32
        1 1 = 64
    operation: bit pattern
        0  0  0 = Equals
        0  0  1 = Not equals
        0  1  0 = Less than
        0  1  1 = Less than or equals
        1  0  0 = Greater than
        1  0  1 = Greater than or equals
        1  1  0 = And
        1  1  1 = Or


FLOW CONTROL

These instructions help performing any necessary flow control, including calls, unconditional and conditional jumps, and
also saving and recovering registers.

PUSH REGISTER

Pushes a control register value on the stack. 
Size pushed is determined by the size of the control register, but in general they are 32 bits.

push_reg     0   1   0   1   0   0   0   
            | opr              |reg    |                    
    opr: bit pattern
    reg: bit pattern
        0 0 = bp (function stack base pointer)
        0 1 = sp (stack pointer)
        1 0 = ip (instruction pointer) 



POP REGISTER

Pops a value from the stack and saves into a control register.
Size popped is determined by the size of the control register, but in general they are 32 bits.
If a value is popped to the instruction pointer, then the IP is not automatically incremented.

pop_reg       0   1   0   1   1   0   0   
            | opr               |reg    |                    
    opr: bit pattern
    reg: bit pattern
        0 0 = bp (function stack base pointer)
        0 1 = sp (stack pointer)
        1 0 = ip (instruction pointer) 

POP

Pops a number of bits from the stack.

Syntax:
pop32
pop8
pop64

{opr}{n}       0   1   1   0   0   0   0   ..
              | opr              |nbits  | unused                           
    opr: bit pattern
    nbits {n}: bit pattern
        0 0 = 8
        0 1 = 16
        1 0 = 32
        1 1 = 64


STACK OFFSET

Sets the stack pointer N bytes away from the base pointer

Syntax:
stackoffset #{num bytes}

{opr}{n}       0   1   1   0   1   0   0   ..
              | opr              |num bytes                         
    opr: bit pattern
    num bytes {n}: immediate (max 27 bits)

CALL

Pushes current IP + 4 to stack and changes IP to the operand provided, or from stack
Sets BP = SP + 4

Syntax:
call #{offset} (offset from operand)
call_stack (pops operand from stack)

{opr}{n}       0   1   1   1   0   0   0   ..
              | opr              |src| operand bytes                         
    opr: bit pattern
    src: bit pattern
      0 = offset from operand
      1 = offset popped from stack
    operand: instruction offset (max 26 bits, can address 128mb of code)


RETURN

Pops from stack, sets popped value as new IP
If popped value is u32::MAX, then the program exits.

Syntax:
return

{opr}{n}       0   1   1   1   1      ..
              | opr               | unused                        
    opr: bit pattern


JUMP IF ZERO

Pops value from stack, and if the value is zero, jumps to offset from 0. Offset can be either 
popped from stack or given by an immediate.


Syntax:
jz_stack (pops a value from stack and jumps to it)
jz #{label}

{opr}{n}       1   0   0   0   0   0   0      ..
              | opr              |src |operand                       
    src: bit pattern
      0 = offset from operand
      1 = offset popped from stack
    operand: instruction offset (max 26 bits, can address 128mb of code)

JUMP IF NOT ZERO

Pops value from stack, and if the value is not zero, jumps to offset from 0. Offset can be either 
popped from stack or given by an immediate.


Syntax:
jnz_stack (pops a value from stack and jumps to it)
jnz #{label}

{opr}{n}       1   0   0   0   1   0   0      ..
              | opr              |src |operand                       
    src: bit pattern
      0 = offset from operand
      1 = offset popped from stack
    operand: instruction offset (max 26 bits, can address 128mb of code)

JUMP UNCONDITIONAL

Jumps unconditionally. Offset can be either 
popped from stack or given by an immediate.


Syntax:
jmp_stack (pops a value from stack and jumps to it)
jmp #{label}

{opr}{n}       1   0   0   1   0   0   0      ..
              | opr              |src |operand                       
    src: bit pattern
      0 = offset from operand
      1 = offset popped from stack
    operand: instruction offset (max 26 bits, can address 128mb of code)


SPECIAL

    vmcall fcall
        calls an intrinsic function on the virtual machine. Deals with things like printing to the console, expect input, etc.
        fcall is a number from 0 - 2^27 that identifies a function call. This is only known
        to the compiler, but the builtin functions will be documented.
        The same calling convention is expected as regular function calls.
        



```

