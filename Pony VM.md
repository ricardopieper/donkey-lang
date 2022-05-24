Pony VM
=======

This file describes an early draft of the Pony VM.

For now it's not gonna be so lightweight, we will have tons of instructions for whatever we need.

The bytecode has no concept of functions, all calls are jumps with a calling convention.

It's stack based, so no general-purpose registers. However, internally the VM will store some internal state, like the instruction pointer, function base address pointer,
stack pointer, and so on as necessary.

Endianness is machine dependent, bytecode produced on a little-endian machine does not run on a big-endian machine. 

Memory is seen as a uniform block of memory, operations that take data from addresses would work on addresses on either stack or heap.

The VM itself might have 2 dedicated allocations for each region, or do whatever it wants, but addresses are just addresses and should
work uniformly.

Pointer arithmetic should work.

We try to assume the least things about the OS or environment we are running with. It would be nice if this VM ran on WASM,
so we will make our own allocatos, and they might not be that efficient or fast.


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

    def square(num: i32, s: i32) -> i32:
        half_val = half(num) + s
        return num * half_val

    def main():
        x : i32 = 15
        y : i32 = 3
        z : i32 = x + y
        result: i32 = square(z, x) + 5
        result = result + y


Prior to calling a function, we have to do some bookkeeping. Stack pointer points to a position where
writes will happen. Therefore reads will read bytes prior to that value, i.e. a read of 4 bytes at 
position 0x04 will read from 0x00 to 0x04.

Some values stored in the stack are reused, so we cannot pop them 


Calling convention:
1 - push argument values to the stack before calling function
2 - call operator stores the return address in the stack as: address of the call operator + 4 bytes
3 - values on the stack after a function call is considered the result


bp = base pointer
sp = stack pointer


call {operand}:
    pushes enough for return value
    pushes a bunch of state (return ip (current + 4), return bp, return sp)
    set bp and sp registers
    jumps to ip

return{size} {operand}
    pops {size} bits
    recover bp and sp registers, pops it
    pushes the 32 bits back
    jumps to return ip

stack offset: 

    - first 8 bytes are calling convention data (return address, SP)
    - 9th byte (index 8) can start being used for function data
    - start uses a 16 bytes stack, so we offset to index 20 from base pointer
    - therefore we do stackoffset 24
    - now stack is:
        [ip, sp, 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00] 


                        -12         -8      -4
start: <function stack: [#0:retaddr, #4:bp, #8:sp], {ip = 0, bp = 12, sp = 12}>, 
0       stackoffset 16          ; stack variable storage (x y z result 4 bytes each, total 16 bytes), new sp = 28
4       push_imm32 15           ; push a value to stack at bp + 16 (24), sp goes to 32
8       storeaddr_rel   bp+0    ; stores data at variable x offset (8), sp back to 28
12      push_imm32 3            ; push a value to stack at bp + 20 (28), sp goes to 32
16      storeaddr_rel32 bp+4    ; stores data at variable y offset (12), sp back to 28
20      loadaddr_rel32  bp+0    ; pushes x to stack, sp goes to 32
24      loadaddr_rel32  bp+4    ; pushes y to stack, sp goes to 36
28      sums32                  ; sums both values on the stack, sp back to 32
32      storeaddr_rel32 bp+8    ; stores the sum (on stack) at variable z offset, sp back to 28
36      loadaddr_rel32  bp+0    ; load x into the stack for the function args (s), sp = 32
40      loadaddr_rel32  bp+8    ; load z into the stack for the function args (num), sp = 36
44      call <square>           ; [.., x, z, (reserved return storage), 48 (return ip), 8 (return bp), 36 (return sp)] {ip = addr(square), bp and sp = 52 (36 + 12 for pushed stack frame + 4 for return value storage)

                            
;half_val = half(num) + s
;num * half_val
square: <function stack: [.., -24:x, -20:z, -16:(reserved return storage), -12:48 (return ip), -8:8 (return bp), -4:32 (return sp)], {ip = addr(square), bp = 52, sp = 52}>
; copy values from previous function stack to this one
xx      loadaddr_rel32  bp-20   ; z var loaded on stack by main, now pushed to this stack at num
xx      loadaddr_rel32  bp-24   ; x var loaded on stack by main, now pushed to this stack at s
xx      stackoffset 12          ; stack variable storage (num, s, half_num 4 bytes each, total 12 bytes), new sp = 64
xx      stackoffset 4           ; reserves storage space for returns
xx      loadaddr_rel32  bp+0    ; loads num, sp = 68 push args
xx      call <half>             ; [(reserved return storage), num, ([xx] return ip), 52 (return bp), 68 (return sp)] {ip = addr(half), bp and sp = 76 (64 + 12 for pushed stack frame)}
;; stack before call:  [num, (reserved return storage), ([xx] return ip), 52 (return bp), 68 (return sp)] 
;; stack after call:   [num, (returned value)]



half: <function stack[.., -20:(reserved return storage), -16:num, -12:([xx] return ip), -8:48(return bp), -4:64 (return sp)], {ip = addr(half), bp = 76, sp = 76}>
xx      loadaddr_rel32  bp-26   ; num var loaded on stack by main, now pushed to this stack at num
xx      stackoffset 4           ; stack variable storage (num)
xx      loadaddr_rel32  bp+0    ; load num in the left hand side
xx      divs_imm32  2           ; divides the number by 2. We could have pushed 2 to the stack before bp+0 and used pure stack instruction, but this is faster
xx      storeaddr32     bp-20   ; pops the result value from the stack and store at the reserved return storage
xx      return                  ; pops the stack frame, recovers the control registers


Opcodes
=======

All instructions are 32 bits in length.
 

There are pseudo-ops, flags, and operands. 
At this point, the bit pattern of each pseudo-operation is not final. The bit layout should be final,
but whether push_imm is 00001 or 00010 is not final, as not all instructions are pushed so far.

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

loadaddr_{mode}{num_bits} {+|-}#{operand}

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

storeaddr_{mode}{num_bytes} #{operand}


Modes:
    -       = pops an address from the stack, and stores to it
    rel     = compute address relative (+/-) to function base pointer. See +/- notation
    imm     = (Immediate) load address given by an absolute address

Operand notation:

    +       = Computes address forward to function base pointer
    -       = Computes address backwards from function base pointer


storeaddr_{n}  0   0   0   1   0   0   0   0   0   0   0   0   0   0   0   0   ....       
              | opr               | nbits | mode | operand
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


{t}shift{n}     0   0   1   0   0   0   0   0   0   0   0   0   0 
              | opr               | nbits |[1] | shift size       | ...
    opt {T}: bit pattern
        0   0   1   0   0  = LEFT SHIFT
        0   0   1   0   1  = RIGHT SHIFT
    
    nbits {n}: bit pattern
        0 0 = 8
        0 1 = 16
        1 0 = 32
        1 1 = 64

    [1] Mode of operation.
        0 = binary operation on stack:
            - pops the first value %1 as a right-hand side of the operator
            - pops the second value %2 as a left hand side of the operator
            - performs %1 << %2 and pushes to stack
        1 = left shift operation with immediate 5 bit (max value)
            - pops the first value %1 as a right-hand side of the operator
            - uses 8 bit value from immediate as %2 
            - performs %1 << %2 and pushes to stack
            - shift size: Size of the shift, 5 bits (max shift size = 63)

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

    binary_{opr}_{imm?}{nbits} {lhs?}

    binary_and32
    binary_xor64
    binary_xor32_imm 0xfefefe



{opr}{nbits}       0   1   0   0   1   0   0   0   0    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
              | opr               |nbits |[1] | left hand side (max 24 bits)
    opr: bit pattern
        0   0   1   1   1  = Bitwise AND
        0   1   0   0   0  = Bitwise OR
        0   1   0   0   1  = Bitwise XOR
    
    nbits {n}: bit pattern
        0 0 = 8
        0 1 = 16
        1 0 = 32
        1 1 = 64

    [1] Mode of operation.
        0 = binary operation on stack:
            - pops the first value %1 as a right-hand side of the operator
            - pops the second value %2 as a left hand side of the operator
            - performs %1 {opt} %2 and pushes to stack
        1 = left shift operation with immediate 24 bit (max value)
            - pops the first value %1 as a right-hand side of the operator
            - uses 24 bit immediate value as %2. If the number of bits > 24, all other bits are interpreted as 0 
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


{opr}{n}       0   1   1   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
              | opr               |nbits |[1] |[2]|immediate unsigned (max 16 bits)                             
    opr: bit pattern
        0   1   0   1   0  = Sum
        0   1   0   1   1  = Subtract
        0   1   1   0   0  = Multiply
        0   1   1   0   1  = Divide
        0   1   1   1   0  = Power
    
    nbits {n}: bit pattern
        0 0 = 8
        0 1 = 16
        1 0 = 32
        1 1 = 64

    [2] Signed or unsigned
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


BINARY INTEGER LOGIC FUNCTIONS

Performs an arithmetic binary operation on integers. Pushes a byte to the stack 0x00 if false, 0x01 if true 

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


{opr}{n}       0   1   1   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
              | opr               |nbits |[1] |[2]|immediate unsigned (max 16 bits)                             
    opr: bit pattern
        0   1   1   1   1  = Equals
        0   1   0   1   1  = Less than
        0   1   1   0   0  = Less than on equals
        0   1   1   0   1  = Greater than
        0   1   1   1   0  = Greater than or equals
    
    nbits {n}: bit pattern
        0 0 = 8
        0 1 = 16
        1 0 = 32
        1 1 = 64

    [2] Signed or unsigned
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


{opr}{n}       1   0   0   1   1   0   0  
              | opr              |nbits  | unused                           
    opr: bit pattern
        0   1   1   1   1  = Float Sum
        1   0   0   0   0  = Float Subtract
        1   0   0   0   1  = Float Multiply
        1   0   0   1   0  = Float Divide
        1   0   0   1   1  = Float Power
    
    nbits {n}: bit pattern
        0 0 = 8
        0 1 = 16
        1 0 = 32
        1 1 = 64


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

{opr}{n}       0   1   1   1   0   0   0   
              | opr               |nbits | unused                         
    opr: bit pattern
        0   1   1   1   1  = Equals
        0   1   0   1   1  = Less than
        0   1   1   0   0  = Less than on equals
        0   1   1   0   1  = Greater than
        0   1   1   1   0  = Greater than or equals
    
    nbits {n}: bit pattern
        1 0 = 32
        1 1 = 64

    [2] Signed or unsigned
        0 = both sides of the operation are unsigned
        1 = both sides of the operation are signed
   [2] Mode of operation.

        1 = left shift operation with immediate LSB 16 bit
            - pops the first value %1 as a left-hand side of the operator
            - uses 16 bit immediate value as LSB %2 right hand side. If the number of bits > 16, all other bits are interpreted as 0. This does not include sign, and it's always > 0
            - performs %1 {opt} %2 and pushes to stack

FLOW CONTROL

These instructions help performing any necessary flow control, including calls, unconditional and conditional jumps, and
also saving and recovering registers.

PUSH REGISTER

Pushes a control register value on the stack. 


POP REGISTER

Pops a value from the stack and saves into a control register.



SPECIAL

    vmcall fcall
        calls an intrinsic function on the virtual machine. Deals with things like printing to the console, expect input, etc.
        fcall is a number from 0 - 2^27 that identifies a function call. This is only known
        to the compiler, but the builtin functions will be documented.
        The same calling convention is expected as regular function calls.
        



```

