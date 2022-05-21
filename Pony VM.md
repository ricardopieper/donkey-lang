Pony VM
=======

This file describes an early draft of the Pony VM.

For now it's not gonna be so lightweight, we will have tons of instructions for whatever we need.

The bytecode has no concept of functions, all calls are jumps with a calling convention.

It's stack based, so no registers.
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
````

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

The data section is modifiable, but cannot grow. Notice it doesn't change the executable on-disk file, since the data from disk is copied to RAM.


Opcodes
=======

All instructions are 32 bits in length.

There are pseudo-ops, flags, and operands.
First 5 bits indicate the pseudo-operation.
Each operation may have a number of flags.
[n] indicates what the bit range means, read the [n] comment for further info.

```

IMMPUSH
Pushes immediate value to stack. Does not pop any values.

Syntax:

    immpush{num_bytes} {bytes} {optional left shift}

immpush8       0   0   0   0   1   0   0   0   0   0   0   0   0   0   0   ...   
              | opr              |*8bits |          immediate  8 bits    | unused
    pushes an immediate 8 bits to the stack.          

immpush16      0   0   0   0   1   0   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   ...  
              | opr              |*16bits|         immediate  16 bits                                    | unused
    pushes an immediate 16 bits to the stack.

immpush32      0   0   0   0   1   0   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   ...   
              | opr              |*16bits|         immediate  16 bits LSB                                |*ls|   
    pushes an immediate 32 bits to the stack. The bits in the immediate are used as the least significant portion of the value.
    *ls: if 1, shifts all bits to the left 16 bits (imm << 16). If 0, no shift is done.

immpush64      0   0   0   0   1   1   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   ...   
              | opr              |*64bits|         immediate  16 bits LSB                                | *ls   | unused
    pushes an immediate 64 bits to the stack. The bits in the immediate are used as the least significant portion of the value.
    *ls: if bit pattern = 0 0, no shift
    *ls: if bit pattern = 0 1, shifts all bits to the left 16 bits (imm << 16)
    *ls: if bit pattern = 1 0, shifts all bits to the left 32 bits (imm << 16)
    *ls: if bit pattern = 1 1, shifts all bits to the left 48 bits (imm << 16)


ADDRESS PUSH

Pushes N bits from an adress popped from the stack. 
To push a const value at an adress known at compile time, first you use immpush to generate the address. 
For other things it shouldn't be necessary.

Syntax:

addrpush{num_bytes}

addrpush8      0   0   0   1   0   0   0   ...   
              | opr              |*8bits | unused
    loads 8 bits from an adress and pushes to the stack.

addrpush16     0   0   0   1   0   0   1   ...
              | opr              |*8bits | unused
    loads 16 bits from an adress and pushes to the stack. 

addrpush32     0   0   0   1   0   1   0   ...
              | opr              |*8bits | unused
    loads 32 bits from an adress and pushes to the stack. 

addrpush64    0   0   0   1   0   1   1   ...   
              | opr              |*8bits | unused
    loads 64 bits from an adress and pushes to the stack.   

    
BINARY SHIFT

Performs a bit shift operation

                                                                         
{t}shift{n}     0   0   0   1   1   0   0   0   0   0   0   0   0   0
              | opr               | nbits |[1] | shift size           | ...
    opt {T}: bit pattern
        0   0   0   1   1  = LEFT SHIFT
        0   0   1   0   0  = RIGHT SHIFT
    
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
            - shift size: Size of the shift, 6 bits (max shift size = 63)




```

