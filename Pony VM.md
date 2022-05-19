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

All of this lives on the heap on the real underlying operating system. This is a virtual machine for a reason.

Of course, the bytecode file format would only have the data and code sections stored in it.

While this is machine-independent, we try to emulate what a computer would do to a certain extent.

So yes, it is entirely possible for the code to read its own bytecode and print it. We probably want to protect it from writes though... but it would be "fun" to have self-modifying code.

The data section is modifiable, but cannot grow. Notice it doesn't change the executable on-disk file, since the data from disk is copied to RAM.


Opcodes
=======

These are the opcodes:

Data Manipulation
-----------------

```
CONSTANT PUSH
cpush8  %1    pushes 1 byte to the stack (value given in the operand)
cpush16 %1    pushes 2 bytes to the stack  (value given in the operand)
cpush32 %1    pushes 4 bytes to the stack  (value given in the operand)
cpush64 %1    pushes 8 bytes to the stack  (value given in the operand)

LOAD PUSH
lpush8   %1    loads 1 byte from the operand address and pushes to stack
lpush16  %1    loads 2 bytes from the operand address and pushes to stack
lpush32  %1    loads 4 bytes from the operand address and pushes to stack
lpush64  %1    loads 8 bytes from the operand address and pushes to stack

```

Math Binary Operators
----

These instructions pop 2 values of appropriate sizes from the stack, and pushes 1 value to the stack.

This is the pattern for operations on integers:

[operator][s|u]i[size]
[s|u] means signed/unsigned

Size is in bits, 8,16,32,64.

For floats it's simpler: [operator]f[size]

```
MULTIPLICATION

pop * pop

Signed
mulsi8           multiplies signed 1-byte integer values
mulsi16          multiplies signed 2-byte integer values
mulsi32          multiplies signed 4-byte integer values
mulsi64          multiplies signed 8-byte integer values

Unsigned
mului8           multiplies unsigned 1-byte integer values
mului16          multiplies unsigned 2-byte integer values
mului32          multiplies unsigned 4-byte integer values
mului64          multiplies unsigned 8-byte integer values

Float
mulf64          multiplies 4-byte float values
mulf32          multiplies 8-byte float values


SUM

pop + pop

Signed
sumsi8           sums signed 1-byte integer values
sumsi16          sums signed 2-byte integer values
sumsi32          sums signed 4-byte integer values
sumsi64          sums signed 8-byte integer values

Unsigned
sumui8           sums unsigned 1-byte integer values
sumui16          sums unsigned 2-byte integer values
sumui32          sums unsigned 4-byte integer values
sumui64          sums unsigned 8-byte integer values

Float
sumf64           sums 4-byte float values
sumf32           sums 8-byte float values


SUBTRACT

pop - pop

Signed
subsi8           subtracts signed 1-byte integer values
subi16           subtracts signed 2-byte integer values
subi32           subtracts signed 4-byte integer values
subi64           subtracts signed 8-byte integer values

Unsigned
subui8           subtracts unsigned 1-byte integer values
subui16          subtracts unsigned 2-byte integer values
subui32          subtracts unsigned 4-byte integer values
subui64          subtracts unsigned 8-byte integer values

Float
subf64           subtracts 4-byte float values
subf32           subtracts 8-byte float values


DIVIDE

pop / pop
Integer divisions will truncate! 3/2 = 1

Signed
divsi8           divides signed 1-byte integer values
divi16           divides signed 2-byte integer values
divi32           divides signed 4-byte integer values
divi64           divides signed 8-byte integer values

Unsigned
divui8           divides unsigned 1-byte integer values
divui16          divides unsigned 2-byte integer values
divui32          divides unsigned 4-byte integer values
divui64          divides unsigned 8-byte integer values

Float
divf64           divides 4-byte float values
divf32           divides 8-byte float values


MODULUS

pop % pop

Signed
modsi8           modulus signed 1-byte integer values
modi16           modulus signed 2-byte integer values
modi32           modulus signed 4-byte integer values
modi64           modulus signed 8-byte integer values

Unsigned
modui8           modulus unsigned 1-byte integer values
modui16          modulus unsigned 2-byte integer values
modui32          modulus unsigned 4-byte integer values
modui64          modulus unsigned 8-byte integer values


POWERS

pop ^ pop

Signed
powsi8           power of signed 1-byte integer values
powi16           power of signed 2-byte integer values
powi32           power of signed 4-byte integer values
powi64           power of signed 8-byte integer values

Unsigned
powui8           power of unsigned 1-byte integer values
powui16          power of unsigned 2-byte integer values
powui32          power of unsigned 4-byte integer values
powui64          power of unsigned 8-byte integer values

Float
powf64           power of 4-byte float values
powf32           power of 8-byte float values


```

Math functions
--------------

```

SQUARE ROOT

Signed
sqrtsi8         square root of signed 1-byte integer value
sqrtsi16        square root of signed 2-byte integer value
sqrtsi32        square root of signed 4-byte integer value
sqrtsi64        square root of signed 8-byte integer value

Unsigned
sqrtui8         square root of unsigned 1-byte integer value
sqrtui16        square root of unsigned 2-byte integer value
sqrtui32        square root of unsigned 4-byte integer value
sqrtui64        square root of unsigned 8-byte integer value

Float
sqrtf32        square root of 4-byte float value
sqrtf64        square root of 8-byte float value

The pattern follows for:
sin
cos
tan


```


Data conversion operators
--------------------------


```


```