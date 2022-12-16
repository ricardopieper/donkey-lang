use crate::compiler::layouts::Bytes;

pub struct Memory {
    pub mem: memmap::MmapMut,
    pub data_pages: u32,
    pub code_pages: u32,
    pub data_start: Bytes,
    pub code_start: Bytes,
    pub stack_start: Bytes,
    pub heap_start: Bytes,
    pub ready: bool,
}

#[derive(PartialEq, Eq, Debug, Clone)]
#[repr(u8)]
#[allow(dead_code)]
pub enum MemorySegment {
    Reserved = 0,
    Data = 1,
    Code = 2,
    Stack = 3,
    Heap = 4,
}

pub trait NativeNumericType<T> {
    fn from_bytes(data: &[u8]) -> T;
    fn from_exact_bytes(data: [u8; std::mem::size_of::<T>()]) -> T;
    fn to_bytes(&self) -> [u8; std::mem::size_of::<T>()];
}

macro_rules! impl_native_read {
    ($type:ty) => {
        impl NativeNumericType<$type> for $type {
            fn from_bytes(data: &[u8]) -> $type {
                if data.len() == std::mem::size_of::<$type>() {
                    unsafe {
                        <$type>::from_le_bytes(
                            *(data.as_ptr() as *const [u8; std::mem::size_of::<$type>()]),
                        )
                    }
                } else {
                    let mut as_bytes = (0 as $type).to_le_bytes();
                    as_bytes[0..(data.len() as usize)].copy_from_slice(data);
                    <$type>::from_le_bytes(as_bytes)
                }
            }
            fn from_exact_bytes(data: [u8; std::mem::size_of::<$type>()]) -> $type {
                <$type>::from_le_bytes(data)
            }
            fn to_bytes(&self) -> [u8; std::mem::size_of::<$type>()] {
                self.to_le_bytes()
            }
        }
    };
}

impl_native_read!(i8);
impl_native_read!(u8);
impl_native_read!(i16);
impl_native_read!(u16);
impl_native_read!(i32);
impl_native_read!(u32);
impl_native_read!(u64);
impl_native_read!(i64);
impl_native_read!(f64);
impl_native_read!(f32);

impl Memory {
    pub fn new() -> Self {
        let mem = memmap::MmapMut::map_anon(4 * 1024 * 1024 * 1024).unwrap();

        Self {
            mem,
            data_pages: 1,
            code_pages: 1,
            data_start: Bytes(0),
            code_start: Bytes(0),
            stack_start: Bytes(0),
            heap_start: Bytes(0),
            ready: false,
        }
    }

    pub fn make_ready(&mut self) {
        let mut page_start = Bytes(0);

        page_start += 1; //skip reserved page
        self.data_start = page_start << 16;

        page_start += self.data_pages as u32;
        self.code_start = page_start << 16;

        page_start += self.code_pages;
        self.stack_start = page_start << 16;

        //pages used by stack (128)
        let stack_pages = 128u32;
        page_start += stack_pages;
        self.heap_start = page_start << 16;

        self.ready = true;
    }

    pub fn get_ptr_mut<T>(&mut self, offset: Bytes) -> *mut T {
        unsafe { self.mem.as_mut_ptr().offset(offset.0 as isize) as *mut T }
    }

    pub fn get_ptr<T>(&self, offset: Bytes) -> *const T {
        unsafe { self.mem.as_ptr().offset(offset.0 as isize) as *const T }
    }

    pub fn native_read<T: NativeNumericType<T> + Copy>(&self, address: Bytes) -> T {
        unsafe { *self.get_ptr::<T>(address) }
    }

    //returns the data read and a flag indicating whether a page fault occured,
    //and the VM must read the remaining bytes again passing another address
    //and handle that accordingly

    pub fn read_single(&self, address: Bytes) -> u8 {
        self.mem[address.0 as usize]
    }

    pub fn write(&mut self, address: Bytes, data: &[u8]) {
        self.mem[address.0 as usize..address.0 as usize + data.len()].copy_from_slice(data);
    }

    pub fn write_value<T>(&mut self, address: Bytes, data: T) {
        unsafe { *self.get_ptr_mut(address) = data }
    }

    pub fn copy<const LEN: usize>(&mut self, address_from: Bytes, address_to: Bytes) {
        unsafe {
            std::ptr::copy_nonoverlapping::<u8>(
                self.mem.as_ptr().offset(address_from.0 as isize),
                self.mem.as_mut_ptr().offset(address_to.0 as isize),
                LEN,
            );
        }
    }

    pub fn copy_with_len(&mut self, address_from: Bytes, address_to: Bytes, len: Bytes) {
        unsafe {
            std::ptr::copy_nonoverlapping::<u8>(
                self.mem.as_ptr().offset(address_from.0 as isize),
                self.mem.as_mut_ptr().offset(address_to.0 as isize),
                len.0 as usize,
            );
        }
    }
}
