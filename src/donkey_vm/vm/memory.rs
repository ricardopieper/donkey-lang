
pub struct Memory {
    pub mem: memmap::MmapMut,
    pub data_pages: u32,
    pub code_pages: u32,
    pub data_start: u32,
    pub code_start: u32,
    pub stack_start: u32,
    pub heap_start: u32,
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
            data_start: 0,
            code_start: 0,
            stack_start: 0,
            heap_start: 0,
            ready: false,
        }
    }

    pub fn make_ready(&mut self) {
        let mut page_start = 0u32;

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

    pub fn get_ptr_mut<T>(&mut self, offset: isize) -> *mut T {
        unsafe {
            self.mem.as_mut_ptr().offset(offset) as *mut T
        }
    }

    pub fn get_ptr<T>(&self, offset: isize) -> *const T {
        unsafe {
            self.mem.as_ptr().offset(offset) as *const T
        }
    }


    //returns the data read and a flag indicating whether a page fault occured,
    //and the VM must read the remaining bytes again passing another address
    //and handle that accordingly
    pub fn read(&self, address: u32, len: u32) -> &[u8] {
        &self.mem[address as usize..address as usize + len as usize]
    }

    //reads an address-sized value u32 from memory.
    pub fn native_read<T: NativeNumericType<T> + Copy>(&self, address: u32) -> T {
        unsafe { *self.get_ptr::<T>(address as isize) }
    }

    //returns the data read and a flag indicating whether a page fault occured,
    //and the VM must read the remaining bytes again passing another address
    //and handle that accordingly

    pub fn read_single(&self, address: u32) -> u8 {
        self.mem[address as usize]
    }

    pub fn write(&mut self, address: u32, data: &[u8]) {
        self.mem[address as usize..address as usize + data.len()].copy_from_slice(data);
    }

    pub fn write_value<T>(&mut self, address: u32, data: T) {
        unsafe { *self.get_ptr_mut(address as isize) = data }
    }

    pub fn copy<const LEN: usize>(&mut self, address_from: u32, address_to: u32) {
        unsafe { 
            std::ptr::copy_nonoverlapping::<u8>(
            self.mem.as_ptr().offset(address_from as isize),
            self.mem.as_mut_ptr().offset(address_to as isize),
            LEN);
        }
    }
}
