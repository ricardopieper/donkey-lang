const PAGE_SIZE: usize = 65536;
const NUM_PAGES: usize = 65536;
const PAGE_LAST_INDEX: usize = PAGE_SIZE - 1;
type Page = [u8; PAGE_SIZE];

pub struct Memory {
    pub mem: Vec<Option<Box<[u8; PAGE_SIZE]>>>, //1 page
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
                let mut as_bytes = (0 as $type).to_le_bytes();
                as_bytes[0..(data.len() as usize)].copy_from_slice(data);
                <$type>::from_le_bytes(as_bytes)
            }
            fn from_exact_bytes(data: [u8; std::mem::size_of::<$type>()]) -> $type {
                <$type>::from_le_bytes(data)
            }
            fn to_bytes(&self) -> [u8; std::mem::size_of::<$type>()] {
                self.to_le_bytes().try_into().unwrap()
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
        let mut mem = vec![];

        for _ in 0..NUM_PAGES {
            mem.push(None);
        }

        Self {
            mem,
            code_pages: 1,
            data_pages: 1,
            data_start: 0,
            code_start: 0,
            stack_start: 0,
            heap_start: 0,
            ready: false,
        }
    }

    #[allow(dead_code)]
    pub fn set_section(&mut self, section: &MemorySegment, section_data: &[u8]) {
        //how many pages?
        let len = section_data.len();
        let pages = (len >> 16) + 1;
        for i in 0..pages {
            let start = i * PAGE_SIZE;
            let mut end = ((i + 1) * PAGE_SIZE) - 1;
            let is_last_page = i == pages - 1;
            if is_last_page {
                end = section_data.len();
            }

            let range = &section_data[start..end];
            let mut page: Page = [0; PAGE_SIZE];

            let bytes_so_far = i * PAGE_SIZE;
            let bytes_in_page = section_data.len() - bytes_so_far;

            page[0..bytes_in_page].clone_from_slice(range);

            match section {
                MemorySegment::Reserved => {
                    panic!("Cannot set to the reserved region")
                }
                MemorySegment::Data => {
                    //starts at page 1
                    self.mem[1 + i] = Some(Box::new(page));
                }
                MemorySegment::Code => {
                    //assume data was set before
                    let page_idx = 1 + self.data_pages as usize + i as usize;
                    self.mem[page_idx as usize] = Some(Box::new(page));
                }
                _ => {
                    panic!("Cannot set to stack and heap")
                }
            }
        }
        match section {
            MemorySegment::Data => {
                self.data_pages = pages as u32;
            }
            MemorySegment::Code => {
                self.code_pages = pages as u32;
            }
            _ => {}
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

    //returns the data read and a flag indicating whether a page fault occured,
    //and the VM must read the remaining bytes again passing another address
    //and handle that accordingly
    pub fn read(&self, address: u32, len: u32) -> (&[u8], bool, u32, u32) {
        let index = address & 0x0000_ffff;
        if index + len > PAGE_SIZE as u32 {
            self.read_page_fault(address, len)
        } else {
            self.read_normal(address, len)
        }
    }

    //reads an address-sized value u32 from memory.
    pub fn native_read<T: NativeNumericType<T>>(&self, address: u32) -> T {
        let (read_bytes, _, _, _) = self.read(address, std::mem::size_of::<T>() as u32);
        T::from_bytes(read_bytes)
    }

    //returns the data read and a flag indicating whether a page fault occured,
    //and the VM must read the remaining bytes again passing another address
    //and handle that accordingly
    pub fn read_single(&self, address: u32) -> u8 {
        //which page?
        let page = self.get_page(address);
        assert!(!page.is_none(), "Cannot read from non-allocated page");
        unsafe {
            *page
                .as_ref()
                .unwrap_unchecked()
                .get_unchecked((address & 0x0000_ffff) as usize)
        }
    }

    fn read_page_fault(&self, address: u32, len: u32) -> (&[u8], bool, u32, u32) {
        let page = self.get_page(address).as_ref();

        match page {
            Some(p) => {
                let index = (address & 0x0000_ffff) as usize;
                let remaining = (index + len as usize) - PAGE_SIZE;

                let bytes_on_page_remaining = PAGE_SIZE as u32 - index as u32;
                (
                    &p[index..],
                    true,
                    remaining as u32,
                    address + bytes_on_page_remaining,
                )
            }
            None => {
                panic!("Read from unallocated page")
            }
        }
    }

    fn read_normal(&self, address: u32, len: u32) -> (&[u8], bool, u32, u32) {
        let page = self.get_page(address).as_ref();
        assert!(page.is_some(), "Read from unallocated page {address}");
        let index = (address & 0x0000_ffff) as usize;

        (
            unsafe {
                let ptr = page.as_ref().unwrap().as_ptr();
                let offsetted = ptr.add(index);
                &*std::ptr::slice_from_raw_parts(offsetted, len as usize)
            },
            false,
            0,
            0,
        )
    }

    fn get_page(&self, address: u32) -> &Option<Box<Page>> {
        unsafe {
            //mem is always filled
            let address_page = address >> 16;

            self.mem.get_unchecked(address_page as usize) as _
        }
    }
    
    fn get_page_and_allocate(&mut self, address: u32) -> &mut Page {
        unsafe {
            //mem is always filled
            let address_page = address >> 16;

            let ptr = self.mem.get_unchecked_mut(address_page as usize);
            if ptr.is_none() {
                *ptr = Some(Box::new([0u8; PAGE_SIZE]));
            }
            return ptr.as_mut().unwrap_unchecked();
        }
    }

    pub fn write(&mut self, address: u32, data: &[u8]) {
        //will this write page fault?
        let index = (address & 0x0000_ffff) as usize;
        let end_index = index + data.len();
        if end_index > PAGE_SIZE {
            //stack overflow detection
            let end_address = address + data.len() as u32;
            assert!(!(address >= self.stack_start && end_address >= self.heap_start), "Donkey VM stack overflow detected");

            //first we write to the current page
            let bytes_until_fill_page = PAGE_SIZE - index;
            {
                let bytes_to_write = &data[0..bytes_until_fill_page];
                let seg_page = self.get_page_and_allocate(address);

                let start = index;
                let end = PAGE_LAST_INDEX;

                seg_page[start..=end].copy_from_slice(bytes_to_write);
            }

            //call write again to write the other pages
            let new_starting_address = address + bytes_until_fill_page as u32;
            self.write(new_starting_address, &data[bytes_until_fill_page..]);
        } else {
            //no page faults
            let seg_page = self.get_page_and_allocate(address);
            seg_page[index..end_index].copy_from_slice(data);
        }
    }

    pub fn copy(&mut self, address_from: u32, address_to: u32, len: u32) {
        //will this write page fault?
        let index_from = address_from & 0x0000_ffff;
        let index_to = address_to & 0x0000_ffff;
        let read_size_before_fault_from = PAGE_SIZE as u32 - index_from as u32;
        let read_size_before_fault_to = PAGE_SIZE as u32 - index_to as u32;

        let max_possible_read = read_size_before_fault_from.min(read_size_before_fault_to);

        let read_size = max_possible_read.min(len);

        //SAFETY: simple copy from array to another array, memory is "unmanaged" by design
        //as our language doesn't care about borrowing rules
        let from_page = self.get_page_and_allocate(address_from) as *mut Page;
        let to_page = self.get_page_and_allocate(address_to) as *mut Page;

        let ptr_from = unsafe {
            let deref = (*from_page).as_mut_ptr();
            deref.offset(index_from as isize)
        };
        let ptr_to = unsafe {
            let deref = (*to_page).as_mut_ptr();
            deref.offset(index_to as isize)
        };

        for i in 0..read_size as isize {
            unsafe {
                *ptr_to.offset(i) = *ptr_from.offset(i);
                //       println!("Set byte {to} = {from} ({from_value})", to = address_to + i as u32, from = address_from + i as u32, from_value = *ptr_from.offset(i) );
            }
        }
        if len > max_possible_read {
            let remaining = len - max_possible_read;
            self.copy(
                address_from + read_size,
                address_from + read_size,
                remaining,
            );
        }
    }
}

#[cfg(test)]
mod tests {

    use super::{Memory, MemorySegment};

    #[test]
    pub fn test_write_data_section() {
        let mut mem = Memory::new();
        mem.make_ready();
        mem.set_section(&MemorySegment::Data, &[5; 42]);
        let (read, fault, remaining, _) = mem.read(0x0001_0000, 42);
        assert!(!fault);
        assert_eq!(remaining, 0);
        assert_eq!(read, &[5u8; 42]);
    }

    #[test]
    pub fn test_write_code_section() {
        let mut mem = Memory::new();
        mem.make_ready();
        mem.set_section(&MemorySegment::Code, &[1; 42]);
        let (read, fault, remaining, _) = mem.read(mem.code_start, 42);
        assert!(!fault);
        assert_eq!(remaining, 0);
        assert_eq!(read, &[1u8; 42]);
    }

    #[test]
    pub fn test_write_stack() {
        //
        let mut mem = Memory::new();
        mem.make_ready();
        mem.write(mem.stack_start, &[3u8; 63]);

        let (read, fault, remaining, _) = mem.read(mem.stack_start, 63);
        assert!(!fault);
        assert_eq!(remaining, 0);
        assert_eq!(read, &[3u8; 63]);
    }

    #[test]
    pub fn test_write_mid_page_stack() {
        //
        let mut mem = Memory::new();
        mem.make_ready();
        mem.write(mem.stack_start + 0xf000, &[3u8; 63]);

        let (read, fault, remaining, _) = mem.read(mem.stack_start + 0xf000, 63);
        assert!(!fault);
        assert_eq!(remaining, 0);
        assert_eq!(read, &[3u8; 63]);
    }

    #[test]
    pub fn test_write_with_page_fault_read_all_pages() {
        //
        let mut mem = Memory::new();
        mem.make_ready();
        mem.write(mem.stack_start + 0xfff0, &[3u8; 63]);

        let (read, fault, remaining, next_read) = mem.read(mem.stack_start + 0xfff0, 63);
        assert_eq!(read.len(), 16);
        assert!(fault);
        assert_eq!(remaining, 47);
        assert_eq!(read, &[3u8; 16]);

        let expected_next_read = mem.stack_start + (1 << 16);
        assert_eq!(
            next_read, expected_next_read,
            "next read at {next_read:#x}, expected {expected_next_read:#x}"
        );

        //test next read
        let (read, fault, remaining, next_read) = mem.read(next_read, remaining);
        assert_eq!(read.len(), 47);
        assert!(!fault);
        assert_eq!(remaining, 0);
        assert_eq!(read, &[3u8; 47]);
        assert_eq!(next_read, 0);
    }

    #[test]
    pub fn test_write_stack_many_pages_ahead() {
        //
        let mut mem = Memory::new();
        mem.make_ready();
        //15 pages ahead, total wil be 16 pages (current + 15)
        mem.write(mem.stack_start + 0xf0000, &[8u8; 64]);

        let (read, fault, remaining, next_read) = mem.read(mem.stack_start + 0xf0000, 64);
        assert_eq!(read.len(), 64);
        assert!(!fault);
        assert_eq!(remaining, 0);
        assert_eq!(read, &[8u8; 64]);
        assert_eq!(next_read, 0);
    }

    #[test]
    pub fn write_read_byte_end_stack_page() {
        assert_eq!(0xffff, 65535);

        let mut mem = Memory::new();
        mem.make_ready();

        mem.write(262_143, &[9u8; 1]);

        let (read, fault, remaining, next_read) = mem.read(262_143, 1);
        assert_eq!(read, &[9u8; 1], "failed to get addr 262143");
        assert!(!fault);
        assert_eq!(remaining, 0);
        assert_eq!(next_read, 0);
    }

    #[test]
    #[ignore]
    pub fn read_write_all_stack_bytes() {
        let mut mem = Memory::new();
        mem.make_ready();

        let start = mem.stack_start;
        let end = mem.stack_start + (128 << 16); //advance 128 pages ahead

        for addr in start..end {
            mem.write(addr, &[9u8; 1]);
        }
        time_test!();
        for _ in 0..1 {
            for addr in start..end {
                let (read, ..) = mem.read(addr, 1);
                assert_eq!(read, &[9u8; 1]);
            }
        }
    }

    #[test]
    #[ignore]
    pub fn read_write_all_stack_bytes_read_single() {
        let mut mem = Memory::new();
        mem.make_ready();

        let start = mem.stack_start;
        let end = mem.stack_start + (128 << 16); //advance 128 pages ahead

        for addr in start..end {
            mem.write(addr, &[9u8; 1]);
        }
        time_test!();
        for _ in 0..1 {
            for addr in start..end {
                let read = mem.read_single(addr);
                assert_eq!(read, 9);
            }
        }
    }

    #[test]
    #[ignore]
    pub fn baseline() {
        let mut array = vec![0u8; 8 * 1024 * 1024];
        let start = 0;
        let end = array.len() - 1; //advance 128 pages ahead

        let ptr = array.as_mut_ptr();

        for addr in start..end {
            let offset = unsafe { ptr.add(addr) };
            unsafe { *offset = 9 };
        }

        time_test!();
        for _ in 0..1 {
            for addr in start..end {
                let read = unsafe { *ptr.add(addr) };
                assert_eq!(read, 9u8);
            }
        }
    }
}
