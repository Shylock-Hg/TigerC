use tiger_c::data_layout;

pub enum Layout {
    String(usize),
}

impl Layout {
    pub fn size(&self) -> usize {
        match self {
            Layout::String(l) => data_layout::STRING_HEADER_SIZE + l + 1, // 1 for null
        }
    }
}

pub const MAX_ALIGN: usize = 16; // max align for general allocation in x86_64

pub fn allocate(layout: &Layout) -> i64 {
    unsafe {
        std::alloc::alloc(std::alloc::Layout::from_size_align(layout.size(), MAX_ALIGN).unwrap())
            as i64
    }
}
