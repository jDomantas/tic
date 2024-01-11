// TODO: make field not pub
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Addr(u64);

pub struct Heap {
    bytes: HeapBytes,
    move_dst: Vec<Addr>,
    bump: usize,
}

impl Heap {
    pub fn new() -> Heap {
        let mut bytes = AlignedBytes::default();
        bytes.ensure_size(256);
        let bump = bytes.len();
        Heap {
            move_dst: std::iter::repeat(Addr(u64::MAX)).take(bytes.len() / Header::SIZE as usize).collect(),
            bytes: HeapBytes { raw: bytes },
            bump,
        }
    }

    pub fn alloc_bytes(&mut self, tag: u64, bytes: u32) -> BytesAlloc<'_> {
        let (addr, _, bytes) = self.raw_alloc(tag, bytes);
        BytesAlloc {
            addr,
            bytes,
        }
    }

    pub fn alloc_object(&mut self, tag: u64, fields: u32) -> ObjAlloc<'_> {
        let (addr, header, bytes) = self.raw_alloc(tag, fields * 8);
        ObjAlloc {
            addr,
            header,
            fields: bytes_to_words_mut(bytes),
        }
    }

    fn raw_alloc(&mut self, tag: u64, bytes: u32) -> (Addr, &mut Header, &mut [u8]) {
        let bytes = round_up(bytes);
        let total_size = (Header::SIZE + bytes) as usize;
        if self.bump < total_size {
            panic!("not enough space left to allocate (capacity = {}, free = {}, request = {})", self.bytes.raw.len(), self.bump, total_size);
        }
        self.bump -= total_size;
        let addr = Addr(self.bump as u64);
        *self.bytes.obj_header(addr) = Header {
            size: total_size as u32,
            mark: 0,
            ptrs: 0,
            tag,
        };
        let (header, bytes) = self.bytes.obj_parts_mut(addr);
        // println!("alloc, addr = {}, total size = {total_size}", addr.0);
        (addr, header, bytes)
    }

    pub fn collect_and_grow<'a>(&mut self, roots: impl Iterator<Item = &'a mut Addr>, ensure_free: u32) {
        self.collect_inner(roots, ensure_free);
    }

    pub fn collect<'a>(&mut self, roots: impl Iterator<Item = &'a mut Addr>) {
        self.collect_inner(roots, 0);
    }

    fn collect_inner<'a>(&mut self, roots: impl Iterator<Item = &'a mut Addr>, ensure_free: u32) {
        let roots = roots.collect::<Vec<_>>();
        // println!("roots: {}", roots.len());
        let mut pending = Vec::new();
        let mut retained = Vec::new();
        let mut retained_bytes = 0;
        for &&mut r in &roots {
            // println!("root: {}", r.0);
            self.bytes.obj_header(r).mark = 1;
        }
        let mut current = Addr(self.bump as u64);
        while current.0 < self.bytes.raw.len() as u64 {
            let (header, bytes) = self.bytes.obj_parts_mut(current);
            if header.mark == 0 {
                // println!("scan object at {} - not marked", current.0);
                current.0 += u64::from(header.size);
                continue;
            }
            // println!("scan object at {} - marked", current.0);
            header.mark = 0;
            retained.push(current);
            retained_bytes += header.size;
            current.0 += u64::from(header.size);
            if header.ptrs != 0 {
                let ptrs = header.ptrs;
                let fields = bytes_to_words(bytes);
                pending.extend(fields.iter().copied());
                for (i, field) in pending.drain(..).enumerate() {
                    if ptrs & (1 << i) != 0 {
                        self.bytes.obj_header(Addr(field)).mark = 1;
                    }
                }
            }
        }
        let min_capacity = (retained_bytes as usize * 4)
            .max(self.bytes.raw.len())
            .max((ensure_free + retained_bytes) as usize);
        if min_capacity > self.bytes.raw.len() {
            // println!("heap grew to {min_capacity}");
            self.bytes.raw.ensure_size(min_capacity);
            self.move_dst.resize(min_capacity / (Header::SIZE as usize), Addr(0));
        }
        // println!("gc retained {} objects", retained.len());
        let mut new_bump = self.bytes.raw.len();
        for addr in retained.into_iter().rev() {
            // println!("checking retained object at {}", addr.0);
            let size = self.bytes.obj_header(addr).size;
            let place_at = new_bump - size as usize;
            let new_addr = Addr(place_at as u64);
            // println!("obj {} moved to {}", addr.0, new_addr.0);
            new_bump = place_at;
            self.bytes.raw.copy_within((addr.0 as usize)..((addr.0 + u64::from(size)) as usize), place_at);
            self.move_dst[(addr.0 / u64::from(Header::SIZE)) as usize] = new_addr;
            let (header, fields) = self.bytes.obj_fields_mut(new_addr);
            if header.ptrs != 0 {
                for (i, ptr) in fields.into_iter().enumerate() {
                    if header.ptrs & (1 << i) != 0 {
                        *ptr = self.move_dst[(*ptr / u64::from(Header::SIZE)) as usize].0;
                    }
                }
            }
        }
        for ptr in roots {
            *ptr = self.move_dst[(ptr.0 / u64::from(Header::SIZE)) as usize];
        }
        self.bump = new_bump;
        // println!("free space after gc: {} / {} (collected {})", self.bump, self.bytes.raw.len(), initial_size - self.allocated());
    }

    pub fn obj_with_fields(&self, addr: Addr) -> (u64, impl Iterator<Item = crate::rt::Value> + '_) {
        let (header, fields) = self.bytes.obj_fields(addr);
        let ptrs = header.ptrs;
        let fields = fields
            .iter()
            .copied()
            .enumerate()
            .map(move |(i, v)| if ptrs & (1 << i) != 0 {
                crate::rt::Value::Ptr(Addr(v))
            } else {
                crate::rt::Value::Int(v)
            });
        (header.tag, fields)
    }

    pub fn obj_with_bytes(&self, addr: Addr) -> (u64, &[u8]) {
        let (header, bytes) = self.bytes.obj_parts(addr);
        (header.tag, bytes)
    }

    pub fn allocated(&self) -> usize {
        self.bytes.raw.len() - self.bump
    }

    pub fn capacity(&self) -> usize {
        self.bytes.raw.len()
    }

    pub fn free_space(&self) -> usize {
        self.capacity() - self.allocated()
    }
}

struct HeapBytes {
    raw: AlignedBytes,
}

impl HeapBytes {
    fn obj_parts_mut(&mut self, addr: Addr) -> (&mut Header, &mut [u8]) {
        let bytes = &mut self.raw[addr.0 as usize..];
        let (header, rest) = bytes.split_at_mut(Header::SIZE as usize);
        let header = unsafe { &mut *(header.as_mut_ptr() as *mut Header) };
        let bytes = &mut rest[..((header.size - Header::SIZE) as usize)];
        (header, bytes)
    }

    fn obj_header(&mut self, addr: Addr) -> &mut Header {
        let bytes = &mut self.raw[addr.0 as usize..][..(Header::SIZE as usize)];
        let header = unsafe { &mut *(bytes.as_mut_ptr() as *mut Header) };
        header
    }

    fn obj_fields_mut(&mut self, addr: Addr) -> (&mut Header, &mut [u64]) {
        let (header, bytes) = self.obj_parts_mut(addr);
        (header, bytes_to_words_mut(bytes))
    }
    
    fn obj_parts(&self, addr: Addr) -> (&Header, &[u8]) {
        let bytes = &self.raw[addr.0 as usize..];
        let (header, rest) = bytes.split_at(Header::SIZE as usize);
        let header = unsafe { &*(header.as_ptr() as *const Header) };
        let bytes = &rest[..((header.size - Header::SIZE) as usize)];
        (header, bytes)
    }

    fn obj_fields(&self, addr: Addr) -> (&Header, &[u64]) {
        let (header, bytes) = self.obj_parts(addr);
        (header, bytes_to_words(bytes))
    }
}

fn bytes_to_words(bytes: &[u8]) -> &[u64] {
    unsafe {
        std::slice::from_raw_parts(bytes.as_ptr() as *const u64, bytes.len() / 8)
    }
}

fn bytes_to_words_mut(bytes: &mut [u8]) -> &mut [u64] {
    unsafe {
        std::slice::from_raw_parts_mut(bytes.as_ptr() as *mut u64, bytes.len() / 8)
    }
}

fn round_up(bytes: u32) -> u32 {
    (bytes + 7) / 8 * 8
}

pub struct ObjAlloc<'a> {
    pub addr: Addr,
    header: &'a mut Header,
    fields: &'a mut [u64],
}

impl ObjAlloc<'_> {
    pub fn put_ptr(&mut self, idx: usize, addr: Addr) {
        self.fields[idx] = addr.0;
        self.header.ptrs |= 1 << idx;
    }

    pub fn put_int(&mut self, idx: usize, value: u64) {
        self.fields[idx] = value;
    }
}

pub struct BytesAlloc<'a> {
    pub addr: Addr,
    pub bytes: &'a mut [u8],
}

#[repr(C)]
struct Header {
    size: u32,
    mark: u32,
    ptrs: u64,
    tag: u64,
}

impl Header {
    const SIZE: u32 = std::mem::size_of::<Header>() as u32;

    const _ASSERT_ALIGNED: () = assert!(Self::SIZE % 8 == 0);

    // does not have to be true, just a note to know what the size is
    const _ASSERT_SIZE: () = assert!(Self::SIZE == 24);
}

#[derive(Default)]
struct AlignedBytes {
    raw: Vec<u64>,
}

impl AlignedBytes {
    pub fn len(&self) -> usize {
        self.raw.len() * 8
    }

    pub fn as_slice(&self) -> &[u8] {
        let ptr = self.raw.as_ptr();
        let len = self.len();
        unsafe { std::slice::from_raw_parts(ptr.cast::<u8>(), len) }
    }

    pub fn as_slice_mut(&mut self) -> &mut [u8] {
        let ptr = self.raw.as_mut_ptr();
        let len = self.len();
        unsafe { std::slice::from_raw_parts_mut(ptr.cast::<u8>(), len) }
    }

    pub fn ensure_size(&mut self, new_size: usize) {
        let need = (new_size + 7) / 8;
        if self.raw.len() < need {
            self.raw.resize(need, 0);
        }
    }
}

impl std::ops::Deref for AlignedBytes {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl std::ops::DerefMut for AlignedBytes {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_slice_mut()
    }
}
