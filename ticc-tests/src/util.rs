use std::{path::Path, fs::{DirEntry, FileType, ReadDir}};

pub(crate) fn read_text_file(path: &Path) -> String {
    let contents = std::fs::read_to_string(path).unwrap_or_else(|e| {
        panic!("failed to read {}: {}", path.display(), e);
    });
    contents.replace("\r\n", "\n")
}

pub(crate) fn filename(path: &Path) -> &str {
    let Some(name) = path.file_name() else {
        panic!("path {} has no name", path.display());
    };
    let Some(str) = name.to_str() else {
        panic!("path {} cannot be named as a string", path.display());
    };
    str
}

pub(crate) fn read_dir(path: &Path) -> Vec<(FileType, DirEntry)> {
    let iter = std::fs::read_dir(path).unwrap_or_else(|e| {
        panic!("failed to read {}: {}", path.display(), e);
    });
    read_dir_inner(path, iter)
}

pub(crate) fn try_read_dir(path: &Path) -> Vec<(FileType, DirEntry)> {
    match std::fs::read_dir(path) {
        Ok(iter) => read_dir_inner(path, iter),
        Err(e) => {
            if e.kind() == std::io::ErrorKind::NotFound {
                Vec::new()
            } else {
                panic!("failed to read {}: {}", path.display(), e);
            }
        }
    }
}

fn read_dir_inner(path: &Path, iter: ReadDir) -> Vec<(FileType, DirEntry)> {
    let mut entries = Vec::new();
    for entry in iter {
        match entry {
            Ok(entry) => {
                let ty = match entry.file_type() {
                    Ok(ty) => ty,
                    Err(e) => panic!("failed to read {}: {}", entry.path().display(), e),
                };
                entries.push((ty, entry));
            }
            Err(e) => panic!("failed to read {}: {}", path.display(), e),
        }
    }
    entries
}

pub(crate) fn write_file(path: &Path, value: &[u8]) {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(&parent).unwrap_or_else(|e| {
            panic!("failed to create {}: {}", parent.display(), e);
        });
    }
    std::fs::write(path, value).unwrap_or_else(|e| {
        panic!("failed to write {}: {}", path.display(), e);
    });
}
