use std::{path::Path, fs::{DirEntry, FileType}};

pub(crate) fn read_file(path: &Path) -> String {
    std::fs::read_to_string(path).unwrap_or_else(|e| {
        panic!("failed to read {}: {}", path.display(), e);
    })
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
    let mut entries = Vec::new();
    let iter = std::fs::read_dir(path).unwrap_or_else(|e| {
        panic!("failed to read {}: {}", path.display(), e);
    });
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
