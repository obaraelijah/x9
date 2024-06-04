#[derive(Debug)]
pub(crate) struct FileRecord {
    path: String,
    file: Option<std::fs::File>,
}

impl Default for FileRecord {
    fn default() -> Self {
        Self { 
            path: "<no path>".to_string(), 
            file: None, 
        }
    }
}

impl FileRecord {
    fn new(f: std::fs::File, path: String) -> FileRecord {
        FileRecord {  
            file: Some(f),
            path,
        }
    }
}