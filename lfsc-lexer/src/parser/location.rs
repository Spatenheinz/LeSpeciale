#[derive(Debug, PartialEq, Clone)]
pub struct Location {
    pub col: usize,
    pub row: usize,
}

impl Location {
    pub fn new(col: usize, row: usize) -> Self {
        Self { col, row }
    }
    pub const fn empty() -> Self {
        Self {
            col: 0,
            row: 0,
        }
    }
}
