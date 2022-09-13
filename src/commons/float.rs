use std::hash::Hash;

#[derive(PartialEq, Hash, Debug, Copy, Clone)]
pub struct Float(pub u64);

impl From<f64> for Float {
    fn from(w: f64) -> Float {
        Float(w.to_bits())
    }
}

impl Eq for Float {}
