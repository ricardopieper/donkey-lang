use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::mem;

#[derive(PartialOrd, PartialEq, Debug, Copy, Clone)]
pub struct Float(pub f64);

impl From<f64> for Float {
    fn from(w: f64) -> Float {
        Float(w)
    }
}

impl Eq for Float {}

impl Ord for Float {
    fn cmp(&self, other: &Float) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Hash for Float {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        //until I find another way to store floats in a map key, this unsafe is necessary :(
        //update: not really necessary to use unsafe, but the alternative is far worse
        let as_u64: u64 = unsafe { mem::transmute(self.0) };
        return as_u64.hash(state);
    }
}
