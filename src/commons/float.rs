use std::hash::Hash;

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct Float(pub f64);

impl Hash for Float {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
    }
}

impl From<f64> for Float {
    fn from(w: f64) -> Float {
        Float(w)
    }
}

impl Eq for Float {}
