use std::hash::Hash;

#[derive(Debug, Copy, Clone)]
pub struct FloatLiteral(pub f64);

impl Hash for FloatLiteral {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
    }
}

impl From<f64> for FloatLiteral {
    fn from(w: f64) -> FloatLiteral {
        FloatLiteral(w)
    }
}

impl PartialEq for FloatLiteral {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl Eq for FloatLiteral {}
