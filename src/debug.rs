
macro_rules! loc {
    () => {
        concat!(file!(), ":", line!())
    };
}
