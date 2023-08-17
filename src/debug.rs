macro_rules! loc {
    () => {
        concat!(file!(), ":", line!())
    };
}

macro_rules! log {
    ($($arg:tt)*) => {
        println!("[{}]: {}", loc!(), format!($($arg)*))
    };
}
