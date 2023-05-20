use std::process::Command;

pub enum LinkerError {
    GenericLinkerError(String),
}

pub fn link(obj: &str, out: &str) -> Result<(), LinkerError> {
    if cfg!(target_os = "macos") && cfg!(target_arch = "aarch64") {
        let output = Command::new("ld")
            .arg(obj)
            .arg("-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib")
            .arg("-execute")
            .arg("-lSystem")
            .arg("-o")
            .arg(out)
            .output()
            .expect("failed to execute process");
        if output.status.success() {
            Ok(())
        } else {
            let stderr = String::from_utf8(output.stderr).unwrap();
            Err(LinkerError::GenericLinkerError(stderr))
        }
    } else {
        todo!("Implement link command here")
    }
}
