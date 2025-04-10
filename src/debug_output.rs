use std::sync::{Mutex, MutexGuard};

static OUTPUT: Mutex<Output> = Mutex::new(Output::new());

pub struct Output {
    output: Vec<String>,
    capture: bool
}

impl Output {
    pub const fn new() -> Self {
        Output {
            output: Vec::new(),
            capture: false
        }
    }

    fn get() -> MutexGuard<'static, Output> {
        OUTPUT.lock().unwrap()
    }

    pub fn enable_capture(enable: bool) {
        Self::get().capture = enable;
    }

    pub fn clear() {
        Self::get().output.clear();
    }

    pub fn println(output: impl Into<String>) {
        if Self::get().capture {
            Self::get().output.push(output.into());
        } else {
            println!("{}", output.into());
        }
    }

    pub fn flush() {
        let output = &mut Self::get().output;
        for line in output.iter() {
            println!("{}", line);
        }
        output.clear();
    }
}