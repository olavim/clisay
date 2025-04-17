use std::cell::RefCell;
use std::sync::Mutex;

static CAPTURE: Mutex<bool> = Mutex::new(false);

thread_local! {
    static OUTPUT: RefCell<Vec<String>> = RefCell::new(Vec::new());
}

pub struct Output {}

impl Output {
    pub const fn new() -> Self {
        Output {}
    }

    pub fn enable_capture(enable: bool) {
        *CAPTURE.lock().unwrap() = enable;
    }

    pub fn clear() {
        OUTPUT.with(|out| out.borrow_mut().clear());
    }

    pub fn println(value: impl Into<String>) {
        OUTPUT.with(|out| {
            if *CAPTURE.lock().unwrap() {
                out.borrow_mut().push(value.into());
            } else {
                println!("{}", value.into());
            }
        });
    }

    pub fn flush() {
        OUTPUT.with(|out| {
            let output = &mut out.borrow_mut();
            for line in output.iter() {
                println!("{}", line);
            }
            output.clear();
        });
    }

    pub fn get_output() -> Vec<String> {
        OUTPUT.with(|out| out.borrow().clone())
    }
}