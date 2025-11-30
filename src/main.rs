pub mod omeganum;

use std::io::Write;

use rand::random_bool;
use rand::random_range;

use omeganum::OmegaNum;

fn test_single(a : &OmegaNum, b : &OmegaNum) {
    print!("{a} + {b} = ");
    std::io::stdout().flush().ok().unwrap();
    let c = a + b;
    println!("{c}");

    print!("{a} - {b} = ");
    std::io::stdout().flush().ok().unwrap();
    let c = a - b;
    println!("{c}");

    print!("{a} * {b} = ");
    std::io::stdout().flush().ok().unwrap();
    let c = a * b;
    println!("{c}");

    print!("{a} / {b} = ");
    std::io::stdout().flush().ok().unwrap();
    let c = a / b;
    println!("{c}");

    print!("{a} ^ {b} = ");
    std::io::stdout().flush().ok().unwrap();
    let c = a.pow(b);
    println!("{c}");

    print!("{b} √ {a} = ");
    std::io::stdout().flush().ok().unwrap();
    let c = a.root(b);
    println!("{c}");

    print!("{a} ^^ {b} = ");
    std::io::stdout().flush().ok().unwrap();
    let c = a.tetrate(b);
    println!("{c}");
    
    print!("{a} ^^^ {b} = ");
    std::io::stdout().flush().ok().unwrap();
    let c = a.pentate(b);
    println!("{c}");

    print!("{a} {{5}} {b} = ");
    std::io::stdout().flush().ok().unwrap();
    let c = a.arrow(5)(b);
    println!("{c}");
}

fn test_oom(max_pow: u64) {
    let mut a = OmegaNum::new(random_range(0.0..10.0));
    let mut b = OmegaNum::new(random_range(0.0..10.0));

    for i in 0..max_pow {
        for _j in 0..random_range(1..(max_pow + 2)) {
            (a, b) = (a.arrow(i)(&b), b.arrow(i.wrapping_sub(1))(&a));
        }

        if random_bool(0.5) {
            (a, b) = (b, a);
        }
    }

    test_single(&a, &b);
}

fn tests() {
    unsafe { std::env::set_var("RUST_BACKTRACE", "1") }
    for i in 0..7 {
        println!("[TEST] ##{i}");
        test_oom(i);
    }
    println!("[TEST END]");
}


fn main() {
    tests();
}