use omeganum_rs::omeganum::OmegaNum;

#[test]
fn integration_nan_inf_and_arithmetic() {
    let nan = OmegaNum::new(f64::NAN);
    assert!(nan.isnan());

    let inf = OmegaNum::new(f64::INFINITY);
    assert!(inf.isinf());

    let one = OmegaNum::new(1.0);
    let sum = &one + &nan;
    assert!(sum.isnan());

    let prod = &inf * &one;
    assert!(prod.isinf());
}

#[test]
fn integration_comparisons_and_large() {
    let small = OmegaNum::new(1e6);
    let large = OmegaNum::new(10.0).pow(&OmegaNum::new(100.0));
    // large may be represented as Infinity internally, but it should compare greater
    assert!(large > small);
}

#[test]
fn integration_tetration_small_values() {
    let two = OmegaNum::new(2.0);
    // 2^^0 == 1
    let t0 = two.tetrate(&OmegaNum::new(0.0));
    assert_eq!(t0.to_number(), 1.0);

    // 2^^1 == 2
    let t1 = two.tetrate(&OmegaNum::new(1.0));
    assert_eq!(t1.to_number(), 2.0);

    // 2^^2 == 4
    let t2 = two.tetrate(&OmegaNum::new(2.0));
    assert_eq!(t2.to_number(), 4.0);

    // 2^^3 == 16
    let t3 = two.tetrate(&OmegaNum::new(3.0));
    assert_eq!(t3.to_number(), 16.0);
}

#[test]
fn integration_pow_root_log_inverse_small() {
    let a = OmegaNum::new(3.0);
    let b = OmegaNum::new(4.0);
    let p = a.pow(&b);
    if p.to_number().is_finite() {
        let r = p.root(&b);
        let diff = (r.to_number() - a.to_number()).abs();
        assert!(diff < 1e-10, "pow/root inverse failed: diff={}", diff);
    } else {
        // If it's infinite, at least detect infinity
        assert!(p.isinf());
    }

    // log10 and pow inverse with base 10
    let x = OmegaNum::new(12345.678);
    let restored = OmegaNum::new(10.0).pow(&x.log10());
    if restored.to_number().is_finite() {
        let diff = (restored.to_number() - x.to_number()).abs();
        assert!(diff < 1e-10, "10^log10(x) inverse failed: diff={}", diff);
    } else {
        assert!(restored.isinf());
    }
}

#[test]
fn integration_parse_and_invalid() {
    // valid parses
    let p = OmegaNum::parse("123.45".to_owned()).expect("parse should succeed");
    assert_eq!(p.to_number(), 123.45);

    let s = OmegaNum::parse("1e3".to_owned()).expect("parse should succeed");
    assert_eq!(s.to_number(), 1000.0);

    // invalid parse
    assert!(OmegaNum::parse("not a number".to_owned()).is_none());
}

#[test]
fn integration_max_arrow_mutex() {
    // set and reset should work (mutex should not be poisoned in normal tests)
    OmegaNum::set_max_arrow(5).expect("set_max_arrow failed");
    assert_eq!(OmegaNum::get_max_arrow().expect("get_max_arrow failed"), 5);
    OmegaNum::reset_max_arrow();
    assert_eq!(OmegaNum::get_max_arrow().expect("get_max_arrow failed"), 1000);
}
