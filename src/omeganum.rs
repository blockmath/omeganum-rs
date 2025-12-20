use std::ops;
use std::ops::*;
use std::cmp::*;

/*
use std::f64::NAN;
use std::f64::INFINITY;
use std::f64::NEG_INFINITY;
*/
use std::sync::PoisonError;

use regex::Regex;
use regex::RegexSet;
use std::sync::LazyLock;
use std::sync::Mutex;

/// Class for extremely large numbers, up to 10{1000}10
/// 
/// The maximum arrow operator level defaults to 1000, but can be configured up to u64::MAX - 2

#[derive(Debug, Clone)]
pub struct OmegaNum {
    array: Vec<f64>,
    sign: i8,
}

const DEBUG_PARSE : bool = false;

const MAX_ARROW_DEFAULT : i64 = 1000;
static MAX_ARROW: Mutex<i64> = Mutex::new(MAX_ARROW_DEFAULT);
const MAX_SAFE_INTEGER : f64 = 9007199254740991.0;

pub static MAX_E: LazyLock<f64> = LazyLock::new(|| f64::log10(MAX_SAFE_INTEGER));
pub static E_MAX_SAFE_INTEGER: LazyLock<OmegaNum> = LazyLock::new(|| OmegaNum { array: vec![MAX_SAFE_INTEGER, 1.0], sign: 1 });
pub static EE_MAX_SAFE_INTEGER: LazyLock<OmegaNum> = LazyLock::new(|| OmegaNum { array: vec![MAX_SAFE_INTEGER, 2.0], sign: 1 });
pub static TETRATED_MAX_SAFE_INTEGER: LazyLock<OmegaNum> = LazyLock::new(|| OmegaNum { array: vec![1.0, MAX_SAFE_INTEGER], sign: 1 });
pub static PENTATED_MAX_SAFE_INTEGER: LazyLock<OmegaNum> = LazyLock::new(|| OmegaNum { array: vec![1.0, 0.0, MAX_SAFE_INTEGER], sign: 1 });


impl OmegaNum {
    pub fn sign(&self) -> i8 { self.sign }

    pub fn isnan(&self) -> bool {
        match self.array.first() {
            None => true,
            Some(x) => x.is_nan()
        }
    }

    pub fn isinf(&self) -> bool {
        match self.array.first() {
            None => false,
            Some(x) => x.is_infinite()
        }
    }

    pub fn isint(&self) -> bool {
        match self.array.first() {
            None => false,
            Some(x) => x.fract() == 0.0
        }
    }

    pub fn normalize(&mut self) {
        if self.array.is_empty() {
            self.array = vec![0.0];
        }

        if self.sign != 1 && self.sign != -1 {
            self.sign = if self.sign < 0 { -1 } else { 1 };
        }

        let mut i = 0;
        while i < self.array.len() {
            let e = self.array[i];

            if e.is_nan() {
                self.array = vec![f64::NAN];
                return
            }

            if !e.is_finite() {
                self.array = vec![f64::INFINITY];
                return
            }

            if i != 0 && e.fract() != 0.0 {
                self.array[i] = e.floor();
            }

            i += 1;
        }

        let mut changed;
        loop {
            changed = false;

            // Remove trailing zeros
            while self.array.last() == Some(&0.0) {
                self.array.pop();
                changed = true;
            }

            // First element overflow
            if !self.array.is_empty() && self.array[0] > MAX_SAFE_INTEGER {
                if self.array.len() <= 1 {
                    self.array.push(0.0);
                }
                self.array[1] += 1.0;
                self.array[0] = self.array[0].log10();
                changed = true;
            }

            // Normalize exponent ladder
            while self.array.len() > 1 && self.array[0] < *MAX_E && self.array[1] > 0.0 {
                self.array[0] = 10f64.powf(self.array[0]);
                self.array[1] -= 1.0;
                changed = true;
            }

            // Collapse higher levels
            if self.array.len() > 2 && self.array[1] == 0.0 {
                let mut idx = 2;
                while idx < self.array.len() && self.array[idx] == 0.0 {
                    idx += 1;
                }
                if idx < self.array.len() {
                    self.array[idx - 1] = self.array[0];
                    self.array[0] = 1.0;
                    self.array[idx] -= 1.0;
                    changed = true;
                }
            }

            // General overflow propagation
            let len = self.array.len();
            for i in 1..len {
                if self.array[i] > MAX_SAFE_INTEGER {
                    if self.array.len() <= i + 1 {
                        self.array.push(0.0);
                    }
                    self.array[i + 1] += 1.0;
                    self.array[0] = self.array[i] + 1.0;

                    for j in 1..=i {
                        self.array[j] = 0.0;
                    }
                    changed = true;
                }
            }

            if !changed {
                break;
            }
        }

        if self.array.is_empty() {
            self.array = vec![0.0];
        }
    }

    fn arrow10(arrows: i64, other: &OmegaNum) -> OmegaNum {
        assert_ne!(arrows, 0);
        let mut ret = OmegaNum::new(1.0);
        ret.array = Vec::new();
        ret.array.resize(arrows as usize, 0.0);
        ret.array[0] = 1.0;
        *ret.array.last_mut().unwrap() = other.to_number();
        ret
    }

    pub fn arrow(&self, arrows: i64) -> Box<dyn Fn(&OmegaNum) -> OmegaNum> {
        let t = self.clone();
        
        if arrows == -1 { Box::new(move |other: &OmegaNum| t.add(other) )}
        else if arrows == 0 { Box::new(move |other: &OmegaNum| t.mul(other) ) }
        else if arrows == 1 { Box::new(move |other: &OmegaNum| t.pow(other) ) }
        else if arrows == 2 { Box::new(move |other: &OmegaNum| t.tetrate(other) ) }
        else { Box::new(move |other: &OmegaNum| {
            if other < &OmegaNum::new(0.0) { return OmegaNum::new(f64::NAN) }
            if other == &OmegaNum::new(0.0) { return OmegaNum::new(1.0) }
            if other == &OmegaNum::new(1.0) { return t.clone() }
            if arrows >= match MAX_ARROW.lock() { Ok(max_arrow) => *max_arrow, Err(_) => i64::MIN } {
                eprintln!("Number too large to reasonably handle it: tried to {}-ate.", arrows + 2);
                return OmegaNum::new(f64::INFINITY)
            }
            if other == &OmegaNum::new(2.0) { return t.arrow(arrows - 1)(&t) }
            if OmegaNum::minmax(&t, other).1 > OmegaNum::arrow10(arrows + 1, &OmegaNum::new(MAX_SAFE_INTEGER)) { return OmegaNum::minmax(&t, other).1 };
            let mut r: OmegaNum;
            if t > OmegaNum::arrow10(arrows, &OmegaNum::new(MAX_SAFE_INTEGER)) || other > &OmegaNum::new(MAX_SAFE_INTEGER) {
                if t > OmegaNum::arrow10(arrows, &OmegaNum::new(MAX_SAFE_INTEGER)) {
                    r=t.clone();
                    if r.array.len() <= arrows as usize { r.array.resize(arrows as usize, 0.0) }
                    r.array[arrows as usize] -= 1.0;
                    r.normalize();
                } else if t > OmegaNum::arrow10(arrows - 1, &OmegaNum::new(MAX_SAFE_INTEGER)) {
                    r = OmegaNum::new(t.array[(arrows - 1) as usize]);
                } else {
                    r = OmegaNum::new(0.0);
                }
                let mut j = r.add(other);
                if j.array.len() <= arrows as usize { j.array.resize(arrows as usize + 1, 0.0) }
                j.array[arrows as usize] += 1.0;
                j.normalize();
                return j;
            }
            let y = other.to_number();
            let mut f = y.floor();
            r = t.arrow(arrows - 1)(&OmegaNum::new(y - f));
            let mut i = 0;
            let m = OmegaNum::arrow10(arrows - 1, &OmegaNum::new(MAX_SAFE_INTEGER));
            while f != 0.0 && r < m && i < 100 {
                if f > 0.0 {
                    r = t.arrow(arrows - 1)(&r);
                    f -= 1.0;
                }
                i += 1;
            }
            if i == 100 { f = 0.0 }
            if r.array.len() <= (arrows - 1) as usize { r.array.resize(arrows as usize, 0.0) }
            r.array[(arrows - 1) as usize] += f;
            r.normalize();
            r
        })}
    }

    fn minmax_m(a: OmegaNum, b: OmegaNum) -> (OmegaNum, OmegaNum) {
        if a < b { (a, b) } else { (b, a) }
    }

    pub fn minmax(a: &OmegaNum, b: &OmegaNum) -> (OmegaNum, OmegaNum) {
        OmegaNum::minmax_m(a.clone(), b.clone())
    }

    pub fn min(a: &OmegaNum, b: &OmegaNum) -> OmegaNum {
        OmegaNum::minmax(a, b).0
    }

    pub fn max(a: &OmegaNum, b: &OmegaNum) -> OmegaNum {
        OmegaNum::minmax(a, b).1
    }

    pub fn abs(&self) -> OmegaNum {
        if self.sign >= 0 { self.clone() }
        else { self.neg() }
    }

    pub fn to_number(&self) -> f64 {
        if self.sign < 0 { -self.neg().to_number() }
        else if self.array.len() > 1 && (self.array[1] >= 2.0 || self.array[1] == 1.0 && self.array[0] > f64::MAX.log10()) { f64::INFINITY }
        else if self.array.len() > 1 && self.array[1] == 1.0 { f64::powf(10.0, self.array[0]) }
        else { self.array[0] }
    }

    pub fn set_max_arrow(value: i64) -> Result<(), PoisonError<()>> {
        match MAX_ARROW.lock() {
            Ok(mut max_arrow) => { *max_arrow = value; Ok(()) },
            Err(_) => Err(PoisonError::<()>::new(()))
        }
    }

    pub fn reset_max_arrow() {
        match MAX_ARROW.lock() {
            Ok(mut max_arrow) => *max_arrow = MAX_ARROW_DEFAULT,
            Err(err) => *err.into_inner() = MAX_ARROW_DEFAULT
        }
    }

    pub fn get_max_arrow() -> Result<i64, PoisonError<()>> {
        match MAX_ARROW.lock() {
            Ok(max_arrow) => Ok(*max_arrow),
            Err(_) => Err(PoisonError::<()>::new(()))
        }
    }

    pub fn new(value: f64) -> Self {
        let mut ret = OmegaNum { sign: if value.signum() < 0.0 { -1 } else { 1 }, array: vec![value.abs()] };
        ret.normalize();
        ret
    }
    
    pub fn parse(str_value: String) -> Option<Self> {

        static RE : LazyLock<RegexSet> = LazyLock::new(|| RegexSet::new([
            "^\\s*\\(10\\{(?<oper>\\d+)\\}\\)\\^(?<pow>\\d+)(?<remainder>.*?)$", // (10{a})^b
            "^\\s*\\(10(?<oper>\\^+)\\)\\^(?<pow>\\d+)(?<remainder>.*?)$", // (10^^^^)^b
            "^\\s*10\\{(?<oper>\\d+)\\}(?<remainder>.*?)$", // 10{a}
            "^\\s*10(?<oper>\\^+)(?<remainder>.*?)$", // 10^^^^
            "^\\s*(?<lead>e+)?(?<value>\\d+(?:\\.\\d+)?(?:e\\d+(?:\\.\\d+)?)?)$" // ee2.17e15
        ]).expect("Regex compilation failure") );

        static RE_SET : LazyLock<Vec<Regex>> = LazyLock::new(||
            RE.patterns()
            .iter()
            .map(|pat| Regex::new(pat).expect("Regex compilation failure"))
            .collect()
        );

        if DEBUG_PARSE { eprintln!("Trying to match on '{str_value}'") };

        let mut s = str_value.clone();
        let mut sign = 1;
        let mut array = vec![];

        while s.starts_with("-") {
            s.drain(0..1);
            sign *= -1;
        }
        
        while !s.is_empty() {
            let m = RE.matches(&s);
            if !m.matched_any() {
                if DEBUG_PARSE { eprintln!("Could not match any pattern on '{s}' in '{str_value}'") };
                return None
            } else {
                for match_index in 0..5 {
                    if m.matched(match_index) {
                        if match_index < 4 {
                            if DEBUG_PARSE { eprintln!("Matched (10^)^ pattern '{s}'") };
                            let single_re = RE_SET.get(match_index).unwrap();
                            let caps = single_re.captures(&s).unwrap();

                            let oper: usize;
                            let pow: f64;
                            if let Some(oper_match) = caps.name("oper") {
                                oper = if oper_match.as_str().starts_with("^") { oper_match.len() } else {
                                    match oper_match.as_str().parse::<usize>() {
                                        Ok(oper_val) => oper_val,
                                        Err(_) => { eprintln!("Could not parse oper (for (10^)^ pattern '{s}') in '{str_value}'"); return None }
                                    }
                                }
                            } else {
                                oper = 1;
                            }
                            if let Some(pow_match) = caps.name("pow") {
                                pow = match pow_match.as_str().parse::<f64>() {
                                    Ok(pow_val) => pow_val,
                                    Err(_) => { eprintln!("Could not parse pow (for (10^)^ pattern '{s}') in '{str_value}'"); return None }
                                }
                            } else {
                                pow = 1.0;
                            }
                            if let Some(remainder_match) = caps.name("remainder") {
                                s = remainder_match.as_str().to_owned();
                            } else {
                                s = "".to_owned();
                            }

                            if array.len() <= oper { array.resize(oper + 1, 0.0) }
                            array[oper] = pow;
                        } else if match_index == 4 {
                            if DEBUG_PARSE { eprintln!("Matched lead-value pattern '{s}'") };
                            // Returning infinity?
                            let single_re = RE_SET.get(match_index).unwrap();
                            let caps = single_re.captures(&s).unwrap();

                            let lead: f64;
                            let value: f64;
                            if let Some(lead_match) = caps.name("lead") {
                                lead = lead_match.len() as f64;
                                if DEBUG_PARSE { eprintln!("Matched lead pattern '{}' (parsed to {lead})", lead_match.as_str()) };
                            } else {
                                lead = 0.0;
                                if DEBUG_PARSE { eprintln!("No lead matched! (parsed to 0.0)") };
                            }
                            if let Some(value_match) = caps.name("value") {
                                value = match value_match.as_str().parse::<f64>() {
                                    Ok(value_val) => value_val,
                                    Err(_) => { eprintln!("Could not parse value (for lead-value pattern '{s}') in '{str_value}'"); return None }
                                };
                                if DEBUG_PARSE { eprintln!("Matched value pattern '{}' (parsed to {value})", value_match.as_str()) };
                            } else {
                                if DEBUG_PARSE { eprintln!("No value found (for lead-value pattern '{s}') in '{str_value}'") };
                                return None
                            }

                            if lead != 0.0 {
                                if array.len() <= 1 { array.resize(2, 0.0) }
                                array[1] = lead;
                            }

                            if value.is_infinite() {
                                if DEBUG_PARSE { eprintln!("Value is infinite! Reparsing...") };
                                let vstr = caps.name("value").unwrap().as_str();
                                let (s1, s2) = vstr.split_at(vstr.find("e").expect("Value is infinity without `e`? How?"));
                                let s2 = s2.trim_start_matches("e");
                                if DEBUG_PARSE { eprintln!("Value split into {s1} e {s2}") };

                                if array.len() <= 1 { array.resize(2, 0.0) };
                                array[1] += 1.0;

                                let v1 = match s1.parse::<f64>() {
                                    Ok(v1_val) => v1_val,
                                    Err(_) => { eprintln!("Could not parse value (for mantissa pattern '{s1}') in '{str_value}'"); return None }
                                };

                                let v2 = match s2.parse::<f64>() {
                                    Ok(v2_val) => v2_val,
                                    Err(_) => { eprintln!("Could not parse value (for exponent pattern '{s2}') in '{str_value}'"); return None }
                                };
                                array[0] = v2 * f64::log10(v1);

                                if DEBUG_PARSE { eprintln!("Value parsed into {v1} e {v2} (e{})", array[0]) };

                            } else {
                                if array.is_empty() { array.resize(1, 0.0) }
                                array[0] = value;
                            }

                            return Some(Self { array, sign })
                        } else {
                            unreachable!()
                        }
                        break;
                    }
                }
            }
        }

        eprintln!("Unexpected end of input ('{str_value}')");

        None
    }
}

impl PartialEq for OmegaNum {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}

impl PartialOrd for OmegaNum {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.isnan() || other.isnan() { return None };
        if self.isinf() && !other.isinf() { return Some(Ordering::Greater) };
        if !self.isinf() && other.isinf() { return Some(Ordering::Less) };
        if self.array.len() == 1 && self.array[0] == 0.0 && other.array.len() == 1 && other.array[0] == 0.0 { return Some(Ordering::Equal) };
        if self.sign.signum() != other.sign.signum() { return Some(self.sign.signum().cmp(&other.sign.signum())) };
        let mut r = 0;
        if self.array.len() > other.array.len() { r = 1 }
        else if self.array.len() < other.array.len() { r = -1 }
        else {
            for i in (0..self.array.len()).rev() {
                if self.array[i] > other.array[i] {
                    r = 1;
                    break;
                } else if self.array[i]<other.array[i] {
                    r = -1;
                    break;
                }
            }
        }
        Some((r * self.sign).cmp(&0))
    }
}

impl ops::AddAssign<&OmegaNum> for OmegaNum {
    fn add_assign(&mut self, rhs: &OmegaNum) {
        // Handle sign combinations explicitly to avoid operating on temporaries
        if self.sign < 0 && rhs.sign < 0 {
            let mut x = self.abs();
            x.add_assign(&rhs.abs());
            x.neg().clone_into(self);
            return;
        } else if self.sign < 0 && rhs.sign >= 0 {
            // (-a) + b == b - a
            let mut x = rhs.clone();
            x.sub_assign(&self.abs());
            x.clone_into(self);
            return;
        } else if rhs.sign < 0 && self.sign >= 0 {
            // a + (-b) == a - b
            let mut x = self.clone();
            x.sub_assign(&rhs.abs());
            x.clone_into(self);
            return;
        }
        if *self == OmegaNum::new(0.0) {
            rhs.clone_into(self);
            return
        }
        if *rhs == OmegaNum::new(0.0) {
            return
        }
        if self.isnan() || rhs.isnan() || (self.isinf() && rhs.isinf() && self.sign != rhs.sign) {
            self.array = vec![f64::NAN];
            return
        }
        if self.isinf() {
            return
        }
        if rhs.isinf() {
            rhs.clone_into(self);
            return
        }
        let (p, q) = OmegaNum::minmax(self, rhs);
        if q > *E_MAX_SAFE_INTEGER || &q / &p > OmegaNum::new(MAX_SAFE_INTEGER) {
            q.clone_into(self);
        } else if q.array.len() < 2 || q.array[1] == 0.0 {
            OmegaNum::new(self.to_number() + rhs.to_number()).clone_into(self);
        } else if q.array[1] == 1.0 {
            let a = if p.array.get(1).unwrap_or(&0.0) != &0.0 { p.array[0] } else { p.array[0].log10() };
            self.array = vec![a + (f64::powf(10.0, q.array[0] - a) + 1.0).log10(), 1.0]
        }
    }
}

impl ops::SubAssign<&OmegaNum> for OmegaNum {
    fn sub_assign(&mut self, rhs: &OmegaNum) {
        // Handle sign combinations explicitly
        if self.sign < 0 && rhs.sign < 0 {
            // (-a) - (-b) = b - a
            let mut x = rhs.clone();
            x.sub_assign(&self.abs());
            x.clone_into(self);
            return;
        } else if self.sign < 0 && rhs.sign >= 0 {
            // (-a) - b = -(a + b)
            let mut x = self.abs();
            x.add_assign(rhs);
            x.neg().clone_into(self);
            return;
        } else if rhs.sign < 0 && self.sign >= 0 {
            // a - (-b) = a + b
            let mut x = self.clone();
            x.add_assign(&rhs.abs());
            x.clone_into(self);
            return;
        }
        if self == rhs {
            self.array = vec![0.0];
            return
        }
        if *rhs == OmegaNum::new(0.0) {
            return
        }
        if self.isnan() || rhs.isnan() || (self.isinf() && rhs.isinf() && self.sign == rhs.sign) {
            self.array = vec![f64::NAN];
            return
        }
        if self.isinf() {
            return
        }
        if rhs.isinf() {
            rhs.clone_into(self);
            self.neg_assign();
            return
        }
        let (p, q) = OmegaNum::minmax(self, rhs);
        let n = rhs > self;
        if q > *E_MAX_SAFE_INTEGER || &q / &p > OmegaNum::new(MAX_SAFE_INTEGER) {
            q.clone_into(self);
            if n { self.neg_assign() };
        } else if q.array.len() < 2 || q.array[1] == 0.0 {
            OmegaNum::new(self.to_number() - rhs.to_number()).clone_into(self);
        } else if q.array[1] == 1.0 {
            let a = if p.array.get(1).unwrap_or(&0.0) != &0.0 { p.array[0] } else { p.array[0].log10() };
            self.array = vec![a + (f64::powf(10.0, q.array[0] - a) - 1.0).log10(), 1.0];
            if n { self.neg_assign() };
        }
    }
}

impl OmegaNum {
    pub fn neg_assign(&mut self) {
        self.sign *= -1;
    }
}

impl ops::MulAssign<&OmegaNum> for OmegaNum {
    fn mul_assign(&mut self, rhs: &OmegaNum) {
        // Only multiply positive numbers
        if self.sign * rhs.sign < 0 { self.abs().mul(&rhs.abs()).neg().clone_into(self); return }
        if self.sign < 0 { self.abs().mul(&rhs.abs()).clone_into(self); return }

        // NaN propagation
        if self.isnan() || rhs.isnan() || self == &OmegaNum::new(0.0) && rhs.isinf() || self.isinf() && rhs == &OmegaNum::new(0.0) { self.array = vec![f64::NAN]; return }
        
        // Multiplying by zero
        if rhs == &OmegaNum::new(0.0) || self == &OmegaNum::new(0.0) { self.array = vec![0.0]; return }

        // Multiplying by one
        if rhs == &OmegaNum::new(1.0) { return }

        // Multiplying by infinity
        if self.isinf() { return }
        if rhs.isinf() { rhs.clone_into(self); return }

        // Number is so large that multiplication won't make a difference
        if *self > *EE_MAX_SAFE_INTEGER || rhs > &*EE_MAX_SAFE_INTEGER { OmegaNum::minmax(self, rhs).1.clone_into(self); return }
        
        // Number is small enough to just multiply floats
        let n = self.to_number() * rhs.to_number();
        if n < f64::INFINITY { OmegaNum::new(n).clone_into(self); return }

        OmegaNum::new(10.0).pow(&self.log10().add(&rhs.log10())).clone_into(self);
    }
}

impl ops::DivAssign<&OmegaNum> for OmegaNum {
    fn div_assign(&mut self, rhs: &OmegaNum) {
        // Only divide positive numbers
        if self.sign * rhs.sign < 0 { self.abs().div(&rhs.abs()).neg().clone_into(self); return }
        if self.sign < 0 { self.abs().div(&rhs.abs()).clone_into(self); return }

        // NaN propagation
        if self.isnan() || rhs.isnan() || self.isinf() && rhs.isinf() || self == &OmegaNum::new(0.0) && rhs == &OmegaNum::new(0.0) { self.array = vec![f64::NAN]; return }
        
        // Division by zero
        if rhs == &OmegaNum::new(0.0) { OmegaNum::new(f64::INFINITY).clone_into(self); return }

        // Division by one
        if rhs == &OmegaNum::new(1.0) { return }

        // Division by self
        if self == rhs { OmegaNum::new(1.0).clone_into(self); return }

        // Division by infinity
        if self.isinf() { return }
        if rhs.isinf() { OmegaNum::new(0.0).clone_into(self); return }

        // Number is so large that division won't make a difference / will slam to 0
        if *self > *EE_MAX_SAFE_INTEGER || rhs > &*EE_MAX_SAFE_INTEGER { if &*self > rhs { return } else { OmegaNum::new(0.0).clone_into(self); return } } 

        // Number is small enough to divide floats
        let n = self.to_number() / rhs.to_number();
        if n < f64::INFINITY { OmegaNum::new(n).clone_into(self); return }

        OmegaNum::new(10.0).pow(&self.log10().sub(&rhs.log10())).clone_into(self);
    }
}

impl ops::RemAssign<&OmegaNum> for OmegaNum {
    fn rem_assign(&mut self, rhs: &OmegaNum) {
        if rhs == &OmegaNum::new(0.0) { OmegaNum::new(0.0).clone_into(self); return }
        if self.sign * rhs.sign < 0 { self.abs().rem(&rhs.abs()).neg().clone_into(self); return }
        if self.sign < 0 { self.abs().rem(&rhs.abs()).clone_into(self); return }
        self.sub(&self.div(rhs).floor().mul(rhs)).clone_into(self);
    }
}

impl OmegaNum {
    pub fn pow_assign(&mut self, rhs: &OmegaNum) {
        if rhs == &OmegaNum::new(0.0) { OmegaNum::new(1.0).clone_into(self); return }
        if rhs == &OmegaNum::new(1.0) { return }
        if rhs < &OmegaNum::new(0.0) { (&OmegaNum::new(1.0) / &self.pow(&rhs.neg())).clone_into(self); return }
        if *self < OmegaNum::new(0.0) && rhs.isint() {
            if rhs % &OmegaNum::new(2.0) < OmegaNum::new(1.0) { self.abs().pow(rhs).clone_into(self); return }
            self.abs().pow(rhs).neg().clone_into(self); return
        }
        if *self < OmegaNum::new(0.0) { self.array = vec![f64::NAN]; return }
        if self == &OmegaNum::new(1.0) { OmegaNum::new(1.0).clone_into(self); return }
        if self == &OmegaNum::new(0.0) { OmegaNum::new(0.0).clone_into(self); return }
        if *self > *TETRATED_MAX_SAFE_INTEGER || rhs > &*TETRATED_MAX_SAFE_INTEGER { OmegaNum::minmax(self, rhs).1.clone_into(self); return }
        if self == &OmegaNum::new(10.0) {
            if rhs > &OmegaNum::new(0.0) {
                rhs.clone_into(self);
                if self.array.len() < 2 { self.array.resize(2, 0.0) }
                self.array[1] += 1.0;
                self.normalize(); return
            } else {
                OmegaNum::new(f64::powf(10.0, rhs.to_number())).clone_into(self); return
            }
        }
        if rhs < &OmegaNum::new(1.0) { self.root_assign(&(&OmegaNum::new(1.0) / rhs)); return }
        let n = f64::powf(self.to_number(), rhs.to_number());
        if n < f64::INFINITY { OmegaNum::new(n).clone_into(self); return }
        OmegaNum::new(10.0).pow(&self.log10().mul(rhs)).clone_into(self);
    }

    pub fn root_assign(&mut self, rhs: &OmegaNum) {
        if rhs == &OmegaNum::new(1.0) { return }
        if rhs < &OmegaNum::new(0.0) { (&OmegaNum::new(1.0) / &self.root(&rhs.neg())).clone_into(self); return }
        if rhs < &OmegaNum::new(1.0) { self.pow(&(&OmegaNum::new(1.0) / rhs)).clone_into(self); return }
        if *self == OmegaNum::new(0.0) && rhs.isint() && rhs % &OmegaNum::new(2.0) == OmegaNum::new(1.0) { self.neg().root(rhs).neg().clone_into(self); return }
        if *self < OmegaNum::new(0.0) { self.array = vec![f64::NAN]; return }
        if *self == OmegaNum::new(1.0) { OmegaNum::new(1.0).clone_into(self); return }
        if *self == OmegaNum::new(0.0) { OmegaNum::new(0.0).clone_into(self); return }
        if *self > *TETRATED_MAX_SAFE_INTEGER || rhs > &*TETRATED_MAX_SAFE_INTEGER { if rhs >= self { OmegaNum::new(1.0).clone_into(self); return } else { return } }
        OmegaNum::new(10.0).pow(&self.log10().div(rhs)).clone_into(self);
    }

    pub fn log10_assign(&mut self) {
        if *self < OmegaNum::new(0.0) { self.array = vec![f64::NAN]; return }
        if *self == OmegaNum::new(0.0) { OmegaNum::new(f64::NEG_INFINITY).clone_into(self); return }
        if *self <= OmegaNum::new(MAX_SAFE_INTEGER) { self.array = vec![self.to_number().log10()]; return }
        if self.isinf() { return }
        if *self > *TETRATED_MAX_SAFE_INTEGER { return }
        self.array[1] -= 1.0;
        self.normalize();
    }

    pub fn tetrate_assign(&mut self, rhs: &OmegaNum) {
        self.tetrate_from_assign(rhs, &OmegaNum::new(1.0))
    }

    pub fn tetrate_from_assign(&mut self, rhs_param: &OmegaNum, payload: &OmegaNum) {
        let mut rhs: OmegaNum = rhs_param.clone();
        if payload != &OmegaNum::new(1.0) { rhs = rhs.add(&payload.slog(self)); }
        if self.isnan() || rhs.isnan() || payload.isnan() { self.array = vec![f64::NAN]; return }
        if rhs.isinf() && rhs.sign > 0 {
            if *self >= OmegaNum::new(f64::exp(1.0 / std::f64::consts::E)) { OmegaNum::new(f64::INFINITY).clone_into(self); return }
            //Formula for infinite height power tower.
            let negln = self.ln().neg();
            negln.lambertw().div(&negln).clone_into(self); return
        }
        if rhs <= OmegaNum::new(-2.0) { self.array = vec![f64::NAN]; return }
        if self == &OmegaNum::new(0.0) {
            if rhs == OmegaNum::new(0.0) { self.array = vec![f64::NAN]; return }
            if &rhs % &OmegaNum::new(2.0) == OmegaNum::new(0.0) { OmegaNum::new(0.0).clone_into(self); return }
            OmegaNum::new(1.0).clone_into(self); return
        }
        if self == &OmegaNum::new(1.0) {
            if rhs == OmegaNum::new(-1.0) { self.array = vec![f64::NAN]; return };
            OmegaNum::new(1.0).clone_into(self); return
        }
        if rhs == OmegaNum::new(-1.0) { OmegaNum::new(0.0).clone_into(self); return }
        if rhs == OmegaNum::new(0.0) { OmegaNum::new(1.0).clone_into(self); return }
        if rhs == OmegaNum::new(1.0) { return }
        if rhs == OmegaNum::new(2.0) { self.pow(self).clone_into(self); return }
        if self == &OmegaNum::new(2.0) {
            if rhs == OmegaNum::new(3.0) { OmegaNum::new(16.0).clone_into(self); return }
            if rhs == OmegaNum::new(4.0) { OmegaNum::new(65536.0).clone_into(self); return }
        }
        let mut m = OmegaNum::minmax(self, &rhs).1;
        if m > *PENTATED_MAX_SAFE_INTEGER { m.clone_into(self); return }
        if m > *TETRATED_MAX_SAFE_INTEGER || rhs > OmegaNum::new(MAX_SAFE_INTEGER) {
            if *self < OmegaNum::new(f64::exp(1.0/std::f64::consts::E)) {
                let negln = self.ln().neg();
                negln.lambertw().div(&negln).clone_into(self); return
            }
            let mut j = self.slog(&OmegaNum::new(10.0)).add(& rhs);
            if j.array.len() < 3 { j.array.resize(3, 0.0) }
            j.array[2] += 1.0;
            j.normalize();
            j.clone_into(self); return
        }
        let y = rhs.to_number();
        let mut f = y.floor();
        let mut r = self.pow(&OmegaNum::new(y - f));
        let mut l = OmegaNum::new(f64::NAN);
        let mut i = 0;
        m = E_MAX_SAFE_INTEGER.clone();
        while f != 0.0 && r < m && i < 100 {
            if f > 0.0 {
                r = self.pow(&r);
                if l == r {
                    f = 0.0;
                    break;
                }
                l = r.clone();
                f -= 1.0;
            }else{
                r = &r.log10() / &self.log10();
                if l == r {
                    f = 0.0;
                    break;
                }
                l = r.clone();
                f += 1.0;
            }
            i += 1;
        }
        if i == 100 || *self < OmegaNum::new(f64::exp(1.0/std::f64::consts::E)) { f = 0.0 }
        if r.array.len() < 2 { r.array.resize(2, 0.0) }
        r.array[1] += f;
        r.normalize();
        r.clone_into(self);
    }

    pub fn slog_assign(&mut self, base: &OmegaNum) {
        if self.isnan() || base.isnan() || self.isinf() && base.isinf() { self.array = vec![f64::NAN]; return }
        if self.isinf() { return }
        if base.isinf() { OmegaNum::new(0.0).clone_into(self); return }
        if *self < OmegaNum::new(0.0) { OmegaNum::new(-1.0).clone_into(self); return }
        if self == &OmegaNum::new(1.0) { OmegaNum::new(0.0).clone_into(self); return }
        if self == base { OmegaNum::new(1.0).clone_into(self); return }
        if base < &OmegaNum::new(f64::exp(1.0/std::f64::consts::E)) {
            let a = OmegaNum::tetrate(base, &OmegaNum::new(f64::INFINITY));
            if self == &a { OmegaNum::new(f64::INFINITY).clone_into(self); return }
            if *self > a { self.array = vec![f64::NAN]; return }
        }
        if *self > *PENTATED_MAX_SAFE_INTEGER || base > &*PENTATED_MAX_SAFE_INTEGER {
            if &*self > base { return }
            OmegaNum::new(0.0).clone_into(self); return
        }
        if *self > *TETRATED_MAX_SAFE_INTEGER || base > &*TETRATED_MAX_SAFE_INTEGER {
            if &*self > base {
                if self.array.len() < 3 { self.array.resize(3, 0.0) }
                self.array[2] -= 1.0;
                self.normalize();
                self.sub(&OmegaNum::new(self.array[1])).clone_into(self); return
            }
            OmegaNum::new(0.0).clone_into(self); return
        }
        let mut r = 0.0;
        let t = self.array.get(1).unwrap_or(&0.0) - base.array.get(1).unwrap_or(&0.0);
        if t > 3.0 {
            let l = t - 3.0;
            r += l;
            if self.array.len() < 2 { self.array.resize(2, 0.0) }
            self.array[1] -= l;
        }
        let mut x = self.clone();
        for _ in 0..100 {
            if x < OmegaNum::new(0.0) {
                x = OmegaNum::pow(base, &x);
                r -= 1.0;
            } else if x <= OmegaNum::new(1.0) {
                OmegaNum::new(r + x.to_number() - 1.0).clone_into(self); return
            } else {
                r += 1.0;
                x = &OmegaNum::log10(&x) / &OmegaNum::log10(base);
            }
        }
        // if (x.gt(10)) // <-- tf is this ???
        OmegaNum::new(r).clone_into(self);
    }

    pub fn slog10_assign(&mut self) {
        self.slog_assign(&OmegaNum::new(10.0));
    }

    fn f_lambertw(z: f64, tol: f64, principal: bool) -> f64 {
        const OMEGA: f64 = 0.5671432904097838;
        let mut w: f64;
        if z.is_infinite() { return z; }
        if principal {
            if z == 0.0 { return z; }
            if z == 1.0 { return OMEGA; }
            if z < 10.0 { w = 0.0; }
            else { w = z.ln() - z.ln().ln(); }
        } else {
            if z == 0.0 { return f64::NEG_INFINITY; }
            if z <= -0.1 { w = -2.0; }
            else { w = (-z).ln() - (-z).ln().ln(); }
        }
        for _ in 0..100 {
            let wn = (z * (-w).exp() + w * w) / (w + 1.0);
            if f64::abs(wn - w) < tol * f64::abs(wn) { return wn; }
            w = wn;
        }
        panic!("f_lambertw: Iteration failed to converge ({z})");
    }

    fn d_lambertw(z: &OmegaNum, tol: f64, principal: bool) -> OmegaNum {
        const OMEGA: f64 = 0.5671432904097838;
        let mut w: OmegaNum;
        if z.isinf() || z.isnan() { return z.clone(); }
        if principal {
            if z == &OmegaNum::new(0.0) { return z.clone(); }
            if z == &OmegaNum::new(1.0) { return OmegaNum::new(OMEGA); }
            w = z.ln();
        } else {
        if z == &OmegaNum::new(0.0) { return OmegaNum::new(f64::NEG_INFINITY); }
            w = z.neg().ln();
        }
        for _ in 0..100 {
            let ew = OmegaNum::new(std::f64::consts::E).pow(&w.neg());
            let wewz = w.sub(&z.mul(&ew));
            let dd = w.add(&OmegaNum::new(1.0)).sub(&w.add(&OmegaNum::new(2.0)).mul(&wewz).div(&(&(&OmegaNum::new(2.0) * &w) + &OmegaNum::new(2.0))));
            if dd == OmegaNum::new(0.0) { return w; } //Escape to fix https://github.com/Naruyoko/ExpantaNum.js/issues/25
            let wn = w.sub(&wewz.div(&dd));
            if OmegaNum::abs(&wn.sub(&w)).lt(&OmegaNum::abs(&wn).mul(&OmegaNum::new(tol))) { return wn; }
            w = wn;
        }
        panic!("d_lambertw: Iteration failed to converge ({z})");
    }

    pub fn lambertw_principal_assign(&mut self, principal: bool) {
        if self.isnan() { return }
        if *self < OmegaNum::new(-0.3678794411710499) { OmegaNum::new(f64::NAN).clone_into(self); return }
        if principal {
            if *self > *TETRATED_MAX_SAFE_INTEGER { return }
            if *self > *EE_MAX_SAFE_INTEGER {
                self.array[1] -= 1.0; // This will always exist! I can be sure of it!
                return
            }
            if *self > OmegaNum::new(MAX_SAFE_INTEGER) { OmegaNum::d_lambertw(self, 1e-10, true).clone_into(self); }
            else { OmegaNum::new(OmegaNum::f_lambertw(self.sign as f64 * self.array.first().unwrap_or(&0.0), 1e-10, true)).clone_into(self); }
        } else {
            if self.sign > 0 { OmegaNum::new(f64::NAN).clone_into(self); return }
            if self.abs() > *EE_MAX_SAFE_INTEGER { self.neg().recip().lambertw().neg().clone_into(self); return }
            if self.abs() > OmegaNum::new(MAX_SAFE_INTEGER) { OmegaNum::d_lambertw(self,1e-10,false).clone_into(self); }
            else { OmegaNum::new(OmegaNum::f_lambertw(self.sign as f64 * self.array.first().unwrap_or(&0.0), 1e-10, false)).clone_into(self); }
        }
    }

    pub fn lambertw_assign(&mut self) {
        self.lambertw_principal_assign(true);
    }

    pub fn floor_assign(&mut self) {
        if *self > OmegaNum::new(MAX_SAFE_INTEGER) { return }
        if self.isnan() { return }
        if self.array.is_empty() { self.array.resize(1, 0.0) }
        self.array[0] = self.array[0].floor();
    }

}

impl ops::Add<&OmegaNum> for &OmegaNum {
    type Output = OmegaNum;

    fn add(self, rhs: &OmegaNum) -> Self::Output {
        let mut lhs = self.clone();
        lhs += rhs;
        lhs
    }
}

impl ops::Sub<&OmegaNum> for &OmegaNum {
    type Output = OmegaNum;

    fn sub(self, rhs: &OmegaNum) -> Self::Output {
        let mut lhs = self.clone();
        lhs -= rhs;
        lhs
    }
}

impl ops::Neg for &OmegaNum {
    type Output = OmegaNum;

    fn neg(self) -> Self::Output {
        let mut res = self.clone();
        res.neg_assign();
        res
    }
}

impl ops::Mul<&OmegaNum> for &OmegaNum {
    type Output = OmegaNum;

    fn mul(self, rhs: &OmegaNum) -> Self::Output {
        let mut lhs = self.clone();
        lhs *= rhs;
        lhs
    }
}

impl ops::Div<&OmegaNum> for &OmegaNum {
    type Output = OmegaNum;

    fn div(self, rhs: &OmegaNum) -> Self::Output {
        let mut lhs = self.clone();
        lhs /= rhs;
        lhs
    }
}

impl ops::Rem<&OmegaNum> for &OmegaNum {
    type Output = OmegaNum;

    fn rem(self, rhs: &OmegaNum) -> Self::Output {
        let mut lhs = self.clone();
        lhs %= rhs;
        lhs
    }
}

impl OmegaNum {
    pub fn recip(&self) -> OmegaNum {
        &OmegaNum::new(1.0) / self
    }

    pub fn pow(&self, rhs: &OmegaNum) -> OmegaNum {
        let mut lhs = self.clone();
        lhs.pow_assign(rhs);
        lhs
    }

    pub fn root(&self, rhs: &OmegaNum) -> OmegaNum {
        let mut lhs = self.clone();
        lhs.root_assign(rhs);
        lhs
    }

    pub fn log10(&self) -> OmegaNum {
        let mut lhs = self.clone();
        lhs.log10_assign();
        lhs
    }

    pub fn ln(&self) -> OmegaNum {
        &self.log10() / &OmegaNum::new(std::f64::consts::E).log10()
    }

    pub fn tetrate(&self, rhs: &OmegaNum) -> OmegaNum {
        self.tetrate_from(rhs, &OmegaNum::new(1.0))
    }

    pub fn tetrate_from(&self, rhs: &OmegaNum, payload: &OmegaNum) -> OmegaNum {
        let mut lhs = self.clone();
        lhs.tetrate_from_assign(rhs, payload);
        lhs
    }

    pub fn slog(&self, base: &OmegaNum) -> OmegaNum {
        let mut lhs = self.clone();
        lhs.slog_assign(base);
        lhs
    }

    pub fn slog10(&self) -> OmegaNum {
        let mut lhs = self.clone();
        lhs.slog10_assign();
        lhs
    }

    pub fn pentate(&self, rhs: &OmegaNum) -> OmegaNum {
        self.arrow(3)(rhs)
    }

    pub fn lambertw_principal(&self, principal: bool) -> OmegaNum {
        let mut lhs = self.clone();
        lhs.lambertw_principal_assign(principal);
        lhs
    }

    pub fn lambertw(&self) -> OmegaNum {
        self.lambertw_principal(true)
    }

    pub fn floor(&self) -> OmegaNum {
        let mut lhs = self.clone();
        lhs.floor_assign();
        lhs
    }
}

const MIN_E: f64 = 6.0;
fn ldts(v: f64, allow_end_trim: bool) -> String {
    let ve = v.log10().floor();
    if ve == f64::NEG_INFINITY {
        return "0".to_owned();
    }
    if ve >= MIN_E || ve <= -MIN_E {
        ldts(v / f64::powf(10.0, ve), false) + "e" + &(ve as i64).to_string()
    } else if allow_end_trim {
        format!("{v:.2}").trim_end_matches(".00").to_owned()
    } else {
        format!("{v:.2}").to_owned()
    }
}

impl std::fmt::Display for OmegaNum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.sign < 0 { return write!(f, "-{}", self.abs()) }
        if self.isnan() { return write!(f, "NaN") }
        if self.isinf() { return write!(f, "Infinity") }
        let mut s: String = String::new();
        if self.array.len() >= 2 {
            for i in (2..=self.array.len()-1).rev() {
                let q = if i >= 5 { "{".to_owned() + &i.to_string() + "}" } else { "^".repeat(i) };
                if self.array[i] > 1.0 { s += &("(10".to_owned() + &q + ")^" + &ldts(self.array[i], true) + " "); }
                else if self.array[i] == 1.0 { s += &("10".to_owned() + &q); }
            }
        }
        if *self.array.get(1).unwrap_or(&0.0) == 0.0 { s += &ldts(self.to_number(), true); }
        else if (self.array[1] < 3.0 && self.array[0].log10() < MIN_E) || (self.array[0] < 100.0 && self.array[1] < 8.0) {
            s += &("e".repeat((self.array[1] - 1.0) as usize) + &ldts(f64::powf(10.0, self.array[0] - f64::floor(self.array[0])), false) + "e" + &ldts(f64::floor(self.array[0]), true));
        }
        else if self.array[1] < 8.0 { s += &("e".repeat(self.array[1] as usize) + &ldts(self.array[0], true)); }
        else { s += &("(10^)^".to_owned() + &ldts(*self.array.get(1).unwrap_or(&0.0), true) + " " + &ldts(*self.array.first().unwrap_or(&0.0), true)); }
        write!(f, "{s}")
    }
}

#[cfg(test)]
mod tests {
    use super::OmegaNum;
    use super::MAX_SAFE_INTEGER;
    use quickcheck::quickcheck;

    #[test]
    fn test_new_and_to_number() {
        let a = OmegaNum::new(42.0);
        assert_eq!(a.to_number(), 42.0);
        let b = OmegaNum::new(-3.5);
        assert_eq!(b.sign(), -1);
        assert_eq!(b.abs().to_number(), 3.5);
    }

    #[test]
    fn test_add_mul_basic() {
        let a = OmegaNum::new(2.0);
        let b = OmegaNum::new(3.0);
        let c = &a + &b; // 5
        assert_eq!(c.to_number(), 5.0);

        let d = &a * &b; // 6
        assert_eq!(d.to_number(), 6.0);
    }

    #[test]
    fn test_parse_and_display() {
        // basic decimal
        let p = OmegaNum::parse("123.45".to_owned()).expect("parse should succeed");
        assert_eq!(p.to_number(), 123.45);

        // simple scientific
        let s = OmegaNum::parse("1e3".to_owned()).expect("parse should succeed");
        assert_eq!(s.to_number(), 1000.0);

        // display should produce a non-empty string
        let d = s.to_string();
        assert!(!d.is_empty());
    }

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

    #[test]
    fn robustness_normalize_overflow() {
        // create an OmegaNum with a first element larger than MAX_SAFE_INTEGER
        let mut a = OmegaNum { array: vec![MAX_SAFE_INTEGER * 10.0], sign: 1 };
        a.normalize();
        assert!(a.array.len() >= 2, "normalize should have created a higher exponent level");
        assert!(a.array[1] > 0.0, "exponent level should be incremented");
    }

    #[test]
    fn robustness_parse_lead_value() {
        // lead 'ee' should set array[1] to 2
        let p = OmegaNum::parse("ee2.17e15".to_owned()).expect("parse should succeed");
        assert!(p.array.len() >= 2 && p.array[1] >= 2.0, "lead-count parse failed: {:?}", p);
        assert!(!p.isnan());
    }

    #[test]
    fn robustness_negative_arithmetic_and_rem() {
        let a = OmegaNum::new(-5.0);
        let b = OmegaNum::new(3.0);
        let sum = &a + &b;
        assert_eq!(sum.to_number(), -2.0);

        let diff = &b - &a; // 3 - (-5) = 8
        assert_eq!(diff.to_number(), 8.0);

        let ten = OmegaNum::new(10.0);
        let three = OmegaNum::new(3.0);
        let r = &ten % &three;
        assert_eq!(r.to_number(), 1.0);
    }

    #[test]
    fn robustness_pow_negative_exponent() {
        let two = OmegaNum::new(2.0);
        let inv = two.pow(&OmegaNum::new(-1.0));
        let val = inv.to_number();
        assert!(val.is_finite(), "2^-1 should be finite");
        assert!((val - 0.5).abs() < 1e-12, "expected 0.5, got {}", val);
    }

    #[test]
    fn higher_order_arrow_equivalence() {
        let base = OmegaNum::new(2.0);
        let other = OmegaNum::new(3.0);

        // arrows==0 should be multiplication
        let m = base.arrow(0)(&other);
        assert_eq!(m.to_number(), (&base * &other).to_number());

        // arrows==1 should be power
        let p = base.arrow(1)(&other);
        assert_eq!(p.to_number(), base.pow(&other).to_number());

        // arrows==2 should be tetration for small integers
        let t = base.arrow(2)(&other);
        assert_eq!(t.to_number(), base.tetrate(&other).to_number());
    }

    #[test]
    fn higher_order_pentation_small() {
        let two = OmegaNum::new(2.0);
        let p0 = two.pentate(&OmegaNum::new(0.0));
        assert!(p0.to_number() == 1.0 || p0.isinf());
        let p1 = two.pentate(&OmegaNum::new(1.0));
        assert!(p1.to_number() == 2.0 || p1.isinf());
        let p2 = two.pentate(&OmegaNum::new(2.0));
        assert!(p2.to_number() == 4.0 || p2.isinf());
    }

    #[test]
    fn higher_order_max_arrow_bound() {
        OmegaNum::set_max_arrow(2).expect("set_max_arrow failed");
        assert_eq!(OmegaNum::get_max_arrow().expect("get_max_arrow failed"), 2);
        OmegaNum::reset_max_arrow();
    }

    #[test]
    fn prop_arithmetic_commutativity() {
        fn prop_add(a: i16, b: i16) -> bool {
            let oa = OmegaNum::new(a as f64);
            let ob = OmegaNum::new(b as f64);
            let r1 = (&oa + &ob).to_number();
            let r2 = (&ob + &oa).to_number();
            if r1.is_nan() { r2.is_nan() } else { (r1 - r2).abs() < 1e-12 }
        }
        quickcheck(prop_add as fn(i16, i16) -> bool);

        fn prop_mul(a: i16, b: i16) -> bool {
            let oa = OmegaNum::new(a as f64);
            let ob = OmegaNum::new(b as f64);
            let r1 = (&oa * &ob).to_number();
            let r2 = (&ob * &oa).to_number();
            if r1.is_nan() { r2.is_nan() } else { (r1 - r2).abs() < 1e-12 }
        }
        quickcheck(prop_mul as fn(i16, i16) -> bool);
    }

    #[test]
    fn prop_pow_root_inverse_small() {
        fn prop(a: u8, n: u8) -> bool {
            let base = (a % 9) + 2;
            let exp = (n % 5) + 1;
            let oa = OmegaNum::new(base as f64);
            let on = OmegaNum::new(exp as f64);
            let p = oa.pow(&on);
            if !p.isinf() {
                let r = p.root(&on);
                (&r - &oa).abs().to_number() < 1e-9
            } else {
                true
            }
        }
        quickcheck(prop as fn(u8, u8) -> bool);
    }

    #[test]
    fn slog_and_lambertw_small() {
        // slog base 10 of 10 is 1
        let ten = OmegaNum::new(10.0);
        assert!((ten.slog10().to_number() - 1.0).abs() < 1e-12);

        // lambertw(e) = 1
        let e = OmegaNum::new(std::f64::consts::E);
        let w = e.lambertw();
        assert!((w.to_number() - 1.0).abs() < 1e-9);
    }
}
