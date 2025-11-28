use std::ops;
use std::ops::*;
use std::cmp::*;

use std::f64::NAN;
use std::f64::INFINITY;
use std::f64::NEG_INFINITY;
use std::sync::PoisonError;
use std::io::Write;

use rand::random_range;
use regex::Regex;
use regex::RegexSet;
use lazy_static::lazy_static;
use std::sync::LazyLock;
use std::sync::Mutex;

#[derive(Debug, Clone)]
pub struct OmegaNum {
    array: Vec<f64>,
    sign: i8,
}

const MAX_ARROW_DEFAULT : u64 = 1000;
static MAX_ARROW: Mutex<u64> = Mutex::new(MAX_ARROW_DEFAULT);
const MAX_SAFE_INTEGER : f64 = 9007199254740991.0;

lazy_static! {
    pub static ref MAX_E : f64 = f64::log10(MAX_SAFE_INTEGER);
    pub static ref E_MAX_SAFE_INTEGER : OmegaNum = OmegaNum { array: vec![MAX_SAFE_INTEGER, 1.0], sign: 1 };
    pub static ref EE_MAX_SAFE_INTEGER : OmegaNum = OmegaNum { array: vec![MAX_SAFE_INTEGER, 2.0], sign: 1 };
    pub static ref TETRATED_MAX_SAFE_INTEGER : OmegaNum = OmegaNum { array: vec![1.0, MAX_SAFE_INTEGER], sign: 1 };
    pub static ref PENTATED_MAX_SAFE_INTEGER : OmegaNum = OmegaNum { array: vec![1.0, 0.0, MAX_SAFE_INTEGER], sign: 1 };
}


impl OmegaNum {
    pub fn sign(&self) -> i8 { self.sign }

    pub fn isnan(&self) -> bool {
        match self.array.get(0) {
            None => true,
            Some(x) => x.is_nan()
        }
    }

    pub fn isinf(&self) -> bool {
        match self.array.get(0) {
            None => false,
            Some(x) => x.is_infinite()
        }
    }

    pub fn isint(&self) -> bool {
        match self.array.get(0) {
            None => false,
            Some(x) => x.fract() == 0.0
        }
    }

    pub fn normalize(&mut self) -> () {
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

    fn arrow10(arrows: u64, other: &OmegaNum) -> OmegaNum {
        assert_ne!(arrows, 0);
        let mut ret = OmegaNum::new(1.0);
        ret.array = Vec::new();
        ret.array.resize(arrows as usize, 0.0);
        ret.array[0] = 1.0;
        *ret.array.last_mut().unwrap() = other.to_number();
        ret
    }

    pub fn arrow(&self, arrows: u64) -> Box<dyn Fn(&OmegaNum) -> OmegaNum> {
        let t = self.clone();
        
        if arrows == u64::MAX { Box::new(move |other: &OmegaNum| t.add(other) )}
        else if arrows == 0 { Box::new(move |other: &OmegaNum| t.mul(other) ) }
        else if arrows == 1 { Box::new(move |other: &OmegaNum| t.pow(other) ) }
        else if arrows == 2 { Box::new(move |other: &OmegaNum| t.tetrate(other) ) }
        else { Box::new(move |other: &OmegaNum| {
            if other < &OmegaNum::new(0.0) { return OmegaNum::new(NAN) }
            if other == &OmegaNum::new(0.0) { return OmegaNum::new(1.0) }
            if other == &OmegaNum::new(1.0) { return t.clone() }
            if arrows >= match MAX_ARROW.lock() { Ok(max_arrow) => *max_arrow, Err(_) => u64::MIN } {
                println!("Number too large to reasonably handle it: tried to {}-ate.", arrows + 2);
                return OmegaNum::new(INFINITY)
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
        return OmegaNum::minmax_m(a.clone(), b.clone());
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
        if self.sign < 0 { -1.0 * self.neg().to_number() }
        else if self.array.len() > 1 && (self.array[1] >= 2.0 || self.array[1] == 1.0 && self.array[0] > f64::MAX.log10()) { INFINITY }
        else if self.array.len() > 1 && self.array[1] == 1.0 { f64::powf(10.0, self.array[0]) }
        else { self.array[0] }
    }

    pub fn set_max_arrow(value: u64) -> Result<(), PoisonError<()>> {
        match MAX_ARROW.lock() {
            Ok(mut max_arrow) => { *max_arrow = value; Ok(()) },
            Err(_) => Err(PoisonError::<()>::new(()))
        }
    }

    pub fn reset_max_arrow() -> () {
        match MAX_ARROW.lock() {
            Ok(mut max_arrow) => *max_arrow = MAX_ARROW_DEFAULT,
            Err(err) => *err.into_inner() = MAX_ARROW_DEFAULT
        }
    }

    pub fn get_max_arrow() -> Result<u64, PoisonError<()>> {
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
    
    pub fn parse(value: String) -> Option<Self> {

        static RE : LazyLock<RegexSet> = LazyLock::new(|| RegexSet::new(&[
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

        let mut s = value;
        let mut sign = 1;
        let mut array = vec![];

        while s.starts_with("-") {
            s.drain(0..1);
            sign *= -1;
        }
        
        while !s.is_empty() {
            let m = RE.matches(&s);
            if !m.matched_any() {
                return None
            } else {
                match (&m).into_iter().position(|x| m.matched(x)).unwrap() {
                    match_index @ 0..4 => {
                        let single_re = RE_SET.get(match_index).unwrap();
                        let caps = single_re.captures(&s).unwrap();

                        let oper: usize;
                        let pow: f64;
                        if let Some(oper_match) = caps.name("oper") {
                            oper = if oper_match.as_str().starts_with("^") { oper_match.len() } else {
                                match oper_match.as_str().parse::<usize>() {
                                    Ok(oper_val) => oper_val,
                                    Err(_) => return None
                                }
                            }
                        } else {
                            oper = 1;
                        }
                        if let Some(pow_match) = caps.name("pow") {
                            pow = match pow_match.as_str().parse::<f64>() {
                                Ok(pow_val) => pow_val,
                                Err(_) => return None
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
                    },
                    match_index @ 4 => {
                        let single_re = RE_SET.get(match_index).unwrap();
                        let caps = single_re.captures(&s).unwrap();

                        let lead: f64;
                        let value: f64;
                        if let Some(lead_match) = caps.name("lead") {
                            lead = lead_match.len() as f64;
                        } else {
                            lead = 0.0;
                        }
                        if let Some(value_match) = caps.name("value") {
                            value = match value_match.as_str().parse::<f64>() {
                                Ok(value_val) => value_val,
                                Err(_) => return None
                            }
                        } else {
                            return None
                        }

                        if lead != 0.0 {
                            if array.len() <= 1 { array.resize(2, 0.0) }
                            array[1] = lead;
                        }
                        if array.len() <= 0 { array.resize(1, 0.0) }
                        array[0] = value;

                        return Some(Self { array, sign })
                    },
                    _ => unreachable!()
                }
            }
        }

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
    fn add_assign(&mut self, rhs: &OmegaNum) -> () {
        if self.sign < 0 {
            (&*self).neg().add_assign(&rhs.neg());
            self.neg_assign();
            return
        }
        if rhs.sign < 0 {
            self.sub_assign(&rhs.neg());
            return
        }
        if *self == OmegaNum::new(0.0) {
            rhs.clone_into(self);
            return
        }
        if *rhs == OmegaNum::new(0.0) {
            return
        }
        if self.isnan() || rhs.isnan() || (self.isinf() && rhs.isinf() && self.sign != rhs.sign) {
            self.array = vec![NAN];
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
    fn sub_assign(&mut self, rhs: &OmegaNum) -> () {
        if self.sign < 0 {
            (&*self).neg().sub_assign(&rhs.neg());
            self.neg_assign();
            return
        }
        if rhs.sign < 0 {
            self.add_assign(&rhs.neg());
            return
        }
        if self == rhs {
            self.array = vec![0.0];
            return
        }
        if *rhs == OmegaNum::new(0.0) {
            return
        }
        if self.isnan() || rhs.isnan() || (self.isinf() && rhs.isinf() && self.sign == rhs.sign) {
            self.array = vec![NAN];
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
    pub fn neg_assign(&mut self) -> () {
        self.sign *= -1;
    }
}

impl ops::MulAssign<&OmegaNum> for OmegaNum {
    fn mul_assign(&mut self, rhs: &OmegaNum) -> () {
        // Only multiply positive numbers
        if self.sign * rhs.sign < 0 { self.abs().mul(&rhs.abs()).neg().clone_into(self); return }
        if self.sign < 0 { self.abs().mul(&rhs.abs()).clone_into(self); return }

        // NaN propagation
        if self.isnan() || rhs.isnan() || self == &OmegaNum::new(0.0) && rhs.isinf() || self.isinf() && rhs == &OmegaNum::new(0.0) { self.array = vec![NAN]; return }
        
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
        if n < INFINITY { OmegaNum::new(n).clone_into(self); return }

        OmegaNum::new(10.0).pow(&self.log10().add(&rhs.log10())).clone_into(self); return
    }
}

impl ops::DivAssign<&OmegaNum> for OmegaNum {
    fn div_assign(&mut self, rhs: &OmegaNum) -> () {
        // Only divide positive numbers
        if self.sign * rhs.sign < 0 { self.abs().div(&rhs.abs()).neg().clone_into(self); return }
        if self.sign < 0 { self.abs().div(&rhs.abs()).clone_into(self); return }

        // NaN propagation
        if self.isnan() || rhs.isnan() || self.isinf() && rhs.isinf() || self == &OmegaNum::new(0.0) && rhs == &OmegaNum::new(0.0) { self.array = vec![NAN]; return }
        
        // Division by zero
        if rhs == &OmegaNum::new(0.0) { OmegaNum::new(INFINITY).clone_into(self); return }

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
        if n < INFINITY { OmegaNum::new(n).clone_into(self); return }

        OmegaNum::new(10.0).pow(&self.log10().sub(&rhs.log10())).clone_into(self);
    }
}

impl ops::RemAssign<&OmegaNum> for OmegaNum {
    fn rem_assign(&mut self, rhs: &OmegaNum) -> () {
        if rhs == &OmegaNum::new(0.0) { OmegaNum::new(0.0).clone_into(self); return }
        if self.sign * rhs.sign < 0 { self.abs().rem(&rhs.abs()).neg().clone_into(self); return }
        if self.sign < 0 { self.abs().rem(&rhs.abs()).clone_into(self); return }
        self.sub(&self.div(rhs).floor().mul(rhs)).clone_into(self); return
    }
}

impl OmegaNum {
    pub fn pow_assign(&mut self, rhs: &OmegaNum) -> () {
        if rhs == &OmegaNum::new(0.0) { OmegaNum::new(1.0).clone_into(self); return }
        if rhs == &OmegaNum::new(1.0) { return }
        if rhs < &OmegaNum::new(0.0) { (&OmegaNum::new(1.0) / &self.pow(&rhs.neg())).clone_into(self); return }
        if &*self < &OmegaNum::new(0.0) && rhs.isint() {
            if rhs % &OmegaNum::new(2.0) < OmegaNum::new(1.0) { self.abs().pow(rhs).clone_into(self); return }
            self.abs().pow(rhs).neg().clone_into(self); return
        }
        if &*self < &OmegaNum::new(0.0) { self.array = vec![NAN]; return }
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
        if n < INFINITY { OmegaNum::new(n).clone_into(self); return }
        OmegaNum::new(10.0).pow(&self.log10().mul(rhs)).clone_into(self); return
    }

    pub fn root_assign(&mut self, rhs: &OmegaNum) -> () {
        if rhs == &OmegaNum::new(1.0) { return }
        if rhs < &OmegaNum::new(0.0) { (&OmegaNum::new(1.0) / &self.root(&rhs.neg())).clone_into(self); return }
        if rhs < &OmegaNum::new(1.0) { self.pow(&(&OmegaNum::new(1.0) / rhs)).clone_into(self); return }
        if &*self == &OmegaNum::new(0.0) && rhs.isint() && rhs % &OmegaNum::new(2.0) == OmegaNum::new(1.0) { self.neg().root(rhs).neg().clone_into(self); return }
        if &*self < &OmegaNum::new(0.0) { self.array = vec![NAN]; return }
        if &*self == &OmegaNum::new(1.0) { OmegaNum::new(1.0).clone_into(self); return }
        if &*self == &OmegaNum::new(0.0) { OmegaNum::new(0.0).clone_into(self); return }
        if *self > *TETRATED_MAX_SAFE_INTEGER || rhs > &*TETRATED_MAX_SAFE_INTEGER { if rhs <= self { OmegaNum::new(0.0).clone_into(self); return } else { return } }
        OmegaNum::new(10.0).pow(&self.log10().div(rhs)).clone_into(self); return
    }

    pub fn log10_assign(&mut self) -> () {
        if &*self < &OmegaNum::new(0.0) { self.array = vec![NAN]; return }
        if &*self == &OmegaNum::new(0.0) { OmegaNum::new(NEG_INFINITY).clone_into(self); return }
        if &*self <= &OmegaNum::new(MAX_SAFE_INTEGER) { self.array = vec![self.to_number().log10()]; return }
        if self.isinf() { return }
        if *self > *TETRATED_MAX_SAFE_INTEGER { return }
        self.array[1] -= 1.0;
        self.normalize();
    }

    pub fn tetrate_assign(&mut self, rhs: &OmegaNum) -> () {
        self.tetrate_from_assign(rhs, &OmegaNum::new(1.0))
    }

    pub fn tetrate_from_assign(&mut self, rhs_param: &OmegaNum, payload: &OmegaNum) -> () {
        let mut rhs: OmegaNum = rhs_param.clone();
        if payload != &OmegaNum::new(1.0) { rhs = rhs.add(&payload.slog(self)); }
        if self.isnan() || rhs.isnan() || payload.isnan() { self.array = vec![NAN]; return }
        if rhs.isinf() && rhs.sign > 0 {
            if &*self >= &OmegaNum::new(f64::exp(1.0 / std::f64::consts::E)) { OmegaNum::new(INFINITY).clone_into(self); return }
            //Formula for infinite height power tower.
            let negln = self.ln().neg();
            negln.lambertw().div(&negln).clone_into(self); return
        }
        if rhs <= OmegaNum::new(-2.0) { self.array = vec![NAN]; return }
        if self == &OmegaNum::new(0.0) {
            if rhs == OmegaNum::new(0.0) { self.array = vec![NAN]; return }
            if &rhs % &OmegaNum::new(2.0) == OmegaNum::new(0.0) { OmegaNum::new(0.0).clone_into(self); return }
            OmegaNum::new(1.0).clone_into(self); return
        }
        if self == &OmegaNum::new(1.0) {
            if rhs == OmegaNum::new(-1.0) { self.array = vec![NAN]; return };
            OmegaNum::new(1.0).clone_into(self); return
        }
        if rhs == OmegaNum::new(-1.0) { OmegaNum::new(0.0).clone_into(self); return }
        if rhs == OmegaNum::new(0.0) { OmegaNum::new(1.0).clone_into(self); return }
        if rhs == OmegaNum::new(1.0) { return }
        if rhs == OmegaNum::new(2.0) { self.pow(self).clone_into(self); return }
        if self == &OmegaNum::new(2.0) {
            if rhs == OmegaNum::new(3.0) { OmegaNum::new(16.0); return }
            if rhs == OmegaNum::new(4.0) { OmegaNum::new(65536.0); return }
        }
        let mut m = OmegaNum::minmax(self, &rhs).1;
        if m > *PENTATED_MAX_SAFE_INTEGER { m.clone_into(self); return }
        if m > *TETRATED_MAX_SAFE_INTEGER || rhs > OmegaNum::new(MAX_SAFE_INTEGER) {
            if &*self < &OmegaNum::new(f64::exp(1.0/std::f64::consts::E)) {
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
        let mut l = OmegaNum::new(NAN);
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
        if i == 100 || &*self < &OmegaNum::new(f64::exp(1.0/std::f64::consts::E)) { f = 0.0 }
        if r.array.len() < 2 { r.array.resize(2, 0.0) }
        r.array[1] += f;
        r.normalize();
        r.clone_into(self); return
    }

    pub fn slog_assign(&mut self, base: &OmegaNum) -> () {
        if self.isnan() || base.isnan() || self.isinf() && base.isinf() { self.array = vec![NAN]; return }
        if self.isinf() { return }
        if base.isinf() { OmegaNum::new(0.0).clone_into(self); return }
        if &*self < &OmegaNum::new(0.0) { OmegaNum::new(-1.0).clone_into(self); return }
        if self == &OmegaNum::new(1.0) { OmegaNum::new(0.0).clone_into(self); return }
        if self == base { OmegaNum::new(1.0).clone_into(self); return }
        if base < &OmegaNum::new(f64::exp(1.0/std::f64::consts::E)) {
            let a = OmegaNum::tetrate(base, &OmegaNum::new(INFINITY));
            if self == &a { OmegaNum::new(INFINITY).clone_into(self); return }
            if &*self > &a { self.array = vec![NAN]; return }
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
        OmegaNum::new(r).clone_into(self); return
    }

    pub fn slog10_assign(&mut self) -> () {
        self.slog_assign(&OmegaNum::new(10.0));
    }

    fn f_lambertw(z: f64, tol: f64, principal: bool) -> f64 {
        const OMEGA: f64 = 0.56714329040978387299997;
        let mut w: f64;
        if z.is_infinite() { return z; }
        if principal {
            if z == 0.0 { return z; }
            if z == 1.0 { return OMEGA; }
            if z < 10.0 { w = 0.0; }
            else { w = z.ln() - z.ln().ln(); }
        } else {
            if z == 0.0 { return -INFINITY; }
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
        const OMEGA: f64 = 0.56714329040978387299997;
        let mut w: OmegaNum;
        if z.isinf() || z.isnan() { return z.clone(); }
        if principal {
            if z == &OmegaNum::new(0.0) { return z.clone(); }
            if z == &OmegaNum::new(1.0) { return OmegaNum::new(OMEGA); }
            w = z.ln();
        } else {
        if z == &OmegaNum::new(0.0) { return OmegaNum::new(NEG_INFINITY); }
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

    pub fn lambertw_principal_assign(&mut self, principal: bool) -> () {
        if self.isnan() { return }
        if &*self < &OmegaNum::new(-0.3678794411710499) { OmegaNum::new(NAN).clone_into(self); return }
        if principal {
            if *self > *TETRATED_MAX_SAFE_INTEGER { return }
            if *self > *EE_MAX_SAFE_INTEGER {
                self.array[1] -= 1.0; // This will always exist! I can be sure of it!
                return
            }
            if &*self > &OmegaNum::new(MAX_SAFE_INTEGER) { OmegaNum::d_lambertw(self, 1e-10, true).clone_into(self); return }
            else { OmegaNum::new(OmegaNum::f_lambertw(self.sign as f64 * self.array.get(0).unwrap_or(&0.0), 1e-10, true)).clone_into(self); return }
        } else {
            if self.sign > 0 { OmegaNum::new(NAN).clone_into(self); return }
            if self.abs() > *EE_MAX_SAFE_INTEGER { self.neg().recip().lambertw().neg().clone_into(self); return }
            if self.abs() > OmegaNum::new(MAX_SAFE_INTEGER) { OmegaNum::d_lambertw(self,1e-10,false).clone_into(self); return }
            else { OmegaNum::new(OmegaNum::f_lambertw(self.sign as f64 * self.array.get(0).unwrap_or(&0.0), 1e-10, false)).clone_into(self); return }
        }
    }

    pub fn lambertw_assign(&mut self) -> () {
        self.lambertw_principal_assign(true);
    }

    pub fn floor_assign(&mut self) -> () {
        if &*self > &OmegaNum::new(MAX_SAFE_INTEGER) { return }
        if self.isnan() { return }
        if self.array.len() < 1 { self.array.resize(1, 0.0) }
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

fn ldts(v: f64, allow_end_trim: bool) -> String {
    const MIN_E: f64 = 6.0;
    let ve = v.log10().floor();
    if ve >= MIN_E || ve <= -MIN_E {
        return ldts(v / f64::powf(10.0, ve), false) + "e" + &(ve as i64).to_string();
    } else {
        if allow_end_trim { return format!("{v:.2}").trim_end_matches(".00").to_owned(); }
        else { return format!("{v:.2}").to_owned(); }
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
        else if self.array[1] < 3.0 || self.array[0] < 100.0 { s += &("e".repeat((self.array[1] - 1.0) as usize) + &ldts(f64::powf(10.0, self.array[0] - f64::floor(self.array[0])), false) + "e" + &ldts(f64::floor(self.array[0]), true)); }
        else if self.array[1] < 8.0 { s += &("e".repeat(self.array[1] as usize) + &ldts(self.array[0], true)); }
        else { s += &("(10^)^".to_owned() + &ldts(*self.array.get(1).unwrap_or(&0.0), true) + " " + &ldts(*self.array.get(0).unwrap_or(&0.0), true)); }
        write!(f, "{s}")
    }
}

fn test_single(a : &OmegaNum, b : &OmegaNum) -> () {
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
}

fn test_oom(max_pow: u64) -> () {
    let mut a = OmegaNum::new(random_range(0.0..10.0));
    let mut b = OmegaNum::new(random_range(0.0..10.0));

    for i in 0..max_pow {
        (a, b) = (a.arrow(i)(&b), b.arrow(i.wrapping_sub(1))(&a));
    }

    test_single(&a, &b);
}

fn tests() -> () {
    unsafe { std::env::set_var("RUST_BACKTRACE", "1") }
    for i in 0..4 {
        test_oom(i);
    }
}


fn main() {
    tests();

    /*
    let mut a = OmegaNum::new(10.0);
    let b = OmegaNum::new(1e100).pow(&OmegaNum::new(1e9));

    let start = Instant::now();
    let mut i = 0_u8;
    let mut it = 0_u64;
    let mut q = vec![];
    let mut tadt = 0_f64;
    q.resize(256, 0.0);
    loop {
        let prev = Instant::now();
        a.pow_assign(&b);
        i = i.wrapping_add(1);
        it += 1;
        let elp = start.elapsed().as_secs_f64();
        let dt = prev.elapsed().as_nanos() as f64 / 1000.0;
        q[i as usize] = dt;
        let adt = q.iter().sum::<f64>() / q.len() as f64;
        let acps = 1000000.0 / adt;

        if it % 65536 == 0 {
            tadt = tadt * (1.0 - 1.0 / (it / 65536) as f64) + adt / (it / 65536) as f64;
            let tacps = 1000000.0 / tadt;
            println!("[{elp:.2}] ({dt:.2} μs) (avg: {adt:.2} μs / {acps:.0} cps) (tot_avg: {tadt:.2} μs / {tacps:.0} cps) ({it} calculations elapsed): {a}");
        }
    }
    */

}
