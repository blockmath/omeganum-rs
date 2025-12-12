pub mod omeganum;

use std::{ops::*, str::FromStr};

use godot::prelude::*;

use crate::omeganum::OmegaNum as OmegaNumInner;


struct OmegaNumExtension;

#[gdextension]
unsafe impl ExtensionLibrary for OmegaNumExtension {}

#[derive(GodotClass, Debug)]
#[class(base=RefCounted)]
struct OmegaNum {
    inner : OmegaNumInner,
    #[base]
    base : Base<RefCounted>
}

#[godot_api]
impl IRefCounted for OmegaNum {
    fn init(base: Base<RefCounted>) -> Self {
        OmegaNum {
            inner: OmegaNumInner::new(0.0),
            base
        }
    }
}

#[godot_api]
impl OmegaNum {
    #[func]
    fn sign(&self) -> i8 {
        self.inner.sign()
    }

    #[func]
    fn isnan(&self) -> bool {
        self.inner.isnan()
    }

    #[func]
    fn isinf(&self) -> bool {
        self.inner.isinf()
    }

    #[func]
    fn isint(&self) -> bool {
        self.inner.isint()
    }

    #[func]
    fn normalize(&mut self) {
        self.inner.normalize();
    }

    #[func]
    fn arrow(&self, arrows: i64, other: Gd<Self>) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: self.inner.arrow(arrows)(&other.bind().inner), base }
        })
    }

    #[func]
    fn min(a: Gd<Self>, b: Gd<Self>) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: OmegaNumInner::min(&a.bind().inner, &b.bind().inner), base }
        })
    }

    #[func]
    fn max(a: Gd<Self>, b: Gd<Self>) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: OmegaNumInner::max(&a.bind().inner, &b.bind().inner), base }
        })
    }

    #[func]
    fn abs(&self) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: self.inner.abs(), base }
        })
    }

    #[func]
    fn to_number(&self) -> f64 {
        self.inner.to_number()
    }

    #[func]
    fn set_max_arrow(value: i64) {
        if OmegaNumInner::set_max_arrow(value).is_err() {
            godot_error!("MAX_ARROW could not be set because the mutex is poisoned. You need to call `reset_max_arrow()` first.")
        }
    }

    #[func]
    fn reset_max_arrow() {
        OmegaNumInner::reset_max_arrow()
    }

    #[func]
    fn get_max_arrow() -> i64 {
        match OmegaNumInner::get_max_arrow() {
            Ok(value) => value,
            Err(_) => { godot_error!("MAX_ARROW could not be read because the mutex is poisoned. You need to call `reset_max_arrow()` first."); 0 }
        }
    }

    #[func]
    fn new() -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: OmegaNumInner::new(0.0), base }
        })
    }

    #[func]
    fn from_number(value: f64) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: OmegaNumInner::new(value), base }
        })
    }

    #[func]
    fn parse(str_value: GString) -> Option<Gd<Self>> {
        match OmegaNumInner::parse(str_value.to_string()) {
            Some(parsed_value) => Some(Gd::from_init_fn(|base| {
                OmegaNum { inner: parsed_value, base }
            })),
            None => {
                godot_error!("Unable to parse OmegaNum '{}'", str_value.to_string());
                None
            }
        }
    }

    #[func]
    fn to_string(&self) -> GString {
        match GString::from_str(&self.inner.to_string()) {
            Ok(value) => value,
            Err(_err) => unreachable!()
        }
    }

    /// impl PartialEq, PartialOrd

    #[func]
    fn eq(&self, other: Gd<Self>) -> bool {
        self.inner == other.bind().inner
    }

    #[func]
    fn ne(&self, other: Gd<Self>) -> bool {
        self.inner != other.bind().inner
    }

    #[func]
    fn gt(&self, other: Gd<Self>) -> bool {
        self.inner > other.bind().inner
    }

    #[func]
    fn ge(&self, other: Gd<Self>) -> bool {
        self.inner >= other.bind().inner
    }

    #[func]
    fn lt(&self, other: Gd<Self>) -> bool {
        self.inner < other.bind().inner
    }

    #[func]
    fn le(&self, other: Gd<Self>) -> bool {
        self.inner <= other.bind().inner
    }

    /// impl std::ops::AnyAss
    
    #[func]
    fn add_ass(&mut self, other: Gd<Self>) {
        self.inner.add_assign(&other.bind().inner);
    }

    #[func]
    fn sub_ass(&mut self, other: Gd<Self>) {
        self.inner.sub_assign(&other.bind().inner);
    }

    #[func]
    fn neg_ass(&mut self) {
        self.inner.neg_assign();
    }

    #[func]
    fn mul_ass(&mut self, other: Gd<Self>) {
        self.inner.mul_assign(&other.bind().inner);
    }

    #[func]
    fn div_ass(&mut self, other: Gd<Self>) {
        self.inner.div_assign(&other.bind().inner);
    }

    #[func]
    fn rem_ass(&mut self, other: Gd<Self>) {
        self.inner.rem_assign(&other.bind().inner);
    }

    #[func]
    fn pow_ass(&mut self, other: Gd<Self>) {
        self.inner.pow_assign(&other.bind().inner);
    }

    #[func]
    fn log_ass(&mut self) {
        self.inner.log10_assign();
    }

    #[func]
    fn tetr_ass(&mut self, other: Gd<Self>) {
        self.inner.tetrate_assign(&other.bind().inner);
    }

    #[func]
    fn slog_ass(&mut self) {
        self.inner.slog10_assign();
    }

    #[func]
    fn arrow_ass(&mut self, arrows: i64, other: Gd<Self>) {
        self.inner = self.inner.arrow(arrows)(&other.bind().inner);
    }

    #[func]
    fn floor_ass(&mut self) {
        self.inner.floor_assign();
    }

    /// impl std::ops::Any

    #[func]
    fn add(&self, other: Gd<Self>) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: &self.inner + &other.bind().inner, base }
        })
    }

    #[func]
    fn sub(&self, other: Gd<Self>) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: &self.inner - &other.bind().inner, base }
        })
    }

    #[func]
    fn neg(&self) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: -&self.inner, base }
        })
    }

    #[func]
    fn mul(&self, other: Gd<Self>) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: &self.inner * &other.bind().inner, base }
        })
    }

    #[func]
    fn div(&self, other: Gd<Self>) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: &self.inner / &other.bind().inner, base }
        })
    }

    #[func]
    fn rem(&self, other: Gd<Self>) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: &self.inner % &other.bind().inner, base }
        })
    }

    #[func]
    fn inv(&self) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: self.inner.recip(), base }
        })
    }

    #[func]
    fn pow(&self, other: Gd<Self>) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: self.inner.pow(&other.bind().inner), base }
        })
    }

    #[func]
    fn log(&self) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: self.inner.log10(), base }
        })
    }

    #[func]
    fn tetr(&self, other: Gd<Self>) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: self.inner.tetrate(&other.bind().inner), base }
        })
    }

    #[func]
    fn slog(&self) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: self.inner.slog10(), base }
        })
    }

    #[func]
    fn pent(&self, other: Gd<Self>) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: self.inner.pentate(&other.bind().inner), base }
        })
    }

    #[func]
    fn floor(&self) -> Gd<Self> {
        Gd::from_init_fn(|base| {
            OmegaNum { inner: self.inner.floor(), base }
        })
    }

}

impl std::fmt::Display for OmegaNum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.inner.to_string())
    }
}