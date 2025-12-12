use std::ops::*;
use std::{collections::HashMap, str::FromStr, sync::LazyLock};

use godot::prelude::*;

use crate::omeganum::{self, OmegaNum};


#[derive(GodotClass, Debug)]
#[class(base=RefCounted)]
struct HyperCalc {
    registers : HashMap<StringName, OmegaNum>,
    call_stack : Vec<HyperPtr>,

    eip : HyperPtr,
    halted : bool,
    
    program : HashMap<StringName, HyperSegment>,

    #[base]
    base : Base<RefCounted>
}

pub static START_SEGMENT : LazyLock<StringName> = LazyLock::new(|| StringName::from_str("__start").unwrap());
#[derive(Debug, Clone)]
struct HyperPtr {
    segment: StringName,
    address: usize
}

impl HyperPtr {
    fn new(segment : StringName, address : usize) -> Self { HyperPtr { segment, address } }
    fn increment(&mut self) { self.address += 1; }
}

type HyperSegment = Vec<HyperInstruction>;

#[derive(Debug, Clone)]
enum HyperInstruction {
    Noop(),

    Call(HyperPtr),
    Ret(),
    Exit(),

    Jmp(usize),
    Jeq(usize, StringName, StringName),
    Jne(usize, StringName, StringName),
    Jgt(usize, StringName, StringName),
    Jge(usize, StringName, StringName),
    Jlt(usize, StringName, StringName),
    Jle(usize, StringName, StringName),

    Mov(StringName, StringName),

    Add(StringName, StringName),
    Sub(StringName, StringName),
    Mul(StringName, StringName),
    Div(StringName, StringName),
    Rem(StringName, StringName),
    Pow(StringName, StringName),
    Root(StringName, StringName),
    Log(StringName, StringName),
    Tetr(StringName, StringName),
    Tetr3(StringName, StringName, StringName),
    Slog(StringName, StringName),
    Arrow(StringName, StringName, i64),
    Arrow3(StringName, StringName, StringName),
    Floor(StringName),

}



#[godot_api]
impl IRefCounted for HyperCalc {
    fn init(base: Base<RefCounted>) -> Self {
        HyperCalc {
            registers: HashMap::new(),
            call_stack: vec![],
            eip: HyperPtr::new(START_SEGMENT.clone(), 0_usize),
            halted: true,
            program: HashMap::new(),
            base
        }
    }
}

impl HyperCalc {
    fn fetch_instruction(&self, ptr: HyperPtr) -> HyperInstruction {
        self.program.get(&ptr.segment).expect("").get(ptr.address).cloned().unwrap_or(HyperInstruction::Exit())
    }

    fn get_register(&self, eax : &StringName) -> &OmegaNum {
        self.registers.get(eax).unwrap_or_else(|| &omeganum::ZERO)
    }

    fn gr(&self, eax : &StringName) -> &OmegaNum {
        self.get_register(eax)
    }

    fn get_register_mut(&mut self, eax : &StringName) -> &mut OmegaNum {
        if !self.registers.contains_key(eax) { self.registers.insert(eax.clone(), omeganum::ZERO.clone()); }
        self.registers.get_mut(eax).unwrap_or_else(|| panic!("Register %{eax} could not be added to the registers registry?"))
    }

    fn gr_mut(&mut self, eax : &StringName) -> &mut OmegaNum {
        self.get_register_mut(eax)
    }

    fn run_one_instr(&mut self) {
        let instr = self.fetch_instruction(self.eip.clone());
        self.eip.increment();
        use HyperInstruction as Hi;
        match instr {
            Hi::Noop() => (),
            Hi::Call(ptr) => {
                let old_eip = std::mem::replace(&mut self.eip, ptr);
                self.call_stack.push(old_eip);
            },
            Hi::Ret() => self.eip = match self.call_stack.pop() {
                Some(ptr) => ptr,
                None => { self.halted = true; return }
            },
            Hi::Exit() => self.halted = true,
            Hi::Jmp(tgt) => self.eip.address = tgt,
            Hi::Jeq(tgt, eax, ebx) => if self.gr(&eax).eq(self.gr(&ebx)) { self.eip.address = tgt },
            Hi::Jne(tgt, eax, ebx) => if self.gr(&eax).ne(self.gr(&ebx)) { self.eip.address = tgt },
            Hi::Jgt(tgt, eax, ebx) => if self.gr(&eax).gt(self.gr(&ebx)) { self.eip.address = tgt },
            Hi::Jge(tgt, eax, ebx) => if self.gr(&eax).ge(self.gr(&ebx)) { self.eip.address = tgt },
            Hi::Jlt(tgt, eax, ebx) => if self.gr(&eax).lt(self.gr(&ebx)) { self.eip.address = tgt },
            Hi::Jle(tgt, eax, ebx) => if self.gr(&eax).le(self.gr(&ebx)) { self.eip.address = tgt },
            
            Hi::Mov(dst, src) => { let src = self.gr(&src).clone(); *self.gr_mut(&dst) = src },
            Hi::Add(dst, src) => { let src = self.gr(&src).clone(); self.gr_mut(&dst).add_assign(&src) },
            Hi::Sub(dst, src) => { let src = self.gr(&src).clone(); self.gr_mut(&dst).sub_assign(&src) },
            Hi::Mul(dst, src) => { let src = self.gr(&src).clone(); self.gr_mut(&dst).mul_assign(&src) },
            Hi::Div(dst, src) => { let src = self.gr(&src).clone(); self.gr_mut(&dst).div_assign(&src) },
            Hi::Rem(dst, src) => { let src = self.gr(&src).clone(); self.gr_mut(&dst).rem_assign(&src) },
            Hi::Pow(dst, src) => { let src = self.gr(&src).clone(); self.gr_mut(&dst).pow_assign(&src) },
            Hi::Root(dst, src) => { let src = self.gr(&src).clone(); self.gr_mut(&dst).root_assign(&src) },
            Hi::Log(dst, src) => { let src = self.gr(&src).clone(); self.gr_mut(&dst).log10_assign(); self.gr_mut(&dst).div_assign(&src.log10()) },
            Hi::Tetr(dst, src) => { let src = self.gr(&src).clone(); self.gr_mut(&dst).tetrate_assign(&src) },
            Hi::Tetr3(dst, src, from) => { let src = self.gr(&src).clone(); let from = self.gr(&from).clone(); self.gr_mut(&dst).tetrate_from_assign(&src, &from) },
            Hi::Slog(dst, src) => { let src = self.gr(&src).clone(); self.gr_mut(&dst).slog_assign(&src) },
            Hi::Arrow(dst, src, i) => { let src = self.gr(&src).clone(); *self.gr_mut(&dst) = self.gr_mut(&dst).arrow(i)(&src) },
            Hi::Arrow3(dst, src, i) => { let src = self.gr(&src).clone(); let i = self.gr(&i).to_number() as i64; *self.gr_mut(&dst) = self.gr_mut(&dst).arrow(i)(&src) },
            Hi::Floor(dst) => self.gr_mut(&dst).floor_assign(),
        }
    }
}

#[godot_api]
impl HyperCalc {
    
}