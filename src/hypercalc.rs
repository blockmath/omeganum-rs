use std::sync::{Arc, Mutex};
use std::{ops::*, thread};
use std::thread::JoinHandle;
use std::{collections::HashMap, str::FromStr, sync::LazyLock};

use regex::Regex;
use godot::prelude::*;

use crate::omeganum::{self, OmegaNum};

#[derive(Debug)]
struct HyperCalcInner {
    registers : HashMap<StringName, OmegaNum>,
    call_stack : Vec<HyperPtr>,

    tables : HashMap<StringName, Vec<OmegaNum>>,

    eip : HyperPtr,
    halted : bool,
    code : i64,
    
    program : HashMap<StringName, HyperSegment>,
}

#[derive(GodotClass, Debug)]
#[class(base=RefCounted)]
struct HyperCalc {
    inner : Arc<Mutex<HyperCalcInner>>,

    thread : Option<JoinHandle<i64>>,

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

impl std::fmt::Display for HyperPtr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.segment, self.address)
    }
}

type HyperSegment = Vec<HyperInstruction>;

#[derive(Debug, Clone)]
enum HyperInstruction {
    Noop(),

    Call(HyperPtr),
    Ret(),
    Exit(),
    Abort(i64),

    Jmp(usize),
    Jeq(usize, StringName, StringName),
    Jne(usize, StringName, StringName),
    Jgt(usize, StringName, StringName),
    Jge(usize, StringName, StringName),
    Jlt(usize, StringName, StringName),
    Jle(usize, StringName, StringName),
    Jr(usize, StringName),

    Movi(StringName, f64),
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

    TableLoad(StringName, StringName, usize),
    TableLoadI(StringName, StringName, StringName),
    TableStore(StringName, usize, StringName),
    TableStoreI(StringName, StringName, StringName)
}



#[godot_api]
impl IRefCounted for HyperCalc {
    fn init(base: Base<RefCounted>) -> Self {
        HyperCalc {
            inner: Arc::new(Mutex::new(HyperCalcInner {
                registers: HashMap::new(),
                call_stack: vec![],
                tables: HashMap::new(),
                eip: HyperPtr::new(START_SEGMENT.clone(), 0_usize),
                halted: true,
                code: 0_i64,
                program: HashMap::new(),
            })),
            thread: None,
            base
        }
    }
}

impl HyperCalcInner {
    fn fetch_instruction(&self, ptr: HyperPtr) -> HyperInstruction {
        self.program.get(&ptr.segment).unwrap_or(&vec![]).get(ptr.address).cloned().unwrap_or(HyperInstruction::Exit())
    }

    fn get_register(&self, eax : &StringName) -> &OmegaNum {
        self.registers.get(eax).unwrap_or_else(|| &omeganum::ZERO)
    }

    fn gr(&self, eax : &StringName) -> &OmegaNum {
        self.get_register(eax)
    }

    fn get_register_mut(&mut self, eax : &StringName) -> &mut OmegaNum {
        if !self.registers.contains_key(eax) { self.registers.insert(eax.clone(), omeganum::ZERO.clone()); }
        self.registers.get_mut(eax).unwrap_or_else(|| panic!("Register {eax} could not be added to the registers registry?"))
    }

    fn gr_mut(&mut self, eax : &StringName) -> &mut OmegaNum {
        self.get_register_mut(eax)
    }

    fn get_table(&self, tbl : &StringName, i : usize) -> OmegaNum {
        match self.tables.get(tbl) {
            Some(table) => match table.get(i) {
                Some(v) => v.clone(),
                None => OmegaNum::new(0.0)
            },
            None => OmegaNum::new(0.0)
        }
    }

    fn get_table_mut(&mut self, tbl : &StringName, i : usize) -> Option<&mut OmegaNum> {
        match self.tables.get_mut(tbl) {
            Some(table) => table.get_mut(i),
            None => None
        }
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
            Hi::Abort(code) => { self.halted = true; self.code = code; godot_error!("Program aborted with code {code} at address {0}", self.eip) },
            Hi::Jmp(tgt) => self.eip.address = tgt,
            Hi::Jeq(tgt, eax, ebx) => if self.gr(&eax).eq(self.gr(&ebx)) { self.eip.address = tgt },
            Hi::Jne(tgt, eax, ebx) => if self.gr(&eax).ne(self.gr(&ebx)) { self.eip.address = tgt },
            Hi::Jgt(tgt, eax, ebx) => if self.gr(&eax).gt(self.gr(&ebx)) { self.eip.address = tgt },
            Hi::Jge(tgt, eax, ebx) => if self.gr(&eax).ge(self.gr(&ebx)) { self.eip.address = tgt },
            Hi::Jlt(tgt, eax, ebx) => if self.gr(&eax).lt(self.gr(&ebx)) { self.eip.address = tgt },
            Hi::Jle(tgt, eax, ebx) => if self.gr(&eax).le(self.gr(&ebx)) { self.eip.address = tgt },
            Hi::Jr(tgt, eax) => self.eip.address = tgt + self.gr(&eax).to_number() as usize,
            
            Hi::Movi(dst, imm) => { *self.gr_mut(&dst) = OmegaNum::new(imm) }
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

            Hi::TableLoad(dst, tbl, i) => { *self.gr_mut(&dst) = self.get_table(&tbl, i).clone() }
            Hi::TableLoadI(dst, tbl, edp) => { *self.gr_mut(&dst) = self.get_table(&tbl, self.gr(&edp).to_number() as usize).clone() }
            Hi::TableStore(tbl, i, src) => { let src = self.gr(&src).clone(); match self.get_table_mut(&tbl, i) { Some(r) => *r = src, None => () } }
            Hi::TableStoreI(tbl, edp, src) => { let src = self.gr(&src).clone(); match self.get_table_mut(&tbl, self.gr(&edp).to_number() as usize) { Some(r) => *r = src, None => () } }
        }
    }

    fn run_synch(&mut self) -> i64 {
        while !self.halted { self.run_one_instr(); }
        self.code
    }
}

#[godot_api]
impl HyperCalc {
    #[func]
    fn new() -> Gd<Self> {
        Gd::from_init_fn(HyperCalc::init)
    }

    #[func]
    fn compile_segment(&mut self, segment: StringName, program : String) -> String {
        let mut inner = self.inner.lock().expect("Mutex was poisoned, cannot continue");
        inner.program.remove(&segment);
        let mut seg = vec![HyperInstruction::Noop(); program.split('\n').count()];

        let mut sym_table : HashMap<StringName, usize> = HashMap::new();

        // Enumerate symbols
        for (i, line) in program.split('\n').enumerate() {
            static LABEL_RE : LazyLock<Regex> = LazyLock::new(|| Regex::new("^\\s*([_a-zA-Z][_a-zA-Z0-9])*:\\s*$").expect("Unable to compile label regex"));
            let m = LABEL_RE.captures(line);
            if let Some(c) = m {
                sym_table.insert(c.get(1).expect("Match without capture should be impossible!").as_str().into(), i);
            }
        }

        // Assemble instructions
        for (i, line) in program.split('\n').enumerate() {
            static INSTR_RE : LazyLock<Regex> = LazyLock::new(|| Regex::new("^\\s*([_a-zA-Z][_a-zA-Z0-9]*)(\\s+(.*?))?\\s*$").expect("Unable to compile instruction regex"));
            let m = INSTR_RE.captures(line);
            if let Some(c) = m {
                static ARGS_RE : LazyLock<Regex> = LazyLock::new(|| Regex::new("([^ ,]+)").expect("Unable to compile arguments regex"));
                let opc = match c.get(1) { Some(m) => m.as_str(), None => "nop" };
                let args : Vec<&str> = ARGS_RE.captures_iter(match c.get(2) { Some(m) => m.as_str(), None => "" }).filter_map(|c| c.get(1)).map(|m| m.as_str()).collect();
                
                use HyperInstruction as Hi;
                seg[i] = match opc.to_ascii_lowercase().as_str() {
                    "nop" | "noop" => Hi::Noop(),
                    "jsr" | "call" => Hi::Call(HyperPtr {
                        segment: (*match args.get(0) { Some(v) => v, None => return format!("Expected a segment name (line {i})") }).into(),
                        address: match (*match args.get(1) { Some(v) => v, None => return format!("Expected an address (line {i})") }).parse::<usize>() { Ok(v) => v, Err(e) => return format!("Unable to parse address (line {i}): {e}") }
                    }),
                    "ret" | "rts" | "rti" => Hi::Ret(),
                    "hlt" | "halt" | "exit" => Hi::Exit(),
                    "hcf" | "err" | "abort" | "panic" => Hi::Abort(match args.get(0).map_or("0", |v|v).parse::<i64>() { Ok(v) => v, Err(e) => return format!("Unable to parse abort value (line {i}): {e}") }),

                    "jmp" | "jump" | "bra" | "goto" => {
                        let tgt = *match args.get(0) { Some(v) => v, None => return format!("Expected an address or label (line {i})") };
                        Hi::Jmp(match tgt.parse::<usize>() {
                            Ok(v) => v,
                            Err(_) => match sym_table.get(&tgt.into()) { Some(v) => *v, None => return format!("Unknown symbol (line {i}): {tgt}") }
                        })
                    },
                    "jeq" | "beq" => {
                        let tgt = *match args.get(0) { Some(v) => v, None => return format!("Expected an address or label (line {i})") };
                        Hi::Jeq(match tgt.parse::<usize>() {
                            Ok(v) => v,
                            Err(_) => match sym_table.get(&tgt.into()) { Some(v) => *v, None => return format!("Unknown symbol (line {i}): {tgt}") }
                        },
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(2) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    )},
                    "jne" | "bne" => {
                        let tgt = *match args.get(0) { Some(v) => v, None => return format!("Expected an address or label (line {i})") };
                        Hi::Jne(match tgt.parse::<usize>() {
                            Ok(v) => v,
                            Err(_) => match sym_table.get(&tgt.into()) { Some(v) => *v, None => return format!("Unknown symbol (line {i}): {tgt}") }
                        },
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(2) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    )},
                    "jgt" | "bgt" => {
                        let tgt = *match args.get(0) { Some(v) => v, None => return format!("Expected an address or label (line {i})") };
                        Hi::Jgt(match tgt.parse::<usize>() {
                            Ok(v) => v,
                            Err(_) => match sym_table.get(&tgt.into()) { Some(v) => *v, None => return format!("Unknown symbol (line {i}): {tgt}") }
                        },
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(2) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    )},
                    "jge" | "bge" => {
                        let tgt = *match args.get(0) { Some(v) => v, None => return format!("Expected an address or label (line {i})") };
                        Hi::Jge(match tgt.parse::<usize>() {
                            Ok(v) => v,
                            Err(_) => match sym_table.get(&tgt.into()) { Some(v) => *v, None => return format!("Unknown symbol (line {i}): {tgt}") }
                        },
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(2) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    )},
                    "jlt" | "blt" => {
                        let tgt = *match args.get(0) { Some(v) => v, None => return format!("Expected an address or label (line {i})") };
                        Hi::Jlt(match tgt.parse::<usize>() {
                            Ok(v) => v,
                            Err(_) => match sym_table.get(&tgt.into()) { Some(v) => *v, None => return format!("Unknown symbol (line {i}): {tgt}") }
                        },
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(2) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    )},
                    "jle" | "ble" => {
                        let tgt = *match args.get(0) { Some(v) => v, None => return format!("Expected an address or label (line {i})") };
                        Hi::Jle(match tgt.parse::<usize>() {
                            Ok(v) => v,
                            Err(_) => match sym_table.get(&tgt.into()) { Some(v) => *v, None => return format!("Unknown symbol (line {i}): {tgt}") }
                        },
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(2) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    )},
                    "jr" | "br" => {
                        let tgt = *match args.get(0) { Some(v) => v, None => return format!("Expected an address or label (line {i})") };
                        Hi::Jr(match tgt.parse::<usize>() {
                            Ok(v) => v,
                            Err(_) => match sym_table.get(&tgt.into()) { Some(v) => *v, None => return format!("Unknown symbol (line {i}): {tgt}") }
                        },
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    )},

                    "movi" | "imov" | "lda" | "ld" => Hi::Movi(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        match (*match args.get(1) { Some(v) => v, None => return format!("Expected a number (line {i})") }).trim_start_matches('#').parse::<f64>() { Ok(v) => v, Err(e) => return format!("Unable to parse number (line {i}): {e}") }
                    ),

                    "mov" => Hi::Mov(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    ),
                    "add" => Hi::Add(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    ),
                    "sub" => Hi::Sub(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    ),
                    "mul" => Hi::Mul(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    ),
                    "div" => Hi::Div(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    ),
                    "rem" | "mod" => Hi::Rem(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    ),
                    "pow" => Hi::Pow(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    ),
                    "root" | "rpow" => Hi::Root(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    ),
                    "log" => Hi::Log(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    ),
                    "tet" | "tetr" => Hi::Tetr(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    ),
                    "tet3" | "tetf" | "tetr3" | "tetrf" => Hi::Tetr3(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(2) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    ),
                    "slog" => Hi::Slog(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    ),
                    "arrow" | "boper" | "knuth" | "knu" => Hi::Arrow(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        match (*match args.get(2) { Some(v) => v, None => return format!("Expected a number (line {i})") }).trim_start_matches('#').parse::<i64>() { Ok(v) => v, Err(e) => return format!("Unable to parse number (line {i}): {e}") }
                    ),
                    "arrow3" | "toper" | "knuth3" | "knu3" | "knuthf" | "knuf" => Hi::Arrow3(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(2) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    ),
                    "floor" => Hi::Floor(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    ),

                    "lwtable" | "tload" | "ldt" => Hi::TableLoad(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a table (line {i})") }).into(),
                        match (*match args.get(2) { Some(v) => v, None => return format!("Expected a number (line {i})") }).trim_start_matches('#').parse::<usize>() { Ok(v) => v, Err(e) => return format!("Unable to parse number (line {i}): {e}") }
                    ),

                    "lwtablei" | "tloadi" | "ldi" => Hi::TableLoadI(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a table (line {i})") }).into(),
                        (*match args.get(2) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    ),

                    "swtable" | "tstore" | "stt" => Hi::TableStore(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a table (line {i})") }).into(),
                        match (*match args.get(1) { Some(v) => v, None => return format!("Expected a number (line {i})") }).trim_start_matches('#').parse::<usize>() { Ok(v) => v, Err(e) => return format!("Unable to parse number (line {i}): {e}") },
                        (*match args.get(2) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    ),

                    "swtablei" | "tstorei" | "sti" => Hi::TableStoreI(
                        (*match args.get(0) { Some(v) => v, None => return format!("Expected a table (line {i})") }).into(),
                        (*match args.get(1) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into(),
                        (*match args.get(2) { Some(v) => v, None => return format!("Expected a register (line {i})") }).into()
                    ),

                    _ => return format!("Unknown opcode {opc}")
                }
            }
        }

        inner.program.insert(segment, seg);

        drop(inner);

        "".to_owned()
    }

    #[func]
    fn compile_table(&mut self, table_segment: StringName, table: Array<Variant>) -> String {
        let mut inner = self.inner.lock().expect("Mutex was poisoned, cannot continue");
        inner.tables.remove(&table_segment);
        let mut tbl: Vec<OmegaNum> = Vec::with_capacity(table.len());
        for elem in table.iter_shared() {
            tbl.push(match OmegaNum::of_variant(elem) {
                Ok(value) => value,
                Err(err) => return err
            });
        }
        inner.tables.insert(table_segment, tbl);

        drop(inner);

        "".to_owned()
    }

    #[func]
    fn run_async(&mut self) {
        let mutex_arc = self.inner.clone();
        self.thread = Some(thread::spawn(move || mutex_arc.lock().expect("poisoned").run_synch()));
    }

    #[func]
    fn is_finished(&self) -> bool {
        match &self.thread { Some(v) => v.is_finished(), None => true }
    }

    #[func]
    fn await_finished(&mut self) -> i64 {
        if self.thread.is_some() {
            std::mem::replace(&mut self.thread, None)
            .unwrap().join().expect("HyperCalc thread panicked")
        } else {
            self.inner.lock().expect("HyperCalc mutex poisoned").code
        }
    }
}