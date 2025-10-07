pub enum MInstr {
    Set {
        l: MReg,
        r: MExpr,
    },
    SetAddr {
        l: MAddrReg,
        r: MAddrExpr,
    },
    Store {
        l: MAddr,
        val: MOperand,
    },
    MemCpy {
        from: MAddr,
        to: MAddr,
        len: MOperand,
    },
    MemMove {
        from: MOperand,
        to: MOperand,
        len: MOperand,
    },
    Return(Option<MOperand>),
    Marker(u32),
    Goto(u32),
    Expr(MExpr),
    Branch {
        if_: MOperand,
        yes: u32,
        no: u32,
    },
}

pub type MReg = u32;
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum MMemLoc {
    Reg(MReg),
    Var(VarID),
    Global(VarName),
}

impl fmt::Debug for MMemLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use MMemLoc::*;

        match self {
            Reg(reg) => write!(f, "{:?}", reg),
            Var(var) => write!(f, "{:?}", var),
            Global(var) => write!(f, "{}", var),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum MAddr {
    Reg(MReg),
    Var(VarID),
}

impl fmt::Debug for MAddr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use MAddr::*;

        match self {
            Reg(reg) => write!(f, "{:?}", reg),
            Var(var) => write!(f, "@{:?}", var),
        }
    }
}

pub enum MOperand {
    Memloc(MMemLoc),
    Int { size: u32, val: u64 },
    Float { size: u32, val: f64 },
    Function(FuncId),
    Bool(bool),
}

#[derive(Debug)]
pub enum MExpr {
    MemLoc(MMemLoc),
    Addr(MAddr),
    BinOp {
        left_type: Type,
        op: Opcode,
        l: MOperand,
        r: MOperand,
    },
    UnarOp {
        ret_type: Type,
        op: Opcode,
        hs: MOperand,
    },
    Call {
        typ: FuncType,
        f: MCallable,
        a: Vec<MOperand>,
        sret: Option<MMemLoc>,
    },
    Free {
        ptr: MOperand,
    },
    Ref {
        on: MAddr,
    },
    Alloc {
        len: MOperand,
    },
    Deref {
        to: Type,
        on: MMemLoc,
    },
}

#[derive(Debug)]
pub enum MCallable {
    Func(FuncId),
    FirstClass(MMemLoc),
}

#[derive(Debug)]
pub enum MAddrExpr {
    Expr(MExpr),
    Addr(MAddr),
    Member {
        object_type: Type,
        object: MAddr,
        field_index: u32,
    },
    Index {
        array_type: Type,
        element_type: Type,
        object: MAddr,
        index: MOperand,
    },
}
