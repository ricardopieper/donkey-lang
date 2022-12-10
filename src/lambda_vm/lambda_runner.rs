use crate::{
    donkey_vm::vm::{memory::Memory, runner::ControlRegisterValues},
    semantic::mir::BlockId,
};

use super::lambda_compiler::{LambdaFunction, LambdaProgram};

pub struct LambdaRunner {
    program: LambdaProgram,
}

impl LambdaRunner {
    pub fn new(prog: LambdaProgram) -> LambdaRunner {
        LambdaRunner { program: prog }
    }

    pub fn next(
        &self,
        mem: &mut Memory,
        ctrl: &mut ControlRegisterValues,
        function: &LambdaFunction,
        block: BlockId,
    ) {
        let mut current_block = block;
        loop {
            let block = unsafe { function.blocks.get_unchecked(current_block.0) };
            (block.code)(mem, ctrl);
            let next_block = (block.finish)(mem, ctrl);
            match next_block {
                Some(x) => current_block = x,
                None => {
                    break;
                }
            };
        }
    }

    pub fn run(&self, mem: &mut Memory, ctrl: &mut ControlRegisterValues) {
        if self.program.functions.len() > 1 {
            panic!("Only one function supported for now")
        }
        let func = &self.program.functions[0];
        ctrl.sp = mem.stack_start + func.bp_offset;
        self.next(mem, ctrl, func, BlockId(0));
    }
}
