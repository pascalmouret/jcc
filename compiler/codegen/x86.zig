const ast = @import("../parser.zig").ast;

const Register = enum {
    eax,
};

pub fn program_to_x86(program: ast.Program) Progam {
    return Progam.from_program(program);
}

const Progam = struct {
    function_definition: FunctionDefinition,
    pub fn from_program(program: ast.Program) Progam {
        return Progam{ .function_definition = FunctionDefinition.from_function(program.function) };
    }
};

const FunctionDefinition = struct {
    name: []u8,
    instructions: []const Instruction,
    pub fn from_function(function: ast.Function) FunctionDefinition {
        return FunctionDefinition{ .name = function.name.name, .instructions = Instruction.from_statement(function.body) };
    }
};

const Instruction = union(enum) {
    mov: Mov,
    ret: Ret,
    pub fn from_statement(statement: ast.Statement) []const Instruction {
        switch (statement) {
            .ret => |ret| {
                return &[_]Instruction{
                    Instruction{
                        .mov = Mov{
                            .src = Operand{ .immediate = Immediate{ .int = ret.expression.constant.value } },
                            .dst = Operand{ .register = Register.eax },
                        },
                    },
                    Instruction{ .ret = Ret{} },
                };
            },
        }
    }
};

const Ret = struct {};

const Mov = struct {
    src: Operand,
    dst: Operand,
};

const Operand = union(enum) {
    register: Register,
    immediate: Immediate,
};

const Immediate = union(enum) {
    int: isize,
};
