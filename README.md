# Toy Compiler

This is a compiler frontend (own codegen impl coming later!), for a language that i've created.

## Core Functionality Required

- [x] variables
- [x] operations: (boolean, logical//bitwise, binary)
- [x] control flow branches: (if/else)
- [x] loops: (while/for)
- [x] types
- [x] functions: (recursion << should be free?)
- [ ] user defined struct
- [ ] array
- [ ] llvm codegen
- [ ] modules (format as odin, all files in a directory include each other)
- [ ] c interop (akin to odin)

### Implementation List

- [x] if
  - [x] rework lexer to handle multi-symbol keywords (i.e "==" or "<=")
  - [x] parse boolean comparison
  - [x]  else & else if parsing
  - [x] code generation
    - [x] invert 'jump' conditions
    - [ ] unsigned vs signed comparison (diff jump instructions) (no types, all signed)
    - [x] binary expr conditions
      - conditional expr, either has explicity bool comparison or implicit, expr = lhs: (lhs > 0)
      - cmp reg1, reg2 ; compare arguments
      - set(EQUIVALENCE e.g e, le) al; sets register 'al' (8 bit) to 1,0 depending on cmp flag
      - movzx output_reg, al ; Move Zero Xtend. copies 'al' into 'output_reg' && zero init reg1 bits.  
  - [x] types of scope
    - inherits variables from parent scope (if, else if, else)
    - doesn't (new function, UNLESS class, inherits 'self')
- [x] split 'TokenKind': 'Symbol' .. 'LogicalOp' .. 'BinaryOp' .. etc
- [x] comments
- [x] re-design multi-symbol in Lexer: match against longest multi-symbol to shortest, until finds match (or illegal token.)
- [x] update grammar to match code.
- [x] variable reassignment (mutability)
- [x] Operators
  - [x] modulus '%' op
  - [x] Associativity
  - [x] binary
  - [x] logical
    - i.e: if (5)  | 5 != 0 so -> if (true)
    - i.e: if (!5) | 5 == 0 so -> if (false)  
    - eval expr, jump if zero. << un-optimal, uses 'al' reg when not necessary
  - [x] bitwise
  - [x] unary
    - [x] LogicalNot
    - [x] BitwiseNot
    - [ ] left hand && right hand unary, e.g &(var) or val_ptr^
- [x] dynamically place variables on stack if they are(nt) used immediately.
  - don't push pop every intlit/var, use registers!
- [ ] Testing infrastructure.
- [ ] data types
  - [x] primitives
  - [x] pointers // get mem location of a val (impl '&')
  - [ ] structs
  - [ ] arrays // just heap pointers?
- [x] functions
  - return type
  - arguments
  - body
  - program entry point ("main")
- [ ] Implement C equivalent operators:
  - [x] comma: ','
  - [x] assignment: '=' | '+=' | '/=' | ..
  - [x] modulus: %
  - [x] logical not: !
  - [x] unary minus: i.e -10
  - [x] function call: '()'
  - [] array subscript: '[]'
  - [ ] struct member: '.' | '->'
  - cast: (type)
