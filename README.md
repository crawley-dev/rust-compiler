an attempt at making a compiler in rust!

          hhh {
        eee {
      aaa {
   ddd {
aaa }
  ccc } 
    hhh }
      eee }


## Core Functionality Required:
- [x] variables WITH DATA TYPES 
- [x] operations: (boolean, logical//bitwise, binary)
- [x] control flow branches: (if/else)
- [ ] loops: (while/for)
- [ ] functions: (recursion << should be free?)

### Currently working on: 
  - [x] if
    - [x] rework lexer to handle multi-symbol keywords (i.e "==" or "<=")
    - [x] parse boolean comparison 
    - [x]  else & else if parsing
    - [ ] code generation
      - [x] invert 'jump' conditions
      - [ ] unsigned vs signed comparison (diff jump instructions)
      - [x] binary expr conditions
        - conditional expr, either has explicity bool comparison or implicit, expr = lhs: (lhs > 0)
        - cmp reg1, reg2 ; compare arguments
        - set(EQUIVALENCE e.g e, le) al; sets register 'al' (8 bit) to 1,0 depending on cmp flag
        - movzx output_reg, al ; Move Zero Xtend. copies 'al' into 'output_reg' && zero init reg1 bits.  
    - [x] types of scope
      - inherits variables from parent scope (if, else if, else) 
      - doesn't (new function, UNLESS class, inherits 'self')
  - [x] (kinda done) split 'TokenKind': 'Symbol' .. 'LogicalOp' .. 'BinaryOp' .. etc
  - [x] comments
  - [ ] dynamically place variables on stack if they are(nt) used immediately. 
    - use multiple registers to store arguments for a binary expr
    - add stack values at top level (expr, stmt), not term. do ^^
  - [x] re-design multi-symbol in Lexer: match against longest multi-symbol to shortest, until finds match (or illegal token.) 
  - [x] update grammar to match code. 
  - [x] variable reassignment (mutability)
  - [ ] Operators
    - [x] binary
    - [] logical
      - i.e: if (5)  | 5 != 0 so -> if (true) 
      - i.e: if (!5) | 5 == 0 so -> if (false)  
    - [] bitwise
    - [] unary
    - [ ] Associativity
  - [ ] data types
    - [ ] bools: al register (8 bit)
    - [ ] u8: al
    - [ ] u16: ax 
    - [ ] u32: eax 
    - [ ] u64: rax
  - [ ] functions
    - return type
    - arguments 
    - body
    - program entry point ("main")
  - [ ] Implement C equivalent operators:
    - [ ] comma: ','
    - [ ] assignment: '=' | '+=' | '/=' | .. 
    - [ ] modulus: %
    - [ ] logical not: !
    - [ ] unary minus: i.e -10
    - [ ] function call: '()'
    - [ ] array subscript: '[]'
    - [ ] struct member: '.' | '->'
    - [ ] Increment/Decrement (OPTIONAL)
    - cast: (type)
  - [ ] (Joke mode:) negative whitespace significance, the most nested piece of code has 0 indentations,    everything out has an indentation.
    - compiler error on uppercase


