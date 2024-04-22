an attempt at making a compiler in rust!

### Currently working on: 
  - [ ] if
    - [x] rework lexer to handle multi-symbol keywords (i.e "==" or "<=")
    - [x] parse boolean comparison 
    - [x]  else & else if parsing
    - [ ] code generation
      - [x] invert 'jump' conditions
      - [ ] rethink boolean expr, all should evaluate to 0,1.
      - [ ] unsigned vs signed comparison (diff jump instructions)
    - [x] types of scope
      - [x] inherits variables from parent scope (if, else if, else) 
      - [x] doesn't (function)
  - [x] (kinda done) split 'TokenKind': 'Symbol' .. 'LogicalOp' .. 'BinaryOp' .. etc
  - [x] comments
  - [ ] dynamically place variables on stack if they are(nt) used immediately. 
  - [ ] redesign Lexer, Result<T> && self.read_char() returns char?, akin to self.consume()
  - [ ] update grammar to match code. 
  - [ ] variable reassignement (mutability)
  - [ ] types
    - [ ] bools: true, false
    
  - [ ] functions


