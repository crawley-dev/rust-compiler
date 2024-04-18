an attempt at making a compiler in rust!

### Currently working on: 
  - if
    - [x] rework lexer to handle multi-symbol keywords (i.e "==" or "<=")
    - [x] parse boolean comparison 
    - [x] code generation for jumps
    - [ ] else if & else
  - [x] types of scope
    - [x] inherits variables from parent scope (if, else if, else) 
    - [x] doesn't (function)
  - [x] (kinda done) split 'TokenKind': 'Symbol' .. 'LogicalOp' .. 'BinaryOp' .. etc
  - [ ] variable reassignement (mutability)
  - [ ] types
  - [ ] functions
