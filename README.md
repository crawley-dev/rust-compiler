an attempt at making a compiler in rust!

### Currently working on: 
  - [ ] if
    - [x] rework lexer to handle multi-symbol keywords (i.e "==" or "<=")
    - [x] parse boolean comparison 
    - [x] else if & else parsing
    - [ ] code generation
  - [x] types of scope
    - [x] inherits variables from parent scope (if, else if, else) 
    - [x] doesn't (function)
  - [x] (kinda done) split 'TokenKind': 'Symbol' .. 'LogicalOp' .. 'BinaryOp' .. etc
  - [x] comments
  - [ ] redesign Lexer, Result<T> && self.read_char() returns char?, akin to self.consume()
  - [ ] update grammar to match code. 
  - [ ] variable reassignement (mutability)
  - [ ] types
  - [ ] functions
