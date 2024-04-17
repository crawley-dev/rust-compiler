an attempt at making a compiler in rust!

### Currently working on: 
  - if
    - rework lexer to handle multi-symbol keywords (i.e "==" or "<=")
    - parse boolean comparison 
    - handle jump to else scope
  - types of scope
    - inherits from parent scope (if, else if, else) 
    - doesn't (function)
  - 'let' --> data type (e.g u8, i32)
  - types
  - functions
