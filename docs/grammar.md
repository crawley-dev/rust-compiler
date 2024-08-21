# Grammar

## Parse Structure

- FILE -> Program
- Program -> List of Scopes (either defined or the global scope)
- Scope -> List of Statments
- Statement -> Arbitrary Set of Grammar and Expressions
- Expression -> Binary or Unary or Term
- Logical -> Expression Operator Expression
- Unary -> Expression and Prefix/Postfix Operator
- Term -> Integer Literal or Variable

## Symbol Registry

- StatementEnd ';' | ends statements
- TypeSeparator ':' | used for stating a variable's types
- LineComment '//' | starts a comment for that line.
- Parentheses "(, )" | function calls & expression precedence.
- CurlyBraces "{, }" | defines a scope
- Open|Close Comment "/*,*/" | defines a commented area of code....

## Operator Registry

- Pascal-like convention(borrowed from odin) "Type on the left, usage on the right"
  - e.g "p: ^u32 = var^ + 5;". u32 pointer equal to de-reference of var plus 5s
- Precedence determines the order of operations of an expression
  - e.g "!(5 + 5 *(10 - 3* 2 / 5) == 10)"
  - operators also have associativity, which determines how precedence is 'climbed' as an expression is parsed.

- Parens           '()' prec = infinite
- Logical Not      '!'  prec = 13
- Multiply         '*'  prec = 12
- Divide           '/'  prec = 12
- Remainder        '%'  prec = 12
- Subtract         '-'  prec = 11
- Add              '+'  prec = 11
- Left Shift       '<<' prec = 10
- Right Shift      '>>' prec = 10
- Equal            '==' prec = 8
- Not Equal        '!=' prec = 8
- Lesser           '<'  prec = 8
- Greater          '>'  prec = 8
- Lesser or Equal  '<=' prec = 8
- Greater or Equal '>=' prec = 8
- Bitwise And      '&'  prec = 7
- Bitwise Xor      '~'  prec = 6
- Bitwise Or       '|'  prec = 5
- Logical And      '&&' prec = 4
- Logical Or       '||' prec = 3
- DirectAssign     '='  prec = 1
- Assign           '_=' prec = 1 | underscore replaced by arithmetic or bitwise binary operator.  
</pre>

## Keyword Registry

- IntLit e.g: '5' | defines a base 10 num (TODO: expand to hex, octal, binary)
- Ident e.g: "Var123" | a variable's name
- Expr e.g "5+5" | comparison, logical or mathematical expression
- Scope: a block of code, with optional variable inheritance
  - OPEN_CURLY STATEMENT(s) CLOSE_CURLY
- let: defining a variable, with optional mutability
  - LET (opt MUTABLE) IDENT COLON TYPE ASSIGN EXPR STATEMENT_END
- If:
  - IF EXPR SCOPE (ELSE_IF) (ELSE)
  - Else If:
    - ELSE IF EXPR SCOPE (ELSE_IF)
  - Else:
    - ELSE EXPR SCOPE
  - 'EXPR' must evaluate to type: Boolean
- while:
  - WHILE EXPR SCOPE
- break: exits current loop
- fn: a function declaration
  - FN IDENT OPEN_PAREN (IDENT COLON TYPE) (COMMA IDENT COLON TYPE)* CLOSE_PAREN (opt RETURN_ARROW TYPE) SCOPE
  - every path must return at the end of the function.

## Type Registry

- All type information is kept on the left, all type operations are performed on the right, taken from odin
  - array: []TYPE
    - access arr: VAR[OFFSET]
  - ptr: ^TYPE
    - de ref: var^
- u8 .. u64: unsigned integers specified by bit length
- i8 .. i64: signed integers specified by bit length
- usize, isize: unsigned and signed integers of maximum length (64 bit)
- f32, f64: floats specified by bit length
- bool: boolean true or false (0, 1)
- (TODO) char: a utf-8 character (!= u8, not needed currently.)
