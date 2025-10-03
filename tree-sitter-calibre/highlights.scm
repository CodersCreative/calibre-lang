; Function calls

(call_expression
  caller: (identifier) @function)

(call_expression
  caller: (identifer) @function.builtin
  (#match? @function.builtin "^(trim|print|len|range|ok|err|some)$"))

; Function definitions

(func_expression
  name: (identifier) @function)

(match_declaration
  name: (identifier) @function.method)

(enum_member_declaration
  name: (identifier) @variable.member)

(type_declaration
  name: (identifier) @type.definition)
; Identifiers

(data_type) @type
(member_expr_member) @property
(identifier) @variable

"->" @punctuation.special
"=>" @punctuation.special

"." @punctuation.delimiter
"," @punctuation.delimiter
";" @punctuation.delimiter

"(" @punctuation.bracket
")" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket
"<" @punctuation.bracket
">" @punctuation.bracket

; Operators

[
  "-"
  "-="
  "!"
  "!="
  ".."
  "..="
  "**="
  "**"
  "*"
  "*="
  "/"
  "/="
  "&"
  "&&"
  "&="
  "%"
  "%="
  "^"
  "^="
  "+"
  "+="
  "<"
  "<<"
  "<<="
  "="
  "=="
  ">"
  ">="
  ">>"
  ">>="
  "|"
  "|="
  "||"
  "in"
  "as"
  "is"
] @operator

; Keywords

[
  "break"
  "match"
  "const"
  "continue"
  "self"
  "async"
  "await"
  "try"
  "mut"
  "impl"
  "for"
  "fn"
  "let"
  "trait"
  "struct"
  "super"
  "global"
] @keyword

"return" @keyword.return

[
  "if"
  "else"
] @keyword.conditional

[
  "import"
  "from"
] @keyword.import

[
  "struct"
  "enum"
  "type"
] @keyword.type


; Literals

[
  (string_literal)
  (rune_literal)
] @string

(escape_sequence) @escape

[
  (int_literal)
  (imaginary_literal)
] @number

(float_literal) @number.float

[
  (true)
  (false)
] @constant.builtin

(comment) @comment
