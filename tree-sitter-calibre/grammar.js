/**
 * @file A scripting language entirely made in rust for educational purposes
 * @author Creative Coders <officialccoders@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC = {
  primary: 150,
  unary: 140,
  as: 130,
  power: 120,
  multiplicative: 110,
  additive: 100,
  shift: 90,
  bitwise: 80,
  is: 70,
  in: 60,
  comparative: 50,
  boolean: 40,
  range: 30,
  composite_literal: -1,
};

const multiplicativeOperators = ["*", "/", "%"];
const additiveOperators = ["+", "-"];
const booleanOperators = ["&&", "||"];
const bitwiseOperators = ["&", "|", "^"];
const shiftOperators = [">>", "<<"];
const comparativeOperators = ["==", "!=", "<", "<=", ">", ">="];
const rangeOperators = ["..", "..="];

const assignmentOperators = multiplicativeOperators
  .concat(additiveOperators)
  .concat(booleanOperators)
  .concat(bitwiseOperators)
  .concat(shiftOperators)
  .map((operator) => operator + "=")
  .concat("=");

const char = /[^"\n\\]+/;

const newline = /\n/;
const terminator = choice(newline, ";", "\0");

const hexDigit = /[0-9a-fA-F]/;
const octalDigit = /[0-7]/;
const decimalDigit = /[0-9]/;
const binaryDigit = /[01]/;

const hexDigits = seq(hexDigit, repeat(seq(optional("_"), hexDigit)));
const octalDigits = seq(octalDigit, repeat(seq(optional("_"), octalDigit)));
const decimalDigits = seq(
  decimalDigit,
  repeat(seq(optional("_"), decimalDigit)),
);
const binaryDigits = seq(binaryDigit, repeat(seq(optional("_"), binaryDigit)));

const hexLiteral = seq("0", choice("x", "X"), optional("_"), hexDigits);
const octalLiteral = seq(
  "0",
  optional(choice("o", "O")),
  optional("_"),
  octalDigits,
);
const decimalLiteral = choice(
  "0",
  seq(/[1-9]/, optional(seq(optional("_"), decimalDigits))),
);
const binaryLiteral = seq("0", choice("b", "B"), optional("_"), binaryDigits);

const intLiteral = choice(
  binaryLiteral,
  decimalLiteral,
  octalLiteral,
  hexLiteral,
);

const decimalExponent = seq(
  choice("e", "E"),
  optional(choice("+", "-")),
  decimalDigits,
);
const decimalFloatLiteral = choice(
  seq(decimalDigits, ".", optional(decimalDigits), optional(decimalExponent)),
  seq(decimalDigits, decimalExponent),
  seq(".", decimalDigits, optional(decimalExponent)),
);

const hexExponent = seq(
  choice("p", "P"),
  optional(choice("+", "-")),
  decimalDigits,
);
const hexMantissa = choice(
  seq(optional("_"), hexDigits, ".", optional(hexDigits)),
  seq(optional("_"), hexDigits),
  seq(".", hexDigits),
);
const hexFloatLiteral = seq("0", choice("x", "X"), hexMantissa, hexExponent);

const floatLiteral = choice(decimalFloatLiteral, hexFloatLiteral);

const imaginaryLiteral = seq(
  choice(decimalDigits, intLiteral, floatLiteral),
  "i",
);

module.exports = grammar({
  name: "calibre",

  extras: ($) => [/\s/, /\n/, ";", $.comment],
  rules: {
    source_file: ($) => seq(repeat($._statement)),

    // conficts: ($) => [[$._expression, $.member_expression]],

    _expression: ($) =>
      prec.left(
        choice(
          $.not_expression,
          $.special_binary_expression,
          $.type_binary_expression,
          $.binary_expression,
          $.try_expression,
          $.identifier,
          $.func_expression,
          $.string_literal,
          $.int_literal,
          $.float_literal,
          $.imaginary_literal,
          $.rune_literal,
          $.true,
          $.false,
          $.scope,
          $.parens,
          $.tuple_literal,
          $.list_literal,
          $.assignment_expression,
          $.object_expression,
          $.member_expression,
          $.pipe_expression,
          $.call_expression,
          $.if_declaration,
          $.match_declaration,
        ),
      ),

    _statement: ($) =>
      choice(
        $.var_declaration,
        $.loop_declaration,
        $.impl_declaration,
        $.stop_statement,
        $.type_declaration,
        $._expression,
      ),

    identifier: (_) =>
      prec(PREC.primary, /(r#)?[_\p{XID_Start}][_\p{XID_Continue}]*/),

    func_expression: ($) =>
      seq(
        "fn",
        field("parameters", $.parameter_list),
        field("async", optional("async")),
        field("result", optional(seq("->", $.data_type))),
        field("body", $.block),
      ),
    impl_declaration: ($) =>
      seq(
        "impl",
        field("name", $.identifier),
        "{",
        field("functions", seq(repeat($.var_declaration))),
        "}",
      ),

    type_declaration: ($) =>
      seq(
        "type",
        field("name", $.identifier),
        "=",
        choice($.data_type, $.enum_declaration, $.struct_declaration),
      ),

    enum_declaration: ($) =>
      prec.left(
        seq("enum", "{", seq(commaSep1($.enum_member_declaration)), "}"),
      ),

    struct_declaration: ($) =>
      prec.left(
        seq(
          "struct",
          field("name", $.identifier),
          optional($.key_type_list_object_val),
        ),
      ),

    enum_member_declaration: ($) =>
      seq(field("name", $.identifier), optional($.key_type_list_object_val)),

    key_type_list_object_val: ($) =>
      choice(
        seq(
          "{",
          field(
            "types",
            seq(
              commaSep1(
                seq(field("name", $.identifier, ":", "type", $.data_type)),
              ),
            ),
          ),
          "}",
        ),
        seq("(", field("types", seq(commaSep1($.data_type))), ")"),
      ),
    parens: ($) => prec(PREC.primary, seq("(", $._statement, ")")),
    list_literal: ($) => seq("[", seq(commaSep($._statement)), "]"),
    tuple_literal: ($) =>
      prec(
        145,
        seq(
          "(",
          choice(seq($._statement, ","), seq(commaSep($._statement))),
          ")",
        ),
      ),
    or_list: ($) => seq(sep1($._statement, "|")),
    conditionals_list: ($) => seq("if", sep1("if", $._statement)),
    if_declaration: ($) =>
      prec.left(
        seq(
          "if",
          field("comparison", choice($.if_let_comparison, $.if_comparison)),
          field("then", $.block),
          field(
            "otherwise",
            optional(seq("else", choice($.if_declaration, $.block))),
          ),
        ),
      ),

    if_let_comparison: ($) =>
      prec(
        5,
        seq(
          "let",
          field("patterns", $.or_list),
          field("conditionals", $.conditionals_list),
          "<-",
          field("mutability", optional($.mutability)),
          field("value", $._statement),
        ),
      ),

    if_comparison: ($) => prec(1, $._expression),

    match_declaration: ($) =>
      seq(
        "match",
        field("async", optional("async")),
        field("mutability", optional($.mutability)),
        field("type", optional($.data_type)),
        field("default", optional(seq("=", $._statement))),
        field("return", optional(seq("->", $.data_type))),
        "{",
        seq(commaSep1($.match_pattern)),
        "}",
      ),
    match_pattern: ($) =>
      seq(
        field("values", $.or_list),
        field("conditionals", $.conditionals_list),
        field("body", $.block),
      ),
    var_declaration: ($) =>
      seq(
        choice(seq("let", optional("mut")), "const"),
        field("name", $.identifier),
        field("type", optional(seq(":", $.data_type))),
        "=",
        field("value", $._statement),
      ),
    loop_declaration: ($) =>
      seq("for", field("loop_type", $.loop_type), field("body", $.block)),
    mutability: ($) => choice("&", "&mut", "mut"),
    loop_type: ($) =>
      choice($.for_loop_type, $.foreach_loop_type, $._statement),
    for_loop_type: ($) =>
      prec(
        2,
        seq(field("name", $.identifier), "in", field("value", $._statement)),
      ),
    foreach_loop_type: ($) =>
      prec(
        1,
        seq(
          field("name", $.identifier),
          "in",
          field("value", choice(seq($.mutability, $._statement), $.identifier)),
        ),
      ),
    block: ($) => seq("=>", $._statement),
    scope: ($) => prec(PREC.primary, seq("{", $.statement_list, "}")),
    statement_list: ($) => seq($._statement, repeat($._statement)),

    member_expression: ($) =>
      prec(
        145,
        prec.left(
          seq(
            field("root", $.identifier),
            choice(
              seq($.member_expr_member, $.key_value),
              seq(repeat1($.member_expr_member)),
            ),
          ),
        ),
      ),
    member_expr_member: ($) =>
      choice(seq("[", $._expression, "]"), seq(".", $.identifier)),
    stop_statement: ($) =>
      prec(-10, choice("break", "continue", seq("return", $._expression))),

    object_expression: ($) => prec(145, $.key_value),

    potential_key_value: ($) =>
      choice(
        $.key_value,
        seq("(", seq(commaSep1(field("value", $._statement))), ")"),
      ),

    pipe_expression: ($) =>
      prec(10, prec.left(seq($._statement, "|>", sep1($._statement, "|>")))),

    key_value: ($) =>
      seq(
        "{",
        seq(
          commaSep1(
            choice(
              field("value", $.identifier),
              seq(
                field("key", $.identifier),
                ":",
                field("value", $._statement),
              ),
            ),
          ),
        ),
        "}",
      ),
    parameter_list: ($) =>
      seq(
        "(",
        optional(
          seq(
            commaSep1(
              seq(
                field("name", sep1($.identifier, " ")),
                field("type", optional(seq(":", $.data_type))),
                field("default", optional(seq("=", $._expression))),
              ),
            ),
            optional(","),
          ),
        ),
        ")",
      ),

    data_type: ($) =>
      prec(
        PREC.primary,
        prec.left(
          choice(
            "int",
            "float",
            "dyn",
            "bool",
            "str",
            "char",
            "range",
            "struct",
            seq("<", field("types", seq(commaSep1($.data_type))), ">"),
            choice(
              seq("!", $.data_type),
              seq($.data_type, "!"),
              seq($.data_type, "!", $.data_type),
            ),
            seq("list", optional(seq("<", $.data_type, ">"))),
            seq($.data_type, "?"),
            seq(
              "fn",
              "(",
              field("parameters", optional(seq(commaSep1($.data_type)))),
              ")",
              "->",
              field("return", $.data_type),
            ),
            $.identifier,
          ),
        ),
      ),

    assignment_expression: ($) =>
      prec.left(
        seq(
          field("left", choice($.identifier, $.member_expression)),
          field("operator", choice(...assignmentOperators)),
          field("right", $._statement),
        ),
      ),

    try_expression: ($) => prec(PREC.primary, seq("try", $._expression)),

    not_expression: ($) =>
      prec(
        PREC.unary,
        seq(
          field("operator", choice("-", "!")),
          field("operand", $._statement),
        ),
      ),

    call_expression: ($) =>
      prec(
        PREC.unary,
        seq(field("caller", $._expression), field("args", $.argument_list)),
      ),
    argument_list: ($) =>
      seq("(", seq(commaSep(field("value", $._statement))), ")"),

    binary_expression: ($) => {
      const table = [
        [PREC.multiplicative, choice(...multiplicativeOperators)],
        [PREC.additive, choice(...additiveOperators)],
        [PREC.comparative, choice(...comparativeOperators)],
        [PREC.shift, choice(...shiftOperators)],
        [PREC.bitwise, choice(...bitwiseOperators)],
        [PREC.boolean, choice(...booleanOperators)],
        [PREC.range, choice(...rangeOperators)],
        [PREC.power, "**"],
      ];

      return choice(
        ...table.map(([precedence, operator]) =>
          // @ts-ignore
          prec.left(
            precedence,
            seq(
              field("left", $._expression),
              // @ts-ignore
              field("operator", operator),
              field("right", $._expression),
            ),
          ),
        ),
      );
    },

    type_binary_expression: ($) => {
      const table = [
        [PREC.as, "as"],
        [PREC.is, "is"],
      ];

      return choice(
        ...table.map(([precedence, operator]) =>
          // @ts-ignore
          prec.left(
            precedence,
            seq(
              field("left", $._expression),
              // @ts-ignore
              field("operator", operator),
              field("type", $.data_type),
            ),
          ),
        ),
      );
    },

    special_binary_expression: ($) => {
      const table = [[PREC.in, "in"]];

      return choice(
        ...table.map(([precedence, operator]) =>
          // @ts-ignore
          prec.left(
            precedence,
            seq(
              field("left", $._expression),
              // @ts-ignore
              field("operator", operator),
              field("type", $._statement),
            ),
          ),
        ),
      );
    },

    string_literal: ($) =>
      prec(
        PREC.primary,
        seq('"', repeat(choice($.escape_sequence, char)), token.immediate('"')),
      ),

    escape_sequence: (_) =>
      token.immediate(
        seq(
          "\\",
          choice(
            /[^xuU]/,
            /\d{2,3}/,
            /x[0-9a-fA-F]{2,}/,
            /u[0-9a-fA-F]{4}/,
            /U[0-9a-fA-F]{8}/,
          ),
        ),
      ),

    int_literal: (_) => prec(PREC.primary, token(intLiteral)),

    float_literal: (_) => prec(PREC.primary, token(floatLiteral)),

    imaginary_literal: (_) => prec(PREC.primary, token(imaginaryLiteral)),

    rune_literal: (_) =>
      prec(
        PREC.primary,
        token(
          seq(
            "'",
            choice(
              /[^'\\]/,
              seq(
                "\\",
                choice(
                  seq("x", hexDigit, hexDigit),
                  seq(octalDigit, octalDigit, octalDigit),
                  seq("u", hexDigit, hexDigit, hexDigit, hexDigit),
                  seq(
                    "U",
                    hexDigit,
                    hexDigit,
                    hexDigit,
                    hexDigit,
                    hexDigit,
                    hexDigit,
                    hexDigit,
                    hexDigit,
                  ),
                  seq(
                    choice("a", "b", "f", "n", "r", "t", "v", "\\", "'", '"'),
                  ),
                ),
              ),
            ),
            "'",
          ),
        ),
      ),

    true: (_) => prec(PREC.primary, "true"),
    false: (_) => prec(PREC.primary, "false"),

    // http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890
    comment: (_) =>
      token(
        choice(seq("//", /.*/), seq("/*", /[^*]*\*+([^/*][^*]*\*+)*/, "/")),
      ),
  },
});

function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}

/**
 * Creates a rule to match zero or more of the rules separated by {sep},
 * with an optional one at the end.
 *
 * @param {RuleOrLiteral} rule
 *
 * @param {RuleOrLiteral} separator
 */
function optionalTrailingSep(rule, separator) {
  return optional(seq(rule, repeat(seq(separator, rule)), optional(separator)));
}

/**
 * Creates a rule to match one or more of the rules separated by a comma
 *
 * @param {Rule} rule
 *
 * @returns {SeqRule}
 */
function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)));
}

/**
 * Creates a rule to optionally match one or more of the rules separated by a comma
 *
 * @param {Rule} rule
 *
 * @returns {ChoiceRule}
 */
function commaSep(rule) {
  return optional(commaSep1(rule));
}
