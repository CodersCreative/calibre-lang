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
  rules: {
    // TODO: add the actual grammar rules
    source_file: ($) => "hello",

    _expression: ($) =>
      choice(
        $.not_expression,
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
        $.stop_statement,
      ),

    identifier: (_) => /(r#)?[_\p{XID_Start}][_\p{XID_Continue}]*/,

    func_expression: ($) =>
      seq(
        "fn",
        field("parameters", $.parameter_list),
        field("result", optional(seq("->", $.data_type))),
        field("body", $.block),
      ),

    block: ($) => seq("=>", choice($._statement, $.scope)),
    scope: ($) => seq("{", $.statement_list, "}"),
    statement_list: ($) => sep1($._statement, terminator),

    stop_statement: ($) =>
      choice("break", "continue", seq("return", $._statement)),

    _statement: ($) => choice($.assignment_expression, $.stop_statement),

    parameter_list: ($) =>
      seq(
        "(",
        optional(
          seq(
            commaSep(
              seq(
                field("name", sep1($.identifier, " ")),
                field("type", optional(seq(":", $.data_type))),
                field("default", optional(seq("=", $._statement))),
              ),
            ),
            optional(","),
          ),
        ),
        ")",
      ),

    data_type: ($) => choice(),

    assignment_expression: ($) =>
      seq(
        field("left", $._expression),
        field("operator", choice(...assignmentOperators)),
        field("right", $._statement),
      ),

    try_expression: ($) => seq("try", $._statement),

    not_expression: ($) =>
      prec(
        PREC.unary,
        seq(
          field("operator", choice("-", "!")),
          field("operand", $._statement),
        ),
      ),

    binary_expression: ($) => {
      const table = [
        [PREC.multiplicative, choice(...multiplicativeOperators)],
        [PREC.additive, choice(...additiveOperators)],
        [PREC.comparative, choice(...comparativeOperators)],
        [PREC.shift, choice(...shiftOperators)],
        [PREC.bitwise, choice(...bitwiseOperators)],
        [PREC.boolean, choice(...booleanOperators)],
        [PREC.range, choice(...rangeOperators)],
        [PREC.as, "as"],
        [PREC.is, "is"],
        [PREC.in, "in"],
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

    string_literal: ($) =>
      seq('"', repeat(choice($.escape_sequence, char)), token.immediate('"')),

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

    int_literal: (_) => token(intLiteral),

    float_literal: (_) => token(floatLiteral),

    imaginary_literal: (_) => token(imaginaryLiteral),

    rune_literal: (_) =>
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
                seq(choice("a", "b", "f", "n", "r", "t", "v", "\\", "'", '"')),
              ),
            ),
          ),
          "'",
        ),
      ),

    true: (_) => "true",
    false: (_) => "false",

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
