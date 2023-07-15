
// 2.3.4 Symbols as Tokens
// Any token that is not a potential number, does not contain a package marker,
// and does not consist entirely of dots will always be interpreted as a
// symbol.
// Therefore, numbers should have higher precedence than symbols.

const PREC = {
  number: 50,
  symbol: 40
};

const WHITESPACE = /[ \t\n\v\f\r\u{0085}\u{00A0}\u{1680}\u{2000}-\u{200A}\u{2028}\u{2029}\u{202F}\u{205F}\u{3000}]+/u;

const DOT = ".";

// 2.1.4 Character Syntax Types

const SYNTAX_TYPES = {

  constituent: /[0-9:<=>?!@a-zA-Z$%&^_*{+}~\x08.\x7f/\-\[\]]/,

  macro_char_term: /[;,"'`()]/,
  macro_char_noterm: "#",

  escape_single: "\\",
  escape_multi: "|",

};

// =============================================================================
// 2.3.1 Numbers as Tokens
// =============================================================================

// - or +
const SIGN = /[\-+]/;

const SLASH = "/";

const DECIMAL_POINT = DOT;

/*
Marker  Meaning
D or d  double-float
E or e  float (see *read-default-float-format*)
F or f  single-float
L or l  long-float
S or s  short-float
*/
const EXPONENT_MARKER = /[DdEeFfLlSs]/;

// a digit in radix 10
const DECIMAL_DIGIT = /[0-9]/;

const DIGIT = /[0-9a-zA-Z]/; // ?

// a digit in the current input radix
// #b  #B  binary   radix 2
// #o  #O  octal    radix 8
// #x  #X  hex      radix 16
// #nr #nR radix-n  radix 2..36 
//         decimal  radix 10

function assert_radix(radix) {
  if (!['b', 'o', 'x', 'r', ''].includes(radix))
    throw new Error("Illegal radix: " + radix);
}

function prefix_radix(radix) {
  assert_radix(radix);

  return {
    'b': /#[bB]/,
    'o': /#[oO]/,
    'x': /#[xX]/,
    'r': /#\d+[rR]/, /* ideally n is 2..36 */
    '' : '',
  }[radix];
}

function digit_radix(radix) {
  assert_radix(radix);

  return {
    'b': /[01]/,
    'o': /[0-7]/,
    'x': /[0-9a-fA-F]/,
    'r': /[0-9a-zA-Z]/,
    '' : /[0-9]/,
  }[radix];
}

// radix is one of { 'b', 'o', 'x', 'r', '' }, where '' stands for decimal
function integer_radix(radix) {
  assert_radix(radix);

  const prefix = prefix_radix(radix);
  const digit = digit_radix(radix);

  if (prefix != '')
    return seq(prefix, optional(SIGN), repeat1(digit));
  else
    return seq(optional(SIGN), repeat1(digit));
}
const INTEGER = choice(
  seq(
    optional(SIGN),
    repeat1(DECIMAL_DIGIT),
    DECIMAL_POINT),
  choice(
    integer_radix('b'),
    integer_radix('o'),
    integer_radix('x'),
    integer_radix('r'),
    integer_radix('')));

const RATIO = seq(
  optional(SIGN),
  repeat1(DIGIT),
  SLASH,
  repeat1(DIGIT));
const EXPONENT = seq(
  EXPONENT_MARKER,
  optional(SIGN),
  repeat1(DIGIT));

const FLOAT = choice(
  seq(
    optional(SIGN),
    repeat(DECIMAL_DIGIT),
    DECIMAL_POINT,
    repeat1(DECIMAL_DIGIT),
    optional(EXPONENT)),
  seq(
    optional(SIGN),
    repeat1(DECIMAL_DIGIT),
    optional(
      seq(
        DECIMAL_POINT,
        repeat(DECIMAL_DIGIT))),
    EXPONENT));


module.exports = grammar({

  name: "commonlisp",

  extras: $ => [],

  rules: {

    source: $ => repeat(choice($._skip, $._token)),

    _skip: $ => WHITESPACE,

    _token: $ => choice($.number, $.symbol),

    number: $ => prec(
      PREC.number,
      token(choice(INTEGER, RATIO, FLOAT))),

    // 2.3.4 Symbols as Tokens

    symbol: $ => prec(PREC.symbol, token(repeat1(SYNTAX_TYPES.constituent))),

  },

});

