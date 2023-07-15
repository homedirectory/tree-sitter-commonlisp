
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


// =============================================================================
// 2.1.4 Character Syntax Types
// =============================================================================

const SYNTAX_TYPES = {

  constituent: /[0-9:<=>?!@a-zA-Z$%&^_*{+}~\x08.\x7f/\-\[\]]/,

  macro_char_term: /[;,"'`()]/,
  macro_char_noterm: "#",

  escape_single: "\\",
  escape_multi: "|",

  whitespace: WHITESPACE

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

// radix is one of { 'b', 'o', 'x', 'r', '' }, where '' stands for decimal
function ratio_radix(radix) {
  assert_radix(radix);

  const prefix = prefix_radix(radix);
  const digit = digit_radix(radix);

  if (prefix != '')
    return seq(prefix, optional(SIGN), repeat1(digit), SLASH, repeat1(digit));
  else
    return seq(optional(SIGN), repeat1(digit), SLASH, repeat1(digit));
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

const RATIO = choice(
  ratio_radix('b'),
  ratio_radix('o'),
  ratio_radix('x'),
  ratio_radix('r'),
  ratio_radix(''));

// Although the grammar for exponents defined in the HyperSpec includes {digit}+,
// which denotes a digit in any radix, radixes other than decimal cannot be used
// to write numbers with exponents (floats). For example, the following are illegal:
// #x1.5ea ; error! mimics 1.5e10
// #x1ea ; valid, but is equal to 1ea in hex, not 1e10

const EXPONENT = seq(
  EXPONENT_MARKER,
  optional(SIGN),
  repeat1(DECIMAL_DIGIT) // *** not DIGIT
);

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

// =============================================================================
// 2.3.4 Symbols as Tokens
// =============================================================================

// We can categorize symbols into 2 groups: 
// 1. multiple-escaped symbols - symbols wrapped inside 'multiple escape' characters,
// that is, by the vertical-bar symbol '|'
// 2. raw symbols - symbols that are not multiple-escaped

// In case of (2) a 'single escape' character ('\') may be used before any other
// character.
// In case of (1) any 'single escape' and 'multiple escape' characters that are to
// appear in the sequence must be preceded by a 'single escape' character

// NOTE: if symbols (1) and (2) are concatenated, then they are read as a
// single symbol. Example: a|b|c

// TODO:
// * symbols can't consist entirely of dots (see 2.3.3)
// * symbols can't contain package markers (unless escaped)

const ANY_CHAR = choice(
  SYNTAX_TYPES.constituent,
  SYNTAX_TYPES.macro_char_term,
  SYNTAX_TYPES.macro_char_noterm,
  SYNTAX_TYPES.escape_single,
  SYNTAX_TYPES.escape_multi,
  SYNTAX_TYPES.whitespace);

const SINGLE_ESCAPED_CHAR = seq(SYNTAX_TYPES.escape_single, ANY_CHAR);

const RAW_SYMBOL_CHAR = choice(SYNTAX_TYPES.constituent, SINGLE_ESCAPED_CHAR);

// symbol (2)
const RAW_SYMBOL = repeat1(RAW_SYMBOL_CHAR);

// symbol (1)
const MULTI_ESCAPED_SYMBOL = seq(
  SYNTAX_TYPES.escape_multi,
  // here we can't use ANY_CHAR or RAW_SYMBOL_CHAR because multi and single
  // escape chars must be escaped; other chars need not be escaped
  repeat(choice(
    choice(
      SYNTAX_TYPES.constituent, SYNTAX_TYPES.macro_char_term,
      SYNTAX_TYPES.macro_char_noterm, SYNTAX_TYPES.whitespace),
    seq(SYNTAX_TYPES.escape_single, SYNTAX_TYPES.escape_single),
    seq(SYNTAX_TYPES.escape_single, SYNTAX_TYPES.escape_multi),
  )),
  SYNTAX_TYPES.escape_multi);


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

    symbol: $ => prec(
      PREC.symbol, 
      token(repeat1(choice(RAW_SYMBOL, MULTI_ESCAPED_SYMBOL)))),

    // TODO 2.3.3 The Consing Dot

    // TODO package (see 2.3.5)

    // TODO list (see 2.4.1)

    // TODO quote (see 2.4.3)

    // TODO backquote (see 2.4.6)

    // TODO comma (see 2.4.7)

    // TODO comment (see 2.4.4)

    // TODO string (see 2.4.5)

    // TODO sharpsigns (see 2.4.8)

  }, 

});

