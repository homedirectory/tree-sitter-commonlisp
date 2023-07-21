
// 2.3.4 Symbols as Tokens
// Any token that is not a potential number, does not contain a package marker,
// and does not consist entirely of dots will always be interpreted as a
// symbol.
// Therefore, numbers should have higher precedence than symbols.

const PREC = {
  number: 50,
  nil: 43,
  t: 43,
  package: 42,
  keyword: 41,
  symbol: 40,
  documentation: 1,
  string: 0,
};

const WHITESPACE = /[ \t\n\v\f\r\u{0085}\u{00A0}\u{1680}\u{2000}-\u{200A}\u{2028}\u{2029}\u{202F}\u{205F}\u{3000}]+/u;

const DOT = ".";
const BACKSLASH = "\\";

// =============================================================================
// 2.1.4 Character Syntax Types
// =============================================================================

const SYNTAX_TYPES = {

  // we exclude the package marker from constituent chars because it is special:
  // it delimits a package from a symbol, and must be escaped to be a part of a
  // symbol
  constituent: /[0-9<=>?!@a-zA-Z$%&^_*{+}~\x08.\x7f/\-\[\]]/,

  // pseudo syntax type for chars that must be escaped in a symbol
  dangerous: /:/,

  macro_char_term: /[;,"'`()]/,
  macro_char_noterm: "#",

  escape_single: "\\",
  escape_multi: "|",

  whitespace: WHITESPACE

};

const PACKAGE_MARKER = ':';

// returns a rule that matches an escaped character given by the argument
function escape_single(c) {
  return seq(SYNTAX_TYPES.escape_single, c);
}


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

// '' stands for decimal
const RADIXES = ['b', 'o', 'x', 'r', ''];

function assert_radix(radix) {
  if (!RADIXES.includes(radix))
    throw new Error("Illegal radix: " + radix);
}

const RADIX_PREFIXES = {
  'b': /#[bB]/,
  'o': /#[oO]/,
  'x': /#[xX]/,
  'r': /#\d+[rR]/, /* ideally n is 2..36 */
  '' : '',
};

const RADIX_DIGITS = {
  'b': /[01]/,
  'o': /[0-7]/,
  'x': /[0-9a-fA-F]/,
  'r': /[0-9a-zA-Z]/,
  '' : /[0-9]/,
};

// transforms a given rule to use a given radix
// * radix  - one of RADIXES
// * fn     - a rule-producing function of 1 argument which stands for a digit in given radix
// Example: in_radix('x', digit => seq(sign, digit));
function in_radix(radix, fn) {
  assert_radix(radix);

  const prefix = RADIX_PREFIXES[radix];
  const digit = RADIX_DIGITS[radix];

    if (prefix != '')
        return seq(prefix, fn(digit));
    else
        return fn(digit);
}

// transforms a given rule into a choice between all existinging radixes
// * fn     - a rule-producing function of 1 argument which stands for a digit in given radix
function radix_choice(fn) {
  const choices = RADIXES.map(rdx => in_radix(rdx, fn));
  return choice(...choices);
}

const INTEGER = choice(
  seq(
    optional(SIGN),
    repeat1(DECIMAL_DIGIT),
    DECIMAL_POINT),
  radix_choice(digit => seq(optional(SIGN), repeat1(digit))));

const RATIO = radix_choice(
  digit => seq(optional(SIGN), repeat1(digit), SLASH, repeat1(digit)));

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

const NUMBER = choice(INTEGER, RATIO, FLOAT);

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
  SYNTAX_TYPES.dangerous,
  SYNTAX_TYPES.macro_char_term,
  SYNTAX_TYPES.macro_char_noterm,
  SYNTAX_TYPES.escape_single,
  SYNTAX_TYPES.escape_multi,
  SYNTAX_TYPES.whitespace);

const RAW_SYMBOL_CHAR = choice(SYNTAX_TYPES.constituent, escape_single(ANY_CHAR));

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
      SYNTAX_TYPES.macro_char_noterm, SYNTAX_TYPES.whitespace,
      SYNTAX_TYPES.dangerous),
    seq(SYNTAX_TYPES.escape_single, SYNTAX_TYPES.escape_single),
    seq(SYNTAX_TYPES.escape_single, SYNTAX_TYPES.escape_multi),
  )),
  SYNTAX_TYPES.escape_multi);

const SYMBOL = repeat1(choice(RAW_SYMBOL, MULTI_ESCAPED_SYMBOL));

// =============================================================================
// List
// =============================================================================
// See 2.4.1 Left-Parenthesis

// Brackets ([]) and braces ({}) are considered constituent characters,
// so the following are valid symbols: [], [a], a{b}c.
// From 2.1.4: 
// > ... are initially constituents, but they are not used in any standard Common Lisp notations. 
// > These characters are explicitly reserved to the programmer.
// Therefore, we use only parens as list enclosing characters.

function in_parens() {
  return seq("(", ...arguments, ")");
  // return choice(
  //   seq("(", rule, ")"),
  //   seq("[", rule, "]"),
  //   seq("{", rule, "}"));
}

// =============================================================================
// Quote
// =============================================================================
// See 2.4.3 Single-Quote

const SINGLE_QUOTE = "'";


// =============================================================================
// String
// =============================================================================
// See 2.4.5 Double-Quote

const DOUBLE_QUOTE = '"';
const NOT_DOUBLE_QUOTE = /[^"]/;

const STRING = token(seq(
  DOUBLE_QUOTE, 
  repeat(choice(escape_single(DOUBLE_QUOTE), NOT_DOUBLE_QUOTE)),
  DOUBLE_QUOTE));

// =============================================================================
// Backquote, unquote, unquote-splicing
// =============================================================================
// See 2.4.6 Backquote, 2.4.7 Comma

const BACKQUOTE = "`";
const COMMA = ",";

// Anywhere ',@' may be used, ',.' may be used instead to indicate
// that the list structure produced by the form can be operated on destructively
// (i.e., using nconc rather than append)
const UNQUOTE_SPLICING = /,[@.]/;

// ==============================================
// Sharpsign
// =============================================================================
// See 2.4.8 Sharpsign

const SHARPSIGN = "#";

// 2.4.8.3 Sharpsign Left-Parenthesis (Vector)
const UNSIGNED_DECIMAL_INTEGER = /[0-9]+/;

// 2.4.8.4 Sharpsign Asterisk (bit vector)
const ASTERISK = "*";

// 2.4.8.5 Sharpsign Colon (uninterned symbol)
const SHARPSIGN_COLON = "#:";

// 2.4.8.6 Sharpsign Dot (read-eval / sharp-dot)
const SHARPSIGN_DOT = "#.";


// Macro DEFPARAMETER, DEFVAR
function make_defvar($, macro_name) {
  return in_parens(
    macro_name,
    field("name", $.symbol), 
    optional(seq(
      field("value", $._element),
      optional($.documentation))))
}


module.exports = grammar({

  name: "commonlisp",

  extras: $ => [WHITESPACE, $.block_comment],

  conflicts: $ => [
    [$.documentation, $.string],
  ],

  rules: {

    source: $ => repeat($._element),

    _element: $ => choice($._token, $._form),

    _token: $ => choice(
      $.number, 
      $.package,
      $.keyword,
      $.symbol, 
      $.list,
      $.quote,
      $.comment,
      $.t,
      $.nil,
      $.string,
      $.backquote,
      $.unquote,
      $.unquote_splicing,
      $.dot,
      $.character,
      $.function,
      $.vector,
      $.bitvector,
      $.uninterned_symbol,
      $.sharp_dot,
      $.complex,
      $.array,
      $.struct,
      $.pathname),

    // ------------------------------------------------------------
    // Tokens
    // ------------------------------------------------------------
    // Tokens include: 
    // * objects (e.g., symbol, list, number)
    // * special characters (e.g., dot, comma)
    // * quotes and quasiquotes
    // In essence - the most primitive language elements

    number: $ => prec(PREC.number, token(NUMBER)),

    package: $ => prec(PREC.package,
      seq(
        field("pkg", $.symbol),
        $.pkg_mark,
        field("sym", $.symbol))),

    pkg_mark: $ => PACKAGE_MARKER,

    // keyword:foo will parse as $.package even if optional("keyword") was prepended but that's fine
    keyword: $ => prec(PREC.keyword, 
      seq($.pkg_mark, $.symbol)),

    symbol: $ => prec(PREC.symbol, token(SYMBOL)),

    list: $ => $._list,

    _list: $ => in_parens(repeat($._element)),

    quote: $ => seq(SINGLE_QUOTE, $._token),

    // 2.4.4 Semicolon
    comment: _ => token(/;.*/),

    nil: _ => prec(PREC.nil, "nil"),

    t: _ => prec(PREC.t, "t"),

    // TODO format specifiers
    string: _ => prec(PREC.string, STRING),

    backquote: $ => seq(BACKQUOTE, $._token),

    unquote: $ => seq(COMMA, $._token),

    unquote_splicing: $ => seq(UNQUOTE_SPLICING, $._token),

    dot: $ => DOT,
    
    // 2.4.8.1 Sharpsign Backslash
    character: $ => token(seq(SHARPSIGN, BACKSLASH, /.+/)),

    // 2.4.8.2 Sharpsign Single-Quote
    function: $ => seq(SHARPSIGN, SINGLE_QUOTE, $._token),

    // 2.4.8.3 Sharpsign Left-Parenthesis
    vector: $ => seq(
      SHARPSIGN, 
      optional(UNSIGNED_DECIMAL_INTEGER), 
      $._list),

    // 2.4.8.4 Sharpsign Asterisk
    // can't specify only [01] after asterisk because #*0123 would parse as
    // (bitvector) (number), which is bound to confuse people; for another
    // example #*01a would parse as (bitvector) (symbol)
    bitvector: $ => token(
      seq(SHARPSIGN, 
        optional(UNSIGNED_DECIMAL_INTEGER), 
        ASTERISK, repeat(SYNTAX_TYPES.constituent))),

    // 2.4.8.5 Sharpsign Colon
    uninterned_symbol: $ => token(seq(SHARPSIGN_COLON, SYMBOL)),

    // 2.4.8.6 Sharpsign Dot
    sharp_dot: $ => seq(SHARPSIGN_DOT, $._token),

    // 2.4.8.11 Sharpsign C (complex)
    complex: $ => seq("#C", in_parens($.number, $.number)),

    // 2.4.8.12 Sharpsign A (array)
    array: $ => choice(
      seq(/#[1-9][0-9]*[aA]/, $._list),
      seq(/#0[aA]/, $._token)),

    // 2.4.8.13 Sharpsign S (structure)
    struct: $ => seq(
      /#[sS]/,
      in_parens(
        field("name", $.symbol),
        repeat(seq(field("slot", $.symbol), field("value", $._token))))),

    // 2.4.8.14 Sharpsign P
    pathname: $ => seq(/#[pP]/, STRING),

    // TODO sharpsigns (see 2.4.8)
    // 2.4.8.15 Sharpsign =
    // 2.4.8.17 Sharpsign + (feature expression)
    // 2.4.8.18 Sharpsign - (feature expression)

    // 2.4.8.19 Sharpsign Vertical-Bar (block comment)
    block_comment: $ => seq("#|", repeat(choice($.block_comment, /[^|]/, /\|[^#]/)), "|#"),

    // ------------------------------------------------------------
    // Forms
    // ------------------------------------------------------------
    // Forms are either special forms, such as let, or macros, such as defun.
    // Forms consist of tokens and other forms.

    _form: $ => choice(
      $.defun,
      $.defvar,
      $.defparameter,
      $.let),

    // --- defun ---

    defun: $ => in_parens(
      "defun", field("name", $.symbol), $.lambda_list,
      repeat($.declare), 
      optional(choice($._doc_body, $._body))),

    _doc_body: $ => seq($.documentation, repeat1($._element)),

    _body: $ => repeat1($._element),

    // TODO &rest &keys &optional
    lambda_list: $ => in_parens(repeat($.symbol)),

    declare: $ => in_parens("declare", repeat($._token)),

    documentation: _ => prec(PREC.documentation, STRING),

    // --- defvar, defparameter ---

    defvar: $ => make_defvar($, "defvar"),

    defparameter: $ => make_defvar($, "defparameter"),

    // --- let, let* ---

    let: $ => in_parens(
      choice("let", "let*"),
      $.let_binds,
      repeat($.declare),
      repeat($._element)),

    let_binds: $ => in_parens(repeat($.let_bind)),

    let_bind: $ => choice(
      field("var", $.symbol),
      in_parens(field("var", $.symbol), optional(field("init", $._element)))),

  }, 

});
