// =============================================================================
// Preface
// =============================================================================
// -- /regex/ vs choice()
// Wherever it is practical choice() is prefered over /regex/ for its greater
// usability in queries. Consider matching "ab" or "ac", we could use /a[bc]/
// or choice("ab", "ac"). Choosing the former would prevent us from using
// ["ab", "ac"] in a query, while the latter just works. Although, it should
// be noted that choice() increases the size of the generated parser.

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

const PACKAGE_MARKER = /::?/;

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

// Anywhere ',@' may be used, ',.' may be used instead to indicate
// that the list structure produced by the form can be operated on destructively
// (i.e., using nconc rather than append)
const UNQUOTE_SPLICING = choice(",@", ",.");

// ==============================================
// Sharpsign
// =============================================================================
// See 2.4.8 Sharpsign

const SHARPSIGN = "#";

// 2.4.8.3 Sharpsign Left-Parenthesis (Vector)
const UNSIGNED_DECIMAL_INTEGER = /[0-9]+/;

// 2.4.8.4 Sharpsign Asterisk (bit vector)
const ASTERISK = "*";

// 2.4.8.1 Sharpsign Backslash (character)
const CHARACTER_NAMES = [
  "Newline",
  "Space",
  "Rubout",
  "Page",
  "Tab",
  "Backspace",
  "Return",
  "Linefeed",
];

const CHARACTER_NAME = choice(...CHARACTER_NAMES);


// Macro DEFPARAMETER, DEFVAR, DEFCONSTANT
function make_defvar($, macro_name, opt_init = true) {
  const init_rule = seq(
    field("init", $._element),
    optional($.documentation));

  return in_parens(
    macro_name,
    field("name", $._symbol),
    opt_init ? optional(init_rule) : init_rule);
}


module.exports = grammar({

  name: "commonlisp",

  extras: $ => [WHITESPACE, $.comment, $.block_comment],

  conflicts: $ => [
    [$.documentation, $.string],
  ],

  supertypes: $=> [$._symbol],

  rules: {

    source: $ => repeat($._element),

    _element: $ => choice($._token, $._form),

    _token: $ => choice(
      $.number,
      $._symbol,
      $.list,
      $.quote,
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

    // FIXME conflict between (pkg_symbol) and [(symbol) (keyword)]

    pkg_symbol: $ => prec(PREC.package,
      token(seq(
        field("pkg", SYMBOL),
        token.immediate(PACKAGE_MARKER),
        // need alias() because can't use token.immediate() with a token() rule
        // field("sym", alias(token.immediate(SYMBOL), $.symbol))))),
        field("sym", token.immediate(SYMBOL))))),

    // keyword:foo will parse as $.pkg_symbol even if optional("keyword") was prepended but that's fine
    keyword: $ => prec(PREC.keyword,
      token(seq(":", token.immediate(SYMBOL)))),

    symbol: $ => prec(PREC.symbol, token(SYMBOL)),

    _symbol: $ => choice($.pkg_symbol, $.keyword, $.symbol),

    list: $ => $._list,

    _list: $ => in_parens(repeat($._element)),

    quote: $ => seq("'", $._token),

    // 2.4.4 Semicolon
    comment: _ => token(/;.*/),

    nil: _ => prec(PREC.nil, "nil"),

    t: _ => prec(PREC.t, "t"),

    // TODO format specifiers
    string: _ => prec(PREC.string, STRING),

    backquote: $ => seq("`", $._token),

    unquote: $ => seq(",", $._token),

    unquote_splicing: $ => seq(UNQUOTE_SPLICING, $._token),

    dot: $ => DOT,

    // 2.4.8.1 Sharpsign Backslash
    character: $ => token(seq(
      "#\\",
      token.immediate(
        choice(
          obj_choice(without(SYNTAX_TYPES, "whitespace")),
          CHARACTER_NAME)))),

    // 2.4.8.2 Sharpsign Single-Quote
    function: $ => seq("#'", $._element),

    // 2.4.8.3 Sharpsign Left-Parenthesis
    vector: $ => seq(
      SHARPSIGN,
      optional(field("len", alias(token.immediate(UNSIGNED_DECIMAL_INTEGER), $.number))),
      $._list),

    // 2.4.8.4 Sharpsign Asterisk
    bitvector: $ => seq(
      SHARPSIGN,
      optional(field("len", alias(token.immediate(UNSIGNED_DECIMAL_INTEGER), $.number))),
      ASTERISK,
      // need alias() to use token.immediate() and represent as (bits)
      optional(alias(token.immediate(/[01]+/), $.bits))),

    bits: $ => /[01]+/,

    // 2.4.8.5 Sharpsign Colon
    uninterned_symbol: $ => seq("#:", $.symbol),

    // 2.4.8.6 Sharpsign Dot
    sharp_dot: $ => seq("#.", $._token),

    // 2.4.8.11 Sharpsign C (complex)
    complex: $ => seq(choice("#c", "#C"), in_parens($.number, $.number)),

    // 2.4.8.12 Sharpsign A (array)
    array: $ => choice(
      seq(/#[1-9][0-9]*[aA]/, $._list),
      seq(/#0[aA]/, $._token)),

    // 2.4.8.13 Sharpsign S (structure)
    struct: $ => seq(
      choice("#s", "#S"),
      in_parens(
        field("name", $.symbol),
        repeat(seq(field("slot", $.symbol), field("value", $._token))))),

    // 2.4.8.14 Sharpsign P
    pathname: $ => seq(choice("#p", "#P"), $.string),

    // TODO sharpsigns (see 2.4.8)
    // 2.4.8.15 Sharpsign =
    // 2.4.8.17 Sharpsign + (feature expression)
    // 2.4.8.18 Sharpsign - (feature expression)

    // 2.4.8.19 Sharpsign Vertical-Bar (block comment)
    block_comment: $ => seq("#|", repeat(choice($.block_comment, /[^|]/, /\|[^#]/)), "|#"),

    _string_designator: $ => choice(
      $.character,
      $._symbol,
      $.string),

    // ------------------------------------------------------------
    // Forms
    // ------------------------------------------------------------
    // Forms are either special forms, such as let, or macros, such as defun.
    // Forms consist of tokens and other forms.
    // A better but longer name is "compound form".
    //
    // See 3.1.2.1.2 Conses as Forms

    _form: $ => choice(
      $.defun,
      $.defmacro,
      $.defmethod,
      $.defgeneric,
      $.lambda,
      $.defvar,
      $.defparameter,
      $.defconstant,
      $.let,
      $.destr_bind,
      $.defclass,
      $.with_slots,
      $.labels, $.flet, $.macrolet,
      $.in_package),

    defun: $ => in_parens(
      "defun", $.fn_name, $.lambda_list,
      repeat($.declare),
      optional(choice($._doc_body, $._body))),

    defmacro: $ => in_parens(
      // is aliasing macro_lambda_list a good choice?
      "defmacro", field("name", $._symbol), alias($.macro_lambda_list, $.lambda_list),
      repeat($.declare),
      optional(choice($._doc_body, $._body))),

    fn_name: $ => choice($._symbol, in_parens("setf", $._symbol)),

    _doc_body: $ => seq($.documentation, repeat1($._element)),

    _body: $ => repeat1($._element),

    // --- lambda list ---

    lambda_list: $ => in_parens(
      repeat($.symbol),
      repeat(choice($.restvar, $.optvar, $.keyvar, $.auxvar))),

    // &rest var
    restvar: $ => seq("&rest", field("var", $.symbol)),

    // &optional var
    optvar: $ => seq(
      "&optional",
      repeat(choice(
        field("var", $.symbol),
        in_parens(
          field("var", $.symbol),
          field("init", $._element),
          optional(field("p", $.symbol)))))),

    // &key - 3.4.1.4 Specifiers for keyword parameters
    // &key
    //   {var |
    //     ({var | (keyword-name var)}
    //      [init-form [supplied-p-parameter]])}*
    //    [&allow-other-keys]
    keyvar: $ => seq(
      "&key",
      repeat(choice(
        field("var", $.symbol),
        in_parens(
          choice(
            field("var", $.symbol),
            in_parens(field("kwd_name", $._symbol), field("var", $.symbol))),
          optional(seq(
            field("init", $._element),
            optional(field("p", $.symbol))))))),
      optional($.allow_other_keys)),

    // &allow-other-keys
    allow_other_keys: _ => "&allow-other-keys",

    // &aux {var | (var [init-form])}*
    auxvar: $ => seq(
      "&aux",
      repeat(choice(
        field("var", $.symbol),
        in_parens(field("var", $.symbol), optional(field("init", $._element)))))),

    // 3.4.4 Macro Lambda Lists
    macro_lambda_list: $ => in_parens(
      repeat($.symbol), 
      repeat(choice($.restvar, $.optvar, $.keyvar, $.auxvar, $.bodyvar, $.envvar, $.wholevar))),

    // &body var
    bodyvar: $ => seq("&body", field("var", $.symbol)),
    
    // &whole var
    wholevar: $ => seq("&whole", field("var", $.symbol)),

    // &environment var
    envvar: $ => seq("&environment", field("var", $.symbol)),

    declare: $ => in_parens("declare", repeat($._token)),

    documentation: _ => prec(PREC.documentation, STRING),

    // --- lambda  ---

    lambda: $ => in_parens(
      "lambda", $.lambda_list,
      repeat($.declare),
      optional(choice($._doc_body, $._body))),

    // --- defvar, defparameter ---

    defvar: $ => make_defvar($, "defvar"),

    defparameter: $ => make_defvar($, "defparameter", false),

    defconstant: $ => make_defvar($, "defconstant", false),

    // --- let, let* ---

    let: $ => in_parens(
      choice("let", "let*"),
      $.let_binds,
      repeat($.declare),
      repeat($._element)),

    let_binds: $ => in_parens(repeat($.let_bind)),

    let_bind: $ => choice(
      field("var", $._symbol),
      in_parens(field("var", $._symbol), optional(field("init", $._element)))),

    // --- defmethod ---

    // (defmethod function-name {method-qualifier}* specialized-lambda-list [[declaration* | documentation]] form*)
    defmethod: $ => in_parens(
      // is aliasing specialized_lambda_list a good choice?
      "defmethod", $.fn_name, repeat($.method_qual), alias($.specialized_lambda_list, $.lambda_list),
      repeat($.declare),
      optional(choice($._doc_body, $._body))),

    method_qual: $ => choice(":before", ":around", ":after"),

    // ({var | (var parameter-specializer-name)}*
    // [&optional {var | (var [initform [supplied-p-parameter] ])}*]
    // [&rest var]
    // [&key {var | ({var | (keywordvar)} [initform [supplied-p-parameter] ])}*
    //   [&allow-other-keys] ]
    // [&aux {var | (var [initform] )}*] )
    specialized_lambda_list: $ => in_parens(
      // {var | (var parameter-specializer-name)}*
      repeat(choice($._symbol, in_parens($._symbol, $.param_spec))),
      repeat(choice($.optvar, $.restvar, $.keyvar, $.auxvar))),

    // parameter-specializer-name ::= symbol | (eql eql-specializer-form)
    param_spec: $ => choice($._symbol, $.eql_spec),

    eql_spec: $ => in_parens("eql", $._element),

    // --- defgeneric ---

    // defgeneric function-name gf-lambda-list [[option | {method-description}*]]
    defgeneric: $ => in_parens(
      "defgeneric", $.fn_name,
      alias($.gf_lambda_list, $.lambda_list),
      repeat(choice($.gf_option, $.declare, $.gf_method_desc))),

    gf_lambda_list: $ => in_parens(
      repeat($.symbol),
      repeat(choice($.restvar,
                    alias($.gf_optvar, $.optvar),
                    alias($.gf_keyvar, $.keyvar)))),

    // Optional parameters and keyword parameters may not have default initial
    // value forms nor use supplied-p param
    gf_keyvar: $ => seq(
      "&key",
      repeat(
        choice(field("var", $.symbol),
               in_parens(
                 choice(field("var", $.symbol),
                        in_parens(field("kwd_name", $._symbol), field("var", $.symbol)))))),
      optional($.allow_other_keys)),

    gf_optvar: $ => seq(
      "&optional",
      repeat(choice(field("var", $.symbol),
                    in_parens(field("var", $.symbol))))),

    gf_option: $ => choice(
      in_parens(":documentation", $.documentation),
      in_parens(":argument-precedence-order", repeat($.symbol)),
      in_parens(":method-combination", $.symbol, $._element),
      in_parens(":generic-function-class", $.symbol),
      in_parens(":method-class", $.symbol)),

    gf_method_desc: $ => in_parens(
      ":method",
      repeat($.method_qual),
      alias($.specialized_lambda_list, $.lambda_list),
      repeat($.declare),
      optional(choice($._doc_body, $._body))),

    // --- destructuring-bind ---
    destr_bind: $ => in_parens(
      "destructuring-bind", 
      alias($.destr_lambda_list, $.lambda_list), 
      repeat($.declare),
      repeat($._element)),

    destr_lambda_list: $ => in_parens(
      repeat(choice($.symbol, alias($.destr_lambda_list, $.lambda_list))),
      repeat(choice($.restvar, $.optvar, $.keyvar, $.auxvar, $.bodyvar, $.wholevar))),

    // --- defclass ---
    defclass: $ => in_parens(
      "defclass",
      field("name", $._symbol),
      field("superclasses", $.superclass_list),
      field("slots", $.slot_list),
      repeat($.class_option)),

    superclass_list: $ => in_parens(repeat($._symbol)),

    slot_list: $ => in_parens(repeat($.slot)),

    slot: $ => choice(
      field("name", $.symbol),
      in_parens(field("name", $.symbol), repeat($.slot_option))),

    slot_option: $ => choice(
      seq(":reader", $.fn_name),
      seq(":writer", $.fn_name),
      seq(":accessor", $.fn_name),
      seq(":allocation", choice(":instance", ":class")),
      seq(":initarg", $._symbol),
      seq(":initform", $._element),
      seq(":type", $.type),
      seq(":documentation", $.documentation)),

    class_option: $ => choice(
      in_parens(":default-initargs", repeat(seq($._symbol, $._element))),
      in_parens(":documentation", $.documentation),
      in_parens(":metaclass", $._symbol)),

    // --- with-slots ---

    // with-slots (slot-entry*) instance-form declaration* form*
    with_slots: $ => in_parens(
      "with-slots",
      in_parens(repeat($.slot_entry)),
      $._element,
      repeat($.declare),
      repeat($._element)
    ),

    slot_entry: $ => choice(
      field("var", $.symbol),
      in_parens(field("var", $.symbol), field("slot", $.symbol))),

    // -- labels, flet, macrolet --

    labels: $ => in_parens(
      "labels",
      in_parens(repeat($.labels1)),
      repeat($.declare),
      optional($._body)),

    labels1: $ => in_parens($.fn_name, $.lambda_list,
      repeat($.declare),
      optional(choice($._doc_body, $._body))),

    flet: $ => in_parens(
      "flet",
      in_parens(repeat($.flet1)),
      repeat($.declare),
      optional($._body)),

    flet1: $ => in_parens($.fn_name, $.lambda_list,
      repeat($.declare),
      optional(choice($._doc_body, $._body))),

    macrolet: $ => in_parens(
      "macrolet",
      in_parens(repeat($.macrolet1)),
      repeat($.declare),
      optional($._body)),

    macrolet1: $ => in_parens($._symbol, $.lambda_list,
      repeat($.declare),
      optional(choice($._doc_body, $._body))),

    in_package: $ => in_parens(
      "in-package", $._string_designator),

    // --- type specifier ---
    type: $ => choice($._symbol, $.list),

  },

});


// =============================================================================
// Utilities
// =============================================================================

// returns object but without specified keys
function without(object, ...keys) {
  const result = {};
  for (const [k, v] of Object.entries(object)) {
    if (!keys.includes(k))
      result[k] = v;
  }
  return result;
}

// works like choice()
function obj_choice(object) {
  return choice(...Object.values(object));
}
