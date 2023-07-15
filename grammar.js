
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

// 2.1.4 Character Syntax Types

const SYNTAX_TYPES = {

  constituent: choice(
    /[0-9:<=>?!@a-zA-Z$%&^_*{+}~]/, "\x08", ".", "\x7f", "/", "-", "[", "]"),

  macro_char_term: /[;,"'`()]/,
  macro_char_noterm: "#",

  escape_single: "\\",
  escape_multi: "|",

};


module.exports = grammar({

  name: 'commonlisp',

  extras: $ => [],

  rules: {

    source: $ => repeat(choice($._skip, $._token)),

    _skip: $ => WHITESPACE,

    _token: $ => choice($.number, $.symbol),

    number: $ => prec(PREC.number, /[0-9]+/),

    symbol: $ => prec(PREC.symbol, token(repeat1(SYNTAX_TYPES.constituent))),

  }

});

