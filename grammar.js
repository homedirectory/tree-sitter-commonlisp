
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

    rules: {
        source: $ => 'hello',
    }

});

