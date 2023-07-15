
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

