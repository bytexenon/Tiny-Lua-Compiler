local Suite = require("tests.library")
local suite = Suite.new()

require("tests.lexer.lexer")(suite)
require("tests.parser.parser")(suite)
require("tests.bytecode_emitter.bytecode_emitter")(suite)
require("tests.language.semantics")(suite)

os.exit(suite:summary())
