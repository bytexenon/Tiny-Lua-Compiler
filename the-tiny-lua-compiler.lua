--[[
  The Tiny Lua Compiler (TLC)
  ===========================

  A minimal, educational compiler for Lua 5.1, written in Lua 5.1.

  This project demonstrates the full compilation pipeline:
  1. Tokenizer/Lexer: Converts source text into a stream of tokens.
  2. Parser: Organizes tokens into an Abstract Syntax Tree (AST).
  3. Code Generator: Translates the AST into a Function Prototype (proto).
  4. Bytecode Emitter: (Optional) Serializes the prototype to binary code (bytecode).
  5. Virtual Machine: Executes the prototype directly.

  Despite its small size (~4000 lines), it implements a register-based VM,
  lexical scoping, upvalues (closures), and proper operator precedence.
--]]

-- Converts a list into a Set (lookup table) for O(1) access.
--
-- In Lua, iterating over a list to check for existence is O(n). By converting
-- the list `{"a", "b"}` into `{a=true, b=true}`, we leverage Lua's internal
-- hash map architecture. This makes checking `if keywords[word]` instant,
-- regardless of how many keywords exist.
local function createLookupTable(list)
  local lookup = {}
  for _, value in ipairs(list) do
    lookup[value] = true
  end
  return lookup
end

-- Constructs a Prefix Tree (Trie) for efficient operator matching.
--
-- This structure solves the "Longest Prefix Match" problem. When the tokenizer
-- encounters a character like `>`, it needs to decide if it's a standalone `>`,
-- or part of a longer operator like `>=`.
--
-- Instead of complex lookahead logic, we traverse this tree. If we can travel
-- deeper (e.g., from `.` to `.`), we continue. If we stop, we know we've
-- matched the longest valid operator.
local function makeTrie(ops)
  local trie = {}
  for _, op in ipairs(ops) do
    local node = trie
    for char in op:gmatch(".") do
      node[char] = node[char] or {}
      node = node[char]
    end
    -- Mark this node as a valid ending point for an operator.
    node.value = op
  end
  return trie
end

-- Pre-computes character pattern matches into a lookup table.
--
-- Running `string.match(char, "%d")` on every character of the source code
-- is computationally expensive due to pattern matching overhead.
--
-- Since a byte can only have 256 possible values, we can pre-calculate
-- the result of the pattern match for every possible character (0-255).
-- This transforms a regex operation into a simple array lookup.
local function makePatternLookup(pattern)
  local lookup = {}
  for code = 1, 255 do
    local char = string.char(code)
    if char:match(pattern) then
      lookup[char] = true
    end
  end
  return lookup
end

--[[
    ============================================================================
                                      (/^▽^)/
                                   THE TOKENIZER!
    ============================================================================

    The First Stage of Compilation: Breaking Code Into Bite-Sized Pieces

    Think of the tokenizer (also called a lexical analyzer or scanner) as a
    factory machine that scans raw text (your Lua source code) character by
    character and outputs a stream of neatly labeled tokens - the fundamental
    units of meaning in the language. It's like splitting a sentence into
    individual words and punctuation marks, but for programming languages.
--]]

--* Tokenizer *--

-- The main object responsible for scanning the source code string
-- and producing a list of tokens.
local Tokenizer = {}
Tokenizer.__index = Tokenizer -- Set up for method calls via `.`

Tokenizer.CONFIG = {
  -- Single-character tokens.
  -- These are the simplest tokens to parse: if we see one of these characters,
  -- we immediately know what token it is without looking ahead.
  -- stylua: ignore
  PUNCTUATION = {
    [":"] = "Colon",       [";"] = "Semicolon",
    [","] = "Comma",       ["."] = "Dot",
    ["("] = "LeftParen",   [")"] = "RightParen",
    ["{"] = "LeftBrace",   ["}"] = "RightBrace",
    ["["] = "LeftBracket", ["]"] = "RightBracket",
    ["="] = "Equals"
  },

  -- Escape Sequence Mapping.
  -- Maps the character following a backslash '\' to its actual byte value.
  -- e.g., 'n' -> '\n' (newline).
  -- stylua: ignore
  ESCAPES =  {
    ["a"]  = "\a", ["b"]  = "\b", ["f"]  = "\f",
    ["n"]  = "\n", ["r"]  = "\r", ["t"]  = "\t",
    ["v"]  = "\v", ["\\"] = "\\", ["\""] = "\"",
    ["\'"] = "\'"
  },

  -- Reserved Keywords.
  -- These words look like identifiers but have special grammatical meaning.
  --
  -- Strategy: "Lex as Identifier, Check if Keyword"
  -- When the tokenizer reads a word like "while", it first consumes it as a
  -- generic identifier. Then, it performs an O(1) check against this table.
  -- If present, the token type is switched from "Identifier" to "Keyword".
  -- stylua: ignore
  KEYWORDS = createLookupTable({
    "and",      "break", "do",    "else",
    "elseif",   "end",   "false", "for",
    "function", "if",    "in",    "local",
    "nil",      "not",   "or",    "repeat",
    "return",   "then",  "true",  "until",
    "while"
  }),

  -- Operator Trie.
  -- A Prefix Tree containing all valid operators.
  -- Used by `consumeOperator` to perform "Longest Prefix Matching".
  -- stylua: ignore
  OPERATOR_TRIE = makeTrie({
    "^",  "*",  "/",  "%",  "+", "-", "<", ">", "#", -- Single-char
    "<=", ">=", "==", "~=", ".."                     -- Multi-char
  })
}

-- Character Classification Tables (Pre-computed)
--
-- Tokenization is the tightest loop in the compiler, running once per character.
-- Calling Lua's pattern matcher (`string.match`) inside this loop is too slow.
--
-- Instead, we pre-calculate the classification for every possible byte (0-255).
-- Checking `if PATTERNS.DIGIT[char]` becomes a simple array lookup, which is
-- orders of magnitude faster than regex matching.
-- stylua: ignore
Tokenizer.PATTERNS = {
  SPACE            = makePatternLookup("%s"),         -- Whitespace
  DIGIT            = makePatternLookup("%d"),         -- 0-9
  HEX_DIGIT        = makePatternLookup("[%da-fA-F]"), -- 0-9, a-f
  IDENTIFIER       = makePatternLookup("[%a%d_]"),    -- Valid inside identifier
  IDENTIFIER_START = makePatternLookup("[%a_]")       -- Valid start of identifier
}

--// Tokenizer Constructor //--
-- Creates a new Tokenizer instance for a given source code string.
function Tokenizer.new(code)
  --// Type Checking //--
  assert(type(code) == "string", "Tokenizer.new requires a string as input code. Got: " .. type(code))

  --// Instance Creation //--
  -- Create the instance table and set its metatable to `Tokenizer`.
  -- This grants the instance access to all methods defined in the Tokenizer
  -- class (like `consume`, `getNextToken`, `tokenize`, etc.) via prototypal
  -- inheritance, avoiding the need to copy methods for each instance.
  local self = setmetatable({}, Tokenizer)

  --// Initialization //--
  self.code = code

  -- Initialize the pointer to the current character position. Lua uses
  -- 1-based indexing for strings and tables, so we start at the first character.
  self.curCharPos = 1

  -- Initialize the current character being processed.
  self.curChar = code:sub(1, 1)

  return self
end

--// Character Navigation //--

-- Looks ahead by 'n' characters in the character stream without advancing
-- the current position. Returns the character at the looked-ahead position,
-- or nil if trying to look past the end of the stream.
function Tokenizer:lookAhead(n)
  local targetCharPos   = self.curCharPos + n
  local lookedAheadChar = self.code:sub(targetCharPos, targetCharPos)
  return lookedAheadChar
end

-- Consumes (advances past) 'n' characters in the character stream.
-- This is the primary way the tokenizer moves forward after identifying
-- or skipping characters/tokens. Updates `curCharPos` and `curChar`.
-- Returns the new current character after consumption.
-- Includes a safety check against infinite loops (e.g., if somehow
-- neither old nor new char is non-nil, which implies no progress).
function Tokenizer:consume(n)
  -- Simple safety check: if we're trying to reach past the end of the stream,
  -- we should throw an error. This prevents infinite loops in the tokenizer.
  if self.curChar == "" then
    error(
      "Internal Tokenizer Error: Attempted to consume past end of stream. "
      .. "Infinite loop detected?"
    )
  end

  local nextCharPos = self.curCharPos + n
  local nextChar    = self.code:sub(nextCharPos, nextCharPos)

  -- Update the tokenizer's state.
  self.curCharPos = nextCharPos
  self.curChar    = nextChar

  return nextChar
end

-- Consumes exactly one character, but only if it matches the expected character.
-- Used for consuming specific punctuation like '(' or '=' after identifying
-- a token kind that implies the next character must be that specific one.
-- Throws an error if the current character doesn't match the expectation.
function Tokenizer:consumeCharacter(character)
  if self.curChar == character then
    return self:consume(1) -- Match! Consume the character and move on.
  end

  -- No match? Syntax error detected at the lexical level.
  -- (An actual Tokenizer would include line/column info here for better error reporting)
  error(
    "Expected character '"
    .. character ..
    "', but found: '"
    .. tostring(self.curChar) ..
    "'"
  )
end

--// Character Checkers //--
-- These functions use the pre-calculated pattern lookup tables
-- for efficient O(1) checks of individual character properties.

-- Checks if a given character is a standard whitespace character.
function Tokenizer:isWhitespace(char)
  return self.PATTERNS.SPACE[char]
end

--- Checks if a given character is a newline character.
function Tokenizer:isNewlineCharacter(char)
  return char == "\n"
end

-- Checks if a given character is a digit (0-9).
function Tokenizer:isDigit(char)
  return self.PATTERNS.DIGIT[char]
end

-- Checks if a given character is a valid hexadecimal digit (0-9, a-f, A-F).
-- Used when scanning hexadecimal number literals (e.g., 0xFF).
function Tokenizer:isHexadecimalNumber(char)
  return self.PATTERNS.HEX_DIGIT[char]
end

-- Checks if a given character is valid as the first character of an identifier.
-- Identifiers must start with a letter or an underscore.
function Tokenizer:isIdentifierStart(char)
  return self.PATTERNS.IDENTIFIER_START[char]
end

-- Checks if a given character is valid after the first character of an identifier.
-- Identifiers can contain letters, digits, or underscores.
function Tokenizer:isIdentifier(char)
  return self.PATTERNS.IDENTIFIER[char]
end

--// Multi-Character Checkers //--
-- These functions look at the current character and potentially the next few
-- characters to determine if they collectively form the start of a larger
-- lexical element like a number, vararg, comment, or string.

-- Checks if the current character sequence suggests the start of a number.
-- This can be a digit, or a decimal point followed by a digit.
function Tokenizer:isNumberStart()
  local curChar = self.curChar
  return (
    self:isDigit(curChar) -- Starts with a digit (e.g., 123, 1.0).
    or (
      -- Starts with a decimal point followed by a digit (e.g., .5).
      -- This matches patterns like ".5" but not just ".".
      curChar == "." and self:isDigit(self:lookAhead(1))
    )
  )
end

-- Checks if the current character sequence is the prefix for a hexadecimal number.
-- This is specifically the "0x" or "0X" sequence.
function Tokenizer:isHexadecimalNumberPrefix()
  local nextChar = self:lookAhead(1)
  return self.curChar == "0" and (
    nextChar == "x" or nextChar == "X"
  )
end

-- Checks if the current character sequence is the vararg literal "...".
function Tokenizer:isVararg()
  return self.curChar       == "."
      and self:lookAhead(1) == "."
      and self:lookAhead(2) == "."
end

-- Checks if the current character sequence is the start of a comment "--".
function Tokenizer:isComment()
  return self.curChar       == "-"
      and self:lookAhead(1) == "-"
end

-- Checks if the current character sequence is the start of a string literal.
-- This can be a single quote ('), double quote ("), or the start of a
-- long string (`[` followed by `[` or `=`).
function Tokenizer:isString()
  local curChar  = self.curChar
  local nextChar = self:lookAhead(1)
  -- Simple strings start with single or double quotes.
  return (curChar == '"' or curChar == "'")
      -- Long strings start with `[` followed by `[` or `=`.
      or (curChar == "[" and (
        nextChar == "[" or nextChar == "="
      ))
end

--// Utils //--
-- These are helper functions used internally by the consumers
-- for specific parsing tasks within tokens (like strings or comments).

-- Calculates the "depth" of a long string or comment delimiter.
-- This is the number of '=' signs between the outer brackets,
-- e.g., `[==[...]==]` has a depth of 2.
-- and `[[...]]` has a depth of 0.
-- Returns the depth (an integer >= 0).
function Tokenizer:calculateDelimiterDepth()
  local depth = 0
  while self.curChar == "=" do
    depth = depth + 1
    self:consume(1)
  end

  return depth
end

-- Consumes ending delimiters for long strings or comments.
function Tokenizer:consumeUntilEndingDelimiter(depth)
  local startPos = self.curCharPos

  while true do
    if self.curChar == "]" then
      self:consume(1) -- Consume the "]".
      local depthMatch = self:calculateDelimiterDepth()
      if self.curChar == "]" and depthMatch == depth then
        -- Consume the closing brackets and return the string.
        self:consume(1) -- Consume the "]".
        return self.code:sub(startPos, self.curCharPos - depth - 3)
        -- -3 is to account for the two closing brackets and the last character.
      end

    -- Check if it's end of stream.
    elseif self.curChar == "" then
      -- End of stream reached without finding the delimiter.
      error("Unexpected end of input while searching for ending delimiter")
    else
      -- Consume the current character and move to the next one in the stream.
      self:consume(1)
    end
  end
end

-- Consumes a numeric escape sequence (\ddd) and returns the corresponding character.
-- Expects the current character to be the first digit after the '\'.
-- Validates that the number is within the byte range (0-255).
function Tokenizer:consumeNumericEscapeSequence(firstDigit)
  local numberString = firstDigit

  -- Consume up to two more digits.
  for _ = 1, 2 do
    local nextChar = self:lookAhead(1)
    if not self:isDigit(nextChar) then
      -- Stop if the next char isn't a digit.
      break
    end
    numberString = numberString .. nextChar -- Append the digit to the string.
    self:consume(1) -- Consume the digit.
  end

  -- Convert the collected digits to a number.
  local number = tonumber(numberString)
  if not number or number > 255 then
    error("escape sequence too large near '\\" .. numberString .. "'")
  end

  -- Return the character corresponding to the number code
  return string.char(number)
end

--// Consumers //--
-- These functions are responsible for consuming a specific kind of token
-- from the character stream and returning its value. They handle the
-- internal structure of the token (like string contents or number format).

-- Consumes a sequence of one or more whitespace characters from the input stream.
-- This function advances the tokenizer's position (`curCharPos` and `curChar`)
-- past any contiguous block of whitespace (spaces, tabs, newlines, etc.).
function Tokenizer:consumeWhitespace()
  while self:isWhitespace(self.curChar) do
    self:consume(1) -- Advance the stream position by one character.
  end

  -- No return value; the function's effect is modifying the tokenizer's state.
end

-- Consumes an identifier (variable name, function name, etc.) from the input stream.
-- An identifier starts with a letter or underscore, followed by any combination
-- of letters, digits, or underscores.
function Tokenizer:consumeIdentifier()
  local startPos = self.curCharPos

  while self:isIdentifier(self.curChar) do
    self:consume(1)
  end

  return self.code:sub(startPos, self.curCharPos - 1)
end

function Tokenizer:consumeNumber()
  local startPos = self.curCharPos

  -- Hexadecimal number case.
  -- 0[xX][0-9a-fA-F]+
  if self:isHexadecimalNumberPrefix() then
    self:consume(2) -- Consume the "0x" part.
    while self:isHexadecimalNumber(self.curChar) do
      self:consume(1)
    end
    return self.code:sub(startPos, self.curCharPos - 1)
  end

  -- [0-9]*
  while self:isDigit(self.curChar) do
    self:consume(1)
  end

  -- Floating point number case.
  -- \.[0-9]+
  if self.curChar == "." then
    self:consume(1) -- Consume the ".".

    -- Lua allows you to end a number with a decimal point (e.g., "42."),
    -- so this check doesn't expect any digits after the decimal.
    while self:isDigit(self.curChar) do
      self:consume(1)
    end
  end

  -- Exponential (scientific) notation case.
  -- [eE][+-]?[0-9]+
  if self.curChar == "e" or self.curChar == "E" then
    self:consume(1) -- Consume the "e" or "E" characters.
    if self.curChar == "+" or self.curChar == "-" then
      self:consume(1) -- Consume an optional sign character.
    end

    -- Exponent part.
    while self:isDigit(self.curChar) do
      self:consume(1)
    end
  end

  return self.code:sub(startPos, self.curCharPos - 1)
end

function Tokenizer:consumeEscapeSequence()
  -- Consume the "\" character.
  self:consumeCharacter("\\")

  local convertedChar = self.CONFIG.ESCAPES[self.curChar]
  if convertedChar then
    return convertedChar
  elseif self:isDigit(self.curChar) then
    return self:consumeNumericEscapeSequence(self.curChar)
  end

  error("invalid escape sequence near '\\" .. self.curChar .. "'")
end

function Tokenizer:consumeSimpleString()
  local delimiter = self.curChar
  local newString = {}
  self:consume(1) -- Consume the quote.

  while self.curChar ~= delimiter do
    -- Check if it's end of stream.
    if self.curChar == "" then
      error("Unclosed string")
    elseif self.curChar == "\\" then
      local consumedEscape = self:consumeEscapeSequence()
      table.insert(newString, consumedEscape)
    else
      table.insert(newString, self.curChar)
    end
    self:consume(1)
  end
  self:consume(1) -- Consume the closing quote.

  return table.concat(newString)
end

function Tokenizer:consumeLongString()
  self:consumeCharacter("[")
  local depth = self:calculateDelimiterDepth()
  self:consumeCharacter("[")

  -- Long string starts with a newline?
  if self:isNewlineCharacter(self.curChar) then
    -- This is an edge case in the official lexer source.
    --
    -- ```
    -- static void read_long_string (LexState *ls, SemInfo *seminfo, int sep) {
    --   ...
    --   if (currIsNewline(ls))  /* string starts with a newline? */
    --     inclinenumber(ls);  /* skip it */
    --   ...
    -- ```
    -- Source: https://www.lua.org/source/5.1/llex.c.html#read_long_string

    -- Skip it.
    self:consume(1)
  end

  local stringValue = self:consumeUntilEndingDelimiter(depth)
  return stringValue
end

-- Consumes both long and simple strings.
function Tokenizer:consumeString()
  if self.curChar == "[" then
    return self:consumeLongString()
  end
  return self:consumeSimpleString()
end

-- Consumes an operator token from the input stream.
-- Uses a Trie (prefix tree) to efficiently match the longest possible operator.
-- This handles cases like distinguishing between `<` and `<=` or `.` and `..`
-- without complex lookahead logic.
function Tokenizer:consumeOperator()
  local node = self.CONFIG.OPERATOR_TRIE
  local operator

  -- Walk the trie character by character.
  local index = 0
  while true do
    local character = self:lookAhead(index)
    node = node[character]

    -- If the path doesn't exist in the trie,
    -- it means that we've gone as far as we can.
    if not node then break end

    -- If this node represents a valid operator, remember it.
    -- We keep going to see if there's a longer match (greedy matching).
    operator = node.value
    index    = index + 1
  end

  -- If no valid operator was found on the path, it's not an operator.
  if not operator then return end

  -- Skip over the operator characters.
  self:consume(#operator)

  return operator
end

function Tokenizer:consumeShortComment()
  while self.curChar ~= "" and not self:isNewlineCharacter(self.curChar) do
    self:consume(1)
  end

  -- No return value; the function's effect is modifying the tokenizer's state.
end

function Tokenizer:consumeLongComment()
  self:consumeCharacter("[")
  if self.curChar ~= "[" then
    return self:consumeShortComment()
  end
  local depth = self:calculateDelimiterDepth()
  self:consumeCharacter("[")
  self:consumeUntilEndingDelimiter(depth)

  -- No return value; the function's effect is modifying the tokenizer's state.
end

function Tokenizer:consumeComment()
  self:consume(2) -- Consume the "--".
  if self.curChar == "[" then
    return self:consumeLongComment()
  end
  return self:consumeShortComment()
end

--// Token Consumer Handler //--
function Tokenizer:getNextToken()
  local curChar = self.curChar

  -- Skip ignorable elements (whitespace and comments).
  if self:isWhitespace(curChar) then
    self:consumeWhitespace()
    return nil -- Return nil to signal that nothing tokenizable was found (skip).
  elseif self:isComment() then
    self:consumeComment()
    return nil -- Return nil to signal that nothing tokenizable was found (skip).
  end

  -- Process identifiers and reserved words AFTER literals.
  -- (e.g., "while" shouldn't be tokenized if it's part of a string)
  if self:isIdentifierStart(curChar) then
    -- Consume the sequence of valid identifier characters.
    local identifier = self:consumeIdentifier()

    if self.CONFIG.KEYWORDS[identifier] then
      return { kind = "Keyword", value = identifier }
    end

    -- Fallthrough: If it's not a keyword, it's a regular identifier (e.g. a variable).
    return { kind = "Identifier", value = identifier }
  end

  -- Handle numeric literals (decimal, hex, scientific, float).
  if self:isNumberStart() then
    local numberString = self:consumeNumber()
    local numberValue = tonumber(numberString)

    -- Basic check, tonumber handles many errors but not all contextually
    if numberValue == nil then
      error("Invalid number format near '" .. numberString .. "'")
    end

    return { kind = "Number", value = numberValue }
  end

  -- Handle string literals (simple or long).
  if self:isString() then
    local stringLiteral = self:consumeString() -- Consume the string contents.
    return { kind = "String", value = stringLiteral }
  end

  -- Handle complex literals or special multi-character tokens.
  -- Check for vararg "..." before numbers/operators that might start with ".".
  if self:isVararg() then
    self:consume(3) -- Consume the "...".
    return { kind = "Vararg" }
  end

  -- Attempt to consume a symbolic operator using the trie.
  -- This handles operators like "+", "==", "<=", "..", etc.
  local operator = self:consumeOperator()
  if operator then
    -- If an operator was matched and consumed, return it as a token.
    return { kind = "Operator", value = operator }
  end

  -- If no other token kinds matched, look up the current character
  -- in the self.CONFIG.PUNCTUATION lookup table. This handles single-character
  -- tokens like punctuation, separators, or other special characters.
  local tokenKind = self.CONFIG.PUNCTUATION[curChar]
  if tokenKind then
    self:consume(1) -- Consume the character.
    return { kind = tokenKind }
  end

  -- If we reach this point, it means the current character doesn't match
  -- any known token kind. This is an error condition, as the character
  -- is not valid in the Lua language syntax.
  error("Unexpected character '" .. curChar .. "' at position " .. self.curCharPos)
end

--// Tokenizer Main Method //--
-- The public method to perform the entire tokenization process
-- on the input code string provided during initialization.
-- Returns a list (Lua table with integer keys) of all identified tokens,
-- excluding whitespace and comments.
function Tokenizer:tokenize()
  local tokens = {}
  local tokenIndex = 1

  -- Loop through the character stream as long as there are characters left.
  while self.curChar ~= "" do
    -- Get the next token (or nil if whitespace/comment was skipped).
    local token = self:getNextToken()

    -- If `getNextToken` returned a valid token (not nil)...
    if token then
      -- Add the token to our list of results.
      -- Note: We don't use table.insert here as this is a very performance-critical
      -- section of the code. Using a simple index is faster for sequential inserts.
      tokens[tokenIndex] = token
      tokenIndex = tokenIndex + 1
    end
  end

  -- Return the list of collected tokens.
  return tokens
end

--[[
    ============================================================================
                  (ﾉ◕ヮ◕)ﾉ*:･ﾟ THE PARSER!
    ============================================================================

    The Second Stage of Compilation: Building the Structure

    While the Tokenizer sees a flat list of words ("if", "x", "then"), the
    Parser sees the structure and hierarchy of the code. It organizes tokens
    into an Abstract Syntax Tree (AST).

    Strategy: Recursive Descent
    ---------------------------
    This parser uses a "Recursive Descent" strategy. This means we have a
    separate Lua function for every grammar rule (e.g., `parseIfStatement`,
    `parseWhileStatement`). These functions call each other recursively to
    match the structure of the code. It is the most intuitive way to write a
    parser by hand.
--]]

--* Parser *--
-- The main object responsible for consuming tokens and building the AST.
local Parser = {}
Parser.__index = Parser -- Set up for method calls via `.`.

Parser.CONFIG = {
  --[[
    Operator Precedence & Associativity Table
    -----------------------------------------
    This table drives the "Precedence Climbing" algorithm used in `parseBinaryExpression`.
    It answers two questions:
    1. Binding Power: In `1 + 2 * 3`, why is `*` evaluated before `+`?
    2. Associativity: In `a ^ b ^ c`, do we calculate `(a^b)^c` or `a^(b^c)`?

    Format: { left_binding_power, right_binding_power }

    How it works:
    - Higher numbers mean tighter binding (evaluated sooner).
    - Left-Associative (e.g., `+`, `-`, `*`):
        Left power >= Right power.
        This makes `1 - 2 - 3` parse as `(1 - 2) - 3`.
    - Right-Associative (e.g., `^`, `..`):
        Right power > Left power.
        This makes `a ^ b ^ c` parse as `a ^ (b ^ c)`.
  --]]
  -- stylua: ignore
  PRECEDENCE = {
    ["+"]   = {6, 6},  ["-"]   = {6, 6}, -- Addition/Subtraction
    ["*"]   = {7, 7},  ["/"]   = {7, 7}, -- Multiplication/Division
    ["%"]   = {7, 7},                    -- Modulo
    ["^"]   = {10, 9},                   -- Exponentiation (Right-associative!)
    [".."]  = {5, 4},                    -- String Concatenation (Right-associative!)
    ["=="]  = {3, 3},  ["~="]  = {3, 3}, -- Equality / Inequality
    ["<"]   = {3, 3},  [">"]   = {3, 3}, -- Less Than / Greater Than
    ["<="]  = {3, 3},  [">="]  = {3, 3}, -- Less Than or Equal / Greater Than or Equal
    ["and"] = {2, 2},                    -- Logical AND (Short-circuiting)
    ["or"]  = {1, 1}                     -- Logical OR (Short-circuiting)
  },

  -- Precedence for all unary operators (like `-x` or `not x`).
  -- They bind tighter than most binary operators but looser than exponentiation.
  UNARY_PRECEDENCE = 8,

  -- L-Values (Locator Values).
  -- These are the specific node types that are allowed to appear on the
  -- LEFT side of an assignment (e.g., `x = 1` or `t.k = 1`).
  -- You can't assign to a number (`5 = 1` is invalid), so "NumericLiteral" isn't here.
  LVALUE_NODES = createLookupTable({ "Variable", "IndexExpression" }),

  -- Keywords that explicitly terminate a code block.
  -- When the parser sees one of these, it knows the current list of statements is done.
  TERMINATION_KEYWORDS = createLookupTable({ "end", "else", "elseif", "until" }),

  -- Statement Dispatcher.
  -- Maps a keyword to the specific function responsible for parsing that statement.
  -- stylua: ignore
  KEYWORD_HANDLERS = {
    ["do"]       = "parseDoStatement",
    ["break"]    = "parseBreakStatement",
    ["for"]      = "parseForStatement",
    ["function"] = "parseFunctionDeclaration",
    ["if"]       = "parseIfStatement",
    ["local"]    = "parseLocalStatement",
    ["repeat"]   = "parseRepeatStatement",
    ["return"]   = "parseReturnStatement",
    ["while"]    = "parseWhileStatement"
  },

  -- Lookups for quick operator classification.
  UNARY_OPERATORS  = createLookupTable({ "-", "#", "not" }),

  -- stylua: ignore
  BINARY_OPERATORS = createLookupTable({
    "+",  "-",   "*",  "/",
    "%",  "^",   "..", "==",
    "~=", "<",   ">",  "<=",
    ">=", "and", "or"
  })
}

--// Parser Constructor //--
-- Creates a new Parser instance given a list of tokens from the Tokenizer.
function Parser.new(tokens)
  --// Type Checking //--
  assert(type(tokens) == "table", "Parser.new requires a table of tokens. Got: " .. type(tokens))

  --// Instance Creation //--
  local self = setmetatable({}, Parser)

  --// Initialization //--
  self.tokens = tokens
  self.currentTokenIndex = 1
  self.currentToken = tokens[1]

  -- Initialize the stack for managing variable scopes.
  self.scopeStack = {}
  self.currentScope = nil -- Pointer to the topmost scope on the stack.

  return self
end

--// Token Navigation //--

-- Looks ahead by 'n' tokens in the token stream without advancing
-- the current position. Useful for peeking to decide which grammar rule
-- to apply (e.g., looking for '=' after an identifier to know if it's an assignment).
-- Returns the token at the looked-ahead index, or nil if trying to look past the end.
function Parser:lookAhead(n)
  local targetTokenIndex = self.currentTokenIndex + n
  return self.tokens[targetTokenIndex]
end


-- Consumes (advances past) 'n' tokens in the token stream.
-- This is how the parser moves forward after successfully parsing a part of the code.
-- Updates `currentTokenIndex` and `currentToken`.
-- Returns the new current token after consumption.
-- Assumes n is a positive integer.
function Parser:consume(n)
    -- Simple safety check: if we're trying to reach past the end of the stream,
    -- we should throw an error. This prevents infinite loops in the parser.
  if not self.currentToken and self.currentTokenIndex ~= 1 then
    error(
      "Internal Parser Error: Attempted to consume past end of stream. "
      .. "Infinite loop detected?"
    )
  end

  -- Advance the token index
  local newTokenIndex = self.currentTokenIndex + n
  local newToken      = self.tokens[newTokenIndex]
  self.currentTokenIndex = newTokenIndex
  self.currentToken      = newToken

  return newToken
end

-- Helper to get a human-readable description of a token for error messages.
-- Returns a string describing the token kind and value (if any).
function Parser:getTokenDescription(token)
  if not token then return "<end of file>" end
  if not token.value then return tostring(token.kind) end
  return string.format("%s [%s]", tostring(token.kind), tostring(token.value))
end

-- Helper to report parsing errors.
-- All error calls should ideally go through this for consistent reporting.
function Parser:error(message)
  -- Homework idea: Add location information to the error message
  -- (e.g., line number, column number, etc.) for better debugging.

  error(
    string.format(
      "Parser Error: %s",
      message
    )
  )
end

-- Consumes the current token, but only if its kind AND value match the expectation.
-- Used for consuming specific keywords or characters (like `end`, `(`, `=`).
-- Throws a detailed error with token location if the expectation is not met.
-- This is a strong check for expected syntax elements.
function Parser:consumeToken(tokenKind, tokenValue)
  local token = self.currentToken

  -- Check if the current token exists and matches the expected kind and value.
  if token and token.kind == tokenKind and token.value == tokenValue then
    -- Match! Consume this token and exit the function.
    self:consume(1)
    return
  end

  -- If we didn't match, throw a syntax error.
  self:error(
    string.format(
      "Expected %s [%s] token, got: %s",
      tostring(tokenKind),
      tostring(tokenValue),
      self:getTokenDescription(token)
    )
  )
end

--// Scope Management //--
-- These functions manage the parser's understanding of variable scopes
-- (blocks, functions) as it traverses the AST structure implicitly during parsing.

-- Enters a new scope by pushing a new scope object onto the scope stack.
-- This new scope becomes the `currentScope`.
-- `isFunctionScope` is a crucial flag: only function scopes create a boundary
-- that causes variables from enclosing scopes to become 'upvalues' if accessed.
function Parser:enterScope(isFunctionScope)
  local newScope = {
    localVariables = {}, -- Used to track local variables declared in this scope.
    isFunctionScope = isFunctionScope or false, -- Is this a function's scope?
  }
  table.insert(self.scopeStack, newScope) -- Push the new scope onto the stack.
  self.currentScope = newScope -- The new scope is now the active one.
  return newScope
end

-- Exits the current scope by popping it from the scope stack.
-- The previous scope on the stack (if any) becomes the `currentScope`.
-- Called when leaving a block, loop body, or function definition.
function Parser:exitScope()
  table.remove(self.scopeStack)

  -- The new `currentScope` is the one now at the top
  -- of the stack (or nil if stack is empty).
  self.currentScope = self.scopeStack[#self.scopeStack]
end

--// In-Scope Variable Management //--
-- These functions help track local variable declarations within the current scope.

-- Records that a local variable with `variable` name has been declared
-- in the `currentScope`. Used when parsing `local variable` or function parameters.
function Parser:declareLocalVariable(variable)
  -- Mark the variable name as existing in the current scope's local list.
  self.currentScope.localVariables[variable] = true
end

-- Declares a list of local variables.
function Parser:declareLocalVariables(variables)
  local currentLocalVariables = self.currentScope.localVariables
  for _, variable in ipairs(variables) do
    currentLocalVariables[variable] = true
  end
end

-- Resolves a variable name to its scope type: Local, Upvalue, or Global.
-- This implements Lua's lexical scoping rules by walking up the scope chain.
--
-- Local: Found in the current function's scope or its blocks.
-- Upvalue: Found in an enclosing function's scope (closure capture).
-- Global: Not found in any open scope, assumed to be in _G.
function Parser:getVariableType(variableName)
  local scopeStack = self.scopeStack
  local isUpvalue  = false

  -- Walk up the scope stack from innermost to outermost.
  for scopeIndex = #scopeStack, 1, -1 do
    local scope = scopeStack[scopeIndex]

    -- If found, determine if it's a local or an upvalue based on whether
    -- we have crossed a function boundary during the search.
    if scope.localVariables[variableName] then
      local variableType = (isUpvalue and "Upvalue") or "Local"
      return variableType, scopeIndex

    -- A function scope acts as a closure boundary. Any variable found
    -- in a scope above this one must be captured as an upvalue.
    elseif scope.isFunctionScope then
      isUpvalue = true
    end
  end

  -- If the variable isn't declare in any enclosing scope,
  -- Lua assumes it refers to a global variable.
  return "Global"
end

--// Token Checkers //--
-- Lightweight checks to see if a token matches a specific kind and/or value
-- without throwing an error if it doesn't match.

-- Checks if the given `token` (or `self.currentToken`) is of the specified `kind`.
-- This is a generic check for any token kind (e.g., "Identifier", "Number", etc.).
function Parser:checkTokenKind(kind, token)
  token = token or self.currentToken
  return token and token.kind == kind
end

-- Checks if the current token is a 'Keyword'
-- token with the specified `keyword` value.
function Parser:checkKeyword(keyword)
  local token = self.currentToken
  return token
        and token.kind  == "Keyword"
        and token.value == keyword
end

-- Checks if the current token is the comma character ','.
-- Used frequently in parsing lists (argument lists, variable lists, etc.).
function Parser:isComma()
  local token = self.currentToken
  return token and token.kind == "Comma"
end

-- Checks if the current token is a recognized unary operator.
-- Uses the `self.CONFIG.UNARY_OPERATORS` lookup table.
function Parser:isUnaryOperator()
  local token = self.currentToken
  return token
        and (token.kind == "Operator" or token.kind == "Keyword")
        and self.CONFIG.UNARY_OPERATORS[token.value]
end

-- Checks if the current token is a recognized binary operator.
-- Uses the `self.CONFIG.BINARY_OPERATORS` lookup table.
function Parser:isBinaryOperator()
  local token = self.currentToken
  return token
        and (token.kind == "Operator" or token.kind == "Keyword")
        and self.CONFIG.BINARY_OPERATORS[token.value]
end

--// Token Expectation //--
-- These functions check the current token's kind or value without consuming it.
-- Useful for making parsing decisions ("Is the next thing an identifier?").

-- Checks if the current token has the expected kind. Throws an error otherwise.
function Parser:expectTokenKind(expectedKind)
  if self:checkTokenKind(expectedKind) then
    local previousToken = self.currentToken
    self:consume(1) -- Advance to the next token.
    return previousToken
  end

  -- No match? Syntax error.
  local actualKind = self.currentToken.kind
  self:error(
    string.format(
      "Expected a %s, but found %s",
      tostring(expectedKind),
      tostring(actualKind)
    )
  )
end

-- Checks if the current token is a 'Keyword' kind with the expected value.
-- Throws an error otherwise.
function Parser:expectKeyword(keyword)
  local token = self.currentToken

  -- Check if the current token is a keyword and matches the value.
  if self:checkKeyword(keyword) then
    self:consume(1) -- Advance to the next token.
    return true
  end

  -- No match? Syntax error.
  self:error(
    string.format(
      "Expected keyword '%s', but found %s",
      keyword,
      self:getTokenDescription(token)
    )
  )
end

--// AST Node Checkers //--
-- Functions to check properties of already-parsed AST nodes.

-- Checks if a given AST `node` is a valid target for the left-hand side of an assignment.
-- Valid LValues are single variables or table access expressions.
-- Uses the `self.CONFIG.LVALUE_NODES` lookup table.
function Parser:isValidAssignmentLvalue(node)
  return node and self.CONFIG.LVALUE_NODES[node.kind]
end

--// Parsers //--
-- These functions are responsible for parsing specific syntactic constructs
-- in the token stream and building the corresponding AST nodes.

function Parser:consumeIdentifier()
  local identifierValue = self:expectTokenKind("Identifier").value
  return identifierValue
end

function Parser:consumeVariable()
  local variableName = self:expectTokenKind("Identifier").value
  local variableType = self:getVariableType(variableName)

  return { kind = "Variable",
    name         = variableName,
    variableType = variableType
  }
end

-- Parses a comma-separated list of identifiers.
-- Consumes the identifier tokens and commas.
-- Example: `a, b, c` -> returns `{"a", "b", "c"}`
function Parser:consumeIdentifierList()
  local identifiers = {}

  -- Loop as long as the current token is an identifier.
  while self.currentToken and self.currentToken.kind == "Identifier" do
    -- Add the identifier's value (name) to the list.
    table.insert(identifiers, self.currentToken.value)
    self:consume(1) -- Consume the identifier token.

    -- Check if the current token is a comma. If not, the list ends after
    -- the current identifier.
    if not self:isComma() then break end
    self:consume(1) -- Consume the comma and continue the loop.
  end

  return identifiers
end

-- Parses a parameter list within a function definition.
-- Handles regular named parameters and the optional vararg token `...`.
-- Consumes the entire parameter list syntax, including parentheses and commas.
-- Returns a list of parameter names and a boolean if vararg is present.
-- Example: `(a, b, c, ...)` -> returns `{"a", "b", "c"}, true`
function Parser:consumeParameters()
  self:consumeToken("LeftParen") -- Expect and consume the opening parenthesis.

  local parameters = {} -- List of parameter names.
  local isVararg = false -- Flag for varargs.

  -- Loop as long as the current token is NOT the closing parenthesis.
  while self.currentToken and not self:checkTokenKind("RightParen") do
    -- Check for regular named parameters (must be identifiers).
    if self.currentToken.kind == "Identifier" then
      -- Add the parameter name to the list.
      table.insert(parameters, self.currentToken.value)
      self:consume(1) -- Consume the identifier token.

    -- Check for the vararg token "...".
    elseif self.currentToken.kind == "Vararg" then
      isVararg = true -- Set the vararg flag.
      self:consumeToken("Vararg") -- Expect and consume the "..." token.
      -- According to Lua grammar, "..." must be the last parameter.
      break -- Exit the loop after consuming vararg.

    -- If it's neither an identifier nor vararg, it's a syntax error.
    else
       self:error(
        "Expected parameter name or '...' in parameter list, but found: " ..
        tostring(self.currentToken.kind)
      )
    end

    -- After a parameter, we expect a comma, unless it's the last parameter
    -- before the closing parenthesis. Check if the current token is a comma.
    -- If not, break the loop (assuming it's the closing paren).
    if not self:isComma() then break end
    self:consume(1) -- Consume the comma.
  end -- Loop ends when ')' is encountered or after consuming '...'.

  -- Expect and consume the closing parenthesis.
  self:consumeToken("RightParen")

  return parameters, isVararg
end

-- Parses a table index expression, e.g., `table.key` or `table["key"]`.
-- `currentExpression` is the AST node representing the table being accessed.
-- Returns an AST node representing the index access.
function Parser:consumeIndexExpression(currentExpression)
  local isPrecomputed = true
  local indexExpression

  -- table.key syntax.
  if self:checkTokenKind("Dot") then
    self:consume(1) -- Consume the '.' character.
    local identifier = self:consumeIdentifier()
    indexExpression = { kind = "StringLiteral",
      value = identifier
    }

  -- table["key"] syntax.
  else
    self:expectTokenKind("LeftBracket") -- Expect and consume the '[' character.
    indexExpression = self:consumeExpression()
    isPrecomputed   = false
    if not indexExpression then
      self:error("Expected an expression inside brackets for table index")
    end
    self:expectTokenKind("RightBracket")
  end

  -- Create the AST node for the table index access.
  return { kind = "IndexExpression",
    base          = currentExpression,
    index         = indexExpression,
    isPrecomputed = isPrecomputed
    -- NOTE: `IsPrecomputed` is not used in the compiler, it's present to make
    -- third-party tools easier to build (e.g., linters, formatters).
  }
end

function Parser:consumeTable()
  self:consumeToken("LeftBrace") -- Consume the "{".

  local implicitKeyCounter = 1
  local tableElements      = {}

  -- Loop through tokens until we find the closing "}".
  while self.currentToken and not self:checkTokenKind("RightBrace") do
    local key, value
    local isImplicitKey = false

    -- Determine which kind of table field we are parsing.
    if self:checkTokenKind("LeftBracket") then
      -- Explicit key in brackets, e.g., `[1+2] = "value"`.
      -- [<expression>] = <expression>
      self:consume(1) -- Consume "["
      key = self:consumeExpression()
      self:expectTokenKind("RightBracket")
      self:expectTokenKind("Equals")
      value = self:consumeExpression()

    elseif self:checkTokenKind("Identifier") and
           self:checkTokenKind("Equals", self:lookAhead(1)) then
      -- Identifier key, e.g., `name = "value"`.
      -- This is syntatic sugar for `["name"] = "value"`.
      -- <identifier> = <expression>
      key = { kind = "StringLiteral",
        value = self.currentToken.value
      }
      self:consume(1) -- Consume the identifier.
      self:consume(1) -- Consume the "=".
      value = self:consumeExpression()

    else
      -- Implicit numeric key, e.g. `"value1", MY_VAR, 42`
      -- This is syntatic sugar for `[1] = "value1", [2] = MY_VAR, [3] = 42`.
      -- <expression>
      isImplicitKey = true
      key = { kind = "NumericLiteral",
        value = implicitKeyCounter
      }
      implicitKeyCounter = implicitKeyCounter + 1
      value = self:consumeExpression()
    end

    -- Create the AST node for this table element.
    local element = { kind = "TableElement",
      key           = key,
      value         = value,
      isImplicitKey = isImplicitKey
    }

    table.insert(tableElements, element)

    -- Table elements can be separated by "," or ";". If no separator is
    -- found, we assume it's the end of the table definition.
    if not (self:checkTokenKind("Comma") or self:checkTokenKind("Semicolon")) then
      break
    end
    self:consume(1) -- Consume the separator.
  end

  self:consumeToken("RightBrace") -- Consume the "}" symbol.

  return { kind = "TableConstructor",
    elements = tableElements
  }
end

function Parser:consumeFunctionCall(currentExpression, isMethodCall)
  local currentToken     = self.currentToken
  local currentTokenKind = currentToken.kind
  local arguments

  -- Explicit call with parentheses: `f(a, b)`.
  if currentTokenKind == "LeftParen" then
    self:consume(1) -- Consume the left parenthesis '('.
    arguments = self:consumeExpressions()
    self:consumeToken("RightParen") -- Expect and consume the right parenthesis ')'.

  -- Implicit call with a string: `print "hello" `
  elseif currentTokenKind == "String" then
    self:consume(1) -- Consume the string.
    arguments = { {
      kind  = "StringLiteral",
      value = currentToken.value
    } }

  -- Implicit call with a table: `f {1, 2, 3} `.
  elseif currentTokenKind == "LeftBrace" then
    arguments = { self:consumeTable() }
  end

  return { kind = "FunctionCall",
    callee       = currentExpression,
    arguments    = arguments,
    isMethodCall = isMethodCall and true,
  }
end

function Parser:consumeMethodCall(currentExpression)
  self:consumeToken("Colon") -- Expect and consume the colon (':').
  local methodName = self:consumeIdentifier()

  -- Convert the `table:method` part to an AST node.
  local methodIndexNode = {
    kind = "IndexExpression",
    base = currentExpression,
    index = { kind = "StringLiteral",
      value = methodName
    },
  }

  -- Consume the function call and mark it as a method call.
  return self:consumeFunctionCall(methodIndexNode, true)
end

function Parser:consumeOptionalSemicolon()
  if self:checkTokenKind("Semicolon") then
    self:consume(1)
  end
end

--// Expression Parsers //--
function Parser:parsePrimaryExpression()
  local currentToken = self.currentToken
  if not currentToken then return end

  local tokenKind  = currentToken.kind
  local tokenValue = currentToken.value

  if tokenKind == "Number" then
    self:consume(1)
    return { kind = "NumericLiteral", value = tokenValue }
  elseif tokenKind == "String" then
    self:consume(1)
    return { kind = "StringLiteral", value = tokenValue }
  elseif tokenKind == "Vararg" then
    self:consume(1)
    return { kind = "VarargExpression" }
  elseif tokenKind == "Keyword" then
    -- Nil literal: `nil`.
    if tokenValue == "nil" then
      self:consume(1) -- Consume 'nil'.
      return { kind = "NilLiteral" }

    -- Boolean literals: `true` or `false`.
    elseif tokenValue == "true" or tokenValue == "false" then
      self:consume(1) -- Consume 'true' or 'false'.
      return { kind = "BooleanLiteral", value = (tokenValue == "true") }

    -- Anonymous function, e.g., `function(arg1) ... end`.
    elseif tokenValue == "function" then
      self:consume(1) -- Consume 'function'.
      local parameters, isVararg = self:consumeParameters()
      local body = self:parseCodeBlock(true, parameters)
      self:expectKeyword("end")
      return { kind = "FunctionExpression",
        body       = body,
        parameters = parameters,
        isVararg   = isVararg
      }
    end

  -- Variable, e.g., `MY_VAR`.
  elseif tokenKind == "Identifier" then
    return self:consumeVariable()

  -- Parenthesized expression, e.g., `(a + b)`.
  elseif tokenKind == "LeftParen" then
    self:consume(1) -- Consume '('
    local expression = self:consumeExpression()
    self:expectTokenKind("RightParen") -- Expect and consume ')'.
    if not expression then
      self:error("Expected an expression inside parentheses")
    end

    -- The "ParenthesizedExpression" node is NOT just for operator precedence.
    -- In Lua, parentheses also force a multi-return expression to adjust to
    -- a single value. e.g., `a, b = f()` is different from `a, b = (f())`.
    -- Therefore, we must keep this wrapper node in the AST.
    return { kind = "ParenthesizedExpression",
      expression = expression
    }

  -- Table constructor, e.g., `{ 42, "string", ["my"] = variable }`.
  elseif tokenKind == "LeftBrace" then
    return self:consumeTable()
  end

  -- If no primary expression pattern is matched, return nil.
  return nil
end

function Parser:parsePrefixExpression()
  local primaryExpression = self:parsePrimaryExpression() -- <primary>
  if not primaryExpression then return end

  -- <suffix>*
  while (true) do
    local currentToken = self.currentToken
    if not currentToken then break end

    local currentTokenKind = currentToken.kind

    -- Function call.
    if currentTokenKind == "LeftParen" then
      -- <expression>(<args>)
      primaryExpression = self:consumeFunctionCall(primaryExpression, false)

      -- NOTE: Lua's grammar can be ambiguous when a function call is
      -- followed by a parenthesized expression. A semicolon is often
      -- needed to prevent error. Our parser does not enforce this.

    -- Table access.
    elseif currentTokenKind == "Dot" or currentTokenKind == "LeftBracket" then
      -- <expression>.<identifier> | <expression>[<expr>]
      primaryExpression = self:consumeIndexExpression(primaryExpression)

    -- Method call.
    elseif currentTokenKind == "Colon" then
      -- <expression>:<identifier>(<args>)
      primaryExpression = self:consumeMethodCall(primaryExpression)

    -- Implicit function call.
    elseif currentTokenKind == "String" or currentTokenKind == "LeftBrace" then
      -- <expression><string> | <expression><table_constructor>
      -- In some edge cases, a user may call a function using only string,
      -- example: `print "Hello, World!"`. This is a valid Lua syntax.
      -- Let's handle both strings and tables here for that case.
      primaryExpression = self:consumeFunctionCall(primaryExpression, false)
    else
      break
    end
  end

  return primaryExpression
end

function Parser:parseUnaryOperator()
  -- <unary> ::= <unary operator> <unary> | <primary>
  if not self:isUnaryOperator() then
    return self:parsePrefixExpression()
  end

  -- <unary operator> <unary>
  local operator = self.currentToken
  self:consume(1) -- Consume the operator
  local expression = self:parseBinaryExpression(self.CONFIG.UNARY_PRECEDENCE)
  if not expression then self:error("Unexpected end") end

  return { kind = "UnaryOperator",
    operator = operator.value,
    operand  = expression
  }
end

function Parser:parseBinaryExpression(minPrecedence)
  -- <binary> ::= <unary> <binary_operator> <binary> | <unary>
  local expression = self:parseUnaryOperator() -- <unary>
  if not expression then return end

  -- [<binary_operator> <binary>]
  while true do
    local operatorToken = self.currentToken
    if not operatorToken then break end

    local precedence = self.CONFIG.PRECEDENCE[operatorToken.value]
    if not self:isBinaryOperator() or precedence[1] <= minPrecedence then
      break
    end

    -- The <binary operator> <binary> part itself
    local nextToken = self:consume(1) -- Advance to and consume the operator
    if not nextToken then self:error("Unexpected end") end

    local right = self:parseBinaryExpression(precedence[2])
    if not right then self:error("Unexpected end") end

    expression = { kind = "BinaryOperator",
      operator = operatorToken.value,
      left     = expression,
      right    = right
    }
  end

  return expression
end

function Parser:consumeExpression()
  return self:parseBinaryExpression(0)
end

function Parser:consumeExpressions()
  -- Make an expression list and consume the first expression.
  local expressionList = { self:consumeExpression() }
  if #expressionList == 0 then return { } end

  -- From now on, consume upcoming expressions only if
  -- they are separated by a comma.
  while self:isComma() do
    self:consume(1) -- Consume the comma (',').
    local expression = self:consumeExpression()
    table.insert(expressionList, expression)
  end

  return expressionList
end

--// STATEMENT PARSERS //--
function Parser:parseLocalStatement()
  -- local <ident_list> [= <expr_list>]
  -- local function <ident>(<params>) <body> end
  self:consumeToken("Keyword", "local")

  -- Check if it's a local function declaration.
  -- local function <ident>(<params>) <body> end
  if self:checkKeyword("function") then
    self:consumeToken("Keyword", "function")
    local functionName = self:consumeIdentifier()
    local parameters, isVararg = self:consumeParameters()
    self:declareLocalVariable(functionName)
    local body = self:parseCodeBlock(true, parameters)
    self:expectKeyword("end")

    -- Unlike non-local function declarations (which desugar to assignments),
    -- local function declarations require special handling to support recursion.
    -- Consider: `local function foo() return foo() end`
    -- If we naively translated this to `local foo = function() return foo() end`,
    -- the inner `foo` would reference the global `foo` instead of the local one.
    -- The correct desugaring is: `local foo; foo = function() return foo() end`,
    -- but since our parser returns a single AST node per statement, we represent
    -- local function declarations as a dedicated node type.
    return { kind = "LocalFunctionDeclaration",
      name = functionName,
      body = {
        kind       = "FunctionExpression",
        body       = body,
        parameters = parameters,
        isVararg   = isVararg
      }
    }
  else
    -- It's a regular local variable declaration.
    -- <ident_list> [= <expr_list>]
    local variables = self:consumeIdentifierList()
    local initializers = {}

    -- Check for optional expressions.
    if self:checkTokenKind("Equals") then
      self:consume(1)
      initializers = self:consumeExpressions()
    end

    self:declareLocalVariables(variables)
    return { kind = "LocalDeclarationStatement",
      variables    = variables,
      initializers = initializers
    }
  end
end

function Parser:parseWhileStatement()
  -- while <condition_expr> do <body> end
  self:consumeToken("Keyword", "while")
  local condition = self:consumeExpression()
  self:expectKeyword("do")
  local body = self:parseCodeBlock()
  self:expectKeyword("end")

  return { kind = "WhileStatement",
    condition = condition,
    body      = body
  }
end

function Parser:parseRepeatStatement()
  -- repeat <body> until <condition_expr>
  self:consumeToken("Keyword", "repeat")

  -- Note: There's a Lua edge case which allows local variables declared
  -- inside the `repeat ... until` block to be used in the `until` condition.
  -- Therefore, we enter the scope before parsing the code block,
  -- but we only exit the scope after parsing the condition.
  self:enterScope()
  local body = self:parseCodeBlockInCurrentScope()
  -- Don't exit the scope yet as its variables can still be used in the condition.
  self:consumeToken("Keyword", "until")
  local condition = self:consumeExpression()
  self:exitScope()

  return { kind = "RepeatStatement",
    body      = body,
    condition = condition
  }
end

function Parser:parseDoStatement()
  -- do <body> end
  self:consumeToken("Keyword", "do")
  local body = self:parseCodeBlock()
  self:expectKeyword("end")

  return { kind = "DoStatement",
    body = body
  }
end

function Parser:parseReturnStatement()
  -- return <expr_list>
  self:consumeToken("Keyword", "return")
  local expressions = self:consumeExpressions()

  return { kind = "ReturnStatement",
    expressions = expressions
  }
end

function Parser:parseBreakStatement()
  -- break
  self:consumeToken("Keyword", "break")
  return { kind = "BreakStatement" }
end

function Parser:parseIfStatement()
  -- if <condition_expr> then <body>
  --  [elseif <condition_expr> then <body>]*
  --  [else <body>]
  -- end

  -- if <condition_expr> then <body>
  self:consumeToken("Keyword", "if")
  local ifCondition = self:consumeExpression()
  self:consumeToken("Keyword", "then")
  local ifBody = self:parseCodeBlock()
  local clauses = {
    -- First clause: the initial "if"
    { kind = "IfClause",
      condition = ifCondition,
      body      = ifBody
    }
  }

  -- [elseif <condition_expr> then <body>]*
  while self:checkKeyword("elseif") do
    -- Consume the "elseif" token
    self:consumeToken("Keyword", "elseif")
    local condition = self:consumeExpression()
    self:expectKeyword("then")
    local body = self:parseCodeBlock()

    -- elseif <condition_expr> then <body>
    local ifClause = { kind = "IfClause",
      condition = condition,
      body      = body
    }
    table.insert(clauses, ifClause)
  end

  -- [else <body>]
  local elseClause
  if self:checkKeyword("else") then
     self:consumeToken("Keyword", "else")
    elseClause = self:parseCodeBlock()
  end
  self:expectKeyword("end")

  return { kind = "IfStatement",
    clauses    = clauses,
    elseClause = elseClause
  }
end

function Parser:parseForStatement()
  -- for <var> = <start_expr>, <limit_expr> [, <step_expr>] do ... end
  -- for <ident_list> in <expr_list> do ... end
  self:consumeToken("Keyword", "for")
  local variableName = self:consumeIdentifier()

  -- At this point, we must distinguish between a generic `for` loop
  -- (e.g., `for i, v in pairs(t)`) and a numeric `for` loop `(e.g., `for i = 1, 10`).
  -- The presence of a comma or the `in` keyword signals a generic loop.
  if self:checkTokenKind("Comma") or self:checkKeyword("in") then
    -- It's a generic 'for' loop.
    -- Even though the generic for loop allows infinite expressions after `in`,
    -- the Lua VM only uses the first three (the generator, state, and control).

    -- Parse the list of iterator variables.
    local iteratorVariables = { variableName }
    while self:checkTokenKind("Comma") do
      self:consumeToken("Comma")
      local newVariableName = self:consumeIdentifier()
      table.insert(iteratorVariables, newVariableName)
    end

    -- Parse the 'in' keyword and the iterator expressions.
    self:expectKeyword("in")
    local expressions = self:consumeExpressions()

    -- Parse the loop body.
    self:consumeToken("Keyword", "do")
    local body = self:parseCodeBlock(false, iteratorVariables)
    self:expectKeyword("end")

    return { kind = "ForGenericStatement",
      iterators   = iteratorVariables,
      expressions = expressions,
      body        = body
    }
  end

  -- It's a numeric 'for' loop.
  -- Parse the '=' and the loop expressions.
  self:consumeToken("Equals")
  local expressions = self:consumeExpressions()
  local startExpr = expressions[1]
  local limitExpr = expressions[2]
  local stepExpr  = expressions[3]
  if not startExpr or not limitExpr then
    self:error("Numeric 'for' loop requires at least a start and limit expression.")
  elseif #expressions > 3 then
    self:error("Numeric 'for' loop allows at most a start, limit, and optional step expression.")
  end

  -- Parse the loop body.
  self:consumeToken("Keyword", "do")
  local body = self:parseCodeBlock(false, { variableName })
  self:expectKeyword("end")

  return { kind = "ForNumericStatement",
    variable = variableName,
    start    = startExpr,
    limit    = limitExpr,
    step     = stepExpr,
    body     = body
  }
end

function Parser:parseFunctionDeclaration()
  -- function <name>[.<field>]*[:<method>](<params>) <body> end
  self:consumeToken("Keyword", "function")

  -- Parse the base function name (e.g., `myFunc` in `myFunc.new`).
  local expression = self:consumeVariable()

  -- This loop handles `.field` and `:method` parts.
  local isMethodDeclaration = false
  while self.currentToken do
    local isDot   = self:checkTokenKind("Dot")
    local isColon = self:checkTokenKind("Colon")

    if not (isDot or isColon) then
      break -- The name chain has ended.
    end

    self:consume(1) -- Consume the `.` or `:`.
    local fieldName = self:consumeIdentifier()
    expression = {
      kind  = "IndexExpression",
      base  = expression,
      index = { kind = "StringLiteral",
        value = fieldName
      }
    }

    if isColon then
      isMethodDeclaration = true
      break -- A method must be the last part of the name.
    end
  end

  -- Parse the parameter list.
  local parameters, isVararg = self:consumeParameters()
  if isMethodDeclaration then
    -- For method declarations, we implicitly add `self` as the first parameter.
    table.insert(parameters, 1, "self")
  end

  -- Parse the function body and construct the final AST node.
  local body = self:parseCodeBlock(true, parameters)
  self:expectKeyword("end")

  -- Non-local function declarations desugar to assignment statements.
  -- Behaviorally, `function f() end` is equivalent to `f = function() end`.
  -- This uniform representation simplifies code generation by eliminating
  -- the need for a separate FunctionDeclaration node handler.
  return {
    kind    = "AssignmentStatement",
    lvalues = { expression },
    expressions = { {
      kind       = "FunctionExpression",
      body       = body,
      parameters = parameters,
      isVararg   = isVararg
    } }
  }
end

function Parser:parseAssignment(lvalue)
  -- <lvalue_list> = <expr_list>

  -- An assignment statement looks like: lvalue, lvalue, ... = expr, expr, ...
  -- We've already parsed the first lvalue. Now we parse the rest of the list.
  local lvalues = { lvalue }
  while self:isComma() do
    self:consume(1) -- Consume the `,`

    -- Parse the next potential lvalue in the list.
    local nextLValue = self:parsePrefixExpression()

    -- Validate that what we parsed is actually a valid assignment target.
    if not self:isValidAssignmentLvalue(nextLValue) then
      self:error("Invalid assignment target: expected variable or table index, got " ..
                 tostring(nextLValue and nextLValue.kind or "nil"))
    end

    table.insert(lvalues, nextLValue)
  end

  -- Now that we have all the lvalues, we expect the `=` sign.
  self:expectTokenKind("Equals")
  local expressions = self:consumeExpressions()

  return {
    kind        = "AssignmentStatement",
    lvalues     = lvalues,
    expressions = expressions
  }
end

function Parser:parseFunctionCallOrAssignmentStatement()
  -- This function handles statements that begin with an expression.
  -- In Lua, these can only be variable assignments or function calls.
  local expression = self:parsePrefixExpression()

  if not expression then
    self:error(
      string.format(
        "Invalid statement: expected a variable or function call, but got %s",
        self:getTokenDescription(self.currentToken)
      )
    )
  end

  -- Check if it's a function call statement (e.g., `myFunc()`).
  --- @diagnostic disable-next-line: need-check-nil
  if expression.kind == "FunctionCall" then
    return { kind = "CallStatement",
      expression = expression
    }
  end

  -- Check if it's the start of an assignment (e.g., `myVar = ...`).
  -- Only Variable and TableIndex nodes can be lvalues.
  if self:isValidAssignmentLvalue(expression) then
    -- The expression is a valid lvalue, so proceed to parse the rest of the assignment.
    return self:parseAssignment(expression)
  end

  -- If we reach here, the expression is not a valid statement.
  -- For example, a line like `x + 1` is a valid expression but not a valid statement.
  --- @diagnostic disable-next-line: need-check-nil
  self:error("Invalid statement: syntax error near '" .. expression.kind ..
             "'. Only function calls and assignments can be statements.")
end

--// CODE BLOCK PARSERS //--

-- Parses the next statement in a code block.
-- This function acts as a dispatcher, determining which specific parsing
-- function to call based on the current token.
function Parser:getNextNode()
  local node
  if self:checkTokenKind("Keyword") then
    local keyword = self.currentToken.value

    -- First, check for keywords that terminate a block. If found, we stop
    -- parsing this block and let the parent parser handle the keyword.
    if self.CONFIG.TERMINATION_KEYWORDS[keyword] then
      return nil -- Signal to stop parsing the current block.
    end

    local handlerName = self.CONFIG.KEYWORD_HANDLERS[keyword]
    if not handlerName then
      self:error("Unsupported keyword used as a statement starter: " .. keyword)
    end

    node = self[handlerName](self) -- e.g., self:parseWhile().
  else
    -- If the statement doesn't start with a keyword, it must be a
    -- variable assignment or a function call.
    node = self:parseFunctionCallOrAssignmentStatement()
  end

  -- Statements can optionally be separated by a semicolon.
  -- In Lua 5.1, semicolons can't be standalone statements,
  -- so they always have to follow a statement.
  self:consumeOptionalSemicolon()

  return node
end

-- A special function needed for the `repeat-until` statement edge case.
-- Unlike in the :parseCodeBlock() method, we don't push scope here as
-- local variables defined inside `repeat-until` statement can still be
-- used in the `until`s expression.
function Parser:parseCodeBlockInCurrentScope()
  local node = { kind = "Block", statements = {} }
  local nodeList = node.statements

  while self.currentToken do
    -- Parse statements one by one until a terminator is found.
    local nextNode = self:getNextNode()
    if not nextNode then
      -- A terminating keyword was found.
      break
    end

    table.insert(nodeList, nextNode)
  end

  return node
end

function Parser:parseCodeBlock(isFunctionScope, codeBlockVariables)
  -- Each block gets its own variable scope.
  self:enterScope(isFunctionScope)
  if codeBlockVariables then
    -- Pre-declare variables for this scope.
    -- (e.g. function parameters, for-loop variables).
    self:declareLocalVariables(codeBlockVariables)
  end

  local blockNode = self:parseCodeBlockInCurrentScope()

  self:exitScope()
  return blockNode
end

--// MAIN //--
function Parser:parse()
  local blockNode = self:parseCodeBlock()

  return {
    kind = "Program",
    body = blockNode
  }
end

--[[
    ============================================================================
                                (づ｡◕‿‿◕｡)づ
                            THE CODE GENERATOR!!!
    ============================================================================

    Possibly the most complex part of the compiler, the Code Generator is
    responsible for converting the AST into Lua instructions, which are
    similar to assembly instructions, but they are much higher level,
    because they're being executed in the Lua VM (Virtual Machine),
    not on a physical CPU. The Code Generator will also be responsible
    for generating the function prototypes, which are used to store
    information about the function, such as the number of arguments,
    the number of local variables, and the number of upvalues.
--]]

--* CodeGenerator *--
local CodeGenerator = {}
CodeGenerator.__index = CodeGenerator

CodeGenerator.CONFIG = {
  -- Stack Limits.
  -- Lua 5.1 allows up to 255 registers per function. We reserve a few for
  -- internal safety, capping user registers slightly lower.
  MIN_STACK_SIZE = 2,
  MAX_REGISTERS  = 250,

  -- Maximum amount of elements for a single SETLIST instruction.
  -- If more elements are needed, multiple SETLIST instructions are emitted.
  SETLIST_MAX = 50,

  -- Node kinds that can return multiple values (multiret).
  -- These require special handling when they appear as the last element
  -- of a list (e.g., `return f()`).
  MULTIRET_NODES = createLookupTable({"FunctionCall", "VarargExpression"}),

  CONTROL_FLOW_OPERATOR_LOOKUP = createLookupTable({"and", "or"}),

  -- stylua: ignore
  UNARY_OPERATOR_LOOKUP = {
    ["-"]   = "UNM",
    ["#"]   = "LEN",
    ["not"] = "NOT"
  },
  -- stylua: ignore
  ARITHMETIC_OPERATOR_LOOKUP = {
    ["+"] = "ADD", ["-"] = "SUB",
    ["*"] = "MUL", ["/"] = "DIV",
    ["%"] = "MOD", ["^"] = "POW"
  },

  -- Format: [operator] = {opname, opposite}
  -- stylua: ignore
  COMPARISON_INSTRUCTION_LOOKUP = {
    ["=="] = {"EQ", 1}, ["~="] = {"EQ", 0},
    ["<"]  = {"LT", 1}, [">"]  = {"LT", 1},
    ["<="] = {"LE", 1}, [">="] = {"LE", 1}
  },

  -- stylua: ignore
  EXPRESSION_HANDLERS = {
    -- Literals --
    ["StringLiteral"]  = "processLiteral",
    ["NumericLiteral"] = "processLiteral",
    ["BooleanLiteral"] = "processLiteral",
    ["NilLiteral"]     = "processLiteral",

    -- Operators --
    ["BinaryOperator"] = "processBinaryOperator",
    ["UnaryOperator"]  = "processUnaryOperator",

    -- Other Expressions --
    ["FunctionCall"]            = "processFunctionCall",
    ["Variable"]                = "processVariable",
    ["TableConstructor"]        = "processTableConstructor",
    ["ParenthesizedExpression"] = "processParenthesizedExpression",
    ["FunctionExpression"]      = "processFunctionExpression",
    ["IndexExpression"]         = "processIndexExpression",
    ["VarargExpression"]        = "processVarargExpression",
  },

  -- stylua: ignore
  STATEMENT_HANDLERS = {
    ["CallStatement"]             = "processCallStatement",
    ["LocalDeclarationStatement"] = "processLocalDeclarationStatement",
    ["LocalFunctionDeclaration"]  = "processLocalFunctionDeclaration",
    ["AssignmentStatement"]       = "processAssignmentStatement",
    ["WhileStatement"]            = "processWhileStatement",
    ["RepeatStatement"]           = "processRepeatStatement",
    ["DoStatement"]               = "processDoStatement",
    ["ReturnStatement"]           = "processReturnStatement",
    ["BreakStatement"]            = "processBreakStatement",
    ["IfStatement"]               = "processIfStatement",
    ["ForGenericStatement"]       = "processForGenericStatement",
    ["ForNumericStatement"]       = "processForNumericStatement",
  },
}

function CodeGenerator.new(ast)
  --// Asserting //
  assert(type(ast) == "table", "Expected 'ast' argument to be a table, got: " .. type(ast))
  assert(ast.kind == "Program", "Expected 'ast' to be a 'Program' node, got: " .. ast.kind)

  --// Initialization //
  local self = setmetatable({}, CodeGenerator)
  self.ast   = ast
  self.proto = nil

  -- Scope-related fields declaration
  self.scopes       = {}
  self.currentScope = nil
  self.stackSize    = 0

  self.breakJumpPCs = nil

  return self
end

--// Scope Management //--
function CodeGenerator:enterScope(isFunctionScope)
  local previousScope = self.currentScope
  local newScope = {
    locals      = {},
    parentScope = previousScope,
    isFunction  = (isFunctionScope and true) or false,
    needClose   = false,
  }

  if previousScope then
    previousScope.stackSize = self.stackSize
  end
  table.insert(self.scopes, newScope)

  self.currentScope = newScope
  if isFunctionScope then
    -- Function scopes start with an empty stack.
    self.stackSize = 0
  end

  return newScope
end

function CodeGenerator:leaveScope()
  local currentScope = self.currentScope
  local scopes       = self.scopes
  if not currentScope then
    error("CodeGenerator: No scope to leave!")

  -- Check for register leaks when leaving a function scope.
  elseif currentScope.isFunction and self.stackSize ~= 0 then
    -- Since all permanent registers are local variables/arguments, we can
    -- count how many local variables are declared in this scope to determine
    -- how many registers should be in use at most.

    -- Arguments are treated as local variables.
    local variableCount = 0
    for _ in pairs(currentScope.locals) do
      variableCount = variableCount + 1
    end

    if self.stackSize > variableCount then
      error(
        string.format(
          "CodeGenerator: Register leak detected when leaving function scope! "
          .. "Expected at most %d registers in use, but found %d.",
          variableCount,
          self.stackSize
        )
      )
    end
  end

  -- Remove the current scope from the stack.
  table.remove(scopes)

  local needClose = currentScope.needClose and not currentScope.isFunction
  currentScope = currentScope.parentScope
  self.currentScope = currentScope
  self.stackSize = (currentScope and currentScope.stackSize) or 0

  if needClose then
    if self.breakJumpPCs then
      self.breakJumpPCs.needClose = true
    end

    -- OP_CLOSE [A]    close all variables in the stack up to (>=) R(A)
    self:emitInstruction("CLOSE", self.stackSize, 0, 0)
  end
end

--// Variable Management //--
function CodeGenerator:declareLocalVariable(varName, register)
  register = register or self:allocateRegisters(1)
  self.currentScope.locals[varName] = register
  return register
end

function CodeGenerator:declareLocalVariables(varNames)
  for _, varName in ipairs(varNames) do
    self:declareLocalVariable(varName)
  end
end

function CodeGenerator:undeclareVariable(varName)
  self.currentScope.locals[varName] = nil
end

function CodeGenerator:undeclareVariables(varNames)
  for _, varName in ipairs(varNames) do
    self:undeclareVariable(varName)
  end
end

-- Finds the variable register index in current function scope.
function CodeGenerator:findVariableRegister(varName)
  local scope = self.currentScope
  while scope do
    local register = scope.locals[varName]
    if register then
      return register
    elseif scope.isFunction then
      break
    end
    scope = scope.parentScope
  end

  error("CodeGenerator: Could not find variable '" .. varName .. "' in any scope.")
end

-- Determines whether a variable is local, upvalue, or global.
function CodeGenerator:getUpvalueType(variableName)
  local scope = self.currentScope
  local isUpvalue = false
  while scope do
    if scope.locals[variableName] then
      scope.needClose = true
      return (isUpvalue and "Upvalue") or "Local"
    elseif scope.isFunction then
      isUpvalue = true
    end
    scope = scope.parentScope
  end

  return "Global"
end

--// Prototype Management //--
function CodeGenerator:emitPrototype(properties)
  local proto = {
    code         = {},
    constants    = {},
    upvalues     = {},
    protos       = {},
    numParams    = properties.numParams    or 0,
    maxStackSize = properties.maxStackSize or self.CONFIG.MIN_STACK_SIZE,
    isVararg     = properties.isVararg     or false,
    functionName = properties.functionName or "@tlc",

    -- Internal lookups for quick access.
    constantLookup = {}, -- format: {[constant] = index in proto.constants}
    upvalueLookup  = {}, -- format: {[varName]  = index in proto.upvalues}
  }

  return proto
end

--// Register/Constant/Upvalue Management //--

-- Allocates `count` registers and returns the index of the last allocated register.
function CodeGenerator:allocateRegisters(count)
  if count <= 0 then return end -- Just ignore non-positive counts.

  local previousStackSize = self.stackSize
  local updatedStackSize  = previousStackSize + count
  if updatedStackSize > self.CONFIG.MAX_REGISTERS then
    error(
      string.format(
        "CodeGenerator: Register overflow! exceeded maximum of %d registers.",
        self.CONFIG.MAX_REGISTERS
      )
    )
  end

  self.stackSize = updatedStackSize
  if updatedStackSize >= self.proto.maxStackSize then
    self.proto.maxStackSize = updatedStackSize
  end

  return updatedStackSize - 1
end

function CodeGenerator:freeRegisters(count)
  if count <= 0 then return end -- Just ignore non-positive counts.

  local stackSize = self.stackSize - count
  if stackSize < 0 then
    error("CodeGenerator: Stack underflow! cannot free below minimum of 0 registers.")
  end

  self.stackSize = stackSize
end

-- A helper function for freeing RK operands.
function CodeGenerator:freeIfRegister(rkOperand)
  -- Is it a register?
  if rkOperand >= 0 then
    self:freeRegisters(1)
  end

  -- If it's below 0, it's a constant index, do nothing.
end

-- Note:
-- Constant indices are returned as negative numbers (e.g., -1 for the first
-- constant) to distinguish them from register indices, which are non-negative.
-- This mirrors how Lua 5.1's VM encodes RK (Register or Konstant) operands in
-- bytecode: registers use values 0-255, while constants are encoded as 256
-- plus the absolute constant index. By using negative indices internally,
-- TLC simplifies operand handling without losing compatibility.
function CodeGenerator:findOrCreateConstant(value)
  local constantLookup = self.proto.constantLookup
  local constants      = self.proto.constants
  local constantIndex  = constantLookup[value]
  -- Hot path: Constant already exists, return its index.
  if constantIndex then
    return constantIndex
  end

  -- Cold path: New constant, add it to the table.
  table.insert(constants, value)
  constantIndex = -#constants -- Constant indices are negative-based.
  constantLookup[value] = constantIndex
  return constantIndex
end

function CodeGenerator:findOrCreateUpvalue(varName)
  local upvalueLookup = self.proto.upvalueLookup
  local upvalues      = self.proto.upvalues
  local upvalueIndex  = upvalueLookup[varName]
  -- Hot path: Upvalue already exists, return its index.
  if upvalueIndex then
    return upvalueIndex
  end

  -- Cold path: New upvalue, add it to the table.
  table.insert(upvalues, varName)

  -- Lua upvalue indices are 0-based, so we subtract 1.
  upvalueIndex = #upvalues - 1
  upvalueLookup[varName] = upvalueIndex
  return upvalueIndex
end

-- Used for RK() operands (Register or Konstant).
-- If it's a register it has to get free'd later.
function CodeGenerator:processConstantOrExpression(node, register)
  local nodeKind = node.kind

  -- Is it a constant?
  if nodeKind == "NumericLiteral" or nodeKind == "StringLiteral" then
    local constantIndex = self:findOrCreateConstant(node.value)

    -- Check if it can fit in 9-bit signed RK operand.
    if -constantIndex < 255 then
      -- Returns a negative index to indicate it's a constant.
      return constantIndex
    end
  end

  -- Otherwise, process it as an expression
  return self:processExpressionNode(node, register)
end

--// Instruction Management //--

-- TODO: Should we switch to different instruction representations?
-- E.g. `{opcode = 0, opname = "MOVE", a = 0, b = 1, c = 0}`?
function CodeGenerator:emitInstruction(opname, a, b, c)
  local instruction = { opname, a, b, c or 0 }
  table.insert(self.proto.code, instruction)

  return instruction
end

function CodeGenerator:emitJump(offset)
  -- OP_JMP [A, sBx]    pc+=sBx
  self:emitInstruction("JMP", 0, offset or 1, 0)

  local instructionIndex = #self.proto.code
  return instructionIndex
end

function CodeGenerator:emitJumpBack(toPC)
  local currentPC = #self.proto.code + 1
  local offset    = toPC - currentPC

  -- OP_JMP [A, sBx]    pc+=sBx
  self:emitJump(offset)
end

function CodeGenerator:patchJump(fromPC, toPC)
  local instruction = self.proto.code[fromPC]
  if not instruction then
    error(
      "CodeGenerator: Invalid 'fromPC' for jump patching: "
      .. tostring(fromPC)
    )
  end

  local offset = toPC - (fromPC + 1)
  instruction[3] = offset
end

function CodeGenerator:patchJumpToHere(fromPC)
  local currentPC = #self.proto.code + 1
  self:patchJump(fromPC, currentPC)
end

function CodeGenerator:patchJumpsToHere(jumpPCs)
  for _, pc in ipairs(jumpPCs) do
    self:patchJumpToHere(pc)
  end
end

function CodeGenerator:patchBreakJumpsToHere()
  if not self.breakJumpPCs then return end
  self:patchJumpsToHere(self.breakJumpPCs)
  if #self.breakJumpPCs > 0 and self.breakJumpPCs.needClose then
    -- OP_CLOSE [A]    close all variables in the stack up to (>=) R(A)
    self:emitInstruction("CLOSE", self.stackSize, 0, 0)
  end
  self.breakJumpPCs = nil
end

--// Wrappers //--
function CodeGenerator:breakable(callback)
  local previousBreakJumpPCs = self.breakJumpPCs
  self.breakJumpPCs = {}

  callback()
  self:patchBreakJumpsToHere()

  self.breakJumpPCs = previousBreakJumpPCs
end

--// Auxiliary/Helper Methods //--

-- Determines whether a node can return values up to the top of the stack.
function CodeGenerator:isMultiReturnNode(node)
  return node and self.CONFIG.MULTIRET_NODES[node.kind]
end

-- Determines whether the last expression in a list is a multiret.
function CodeGenerator:isMultiReturnList(expressions)
  if #expressions == 0 then return false end

  local lastExpression = expressions[#expressions]
  return self:isMultiReturnNode(lastExpression)
end

-- Used to check if we can optimize a `return <func_call>` into a tail call.
function CodeGenerator:isSingleFunctionCall(expressions)
  if #expressions ~= 1 then return false end

  local expression = expressions[1]
  return expression.kind == "FunctionCall"
end

-- Splits table's elements into implicit and explicit ones.
function CodeGenerator:splitTableElements(elements)
  local implicitElems = {}
  local explicitElems = {}
  for _, element in ipairs(elements) do
    if element.isImplicitKey then
      table.insert(implicitElems, element)
    else
      table.insert(explicitElems, element)
    end
  end

  return implicitElems, explicitElems
end

-- Sets the value of the list of variables or table indices from a range of registers.
function CodeGenerator:assignValuesToRegisters(nodes, index, copyFromRegister)
  if index > #nodes then return end
  local node     = nodes[index]
  local nodeKind = node.kind

  if nodeKind == "Variable" then
    -- First give the other index assignments a chance
    -- to read the base and index before it might change.
    self:assignValuesToRegisters(nodes, index + 1, copyFromRegister + 1)

    local variableType = node.variableType
    local variableName = node.name
    if variableType == "Local" then
      local variableRegister = self:findVariableRegister(variableName)
      -- OP_MOVE [A, B]    R(A) := R(B)
      self:emitInstruction("MOVE", variableRegister, copyFromRegister)
    elseif variableType == "Upvalue" then
      local upvalueIndex = self:findOrCreateUpvalue(variableName)
      -- OP_SETUPVAL [A, B]    UpValue[B] := R(A)
      self:emitInstruction("SETUPVAL", copyFromRegister, upvalueIndex)
    elseif variableType == "Global" then
      local constantIndex = self:findOrCreateConstant(variableName)
      -- OP_SETGLOBAL [A, Bx]    Gbl[Kst(Bx)] := R(A)
      self:emitInstruction("SETGLOBAL", copyFromRegister, constantIndex)
    end
    return
  elseif nodeKind == "IndexExpression" then
    local baseNode  = node.base
    local indexNode = node.index

    local baseRegister  = self:processConstantOrExpression(baseNode)
    local indexRegister = self:processConstantOrExpression(indexNode)

    -- This index assignment did read its base and index.
    -- Now give other index assignments a chance before doing the assignment.
    self:assignValuesToRegisters(nodes, index + 1, copyFromRegister + 1)

    -- OP_SETTABLE [A, B, C]    R(A)[RK(B)] := RK(C)
    self:emitInstruction("SETTABLE", baseRegister, indexRegister, copyFromRegister)
    self:freeIfRegister(baseRegister)
    self:freeIfRegister(indexRegister)

    return
  end

  error("CodeGenerator: Unsupported lvalue kind in assignValuesToRegisters: " .. nodeKind)
end

-- Processes a single page (50 or less) of implicit table elements.
function CodeGenerator:processTablePage(
  register,
  implicitElems,
  page,
  totalPages,
  isLastElemImplicit
)
  local batchSize    = self.CONFIG.SETLIST_MAX
  local startIndex   = (page - 1) * batchSize + 1
  local endIndex     = math.min(page * batchSize, #implicitElems)
  local elementCount = endIndex - startIndex + 1

  local isLastPage  = (page == totalPages)
  local lastElement = implicitElems[#implicitElems]

  -- Determine if the last element of this batch is the absolute last element
  -- of the table constructor AND if it should be treated as a multiret.
  local isMultiret = isLastPage
                  and isLastElemImplicit
                  and self:isMultiReturnNode(lastElement.value)

  for i = startIndex, endIndex do
    local isLastInBatch = (i == endIndex)
    -- Only the absolute last element gets -1 (multiret), others get 1.
    local returnCount = (isLastInBatch and isMultiret and -1)

    self:processExpressionNode(implicitElems[i].value, nil, returnCount)
  end

  -- B = 0 means "take all values from stack top" (used for multiret).
  -- Otherwise, B is the number of elements to set.
  local setListCount = (isMultiret and 0) or elementCount

  -- OP_SETLIST [A, B, C]    R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B
  self:emitInstruction("SETLIST", register, setListCount, page)
  self:freeRegisters(elementCount)
end

--// Expression Handlers //--

-- Generic processor for both string and numeric literals.
function CodeGenerator:processLiteral(node, register)
  local nodeKind = node.kind
  if nodeKind == "NilLiteral" then

    -- OP_LOADNIL [A, B]    R(A) := ... := R(B) := nil
    self:emitInstruction("LOADNIL", register, register, 0)
  elseif nodeKind == "BooleanLiteral" then
    local value = (node.value and 1) or 0

    -- OP_LOADBOOL [A, B, C]    R(A) := (Bool)B; if (C) pc++
    self:emitInstruction("LOADBOOL", register, value, 0)
  elseif nodeKind == "NumericLiteral" or nodeKind == "StringLiteral" then
    local value         = node.value
    local constantIndex = self:findOrCreateConstant(value)

    -- OP_LOADK [A, Bx]    R(A) = Kst(Bx)
    self:emitInstruction("LOADK", register, constantIndex, 0)
  else
    error("CodeGenerator: Unsupported literal type: " .. tostring(nodeKind))
  end

  return register
end

function CodeGenerator:processParenthesizedExpression(node, register)
  local expression = node.expression
  return self:processExpressionNode(expression, register)
end

function CodeGenerator:processBinaryOperator(node, register)
  local operator = node.operator
  local left     = node.left
  local right    = node.right

  -- Simple arithmetic operators (+, -, /, *, %, ^)
  if self.CONFIG.ARITHMETIC_OPERATOR_LOOKUP[operator] then
    local opcode = self.CONFIG.ARITHMETIC_OPERATOR_LOOKUP[operator]

    local rightOperand = self:processConstantOrExpression(left, register)
    local leftOperand  = self:processConstantOrExpression(right)
    self:emitInstruction(opcode, register, rightOperand, leftOperand)

    -- Don't deallocate `rightOperand` as it is `register`.
    self:freeIfRegister(leftOperand)

    return register

  -- Control flow operators (and, or)
  elseif self.CONFIG.CONTROL_FLOW_OPERATOR_LOOKUP[operator] then
    self:processExpressionNode(left, register)
    local isConditionTrue = (operator == "and" and 0) or 1
    -- OP_TEST [A, C]    if not (R(A) <=> C) then pc++
    self:emitInstruction("TEST", register, 0, isConditionTrue)
    local jumpPC = self:emitJump()
    self:processExpressionNode(right, register)
    self:patchJumpToHere(jumpPC)

    return register

  -- Comparison operators (==, ~=, <, >, <=, >=)
  elseif self.CONFIG.COMPARISON_INSTRUCTION_LOOKUP[operator] then
    local leftReg      = self:processConstantOrExpression(left, register)
    local rightReg     = self:processConstantOrExpression(right)
    local nodeOperator = self.CONFIG.COMPARISON_INSTRUCTION_LOOKUP[operator]
    local instruction, opposite = nodeOperator[1], nodeOperator[2]

    local flip = (operator == ">" or operator == ">=")
    local b, c = leftReg, rightReg
    if flip then b, c = rightReg, leftReg end

    self:emitInstruction(instruction, opposite, b, c)
    self:emitJump(1) -- Skip next instruction if comparison is false.

    -- OP_LOADBOOL [A, B, C]    R(A) := (Bool)B; if (C) pc++
    self:emitInstruction("LOADBOOL", register, 0, 1)
    self:emitInstruction("LOADBOOL", register, 1, 0)

    -- Don't deallocate `leftReg` as it is `register`.
    self:freeIfRegister(rightReg)

    return register

  -- String concatenation (..)
  elseif operator == ".." then
    local bottomRegister = self:processExpressionNode(left)

    -- Optimization: Flatten consecutive concatenations.
    -- That will help us reduce the number of CONCAT instructions.
    -- For example, the expression:
    --  "Hello, " .. "world" .. "!"
    -- Will be compiled into:
    --  LOADK 0, -1     ; "Hello, "
    --  LOADK 1, -2     ; "world"
    --  LOADK 2, -3     ; "!"
    --  CONCAT 3, 0, 2  ; R(3) = R(0) .. R(1) .. R(2)
    local curRight = right
    while curRight.operator == ".." do
      -- The expression is guaranteed to take only one register.
      self:processExpressionNode(curRight.left)
      curRight = curRight.right -- Advance to the next right node.
    end

    local topRegister = self:processExpressionNode(curRight)

    -- OP_CONCAT [A, B, C]    R(A) := R(B).. ... ..R(C)
    self:emitInstruction("CONCAT", register, bottomRegister, topRegister)
    self:freeRegisters(topRegister - bottomRegister + 1)

    return register
  end

  error("CodeGenerator: Unsupported binary operator: " .. operator)
end

function CodeGenerator:processUnaryOperator(node, register)
  local operator = node.operator
  local operand  = node.operand
  local opname   = self.CONFIG.UNARY_OPERATOR_LOOKUP[operator]
  if not opname then
    error("CodeGenerator: Unsupported unary operator: " .. operator)
  end

  self:processExpressionNode(operand, register)
  self:emitInstruction(opname, register, register)

  return register
end

function CodeGenerator:processTableConstructor(node, register)
  local elements    = node.elements
  local lastElement = node.elements[#node.elements]
  local isLastElementImplicit = lastElement and lastElement.isImplicitKey
  local implicitElems, explicitElems = self:splitTableElements(elements)

  -- TODO: We're doing wrong calculation, fix it later.
  -- https://www.lua.org/source/5.1/lparser.c.html#constructor
  -- https://www.lua.org/source/5.1/lobject.c.html#luaO_int2fb
  local sizeB = math.min(#implicitElems, 100)
  local sizeC = math.min(#explicitElems, 100)

  -- OP_NEWTABLE [A, B, C]    R(A) := {} (size = B,C)
  self:emitInstruction("NEWTABLE", register, sizeB, sizeC)

  for _, elem in ipairs(explicitElems) do
    local keyRegister   = self:processExpressionNode(elem.key)
    local valueRegister = self:processExpressionNode(elem.value)

    -- OP_SETTABLE [A, B, C]    R(A)[RK(B)] := RK(C)
    self:emitInstruction("SETTABLE", register, keyRegister, valueRegister)
    self:freeRegisters(2)
  end

  local totalPages = math.ceil(#implicitElems / self.CONFIG.SETLIST_MAX)

  -- Table page limit in SETLIST instructions.
  -- The C operand is 9 bits, limiting it to 511 (1-511 valid range).
  -- This caps table constructors at 511 * SETLIST_MAX elements (25550 with SETLIST_MAX=50).
  -- Official Lua 5.1 handles overflows by setting C=0 and encoding the page count
  -- in the next pseudo-instruction. TLC errors out instead for simplicity.
  if totalPages >= 512 then
    error("CodeGenerator: Table constructor page overflow! Maximum of 511 pages allowed.")
  end

  for page = 1, totalPages do
    self:processTablePage(
      register,
      implicitElems,
      page,
      totalPages,
      isLastElementImplicit
    )
  end

  return register
end

function CodeGenerator:processVariable(node, register)
  local varName = node.name
  local varType = node.variableType -- "Local" / "Upvalue" / "Global"

  if varType == "Local" then
    -- Local variables are stored in the closure's stack (registers).
    local variable = self:findVariableRegister(varName)

    -- OP_MOVE [A, B]    R(A) := R(B)
    self:emitInstruction("MOVE", register, variable)
  elseif varType == "Global" then
    -- Globals are stored in the global environment table.
    local constantIndex = self:findOrCreateConstant(varName)

    -- OP_GETGLOBAL [A, Bx]    R(A) := Gbl[Kst(Bx)]
    self:emitInstruction("GETGLOBAL", register, constantIndex)
  elseif varType == "Upvalue" then
    -- Upvalues are stored in the function's upvalue list.
    local upvalueIndex = self:findOrCreateUpvalue(varName)

    -- OP_GETUPVAL [A, B]    R(A) := UpValue[B]
    self:emitInstruction("GETUPVAL", register, upvalueIndex)
  end

  return register
end

function CodeGenerator:processFunctionCall(node, register, resultRegisters)
  local callee       = node.callee
  local arguments    = node.arguments
  local isMethodCall = node.isMethodCall

  -- Method calls (e.g., `obj:method()`) need special handling.
  -- We need to load the `self` parameter (the table) into the
  -- register before the function call, so we use the `SELF` instruction.
  if isMethodCall then
    local calleeExpressionIndex = callee.index
    local calleeExpressionBase  = callee.base

    self:processExpressionNode(calleeExpressionBase, register)
    self:allocateRegisters(1) -- Used for implicit `self` argument.

    local calleeIndexRegister = self:processConstantOrExpression(calleeExpressionIndex)

    -- OP_SELF [A, B, C]    R(A+1) := R(B); R(A) := R(B)[RK(C)]
    self:emitInstruction("SELF", register, register, calleeIndexRegister)

    self:freeIfRegister(calleeIndexRegister)
  else
    self:processExpressionNode(callee, register)
  end

  local argumentRegisterCount, numArgs = self:processExpressionList(arguments)
  local isArgsMultiRet = self:isMultiReturnList(arguments)
  if isMethodCall then
    -- Make sure the `CALL` instruction captures the "self" (table) param.
    argumentRegisterCount = argumentRegisterCount + 1
    if not isArgsMultiRet then
      -- Check if it's not multiret, as multiret already includes all args.
      -- Increasing it by one would turn multiret (-1 + 1 = 0) into a fixed count (0 + 1 = 1)
      -- which will make it capture only one argument instead of all of them.
      numArgs = numArgs + 1
    end
  end

  local returnValueCount = (resultRegisters and resultRegisters + 1) or 2
  local resultCount      = (register + returnValueCount) - 2

  -- OP_CALL [A, B, C]    R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
  self:emitInstruction("CALL", register, numArgs + 1, returnValueCount)
  self:freeRegisters(argumentRegisterCount)
  self:allocateRegisters(returnValueCount - 2) -- One register is already in 'register'

  return register, resultCount
end

function CodeGenerator:processFunctionExpression(node, register)
  self:processFunction(node, register)
  return register
end

function CodeGenerator:processIndexExpression(node, register)
  local baseNode  = node.base
  local indexNode = node.index

  local baseRegister  = self:processExpressionNode(baseNode, register)
  local indexRegister = self:processConstantOrExpression(indexNode)

  -- OP_GETTABLE [A, B, C]    R(A) := R(B)[RK(C)]
  self:emitInstruction("GETTABLE", baseRegister, baseRegister, indexRegister)
  self:freeIfRegister(indexRegister)

  return baseRegister
end

function CodeGenerator:processVarargExpression(_, register, resultRegisters)
  local varargCount = (resultRegisters or 1) + 1

  -- OP_VARARG [A, B]    R(A), R(A+1), ..., R(A+B-1) = vararg
  self:emitInstruction("VARARG", register, varargCount)
  self:allocateRegisters(varargCount - 2) -- One register is already in 'register'
  return register
end

--// Statement Handlers //--
function CodeGenerator:processCallStatement(node)
  self:processFunctionCall(node.expression, self:allocateRegisters(1), 0)
  -- We shouldn't have allocated registers from statements.
  -- (Unless it's a statement that declares local variables.)
  self:freeRegisters(1)
end

function CodeGenerator:processDoStatement(node)
  self:processBlockNode(node.body)
end

function CodeGenerator:processBreakStatement(_)
  if not self.breakJumpPCs then
    error("CodeGenerator: no loop to break")
  end

  table.insert(self.breakJumpPCs, self:emitJump())
end

function CodeGenerator:processLocalDeclarationStatement(node)
  local variables    = node.variables
  local initializers = node.initializers

  local variableBaseRegister = self.stackSize - 1
  self:processExpressionList(initializers, #variables)

  for index, variableName in ipairs(variables) do
    local expressionRegister = variableBaseRegister + index
    self:declareLocalVariable(variableName, expressionRegister)
  end
end

function CodeGenerator:processLocalFunctionDeclaration(node)
  local functionName = node.name
  local body         = node.body

  local variableRegister = self:declareLocalVariable(functionName)
  self:processFunction(body, variableRegister)
end

function CodeGenerator:processAssignmentStatement(node)
  local lvalues     = node.lvalues
  local expressions = node.expressions

  local variableBaseRegister = self.stackSize
  local lvalueRegisterCount  = self:processExpressionList(expressions, #lvalues)

  self:assignValuesToRegisters(lvalues, 1, variableBaseRegister)

  self:freeRegisters(lvalueRegisterCount)
end

function CodeGenerator:processIfStatement(node)
  local clauses    = node.clauses
  local elseClause = node.elseClause

  local jumpToEndPCs = {}
  local lastClause   = clauses[#clauses]

  -- Process all 'if' and 'elseif' clauses first.
  for _, clause in ipairs(clauses) do
    local condition = clause.condition
    local body      = clause.body

    local conditionRegister = self:processExpressionNode(condition)

    -- OP_TEST [A, C]    if not (R(A) <=> C) then pc++
    -- If the condition is false, jump to the next instruction.
    -- (which is always a jump to the next clause)
    self:emitInstruction("TEST", conditionRegister, 0, 0)
    self:freeRegisters(1) -- Free conditionRegister

    -- This is the jump that occurs if the condition is false.
    -- We will patch it later to jump to the next clause.
    local conditionIsFalseJumpPC = self:emitJump()
    self:processBlockNode(body)

    local isLastClause    = (clause == lastClause)
    local shouldJumpToEnd = not isLastClause or elseClause
    if shouldJumpToEnd then
      local jumpToEndPC = self:emitJump()
      table.insert(jumpToEndPCs, jumpToEndPC)
    end

    -- Patch the condition false jump to here.
    self:patchJumpToHere(conditionIsFalseJumpPC)
  end

  -- Is there an 'else' clause to process?
  -- NOTE: The previous clause's condition already jumps
  -- to here, so we don't need to patch anything.
  if elseClause then
    self:processBlockNode(elseClause)
  end

  -- Patch all jumps at the end of clauses to here.
  self:patchJumpsToHere(jumpToEndPCs)
end

function CodeGenerator:processForGenericStatement(node)
  local iterators   = node.iterators
  local expressions = node.expressions
  local body        = node.body

  self:enterScope()
  local baseRegister = self.stackSize
  self:processExpressionList(expressions, 3)
  self:declareLocalVariables(iterators)

  local startJmpInstruction = self:emitJump()
  local loopStartPC = #self.proto.code
  self:breakable(function()
    self:processBlockNode(body)
    self:patchJumpToHere(startJmpInstruction)

    -- OP_TFORLOOP [A, C]    R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2))
    --                       if R(A+3) ~= nil then R(A+2)=R(A+3) else pc++
    -- Skips next instruction if there are no more values.
    self:emitInstruction("TFORLOOP", baseRegister, 0, #iterators)

    -- Emit jump back to the start of the loop if there are more values.
    self:emitJumpBack(loopStartPC)
  end)
  self:leaveScope()
end

function CodeGenerator:processForNumericStatement(node)
  local varName   = node.variable
  local startExpr = node.start
  local limitExpr = node.limit
  local stepExpr  = node.step
  local body      = node.body

  self:enterScope()
  local startRegister = self:processExpressionNode(startExpr)
  self:processExpressionNode(limitExpr)
  local stepRegister = self:allocateRegisters(1)

  -- Is there a step expression?
  if stepExpr then
    self:processExpressionNode(stepExpr, stepRegister)
  else
    -- OP_LOADK [A, Bx]    R(A) := Kst(Bx)
    -- Default step is 1.
    self:emitInstruction("LOADK", stepRegister, self:findOrCreateConstant(1))
  end

  -- OP_FORPREP [A, sBx]    R(A)-=R(A+2) pc+=sBx
  local forPrepInstruction = self:emitInstruction("FORPREP", startRegister, 0)
  local loopStartPC = #self.proto.code

  -- Declare the loop variable.
  self:declareLocalVariable(varName, startRegister)
  self:breakable(function()
    self:processBlockNode(body)

    local loopEndPC = #self.proto.code
    forPrepInstruction[3] = loopEndPC - loopStartPC

    -- OP_FORLOOP [A, sBx]   R(A)+=R(A+2)
    --                       if R(A) <?= R(A+1) then { pc+=sBx R(A+3)=R(A) }
    self:emitInstruction("FORLOOP", startRegister, loopStartPC - loopEndPC - 1)
  end)
  self:leaveScope()
end

function CodeGenerator:processWhileStatement(node)
  local condition = node.condition
  local body      = node.body

  local startPC = #self.proto.code
  local conditionRegister = self:processExpressionNode(condition)

  -- OP_TEST [A, C]    if not (R(A) <=> C) then pc++
  self:emitInstruction("TEST", conditionRegister, 0, 0)
  self:freeRegisters(1) -- The condition register is not needed anymore.

  local endJumpPC = self:emitJump()
  self:breakable(function()
    self:processBlockNode(body)
    self:emitJumpBack(startPC)
  end)
  self:patchJumpToHere(endJumpPC)
end

function CodeGenerator:processRepeatStatement(node)
  local body        = node.body
  local condition   = node.condition
  local loopStartPC = #self.proto.code

  -- NOTE: Repeat statements' body and condition are in the same scope.
  self:enterScope()
  self:breakable(function()
    self:processStatementList(body.statements)
    local conditionRegister = self:processExpressionNode(condition)

    if self.currentScope.needClose then
      -- OP_CLOSE [A]    close all variables in the stack up to (>=) R(A)
      self:emitInstruction("CLOSE", self.currentScope.parentScope.stackSize, 0, 0)
      self.currentScope.needClose = false
    end

    -- OP_TEST [A, C]    if not (R(A) <=> C) then pc++
    self:emitInstruction("TEST", conditionRegister, 0, 0)
    self:emitJumpBack(loopStartPC)
    self:freeRegisters(1) -- The condition register is not needed anymore.
  end)
  self:leaveScope()
end

function CodeGenerator:processReturnStatement(node)
  local expressions      = node.expressions
  local resultRegister   = self.stackSize
  local lastExpression   = expressions[#expressions]
  local isSingleFuncCall = self:isSingleFunctionCall(expressions)
  local numResults, exprRegisterCount

  -- Optimization:
  -- If it's a single function call, we can optimize it to a TAILCALL.
  -- Tail calls are more efficient as they reuse the current function's stack frame.
  if isSingleFuncCall then
    self:processFunctionCall(
      lastExpression,
      self:allocateRegisters(1), -- This register must get manually free'd later.
      -1
    )

    local callInstruction = self.proto.code[#self.proto.code]
    if not callInstruction or callInstruction[1] ~= "CALL" then
      error("CodeGenerator: Expected CALL instruction for tail call optimization.")
    end

    -- OP_TAILCALL [A, B, C]    return R(A)(R(A+1), ... ,R(A+B-1))
    callInstruction[1] = "TAILCALL" -- Change CALL to TAILCALL
    numResults         = -1         -- -1 = MULTIRET (all results)

    self:freeRegisters(1) -- Free the temp register used for the call.
  else
    exprRegisterCount, numResults = self:processExpressionList(expressions)
    self:freeRegisters(exprRegisterCount)
  end

  -- OP_RETURN [A, B]    return R(A), ... ,R(A+B-2)
  self:emitInstruction("RETURN", resultRegister, numResults + 1, 0)
end

--// Main Methods //--

-- Process an expression AST node and return the register(s) holding its value.
-- If 'register' is nil a new register is allocated. 'resultRegisters' requests
-- multiple results (used for calls/vararg). Dispatches to the handler named
-- in CONFIG.EXPRESSION_HANDLERS for the node.kind.
function CodeGenerator:processExpressionNode(node, register, resultRegisters)
  if not node then error("Node not found.") end
  register = register or self:allocateRegisters(1)

  local nodeKind = node.kind
  local handler  = self.CONFIG.EXPRESSION_HANDLERS[nodeKind]
  local func     = self[handler]
  if not func then
    error("CodeGenerator: Expression node kind not supported: " .. nodeKind)
  end

  return func(self, node, register, resultRegisters)
end

-- Process a statement AST node. Dispatches to the handler named
-- in CONFIG.STATEMENT_HANDLERS for the node.kind.
function CodeGenerator:processStatementNode(node)
  if not node then error("Node not found.") end

  local nodeKind = node.kind
  local handler  = self.CONFIG.STATEMENT_HANDLERS[nodeKind]
  local func     = self[handler]
  if not func then
    error("CodeGenerator: Statement node kind not supported: " .. nodeKind)
  end

  return func(self, node)
end

-- Processes a list of expressions, allocating registers and handling multi-return
-- cases. Returns the number of registers allocated and the result count (-1 for
-- multiret). If expectedRegisters is provided, it limits allocation and pads
-- with nil or discards excess results to match.
function CodeGenerator:processExpressionList(expressionList, expectedRegisters)
  local count = #expressionList
  if count == 0 and not expectedRegisters then return 0, 0 end

  local lastExpression = expressionList[count]
  local isLastMultiret = self:isMultiReturnNode(lastExpression)

  -- Process "fixed" expressions (all except the last if it's multiret).
  local fixedCount = (isLastMultiret and (count - 1)) or count

  for i = 1, fixedCount do
    self:processExpressionNode(expressionList[i])

    -- If we are restricted to a certain amount of registers,
    -- discard any extra registers after reaching the limit.
    if expectedRegisters and i > expectedRegisters then
      self:freeRegisters(1)
    end
  end

  -- Process the last expression (Special handling for multiret).
  if isLastMultiret then
    -- Calculate how many registers are remaining to be filled.
    -- If `expectedRegisters` is nil, we pass -1 to capture all results.
    local remaining = (expectedRegisters and (expectedRegisters - fixedCount)) or -1
    self:processExpressionNode(lastExpression, nil, remaining)

  -- Padding: If we have fewer expressions than expected, fill the rest with nils.
  elseif expectedRegisters and expectedRegisters > count then
    local needed   = expectedRegisters - count
    local startReg = self.stackSize
    local endReg   = self:allocateRegisters(needed)

    -- OP_LOADNIL [A, B]    R(A) := ... := R(B) := nil
    self:emitInstruction("LOADNIL", startReg, endReg, 0)
  end

  local allocatedCount = expectedRegisters or count
  local resultCount    = (isLastMultiret and -1) or allocatedCount

  return allocatedCount, resultCount
end

function CodeGenerator:processStatementList(statementList)
  for _, node in ipairs(statementList) do
    self:processStatementNode(node)
  end
end

function CodeGenerator:processBlockNode(blockNode)
  self:enterScope()
  self:processStatementList(blockNode.statements)
  self:leaveScope()
end

function CodeGenerator:processFunctionBody(node)
  local previousBreakJumpPCs = self.breakJumpPCs
  self.breakJumpPCs = nil
  self:enterScope(true)
  for _, paramName in ipairs(node.parameters) do
    self:declareLocalVariable(paramName)
  end

  if node.isVararg then
    -- Declare implicit "arg" variable to hold vararg values.
    self:declareLocalVariable("arg")
  end

  self:processStatementList(node.body.statements)
  -- Generate default return statement.
  -- OP_RETURN [A, B]    return R(A), ... ,R(A+B-2)
  self:emitInstruction("RETURN", 0, 1, 0)
  self:leaveScope()
  self.breakJumpPCs = previousBreakJumpPCs
end

-- Adds `proto` prototype to the `self.proto.protos` list and generates
-- the "CLOSURE" and upvalue capturing pseudo instructions.
function CodeGenerator:generateClosure(proto, closureRegister)
  local parentProto = self.proto
  local protoIndex  = #parentProto.protos + 1
  table.insert(parentProto.protos, proto)

  -- OP_CLOSURE [A, Bx]    R(A) := closure(KPROTO[Bx], R(A), ... ,R(A+n))
  -- Proto indices are zero-based.
  self:emitInstruction("CLOSURE", closureRegister, protoIndex - 1, 0)

  -- After the "CLOSURE" instruction, we need to set up the upvalues
  -- for the newly created function prototype. It is done by pseudo
  -- instructions that follow the CLOSURE instruction.
  for _, upvalueName in ipairs(proto.upvalues) do
    local upvalueType = self:getUpvalueType(upvalueName)
    if upvalueType == "Local" then
      -- OP_MOVE [A, B]    R(A) := R(B)
      self:emitInstruction("MOVE", 0, self:findVariableRegister(upvalueName))
    elseif upvalueType == "Upvalue" then
      -- OP_GETUPVAL [A, B]    R(A) := UpValue[B]
      self:emitInstruction("GETUPVAL", 0, self:findOrCreateUpvalue(upvalueName))
    else -- Assume it's a global.
      error(
        "CodeGenerator: The upvalue cannot be a global: "
        .. upvalueName
      )
    end
  end
end

-- Processes a function AST node into a function prototype (proto), creating a child
-- prototype for the function's body and handling upvalue capture. If
-- closureRegister is provided, emits a CLOSURE instruction to create the
-- closure in that register. Returns the compiled child prototype.
function CodeGenerator:processFunction(functionNode, closureRegister)
  local parentProto = self.proto
  local childProto  = self:emitPrototype({
    isVararg  = functionNode.isVararg,
    numParams = #functionNode.parameters,
  })
  self.proto = childProto
  self:processFunctionBody(functionNode)
  self.proto = parentProto

  -- Remove internal lookups, as they're no longer needed after codegen.
  childProto.constantLookup = nil
  childProto.upvalueLookup  = nil

  if closureRegister then
    self:generateClosure(childProto, closureRegister)
  end

  return childProto
end

--// Entry Point //--

function CodeGenerator:generate()
  return self:processFunction({
    body       = self.ast.body,
    parameters = {},
    isVararg   = true -- The main chunk always accepts varargs.
                      -- (used for command-line arguments)
  })
end

--[[
  ============================================================================
                                  (۶* ‘ヮ’)۶”
                            THE BYTECODE EMITTER!
  ============================================================================

  The bytecode emitter is responsible for converting the given Lua Function Prototypes
  into Lua bytecode. The bytecode emitter will implement binary writing logic
  to write the bytecode to a file, which can then be executed by the
  Lua Virtual Machine (VM).
--]]

-- Lua 5.1 Instruction Argument Modes. Instructions have different ways of
-- encoding their arguments (operands) within their 32 bits.
-- See "Lua 5.1 Instruction Formats" diagram below for details.

-- Instruction argument modes
--  - iABC:  Uses three arguments: A (8 bits), B (9 bits), C (9 bits).
--           Common for operations involving 3 registers or 2 regs + 1 constant/literal.
--           [ Op(6) | A(8) | C(9) | B(9) ]
local MODE_iABC = 0

--  - iABx:  Uses two arguments: A (8 bits), Bx (18 bits).
--           'Bx' combines B and C fields to hold a larger unsigned value,
--           often an index into the constant table or a prototype index.
--           [ Op(6) | A(8) |    Bx(18)    ]
local MODE_iABx = 1

--  - iAsBx: Uses two arguments: A (8 bits), sBx (18 bits, signed).
--           'sBx' is like Bx but represents a signed offset, primarily used
--           for jump instructions. The offset is biased, to get the actual value,
--           you need to subtract 131072 (2^17) from the value.
--           This allows for jumps in both directions (forward and backward).
--           [ Op(6) | A(8) |   sBx(18)    ]
local MODE_iAsBx = 2


-- Operand types
local OP_ARG_N = 0 -- Argument is not used
local OP_ARG_U = 1 -- Argument is used
local OP_ARG_R = 2 -- Argument is a register or a jump offset
local OP_ARG_K = 3 -- Argument is a constant or register/constant

-- Function Header Flags
local VARARG_NONE     = 0
local VARARG_ISVARARG = 2 -- Flag indicating a function accepts '...'

-- Note: There's another flag (VARARG_NEEDSARG) that indicates a function
--        uses the implicit `arg` variable (Lua 5.0+ feature), but since it's so
--        obscure and non-widely used, we won't implement it in TLC.
--
-- Read more:
--  What is VARARG_NEEDSARG - https://sourceforge.net/p/unluac/discussion/general/thread/5172ffe56a/
--  Lua 5.1 vararg system change - https://www.lua.org/manual/5.1/manual.html#7.1

-- Constant type flags
local LUA_TBOOLEAN = 1
local LUA_TNUMBER  = 3
local LUA_TSTRING  = 4

-- Operand sizes/positions
local POS_A   = 6
local POS_B   = 23
local POS_C   = 14
local POS_Bx  = 14
local POS_sBx = 14

--* BytecodeEmitter *--
local BytecodeEmitter = {}
BytecodeEmitter.__index = BytecodeEmitter

BytecodeEmitter.CONFIG = {
  LUA_COMMON_HEADER = "\27Lua" -- Standard magic number identifying Lua bytecode
    --[[version=]]             .. string.char(0x51) -- 0x51 signifies Lua version 5.1
    --[[format=]]              .. string.char(0)    -- 0 = official format version
    --[[endianness=]]          .. string.char(1)    -- 1 = little-endian, 0 = big-endian
    --[[sizeof(int)=]]         .. string.char(4)    -- sizeof(int) = 4 bytes
    --[[sizeof(size_t)=]]      .. string.char(8)    -- sizeof(size_t) = 8 bytes
    --[[sizeof(Instruction)=]] .. string.char(4)    -- sizeof(Instruction) = 4 bytes
    --[[sizeof(lua_Number)=]]  .. string.char(8)    -- sizeof(lua_Number) = 8 bytes
    --[[integral flag=]]       .. string.char(0),   -- 0 = lua_Number is floating-point

  --// Lua 5.1 Instruction Set //--

  -- This table maps all Lua 5.1 opcodes to their respective argument modes,
  -- it's used to determine how to encode the instruction arguments.
  --
  --  Format: [Opname] = {opcodeIndex, argumentMode, argBMode, argCMode}
  --   - opcodeIndex: The index (0-based) of the opcode in the Lua 5.1 instruction set.
  --   - argumentMode: The mode used to encode the instruction arguments.
  --   - argBMode: The type of the B argument (OP_ARG_N, OP_ARG_U, OP_ARG_R, OP_ARG_K).
  --   - argCMode: The type of the C argument (OP_ARG_N, OP_ARG_U, OP_ARG_R, OP_ARG_K).
  --
  -- Source: https://www.lua.org/source/5.1/lopcodes.c.html#luaP_opmodes
  -- stylua: ignore
  OPCODE_LOOKUP = {
    ["MOVE"]     = {0, MODE_iABC, OP_ARG_R, OP_ARG_N},   ["LOADK"]     = {1, MODE_iABx, OP_ARG_K, OP_ARG_N},
    ["LOADBOOL"] = {2, MODE_iABC, OP_ARG_U, OP_ARG_U},   ["LOADNIL"]   = {3, MODE_iABC, OP_ARG_R, OP_ARG_N},
    ["GETUPVAL"] = {4, MODE_iABC, OP_ARG_U, OP_ARG_N},   ["GETGLOBAL"] = {5, MODE_iABx, OP_ARG_K, OP_ARG_N},
    ["GETTABLE"] = {6, MODE_iABC, OP_ARG_R, OP_ARG_K},   ["SETGLOBAL"] = {7, MODE_iABx, OP_ARG_K, OP_ARG_N},
    ["SETUPVAL"] = {8, MODE_iABC, OP_ARG_U, OP_ARG_N},   ["SETTABLE"]  = {9, MODE_iABC, OP_ARG_K, OP_ARG_K},
    ["NEWTABLE"] = {10, MODE_iABC, OP_ARG_U, OP_ARG_U},  ["SELF"]      = {11, MODE_iABC, OP_ARG_R, OP_ARG_K},
    ["ADD"]      = {12, MODE_iABC, OP_ARG_K, OP_ARG_K},  ["SUB"]       = {13, MODE_iABC, OP_ARG_K, OP_ARG_K},
    ["MUL"]      = {14, MODE_iABC, OP_ARG_K, OP_ARG_K},  ["DIV"]       = {15, MODE_iABC, OP_ARG_K, OP_ARG_K},
    ["MOD"]      = {16, MODE_iABC, OP_ARG_K, OP_ARG_K},  ["POW"]       = {17, MODE_iABC, OP_ARG_K, OP_ARG_K},
    ["UNM"]      = {18, MODE_iABC, OP_ARG_R, OP_ARG_N},  ["NOT"]       = {19, MODE_iABC, OP_ARG_R, OP_ARG_N},
    ["LEN"]      = {20, MODE_iABC, OP_ARG_R, OP_ARG_N},  ["CONCAT"]    = {21, MODE_iABC, OP_ARG_R, OP_ARG_R},
    ["JMP"]      = {22, MODE_iAsBx, OP_ARG_R, OP_ARG_N}, ["EQ"]        = {23, MODE_iABC, OP_ARG_K, OP_ARG_K},
    ["LT"]       = {24, MODE_iABC, OP_ARG_K, OP_ARG_K},  ["LE"]        = {25, MODE_iABC, OP_ARG_K, OP_ARG_K},
    ["TEST"]     = {26, MODE_iABC, OP_ARG_R, OP_ARG_U},  ["TESTSET"]   = {27, MODE_iABC, OP_ARG_R, OP_ARG_U},
    ["CALL"]     = {28, MODE_iABC, OP_ARG_U, OP_ARG_U},  ["TAILCALL"]  = {29, MODE_iABC, OP_ARG_U, OP_ARG_U},
    ["RETURN"]   = {30, MODE_iABC, OP_ARG_U, OP_ARG_N},  ["FORLOOP"]   = {31, MODE_iAsBx, OP_ARG_R, OP_ARG_N},
    ["FORPREP"]  = {32, MODE_iAsBx, OP_ARG_R, OP_ARG_N}, ["TFORLOOP"]  = {33, MODE_iABC, OP_ARG_N, OP_ARG_U},
    ["SETLIST"]  = {34, MODE_iABC, OP_ARG_U, OP_ARG_U},  ["CLOSE"]     = {35, MODE_iABC, OP_ARG_N, OP_ARG_N},
    ["CLOSURE"]  = {36, MODE_iABx, OP_ARG_U, OP_ARG_N},  ["VARARG"]    = {37, MODE_iABC, OP_ARG_U, OP_ARG_N}
  }
}

--// BytecodeEmitter Constructor //--
function BytecodeEmitter.new(mainProto)
  --// Type Checking //--
  assert(type(mainProto) == "table", "Expected table for 'mainProto', got " .. type(mainProto))
  assert(mainProto.code and mainProto.constants, "Expected a valid Lua function prototype for 'mainProto'")

  --// Instance //--
  local self = setmetatable({}, BytecodeEmitter)

  --// Initialization //--
  self.mainProto = mainProto

  return self
end

--// Utility Functions //--

-- A custom implementation of the `frexp` function, which decomposes
-- a floating-point number into its mantissa and exponent.
-- This is necessary because Lua 5.3+ does not have `math.frexp`,
function BytecodeEmitter:frexp(value)
  -- Use built-in if available (Lua 5.1, Lua 5.2)
  if math.frexp then return math.frexp(value) end
  if value == 0 then return 0, 0 end

  local exponent = math.floor(math.log(math.abs(value)) / math.log(2)) + 1
  local mantissa = value / (2 ^ exponent)
  return mantissa, exponent
end

-- A bitwise left shift, simulated with multiplication.
function BytecodeEmitter:lshift(value, shift)
  return value * (2 ^ shift)
end

-- RK (Register-or-Constant) encoding:
--
-- Lua packs a register index and a constant index into the same 9-bit operand.
-- Values 0..255 refer to registers (stack[operand]).
-- Values >= 256 refer to constants and map to proto.constants[operand - 256].
--
-- TLC represents constants as negative indices (e.g. -1 == first constant).
-- encodeRKValue converts TLC's negative constant index into the unsigned RK
-- form used in bytecode (for example: -1 -> 256).
function BytecodeEmitter:encodeRKValue(value)
  if value < 0 then
    return 255 - value -- Equivalent to: 255 + math.abs(value)
  end
  return value
end

-- We use negative indices to represent constants internally, so we need to
-- convert them to unsigned indices for bytecode.
--
-- Example:
--   LOADK     5, -4 --> LOADK     5, 3
--   GETGLOBAL 2, -1 --> GETGLOBAL 2, 0
function BytecodeEmitter:toUnsigned(value)
  if value < 0 then
    return -value - 1 -- Equivalent to: math.abs(value) - 1
  end
  return value
end

-- Converts `value` number into 1 byte (8 bits) representation.
function BytecodeEmitter:encodeUint8(value)
  if value < 0 or value > 2^8 - 1 then
    error(
      "BytecodeEmitter: encodeUint8 value out of range of u8: "
      .. tostring(value)
    )
  end

  return string.char(value % 256)
end

-- Converts `value` number into 4 bytes (32 bits) little-endian representation.
function BytecodeEmitter:encodeUint32(value)
  if value < 0 or value > 2^32 - 1 then
    error(
      "BytecodeEmitter: encodeUint32 value out of range of u32: "
      .. tostring(value)
    )
  end

  local b1 = value % 256 value = math.floor(value / 256)
  local b2 = value % 256 value = math.floor(value / 256)
  local b3 = value % 256 value = math.floor(value / 256)
  local b4 = value % 256

  return string.char(b1, b2, b3, b4)
end

-- Converts `value` number into 8 bytes (64 bits) little-endian representation.
function BytecodeEmitter:encodeUint64(value)
  if value < 0 or value > 2^64 - 1 then
    error(
      "BytecodeEmitter: encodeUint64 value out of range of u64: "
      .. tostring(value)
    )
  end

  local lowWord  = value % 2^32
  local highWord = math.floor(value / 2^32)

  return self:encodeUint32(lowWord) .. self:encodeUint32(highWord)
end

-- Encodes a Lua number (float64) into its IEEE 754 binary representation (8 bytes).
-- Reference: https://en.wikipedia.org/wiki/Double-precision_floating-point_format.
function BytecodeEmitter:encodeFloat64(value)
  -- Handle the simple edge cases first.
  if value == 0 then
    -- Zero is just 8 zero-bytes. Easy!
    return self:encodeUint64(0)
  elseif value == 1/0 then -- Check for infinity
    -- The special representation for infinity is:
    --  exponent = all bits set to 1 (2047)
    --  faction  = 0
    local lowWord = 0
    local highWord = self:lshift(2047, 20) -- 0x7FF00000
    return self:encodeUint32(highWord) .. self:encodeUint32(lowWord)
  elseif value ~= value then -- Check for NaN (Not a Number)
    -- The special representation for NaN is:
    --  exponent = all bits set to 1 (2047)
    --  faction  = non-zero value for quiet NaN (we'll use 1)
    --
    -- Fun fact: IEEE 754 defines two types of NaN:
    --   - Quiet NaN (qNaN): The most significant bit of the fraction is set to 1.
    --   - Signaling NaN (sNaN): The most significant bit of the fraction is set to 0.
    --
    -- Here, we are using a quiet NaN representation.
    local lowWord = 1
    local highWord = self:lshift(2047, 20) -- 0x7FF00000
    return self:encodeUint32(highWord) .. self:encodeUint32(lowWord)
  end

  -- Deconstruct the number into its core parts.
  local sign = (value < 0 and 1) or 0
  local mantissa, exponent = self:frexp(math.abs(value))

  -- The exponent needs a "bias". frexp gives us an exponent 'e' where the
  -- number is `mantissa * 2^e`. IEEE 754 format needs a biased exponent.
  -- The math works out that we need to add 1022 to the frexp exponent.
  local biasedExponent = exponent + 1022

  -- The mantissa from frexp is between 0.5 and 1.0. We need to scale it
  -- to fit into the 52 bits of the fraction field. We also discard the
  -- implicit leading '1' bit of the fraction, hence the `-1`.
  --  `(mantissa * 2 - 1)` gets the fractional part, and we scale it by 2^52.
  local integerMantissa = (mantissa * 2 - 1) * (2^52)

  -- The lower 32 bits are simply the lower 32 bits of our 52-bit mantissa.
  -- We can get this with a modulo operation.
  local lowWord = integerMantissa % 2^32

  -- The higher 32 bits are a combination of the sign, exponent, and the
  -- remaining 20 bits of the mantissa.
  local mantissaHigh = math.floor(integerMantissa / 2^32)

  -- Now we use lshift to put everything in its place for the high word:
  --  - The sign bit is the 31st bit.
  --  - The exponent's 11 bits start at the 20th bit.
  --  - The remaining 20 bits of the mantissa fill the rest.
  local highWord = self:lshift(sign, 31)
                  + self:lshift(biasedExponent, 20)
                  + mantissaHigh

  -- Convert the two 32-bit chunks to bytes.
  -- Since we're little-endian, the "low" part comes first.
  return self:encodeUint32(lowWord) .. self:encodeUint32(highWord)
end

--// Bytecode Generation //--
function BytecodeEmitter:encodeString(value)
  value = value .. "\0"
  local size = self:encodeUint64(#value)
  return size .. value
end

-- Encodes a Lua constant into its bytecode representation.
function BytecodeEmitter:encodeConstant(constantValue)
  local constType = type(constantValue)

  if constType == "boolean" then
    return self:encodeUint8(LUA_TBOOLEAN) .. self:encodeUint8(constantValue and 1 or 0)
  elseif constType == "number" then
    return self:encodeUint8(LUA_TNUMBER) .. self:encodeFloat64(constantValue)
  elseif constType == "string" then
    return self:encodeUint8(LUA_TSTRING) .. self:encodeString(constantValue)
  end

  error("BytecodeEmitter: Unsupported constant type '" .. constType .. "'")
end

-- Helper to validate operand ranges.
-- Prevents silent overflows that would corrupt the bytecode.
function BytecodeEmitter:validateOperand(value, min, max, operandName, instructionName)
  if value < min or value > max then
    error(
      string.format(
        "BytecodeEmitter: Operand %s overflow in instruction '%s'. Got %d. Valid range is %d to %d.",
        operandName, instructionName, value, min, max
      )
    )
  end
end

-- Encodes an instruction in iABC mode.
-- Format: [ Opcode(6) | A(8) | C(9) | B(9) ]
--
-- NOTE: In the binary layout, C (bits 14-22) comes BEFORE B (bits 23-31).
function BytecodeEmitter:encodeABCInstruction(opcode, a, instruction, argBMode, argCMode)
  local instructionName = instruction[1]
  local b, c = instruction[3], instruction[4]

  -- Validate operands B and C (9 bits signed: -256 to 255 during RK encoding steps).
  self:validateOperand(b, -256, 255, "B", instructionName)
  self:validateOperand(c, -256, 255, "C", instructionName)

  -- Handle RK() operands (register or constant).
  -- If the argument mode is OpArgK, we encode constants specially.
  if argBMode == OP_ARG_K then b = self:encodeRKValue(b) end
  if argCMode == OP_ARG_K then c = self:encodeRKValue(c) end

  -- Shift components to their positions.
  local shiftedA = self:lshift(a, POS_A)
  local shiftedB = self:lshift(b, POS_B)
  local shiftedC = self:lshift(c, POS_C)

  -- Combine all parts into a single 32-bit integer.
  return self:encodeUint32(opcode + shiftedA + shiftedB + shiftedC)
end

-- Encodes an instruction in iABx mode.
-- Format: [ Opcode(6) | A(8) | Bx(18) ]
--
-- Used for loading constants (LOADK) and global lookups (GETGLOBAL/SETGLOBAL).
function BytecodeEmitter:encodeABxInstruction(opcode, a, instruction)
  -- Bx is an unsigned 18-bit integer.
  -- Since we use negative indices for constants internally,
  -- we need to convert it to unsigned form.
  local instructionName = instruction[1]
  local bx = self:toUnsigned(instruction[3])

  -- Validate operand Bx (18 bits unsigned: 0 to 262143).
  self:validateOperand(bx, 0, 262143, "Bx", instructionName)

  -- Shift components to their positions.
  local shiftedA  = self:lshift(a, POS_A)
  local shiftedBx = self:lshift(bx, POS_Bx)

  -- Combine all parts into a single 32-bit integer.
  return self:encodeUint32(opcode + shiftedA + shiftedBx)
end

-- Encodes an instruction in iAsBx mode.
-- Format: [ Opcode(6) | A(8) | sBx(18) ]
--
-- Used for jumps (JMP, FORLOOP).
-- sBx is a signed 18-bit integer represented in "Excess-K" encoding.
function BytecodeEmitter:encodeAsBxInstruction(opcode, a, instruction)
  local instructionName = instruction[1]
  local b = instruction[3]

  -- Validate operand sBx (18 bits signed: -131072 to 131071).
  self:validateOperand(b, -131072, 131071, "sBx", instructionName)

  -- sBx Encoding (Bias):
  -- We add a bias of 131071 (2^17 - 1) to the value.
  -- This maps the range [-131071, 131072] to the unsigned range [0, 262143].
  -- This allows the processor to treat the field as unsigned while strictly
  -- representing signed jump offsets.
  local bias = 2^17 - 1
  local sbx  = b + bias

  -- Shift components to their positions.
  local shiftedA  = self:lshift(a, POS_A)
  local shiftedBx = self:lshift(sbx, POS_sBx)

  -- Combine all parts into a single 32-bit integer.
  return self:encodeUint32(opcode + shiftedA + shiftedBx)
end

-- Serializes a single instruction into its 4-byte representation.
-- We build the 32-bit number by shifting each component to its designated
-- position and adding them together.
--
-- Field     | Size (bits) | Shift Left By | Position (from bit 0)
-- ----------|-------------|---------------|-----------------------
-- Opcode    | 6           | 0             | 0-5
-- A         | 8           | 6             | 6-13
-- C         | 9           | 14            | 14-22 (for iABC)
-- Bx/sBx    | 18          | 14            | 14-31 (for iABx/iAsBx)
-- B         | 9           | 23            | 23-31 (for iABC)
function BytecodeEmitter:encodeInstruction(instruction)
  -- First, let's look up the opcode's number and its argument format.
  local instructionName = tostring(instruction[1] or "<unknown>")
  local opcodeData      = self.CONFIG.OPCODE_LOOKUP[instructionName]

  if not opcodeData then
    error("BytecodeEmitter: Unsupported instruction '" .. instructionName .. "'")
  end

  -- Unpack opcode data.
  local opcode, opmode     = opcodeData[1], opcodeData[2]
  local argBMode, argCMode = opcodeData[3], opcodeData[4]
  local a = instruction[2]

  -- Validate operand A (8 bits unsigned: 0 to 255)
  self:validateOperand(a, 0, 255, "A", instructionName)

  -- Dispatch to the appropriate encoder based on the instruction mode.
  if opmode == MODE_iABC then
    return self:encodeABCInstruction(opcode, a, instruction, argBMode, argCMode)
  elseif opmode == MODE_iABx then
    return self:encodeABxInstruction(opcode, a, instruction)
  elseif opmode == MODE_iAsBx then
    return self:encodeAsBxInstruction(opcode, a, instruction)
  end

  error(
    "BytecodeEmitter: Unsupported instruction format for '"
    .. instructionName
    .. "'"
  )
end

-- Serializes the code section of a function prototype.
function BytecodeEmitter:encodeCodeSection(proto)
  local codeSection = { }
  for index, instruction in ipairs(proto.code) do
    codeSection[index] = self:encodeInstruction(instruction)
  end
  return table.concat(codeSection)
end

-- Serializes the constant section of a function prototype.
function BytecodeEmitter:encodeConstantSection(proto)
  local constantSection = { }
  for index, constant in ipairs(proto.constants) do
    constantSection[index] = self:encodeConstant(constant)
  end
  return table.concat(constantSection)
end

-- Serializes the nested prototype section of a function prototype.
function BytecodeEmitter:encodePrototypeSection(proto)
  local protoSection = { }
  for index, childPrototype in ipairs(proto.protos) do
    protoSection[index] = self:encodePrototype(childPrototype)
  end
  return table.concat(protoSection)
end

-- Serializes a complete Lua Function Prototype chunk.
function BytecodeEmitter:encodePrototype(proto)
  -- Basic validation of the function prototype.
  if not proto.code or not proto.constants then
    error("BytecodeEmitter: Invalid function prototype")
  end

  return table.concat({
    -- Header --
    self:encodeString(proto.functionName), -- Source name.
    self:encodeUint32(0),                  -- Line defined (debug).
    self:encodeUint32(0),                  -- Last line defined (debug).
    self:encodeUint8(#proto.upvalues),     -- nups (Number of upvalues).
    self:encodeUint8(proto.numParams),     -- Number of parameters.
    self:encodeUint8((proto.isVararg and VARARG_ISVARARG) or VARARG_NONE),
    self:encodeUint8(proto.maxStackSize),  -- Max stack size.

    -- Sections --
    self:encodeUint32(#proto.code),        -- Instruction count.
    self:encodeCodeSection(proto),         -- Code section.

    self:encodeUint32(#proto.constants),   -- Constant count.
    self:encodeConstantSection(proto),     -- Constant section.

    self:encodeUint32(#proto.protos),      -- Prototype count.
    self:encodePrototypeSection(proto),    -- Nested prototype section.

    -- Debug info (not implemented) --
    self:encodeUint32(0), -- Line info (debug).
    self:encodeUint32(0), -- Local variables (debug).
    self:encodeUint32(0)  -- Upvalues (debug).

    -- Note: TLC doesn't implement debug info as that would require tracking
    --       line numbers, local variables, and upvalue names, etc. which will
    --       add a lot of unnecessary complexity to the compiler.
  })
end

--// Main //--
function BytecodeEmitter:emit()
  return self.CONFIG.LUA_COMMON_HEADER .. self:encodePrototype(self.mainProto)
end

--[[
    ============================================================================
                                      (/^▽^)/
                                THE VIRTUAL MACHINE!
    ============================================================================

    Welcome to the final stage of the pipeline!

    The Virtual Machine (VM) is the engine that actually "runs" your code. While
    the Compiler translates source text into a structure the computer can understand
    (the Function Prototype), the VM is the component that interprets that structure
    and performs the actions it describes.

    This VM implements Lua 5.1's register-based architecture. Unlike stack-based
    VMs (like the JVM) where operands are pushed and popped, Lua instructions
    refer directly to specific slots (registers) in a stack array. This approach
    maps efficiently to modern CPU architectures and reduces the total instruction
    count.

    Our VM executes a "Prototype" (proto) object generated by the Code Generator,
    which holds the bytecode instructions, constants, and upvalues. To keep the focus
    on logic rather than file formats, we execute the Lua table representation
    of the prototype directly instead of parsing binary `.luac` files. This
    avoids complex bitwise manipulation and lets us focus entirely on how the
    instructions work.
--]]

--* Constants *--
local unpack = (unpack or table.unpack) -- Lua 5.1/5.2+ compatibility.
local USE_CURRENT_TOP = 0

--* VirtualMachine *--
local VirtualMachine = {}
VirtualMachine.__index = VirtualMachine -- Set up for method calls via `.`.

VirtualMachine.CONFIG = {
  -- Number of list items to accumulate before a SETLIST instruction.
  FIELDS_PER_FLUSH = 50
}

--// VirtualMachine Constructor //--
function VirtualMachine.new(proto)
  local self = setmetatable({}, VirtualMachine)

  self.mainProto = proto -- Should only be used in `:execute()`.
  self.closure   = nil   -- Current closure we're working with.

  return self
end

--// Auxiliary functions //--
local function pack(...)
  return select("#", ...), { ... }
end

function VirtualMachine:pushClosure(closure)
  closure = {
    env       = closure.env       or _G,
    proto     = closure.proto,
    upvalues  = closure.upvalues  or {},
  }
  self.closure = closure

  return closure
end

--// Execution //--

-- Implementation Note:
--  This is not a full Lua VM implementation! It's a simplified VM designed
--  specifically to be small, easy to understand, and capable of executing
--  TLC-generated bytecode.
--
--  For example, in a standard Lua VM, "CLOSURE" would allocate a proper LClosure
--  structure (see lvm.c, lfunc.c, lobject.h in Lua 5.1 source). This structure
--  holds a reference to the function prototype, an array of upvalue pointers,
--  and runtime state like the environment.
--
--  TLC simplifies this by wrapping the closure in a native Lua function.
--
--  Additionally, we don't handle such things as garbage collection, proper
--  upvalue closing, debug hooks, metamethods, or coroutines, we rely on the
--  host Lua environment for that.
--
--  Why this design? First, it provides seamless interoperability, allowing
--  TLC-compiled functions to be passed to native Lua APIs like table.foreach
--  or pcall without reimplementing the entire standard library. Second, it
--  dramatically simplifies the implementation by avoiding the need for
--  CallInfo frames, stack bases, and complex upvalue management with open
--  and closed upvalue lists.
--
--  The trade-off is that we lose some fidelity compared to a real Lua VM.
--  We don't have proper function identity tracking, and debug hooks are
--  harder to implement. However, we gain a working VM in roughly 500 lines
--  of code.
function VirtualMachine:executeClosure(...)
  -- Optimization: localize fields.
  local closure   = self.closure
  local stack     = {}
  local env       = closure.env
  local proto     = closure.proto
  local upvalues  = closure.upvalues
  local code      = proto.code
  local constants = proto.constants
  local numparams = proto.numParams
  local isVararg  = proto.isVararg

  local maxStackSize = proto.maxStackSize
  local top = maxStackSize
  local upvalueStack = {}
  local maxUpvalue = -1

  -- Gets a value from either the stack or constants table.
  -- NOTE: Constant indices are represented as negative numbers
  -- in our implementation, so in order to get the correct constant,
  -- we negate the index when accessing the constants table.
  local function rk(index)
    if index < 0 then return constants[-index] end
    return stack[index]
  end

  -- Initialize parameters.
  local vararg, varargLen
  local params = { ... }

  for paramIdx = 1, numparams do
    stack[paramIdx - 1] = params[paramIdx]
  end
  if isVararg then
    varargLen, vararg = pack(select(numparams + 1, ...))
    stack[numparams] = vararg -- Implicit "arg" argument.
  end

  -- "PC" stands for Program Counter, it tracks which instruction we're on.
  -- Sometimes we need to modify it directly (for jumps), so we can't use
  -- a simple for-loop to iterate over instructions linearly.
  local pc = 1
  while true do
    local instruction = code[pc]
    if not instruction then
      break
    end

    local opname = instruction[1]
    local a, b, c = instruction[2], instruction[3], instruction[4]

    -- NOTE:
    --  Many Lua VM implementations written in Lua use binary search on sorted
    --  opcode tables for faster dispatch, reducing lookup time from O(n) to
    --  O(log n). For clarity and maintainability, TLC uses a simple linear
    --  if-else chain instead. This makes the code easier to read and modify.

    -- OP_MOVE [A, B]    R(A) := R(B)
    -- Copy a value between registers.
    if opname == "MOVE" then
      stack[a] = stack[b]

    -- OP_LOADK [A, Bx]    R(A) = Kst(Bx)
    -- Load a constant into a register.
    elseif opname == "LOADK" then
      stack[a] = constants[-b]

    -- OP_LOADBOOL [A, B, C]    R(A) := (Bool)B; if (C) pc++
    -- Load a boolean into a register.
    elseif opname == "LOADBOOL" then
      stack[a] = (b == 1)
      if c == 1 then
        pc = pc + 1
      end

    -- OP_LOADNIL [A, B]    R(A) := ... := R(B) := nil
    -- Load nil values into a range of registers.
    elseif opname == "LOADNIL" then
      local reg = b
      repeat
        stack[reg] = nil
        reg = reg - 1
      until reg < a

    -- OP_GETUPVAL [A, B]    R(A) := UpValue[B]
    -- Read an upvalue into a register.
    elseif opname == "GETUPVAL" then
      local upvalue = upvalues[b + 1]
      stack[a] = upvalue.stack[upvalue.index]

    -- OP_GETGLOBAL [A, Bx]    R(A) := Gbl[Kst(Bx)]
    -- Read a global variable into a register.
    elseif opname == "GETGLOBAL" then
      stack[a] = env[constants[-b]]

    -- OP_GETTABLE [A, B, C]    R(A) := R(B)[RK(C)]
    -- Read a table element into a register.
    elseif opname == "GETTABLE" then
      stack[a] = stack[b][rk(c)]

    -- OP_SETGLOBAL [A, Bx]    Gbl[Kst(Bx)] := R(A)
    -- Write a register value into a global variable.
    elseif opname == "SETGLOBAL" then
      env[rk(b)] = stack[a]

    -- OP_SETUPVAL [A, B]    UpValue[B] := R(A)
    -- Write a register value into an upvalue.
    elseif opname == "SETUPVAL" then
      local upvalue = upvalues[b + 1]
      upvalue.stack[upvalue.index] = stack[a]

    -- OP_SETTABLE [A, B, C]    R(A)[RK(B)] := RK(C)
    -- Write a register value into a table element.
    elseif opname == "SETTABLE" then
      stack[a][rk(b)] = rk(c)

    -- OP_NEWTABLE [A, B, C]    R(A) := {} (size = B,C)
    -- Create a new table and store it in a register.
    -- B = array part size hint, C = hash part size hint (ignored here).
    elseif opname == "NEWTABLE" then
      stack[a] = {}

    -- OP_SELF [A, B, C]    R(A+1) := R(B); R(A) := R(B)[RK(C)]
    -- Prepare an object method for calling.
    elseif opname == "SELF" then
      local rb = stack[b]
      stack[a + 1] = rb
      stack[a] = rb[rk(c)]

    -- OP_ADD [A, B, C]    R(A) := RK(B) + RK(C)
    -- Add two values and store the result in a register.
    elseif opname == "ADD" then
      stack[a] = rk(b) + rk(c)

    -- OP_SUB [A, B, C]    R(A) := RK(B) - RK(C)
    -- Subtract two values and store the result in a register.
    elseif opname == "SUB" then
      stack[a] = rk(b) - rk(c)

    -- OP_MUL [A, B, C]    R(A) := RK(B) * RK(C)
    -- Multiply two values and store the result in a register.
    elseif opname == "MUL" then
      stack[a] = rk(b) * rk(c)

    -- OP_DIV [A, B, C]    R(A) := RK(B) / RK(C)
    -- Divide two values and store the result in a register.
    elseif opname == "DIV" then
      stack[a] = rk(b) / rk(c)

    -- OP_MOD [A, B, C]    R(A) := RK(B) % RK(C)
    -- Calculate the modulus of two values and store the result in a register.
    elseif opname == "MOD" then
      stack[a] = rk(b) % rk(c)

    -- OP_POW [A, B, C]    R(A) := RK(B) ^ RK(C)
    -- Raise a value to the power of another and store the result in a register.
    elseif opname == "POW" then
      stack[a] = rk(b) ^ rk(c)

    -- OP_UNM [A, B]    R(A) := -R(B)
    -- Negate a value and store the result in a register.
    elseif opname == "UNM" then
      stack[a] = -stack[b]

    -- OP_NOT [A, B]    R(A) := not R(B)
    -- Logical NOT of a value and store the result in a register.
    elseif opname == "NOT" then
      stack[a] = not stack[b]

    -- OP_LEN [A, B]    R(A) := length of R(B)
    -- Get the length of a value and store it in a register.
    elseif opname == "LEN" then
      stack[a] = #stack[b]

    -- OP_CONCAT [A, B, C]    R(A) := R(B).. ... ..R(C)
    -- Concatenate a range of registers and store the result in a register.
    elseif opname == "CONCAT" then
      for reg = c - 1, b, -1 do
        -- NOTE: We cannot use table.concat as it does not call metamethods
        stack[reg] = stack[reg] .. stack[reg + 1]
      end
      stack[a] = stack[b]

    -- OP_JMP [A, sBx]    pc+=sBx
    -- Jump to a new instruction offset.
    elseif opname == "JMP" then
      pc = pc + b

    -- OP_EQ [A, B, C]    if ((RK(B) == RK(C)) ~= A) then pc++
    -- Conditional jump based on equality.
    elseif opname == "EQ" then
      local isEqual = rk(b) == rk(c)
      local bool = (a == 1)
      if isEqual ~= bool then
        pc = pc + 1
      end

    -- OP_LT [A, B, C]    if ((RK(B) < RK(C)) ~= A) then pc++
    -- Conditional jump based on less-than comparison.
    elseif opname == "LT" then
      local isLessThan = rk(b) < rk(c)
      local bool = (a == 1)
      if isLessThan ~= bool then
        pc = pc + 1
      end

    -- OP_LE [A, B, C]    if ((RK(B) <= RK(C)) ~= A) then pc++
    -- Conditional jump based on less-than-or-equal comparison.
    elseif opname == "LE" then
      local isLessThanOrEqual = rk(b) <= rk(c)
      local bool = (a == 1)
      if isLessThanOrEqual ~= bool then
        pc = pc + 1
      end

    -- OP_TEST [A, C]    if not (R(A) <=> C) then pc++
    -- Boolean test, with a conditional jump.
    -- NOTE: After this instruction, the next instruction will always be a jump.
    elseif opname == "TEST" then
      local bool = (c == 1)
      if (not stack[a]) == bool then
        pc = pc + 1
      else
        -- Optimization:
        --  Since we know that the next instruction is guaranteed to be a jump,
        --  we can implement it here to avoid an extra iteration of the loop.
        --
        -- TODO: Should we verify that the next instruction is indeed a JMP?
        local nextInstruction = code[pc + 1]
        local jumpDistance = nextInstruction[3]
        pc = pc + 1 + jumpDistance
      end

    -- OP_TESTSET [A, B, C]    if (R(B) <=> C) then R(A) := R(B) else pc++
    -- Boolean test, with conditional assignment and jump.
    -- NOTE: After this instruction, the next instruction will always be a jump.
    elseif opname == "TESTSET" then
      local bool = (c == 1)
      if (not stack[b]) == bool then
        stack[a] = stack[b]

        -- Optimization:
        --  Since we know that the next instruction is guaranteed to be a jump,
        --  we can implement it here to avoid an extra iteration of the loop.
        --
        -- TODO: Should we verify that the next instruction is indeed a JMP?
        local nextInstruction = code[pc + 1]
        local jumpDistance = nextInstruction[3]
        pc = pc + 1 + jumpDistance
      else
        pc = pc + 1
      end

    -- OP_CALL [A, B, C]    R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
    -- Call a closure (function) with arguments and handle returns.
    elseif opname == "CALL" then
      local func = stack[a]
      if b ~= USE_CURRENT_TOP then
        top = a + b
      end

      local nReturns, returns = pack(func(unpack(stack, a + 1, top - 1)))

      if c ~= USE_CURRENT_TOP then
        nReturns = c - 1
      else
        -- Multi-return: push all results onto the stack.
        top = a + nReturns
      end
      for index = 1, nReturns do
        stack[a + index - 1] = returns[index]
      end

    -- OP_TAILCALL [A, B, C]    return R(A)(R(A+1), ... ,R(A+B-1))
    -- Perform a tail call to a closure (function).
    elseif opname == "TAILCALL" then
      local func = stack[a]
      if b ~= USE_CURRENT_TOP then
        top = a + b
      end

      -- Optimization:
      --   Since TAILCALL always comes before RETURN,
      --   we can just return the function call results directly.

      return func(unpack(stack, a + 1, top - 1))

    -- OP_RETURN [A, B]    return R(A), ... ,R(A+B-2)
    -- Return values from function call.
    elseif opname == "RETURN" then
      if b ~= USE_CURRENT_TOP then
        top = a + b - 1
      end

      return unpack(stack, a, top - 1)

    -- OP_FORLOOP [A, sBx]   R(A)+=R(A+2)
    --                       if R(A) <?= R(A+1) then { pc+=sBx R(A+3)=R(A) }
    -- Iterate a numeric for loop.
    elseif opname == "FORLOOP" then
      local step  = stack[a + 2]
      local idx   = stack[a] + step
      local limit = stack[a + 1]

      local shouldContinue = false
      if step >= 0 then
        shouldContinue = (idx <= limit)
      else
        shouldContinue = (idx >= limit)
      end

      if shouldContinue then
        pc = pc + b
        stack[a] = idx
        stack[a + 3] = idx
      end

    -- OP_FORPREP [A, sBx]    R(A)-=R(A+2) pc+=sBx
    -- Initialize a numeric for loop.
    elseif opname == "FORPREP" then
      local init   = stack[a]
      local plimit = stack[a + 1]
      local pstep  = stack[a + 2]
      if not tonumber(init) then
        error("Initial value must be a number")
      elseif not tonumber(plimit) then
        error("Limit must be a number")
      elseif not tonumber(pstep) then
        error("Step must be a number")
      end

      stack[a] = init - pstep
      pc = pc + b

    -- OP_TFORLOOP [A, C]    R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2))
    --                       if R(A+3) ~= nil then R(A+2)=R(A+3) else pc++
    -- Iterate a generic for loop.
    -- NOTE: After this instruction, the next instruction will always be a jump.
    elseif opname == "TFORLOOP" then
      local cb = a + 3

      -- Copy function and arguments for the call.
      stack[cb + 2] = stack[a + 2] -- The iterator function.
      stack[cb + 1] = stack[a + 1] -- The state.
      stack[cb]     = stack[a]     -- The control variable.

      -- Call the iterator function.
      local iteratorFunc = stack[cb]
      local returns = { iteratorFunc(stack[cb + 1], stack[cb + 2]) }

      -- Place results on the stack, starting at cb.
      -- The number of results to keep is specified by `c`.
      for i = 1, c do
        stack[cb + i - 1] = returns[i]
      end

      -- Check if the new control variable is nil.
      if stack[cb] ~= nil then
        -- If not nil, copy it to R(A+2) (the control variable).
        stack[cb - 1] = stack[cb]

        -- Optimization:
        --  Since we know that the next instruction is guaranteed to be a jump,
        --  we can implement it here to avoid an extra iteration of the loop.
        --
        -- TODO: Should we verify that the next instruction is indeed a JMP?
        local nextInstruction = code[pc + 1]
        local jumpDistance = nextInstruction[3]
        pc = pc + 1 + jumpDistance
      else
        -- Otherwise, skip the next instruction.
        -- (Should be the jump back to the loop start.)
        pc = pc + 1
      end

    -- OP_SETLIST [A, B, C]    R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B
    -- Set a range of array elements in a table.
    elseif opname == "SETLIST" then
      local targetTable = stack[a]
      if type(targetTable) ~= "table" then
        error("SETLIST target is not a table")
      end

      local len = b
      if len == USE_CURRENT_TOP then
        len = top - a - 1
      end

      -- If C is 0, it indicates that the next instruction
      -- contains the actual C value (not implemented here).
      if c == 0 then
        error("SETLIST with C=0 is not supported in this VM implementation")
      end

      local offset = (c - 1) * self.CONFIG.FIELDS_PER_FLUSH
      for i = 1, len do
        targetTable[offset + i] = stack[a + i]
      end

    -- OP_CLOSE [A]    close all variables in the stack up to (>=) R(A)
    elseif opname == "CLOSE" then
      for i = a, maxUpvalue do
        local uv = upvalueStack[i]
        if uv then
          uv.stack = { stack[uv.index] }
          uv.index = 1
          upvalueStack[i] = nil
        end
      end
      maxUpvalue = a - 1

    -- OP_CLOSURE [A, Bx]    R(A) := closure(KPROTO[Bx], R(A), ... ,R(A+n))
    -- Create a new closure (function) and store it in a register.
    elseif opname == "CLOSURE" then
      local tProto = proto.protos[b + 1]
      local tProtoUpvalues = {}

      -- Capture upvalues for the new closure.
      -- These are pseudo-instructions, and we need to skip
      -- over them while processing the CLOSURE instruction.
      for _ = 1, #tProto.upvalues do
        pc = pc + 1

        local pseudoInstruction = code[pc]
        local nextOpname = pseudoInstruction[1]
        local index = pseudoInstruction[3]
        if nextOpname == "MOVE" then
          local upvalue = upvalueStack[index] or {
            index = index,
            stack = stack,
          }
          upvalueStack[index] = upvalue
          if index > maxUpvalue then
            maxUpvalue = index
          end
          table.insert(tProtoUpvalues, upvalue)
        elseif nextOpname == "GETUPVAL" then
          local upvalue = upvalues[index + 1]
          table.insert(tProtoUpvalues, upvalue)
        else
          error(
            "Unexpected instruction while capturing upvalues: "
            .. tostring(nextOpname)
          )
        end
      end

      local tClosure = {
        env       = nil,
        proto     = tProto,
        upvalues  = tProtoUpvalues,
      }

      stack[a] = function(...)
        self:pushClosure(tClosure)
        local nReturns, returns = pack(self:executeClosure(...))
        self:pushClosure(closure)

        return unpack(returns, 1, nReturns)
      end

    -- OP_VARARG [A, B]    R(A), R(A+1), ..., R(A+B-1) = vararg
    -- Load vararg function arguments into registers.
    elseif opname == "VARARG" then
      local upperBound = b - 1
      if b == USE_CURRENT_TOP then
        top = a + varargLen
        upperBound = top
      end

      for i = 1, upperBound do
        stack[a + i - 1] = vararg[i]
      end
    else
      error("Unimplemented instruction: " .. tostring(opname))
    end

    -- Advance to the next instruction.
    pc = pc + 1
  end
end

function VirtualMachine:execute()
  -- Push main closure.
  self:pushClosure({
    env       = _G,
    proto     = self.mainProto,
    upvalues  = {},
  })

  return self:executeClosure()
end

-- Now I'm just exporting everything...
return {
  Tokenizer       = Tokenizer,
  Parser          = Parser,
  CodeGenerator   = CodeGenerator,
  BytecodeEmitter = BytecodeEmitter,
  VirtualMachine  = VirtualMachine,

  -- Shortcuts for convenience.
  -- It is highly recommended to use these shortcut functions for all tasks!
  -- Why? Because these functions act as a stable, future-proof entry points.
  -- If the internal compilation steps (tokenizer, parser, codegen, etc.) ever change,
  -- code that uses ONLY these functions is more likely to continue to work as expected.
  -- If you use the classes directly, your code may break with future updates!
  -- Always use the shortcut functions unless you are hacking on the compiler internals.
  fullCompile = function(code)
    assert(type(code) == "string", "Expected a string for 'code', got " .. type(code))

    local tokens   = Tokenizer.new(code):tokenize()
    local ast      = Parser.new(tokens):parse()
    local proto    = CodeGenerator.new(ast):generate()
    local bytecode = BytecodeEmitter.new(proto):emit()

    return bytecode
  end,
  compileAndRun = function(code)
    assert(type(code) == "string", "Expected a string for 'code', got " .. type(code))

    local tokens = Tokenizer.new(code):tokenize()
    local ast    = Parser.new(tokens):parse()
    local proto  = CodeGenerator.new(ast):generate()

    local vm = VirtualMachine.new(proto)
    return vm:execute()
  end
}