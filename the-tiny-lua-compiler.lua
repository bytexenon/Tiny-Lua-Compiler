--[[
  Hey everyone!

  Today, we're embarking on a fun little adventure: writing a compiler
  for Lua 5.1... entirely in Lua itself!

  But this isn't just ANY compiler. Oh no. This is a Super Tiny
  compiler! So small, in fact, that if you strip away all these lovely
  comments, it clocks in at around ~3000 lines of pure, sweet code.

  Despite its size, this little guy is mighty! It's designed to
  tokenize, parse, and compile (most of!) the Lua 5.1 code you can
  throw at it. It's even brave enough to compile itself! How cool is that?!
--]]

-- Converts lists into lightning-fast lookup dictionaries (or 'sets').
-- WHY? When you need to quickly check if an item is present in a collection
-- (like checking if a word is a keyword), iterating over a simple list is slow
-- (O(n)). This function transforms a list like {"a", "b"} into a table like
-- {a=true, b=true}. Checking `lookup[value]` is then near-instant (O(1)). Boom!
local function createLookupTable(list)
  local lookup = {}
  for _, value in ipairs(list) do
    lookup[value] = true
  end
  return lookup
end

-- Creates prefix trees (Tries) for operator matching awesomeness.
-- Imagine a branching path where each character in an operator ("+", "==", "and")
-- takes you down a specific branch. This structure allows us to efficiently
-- look ahead in the input stream and match the longest possible operator
-- starting at the current character, which is crucial for languages like Lua
-- that have multi-character operators (e.g., distinguishing "=" from "==").
local function makeTrie(ops)
  local trie = {} -- Our empty tree, ready to grow branches
  for _, op in ipairs(ops) do
    local node = trie -- Start planting at the root of the tree

    -- Plant each character of the operator as a node in the tree...
    for char in op:gmatch(".") do
      -- If the branch for this character doesn't exist, create it
      node[char] = node[char] or {}
      -- Move deeper into the tree along the branch for this character
      node = node[char]
    end

    -- Mark the end of the path with the full operator string.
    -- This signifies that a complete operator has been matched along this path.
    node.Value = op
  end
  return trie
end

-- Creates pattern lookup tables for lightning-fast single-character checks.
-- Pro tip: Repeatedly checking if a single character matches a pattern (like "%s" for whitespace)
-- using `string.match` is like re-reading the manual for every character.
-- We pre-calculate which of the 255 possible single-byte characters match a given pattern
-- and store the results in a lookup table. This turns `char:match(pattern)`
-- into a fast `lookup[char]` check.
local function makePatternLookup(pattern)
  local lookup = {}

  -- Check ALL single-byte characters (ASCII 1 to 255) against the pattern
  for code = 1, 255 do
    local char = string.char(code)
    -- Does this character match our secret pattern?
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
  -- Maps single-character punctuation to their respective token types.
  PUNCTUATION = {
    [":"] = "Colon",       [";"] = "Semicolon",
    [","] = "Comma",       ["."] = "Dot",
    ["("] = "LeftParen",   [")"] = "RightParen",
    ["{"] = "LeftBrace",   ["}"] = "RightBrace",
    ["["] = "LeftBracket", ["]"] = "RightBracket",
    ["="] = "Equals"
  },

  --[[
    Escape Sequence Decoder Ring

    When the tokenizer encounters a backslash '\' inside a string literal,
    it looks up the next character in this table to determine if it's the
    start of a known escape sequence (like '\n' for newline or '\\' for a
    literal backslash). This table provides the mapping from the escaped
    character (e.g., "n") to the actual character value (e.g., newline character).
    Numeric escape sequences (\ddd) are handled separately.
  --]]
  ESCAPES =  {
    ["a"]  = "\a",  -- Bell (alert sound)
    ["b"]  = "\b",  -- Backspace
    ["f"]  = "\f",  -- Form feed (printer page eject)
    ["n"]  = "\n",  -- New line
    ["r"]  = "\r",  -- Carriage return
    ["t"]  = "\t",  -- Horizontal tab
    ["v"]  = "\v",  -- Vertical tab

    ["\\"] = "\\",  -- Literal backslash character itself
    ["\""] = "\"",  -- Literal double quote character
    ["\'"] = "\'"   -- Literal single quote character
  },

  --[[
    Language Keyword Dictionary (Lookup)

    This lookup table (created from a list via `createLookupTable`) contains
    all of Lua 5.1's reserved keywords. These words have special grammatical
    meaning and cannot be used as identifiers (variable names, function names, etc.).
    We check against this table using an O(1) lookup after potentially identifying
    an identifier sequence to see if it's actually a reserved keyword.
  --]]
  KEYWORDS = createLookupTable({
    "and",      "break", "do",    "else",
    "elseif",   "end",   "false", "for",
    "function", "if",    "in",    "local",
    "nil",      "not",   "or",    "repeat",
    "return",   "then",  "true",  "until",
    "while"
  }),

  --[[
    Master List of Lua Operators

    This table isn't used in the tokenizer itself, instead, it's being used
    as a template for other constants to be created from, like the operator trie
    and lookup table. It's a handy reference for all the operators in Lua.
  --]]
  OPERATOR_TRIE = makeTrie({
    "^", "*", "/", "%", "+", "-", "<", ">", "#", -- Single-char
    "<=", ">=", "==", "~=", ".."                 -- Multi-char
  })
}

--[[
  Pattern Accelerators (Lookup Tables)

  These lookup tables, generated by `makePatternLookup`, provide lightning-fast
  checks for common character properties without needing to call `string.match`
  repeatedly. The tokenizer uses these to quickly determine if a character is,
  for example, a digit, part of an identifier, etc.

  Here are the patterns they are based on:
  - %s: Matches whitespace characters (space, tab, newline, etc.)
  - %d: Matches digit characters (0-9)
  - %a: Matches alphabetic characters (a-z, A-Z)
  - _: The underscore character (special in identifiers)
  - [pattern]: A character set that matches any single character within the set.
               Inside `[]`, `-` denotes a range (e.g., `a-z`), and `^` at the
               start negates the set (`[^%d]` matches non-digits).
               If a character isn't a magic '%' escape or range indicator,
               it matches literally (e.g., `[%a%d_]` matches letters, digits, or underscore).

  Why pre-make these? Tokenization scans every character of the input code,
  making it a performance-sensitive phase. Using these pre-calculated
  lookups makes the fundamental character checks as fast as possible.
--]]
Tokenizer.PATTERNS = {
  SPACE            = makePatternLookup("%s"),
  DIGIT            = makePatternLookup("%d"),
  HEX_DIGIT        = makePatternLookup("[%da-fA-F]"),
  IDENTIFIER       = makePatternLookup("[%a%d_]"),
  IDENTIFIER_START = makePatternLookup("[%a_]")
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
  local TokenizerInstance = setmetatable({}, Tokenizer)

  --// Initialization //--
  TokenizerInstance.code = code

  -- Initialize the pointer to the current character position. Lua uses
  -- 1-based indexing for strings and tables, so we start at the first character.
  TokenizerInstance.curCharPos = 1

  -- Initialize the current character being processed.
  TokenizerInstance.curChar = code:sub(1, 1)

  return TokenizerInstance
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
    error("Internal Tokenizer Error: Attempted to consume past end of stream. Infinite loop detected?")
  end

  local nextCharPos = self.curCharPos + n
  local nextChar    = self.code:sub(nextCharPos, nextCharPos)

  -- Update the tokenizer's state
  self.curCharPos = nextCharPos
  self.curChar    = nextChar

  return nextChar
end

-- Consumes exactly one character, but only if it matches the expected character.
-- Used for consuming specific punctuation like '(' or '=' after identifying
-- a token type that implies the next character must be that specific one.
-- Throws an error if the current character doesn't match the expectation.
function Tokenizer:consumeCharacter(character)
  if self.curChar == character then
    return self:consume(1) -- Match! Consume the character and move on.
  end

  -- No match? Syntax error detected at the lexical level.
  -- (An actual Tokenizer would include line/column info here for better error reporting)
  error("Expected character '" .. character .. "', but found: '" .. tostring(self.curChar) .. "'")
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
    self:isDigit(curChar) -- Starts with a digit (e.g., 123, 1.0)
    or (
      -- Starts with a decimal point followed by a digit (e.g., .5).
      -- This matches patterns like ".5" but not just "."
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
  -- Simple strings start with single or double quotes
  return (curChar == '"' or curChar == "'")
      -- Long strings start with `[` followed by `[` or `=`
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
      self:consume(1) -- Consume the "]"
      local depthMatch = self:calculateDelimiterDepth()
      if self.curChar == "]" and depthMatch == depth then
        -- Consume the closing brackets and return the string
        self:consume(1) -- Consume the "]"
        return self.code:sub(startPos, self.curCharPos - depth - 3)
        -- -3 is to account for the two closing brackets and the last character
      end

    -- Check if it's end of stream
    elseif self.curChar == "" then
      -- End of stream reached without finding the delimiter
      error("Unexpected end of input while searching for ending delimiter")
    end

    -- Consume the current character and move to the next one in the stream.
    self:consume(1)
  end
end

-- Consumes a numeric escape sequence (\ddd) and returns the corresponding character.
-- Expects the current character to be the first digit after the '\'.
-- Validates that the number is within the byte range (0-255).
function Tokenizer:consumeNumericEscapeSequence(firstDigit)
  local numberString = firstDigit

  -- Consume up to two more digits
  for _ = 1, 2 do
    local nextChar = self:lookAhead(1)
    if not self:isDigit(nextChar) then
      -- Stop if the next char isn't a digit
      break
    end
    numberString = numberString .. nextChar -- Append the digit to the string
    self:consume(1) -- Consume the digit
  end

  -- Convert the collected digits to a number
  local number = tonumber(numberString)
  if not number or number > 255 then
    error("escape sequence too large near '\\" .. numberString .. "'")
  end

  -- Return the character corresponding to the number code
  return string.char(number)
end

--// Consumers //--
-- These functions are responsible for consuming a specific type of token
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

  -- Hexadecimal number case
  -- 0[xX][0-9a-fA-F]+
  if self:isHexadecimalNumberPrefix() then
    self:consume(2) -- Consume the "0x" part
    while self:isHexadecimalNumber(self.curChar) do
      self:consume(1)
    end
    return self.code:sub(startPos, self.curCharPos - 1)
  end

  -- [0-9]*
  while self:isDigit(self.curChar) do
    self:consume(1)
  end

  -- Floating point number case
  -- \.[0-9]+
  if self.curChar == "." then
    self:consume(1) -- Consume the "."

    -- Lua allows you to end a number with a decimal point (e.g., "42."),
    -- so this check doesn't expect any digits after the decimal.
    while self:isDigit(self.curChar) do
      self:consume(1)
    end
  end

  -- Exponential (scientific) notation case
  -- [eE][+-]?[0-9]+
  if self.curChar == "e" or self.curChar == "E" then
    self:consume(1) -- Consume the "e" or "E" characters
    if self.curChar == "+" or self.curChar == "-" then
      self:consume(1) -- Consume an optional sign character
    end

    -- Exponent part
    while self:isDigit(self.curChar) do
      self:consume(1)
    end
  end

  return self.code:sub(startPos, self.curCharPos - 1)
end

function Tokenizer:consumeEscapeSequence()
  -- Consume the "\" character
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
  self:consume(1) -- Consume the quote

  while self.curChar ~= delimiter do
    -- Check if it's end of stream
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
  self:consume(1) -- Consume the closing quote

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

    -- Skip it
    self:consume(1)
  end

  local stringValue = self:consumeUntilEndingDelimiter(depth)
  return stringValue
end

function Tokenizer:consumeString()
  if self.curChar == "[" then
    return self:consumeLongString()
  end
  return self:consumeSimpleString()
end

function Tokenizer:consumeOperator()
  local node = self.CONFIG.OPERATOR_TRIE
  local operator

  -- Trie walker
  local index = 0
  while true do
    local character = self:lookAhead(index)
    node = node[character]
    if not node then break end
    operator = node.Value
    index    = index + 1
  end

  if not operator then return end

  -- Consume the operator
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
  local depth = self:calculateDelimiterDepth()
  if self.curChar ~= "[" then
    return self:consumeShortComment()
  end
  self:consumeCharacter("[")
  self:consumeUntilEndingDelimiter(depth)

  -- No return value; the function's effect is modifying the tokenizer's state.
end

function Tokenizer:consumeComment()
  self:consume(2) -- Consume the "--"
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
    -- Consume the sequence of valid identifier characters
    local identifier = self:consumeIdentifier()

    if self.CONFIG.KEYWORDS[identifier] then
      return { TYPE = "Keyword", Value = identifier }
    end

    -- Fallthrough: If it's not a keyword, it's a regular identifier (e.g. a variable).
    return { TYPE = "Identifier", Value = identifier }
  end

  -- Handle numeric literals (decimal, hex, scientific, float)
  if self:isNumberStart() then
    local numberString = self:consumeNumber() -- Consume the whole number sequence
    local numberValue = tonumber(numberString) -- Convert the string to a Lua number
    if numberValue == nil then -- Basic check, tonumber handles many errors but not all contextually
      error("Invalid number format near '" .. numberString .. "'")
    end
    return { TYPE = "Number", Value = numberValue }
  end

  -- Handle string literals (simple or long)
  if self:isString() then
    local stringLiteral = self:consumeString() -- Consume the string contents
    return { TYPE = "String", Value = stringLiteral }
  end

  -- Handle complex literals or special multi-character tokens
  -- Check for vararg "..." before numbers/operators that might start with "."
  if self:isVararg() then
    self:consume(3) -- Consume the "..."
    return { TYPE = "Vararg" }
  end

  -- Attempt to consume a symbolic operator using the trie.
  -- This handles operators like "+", "==", "<=", "..", etc.
  local operator = self:consumeOperator()
  if operator then
    -- If an operator was matched and consumed, return it as a token.
    return { TYPE = "Operator", Value = operator }
  end

  -- If no other token types matched, look up the current character
  -- in the TOKENIZER_CHARACTER_TYPES lookup table. This handles single-character
  -- tokens like punctuation, separators, or other special characters.
  local tokenType = self.CONFIG.PUNCTUATION[curChar]
  if tokenType then
    self:consume(1) -- Consume the character
    return { TYPE = tokenType }
  end

  -- If we reach this point, it means the current character doesn't match
  -- any known token type. This is an error condition, as the character
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

  -- Loop through the character stream as long as there are characters left
  while self.curChar ~= "" do
    -- Get the next token (or nil if whitespace/comment was skipped)
    local token = self:getNextToken()

    -- If `getNextToken` returned a valid token (not nil)...
    if token then
      -- Add the token to our list of results
      -- Note: We don't use table.insert here as this is a very performance-critical
      -- section of the code. Using a simple index is faster for sequential inserts.
      tokens[tokenIndex] = token
      tokenIndex = tokenIndex + 1
    end
  end

  -- Return the list of collected tokens
  return tokens
end

--[[
    ============================================================================
                        (ﾉ◕ヮ◕)ﾉ*:･ﾟ AST FACTORY ONLINE!
    ============================================================================

--]]

--* Parser *--
-- The main object responsible for consuming tokens and building the AST.
local Parser = {}
Parser.__index = Parser -- Set up for method calls via `.`

Parser.CONFIG = {
  --[[
    Who gets solved first in an expression? This table settles all arguments.
    It defines the binding power and associativity of binary operators.
    We use this in our precedence climbing algorithm (`parseBinaryExpression`).

    Format: {left_precedence, right_precedence}

    Key Rules:
      - Higher number = tighter binding (operator gets evaluated sooner).
         Example: `5 + 3 * 2`. `*` (prec 7) has higher precedence than `+` (prec 6),
         so `3 * 2` is evaluated first.
      - If right_precedence > left_precedence: Right-Associative.
         Operators group from right to left.
         Example: `2 ^ 3 ^ 2`. `^` is right-associative ({10, 9}). `3 ^ 2` is grouped first,
         then `2 ^ (3^2)`.
      - If left_precedence >= right_precedence: Left-Associative.
         Operators group from left to right (most common).
         Example: `5 + 3 - 2`. Both `+` and `-` are left-associative ({6, 6}).
         `5 + 3` is grouped first, then `(5+3) - 2`.

    Using distinct left/right precedences is key to handling associativity
    correctly in the precedence climbing algorithm.
  --]]
  PRECEDENCE = {
    ["+"]   = {6, 6},  ["-"]   = {6, 6}, -- Addition/Subtraction
    ["*"]   = {7, 7},  ["/"]   = {7, 7}, -- Multiplication/Division
    ["%"]   = {7, 7},                    -- Modulo
    ["^"]   = {10, 9},                   -- Exponentiation (Right-associative, binds tighter than unary)
    [".."]  = {5, 4},                    -- String Concatenation (Right-associative)
    ["=="]  = {3, 3},  ["~="]  = {3, 3}, -- Equality / Inequality
    ["<"]   = {3, 3},  [">"]   = {3, 3}, -- Less Than / Greater Than
    ["<="]  = {3, 3},  [">="]  = {3, 3}, -- Less Than or Equal / Greater Than or Equal
    ["and"] = {2, 2},                    -- Logical AND (Short-circuiting)
    ["or"]  = {1, 1}                     -- Logical OR (Short-circuiting)
  },

  -- Precedence for all unary operators.
  UNARY_PRECEDENCE = 8,

  -- A set of valid expression node types that can appear on the
  -- left-hand side of an assignment statement.
  LVALUE_NODES = createLookupTable({ "Variable", "IndexExpression" }),

  -- Keywords that explicitly terminate a code block.
  TERMINATION_KEYWORDS = createLookupTable({ "end", "else", "elseif", "until" }),

  -- Dispatches statement-starting keywords to their respective parsing methods.
  KEYWORD_HANDLERS = {
    ["break"]    = "parseBreakStatement",
    ["do"]       = "parseDoStatement",
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
  local ParserInstance = setmetatable({}, Parser)

  --// Initialization //--
  ParserInstance.tokens = tokens
  ParserInstance.currentTokenIndex = 1
  ParserInstance.currentToken = tokens[1]

  -- Initialize the stack for managing variable scopes (local, upvalue, global)
  ParserInstance.scopeStack = {}
  ParserInstance.currentScope = nil -- Pointer to the topmost scope on the stack

  return ParserInstance
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
    error("Internal Parser Error: Attempted to consume past end of stream. Infinite loop detected?")
  end

  -- Advance the token index
  local newTokenIndex = self.currentTokenIndex + n
  local newToken      = self.tokens[newTokenIndex]
  self.currentTokenIndex = newTokenIndex
  self.currentToken      = newToken

  return newToken
end

-- Helper to get a human-readable description of a token for error messages.
-- Returns a string describing the token type and value (if any).
function Parser:getTokenDescription(token)
  if not token then return "<end of file>" end
  if not token.Value then return tostring(token.TYPE) end
  return string.format("%s [%s]", tostring(token.TYPE), tostring(token.Value))
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

-- Consumes the current token, but only if its type AND value match the expectation.
-- Used for consuming specific keywords or characters (like `end`, `(`, `=`).
-- Throws a detailed error with token location if the expectation is not met.
-- This is a strong check for expected syntax elements.
function Parser:consumeToken(tokenType, tokenValue)
  local token = self.currentToken
  -- Check if the current token exists and matches the expected type and value
  if token and token.TYPE == tokenType and token.Value == tokenValue then
    -- Match! Consume this token and exit the function.
    self:consume(1)
    return
  end

  -- If we didn't match, throw a syntax error.
  self:error(
    string.format(
      "Expected %s [%s] token, got: %s",
      tostring(tokenType),
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
    localVariables = {}, -- Used to track local variables declared in this scope
    isFunctionScope = isFunctionScope or false, -- Is this a function's scope?
  }
  table.insert(self.scopeStack, newScope) -- Push the new scope onto the stack
  self.currentScope = newScope -- The new scope is now the active one
  return newScope
end

-- Exits the current scope by popping it from the scope stack.
-- The previous scope on the stack (if any) becomes the `currentScope`.
-- Called when leaving a block, loop body, or function definition.
function Parser:exitScope()
  table.remove(self.scopeStack)
  -- The new `currentScope` is the one now at the top of the stack (or nil if stack is empty)
  self.currentScope = self.scopeStack[#self.scopeStack]
end

--// In-Scope Variable Management //--
-- These functions help track local variable declarations within the current scope.

-- Records that a local variable with `variable` name has been declared in the `currentScope`.
-- Used when parsing `local variable` or function parameters.
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

-- Determines the type of variable reference (Local, Upvalue, or Global)
-- by searching upwards through the scope stack from the `currentScope`.
-- This is a fundamental part of static analysis during parsing to understand
-- how a variable name should be resolved.
-- Returns the type string ("Local", "Upvalue", "Global").
-- Optionally returns the index of the scope where it was found (for Local/Upvalue).
function Parser:getVariableType(variableName)
  local scopeStack = self.scopeStack
  local isUpvalue  = false -- Flag to track if we've crossed a function boundary

  -- Iterate backwards from the current scope up the stack
  for scopeIndex = #scopeStack, 1, -1 do
    local scope = scopeStack[scopeIndex]
    -- Check if the variable is declared as a local in this scope
    if scope.localVariables[variableName] then
      -- Found it! Determine if it's a direct local or an upvalue
      local variableType = (isUpvalue and "Upvalue") or "Local"
      return variableType, scopeIndex -- Return type and the scope index where it was found

    -- If this scope is a function scope, any variable found further out
    -- will be an upvalue relative to subsequent nested function scopes.
    -- We set this flag after checking locals in the current function scope.
    elseif scope.isFunctionScope then
      isUpvalue = true
    end
  end

  -- If the loop finishes without finding the variable in any scope, it's global.
  return "Global"
end

--// Token Checkers //--
-- Lightweight checks to see if a token matches a specific type and/or value
-- without throwing an error if it doesn't match. Useful for `while` loop conditions
-- or `if` checks during parsing.

-- Checks if the given `token` (or `self.currentToken`) is of the specified `tokenType`.
-- This is a generic check for any token type (e.g., "Identifier", "Number", etc.).
function Parser:checkTokenType(tokenType, token)
  token = token or self.currentToken
  return token and token.TYPE == tokenType
end

-- Checks if the current token is a 'Keyword'
-- token with the specified `keyword` value.
function Parser:checkKeyword(keyword)
  local token = self.currentToken
  return token
        and token.TYPE  == "Keyword"
        and token.Value == keyword
end

-- Checks if the current token is the comma character ','.
-- Used frequently in parsing lists (argument lists, variable lists, table elements).
function Parser:isComma()
  local token = self.currentToken
  return token and token.TYPE == "Comma"
end

-- Checks if the current token is a recognized unary operator.
-- Uses the `PARSER_LUA_UNARY_OPERATORS` lookup table.
function Parser:isUnaryOperator()
  local token = self.currentToken
  return token
        and (token.TYPE == "Operator" or token.TYPE == "Keyword")
        and self.CONFIG.UNARY_OPERATORS[token.Value]
end

-- Checks if the current token is a recognized binary operator.
-- Uses the `PARSER_LUA_BINARY_OPERATORS` lookup table.
function Parser:isBinaryOperator()
  local token = self.currentToken
  return token
        and (token.TYPE == "Operator" or token.TYPE == "Keyword")
        and self.CONFIG.BINARY_OPERATORS[token.Value]
end

--// Token Expectation //--
-- These functions check the current token's type or value without consuming it.
-- Useful for making parsing decisions ("Is the next thing an identifier?").

-- Checks if the current token has the expected type. Throws an error otherwise.
function Parser:expectTokenType(expectedType)
  if self:checkTokenType(expectedType) then
    local previousToken = self.currentToken
    self:consume(1) -- Advance to the next token
    return previousToken
  end

  -- No match? Syntax error.
  local actualType = self.currentToken.TYPE
  self:error(
    string.format(
      "Expected a %s, but found %s",
      tostring(expectedType),
      tostring(actualType)
    )
  )
end

-- Checks if the current token is a 'Keyword' type with the expected value.
-- Throws an error otherwise.
function Parser:expectKeyword(keyword)
  local token = self.currentToken

  -- Check if the current token is a keyword and matches the value
  if self:checkKeyword(keyword) then
    self:consume(1) -- Advance to the next token
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
-- Uses the `PARSER_LVALUE_NODE_TYPES` lookup table.
function Parser:isValidAssignmentLvalue(node)
  return node and self.CONFIG.LVALUE_NODES[node.TYPE]
end

--// Parsers //--
-- These functions are responsible for parsing specific syntactic constructs
-- in the token stream and building the corresponding AST nodes.

function Parser:consumeIdentifier()
  local identifierValue = self:expectTokenType("Identifier").Value
  return identifierValue
end

function Parser:consumeVariable()
  local variableName = self:expectTokenType("Identifier").Value
  local variableType = self:getVariableType(variableName)

  return { TYPE = "Variable",
    Name         = variableName,
    VariableType = variableType
  }
end

-- Parses a comma-separated list of identifiers.
-- Example: `a, b, c` -> returns `{"a", "b", "c"}`
-- Consumes the identifier tokens and commas.
function Parser:consumeIdentifierList()
  local identifiers = {}

  -- Loop as long as the current token is an identifier
  while self.currentToken and self.currentToken.TYPE == "Identifier" do
    -- Add the identifier's value (name) to the list
    table.insert(identifiers, self.currentToken.Value)
    self:consume(1) -- Consume the identifier token
    -- Check if the current token is a comma. If not, the list ends after the current identifier.
    if not self:isComma() then break end
    self:consume(1) -- Consume the comma and continue the loop
  end

  return identifiers
end

-- Parses a parameter list within a function definition.
-- Example: `(a, b, ...)`
-- Handles regular named parameters and the optional vararg token `...`.
-- Consumes the entire parameter list syntax, including parentheses and commas.
-- Returns a list of parameter names and a boolean indicating if vararg was present.
function Parser:consumeParameters()
  self:consumeToken("LeftParen") -- Expect and consume the opening parenthesis

  local parameters = {} -- List of parameter names
  local isVararg = false -- Flag for varargs

  -- Loop as long as the current token is NOT the closing parenthesis
  while self.currentToken and not self:checkTokenType("RightParen") do
    -- Check for regular named parameters (must be identifiers)
    if self.currentToken.TYPE == "Identifier" then
      -- Add the parameter name to the list
      table.insert(parameters, self.currentToken.Value)
      self:consume(1) -- Consume the identifier token

    -- Check for the vararg token "..."
    elseif self.currentToken.TYPE == "Vararg" then
      isVararg = true -- Set the vararg flag
      self:consumeToken("Vararg") -- Expect and consume the "..." token
      -- According to Lua grammar, "..." must be the last parameter.
      break -- Exit the loop after consuming vararg

    -- If it's neither an identifier nor vararg, it's a syntax error.
    else
       self:error(
        "Expected parameter name or '...' in parameter list, but found: " ..
        tostring(self.currentToken.TYPE)
      )
    end

    -- After a parameter, we expect a comma, unless it's the last parameter before the closing parenthesis.
    -- Check if the current token is a comma. If not, break the loop (assuming it's the closing paren).
    if not self:isComma() then break end
    self:consume(1) -- Consume the comma

  end -- Loop ends when ')' is encountered or after consuming '...'

  self:consumeToken("RightParen") -- Expect and consume the closing parenthesis
  return parameters, isVararg
end

-- Parses a table index expression, e.g., `table.key` or `table["key"]`.
-- `currentExpression` is the AST node representing the table being accessed.
-- Returns an AST node representing the index access.
function Parser:consumeIndexExpression(currentExpression)
  local isPrecomputed = true
  local indexExpression

  -- table.key syntax
  if self:checkTokenType("Dot") then
    self:consume(1) -- Consume the '.' character
    local identifier = self:consumeIdentifier()
    indexExpression = { TYPE = "StringLiteral", Value = identifier }

  -- table["key"] syntax
  else
    self:expectTokenType("LeftBracket") -- Expect and consume the '[' character
    indexExpression = self:consumeExpression()
    isPrecomputed   = false
    if not indexExpression then
      self:error("Expected an expression inside brackets for table index")
    end
    self:expectTokenType("RightBracket")
  end

  -- Create the AST node for the table index access.
  return { TYPE = "IndexExpression",
    Base          = currentExpression,
    Index         = indexExpression,
    IsPrecomputed = isPrecomputed
    -- NOTE: `IsPrecomputed` is not used in the compiler, it's present to make
    -- third-party tools easier to build (e.g., linters, formatters).
  }
end

function Parser:consumeTable()
  self:consumeToken("LeftBrace") -- Consume the "{"

  local implicitKeyCounter = 1
  local tableElements      = {}

  -- Loop through tokens until we find the closing "}"
  while self.currentToken and not self:checkTokenType("RightBrace") do
    local key, value
    local isImplicitKey = false

    -- Determine which type of table field we are parsing.
    if self:checkTokenType("LeftBracket") then
      -- Explicit key in brackets, e.g., `[1+2] = "value"`.
      -- [<expression>] = <expression>
      self:consume(1) -- Consume "["
      key = self:consumeExpression()
      self:expectTokenType("RightBracket")
      self:expectTokenType("Equals")
      value = self:consumeExpression()

    elseif self:checkTokenType("Identifier") and
           self:checkTokenType("Equals", self:lookAhead(1)) then
      -- Identifier key, e.g., `name = "value"`
      -- This is syntatic sugar for `["name"] = "value"`.
      -- <identifier> = <expression>
      key = { TYPE  = "StringLiteral", Value = self.currentToken.Value }
      self:consume(1) -- Consume the identifier
      self:consume(1) -- Consume the "="
      value = self:consumeExpression()

    else
      -- Implicit numeric key, e.g. `"value1", MY_VAR, 42`
      -- This is syntatic sugar for `[1] = "value1", [2] = MY_VAR, [3] = 42`.
      -- <expression>
      isImplicitKey = true
      key = { TYPE = "NumericLiteral", Value = implicitKeyCounter }
      implicitKeyCounter = implicitKeyCounter + 1
      value = self:consumeExpression()
    end

    -- Create the AST node for this table element.
    local element = { TYPE = "TableElement",
      Key           = key,
      Value         = value,
      IsImplicitKey = isImplicitKey
    }

    table.insert(tableElements, element)

    -- Table elements can be separated by "," or ";". If no separator is
    -- found, we assume it's the end of the table definition.
    if not (self:checkTokenType("Comma") or self:checkTokenType("Semicolon")) then
      break
    end
    self:consume(1) -- Consume the separator.
  end

  self:consumeToken("RightBrace") -- Consume the "}" symbol

  return { TYPE = "TableConstructor",
    Elements = tableElements
  }
end

function Parser:consumeFunctionCall(currentExpression, isMethodCall)
  local currentToken     = self.currentToken
  local currentTokenType = currentToken.TYPE
  local arguments

  -- Explicit call with parentheses: `f(a, b)`
  if currentTokenType == "LeftParen" then
    self:consume(1) -- Consume the left parenthesis '('
    arguments = self:consumeExpressions()
    self:consumeToken("RightParen") -- Expect and consume the right parenthesis ')'

  -- Implicit call with a string: `print "hello" `
  elseif currentTokenType == "String" then
    self:consume(1) -- Consume the string
    arguments = { {
      TYPE  = "StringLiteral",
      Value = currentToken.Value
    } }

  -- Implicit call with a table: `f {1, 2, 3} `
  elseif currentTokenType == "LeftBrace" then
    arguments = { self:consumeTable() }
  end

  return { TYPE = "FunctionCall",
    Callee       = currentExpression,
    Arguments    = arguments,
    IsMethodCall = isMethodCall and true,
  }
end

function Parser:consumeMethodCall(currentExpression)
  self:consumeToken("Colon") -- Expect and consume the colon (':')
  local methodName = self:consumeIdentifier()

  -- Convert the `table:method` part to an AST node
  local methodIndexNode = {
    TYPE = "IndexExpression",
    Base = currentExpression,
    Index = { TYPE = "StringLiteral", Value = methodName },
  }

  -- Consume the function call and mark it as a method call
  return self:consumeFunctionCall(methodIndexNode, true)
end

function Parser:consumeOptionalSemicolon()
  if self:checkTokenType("Semicolon") then
    self:consume(1)
  end
end

--// Expression Parsers //--
function Parser:parsePrimaryExpression()
  local currentToken = self.currentToken
  if not currentToken then return end

  local tokenType  = currentToken.TYPE
  local tokenValue = currentToken.Value

  if tokenType == "Number" then
    self:consume(1)
    return { TYPE = "NumericLiteral", Value = tokenValue }
  elseif tokenType == "String" then
    self:consume(1)
    return { TYPE = "StringLiteral", Value = tokenValue }
  elseif tokenType == "Vararg" then
    self:consume(1)
    return { TYPE = "VarargExpression" }
  elseif tokenType == "Keyword" then
    -- Nil literal: `nil`.
    if tokenValue == "nil" then
      self:consume(1) -- Consume 'nil'.
      return { TYPE = "NilLiteral" }

    -- Boolean literals: `true` or `false`.
    elseif tokenValue == "true" or tokenValue == "false" then
      self:consume(1) -- Consume 'true' or 'false'.
      return { TYPE = "BooleanLiteral", Value = (tokenValue == "true") }

    -- Anonymous function, e.g., `function(arg1) ... end`.
    elseif tokenValue == "function" then
      self:consume(1) -- Consume 'function'
      local parameters, isVararg = self:consumeParameters()
      local body = self:parseCodeBlock(true, parameters)
      self:expectKeyword("end")
      return { TYPE = "FunctionExpression",
        Body       = body,
        Parameters = parameters,
        IsVararg   = isVararg
      }
    end

  -- Variable, e.g., `MY_VAR`.
  elseif tokenType == "Identifier" then
    return self:consumeVariable()

  -- Parenthesized expression, e.g., `(a + b)`.
  elseif tokenType == "LeftParen" then
    self:consume(1) -- Consume '('
    local expression = self:consumeExpression()
    self:expectTokenType("RightParen") -- Expect and consume ')'
    if not expression then
      self:error("Expected an expression inside parentheses")
    end

    -- The "ParenthesizedExpression" node is NOT just for operator precedence.
    -- In Lua, parentheses also force a multi-return expression to adjust to
    -- a single value. e.g., `a, b = f()` is different from `a, b = (f())`.
    -- Therefore, we must keep this wrapper node in the AST.
    return { TYPE = "ParenthesizedExpression",
      Expression = expression
    }

  -- Table constructor, e.g., `{ 42, "string", ["my"] = variable }`
  elseif tokenType == "LeftBrace" then
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

    local currentTokenType = currentToken.TYPE
    if currentTokenType == "LeftParen" then -- Function call
      -- <expression>(<args>)
      primaryExpression = self:consumeFunctionCall(primaryExpression, false)

      -- NOTE: In Lua, a function call may be considered ambiguous
      -- without a semicolon, which will throw an error.
      -- TLC does not enforce this as the Parser does not have line/column info.
    elseif currentTokenType == "Dot" or currentTokenType == "LeftBracket" then -- Table access
      -- <expression>.<identifier> | <expression>[<expr>]
      primaryExpression = self:consumeIndexExpression(primaryExpression)
    elseif currentTokenType == "Colon" then -- Method call
      -- <expression>:<identifier>(<args>)
      primaryExpression = self:consumeMethodCall(primaryExpression)
    elseif currentTokenType == "String" or currentTokenType == "LeftBrace" then -- Implicit function call
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

  return { TYPE = "UnaryOperator",
    Operator = operator.Value,
    Operand  = expression
  }
end

function Parser:parseBinaryExpression(minPrecedence)
  -- <binary> ::= <unary> <binary_operator> <binary> | <unary>
  local expression = self:parseUnaryOperator() -- <unary>
  if not expression then return end

  -- [<binary_operator> <binary>]
  while true do
    local operatorToken = self.currentToken
    local precedence = operatorToken and self.CONFIG.PRECEDENCE[operatorToken.Value]
    if not self:isBinaryOperator() or precedence[1] <= minPrecedence then
      break
    end

    -- The <binary operator> <binary> part itself
    local nextToken = self:consume(1) -- Advance to and consume the operator
    if not nextToken then self:error("Unexpected end") end

    local right = self:parseBinaryExpression(precedence[2])
    if not right then self:error("Unexpected end") end

    expression = { TYPE = "BinaryOperator",
      Operator = operatorToken.Value,
      Left     = expression,
      Right    = right
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

  -- function <ident>(<params>) <body> end
  if self:checkKeyword("function") then
    self:consumeToken("Keyword", "function")
    local functionName = self:consumeIdentifier()
    local parameters, isVararg = self:consumeParameters()
    self:declareLocalVariable(functionName)
    local body = self:parseCodeBlock(true, parameters)
    self:expectKeyword("end")
    return { TYPE = "LocalFunctionDeclaration",
      Name = functionName,
      Body = {
        TYPE       = "FunctionExpression",
        Body       = body,
        Parameters = parameters,
        IsVararg   = isVararg
      }
    }
  else
    -- <ident_list> [= <expr_list>]
    local variables = self:consumeIdentifierList()
    local initializers = {}

    -- Check for optional expressions.
    if self:checkTokenType("Equals") then
      self:consume(1)
      initializers = self:consumeExpressions()
    end

    self:declareLocalVariables(variables)
    return { TYPE = "LocalDeclarationStatement",
      Variables    = variables,
      Initializers = initializers
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

  return { TYPE = "WhileStatement",
    Condition = condition,
    Body      = body
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

  return { TYPE = "RepeatStatement",
    Body      = body,
    Condition = condition
  }
end

function Parser:parseDoStatement()
  -- do <body> end
  self:consumeToken("Keyword", "do")
  local body = self:parseCodeBlock()
  self:expectKeyword("end")

  return { TYPE = "DoStatement", Body = body }
end

function Parser:parseReturnStatement()
  -- return <expr_list>
  self:consumeToken("Keyword", "return")
  local expressions = self:consumeExpressions()

  return { TYPE = "ReturnStatement", Expressions = expressions }
end

function Parser:parseBreakStatement()
  -- break
  self:consumeToken("Keyword", "break")
  return { TYPE = "BreakStatement" }
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
    -- First branch: the initial "if"
    { TYPE = "IfClause",
      Condition = ifCondition,
      Body      = ifBody
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
    local ifClause = { TYPE = "IfClause",
      Condition = condition,
      Body      = body
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

  return { TYPE = "IfStatement",
    Clauses    = clauses,
    ElseClause = elseClause
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
  if self:checkTokenType("Comma") or self:checkKeyword("in") then
    -- It's a generic 'for' loop: `for var1, var2, ... in expr1, expr2, ... do ... end`.
    -- Even though the generic for loop allows infinite expressions after `in`,
    -- the Lua VM only uses the first three (the generator, state, and control).

    -- Parse the list of iterator variables.
    local iteratorVariables = { variableName }
    while self:checkTokenType("Comma") do
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

    return { TYPE = "ForGenericStatement",
      Iterators   = iteratorVariables,
      Expressions = expressions,
      Body        = body
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

  return {
    TYPE     = "ForNumericStatement",
    Variable = variableName,
    Start    = startExpr,
    End      = limitExpr,
    Step     = stepExpr,
    Body     = body
  }
end

function Parser:parseFunctionDeclaration()
  -- function <name>[.<field>]*[:<method>](<params>) <body> end
  self:consumeToken("Keyword", "function")

  -- Parse the base function name (e.g., `myFunc` in `myFunc.new`)
  local expression = self:consumeVariable()

  -- This loop handles `.field` and `:method` parts
  local isMethodDeclaration = false
  while self.currentToken do
    local isDot   = self:checkTokenType("Dot")
    local isColon = self:checkTokenType("Colon")

    if not (isDot or isColon) then
      break -- The name chain has ended.
    end

    self:consume(1) -- Consume the `.` or `:`
    local fieldName = self:consumeIdentifier()
    expression = {
      TYPE  = "IndexExpression",
      Base  = expression,
      Index = { TYPE  = "StringLiteral", Value = fieldName }
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

  -- Instead of having a separate node type for non-local function declarations,
  -- we use a "AssignmentStatement" node that assigns a "Function" node to a variable or table index.
  -- Behaviorally, `function <name>[.<field>]*[:<method>](<params>) <body> end` is equivalent to
  -- `<name>[.<field>]*[.method] = function([self,] <params>) <body> end`.
  -- This will make the code generator much smaller and simpler.
  return {
    TYPE    = "AssignmentStatement",
    LValues = { expression },
    Expressions = { {
      TYPE       = "FunctionExpression",
      Body       = body,
      Parameters = parameters,
      IsVararg   = isVararg
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
                 tostring(nextLValue and nextLValue.TYPE or "nil"))
    end

    table.insert(lvalues, nextLValue)
  end

  -- Now that we have all the lvalues, we expect the `=` sign.
  self:expectTokenType("Equals")
  local expressions = self:consumeExpressions()

  return {
    TYPE        = "AssignmentStatement",
    LValues     = lvalues,
    Expressions = expressions
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
  if expression.TYPE == "FunctionCall" then
    return { TYPE = "CallStatement",
      Expression = expression
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
  self:error("Invalid statement: syntax error near '" .. expression.TYPE ..
             "'. Only function calls and assignments can be statements.")
end

--// CODE BLOCK PARSERS //--

-- Parses the next statement in a code block.
-- This function acts as a dispatcher, determining which specific parsing
-- function to call based on the current token.
function Parser:getNextNode()
  local node
  if self:checkTokenType("Keyword") then
    local keyword = self.currentToken.Value

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
  local node = { TYPE = "Block", Statements = {} }
  local nodeList = node.Statements

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
    TYPE = "Program",
    Body = blockNode
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
  MIN_STACK_SIZE = 2,   -- Registers 0/1 are always valid
  MAX_REGISTERS  = 250, -- 200 variables, 50 temp (5 reserved for safety)
  SETLIST_MAX    = 50,  -- Max number of table elements for a single SETLIST instruction

  MULTIRET_NODES = createLookupTable({"FunctionCall", "VarargExpression"}),
  CONTROL_FLOW_OPERATOR_LOOKUP = createLookupTable({"and", "or"}),
  UNARY_OPERATOR_LOOKUP = { ["-"] = "UNM", ["#"] = "LEN", ["not"] = "NOT" },
  ARITHMETIC_OPERATOR_LOOKUP = {
    ["+"] = "ADD", ["-"] = "SUB",
    ["*"] = "MUL", ["/"] = "DIV",
    ["%"] = "MOD", ["^"] = "POW"
  },

  -- Format: [operator] = {opname, opposite}
  COMPARISON_INSTRUCTION_LOOKUP = {
    ["=="] = {"EQ", 1}, ["~="] = {"EQ", 0},
    ["<"]  = {"LT", 1}, [">"]  = {"LT", 1},
    ["<="] = {"LE", 1}, [">="] = {"LE", 1}
  },

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
  assert(ast.TYPE == "Program", "Expected 'ast' to be a 'Program' node, got: " .. ast.TYPE)

  --// Initialization //
  local self = setmetatable({}, CodeGenerator)
  self.ast   = ast
  self.proto = nil

  -- Scope-related fields declaration
  self.scopes       = {}
  self.currentScope = nil
  self.stackSize    = 0

  self.breakControlList = nil

  return self
end

--// Scope Management //--
function CodeGenerator:enterScope(isFunctionScope)
  local previousScope = self.currentScope
  local newScope = {
    locals      = {},
    parentScope = previousScope,
    isFunction  = (isFunctionScope and true) or false,
  }

  if previousScope then
    previousScope.stackSize = self.stackSize
  end
  table.insert(self.scopes, newScope)

  self.currentScope = newScope
  if isFunctionScope then
    self.stackSize = 0
  end

  return newScope
end

function CodeGenerator:leaveScope()
  local currentScope = self.currentScope
  local scopes       = self.scopes
  if not currentScope then
    error("Compiler: No scope to leave!")
  elseif currentScope.isFunction then
    -- Mess.
    if self.stackSize ~= 0 then
      local variableCount = 0
      for _ in pairs(currentScope.locals) do
        variableCount = variableCount + 1
      end

      if self.stackSize > variableCount then
        error(
          string.format(
            "Compiler: Register leak detected when leaving function scope! "
            .. "Expected at most %d registers in use, but found %d.",
            variableCount,
            self.stackSize
          )
        )
      end
    end
  end

  table.remove(scopes)

  currentScope = currentScope.parentScope
  self.currentScope = currentScope
  self.stackSize = (currentScope and currentScope.stackSize) or 0
end

--// Variable Management //--
function CodeGenerator:declareLocalVariable(varName, register)
  register = register or self:allocateRegister()
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

  error("Compiler: Could not find variable '" .. varName .. "' in any scope.")
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
    constantLookup = {},
    upvalueLookup  = {},
  }

  return proto
end

--// Register/Constant/Upvalue Management //--
function CodeGenerator:allocateRegister()
  local previousStackSize = self.stackSize
  if previousStackSize >= self.CONFIG.MAX_REGISTERS then
    error(
      string.format(
        "Compiler: Register overflow! exceeded maximum of %d registers.",
        self.CONFIG.MAX_REGISTERS
      )
    )
  end

  local stackSize = previousStackSize + 1
  self.stackSize = stackSize
  if stackSize >= self.proto.maxStackSize then
    self.proto.maxStackSize = stackSize + 1
  end

  return previousStackSize
end

function CodeGenerator:allocateRegisters(count)
  if count <= 0 then return end -- Just ignore non-positive counts.

  for _ = 1, count do
    self:allocateRegister()
  end
end

function CodeGenerator:freeRegister()
  local stackSize = self.stackSize
  if stackSize < 0 then
    error("Compiler: Stack underflow! cannot free below minimum of 0 registers.")
  end

  self.stackSize = stackSize - 1
end

function CodeGenerator:freeRegisters(count)
  if count <= 0 then return end -- Just ignore non-positive counts.

  local stackSize = self.stackSize - count
  if stackSize < 0 then
    error("Compiler: Stack underflow! cannot free below minimum of 0 registers.")
  end

  self.stackSize = stackSize
end

-- A helper function for freeing RK operands.
function CodeGenerator:freeIfRegister(rkOperand)
  -- Is it a register?
  if rkOperand >= 0 then
    self:freeRegister()
  end

  -- If it's below 0, it's a constant index, do nothing.
end

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
  upvalueIndex = #upvalues - 1 -- Upvalue indices are zero-based.
  upvalueLookup[varName] = upvalueIndex
  return upvalueIndex
end

--// Instruction/Label Management //--

-- TODO: The label/jump logic is a bit messy, and it cannot be used
--       in most places, is there a better way to mark jumps and their
--       locations?

function CodeGenerator:makeLabel()
  return { jumps = {} }
end

function CodeGenerator:emitInstruction(opname, a, b, c)
  local instruction = { opname, a, b, c or 0 }
  table.insert(self.proto.code, instruction)
  return instruction
end

function CodeGenerator:emitJump(label)
  local instruction = { "JMP", 0, 0, 0}
  table.insert(self.proto.code, instruction)
  local instructionIndex = #self.proto.code

  return table.insert(label.jumps, { instruction, instructionIndex })
end

function CodeGenerator:patchLabelJumpsToHere(label)
  local currentPC = #self.proto.code
  for _, jump in ipairs(label.jumps) do
    local instruction      = jump[1]
    local instructionIndex = jump[2]
    instruction[3] = currentPC - instructionIndex
  end
end

function CodeGenerator:patchBreakJumpsToHere()
  if not self.breakControlList then return end

  local currentPC = #self.proto.code
  for _, breakTable in ipairs(self.breakControlList) do
    local instruction = breakTable.instruction
    local index       = breakTable.index
    instruction[3] = currentPC - index
  end

  self.breakControlList = nil
end

--// Wrappers //--
function CodeGenerator:withTemporaryRegister(callback)
  local register = self:allocateRegister()
  local result   = callback(register)
  self:freeRegister()
  return result
end

function CodeGenerator:withTwoTemporaryRegisters(callback)
  local firstRegister  = self:allocateRegister()
  local secondRegister = self:allocateRegister()
  local result         = callback(firstRegister, secondRegister)
  self:freeRegisters(2)
  return result
end

function CodeGenerator:breakable(callback)
  local previousBreakControlList = self.breakControlList
  self.breakControlList = {}

  callback()
  self:patchBreakJumpsToHere()

  local breakControlList = self.breakControlList
  self.breakControlList = previousBreakControlList

  return breakControlList
end

--// Auxiliary/Helper Methods //--

-- Returns either "Upvalue", "Local", or "Global".
-- TODO: We shouldn't do that at compiling phase.
--       Is there a better way to do that?
function CodeGenerator:getUpvalueType(variableName)
  local scope = self.currentScope
  local isUpvalue = false
  while scope do
    if scope.locals[variableName] then
      return (isUpvalue and "Upvalue") or "Local"
    elseif scope.isFunction then
      isUpvalue = true
    end
    scope = scope.parentScope
  end

  return "Global"
end

function CodeGenerator:isMultiReturnNode(node)
  return node and self.CONFIG.MULTIRET_NODES[node.TYPE]
end

function CodeGenerator:isMultiReturnList(expressions)
  if #expressions == 0 then return false end

  local lastExpression = expressions[#expressions]
  return self:isMultiReturnNode(lastExpression)
end

function CodeGenerator:isTailCall(expressions)
  if #expressions ~= 1 then return false end

  local expression = expressions[1]
  return expression.TYPE == "FunctionCall"
end

-- Splits table's elements into implicit and explicit ones.
function CodeGenerator:splitTableElements(elements)
  local implicitElems = {}
  local explicitElems = {}
  for _, element in ipairs(elements) do
    if element.IsImplicitKey then
      table.insert(implicitElems, element)
    else
      table.insert(explicitElems, element)
    end
  end

  return implicitElems, explicitElems
end

-- Used in methods like `processAssignmentStatement` to set
-- the value of a register to another register's value.
function CodeGenerator:setRegisterValue(node, copyFromRegister)
  local nodeType = node.TYPE

  if nodeType == "Variable" then
    local variableType = node.VariableType
    local variableName = node.Name
    if variableType == "Local" then
      local variableRegister = self:findVariableRegister(variableName)
      -- OP_MOVE [A, B]    R(A) := R(B)
      self:emitInstruction("MOVE", variableRegister, copyFromRegister)
    elseif variableType == "Upvalue" then
      -- OP_SETUPVAL [A, B]    UpValue[B] := R(A)
      self:emitInstruction("SETUPVAL", copyFromRegister, self:findOrCreateUpvalue(variableName))
    elseif variableType == "Global" then
      -- OP_SETGLOBAL [A, Bx]    Gbl[Kst(Bx)] := R(A)
      self:emitInstruction("SETGLOBAL", copyFromRegister, self:findOrCreateConstant(variableName))
    end
    return
  elseif nodeType == "IndexExpression" then
    -- self:withTwoTemporaryRegisters(function(baseRegister, indexRegister)
    local baseNode = node.Base
    local indexNode = node.Index

    local baseRegister  = self:processConstantOrExpression(baseNode)
    local indexRegister = self:processConstantOrExpression(indexNode)

    -- OP_SETTABLE [A, B, C]    R(A)[R(B)] := R(C)
    self:emitInstruction("SETTABLE", baseRegister, indexRegister, copyFromRegister)
    self:freeIfRegister(baseRegister)
    self:freeIfRegister(indexRegister)

    -- end)
    return
  end

  error("Compiler: Unsupported lvalue type in setRegisterValue: " .. nodeType)
end

-- TODO: Make it much cleaner
-- NOTE: `isLastElementImplicit` is needed to prevent false multirets
--        in case there's an explicit node element that goes just after
--        the supposed multiret implicit element. Like here:
--        `{ 1, 2, get_three_four(), a = 2 }`. The table should look like this:
--        `{ 1, 2, 3, a = 2 }`, and NOT this: `{ 1, 2, 3, 4, a = 2 }`
function CodeGenerator:processTablePage(
  register,
  implicitElems,
  page,
  pageAmount,
  isLastElemImplicit
)
  local isLastPage = (page == pageAmount)
  local startIndex = (page - 1) * self.CONFIG.SETLIST_MAX + 1
  local endIndex = math.min(page * self.CONFIG.SETLIST_MAX, #implicitElems)

  local lastPageElement       = implicitElems[endIndex]
  local lastElementValue      = lastPageElement.Value
  local isLastElementMultiRet = self:isMultiReturnNode(lastElementValue)

  for elementIndex = startIndex, endIndex do
    local element = implicitElems[elementIndex]
    local elementValue  = element.Value
    local isLastElement = elementIndex == endIndex
    local isMultireturn = isLastElement and isLastPage and isLastElementMultiRet
    local returnCount   = (isMultireturn and -1) or 1

    self:processExpressionNode(elementValue, nil, returnCount)
  end

  local currentPageRegisters = endIndex - startIndex + 1
  local currentPageResults   = currentPageRegisters
  if isLastElemImplicit and isLastPage and isLastElementMultiRet then
    -- B = 0: Doesn't have a fixed amount of keys (multiret)
    currentPageResults = 0
  end

  -- OP_SETLIST [A, B, C]    R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B
  self:emitInstruction("SETLIST", register, currentPageResults, page)
  self:freeRegisters(currentPageRegisters)
end

--// Expression Handlers //--

-- Generic processor for both string and numeric literals.
function CodeGenerator:processLiteral(node, register)
  local nodeType = node.TYPE
  if nodeType == "NilLiteral" then

    -- OP_LOADNIL [A, B]    R(A) := ... := R(B) := nil
    self:emitInstruction("LOADNIL", register, register, 0)
  elseif nodeType == "BooleanLiteral" then
    local value = (node.Value and 1) or 0

    -- OP_LOADBOOL [A, B, C]    R(A) := (Bool)B; if (C) pc++
    self:emitInstruction("LOADBOOL", register, value, 0)
  elseif nodeType == "NumericLiteral" or nodeType == "StringLiteral" then
    local value         = node.Value
    local constantIndex = self:findOrCreateConstant(value)

    -- OP_LOADK [A, Bx]    R(A) = Kst(Bx)
    self:emitInstruction("LOADK", register, constantIndex, 0)
  else
    error("Compiler: Unsupported literal type: " .. tostring(nodeType))
  end

  return register
end

function CodeGenerator:processParenthesizedExpression(node, register)
  local expression = node.Expression
  return self:processExpressionNode(expression, register)
end

function CodeGenerator:processBinaryOperator(node, register)
  local operator = node.Operator
  local left     = node.Left
  local right    = node.Right

  -- Simple arithmetic operators (+, -, /, *, %, ^)
  if self.CONFIG.ARITHMETIC_OPERATOR_LOOKUP[operator] then
    local opcode = self.CONFIG.ARITHMETIC_OPERATOR_LOOKUP[operator]

    -- NOTE: Do not move `self:processExpressionNode(left, register)` into
    -- `self:withTemporaryRegister` as it would cause register allocation issues.
    -- (For example, if left expression is a function call, it might use more than
    -- one register, which would make it allocate more registers AFTER the temporary register
    -- which shouldn't happen, the simple solution is to move it out of the temporary register scope.)
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
    self:emitInstruction("TEST", register, 0, isConditionTrue)
    local label = self:makeLabel()
    self:emitJump(label)
    self:processExpressionNode(right, register)
    self:patchLabelJumpsToHere(label)

    return register

  -- Comparison operators (==, ~=, <, >, <=, >=)
  elseif self.CONFIG.COMPARISON_INSTRUCTION_LOOKUP[operator] then
    local leftReg = self:processExpressionNode(left, register)
    self:withTemporaryRegister(function(tempRegister)
      local rightReg     = self:processExpressionNode(right, tempRegister)
      local nodeOperator = self.CONFIG.COMPARISON_INSTRUCTION_LOOKUP[operator]
      local instruction, flag = nodeOperator[1], nodeOperator[2]

      local flip = (operator == ">" or operator == ">=")
      local b, c = leftReg, rightReg
      if flip then b, c = rightReg, leftReg end

      self:emitInstruction(instruction, flag, b, c)
      self:emitInstruction("JMP", 0, 1)

      -- OP_LOADBOOL [A, B, C]    R(A) := (Bool)B; if (C) pc++
      self:emitInstruction("LOADBOOL", register, 0, 1)
      self:emitInstruction("LOADBOOL", register, 1, 0)
    end)

    return register

  -- String concatenation (..)
  elseif operator == ".." then
    -- NOTE: Do not wrap these two calls in `withTemporaryRegister`,
    -- as it would cause register allocation issues (see the comment above).
    local leftRegister  = self:processExpressionNode(left)
    local rightRegister = self:processExpressionNode(right)

    -- OP_CONCAT [A, B, C]    R(A) := R(B).. ... ..R(C)
    self:emitInstruction("CONCAT", register, leftRegister, rightRegister)
    self:freeRegisters(2)

    return register
  end

  error("Compiler: Unsupported binary operator: " .. operator)
end

function CodeGenerator:processUnaryOperator(node, register)
  local operator = node.Operator
  local operand  = node.Operand
  local opname   = self.CONFIG.UNARY_OPERATOR_LOOKUP[operator]

  -- TODO: Allocate a new register?
  self:processExpressionNode(operand, register)
  self:emitInstruction(opname, register, register)

  return register
end

-- TODO: Make this function less messy.
function CodeGenerator:processTableConstructor(node, register)
  local elements    = node.Elements
  local lastElement = node.Elements[#node.Elements]
  local isLastElementImplicit = lastElement and lastElement.IsImplicitKey
  local implicitElems, explicitElems = self:splitTableElements(elements)

  -- TODO: We're doing wrong calculation, fix it later.
  local sizeB = math.min(#implicitElems, 100)
  local sizeC = math.min(#explicitElems, 100)

  -- OP_NEWTABLE [A, B, C]    R(A) := {} (size = B,C)
  self:emitInstruction("NEWTABLE", register, sizeB, sizeC)

  for _, elem in ipairs(explicitElems) do
    -- self:withTwoTemporaryRegisters(function(keyRegister, valueRegister)
    -- NOTE: Do not use `withTwoTemporaryRegisters` here.
    local keyRegister   = self:processExpressionNode(elem.Key)
    local valueRegister = self:processExpressionNode(elem.Value)

    -- OP_SETTABLE [A, B, C]    R(A)[R(B)] := R(C)
    self:emitInstruction("SETTABLE", register, keyRegister, valueRegister)
    self:freeRegisters(2)
    -- end)
  end

  local pageAmount = math.ceil(#implicitElems / self.CONFIG.SETLIST_MAX)
  for page = 1, pageAmount do
    self:processTablePage(
      register,
      implicitElems,
      page,
      pageAmount,
      isLastElementImplicit
    )
  end

  return register
end

function CodeGenerator:processVariable(node, register)
  local varName = node.Name
  local varType = node.VariableType -- "Local" / "Upvalue" / "Global"

  if varType == "Local" then
    local variable = self:findVariableRegister(varName)

    -- OP_MOVE [A, B]    R(A) := R(B)
    self:emitInstruction("MOVE", register, variable)
  elseif varType == "Global" then
    local constantIndex = self:findOrCreateConstant(varName)

    -- OP_GETGLOBAL [A, Bx]    R(A) := Gbl[Kst(Bx)]
    self:emitInstruction("GETGLOBAL", register, constantIndex)
  elseif varType == "Upvalue" then
    local upvalueIndex = self:findOrCreateUpvalue(varName)

    -- OP_GETUPVAL [A, B]    R(A) := UpValue[B]
    self:emitInstruction("GETUPVAL", register, upvalueIndex)
  end

  return register
end

function CodeGenerator:processFunctionCall(node, register, resultRegisters)
  local callee       = node.Callee
  local arguments    = node.Arguments
  local isMethodCall = node.IsMethodCall

  if isMethodCall then
    local calleeExpressionIndex = callee.Index
    local calleeExpressionBase  = callee.Base

    self:processExpressionNode(calleeExpressionBase, register)
    self:allocateRegister() -- Use for `self` arg, will get free'd later.

    self:withTemporaryRegister(function(calleeIndexRegister)
      calleeIndexRegister = self:processConstantOrExpression(calleeExpressionIndex, calleeIndexRegister)

      -- OP_SELF [A, B, C]    R(A+1) := R(B) R(A) := R(B)[RK(C)]
      self:emitInstruction("SELF", register, register, calleeIndexRegister)
    end)
  else
    self:processExpressionNode(callee, register)
  end

  local argRegisters, numArgs = self:processExpressionList(arguments)
  local isArgsMultiRet = self:isMultiReturnList(arguments)
  if isMethodCall then
    -- Make sure the `CALL` instruction captures the "self" (table) param.
    argRegisters = argRegisters + 1
    if not isArgsMultiRet then
      -- Check if it's not multiret, as multiret already includes all args.
      -- Increasing it by one would turn multiret (-1 + 1 = 0) into a fixed count (0 + 1 = 1)
      -- which will make it capture only one argument instead of all of them.
      numArgs = numArgs + 1
    end
  end

  local returnValueCount = (resultRegisters and resultRegisters + 1) or 2

  -- OP_CALL [A, B, C]    R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
  self:emitInstruction("CALL", register, numArgs + 1, returnValueCount)
  self:freeRegisters(argRegisters)
  self:allocateRegisters(returnValueCount - 2) -- One register is already in 'register'

  if resultRegisters then
    return register, register + returnValueCount - 2
  end
  return register
end

function CodeGenerator:processFunctionExpression(node, register)
  self:processFunction(node, register)
  return register
end

function CodeGenerator:processIndexExpression(node, register)
  local baseNode  = node.Base
  local indexNode = node.Index

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
  self:processFunctionCall(node.Expression, self:allocateRegister(), 0)
  self:freeRegister() -- We shouldn't have allocated registers in statements.
end

function CodeGenerator:processDoStatement(node)
  self:processBlockNode(node.Body)
end

function CodeGenerator:processBreakStatement(_)
  if not self.breakControlList then
    error("Compiler: no loop to break")
  end

  table.insert(self.breakControlList, {
    instruction = self:emitInstruction("JMP", 0, 0, 0),
    index       = #self.proto.code
  })
end

function CodeGenerator:processLocalDeclarationStatement(node)
  local variables    = node.Variables
  local initializers = node.Initializers

  local variableBaseRegister = self.stackSize - 1
  self:processExpressionList(initializers, #variables)

  for index, variableName in ipairs(variables) do
    local expressionRegister = variableBaseRegister + index
    self:declareLocalVariable(variableName, expressionRegister)
  end
end

function CodeGenerator:processLocalFunctionDeclaration(node)
  local functionName = node.Name
  local body         = node.Body

  local variableRegister = self:declareLocalVariable(functionName)
  self:processFunction(body, variableRegister)
end

function CodeGenerator:processAssignmentStatement(node)
  local lvalues     = node.LValues
  local expressions = node.Expressions

  local variableBaseRegister = self.stackSize - 1
  local lvalueRegisters = self:processExpressionList(expressions, #lvalues)

  for index, lvalue in ipairs(lvalues) do
    local lvalueRegister = variableBaseRegister + index
    self:setRegisterValue(lvalue, lvalueRegister)
  end

  self:freeRegisters(lvalueRegisters)
end

function CodeGenerator:processIfStatement(node)
  local clauses    = node.Clauses
  local elseClause = node.ElseClause

  local jumpToEndLabels = {}
  local previousLabel   = nil
  local lastClause      = clauses[#clauses]

  for _, clause in ipairs(clauses) do
    local condition = clause.Condition
    local body      = clause.Body

    local conditionRegister = self:processExpressionNode(condition)
    self:emitInstruction("TEST", conditionRegister, 0, 0)
    self:freeRegister() -- Free conditionRegister

    previousLabel = self:makeLabel()
    self:emitJump(previousLabel)
    self:processBlockNode(body)

    local isLastClause = (clause == lastClause)
    if not isLastClause or elseClause then
      local jumpToEndLabel = self:makeLabel()
      self:emitJump(jumpToEndLabel)
      table.insert(jumpToEndLabels, jumpToEndLabel)
    end
    self:patchLabelJumpsToHere(previousLabel)
  end

  if elseClause then
    self:processBlockNode(elseClause)
  end

  for _, v in ipairs(jumpToEndLabels) do
    self:patchLabelJumpsToHere(v)
  end
end

function CodeGenerator:processForGenericStatement(node)
  local iterators   = node.Iterators
  local expressions = node.Expressions
  local body        = node.Body

  local baseStackSize = self.stackSize
  local expressionRegisters = self:processExpressionList(expressions, 3)
  self:declareLocalVariables(iterators)

  local startJmpInstruction = self:emitInstruction("JMP", 0, 0)
  local loopStart = #self.proto.code
  self:breakable(function()
    self:processBlockNode(body)
    startJmpInstruction[3] = #self.proto.code - loopStart

    -- OP_TFORLOOP [A, C]    R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2))
    --                       if R(A+3) ~= nil then R(A+2)=R(A+3) else pc++
    self:emitInstruction("TFORLOOP", baseStackSize, 0, #iterators)

    -- OP_JMP [A, sBx]    pc+=sBx
    self:emitInstruction("JMP", 0, loopStart - #self.proto.code - 1)
  end)
  self:undeclareVariables(iterators)
  self:freeRegisters(#iterators + expressionRegisters)
end

function CodeGenerator:processForNumericStatement(node)
  local varName   = node.Variable
  local startExpr = node.Start
  local endExpr   = node.End
  local stepExpr  = node.Step
  local body      = node.Body

  local startRegister = self:processExpressionNode(startExpr)
  self:processExpressionNode(endExpr)
  local stepRegister  = self:allocateRegister()
  if stepExpr then
    self:processExpressionNode(stepExpr, stepRegister)
  else
    -- OP_LOADK [A, Bx]    R(A) := Kst(Bx)
    self:emitInstruction("LOADK", stepRegister, self:findOrCreateConstant(1))
  end

  -- OP_FORPREP [A, sBx]    R(A)-=R(A+2) pc+=sBx
  local forPrepInstruction = self:emitInstruction("FORPREP", startRegister, 0)
  local loopStart = #self.proto.code
  self:declareLocalVariable(varName, startRegister)
  self:breakable(function()
    self:processBlockNode(body)

    local loopEnd = #self.proto.code
    forPrepInstruction[3] = #self.proto.code - loopStart

    -- OP_FORLOOP [A, sBx]   R(A)+=R(A+2)
    --                       if R(A) <?= R(A+1) then { pc+=sBx R(A+3)=R(A) }
    self:emitInstruction("FORLOOP", startRegister, loopStart - loopEnd - 1)
  end)
  self:freeRegisters(3) -- Free startRegister, endRegister, stepRegister
  self:undeclareVariable(varName)
end

function CodeGenerator:processWhileStatement(node)
  local condition = node.Condition
  local body      = node.Body

  local startPC = #self.proto.code
  local conditionRegister = self:processExpressionNode(condition)
  self:emitInstruction("TEST", conditionRegister, 0, 0)
  self:freeRegister() -- The condition register is not needed anymore.

  local endLabel = self:makeLabel()
  self:emitJump(endLabel)
  self:breakable(function()
    self:processBlockNode(body)
    self:emitInstruction("JMP", 0, startPC - #self.proto.code - 1)
  end)
  self:patchLabelJumpsToHere(endLabel)
end

function CodeGenerator:processRepeatStatement(node)
  local body      = node.Body
  local condition = node.Condition
  local loopStart = #self.proto.code

  -- NOTE: Repeat statements' body and condition are in the same scope.
  self:enterScope()
  self:breakable(function()
    self:processStatementList(body.Statements)
    self:withTemporaryRegister(function(conditionRegister)
      self:processExpressionNode(condition, conditionRegister)

      -- OP_TEST [A, C]    if not (R(A) <=> C) then pc++
      self:emitInstruction("TEST", conditionRegister, 0, 0)
      -- OP_JMP [A, sBx]    pc+=sBx
      self:emitInstruction("JMP", 0, loopStart - #self.proto.code - 1)
    end)
  end)
  self:leaveScope()
end

function CodeGenerator:processReturnStatement(node)
  local expressions    = node.Expressions
  local resultRegister = self.stackSize
  local lastExpression = expressions[#expressions]
  local isTailcall     = self:isTailCall(expressions)

  local numResults, exprRegisters
  if isTailcall then
    self:withTemporaryRegister(function(tempRegister)
      -- Move the function call result to the correct register.
      self:processFunctionCall(
        lastExpression,
        tempRegister,
        -1
      )

      -- OP_TAILCALL [A, B, C]    return R(A)(R(A+1), ... ,R(A+B-1))
      local callInstruction = self.proto.code[#self.proto.code]
      callInstruction[1] = "TAILCALL" -- Change CALL to TAILCALL
      numResults         = -1         -- -1 = MULTIRET (all results)
    end)
  else
    exprRegisters, numResults = self:processExpressionList(expressions)
    self:freeRegisters(exprRegisters)
  end

  -- OP_RETURN [A, B]    return R(A), ... ,R(A+B-2)
  self:emitInstruction("RETURN", resultRegister, numResults + 1, 0)
end

--// Main Methods //--

-- Processes an expression node and returns the register(s) where the result is stored.
-- By default returns a single register (result), even for multi-return expressions.
--
--  node            - The AST node representing the expression.
--
--  register        - (Optional) The register to store the result. If not
--                    provided, a new register is allocated.
--
--  resultRegisters - (Optional) Number of result registers expected
--                    (used for function calls/varargs).
function CodeGenerator:processExpressionNode(node, register, resultRegisters)
  if not node then error("Node not found.") end
  register = register or self:allocateRegister()

  local nodeType = node.TYPE
  local handler  = self.CONFIG.EXPRESSION_HANDLERS[nodeType]
  local func     = self[handler]
  if not func then
    error("Compiler: Expression node type not supported: " .. nodeType)
  end

  return func(self, node, register, resultRegisters)
end

function CodeGenerator:processStatementNode(node)
  if not node then error("Node not found.") end

  local nodeType = node.TYPE
  local handler  = self.CONFIG.STATEMENT_HANDLERS[nodeType]
  local func     = self[handler]
  if not func then
    error("Compiler: Statement node type not supported: " .. nodeType)
  end

  return func(self, node)
end

-- Used for RK() operands (Register or Konstant).
-- If it's a register it has to get free'd later.
-- TODO: Find a better way to deallocate RK-type operands.
function CodeGenerator:processConstantOrExpression(node, register)
  local nodeType = node.TYPE

  -- Is it a constant?
  if nodeType == "NumericLiteral" or nodeType == "StringLiteral" then
    local constantIndex = self:findOrCreateConstant(node.Value)

    -- Check if it can fit in 9-bit signed RK operand.
    if -constantIndex < 255 then
      -- Returns a negative index to indicate it's a constant.
      return constantIndex
    end
  end

  return self:processExpressionNode(node, register)
end

-- Processes a list of expressions and returns:
--
--  allocatedRegisters - The total number of registers allocated for the
--                       expression list.
--
--  numResults         - The number of results produced by the
--                       expression list. If -1, indicates that the last
--                       expression can return multiple results (multiret).
--                       Used for "B" operand in CALL/RETURN instructions.
--
-- Sigh.
function CodeGenerator:processExpressionList(expressionList, expectedRegisters)
  if #expressionList == 0 and not expectedRegisters then
    return 0, 0
  end

  local lastExpression = expressionList[#expressionList]
  local isMultiret     = self:isMultiReturnNode(lastExpression)

  -- Process fixed expressions first.
  -- NOTE: We exclude the multiret last expression from fixedCount, because
  -- we will handle it later.
  local fixedCount = (isMultiret and (#expressionList - 1)) or #expressionList
  local maxAllocatedRegisters = expectedRegisters or fixedCount
  for expressionIndex = 1, fixedCount do
    self:processExpressionNode(expressionList[expressionIndex])
    if expressionIndex > maxAllocatedRegisters then
      -- Not used, free the register.
      self:freeRegister()
    end
  end

  -- Handle multiret last expression (if present).
  if isMultiret then
    local remaining = (expectedRegisters and (expectedRegisters - fixedCount)) or -1
    self:processExpressionNode(lastExpression, nil, remaining)
  elseif expectedRegisters and expectedRegisters > #expressionList then
    -- Fill with nils if needed.
    -- NOTE: We check for multiret above because if the last expression is multiret,
    --       it will already fill the remaining registers as needed.

    local toFill        = expectedRegisters - #expressionList
    local baseStackSize = self.stackSize
    self:allocateRegisters(toFill)
    self:emitInstruction(
      "LOADNIL",
      baseStackSize,
      baseStackSize + toFill - 1,
      0
    )
  end

  local allocatedRegisters = expectedRegisters or #expressionList
  local numResults = (isMultiret and -1) or allocatedRegisters

  -- NOTE: if `numResults` is -1, it indicates that the last expression
  --       can return multiple results (multiret), so the amount of results
  --       is not fixed (or known) at compile time.
  --
  --       `allocatedRegisters` is used to further free the correct amount
  --       of registers allocated for this expression list.
  return allocatedRegisters, numResults
end

function CodeGenerator:processStatementList(statementList)
  for _, node in ipairs(statementList) do
    self:processStatementNode(node)
  end
end

function CodeGenerator:processBlockNode(blockNode, variables, isFunction)
  self:enterScope(isFunction)
  if variables then
    -- Declare variables while in the scope.
    self:declareLocalVariables(variables)
  end
  self:processStatementList(blockNode.Statements)
  self:leaveScope()
end

-- Processes and compiles a function node into a function prototype.
--
--  functionNode     - The AST node representing the function.
--  closureRegister  - (Optional) If provided, emits a CLOSURE instruction
--                     to create the closure in the given register.
function CodeGenerator:processFunction(functionNode, closureRegister)
  local parentProto = self.proto
  local childProto  = self:emitPrototype({
    isVararg  = functionNode.IsVararg,
    numParams = #functionNode.Parameters,
  })
  self.proto = childProto
  self:processBlockNode(functionNode.Body, functionNode.Parameters, true)
  self:emitInstruction("RETURN", 0, 1, 0) -- Default return statement
  self.proto = parentProto

  if closureRegister then
    local protoIndex = #parentProto.protos + 1
    parentProto.protos[protoIndex] = childProto

    -- OP_CLOSURE [A, Bx]    R(A) := closure(KPROTO[Bx], R(A), ... ,R(A+n))
    self:emitInstruction("CLOSURE", closureRegister, protoIndex - 1, 0) -- Proto indices are zero-based.

    -- After the "CLOSURE" instruction, we need to set up the upvalues
    -- for the newly created function prototype. It is done by pseudo
    -- instructions that follow the CLOSURE instruction.
    for _, upvalueName in ipairs(childProto.upvalues) do
      local upvalueType = self:getUpvalueType(upvalueName)
      if upvalueType == "Local" then
        -- OP_MOVE [A, B]    R(A) := R(B)
        self:emitInstruction("MOVE", 0, self:findVariableRegister(upvalueName))
      elseif upvalueType == "Upvalue" then
        -- OP_GETUPVAL [A, B]    R(A) := UpValue[B]
        self:emitInstruction("GETUPVAL", 0, self:findOrCreateUpvalue(upvalueName))
      else -- Assume it's a global.
        error("Compiler: Possible parser error, the upvalue cannot be a global: " .. upvalueName)
      end
    end
  end

  return childProto
end

--// Entry Point //--

function CodeGenerator:generate()
  return self:processFunction({
    Body       = self.ast.Body,
    Parameters = {},
    IsVararg   = true -- The main chunk is always vararg.
                      -- (used for command-line arguments)
  })
end

--[[
  ============================================================================
                                   (۶* ‘ヮ’)۶”
                          !!!!!!!!THE COMPILER!!!!!!!!
  ============================================================================

  The final part of the compiler is the compiler itself (duh). The
  compiler is responsible for converting the given Lua Function Prototypes
  into Lua bytecode. The compiler will implement binary writing logic
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
local OpArgN = 0 -- Argument is not used
local OpArgU = 1 -- Argument is used
local OpArgR = 2 -- Argument is a register or a jump offset
local OpArgK = 3 -- Argument is a constant or register/constant

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
local LUA_TNIL     = 0
local LUA_TBOOLEAN = 1
local LUA_TNUMBER  = 3
local LUA_TSTRING  = 4

--* Compiler *--
local Compiler = {}
Compiler.__index = Compiler

Compiler.CONFIG = {
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
  --   - argBMode: The type of the B argument (OpArgN, OpArgU, OpArgR, OpArgK).
  --   - argCMode: The type of the C argument (OpArgN, OpArgU, OpArgR, OpArgK).
  --
  -- Source: https://www.lua.org/source/5.1/lopcodes.c.html#luaP_opmodes
  COMPILER_OPCODE_LOOKUP = {
    ["MOVE"]     = {0, MODE_iABC, OpArgR, OpArgN},   ["LOADK"]     = {1, MODE_iABx, OpArgK, OpArgN},
    ["LOADBOOL"] = {2, MODE_iABC, OpArgU, OpArgU},   ["LOADNIL"]   = {3, MODE_iABC, OpArgR, OpArgN},
    ["GETUPVAL"] = {4, MODE_iABC, OpArgU, OpArgN},   ["GETGLOBAL"] = {5, MODE_iABx, OpArgK, OpArgN},
    ["GETTABLE"] = {6, MODE_iABC, OpArgR, OpArgK},   ["SETGLOBAL"] = {7, MODE_iABx, OpArgK, OpArgN},
    ["SETUPVAL"] = {8, MODE_iABC, OpArgU, OpArgN},   ["SETTABLE"]  = {9, MODE_iABC, OpArgK, OpArgK},
    ["NEWTABLE"] = {10, MODE_iABC, OpArgU, OpArgU},  ["SELF"]      = {11, MODE_iABC, OpArgR, OpArgK},
    ["ADD"]      = {12, MODE_iABC, OpArgK, OpArgK},  ["SUB"]       = {13, MODE_iABC, OpArgK, OpArgK},
    ["MUL"]      = {14, MODE_iABC, OpArgK, OpArgK},  ["DIV"]       = {15, MODE_iABC, OpArgK, OpArgK},
    ["MOD"]      = {16, MODE_iABC, OpArgK, OpArgK},  ["POW"]       = {17, MODE_iABC, OpArgK, OpArgK},
    ["UNM"]      = {18, MODE_iABC, OpArgR, OpArgN},  ["NOT"]       = {19, MODE_iABC, OpArgR, OpArgN},
    ["LEN"]      = {20, MODE_iABC, OpArgR, OpArgN},  ["CONCAT"]    = {21, MODE_iABC, OpArgR, OpArgR},
    ["JMP"]      = {22, MODE_iAsBx, OpArgR, OpArgN}, ["EQ"]        = {23, MODE_iABC, OpArgK, OpArgK},
    ["LT"]       = {24, MODE_iABC, OpArgK, OpArgK},  ["LE"]        = {25, MODE_iABC, OpArgK, OpArgK},
    ["TEST"]     = {26, MODE_iABC, OpArgR, OpArgU},  ["TESTSET"]   = {27, MODE_iABC, OpArgR, OpArgU},
    ["CALL"]     = {28, MODE_iABC, OpArgU, OpArgU},  ["TAILCALL"]  = {29, MODE_iABC, OpArgU, OpArgU},
    ["RETURN"]   = {30, MODE_iABC, OpArgU, OpArgN},  ["FORLOOP"]   = {31, MODE_iAsBx, OpArgR, OpArgN},
    ["FORPREP"]  = {32, MODE_iAsBx, OpArgR, OpArgN}, ["TFORLOOP"]  = {33, MODE_iABC, OpArgN, OpArgU},
    ["SETLIST"]  = {34, MODE_iABC, OpArgU, OpArgU},  ["CLOSE"]     = {35, MODE_iABC, OpArgN, OpArgN},
    ["CLOSURE"]  = {36, MODE_iABx, OpArgU, OpArgN},  ["VARARG"]    = {37, MODE_iABC, OpArgU, OpArgN}
  }
}

--// Compiler Constructor //--
function Compiler.new(mainProto)
  --// Type Checking //--
  assert(type(mainProto) == "table", "Expected table for 'mainProto', got " .. type(mainProto))
  assert(mainProto.code and mainProto.constants, "Expected a valid Lua function prototype for 'mainProto'")

  --// Instance //--
  local CompilerInstance = setmetatable({}, Compiler)

  --// Initialization //--
  CompilerInstance.mainProto = mainProto

  return CompilerInstance
end

--// Utility Functions //--

-- A custom implementation of the `frexp` function, which decomposes
-- a floating-point number into its mantissa and exponent.
-- This is necessary because Lua 5.3+ does not have `math.frexp`,
function Compiler:frexp(value)
  -- Use built-in if available (Lua 5.1, Lua 5.2)
  if math.frexp then return math.frexp(value) end
  if value == 0 then return 0, 0 end

  local exponent = math.floor(math.log(math.abs(value)) / math.log(2)) + 1
  local mantissa = value / (2 ^ exponent)
  return mantissa, exponent
end

-- A bitwise left shift, simulated with multiplication.
function Compiler:lshift(value, shift)
  return value * (2 ^ shift)
end

--[[
  RK (Register or Konstant) Encoding:

  Lua uses a clever trick to encode both register indices and constant indices
  in a single 9-bit field (B or C operands in iABC instructions).

  The encoding works as follows:
  - Positive values (0-255): Direct register indices
  - Negative values (-1 to -256): Constant indices (encoded as 256 + abs(value))

  Example:
    Register R5:  5   -> encoded as 5
    Constant K0: -1   -> encoded as 256
    Constant K1: -2   -> encoded as 257
    Constant K42: -43 -> encoded as 299

  The VM decodes this by checking if the value is >= 256:
    if (operand >= 256) then
      -- It's a constant, get from proto.constants[operand - 256]
    else
      -- It's a register, get from stack[operand]
    end
--]]
function Compiler:encodeRKValue(value)
  if value < 0 then
    return 255 - value
  end
  return value
end

-- We use negative indices to represent constants internally,
-- so we need to convert them to unsigned indices for bytecode.
-- Example:
--   LOADK     5, -4 --> LOADK     5, 3
--   GETGLOBAL 2, -1 --> GETGLOBAL 2, 0
function Compiler:toUnsigned(value)
  if value < 0 then
    return (-value) - 1
  end
  return value
end

function Compiler:makeOneByte(value)
  if value < 0 or value > 255 then
    error("Compiler: makeOneByte value out of range (0-255): " .. tostring(value))
  end

  return string.char(value % 256)
end

function Compiler:makeFourBytes(value)
  if value < 0 or value > 4294967295 then
    error("Compiler: makeFourBytes value out of range (0-4294967295): " .. tostring(value))
  end

  local b1 = value % 256 value = math.floor(value / 256)
  local b2 = value % 256 value = math.floor(value / 256)
  local b3 = value % 256 value = math.floor(value / 256)
  local b4 = value % 256

  return string.char(b1, b2, b3, b4)
end

function Compiler:makeEightBytes(value)
  if value < 0 or value > 18446744073709551615 then
    error("Compiler: makeEightBytes value out of range (0-18446744073709551615): " .. tostring(value))
  end

  local lowWord  = value % 2^32
  local highWord = math.floor(value / 2^32)

  return self:makeFourBytes(lowWord) .. self:makeFourBytes(highWord)
end

function Compiler:makeDouble(value)
  -- Handle the simple edge cases first.
  if value == 0 then
    -- Zero is just 8 zero-bytes. Easy!
    return self:makeEightBytes(0)
  elseif value == 1/0 then -- Check for infinity
    -- The special representation for infinity is:
    --  exponent = all bits set to 1 (2047)
    --  faction  = 0
    local lowWord = 0
    local highWord = self:lshift(2047, 20) -- 0x7FF00000
    return self:makeFourBytes(highWord) .. self:makeFourBytes(lowWord)
  elseif value ~= value then -- Check for NaN (Not a Number)
    -- The special representation for NaN is:
    --  exponent = all bits set to 1 (2047)
    --  faction  = non-zero value (we'll use 1)
    local lowWord = 1
    local highWord = self:lshift(2047, 20) -- 0x7FF00000
    return self:makeFourBytes(highWord) .. self:makeFourBytes(lowWord)
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
  -- We can get this with a modulo operation. 2^32 is 4294967296.
  local lowWord = integerMantissa % 4294967296

  -- The higher 32 bits are a combination of the sign, exponent, and the
  -- remaining 20 bits of the mantissa.
  local mantissaHigh = math.floor(integerMantissa / 4294967296)

  -- Now we use lshift to put everything in its place for the high word:
  --  - The sign bit is the 31st bit.
  --  - The exponent's 11 bits start at the 20th bit.
  --  - The remaining 20 bits of the mantissa fill the rest.
  local highWord = self:lshift(sign, 31)
                  + self:lshift(biasedExponent, 20)
                  + mantissaHigh

  -- Convert the two 32-bit chunks to bytes.
  -- Since we're little-endian, the "low" part comes first.
  return self:makeFourBytes(lowWord) .. self:makeFourBytes(highWord)
end

--// Bytecode Generation //--
function Compiler:makeString(value)
  value = value .. "\0"
  local size = self:makeEightBytes(#value)
  return size .. value
end

--[[
  Serializes a constant value (nil, boolean, number, or string).

  Format:
  | Type Tag (1 byte) | Value (Variable Bytes) |

  Type Tags: 0=nil, 1=bool, 3=number, 4=string
  Value Format:
    - nil: (none)
    - boolean: 1 byte (0=false, 1=true)
    - number: 8 bytes (IEEE 754 double)
    - string: <size_t size><data...>'\0'
--]]
function Compiler:makeConstant(constantValue)
  local constType = type(constantValue)

  if constantValue == nil then
    return self:makeOneByte(LUA_TNIL)
  elseif constType == "boolean" then
    return self:makeOneByte(LUA_TBOOLEAN) .. self:makeOneByte(constantValue and 1 or 0)
  elseif constType == "number" then
    return self:makeOneByte(LUA_TNUMBER) .. self:makeDouble(constantValue)
  elseif constType == "string" then
    return self:makeOneByte(LUA_TSTRING) .. self:makeString(constantValue)
  end

  error("Compiler: Unsupported constant type '" .. constType .. "'")
end

function Compiler:makeInstruction(instruction)
  -- First, let's look up the opcode's number and its argument format.
  local instructionName = instruction[1]
  local opcodeTable     = self.CONFIG.COMPILER_OPCODE_LOOKUP[instruction[1]]
  if not opcodeTable or not instructionName then
    error("Compiler: Unsupported instruction '" .. tostring(instructionName) .. "'")
  end

  local opcode, opmode, arg1, arg2 = opcodeTable[1], opcodeTable[2], opcodeTable[3], opcodeTable[4]
  local a = instruction[2]

  -- Check for overflow first
  if a < 0 or a > 255 then
    error(
      string.format(
        "Compiler: Operand A overflow in instruction '%s'. Got %d. Valid range is 0 to 255.",
        tostring(instructionName), a
      )
    )
  end

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
  --
  -- iABC:  [ Opcode(0-5 : 6) | A(6-13 : 8) | C(14-22 : 9) | B(23-31 : 9) ]
  -- iABx:  [ Opcode(0-5 : 6) | A(6-13 : 8) |       Bx(14-31 : 18)        ]
  -- iAsBx: [ Opcode(0-5 : 6) | A(6-13 : 8) |      sBx(14-31 : 18)        ]
  --
  -- NOTE: In iABC instructions, C comes before B in the byte layout.

  a = self:lshift(a, 6)
  if opmode == MODE_iABC then
    local b, c = instruction[3], instruction[4]

    -- Check for overflows first
    if b < -256 or b > 255 then
      error(
        string.format(
          "Compiler: Operand B overflow in instruction '%s'. Got %d. Valid range is -256 to 255.",
          tostring(instructionName), b
        )
      )
    end
    if c < -256 or c > 255 then
      error(
        string.format(
          "Compiler: Operand C overflow in instruction '%s'. Got %d. Valid range is -256 to 255.",
          tostring(instructionName), c
        )
      )
    end

    -- Handle RK() operands (register or constant)
    if arg1 == OpArgK then b = self:lshift(self:encodeRKValue(b), 23)
    else                   b = self:lshift(b, 23) end
    if arg2 == OpArgK then c = self:lshift(self:encodeRKValue(c), 14)
    else                   c = self:lshift(c, 14) end

    -- Combine: [ Opcode(0-5 : 6) | A(6-13 : 8) | C(14-22 : 9) | B(23-31 : 9) ]
    return self:makeFourBytes(opcode + a + b + c)
  elseif opmode == MODE_iABx then
    local bx = self:lshift(self:toUnsigned(instruction[3]), 14)

    -- Combine: [ Opcode(0-5 : 6) | A(6-13 : 8) | Bx(14-31 : 18) ]
    return self:makeFourBytes(opcode + a + bx)
  elseif opmode == MODE_iAsBx then
    local b = instruction[3]

    -- sBx (signed Bx) is split into two parts:
    --  - negative values (-131072 to -1)
    --  - positive values (0 to 131071)
    --
    -- This allows us to represent both positive and negative offsets.
    -- To get sBx, we need to add 131071 (2^17 - 1) to Bx.
    local sbx = self:lshift(b + (2^17 - 1), 14)

    -- Combine: [ Opcode(0-5 : 6) | A(6-13 : 8) | sBx(14-31 : 18) ]
    return self:makeFourBytes(opcode + a + sbx)
  end

  error("Compiler: Unsupported instruction format for '" .. tostring(instructionName) .. "'")
end

function Compiler:makeConstantSection(proto)
  local constantSection = { self:makeFourBytes(#proto.constants) } -- Number of constants
  for index, constant in ipairs(proto.constants) do
    constantSection[index + 1] = self:makeConstant(constant)
  end
  return table.concat(constantSection)
end

function Compiler:makeProtoSection(proto)
  local protoSection = { self:makeFourBytes(#proto.protos) } -- Number of protos
  for index, childProto in ipairs(proto.protos) do
    protoSection[index + 1] = self:makeFunction(childProto)
  end
  return table.concat(protoSection)
end

-- Serializes the code section of a function prototype.
-- Format:
-- | Instruction Count (4 bytes) | Instruction 1 (4 bytes) | Instruction 2 (4 bytes) | ... |
function Compiler:makeCodeSection(proto)
  local codeSection = { self:makeFourBytes(#proto.code) } -- Number of instructions
  for index, instruction in ipairs(proto.code) do
    codeSection[index + 1] = self:makeInstruction(instruction)
  end
  return table.concat(codeSection)
end


--[[
  Serializes a complete Lua Function Prototype chunk.

  Format:
    +-----------------------------+-----------------------------------------+
    | Field                       | Size (bytes) / Format                   |
    +-----------------------------+-----------------------------------------+
    | Source Name                 | <size_t size><chars...>\0 (string)      |
    | Line Defined                | 4 (integer, debug)                      |
    | Last Line Defined           | 4 (integer, debug)                      |
    | Number of Upvalues          | 1 (byte)                                |
    | Number of Parameters        | 1 (byte)                                |
    | Is Vararg Flag              | 1 (byte, 0 or 2)                        |
    | Max Stack Size              | 1 (byte)                                |
    +-----------------------------+-----------------------------------------+
    | Code Section                | <See makeCodeSection>                   |
    +-----------------------------+-----------------------------------------+
    | Constant Section            | <See makeConstantSection>               |
    +-----------------------------+-----------------------------------------+
    | Debug Info (Line Numbers)   | Size=0 (4 bytes) + No Data              |
    | Debug Info (Local Vars)     | Size=0 (4 bytes) + No Data              |
    | Debug Info (Upvalue Names)  | Size=0 (4 bytes) + No Data              |
    +-----------------------------+-----------------------------------------+
--]]
function Compiler:makeFunction(proto)
  -- Basic validation of the function prototype.
  if not proto.code or not proto.constants then error("Compiler: Invalid function prototype")
  elseif #proto.upvalues > 255    then error("Compiler: Too many upvalues in function prototype (max 255)")
  elseif proto.numParams > 255    then error("Compiler: Too many parameters in function prototype (max 255)")
  elseif proto.maxStackSize > 255 then error("Compiler: Max stack size too large in function prototype (max 255)")
  end

  return table.concat({
    self:makeString(proto.functionName),  -- Source name
    self:makeFourBytes(0),                -- Line defined (debug)
    self:makeFourBytes(0),                -- Last line defined (debug)
    self:makeOneByte(#proto.upvalues),    -- nups (Number of upvalues)
    self:makeOneByte(proto.numParams),    -- Number of parameters
    self:makeOneByte((proto.isVararg and VARARG_ISVARARG) or VARARG_NONE),
    self:makeOneByte(proto.maxStackSize), -- Max stack size
    self:makeCodeSection(proto),          -- Code section
    self:makeConstantSection(proto),      -- Constant section
    self:makeProtoSection(proto),         -- Nested prototype section

    -- Debug info (not implemented)
    self:makeFourBytes(0), -- Line info (debug)
    self:makeFourBytes(0), -- Local variables (debug)
    self:makeFourBytes(0)  -- Upvalues (debug)

    -- Note: TLC doesn't implement debug info as that would require tracking
    --       line numbers, local variables, and upvalue names, etc. which will
    --       add a lot of unnecessary complexity to the compiler.
  })
end

--// Main //--
function Compiler:compile()
  return self.CONFIG.LUA_COMMON_HEADER .. self:makeFunction(self.mainProto)
end

--[[
    ============================================================================
                                      (/^▽^)/
                                   THE VIRTUAL MACHINE!
    ============================================================================
--]]

--* Constants *--
local LUA_STACK_TOP = 0

--* VirtualMachine *--
local VirtualMachine = {}
VirtualMachine.__index = VirtualMachine -- Set up for method calls via `.`

VirtualMachine._CONFIG = {
  -- Number of list items to accumulate before a SETLIST instruction.
  LFIELDS_PER_FLUSH = 50
}

--// VirtualMachine Constructor //--
function VirtualMachine.new(proto)
  local self = setmetatable({}, VirtualMachine)

  self.mainProto = proto -- Should only be used in `:execute()`.
  self.lclosure  = nil   -- Current closure we're working with.

  return self
end

--// Auxiliary functions //--
function VirtualMachine:getLength(...)
  return select("#", ...)
end

function VirtualMachine:pushClosure(closure)
  closure = {
    nupvalues = closure.nupvalues or 0,
    env       = closure.env       or _G,
    proto     = closure.proto,
    upvalues  = closure.upvalues  or {},
  }
  self.lclosure = closure

  return closure
end

--// Execution //--
function VirtualMachine:executeClosure(...)
  -- Optimization: localize fields.
  local lclosure  = self.lclosure
  local stack     = {}
  local env       = lclosure.env
  local proto     = lclosure.proto
  local upvalues  = lclosure.upvalues
  local code      = proto.code
  local constants = proto.constants
  local numparams = proto.numParams
  local isVararg  = proto.isVararg

  local maxStackSize = proto.maxStackSize
  local top = maxStackSize

  -- Initialize parameters.
  local vararg
  local params = { ... }

  for paramIdx = 1, numparams do
    stack[paramIdx - 1] = params[paramIdx]
  end
  if isVararg then
    vararg = { select(numparams + 1, ...) }
    stack[numparams] = vararg -- Implicit "arg" argument.
  end

  -- Only gets set to a table of results when we have to return.
  local returnValues = nil

  -- Prepare for execution loop.
  local pc = 1
  while true do
    local instruction = code[pc]
    if not instruction then
      break
    end

    local opcode, a, b, c = instruction[1], instruction[2], instruction[3], instruction[4]

    -- OP_MOVE [A, B]    R(A) := R(B)
    if opcode == "MOVE" then
      stack[a] = stack[b]

    -- OP_LOADK [A, Bx]    R(A) = Kst(Bx)
    elseif opcode == "LOADK" then
      stack[a] = constants[-b]

    -- OP_LOADBOOL [A, B, C]    R(A) := (Bool)B; if (C) pc++
    elseif opcode == "LOADBOOL" then
      stack[a] = (b == 1)
      if c == 1 then
        pc = pc + 1
      end

    -- OP_LOADNIL [A, B]    R(A) := ... := R(B) := nil
    elseif opcode == "LOADNIL" then
      for reg = b, a, -1 do
        stack[reg] = nil
      end

    -- OP_GETUPVAL [A, B]    R(A) := UpValue[B]
    elseif opcode == "GETUPVAL" then
      local upvalue = upvalues[b + 1]
      stack[a] = upvalue.stack[upvalue.index]

    -- OP_GETGLOBAL [A, Bx]    R(A) := Gbl[Kst(Bx)]
    elseif opcode == "GETGLOBAL" then
      stack[a] = env[constants[-b]]

    -- OP_GETTABLE [A, B, C]    R(A) := R(B)[RK(C)]
    elseif opcode == "GETTABLE" then
      stack[a] = stack[b][stack[c] or constants[-c]]

    -- OP_SETGLOBAL [A, Bx]    Gbl[Kst(Bx)] := R(A)
    elseif opcode == "SETGLOBAL" then
      env[stack[b] or constants[-b]] = stack[a]

    -- OP_SETUPVAL [A, B]    UpValue[B] := R(A)
    elseif opcode == "SETUPVAL" then
      local upvalue = upvalues[b + 1]
      upvalue.stack[upvalue.index] = stack[a]

    -- OP_SETTABLE [A, B, C]    R(A)[R(B)] := R(C)
    elseif opcode == "SETTABLE" then
      stack[a][stack[b] or constants[-b]] = stack[c] or constants[-c]

    -- OP_NEWTABLE [A, B, C]    R(A) := {} (size = B,C)
    elseif opcode == "NEWTABLE" then
      stack[a] = {}

    -- OP_SELF [A, B, C]    R(A+1) := R(B) R(A) := R(B)[RK(C)]
    elseif opcode == "SELF" then
      local rb = stack[b]
      stack[a + 1] = rb
      stack[a] = rb[stack[c] or constants[-c]]

    -- OP_ADD [A, B, C]    R(A) := RK(B) + RK(C)
    elseif opcode == "ADD" then
      stack[a] = (stack[b] or constants[-b]) + (stack[c] or constants[-c])

    -- OP_SUB [A, B, C]    R(A) := RK(B) - RK(C)
    elseif opcode == "SUB" then
      stack[a] = (stack[b] or constants[-b]) - (stack[c] or constants[-c])

    -- OP_MUL [A, B, C]    R(A) := RK(B) * RK(C)
    elseif opcode == "MUL" then
      stack[a] = (stack[b] or constants[-b]) * (stack[c] or constants[-c])

    -- OP_DIV [A, B, C]    R(A) := RK(B) / RK(C)
    elseif opcode == "DIV" then
      stack[a] = (stack[b] or constants[-b]) / (stack[c] or constants[-c])

    -- OP_MOD [A, B, C]    R(A) := RK(B) % RK(C)
    elseif opcode == "MOD" then
      stack[a] = (stack[b] or constants[-b]) % (stack[c] or constants[-c])

    -- OP_POW [A, B, C]    R(A) := RK(B) ^ RK(C)
    elseif opcode == "POW" then
      stack[a] = (stack[b] or constants[-b]) ^ (stack[c] or constants[-c])

    -- OP_UNM [A, B]    R(A) := -R(B)
    elseif opcode == "UNM" then
      stack[a] = -stack[b]

    -- OP_NOT [A, B]    R(A) := not R(B)
    elseif opcode == "NOT" then
      stack[a] = not stack[b]

    -- OP_LEN [A, B]    R(A) := length of R(B)
    elseif opcode == "LEN" then
      stack[a] = #stack[b]

    -- OP_CONCAT [A, B, C]    R(A) := R(B).. ... ..R(C)
    elseif opcode == "CONCAT" then
      local values = {}
      for reg = b, c do
        table.insert(values, stack[reg])
      end
      stack[a] = table.concat(values)

    -- OP_JMP [A, sBx]    pc+=sBx
    elseif opcode == "JMP" then
      pc = pc + b

    -- OP_EQ [A, B, C]    if ((RK(B) == RK(C)) ~= A) then pc++
    elseif opcode == "EQ" then
      if ((stack[b] or constants[-b]) == (stack[c] or constants[-c])) ~= (a == 1) then
        pc = pc + 1
      end

    -- OP_LT [A, B, C]    if ((RK(B) < RK(C)) ~= A) then pc++
    elseif opcode == "LT" then
      if ((stack[b] or constants[-b]) < (stack[c] or constants[-c])) ~= (a == 1) then
        pc = pc + 1
      end

    -- OP_LE [A, B, C]    if ((RK(B) <= RK(C)) ~= A) then pc++
    elseif opcode == "LE" then
      if ((stack[b] or constants[-b]) <= (stack[c] or constants[-c])) ~= (a == 1) then
        pc = pc + 1
      end

    -- OP_TEST [A, C]    if not (R(A) <=> C) then pc++
    elseif opcode == "TEST" then
      if not stack[a] == (c == 1) then
        pc = pc + 1
      end

    -- OP_TESTSET [A, B, C]    if (R(B) <=> C) then R(A) := R(B) else pc++
    elseif opcode == "TESTSET" then
      if not stack[a] == (c == 1) then
        stack[a] = stack[b]
      else
        pc = pc + 1
      end

    -- OP_CALL [A, B, C]    R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
    elseif opcode == "CALL" then
      local func = stack[a]
      local args = {}
      local nArgs
      if b ~= LUA_STACK_TOP then
        nArgs = b - 1
        for index = 1, nArgs do
          table.insert(args, stack[a + index])
        end
      else
        nArgs = top - (a + 1)
        for index = a + 1, top - 1 do
          table.insert(args, stack[index])
        end
      end

      local returns = { func(unpack(args)) }

      if c ~= LUA_STACK_TOP then
        for index = 0, c - 2 do
          stack[a + index] = returns[index + 1]
        end
      else
        -- Multi-return: push all results onto the stack.
        local nReturns = self:getLength(unpack(returns))
        top = a + nReturns
        for index = 1, nReturns do
          stack[a + index - 1] = returns[index]
        end
      end

    -- OP_TAILCALL [A, B, C]    return R(A)(R(A+1), ... ,R(A+B-1))
    elseif opcode == "TAILCALL" then
      local func = stack[a]
      local args = {}
      if b ~= LUA_STACK_TOP then
        for reg = a + 1, a + b - 1 do
          table.insert(args, stack[reg])
        end
      else
        for reg = a + 1, a + top do
          table.insert(args, stack[reg])
        end
      end

      local returns = { func(unpack(args)) }
      local nReturns = self:getLength(unpack(returns))
      top = a + nReturns - 1
      for index = 1, nReturns do
        stack[a + index - 1] = returns[index]
      end

    -- OP_RETURN [A, B]    return R(A), ... ,R(A+B-2)
    elseif opcode == "RETURN" then
      local returns = {}
      if b == LUA_STACK_TOP then
        for reg = a, top do
          table.insert(returns, stack[reg])
        end
      else
        for reg = a, a + b - 2 do
          table.insert(returns, stack[reg])
        end
      end

      returnValues = returns
      break

    -- OP_FORLOOP [A, sBx]   R(A)+=R(A+2)
    --                       if R(A) <?= R(A+1) then { pc+=sBx R(A+3)=R(A) }
    elseif opcode == "FORLOOP" then
      local step  = stack[a + 2]
      local idx   = stack[a] + step
      local limit = stack[a + 1]
      if (step < 0 and idx >= limit) or (step > 0 and idx <= limit) then
        pc = pc + b
        stack[a] = idx
        stack[a + 3] = idx
      end

    -- OP_FORPREP [A, sBx]    R(A)-=R(A+2) pc+=sBx
    elseif opcode == "FORPREP" then
      local init = stack[a]
      local plimit = stack[a + 1]
      local pstep = stack[a + 2]
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
    elseif opcode == "TFORLOOP" then
      local cb = a + 3

      -- Copy function and arguments for the call.
      stack[cb + 2] = stack[a + 2] -- The iterator function.
      stack[cb + 1] = stack[a + 1] -- The state.
      stack[cb] = stack[a]         -- The control variable.

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
      else
        -- Otherwise, skip the next instruction.
        -- (Should be the jump back to the loop start.)
        pc = pc + 1
      end

    -- OP_SETLIST [A, B, C]    R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B
    elseif opcode == "SETLIST" then
      local n = b
      if n == LUA_STACK_TOP then
        n = top - a - 1
      end

      local h = stack[a]
      assert(type(h) == "table")

      local last = ((c - 1) * self._CONFIG.LFIELDS_PER_FLUSH) + n
      for i = n, 1, -1 do
        local val = stack[a + i]
        h[last] = val
        last = last - 1
      end

    -- OP_VARARG [A]    close all variables in the stack up to (>=) R(A)
    elseif opcode == "CLOSE" then
      -- Stub. No implementation needed for this VM.

    -- OP_CLOSURE [A, Bx]    R(A) := closure(KPROTO[Bx], R(A), ... ,R(A+n))
    elseif opcode == "CLOSURE" then
      local tProto = proto.protos[b + 1]
      local tProtoUpvalues = {}

      for _ = 1, #tProto.upvalues do
        pc = pc + 1
        local instr = code[pc]
        local opname = instr[1]
        if opname == "MOVE" then
          table.insert(tProtoUpvalues, {
            stack = stack,
            index = instr[3],
          })
        elseif opname == "GETUPVAL" then
          local upvalue = upvalues[instr[3] + 1]
          table.insert(tProtoUpvalues, {
            stack = upvalue.stack,
            index = upvalue.index,
          })
        else
          error("Unexpected instruction: " .. opname)
        end
      end

      local tClosure = {
        nupvalues = #tProtoUpvalues,
        env       = nil,
        proto     = tProto,
        upvalues  = tProtoUpvalues,
      }

      stack[a] = function(...)
        self:pushClosure(tClosure)
        local returns = { self:executeClosure(...) }
        self:pushClosure(lclosure)

        return unpack(returns)
      end

    -- OP_VARARG [A, B]    R(A), R(A+1), ..., R(A+B-1) = vararg
    elseif opcode == "VARARG" then
      local varargCount = #vararg
      if b == LUA_STACK_TOP then
        top = a + varargCount
        for i = 1, varargCount do
          stack[a + i - 1] = vararg[i]
        end
      else
        for i = 1, b - 1 do
          stack[a + i - 1] = vararg[i]
        end
      end
    else
      error("Unimplemented instruction: " .. tostring(opcode))
    end

    pc = pc + 1
  end

  if returnValues then
    return unpack(returnValues)
  end
end

function VirtualMachine:execute()
  -- Push main closure.
  self:pushClosure({
    nupvalues = 0,
    env       = _G,
    proto     = self.mainProto,
    upvalues  = {},
  })

  return self:executeClosure()
end

-- Now I'm just exporting everything...
return {
  Tokenizer      = Tokenizer,
  Parser         = Parser,
  CodeGenerator  = CodeGenerator,
  Compiler       = Compiler,
  VirtualMachine = VirtualMachine,

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
    local bytecode = Compiler.new(proto):compile()

    return bytecode
  end
}