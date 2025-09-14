--[[
  Hey everyone!

  Today, we're embarking on a fun little adventure: writing a compiler
  for Lua 5.1... entirely in Lua itself!

  But this isn't just ANY compiler. Oh no. This is a Super Tiny
  compiler! So small, in fact, that if you strip away all these lovely
  comments, it clocks in at around ~2000 lines of pure, sweet code.

  Despite its size, this little guy is mighty! It's designed to
  tokenize, parse, and compile (most of!) the Lua 5.1 code you can
  throw at it. It's even brave enough to compile *itself*! How cool is that?!

  Alright, enough talk! Let's roll up our sleeves and get this party started!

  ----------------------------------------------------------------------------

  So, what exactly are we building? Our mission is to transform human-readable
  Lua source code into low-level Lua Bytecode. This bytecode is the
  secret language the Lua Virtual Machine (VM) understands and can execute directly!

  Here's the game plan, broken down into our compiler's main stages:

  =============================================================================

                              Tokenizer (Lexer)

      First, our code goes through the "reading" machine. It takes the raw
      string of code and breaks it down into meaningful chunks called tokens.
      These are the basic "words" and "punctuation" of our language, like
      `if`, `variableName`, `123`, `"+"`, `"hello"`. Whitespace and comments
      are discarded here - the VM doesn't need them!

  =============================================================================

                                Parser

      The parser takes the flat list of tokens from the tokenizer and figures
      out how they relate to each other, based on Lua's grammar rules.
      It builds a tree-like structure called the Abstract Syntax Tree (AST).
      The AST shows the hierarchical structure of the code - like figuring out
      the subject, verb, and object in a sentence.

  =============================================================================

                            Code Generator

      Now we take that structured AST and walk over it, translating each node
      (each part of the code's structure) into a sequence of simple,
      VM-friendly operations. These are called instructions in Lua's VM.

  =============================================================================

                        Compiler (Bytecode Emitter)

      This is the final packaging step! We take the list of VM instructions
      from the code generator and encode them into the compact, binary format
      known as Lua Bytecode. This is the exact format the Lua VM loads
      and executes.

  =============================================================================


  Each step refines the code's representation, getting it closer and closer
  to a form the Lua VM can gobble up and run!

  Alright, that's the roadmap! Let's start building our toolbox of helper
  functions before we dive into the first stage: the Tokenizer!

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
-- look ahead in the input stream and match the *longest possible* operator
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
                                (•_•) TOOLBOX TIME!
                          The Tokenizer's Constant Collection
    ============================================================================

    What's All This Then?

    Before we start scanning characters, we need our lexical tools - these
    constants act as the tokenizer's cheat sheet for recognizing Lua's special
    symbols and patterns. Think of them as the rulebook for our text-slicing game!
    They pre-define known entities like keywords, operators, and how to interpret
    escape sequences or character types efficiently.
--]]

--[[
  Master List of Lua Operators

  This table isn't used in the tokenizer itself, instead, it's being used
  as a template for other constants to be created from, like the operator trie
  and lookup table. It's a handy reference for all the operators in Lua.
--]]
local TOKENIZER_LUA_OPERATORS = {
  -- Single-character operators
  "^", "*", "/", "%", "+", "-", "<", ">", "#",

  -- Double-character operators
  "<=", ">=", "==", "~=", "..",
}

-- Logical keywords that act like operators
local TOKENIZER_LUA_KEYWORD_OPERATORS_LOOKUP = createLookupTable({"and", "or", "not"})

local TOKENIZER_CHARACTER_TYPES = {
  [":"] = "Colon",       [";"] = "Semicolon",
  [","] = "Comma",       ["."] = "Dot",
  ["("] = "LeftParen",   [")"] = "RightParen",
  ["{"] = "LeftBrace",   ["}"] = "RightBrace",
  ["["] = "LeftBracket", ["]"] = "RightBracket",
  ["="] = "Equals"
}

--[[
  Escape Sequence Decoder Ring

  When the tokenizer encounters a backslash '\' inside a string literal,
  it looks up the *next* character in this table to determine if it's the
  start of a known escape sequence (like '\n' for newline or '\\' for a
  literal backslash). This table provides the mapping from the escaped
  character (e.g., "n") to the actual character value (e.g., newline character).
  Numeric escape sequences (\ddd) are handled separately.
--]]
local TOKENIZER_ESCAPED_CHARACTER_CONVERSIONS = {
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
}

--[[
  Language Keyword Dictionary (Lookup)

  This lookup table (created from a list via `createLookupTable`) contains
  all of Lua 5.1's reserved keywords. These words have special grammatical
  meaning and *cannot* be used as identifiers (variable names, function names, etc.).
  We check against this table using an O(1) lookup after potentially identifying
  an identifier sequence to see if it's actually a reserved keyword.
--]]
local TOKENIZER_RESERVED_KEYWORDS_LOOKUP = createLookupTable({
  "break", "do",    "else",     "elseif",
  "end",   "for",   "function", "if",
  "in",    "local", "repeat",   "return",
  "then",  "until", "while",
})


--[[
  Constant Truths (and Falses, and Nils) Dictionary (Lookup)

  Similar to keywords, these represent the fixed, built-in constant values
  in Lua. They also function like reserved words in that they cannot be
  used as identifiers. This lookup table allows for fast checks after
  an identifier sequence is potentially matched.
--]]
local TOKENIZER_LUA_CONSTANTS_LOOKUP = createLookupTable({
  "true", "false", "nil"
})

--[[
  Operator Recognition Structures

  To efficiently identify operators, especially multi-character ones, we use:
    TOKENIZER_OPERATOR_TRIE: A prefix tree built from `TOKENIZER_LUA_OPERATORS`.
       Allows for quick lookahead to find the longest matching operator starting
       at the current character position.
--]]
local TOKENIZER_OPERATOR_TRIE = makeTrie(TOKENIZER_LUA_OPERATORS)

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
  - [eE]: Matches either 'e' or 'E', used for scientific notation.
  - [xX]: Matches either 'x' or 'X', used for hexadecimal number prefixes.

  Why pre-make these? Tokenization scans every character of the input code,
  making it a performance-sensitive phase. Using these pre-calculated
  lookups makes the fundamental character checks as fast as possible.
--]]
local TOKENIZER_SPACE_PATTERN_LOOKUP               = makePatternLookup("%s")         -- whitespace
local TOKENIZER_DIGIT_PATTERN_LOOKUP               = makePatternLookup("%d")         -- digits
local TOKENIZER_IDENTIFIER_PATTERN_LOOKUP          = makePatternLookup("[%a%d_]")    -- letters + digits + underscore
local TOKENIZER_IDENTIFIER_START_PATTERN_LOOKUP    = makePatternLookup("[%a_]")      -- letters + underscore
local TOKENIZER_HEX_PATTERN_LOOKUP                 = makePatternLookup("[%da-fA-F]") -- digits + hex letters (case insensitive)
local TOKENIZER_SCIENTIFIC_NOTATION_PATTERN_LOOKUP = makePatternLookup("[eE]")       -- 'e' or 'E' for scientific notation
local TOKENIZER_HEXADECIMAL_X_PATTERN_LOOKUP       = makePatternLookup("[xX]")       -- 'x' or 'X' for hexadecimal number prefix

--[[
  (•_•) Why This Matters?

  These constants are the tokenizer's essential rulebook and tools. By defining
  them clearly and using efficient data structures (lookup tables, trie, pattern lookups),
  we enable the tokenizer to rapidly scan the input text and correctly classify
  its building blocks. Separating these definitions from the processing logic
  makes the tokenizer module:
    Easier to understand: The rules are defined upfront.
    Simpler to maintain: Changes to operators or keywords happen in one place.
    More efficient: Pre-computed structures speed up the core scanning loop.
--]]

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
    individual words and punctuation marks, but for programming languages!

    The Key Role of Tokens:
      Tokens are the smallest, meaningful units the parser needs to understand.
      They represent categories like:
      - Identifiers: Names you create (variables, functions, labels) -> "x", "calculateTotal"
      - Keywords: Reserved words with special, fixed meaning -> "if", "while", "end"
      - Constants: Fixed, built-in values -> "true", "false", "nil"
      - Operators: Symbols or keywords for logic/math/operations -> "+", "==", "and", "not"
      - Literals: Raw data values embedded in the code -> numbers (42), strings ("hello")
      - Special Symbols/Separators: Structural characters -> "(", ")", ";", "=", ":", "."
      - Vararg: The special ellipsis symbol `...` used for variable-length argument lists.

    Tokenizer's Core Tasks:
      Scanning: Read the input character stream sequentially.
      Lexical Analysis: Group sequences of characters into known patterns.
      Token Identification: Determine the type and value of each matched group.
      Filtering: Discard elements not needed by the parser, primarily whitespace
        and comments.
      Handling Ambiguity: Apply rules like "longest match" (e.g., "==" vs "=")
        and context-sensitivity (e.g., recognizing numeric escapes in strings)
        to ensure correct token boundaries.

    TOKENIZATION IN ACTION (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧
      Let's trace a simple line: `if (x == 10) then`

      Output Tokens (Parser-Friendly Format):
        |-----------------|------------------|
        | Type            | Value            |
        |-----------------|------------------|
        | keyword         | if               |
        | character       | (                |
        | identifier      | x                |
        | operator        | ==               |
        | number          | 10               |
        | character       | )                |
        | keyword         | then             |
        |-----------------|------------------|

      Step-by-Step Process:
        1. Start at 'i'. Is it whitespace/comment? No.
        2. Is it a number start? No.
        3. Is it an identifier start? Yes ('i' is a letter). Consume while it's an identifier character ('f').
            Matched "if". Is "if" a keyword? Yes. -> Emit Keyword "if". Advance past "if".

        4. Next char is ' '. Is it whitespace? Yes. Consume whitespace. Advance past ' '.
        5. Next char is '('. Is it whitespace/comment/number/identifier/string/vararg? No.
           Is it an operator? Trie says no. Is it a character? Yes.
          Emit Character "(". Advance past '('.

        6. Next char is 'x'. Identifier start? Yes. Consume while identifier chars.
           Matched "x". Is "x" a keyword/constant/operator? No. -> Emit Identifier "x". Advance past "x".

        7. Next char is ' '. Whitespace? Yes. Consume whitespace. Advance past ' '.
        8. Next char is '='. Operator? Check trie. Path exists for '='. Look ahead. Next char is '='.
           Path exists for "=="? Yes. Longest match is "==". Consume "==".
           Emit Operator "==". Advance past "==".

        9.  Next char is ' '. Whitespace? Yes. Consume whitespace. Advance past ' '.
        10. Next char is '1'. Number start? Yes (it's a digit). Consume digits.
            Matched "10". Convert to number 10. -> Emit Number 10. Advance past "10".

        11. Next char is ')'. Character? Yes. -> Emit Character ")". Advance past ')'.
        12. Next char is ' '. Whitespace? Yes. Consume whitespace. Advance past ' '
        13. Next char is 't'. Identifier start? Yes. Consume while identifier chars.
             Matched "then". Keyword? Yes. -> Emit Keyword "then". Advance past "then".


      The tokenizer continues this loop until the entire input stream is consumed.
      The final output is the sequence of tokens the parser will work with.

      (◕‿◕) The tokenizer only cares about *what* the pieces are, not how they fit together!
--]]

--* Tokenizer *--

-- The main object responsible for scanning the source code string
-- and producing a list of tokens.
local Tokenizer = {}
Tokenizer.__index = Tokenizer -- Set up for method calls via `.`

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
  return TOKENIZER_SPACE_PATTERN_LOOKUP[char]
end

--- Checks if a given character is a newline character.
function Tokenizer:isNewlineCharacter(char)
  return char == "\n"
end

-- Checks if a given character is a digit (0-9).
function Tokenizer:isDigit(char)
  return TOKENIZER_DIGIT_PATTERN_LOOKUP[char]
end

-- Checks if a given character is a valid hexadecimal digit (0-9, a-f, A-F).
-- Used when scanning hexadecimal number literals (e.g., 0xFF).
function Tokenizer:isHexadecimalNumber(char)
  return TOKENIZER_HEX_PATTERN_LOOKUP[char]
end

-- Checks if a given character is valid as the *first* character of an identifier.
-- Identifiers must start with a letter or an underscore.
function Tokenizer:isIdentifierStart(char)
  return TOKENIZER_IDENTIFIER_START_PATTERN_LOOKUP[char]
end

-- Checks if a given character is valid *after the first* character of an identifier.
-- Identifiers can contain letters, digits, or underscores.
function Tokenizer:isIdentifier(char)
  return TOKENIZER_IDENTIFIER_PATTERN_LOOKUP[char]
end

-- Checks if a given character is the letter 'e' or 'E', indicating the
-- potential start of the exponential part of a number (scientific notation).
function Tokenizer:isScientificNotationPrefix(char)
  return TOKENIZER_SCIENTIFIC_NOTATION_PATTERN_LOOKUP[char]
end

--// Multi-Character Checkers //--
-- These functions look at the current character and potentially the next few
-- characters to determine if they *collectively* form the start of a larger
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
    TOKENIZER_HEXADECIMAL_X_PATTERN_LOOKUP[nextChar] -- Must be followed by 'x' or 'X'
  )
end

-- Checks if the current character sequence is the vararg literal "...".
function Tokenizer:isVarArg()
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
    while self:isDigit(self.curChar) do
      self:consume(1)
    end
  end

  -- Exponential (scientific) notation case
  -- [eE][+-]?[0-9]+
  if self:isScientificNotationPrefix(self.curChar) then
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

  local convertedChar = TOKENIZER_ESCAPED_CHARACTER_CONVERSIONS[self.curChar]
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
    -- This is an edge case in the official lexer source
    --
    --  https://www.lua.org/source/5.1/llex.c.html#read_long_string

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
  local node = TOKENIZER_OPERATOR_TRIE
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
    local identifier = self:consumeIdentifier() -- Consume the sequence of valid identifier characters

    -- Check if the consumed identifier string is actually a reserved keyword,
    -- a built-in constant, or a keyword operator. This check is done *after*
    -- identifying the identifier shape because keywords/constants *are* identifier shapes.

    -- Reserved keywords (e.g., "if", "while", "function")
    if TOKENIZER_RESERVED_KEYWORDS_LOOKUP[identifier] then
      return { TYPE = "Keyword", Value = identifier }

    -- Keyword operators (e.g., "and", "or", "not")
    elseif TOKENIZER_LUA_KEYWORD_OPERATORS_LOOKUP[identifier] then
      -- This handles keyword operators like "and", "or", and "not".
      -- We must check for these *after* we've parsed them as an identifier,
      -- because they follow identifier rules (e.g., must be surrounded
      -- by whitespace or non-identifier characters), unlike symbolic
      -- operators like `+` which can be next to anything.
      return { TYPE = "Operator", Value = identifier }

    -- Lua constants (e.g., "true", "false", "nil")
    elseif TOKENIZER_LUA_CONSTANTS_LOOKUP[identifier] then
      -- Constants are often treated similarly to literals in parsing.
      return { TYPE = "Constant", Value = identifier }
    end

    -- Fallthrough: If it's not a keyword, constant, or keyword operator,
    -- it's a regular identifier (likely a variable or function name).
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
  if self:isVarArg() then
    self:consume(3) -- Consume the "..."
    return { TYPE = "VarArg" }
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
  local tokenType = TOKENIZER_CHARACTER_TYPES[curChar]
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
                              (⊙_☉) PARSER'S RULEBOOK!
    ============================================================================

    Alright, tokens have arrived from the Tokenizer. Now it's the Parser's job
    to figure out how they all fit together based on Lua's grammar rules.
    These constants are the rulebook - they tell us things like which operators
    stick tighter than others, what counts as a valid assignment target, and
    where code blocks end!
--]]

local PARSER_UNARY_OPERATOR_PRECEDENCE = 8

--[[
    (✧Д✧) Node Type Nightclub Guest List

    These lookup tables list specific AST node types that need special handling
    or checks during parsing:

    - MultiRet Nodes: These are expression types (like function calls or varargs)
                      that can potentially return more than one value. We need to
                      know this during parsing (especially for assignments or
                      table constructors) to correctly handle potential multiple
                      return values vs. single values or padding with nils.

    - LValue Nodes: These are expression types that are valid targets on the
                    Left Value side of an assignment (`=`). In Lua 5.1, this
                    is restricted to single variables or table accesses (indexed
                    or field access). `a = 1`, `t.x = 2`, `t[i] = 3` are valid.
                    `a + b = 4` is not.

    - Termination Keywords: These are keywords that signal the *end* of a code block
                            to the `parseCodeBlock` function (e.g., `end`, `else`, `until`).
--]]
local PARSER_MULTIRET_NODE_TYPES  = createLookupTable({ "FunctionCall", "VarArg" })
local PARSER_LVALUE_NODE_TYPES    = createLookupTable({ "Variable", "TableIndex" })
local PARSER_TERMINATION_KEYWORDS = createLookupTable({ "end", "else", "elseif", "until" })

--[[
    (☞ﾟヮﾟ)☞ Keyword-to-Parser Dispatch Table

    This table maps statement-starting keywords directly to the parser methods
    that handle them. Instead of a long `if/elseif` chain, `getNextNode` can
    simply look up the keyword in this table and call the corresponding function.
    This is cleaner, more efficient, and easier to extend with new keywords.
--]]
local PARSER_KEYWORD_HANDLERS = {
    ["break"]    = "parseBreak",
    ["do"]       = "parseDo",
    ["for"]      = "parseFor",
    ["function"] = "parseFunction",
    ["if"]       = "parseIf",
    ["local"]    = "parseLocal",
    ["repeat"]   = "parseRepeat",
    ["return"]   = "parseReturn",
    ["while"]    = "parseWhile"
}

--[[
    (╯°□°)╯︵ ┻━┻ Operator Precedence Madness!

    Who gets solved first in an expression? This table settles all arguments!
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
local PARSER_OPERATOR_PRECEDENCE = {
  ["+"]   = {6, 6},  ["-"]   = {6, 6},   -- Addition/Subtraction
  ["*"]   = {7, 7},  ["/"]   = {7, 7},   -- Multiplication/Division
  ["%"]   = {7, 7},                      -- Modulo
  ["^"]   = {10, 9},                     -- Exponentiation (Right-associative, binds tighter than unary)
  [".."]  = {5, 4},                      -- String Concatenation (Right-associative)
  ["=="]  = {3, 3},  ["~="]  = {3, 3},   -- Equality / Inequality
  ["<"]   = {3, 3},  [">"]   = {3, 3},   -- Less Than / Greater Than
  ["<="]  = {3, 3},  [">="]  = {3, 3},   -- Less Than or Equal / Greater Than or Equal
  ["and"] = {2, 2},                      -- Logical AND (Short-circuiting)
  ["or"]  = {1, 1}                       -- Logical OR (Short-circuiting)
}

--[[
    Operator classification lookups for quick checks.
    These help the parser quickly distinguish between unary and binary operators
    or check if a token is an operator at all.
--]]
local PARSER_LUA_UNARY_OPERATORS  = createLookupTable({ "-", "#", "not" })
local PARSER_LUA_BINARY_OPERATORS = createLookupTable({
    "+",  "-",   "*",  "/",
    "%",  "^",   "..", "==",
    "~=", "<",   ">",  "<=",
    ">=", "and", "or"
})

--[[
    ============================================================================
                        (ﾉ◕ヮ◕)ﾉ*:･ﾟ AST FACTORY ONLINE!
    ============================================================================

    This is where the magic happens! The Parser consumes the flat list of tokens
    from the Tokenizer and builds a hierarchical Abstract Syntax Tree (AST)
    that represents the structure and meaning of the code. Think of it as
    transforming raw ingredients (tokens) into a fully structured cake (AST)
    following our grammar rules as the recipe.

    [BEFORE - Token Soup]
    `local x = 10 + 20 * (3 - 4)` (as tokens with types/values)

    [AFTER - AST Cake (Simplified)]
    AST
    └─ Block
        └─ LocalDeclaration
            ├─ Variables: ["x"]
            └─ Expressions: [ -- Single expression for '10 + ...'
                BinaryOperator {
                    Operator: "+",
                    Left: Number(10),
                    Right: BinaryOperator {
                        Operator: "*",
                        Left: Number(20),
                        Right: BinaryOperator {
                            Operator: "-",
                            Left: Number(3),
                            Right: Number(4)
                        }
                    }
                }
            ]

    General tips:
      - The structure of the AST directly reflects the grouping dictated by
        operator precedence and parentheses.
      - Recursive descent is like solving math problems by breaking them
        into smaller, self-similar problems (remember PEMDAS? That's related
        to the precedence climbing part!).
--]]

--* Parser *--
-- The main object responsible for consuming tokens and building the AST.
local Parser = {}
Parser.__index = Parser -- Set up for method calls via `.`

--// Parser Constructor //--
-- Creates a new Parser instance given a list of tokens from the Tokenizer.
function Parser.new(tokens)
  --// Type Checking //--
  assert(type(tokens) == "table", "Parser.new requires a table of tokens. Got: " .. type(tokens))

  --// Instance Creation //--
  local ParserInstance = setmetatable({}, Parser)

  --// Initialization //--
  ParserInstance.tokens = tokens
  -- Point to the first token to start parsing
  ParserInstance.currentTokenIndex = 1
  ParserInstance.currentToken = tokens[1]
  -- Initialize the stack for managing variable scopes (local, upvalue, global)
  ParserInstance.scopeStack = {}
  ParserInstance.currentScope = nil -- Pointer to the topmost scope on the stack

  return ParserInstance
end

--// Token Navigation //--

-- Looks ahead by 'n' tokens in the token stream *without* advancing
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
-- Returns the *new* current token after consumption.
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

-- Consumes the current token, but only if its type AND value match the expectation.
-- Used for consuming specific keywords or characters (like `end`, `(`, `=`).
-- Throws a detailed error with token location if the expectation is not met.
-- This is a strong check for expected syntax elements.
function Parser:consumeToken(tokenType, tokenValue)
  local token = self.currentToken
  -- Check if the current token exists and matches the expected type and value
  if token and token.TYPE == tokenType and token.Value == tokenValue then
    return self:consume(1) -- Match! Consume this token and return the *next* one.
  end

  -- If we didn't match, throw a syntax error.
  self:error(string.format(
    "Expected %s [%s] token, got: %s [%s]",
    tostring(tokenType), tostring(tokenValue), tostring(token.TYPE), tostring(token.Value)
  ))
end

-- Helper to report parsing errors.
-- All error calls should ideally go through this for consistent reporting.
function Parser:error(message)
  -- Homework idea: Add location information to the error message
  -- (e.g., line number, column number, etc.) for better debugging.

  error(string.format(
    "Parser Error: %s",
    message
  ))
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
    localVariables = {}, -- Table to track local variables declared *in this scope*
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

    -- If this scope is a function scope, any variable found *further out*
    -- will be an upvalue relative to subsequent nested function scopes.
    -- We set this flag *after* checking locals in the current function scope.
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

-- Checks if the given `token` (or `self.currentToken`) is a 'Keyword'
-- token with the specified `keyword` value.
function Parser:checkKeyword(keyword, token)
  token = token or self.currentToken
  return token
        and token.TYPE  == "Keyword"
        and token.Value == keyword
end

-- Checks if the given `token` (or `self.currentToken`) is the comma character ','.
-- Used frequently in parsing lists (argument lists, variable lists, table elements).
function Parser:isComma(token)
  return token and token.TYPE == "Comma"
end

-- Checks if the given `token` (or `self.currentToken`) is a recognized unary operator.
-- Uses the `PARSER_LUA_UNARY_OPERATORS` lookup table.
function Parser:isUnaryOperator(token)
  return token
        and token.TYPE == "Operator"
        and PARSER_LUA_UNARY_OPERATORS[token.Value]
end

-- Checks if the given `token` (or `self.currentToken`) is a recognized binary operator.
-- Uses the `PARSER_LUA_BINARY_OPERATORS` lookup table.
function Parser:isBinaryOperator(token)
  return token
        and token.TYPE == "Operator"
        and PARSER_LUA_BINARY_OPERATORS[token.Value]
end

--// Token Expectation //--
-- These functions check the *current* token's type or value without consuming it.
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
  self:error(string.format("Expected a %s, but found %s", expectedType,
    tostring(actualType)))
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
  self:error(string.format(
    "Expected keyword '%s', but found %s [%s]",
    keyword, tostring(token and token.TYPE), tostring(token and token.Value)
  ))
end

--// AST Node Checkers //--
-- Functions to check properties of already-parsed AST nodes.

-- Checks if a given AST `node` is a valid target for the left-hand side of an assignment.
-- Valid LValues are single variables or table access expressions.
-- Uses the `PARSER_LVALUE_NODE_TYPES` lookup table.
function Parser:isValidAssignmentLvalue(node)
  return node and PARSER_LVALUE_NODE_TYPES[node.TYPE]
end

-- Checks if a given AST `node` is a type that can return multiple values in Lua 5.1.
-- This includes function calls and the vararg literal (...).
-- Used to determine if the last expression in a list might return multiple values
-- that need to be handled specially by the compiler (OP_CALL/OP_VARARG with B=0).
function Parser:isMultiretNode(node)
  return node and PARSER_MULTIRET_NODE_TYPES[node.TYPE]
end

--// Auxiliary Functions //--
-- Small helper functions for creating common AST node types or manipulating node lists.

-- Creates a simple AST node representing the Lua `nil` constant.
function Parser:createNilNode()
  return { TYPE = "Constant", Value = "nil" }
end

-- Adjusts a list of expression nodes to match an `expectedReturnAmount`.
-- This handles Lua's multi-return semantics for assignments and list constructors.
-- If the `expectedReturnAmount` is positive:
--   - If the last node is multi-ret, its `ReturnValueAmount` is set to `expectedReturnAmount - #expressions + 1`
--      (where -1 means all values are returned).
--   - Otherwise, the list is padded with `nil` nodes until it reaches `expectedReturnAmount`.
--
-- If `expectedReturnAmount` is -1 (or any negative number typically meaning 'return all'):
--   - If the last node is multi-ret, its `ReturnValueAmount` is set to -1 (return all).
--   - Otherwise, no padding is needed, as single-return values are sufficient.
--
-- (You're not required to fully understand this function, but it's important to know it exists!)
function Parser:adjustMultiretNodes(expressionList, expectedReturnAmount)
  local listLength = #expressionList
  local lastNode = expressionList[listLength]

  -- Check if the last node is a multi-ret producer
  if self:isMultiretNode(lastNode) then
    -- Calculate the number of extra returns needed (beyond the fixed arguments already present)
    -- For assignments/calls expecting a fixed number N, the last multi-ret expression
    -- should return N minus the number of expressions *before* it.
    -- For cases expecting "all" (-1), the multi-ret expression should return all.
    local numFixedArgsBefore = listLength - 1
    local adjustedReturnAmount = expectedReturnAmount - numFixedArgsBefore

    -- Set the ReturnValueAmount on the multi-ret node.
    -- Lua's OP_CALL/OP_VARARG use C-1 for the number of results,
    -- so a C of 0 means "return all". In our AST, -1 will also mean "return all".
    -- The C operand is typically `expected returns + 1` (0 for all), but here we
    -- set the *number of results*, which the code generator will convert.
    lastNode.ReturnValueAmount = adjustedReturnAmount -- Code generator will handle the +1 or 0 mapping
  elseif expectedReturnAmount > listLength then
    -- If the list is shorter than expected and the last node is NOT multi-ret,
    -- pad the list with `nil` nodes to meet the expected count.
    local neededPadding = expectedReturnAmount - listLength
    for _ = 1, neededPadding do
      table.insert(expressionList, self:createNilNode())
    end

    -- If the list is longer than expected, the extra values are simply discarded by the assignment/call context.
    -- If the list is exactly the expected length and the last node is not multi-ret, nothing is needed.
  end
end

--// Parsers //--
-- These functions are responsible for parsing specific syntactic constructs
-- in the token stream and building the corresponding AST nodes.

function Parser:consumeIdentifier()
  local identifierValue = self:expectTokenType("Identifier").Value
  return identifierValue
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
    local currentToken = self:consume(1)
    -- Check if the current token is a comma. If not, the list ends after the current identifier.
    if not self:isComma(currentToken) then break end
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
  local isVarArg = false -- Flag for varargs

  -- Loop as long as the current token is NOT the closing parenthesis
  while self.currentToken and not self:checkTokenType("RightParen") do
    -- Check for regular named parameters (must be identifiers)
    if self.currentToken.TYPE == "Identifier" then
      -- Add the parameter name to the list
      table.insert(parameters, self.currentToken.Value)
      self:consume(1) -- Consume the identifier token

    -- Check for the vararg token "..."
    elseif self.currentToken.TYPE == "VarArg" then
      isVarArg = true -- Set the vararg flag
      self:consumeToken("VarArg") -- Expect and consume the "..." token
      -- According to Lua grammar, "..." must be the last parameter.
      break -- Exit the loop after consuming vararg

    -- If it's neither an identifier nor vararg, it's a syntax error.
    else
       self:error("Expected parameter name or '...' in parameter list, but found: " .. tostring(self.currentToken.TYPE))
    end

    -- After a parameter, we expect a comma, unless it's the last parameter before the closing parenthesis.
    -- Check if the current token is a comma. If not, break the loop (assuming it's the closing paren).
    if not self:isComma(self.currentToken) then break end
    self:consume(1) -- Consume the comma

  end -- Loop ends when ')' is encountered or after consuming '...'

  self:consumeToken("RightParen") -- Expect and consume the closing parenthesis
  return parameters, isVarArg
end

-- Parses a table field access using dot notation (`table.identifier`).
-- Expects the current token to be the '.' character.
-- Creates a `TableIndex` AST node. The 'Index' field of this node is a String literal
-- holding the identifier name (as per Lua VM instruction format).
-- `currentExpression` is the AST node representing the table being accessed (e.g., a Variable or another TableIndex).
function Parser:consumeTableIndex(currentExpression)
  self:consumeToken("Dot") -- Expect and consume the dot '.'
  -- The token immediately after '.' MUST be an identifier according to Lua syntax.
  local indexValue = self:consumeIdentifier()

  -- Create the AST node for the table index access.
  return {
    TYPE  = "TableIndex",
    -- The index in dot notation is treated as a string constant in the Lua VM.
    Index = {
      TYPE  = "String",
      Value = indexValue
    },
    Expression = currentExpression
  }
end

-- Parses a table element access using bracket notation (`[expression]`).
-- Expects the current token to be the '[' character.
-- Creates a `TableIndex` AST node. The 'Index' field of this node is
-- the AST node for the expression within the brackets.
-- `currentExpression` is the AST node representing the table being accessed.
function Parser:consumeBracketTableIndex(currentExpression)
  self:consumeToken("LeftBracket") -- Expect and consume the '[' character

  -- Parse the expression that defines the index inside the brackets.
  local indexExpression = self:consumeExpression()
  if not indexExpression then
    self:error("Expected an expression inside brackets for table index")
  end

  -- Expect and consume the closing bracket ']'
  self:expectTokenType("RightBracket")

  -- Create the AST node for the table index access.
  return {
    TYPE       = "TableIndex",
    Index      = indexExpression,
    Expression = currentExpression
  }
end

function Parser:consumeTable()
  self:consumeToken("LeftBrace") -- Consume the "{"

  local implicitElements      = {}
  local explicitElements      = {}
  local nextImplicitKey       = 1
  local isLastElementImplicit = false
  local lastElement

  -- Loop through tokens until we find the closing "}"
  while self.currentToken and not self:checkTokenType("RightBrace") do
    local key, value
    local isImplicitKey = false

    -- Determine which type of table field we are parsing.
    if self:checkTokenType("LeftBracket") then
      -- Explicit key in brackets, e.g., `[1+2] = "value"`
      self:consume(1) -- Consume "["
      key = self:consumeExpression()
      self:expectTokenType("RightBracket")
      self:expectTokenType("Equals")
      value = self:consumeExpression()

    elseif self:checkTokenType("Identifier") and self:checkTokenType("Equals", self:lookAhead(1)) then
      -- Identifier key, e.g., `name = "value"`
      -- This is syntatic sugar for `["name"] = "value"`.
      key = { TYPE  = "String", Value = self.currentToken.Value }
      self:consume(2) -- Consume the identifier and the "="
      value = self:consumeExpression()

    else
      -- Implicit numeric key, e.g. `"value1", MY_VAR, 42`
      -- This is syntatic sugar for `[1] = "value1", [2] = MY_VAR, [3] = 42`.
      isImplicitKey = true
      key = { TYPE  = "Number", Value = nextImplicitKey }
      nextImplicitKey = nextImplicitKey + 1
      value = self:consumeExpression()
    end

    -- Create the AST node for this table element.
    local element = { TYPE = "TableElement", Key = key, Value = value}
    lastElement = element

    -- Separate implicit and explicit elements for the code generator.
    if isImplicitKey then
      isLastElementImplicit = true
      table.insert(implicitElements, element)
    else
      isLastElementImplicit = false
      table.insert(explicitElements, element)
    end

    -- Table elements can be separated by "," or ";". If no separator is
    -- found, we assume it's the end of the table definition.
    if not (self:checkTokenType("Comma") or self:checkTokenType("Semicolon")) then
      break
    end
    self:consume(1) -- Consume the separator.
  end

  -- Handle multi-return semantics for the last element if it's implicit.
  -- e.g., `t = {a, b, f() }` where `f()` can return multiple values.
  if isLastElementImplicit and self:isMultiretNode(lastElement.Value) then
    -- Signal the code generator to make the last element return all values (not just one)
    -- "-1" stands for "all"
    lastElement.Value.ReturnValueAmount = -1
  end

  self:consumeToken("RightBrace") -- Consume the "}" symbol

  return {
    TYPE             = "Table",
    ImplicitElements = implicitElements,
    ExplicitElements = explicitElements
  }
end

function Parser:consumeFunctionCall(currentExpression, isMethodCall)
  self:consumeToken("LeftParen") -- Expect and consume the left parenthesis '('
  local arguments = self:consumeExpressions()
  self:adjustMultiretNodes(arguments, -1)
  self:consumeToken("RightParen") -- Expect and consume the right parenthesis ')'

  return {
    TYPE              = "FunctionCall",
    Expression        = currentExpression,
    Arguments         = arguments,
    IsMethodCall      = isMethodCall and true,
    IsTailcall        = false,
    ReturnValueAmount = 1
  }
end

function Parser:consumeImplicitFunctionCall(lvalue)
  local currentToken     = self.currentToken
  local currentTokenType = currentToken.TYPE
  local arguments = {}

  -- `print "hello, world"` case
  if currentTokenType == "String" then
    arguments = { {
      TYPE  = "String",
      Value = currentToken.Value
    } }
    self:consume(1) -- Consume the string

  -- `print {1, 2, 3}` case
  elseif currentTokenType == "LeftBrace" then
    arguments = { self:consumeTable() }
  end

  return {
    TYPE              = "FunctionCall",
    Expression        = lvalue,
    Arguments         = arguments,
    IsMethodCall      = false,
    IsTailcall        = false,
    ReturnValueAmount = 1
  }
end

function Parser:consumeMethodCall(currentExpression)
  self:consumeToken("Colon") -- Expect and consume the colon (':')
  local methodIdentifier = self:consumeIdentifier()

  -- Convert the `table:method` part to an AST node
  local methodIndexNode = {
    TYPE  = "TableIndex",
    Index = {
      TYPE  = "String",
      Value = methodIdentifier
    },
    Expression = currentExpression
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

  if     tokenType == "Number"   then self:consume(1) return { TYPE = "Number",   Value = tokenValue    }
  elseif tokenType == "String"   then self:consume(1) return { TYPE = "String",   Value = tokenValue    }
  elseif tokenType == "Constant" then self:consume(1) return { TYPE = "Constant", Value = tokenValue    }
  elseif tokenType == "VarArg"   then self:consume(1) return { TYPE = "VarArg",   ReturnValueAmount = 1 }

  -- Variable, e.g., `MY_VAR`.
  elseif tokenType == "Identifier" then
    local variableType = self:getVariableType(tokenValue)
    self:consume(1)
    return {
      TYPE         = "Variable",
      Name         = tokenValue,
      VariableType = variableType
    }

  -- Parenthesized expression, e.g., `(a + b)`.
  elseif tokenType == "LeftParen" then
    self:consume(1) -- Consume '('
    local expression = self:consumeExpression()
    self:expectTokenType("RightParen") -- Expect and consume ')'

    -- The "ParenthesizedExpr" node is NOT just for operator precedence.
    -- In Lua, parentheses also force a multi-return expression to adjust to
    -- a single value. e.g., `a, b = f()` is different from `a, b = (f())`.
    -- Therefore, we MUST keep this wrapper node in the AST.
    return {
        TYPE       = "ParenthesizedExpr",
        Expression = expression
    }

  -- Table constructor, e.g., `{ 42, "string", ["my"] = variable }`
  elseif tokenType == "LeftBrace" then
    return self:consumeTable()

  -- Check if it's a keyword for anonymous function
  elseif tokenType == "Keyword" then
    -- Anonymous function, e.g., `function(arg1) ... end`
    if tokenValue == "function" then
      self:consume(1) -- Consume 'function'
      local parameters, isVarArg = self:consumeParameters()
      local codeblock = self:parseCodeBlock(true, parameters)
      self:expectKeyword("end")
      return {
        TYPE       = "Function",
        CodeBlock  = codeblock,
        Parameters = parameters,
        IsVarArg   = isVarArg
      }
    end
  end

  -- If no primary expression pattern is matched, return nil.
  return nil
end

function Parser:parseSuffixExpression(primaryExpression)
  local currentToken = self.currentToken
  if not currentToken then return nil end

  local currentTokenType = currentToken.TYPE
  if currentTokenType == "LeftParen" then -- Function call
    -- <expression>(<args>)
    return self:consumeFunctionCall(primaryExpression)
  elseif currentTokenType == "Dot" then -- Table access
    -- <expression>.<identifier>
    return self:consumeTableIndex(primaryExpression)
  elseif currentTokenType == "Colon" then -- Method call
    -- <expression>:<identifier>(<args>)
    return self:consumeMethodCall(primaryExpression)
  elseif currentTokenType == "LeftBracket" then -- Table index
    -- <expression>[<expression>]
    return self:consumeBracketTableIndex(primaryExpression)
  else
    -- In some edge cases, a user may call a function using only string,
    -- example: `print "Hello, World!"`. This is a valid Lua syntax.
    -- Let's handle both strings and tables here for that case.
    if currentTokenType == "String" or currentTokenType == "LeftBrace" then
      return self:consumeImplicitFunctionCall(primaryExpression)
    end
  end

  return nil
end

function Parser:parsePrefixExpression()
  local primaryExpression = self:parsePrimaryExpression() -- <primary>
  if not primaryExpression then return end

  -- <suffix>*
  while (true) do
    local newExpression = self:parseSuffixExpression(primaryExpression)
    if not newExpression then break end
    primaryExpression = newExpression
  end

  return primaryExpression
end

function Parser:parseUnaryOperator()
  local operator = self.currentToken

  -- <unary> ::= <unary operator> <unary> | <primary>
  if not self:isUnaryOperator(operator) then
    return self:parsePrefixExpression()
  end

  -- <unary operator> <unary>
  self:consume(1) -- Consume the operator
  local expression = self:parseBinaryExpression(PARSER_UNARY_OPERATOR_PRECEDENCE)
  if not expression then self:error("Unexpected end") end

  return {
    TYPE     = "UnaryOperator",
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
    local precedence = operatorToken and PARSER_OPERATOR_PRECEDENCE[operatorToken.Value]
    if not self:isBinaryOperator(operatorToken) or precedence[1] <= minPrecedence then
      break
    end

    -- The <binary operator> <binary> part itself
    local nextToken = self:consume(1) -- Advance to and consume the operator
    if not nextToken then self:error("Unexpected end") end

    local right = self:parseBinaryExpression(precedence[2])
    if not right then self:error("Unexpected end") end

    expression = {
      TYPE     = "BinaryOperator",
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
  -- Make an expression list and consume the first expression
  local expressionList = { self:consumeExpression() }
  if #expressionList == 0 then return expressionList end

  -- From now on, consume upcoming expressions
  -- only if they are separated by a comma
  while self:isComma(self.currentToken) do
    self:consume(1) -- Consume the comma (',')
    local expression = self:consumeExpression()
    table.insert(expressionList, expression)
  end

  return expressionList
end

--// STATEMENT PARSERS //--
function Parser:parseLocal()
  -- local <ident_list> [= <expr_list>]
  -- local function <ident>(<params>) <codeblock> end
  self:consumeToken("Keyword", "local")

  -- local function <ident>(<params>) <codeblock> end
  if self:checkKeyword("function") then
    self:consumeToken("Keyword", "function")
    local functionName = self:consumeIdentifier()
    local parameters, isVarArg = self:consumeParameters()
    self:declareLocalVariable(functionName)
    local codeblock = self:parseCodeBlock(true, parameters)
    self:expectKeyword("end")
    return {
      TYPE       = "LocalFunctionDeclaration",
      Name       = functionName,
      CodeBlock  = codeblock,
      Parameters = parameters,
      IsVarArg   = isVarArg
    }
  else
    -- local <ident_list> [= <expr_list>]
    local variables = self:consumeIdentifierList()
    local expressions = {}

    -- Check for optional expressions
    if self:checkTokenType("Equals") then
      self:consume(1) -- Consume the equals sign (`=`)
      expressions = self:consumeExpressions()
      self:adjustMultiretNodes(expressions, #variables)
    end

    self:declareLocalVariables(variables)
    return {
      TYPE        = "LocalDeclaration",
      Variables   = variables,
      Expressions = expressions
    }
  end
end

function Parser:parseWhile()
  -- while <condition_expr> do <codeblock> end
  self:consumeToken("Keyword", "while")
  local condition = self:consumeExpression()
  self:expectKeyword("do")
  local codeblock = self:parseCodeBlock()
  self:expectKeyword("end")

  return {
    TYPE      = "WhileLoop",
    Condition = condition,
    CodeBlock = codeblock
  }
end

function Parser:parseRepeat()
  -- repeat <codeblock> until <condition_expr>
  self:consumeToken("Keyword", "repeat")
  self:enterScope()
  -- Note: There's a Lua edge case which allows local variables declared
  -- inside the `repeat ... until` block to be used in the `until` condition.
  -- Therefore, we enter the scope before parsing the code block,
  -- but we only exit the scope after parsing the condition.
  local codeblock = self:parseCodeBlockInCurrentScope()
  -- Don't exit the scope yet as its variables can still be used in the condition.
  self:consumeToken("Keyword", "until")
  local condition = self:consumeExpression()
  self:exitScope()

  return {
    TYPE      = "RepeatLoop",
    CodeBlock = codeblock,
    Condition = condition
  }
end

function Parser:parseDo()
  -- do <codeblock> end
  self:consumeToken("Keyword", "do")
  local codeblock = self:parseCodeBlock()
  self:expectKeyword("end")

  return {
    TYPE      = "DoBlock",
    CodeBlock = codeblock
  }
end

function Parser:parseReturn()
  -- return <expr_list>
  self:consumeToken("Keyword", "return")
  local expressions = self:consumeExpressions()
  self:adjustMultiretNodes(expressions, -1)

  -- Check if the return statement has only one expression and it's a "FunctionCall" node,
  -- if it is, we mark the function call with "IsTailcall" flag to give a hint to
  -- the code generator to make it generate TAILCALL instruction instead, which is
  -- faster than CALL.
  local lastExpression = expressions[#expressions]
  if lastExpression and lastExpression.TYPE == "FunctionCall" then
    lastExpression.IsTailcall = true
  end

  return {
    TYPE        = "ReturnStatement",
    Expressions = expressions
  }
end

function Parser:parseBreak()
  -- break
  self:consumeToken("Keyword", "break")
  return { TYPE = "BreakStatement" }
end

function Parser:parseIf()
  -- if <condition_expr> then <codeblock>
  --  [elseif <condition_expr> then <codeblock>]*
  --  [else <codeblock>]
  -- end
  self:consumeToken("Keyword", "if")
  local ifCondition = self:consumeExpression()
  self:consumeToken("Keyword", "then")
  local ifCodeBlock = self:parseCodeBlock()
  local branches = {
    TYPE = "IfBranchList",
    {
      TYPE      = "IfBranch",
      Condition = ifCondition,
      CodeBlock = ifCodeBlock
    }
  }
  while self:checkKeyword("elseif") do
    -- Consume the "elseif" token
    self:consumeToken("Keyword", "elseif")
    local elseifCondition = self:consumeExpression()
    self:expectKeyword("then")
    local elseifCodeBlock = self:parseCodeBlock()
    local ifBranch = {
      TYPE      = "IfBranch",
      Condition = elseifCondition,
      CodeBlock = elseifCodeBlock
    }
    table.insert(branches, ifBranch)
  end

  local elseCodeBlock
  if self:checkKeyword("else") then
     self:consumeToken("Keyword", "else")
    elseCodeBlock = self:parseCodeBlock()
  end
  self:expectKeyword("end")

  return {
    TYPE          = "IfStatement",
    Branches      = branches,
    ElseCodeBlock = elseCodeBlock
  }
end

function Parser:parseFor()
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

    -- Adjust the expressions to account for the generator, state, and control expressions.
    self:adjustMultiretNodes(expressions, 3)

    -- Parse the loop body.
    self:consumeToken("Keyword", "do")
    local codeblock = self:parseCodeBlock(false, iteratorVariables)
    self:expectKeyword("end")

    return {
      TYPE              = "GenericForLoop",
      IteratorVariables = iteratorVariables,
      Expressions       = expressions,
      CodeBlock         = codeblock
    }
  else
    -- It's a numeric 'for' loop.

    -- Parse the '=' and the loop expressions.
    self:consumeToken("Equals")
    local expressions = self:consumeExpressions()

    -- Parse the loop body.
    self:consumeToken("Keyword", "do")
    local codeblock = self:parseCodeBlock(false, { variableName })
    self:expectKeyword("end")

    return {
      TYPE         = "NumericForLoop",
      VariableName = variableName,
      Expressions  = expressions,
      CodeBlock    = codeblock
    }
  end
end

function Parser:parseFunction()
  -- function <name>[.<field>]*[:<method>](<params>) <codeblock> end
  self:consumeToken("Keyword", "function")

  -- Parse the base function name (e.g., `myFunc` in `myFunc.new`)
  local variableName = self:consumeIdentifier()
  local variableType = self:getVariableType(variableName)
  local expression = {
    TYPE         = "Variable",
    Name         = variableName,
    VariableType = variableType
  }

  -- Parse the chain of fields and an optional method name.
  -- This loop handles `.field` and `:method` parts
  local fields, isMethodCall = { }, false
  while self.currentToken do
    local isDot = self:checkTokenType("Dot")
    local isColon = self:checkTokenType("Colon")

    if not (isDot or isColon) then
      break -- The name chain has ended.
    end

    self:consume(1) -- Consume the `.` or `:`
    local fieldName = self:consumeIdentifier()
    table.insert(fields, fieldName)

    if isColon then
      isMethodCall = true
      break -- A method call must be the last part of the name.
    end
  end

  -- Parse the parameter list.
  local parameters, isVarArg = self:consumeParameters()
  if isMethodCall then
    -- For method calls (`:`) implicitly add "self" as the first parameter.
    table.insert(parameters, 1, "self")
  end

  -- Parse the function body and construct the final AST node.
  local codeblock = self:parseCodeBlock(true, parameters)
  self:expectKeyword("end")
  return {
    TYPE         = "FunctionDeclaration",
    Expression   = expression,
    Fields       = fields,
    IsMethodCall = isMethodCall,
    CodeBlock    = codeblock,
    Parameters   = parameters,
    IsVarArg     = isVarArg
  }
end

function Parser:parseAssignment(lvalue)
  -- <lvalue_list> = <expr_list>

  -- An assignment statement looks like: lvalue, lvalue, ... = expr, expr, ...
  -- We've already parsed the first lvalue. Now we parse the rest of the list.
  local lvalues = { lvalue }
  while self:isComma(self.currentToken) do
    self:consume(1) -- Consume the `,`

    -- Parse the next potential lvalue in the list.
    local nextLValue = self:parsePrefixExpression()

    -- Validate that what we parsed is actually a valid assignment target (lvalue).
    if not self:isValidAssignmentLvalue(nextLValue) then
       local nodeType = tostring(nextLValue and nextLValue.TYPE)
      self:error("Invalid assignment target. Expected a variable or table index, but got " .. nodeType)
    end

    table.insert(lvalues, nextLValue)
  end

  -- Now that we have all the lvalues, we expect the `=` sign.
  self:expectTokenType("Equals")

  -- Parse the list of expressions on the right-hand side.
  local expressions = self:consumeExpressions()

  -- Handle Lua's multi-return semantics. For example, in `a, b = f()`,
  -- `f()` might return two values. In `a, b, c = f()`, `f()` should provide
  -- two values and the third (`c`) will be nil.
  self:adjustMultiretNodes(expressions, #lvalues)

  -- Construct and return the AST node for the assignment.
  return {
    TYPE        = "VariableAssignment",
    LValues     = lvalues,
    Expressions = expressions
  }
end

function Parser:parseFunctionCallOrVariableAssignment()
  -- This function handles statements that begin with an expression.
  -- In Lua, these can only be variable assignments or function calls.
  local expression = self:parsePrefixExpression()

  if not expression then
    self:error("Invalid statement: expected a variable or function call.")
  end

  -- Check if it's a function call statement (e.g., `myFunc()`).
  --- @diagnostic disable-next-line: need-check-nil
  if expression.TYPE == "FunctionCall" then
    -- When a function call is a statement, its return values are discarded.
    expression.ReturnValueAmount = 0
    return expression
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
  self:error("Invalid statement: syntax error near '" .. tostring(expression.TYPE).. "'. Only function calls and assignments can be statements.")
end

--// CODE BLOCK PARSERS //--

-- Parses the next statement in a code block.
-- This function acts as a dispatcher, determining which specific parsing
-- function to call based on the current token.
function Parser:getNextNode()
  local currentToken = self.currentToken
  if not currentToken then return end

  local node
  if self:checkTokenType("Keyword") then
    local keyword = currentToken.Value

    -- First, check for keywords that terminate a block. If found, we stop
    -- parsing this block and let the parent parser handle the keyword.
    if PARSER_TERMINATION_KEYWORDS[keyword] then
      return nil -- Signal to stop parsing the current block.
    end

    local handlerName = PARSER_KEYWORD_HANDLERS[keyword]
    if handlerName then
      node = self[handlerName](self) -- e.g., self:parseWhile().
    else
      self:error("Unsupported keyword used as a statement starter: " .. keyword)
    end
  else
    -- If the statement doesn't start with a keyword, it must be a
    -- variable assignment or a function call.
    node = self:parseFunctionCallOrVariableAssignment()
  end

  -- Statements can optionally be separated by a semicolon.
  self:consumeOptionalSemicolon()

  return node
end

-- A special function needed for the `repeat-until` statement edge case
-- Unlike in the :parseCodeBlock() method, we don't push scope here as
-- local variables defined inside `repeat-until` statement can still be
-- used in the `until`s expression.
function Parser:parseCodeBlockInCurrentScope()
  local nodeList = { TYPE = "Block" }
  while self.currentToken do
    -- Parse statements one by one until a terminator is found.
    local node = self:getNextNode()
    if not node then
      break -- A terminating keyword was found.
    end

    table.insert(nodeList, node)
  end

  return nodeList
end

function Parser:parseCodeBlock(isFunctionScope, codeBlockVariables)
  -- Each block gets its own variable scope.
  self:enterScope(isFunctionScope)
  if codeBlockVariables then
    -- Pre-declare variables for this scope (e.g. function parameters, for-loop variables).
    self:declareLocalVariables(codeBlockVariables)
  end

  local nodeList = { TYPE = "Block" }
  while self.currentToken do
    -- Parse statements one by one until a terminator is found.
    local node = self:getNextNode()
    if not node then
      break -- A terminating keyword was found.
    end

    table.insert(nodeList, node)
  end

  self:exitScope()
  return nodeList
end

--// MAIN //--
function Parser:parse()
  local ast = self:parseCodeBlock()
  ast.TYPE = "AST"

  return ast
end

--[[
    ============================================================================
                                     (•_•)?
                              CODE GENERATOR CONSTANTS
    ============================================================================

    Before diving into the compiler's implementation, let's explore the essential
    constants and lookup tables that will guide the compilation process. These
    constants include Lua operators, unary operators, and termination keywords.
    By defining these constants upfront, we can streamline the compilation logic
    and ensure accurate identification and classification of tokens within the
    Lua code.
--]]

local unpack = (unpack or table.unpack)

local COMPILER_MIN_STACK_SIZE = 2   -- Registers 0/1 are always valid
local COMPILER_MAX_REGISTERS  = 250 -- 200 variables, 50 temp (5 reserved for safety)

local COMPILER_SETLIST_MAX = 50
local COMPILER_ARITHMETIC_OPERATOR_LOOKUP = {
  ["+"] = "ADD", ["-"] = "SUB",
  ["*"] = "MUL", ["/"] = "DIV",
  ["%"] = "MOD", ["^"] = "POW"
}
local COMPILER_UNARY_OPERATOR_LOOKUP = { ["-"] = "UNM", ["#"] = "LEN", ["not"] = "NOT" }
local COMPILER_COMPARISON_INSTRUCTION_LOOKUP = {
  ["=="] = {"EQ", 1}, ["~="] = {"EQ", 0},
  ["<"]  = {"LT", 1}, [">"]  = {"LT", 1},
  ["<="] = {"LE", 1}, [">="] = {"LE", 1}
}
local COMPILER_COMPARISON_OPERATOR_LOOKUP = createLookupTable({"==", "~=", "<", ">", "<=", ">="})
local COMPILER_CONTROL_FLOW_OPERATOR_LOOKUP = createLookupTable({"and", "or"})

local COMPILER_EXPRESSION_HANDLERS = {
  ["BinaryOperator"] = "compileBinaryOperatorNode",
  ["Constant"]       = "compileConstantNode",
  ["Function"]       = "compileFunctionNode",
  ["FunctionCall"]   = "compileFunctionCallNode",
  ["Number"]         = "compileNumberNode",
  ["String"]         = "compileStringNode",
  ["Table"]          = "compileTableNode",
  ["TableIndex"]     = "compileTableIndexNode",
  ["UnaryOperator"]  = "compileUnaryOperatorNode",
  ["VarArg"]         = "compileVarArgNode",
  ["Variable"]       = "compileVariableNode"
}

local COMPILER_STATEMENT_HANDLERS = {
  ["BreakStatement"]           = "compileBreakStatementNode",
  ["DoBlock"]                  = "compileDoBlockNode",
  ["FunctionDeclaration"]      = "compileFunctionDeclarationNode",
  ["GenericForLoop"]           = "compileGenericForLoopNode",
  ["IfStatement"]              = "compileIfStatementNode",
  ["LocalDeclaration"]         = "compileLocalDeclarationNode",
  ["LocalFunctionDeclaration"] = "compileLocalFunctionDeclarationNode",
  ["NumericForLoop"]           = "compileNumericForLoopNode",
  ["RepeatLoop"]               = "compileRepeatLoopNode",
  ["ReturnStatement"]          = "compileReturnStatementNode",
  ["VariableAssignment"]       = "compileVariableAssignmentNode",
  ["WhileLoop"]                = "compileWhileLoopNode"
}

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

--// CodeGenerator Constructor //--
function CodeGenerator.new(ast)
  --// Type Checking //--
  assert(type(ast) == "table", "Expected table for 'ast', got " .. type(ast))
  assert(ast.TYPE == "AST", "Expected 'ast' to be an AST (root) node, got " .. tostring(ast.TYPE))

  --// Instance //--
  local CodeGeneratorInstance = setmetatable({}, CodeGenerator)

  --// Initialization //--
  CodeGeneratorInstance.ast = ast
  CodeGeneratorInstance.scopes = {}
  CodeGeneratorInstance.breakInstructions = {}
  CodeGeneratorInstance.currentProto = nil

  -- Scope-related fields declaration
  CodeGeneratorInstance.locals = nil
  CodeGeneratorInstance.nextFreeRegister = nil

  return CodeGeneratorInstance
end

--// Prototype Management //--
function CodeGenerator:newProto()
  self.currentProto = {
    code           = {},
    constants      = {},
    constantLookup = {},
    upvalues       = {},
    upvalueLookup  = {},
    protos         = {},
    numParams      = 0,
    maxStackSize   = COMPILER_MIN_STACK_SIZE,
    isVarArg       = false,
    functionName   = "@tlc",
  }
  return self.currentProto
end

--// Register Management //--
function CodeGenerator:allocateRegister()
  local oldRegister = self.nextFreeRegister
  local newRegister = oldRegister + 1
  self.nextFreeRegister = newRegister

  -- Grow the stack size if necessary
  if newRegister > self.currentProto.maxStackSize then
    if newRegister > COMPILER_MAX_REGISTERS then
      error("Exceeded maximum register limit of " .. COMPILER_MAX_REGISTERS .. " registers")
    end

    -- Lua registers are 0-indexed, so we add 1
    -- to compensate for the extra register (0)
    self.currentProto.maxStackSize = newRegister + 1
  end

  return oldRegister
end

function CodeGenerator:deallocateRegister(register)
  local expectedRegister = self.nextFreeRegister - 1
  if register ~= expectedRegister then
    error(
      string.format(
        "Attempt to deallocate register out of order. Expected: %d, got: %d",
        expectedRegister,
        register
      )
    )
  end

  self.nextFreeRegister = expectedRegister
end

function CodeGenerator:deallocateRegisters(registers)
  local registerLookup = createLookupTable(registers)
  local amountOfRegistersToDeallocate = #registers
  for i = 1, amountOfRegistersToDeallocate do
    local register = self.nextFreeRegister - i
    if not registerLookup[register] then
      print("Invalid register: " .. register)
      print("Expected one of:")
      for _, reg in ipairs(registers) do
        print(reg)
      end
      error("Attempt to deallocate register out of order")
    end
  end

  self.nextFreeRegister = self.nextFreeRegister - amountOfRegistersToDeallocate
end

function CodeGenerator:deallocateIfRegister(registerOrConstant)
  if registerOrConstant >= 0 then
    -- It's a register, not a constant index, deallocate it.
    return self:deallocateRegister(registerOrConstant)
  end
end

--// Variable Management //--
function CodeGenerator:getVariableType(variableName)
  local currentScope = self.currentScope
  local isUpvalue = false
  while currentScope do
    if currentScope.locals[variableName] then
      return (isUpvalue and "Upvalue") or "Local"
    elseif currentScope.isFunctionScope then
      isUpvalue = true
    end
    currentScope = currentScope.previousScope
  end
  return "Global"
end

function CodeGenerator:findVariableRegister(localName)
  local currentScope = self.currentScope
  while currentScope do
    local variableRegister = currentScope.locals[localName]
    if variableRegister then
      return variableRegister
    elseif currentScope.isFunctionScope then
      break
    end
    currentScope = currentScope.previousScope
  end
  error("Could not find variable: " .. localName)
end

function CodeGenerator:registerVariable(localName, register)
  self.locals[localName] = register
end

-- Used only when we register a variable with a placeholder register
-- and we need to change it to the correct register
function CodeGenerator:changeVariableRegister(localName, register)
  local variable = self.locals[localName]
  if not variable then
    error("Attempt to change register of undeclared variable: " .. localName)
  end

  self.locals[localName] = register
end

function CodeGenerator:unregisterVariable(variableName)
  local variableRegister = self.locals[variableName]
  if not variableRegister then
    error("Attempt to unregister undeclared variable: " .. variableName)
  end

  self:deallocateRegister(variableRegister)
  self.locals[variableName] = nil
end

function CodeGenerator:unregisterVariables(variables)
  -- Note: Unregister in reverse order as the variables' registers
  --       are allocated in the order they are declared
  for index = #variables, 1, -1 do
    local variableName = variables[index]
    self:unregisterVariable(variableName)
  end
end

--// Scope Management //--
function CodeGenerator:enterScope(isFunctionScope)
  local currentScope = self.currentScope
  if currentScope then
    -- Save any changes to the current scope
    currentScope.nextFreeRegister = self.nextFreeRegister
  end
  local nextFreeRegister = self.nextFreeRegister or 0
  if isFunctionScope then
    -- Functions always initialize a new stack frame
    nextFreeRegister = 0
  end

  local newScope = {
    locals = {},
    isFunctionScope  = isFunctionScope,
    previousScope    = self.scopes[#self.scopes],
    nextFreeRegister = nextFreeRegister
  }
  self.locals           = newScope.locals
  self.nextFreeRegister = newScope.nextFreeRegister
  self.currentScope     = newScope

  table.insert(self.scopes, newScope)
  return newScope
end

function CodeGenerator:exitScope()
  local scopes = self.scopes
  table.remove(scopes) -- Remove the last scope

  if #scopes > 0 then
    local currentScope = scopes[#scopes]

    self.currentScope = currentScope
    self.locals = currentScope.locals
    self.nextFreeRegister = currentScope.nextFreeRegister
    return
  end

  -- Just deinitialize the variables
  self.locals = nil
  self.nextFreeRegister = nil
  self.currentScope = nil
end

--// Utility Functions //--
function CodeGenerator:isMultiretNode(node)
  if not node then return false end
  return PARSER_MULTIRET_NODE_TYPES[node.TYPE]
end

function CodeGenerator:updateJumpInstruction(instructionIndex)
  local currentInstructionIndex = #self.currentProto.code
  local jumpDistance = currentInstructionIndex - instructionIndex
  local instruction = self.currentProto.code[instructionIndex]
  instruction[3] = jumpDistance
end

function CodeGenerator:updateJumpInstructions(list)
  for _, instructionIndex in ipairs(list) do
    self:updateJumpInstruction(instructionIndex)
  end
end

function CodeGenerator:findOrCreateConstant(value)
  local constantLookup = self.currentProto.constantLookup
  local constants      = self.currentProto.constants
  local constantIndex  = constantLookup[value]
  if constantIndex then
    return constantIndex
  end

  table.insert(constants, value)
  constantIndex = -#constants
  constantLookup[value] = constantIndex
  return constantIndex
end

function CodeGenerator:findOrCreateUpvalue(value)
  local upvalueLookup = self.currentProto.upvalueLookup
  local upvalues      = self.currentProto.upvalues
  local upvalueIndex  = upvalueLookup[value]
  if upvalueIndex then
    return upvalueIndex
  end

  table.insert(upvalues, value)
  upvalueIndex = #upvalues - 1
  upvalueLookup[value] = upvalueIndex
  return upvalueIndex
end

function CodeGenerator:emitInstruction(opname, a, b, c)
  local instruction = { opname, a, b, c or 0 }
  table.insert(self.currentProto.code, instruction)
  return #self.currentProto.code
end

--// Expression Compilation //--
function CodeGenerator:compileNumberNode(node, expressionRegister)
  local constantIndex = self:findOrCreateConstant(node.Value)
  -- OP_LOADK [A, Bx]    R(A) := Kst(Bx)
  self:emitInstruction("LOADK", expressionRegister, constantIndex)
  return expressionRegister
end

function CodeGenerator:compileStringNode(node, expressionRegister)
  local constantIndex = self:findOrCreateConstant(node.Value)
  -- OP_LOADK [A, Bx]    R(A) := Kst(Bx)
  self:emitInstruction("LOADK", expressionRegister, constantIndex)
  return expressionRegister
end

function CodeGenerator:compileFunctionNode(node, expressionRegister)
  self:processFunction(node, expressionRegister)
  return expressionRegister
end

function CodeGenerator:compileFunctionCallNode(node, expressionRegister)
  -- Special register allocation case for function calls as they can be used both
  -- as expressions and statements
  expressionRegister = expressionRegister or self:allocateRegister()

  local selfArgumentRegister
  if not node.IsMethodCall then
    self:processExpressionNode(node.Expression, expressionRegister)
  else
    -- Prepare a register for the self argument to be used
    -- later in the method call instruction
    local nodeExpressionIndex      = node.Expression.Index
    local nodeExpressionExpression = node.Expression.Expression
    self:processExpressionNode(nodeExpressionExpression, expressionRegister)
    selfArgumentRegister = self:allocateRegister()
    local nodeIndexRegister = self:processConstantOrExpression(nodeExpressionIndex)
    -- OP_SELF [A, B, C]    R(A+1) := R(B) R(A) := R(B)[RK(C)]
    self:emitInstruction("SELF", expressionRegister, expressionRegister, nodeIndexRegister)
    self:deallocateIfRegister(nodeIndexRegister)
  end
  local argumentRegisters = self:processExpressionList(node.Arguments)
  if selfArgumentRegister then
    -- Add an extra register for the self argument
    table.insert(argumentRegisters, 1, selfArgumentRegister)
  end
  local returnAmount   = math.max(0, node.ReturnValueAmount + 1)
  local argumentAmount = #argumentRegisters + 1
  local lastArgumentNode = node.Arguments[#node.Arguments]
  if self:isMultiretNode(lastArgumentNode) then
    argumentAmount = 0 -- Use MULTIRET
  end
  -- OP_TAILCALL [A, B, C]    return R(A)(R(A+1), ... ,R(A+B-1))
  -- OP_CALL [A, B, C]    R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
  local opcode = (node.IsTailcall and "TAILCALL") or "CALL"
  self:emitInstruction(opcode, expressionRegister, argumentAmount, returnAmount)

  self:deallocateRegisters(argumentRegisters)
  local returnRegisters = { expressionRegister }
  for _ = expressionRegister + 1, expressionRegister + node.ReturnValueAmount - 1 do
    table.insert(returnRegisters, self:allocateRegister())
  end

  return unpack(returnRegisters)
end

function CodeGenerator:compileConstantNode(node, expressionRegister)
  local nodeValue = node.Value

  -- Is it a boolean?
  if nodeValue ~= "nil" then
    local secondValue = (nodeValue == "true" and 1) or 0
    -- OP_LOADBOOL [A, B, C]    R(A) := (Bool)B if (C) pc++
    self:emitInstruction("LOADBOOL", expressionRegister, secondValue, 0)

  -- It's nil
  else
    -- OP_LOADNIL [A, B]    R(A) := ... := R(B) := nil
    self:emitInstruction("LOADNIL", expressionRegister, expressionRegister)
  end

  return expressionRegister
end

function CodeGenerator:compileVarArgNode(node, expressionRegister)
  local returnAmount = math.max(0, node.ReturnValueAmount + 1)
  -- OP_VARARG [A, B]    R(A), R(A+1), ..., R(A+B-1) = vararg
  self:emitInstruction("VARARG", expressionRegister, returnAmount)
  local returnRegisters = { expressionRegister }
  for _ = expressionRegister + 1, expressionRegister + node.ReturnValueAmount - 1 do
    table.insert(returnRegisters, self:allocateRegister())
  end
  return unpack(returnRegisters)
end

function CodeGenerator:compileTableIndexNode(node, expressionRegister)
  self:processExpressionNode(node.Expression, expressionRegister)
  local indexRegister = self:processConstantOrExpression(node.Index)
  -- OP_GETTABLE [A, B, C]    R(A) := R(B)[RK(C)]
  self:emitInstruction("GETTABLE", expressionRegister, expressionRegister, indexRegister)
  self:deallocateIfRegister(indexRegister)
  return expressionRegister
end

function CodeGenerator:compileTableNode(node, expressionRegister)
  local implicitElements = node.ImplicitElements
  local explicitElements = node.ExplicitElements
  local sizeB = math.min(#implicitElements, 255)
  local sizeC = math.min(#explicitElements, 255)
  -- OP_NEWTABLE [A, B, C]    R(A) := {} (size = B,C)
  self:emitInstruction("NEWTABLE", expressionRegister, sizeB, sizeC)
  for _, element in ipairs(explicitElements) do
    local keyRegister   = self:processConstantOrExpression(element.Key)
    local valueRegister = self:processConstantOrExpression(element.Value)

    -- OP_SETTABLE [A, B, C]    R(A)[RK(B)] := RK(C)
    self:emitInstruction("SETTABLE", expressionRegister, keyRegister, valueRegister)

    self:deallocateIfRegister(valueRegister)
    self:deallocateIfRegister(keyRegister)
  end

  local pageAmount = math.ceil(#implicitElements / COMPILER_SETLIST_MAX)
  for page = 1, pageAmount do
    local startIndex = (page - 1) * COMPILER_SETLIST_MAX + 1
    local endIndex   = math.min(page * COMPILER_SETLIST_MAX, #implicitElements)
    local currentPageRegisters = {}
    for elementIndex = startIndex, endIndex do
      local element       = implicitElements[elementIndex]
      local elementValue  = element.Value
      local valueRegister = self:processExpressionNode(elementValue)

      table.insert(currentPageRegisters, valueRegister)
    end
    local lastElement               = implicitElements[endIndex]
    local lastElementValue          = lastElement.Value
    local currentPageRegisterAmount = #currentPageRegisters
    if page == pageAmount and self:isMultiretNode(lastElementValue) then
      -- B = 0: Doesn't have a fixed amount of keys (multiret)
      currentPageRegisterAmount = 0
    end
    -- OP_SETLIST [A, B, C]    R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B
    self:emitInstruction("SETLIST", expressionRegister, currentPageRegisterAmount, page)
    self:deallocateRegisters(currentPageRegisters)
  end

  return expressionRegister
end

function CodeGenerator:compileVariableNode(node, expressionRegister)
  local variableType = node.VariableType
  if variableType == "Global" then
    -- OP_GETGLOBAL [A, Bx]    R(A) := Gbl[Kst(Bx)]
    self:emitInstruction("GETGLOBAL", expressionRegister, self:findOrCreateConstant(node.Name))
  elseif variableType == "Local" then
    local variableRegister = self:findVariableRegister(node.Name)
    -- OP_MOVE [A, B]    R(A) := R(B)
    self:emitInstruction("MOVE", expressionRegister, variableRegister)
  elseif variableType == "Upvalue" then
    -- OP_GETUPVAL [A, B]    R(A) := UpValue[B]
    self:emitInstruction("GETUPVAL", expressionRegister, self:findOrCreateUpvalue(node.Name))
  end

  return expressionRegister
end

function CodeGenerator:compileBinaryOperatorNode(node, expressionRegister)
  local nodeOperator = node.Operator

  -- Simple arithmetic operators (+, -, /, *, %, ^)
  if COMPILER_ARITHMETIC_OPERATOR_LOOKUP[nodeOperator] then
    local opcode = COMPILER_ARITHMETIC_OPERATOR_LOOKUP[nodeOperator]
    local leftExpressionRegister  = self:processConstantOrExpression(node.Left, expressionRegister)
    local rightExpressionRegister = self:processConstantOrExpression(node.Right)

    -- [A, B, C]    R(A) := RK(B) <operator> RK(C)
    self:emitInstruction(opcode, expressionRegister, leftExpressionRegister, rightExpressionRegister)
    self:deallocateIfRegister(rightExpressionRegister)

  -- Control flow operators (and, or)
  elseif COMPILER_CONTROL_FLOW_OPERATOR_LOOKUP[nodeOperator] then
    local leftExpressionRegister = self:processExpressionNode(node.Left, expressionRegister)
    local isConditionTrue = (nodeOperator == "and" and 0) or 1
    -- OP_TEST [A, C]    if not (R(A) <=> C) then pc++
    self:emitInstruction("TEST", leftExpressionRegister, 0, isConditionTrue)
    -- OP_JMP [A, sBx]    pc+=sBx
    local jumpInstructionIndex = self:emitInstruction("JMP", 0, 0)
    self:processExpressionNode(node.Right, expressionRegister)
    self:updateJumpInstruction(jumpInstructionIndex)

  -- Comparison operators (~=, <=, >=)
  elseif COMPILER_COMPARISON_OPERATOR_LOOKUP[nodeOperator] then
    local leftExpressionRegister  = self:processConstantOrExpression(node.Left)
    local rightExpressionRegister = self:processConstantOrExpression(node.Right)
    local nodeOperatorTable = COMPILER_COMPARISON_INSTRUCTION_LOOKUP[nodeOperator]
    local instruction, flag = nodeOperatorTable[1], nodeOperatorTable[2]
    local flipOperands = (nodeOperator == ">" or nodeOperator == ">=")
    local b, c = leftExpressionRegister, rightExpressionRegister
    if flipOperands then
      b, c = rightExpressionRegister, leftExpressionRegister
    end

    -- [A, B, C]    if ((RK(B) <operator> RK(C)) ~= A) then pc++
    self:emitInstruction(instruction, flag, b, c)
    -- OP_JMP [A, sBx]    pc+=sBx
    self:emitInstruction("JMP", 0, 1)
    -- OP_LOADBOOL [A, B, C]    R(A) := (Bool)B if (C) pc++
    self:emitInstruction("LOADBOOL", expressionRegister, 0, 1)
    self:emitInstruction("LOADBOOL", expressionRegister, 1, 0)
    self:deallocateIfRegister(rightExpressionRegister)
    self:deallocateIfRegister(leftExpressionRegister)

  -- Concatenation operator
  elseif nodeOperator == ".." then
    local leftExpressionRegister  = self:processExpressionNode(node.Left)
    local rightExpressionRegister = self:processExpressionNode(node.Right)
    if (rightExpressionRegister - leftExpressionRegister) ~= 1 then
      error("Concatenation requires consecutive registers")
    end
    -- OP_CONCAT [A, B, C]    R(A) := R(B).. ... ..R(C)
    self:emitInstruction("CONCAT", expressionRegister, leftExpressionRegister, rightExpressionRegister)
    self:deallocateRegisters({ leftExpressionRegister, rightExpressionRegister })
  else
    error("Invalid operator: " .. tostring(nodeOperator))
  end

  return expressionRegister
end

function CodeGenerator:compileUnaryOperatorNode(node, expressionRegister)
  local nodeOperator      = node.Operator
  local operatorOpcode    = COMPILER_UNARY_OPERATOR_LOOKUP[nodeOperator]
  local operandExpression = self:processExpressionNode(node.Operand)
  self:emitInstruction(operatorOpcode, expressionRegister, operandExpression)
  self:deallocateRegister(operandExpression)
  return expressionRegister
end

--// Statement Compilation //--
function CodeGenerator:compileBreakStatementNode()
  -- OP_JMP [A, sBx]    pc+=sBx
  local jumpInstructionIndex = self:emitInstruction("JMP", 0, 0)
  table.insert(self.breakInstructions, jumpInstructionIndex)
end

function CodeGenerator:compileLocalFunctionDeclarationNode(node)
  local name = node.Name
  local localRegister = self:allocateRegister()
  self:registerVariable(name, localRegister)
  self:processFunction(node, localRegister, name)
end

function CodeGenerator:compileFunctionDeclarationNode(node)
  local expression = node.Expression
  local fields     = node.Fields

  -- `function <expression>.<field>[.<field>][:<field>](...)` style declaration
  if #fields > 0 then
    local closureRegister = self:allocateRegister()
    local lastField = fields[#fields]
    self:processFunction(node, closureRegister, lastField)
    local expressionRegister = self:processExpressionNode(expression)
    for index, field in ipairs(fields) do
      -- TODO: Do this without creating a fake AST node
      -- (can't use self:findOrCreateConstant() here due to potential overflows)
      local fieldConstantIndex = self:processConstantOrExpression({ TYPE = "String", Value = field })

      -- Is the field the last one?
      if index == #fields then
        -- OP_SETTABLE [A, B, C]    R(A)[RK(B)] := RK(C)
        self:emitInstruction("SETTABLE", expressionRegister, fieldConstantIndex, closureRegister)
      else
        -- OP_GETTABLE [A, B, C]    R(A) := R(B)[RK(C)]
        self:emitInstruction("GETTABLE", expressionRegister, expressionRegister, fieldConstantIndex)
      end

      self:deallocateIfRegister(fieldConstantIndex)
    end
    self:deallocateRegisters({ closureRegister, expressionRegister })
    return
  end
  -- `function variable(...)` style declaration

  local variableName = expression.Name
  if expression.VariableType == "Local" then
    local localRegister = self:findVariableRegister(variableName)
    self:processFunction(node, localRegister, variableName)
  elseif expression.VariableType == "Upvalue" then
    local closureRegister = self:allocateRegister()
    self:processFunction(node, closureRegister, variableName)
    -- OP_SETUPVAL [A, B]    UpValue[B] := R(A)
    self:emitInstruction("SETUPVAL", closureRegister, self:findOrCreateUpvalue(variableName))
    self:deallocateRegister(closureRegister)
  elseif expression.VariableType == "Global" then
    local globalRegister = self:allocateRegister()
    self:processFunction(node, globalRegister, variableName)
    -- OP_SETGLOBAL [A, Bx]    Gbl[Kst(Bx)] := R(A)
    self:emitInstruction("SETGLOBAL", globalRegister, self:findOrCreateConstant(variableName))
    self:deallocateRegister(globalRegister)
  end
end

function CodeGenerator:compileLocalDeclarationNode(node)
  local variableExpressionRegisters = {}
  for index, expression in ipairs(node.Expressions) do
    local expressionRegisters = { self:processExpressionNode(expression) }
    for index2, expressionRegister in ipairs(expressionRegisters) do
      table.insert(variableExpressionRegisters, expressionRegister)
      if not node.Variables[index + index2 - 1] then
        -- If this expression doesn't have a corresponding variable, deallocate it
        self:deallocateRegister(expressionRegister)
      end
    end
  end

  for index, localName in ipairs(node.Variables) do
    local expressionRegister = variableExpressionRegisters[index]
    if not expressionRegister then
      expressionRegister = self:allocateRegister()
      -- Load nil into the register
      -- OP_LOADNIL [A, B]    R(A) := ... := R(B) := nil
      self:emitInstruction("LOADNIL", expressionRegister, expressionRegister)
    end
    self:registerVariable(localName, expressionRegister)
  end
end

function CodeGenerator:compileNumericForLoopNode(node)
  local variableName  = node.VariableName
  local expressions   = node.Expressions
  local codeblock     = node.CodeBlock
  local startRegister = self:processExpressionNode(expressions[1])
  local endRegister   = self:processExpressionNode(expressions[2])
  local stepRegister  = self:allocateRegister()
  if expressions[3] then
    stepRegister = self:processExpressionNode(expressions[3], stepRegister)
  else
    -- OP_LOADK [A, Bx]    R(A) := Kst(Bx)
    self:emitInstruction("LOADK", stepRegister, self:findOrCreateConstant(1))
  end
  -- OP_FORPREP [A, sBx]    R(A)-=R(A+2) pc+=sBx
  local forprepInstructionIndex = self:emitInstruction("FORPREP", startRegister, 0)
  local loopStart = #self.currentProto.code
  self:registerVariable(variableName, startRegister)
  local oldBreakInstructions = self.breakInstructions
  self.breakInstructions = {}
  self:processCodeBlock(codeblock)
  local loopEnd = #self.currentProto.code
  self:updateJumpInstruction(forprepInstructionIndex)
  -- OP_FORLOOP [A, sBx]   R(A)+=R(A+2)
  --                       if R(A) <?= R(A+1) then { pc+=sBx R(A+3)=R(A) }
  self:emitInstruction("FORLOOP", startRegister, loopStart - loopEnd - 1)
  self:updateJumpInstructions(self.breakInstructions)
  self.breakInstructions = oldBreakInstructions
  self:deallocateRegisters({ endRegister, stepRegister }) -- (start register is already deallocated)
  self:unregisterVariable(variableName)
end

function CodeGenerator:compileGenericForLoopNode(node)
  local iteratorVariables   = node.IteratorVariables
  local expressions         = node.Expressions
  local codeblock           = node.CodeBlock
  local expressionRegisters = self:processExpressionList(expressions)
  -- OP_JMP [A, sBx]    pc+=sBx
  local startJmpInstructionIndex = self:emitInstruction("JMP", 0, 0)
  local forGeneratorRegister = expressionRegisters[1]
  local forStateRegister     = expressionRegisters[2]
  local forControlRegister   = expressionRegisters[3]
  if not (forGeneratorRegister and forStateRegister and forControlRegister) then
    error("Expected non-nil registers for the generator, state, and control values")
  end
  local loopStart = #self.currentProto.code
  local iteratorVariableRegisters = {}
  for _, iteratorVariable in ipairs(iteratorVariables) do
    local iteratorRegister = self:allocateRegister()
    self:registerVariable(iteratorVariable, iteratorRegister)
    table.insert(iteratorVariableRegisters, iteratorRegister)
  end
  local oldBreakInstructions = self.breakInstructions
  self.breakInstructions = {}
  self:processCodeBlock(codeblock)
  self:updateJumpInstruction(startJmpInstructionIndex)
  -- OP_TFORLOOP [A, C]    R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2))
  --                       if R(A+3) ~= nil then R(A+2)=R(A+3) else pc++
  self:emitInstruction("TFORLOOP", forGeneratorRegister, 0, #iteratorVariables)
  -- OP_JMP [A, sBx]    pc+=sBx
  self:emitInstruction("JMP", 0, loopStart - #self.currentProto.code - 1)
  self:updateJumpInstructions(self.breakInstructions)
  self.breakInstructions = oldBreakInstructions
  self:unregisterVariables(iteratorVariables)
  self:deallocateRegisters(expressionRegisters)
end

function CodeGenerator:compileReturnStatementNode(node)
  local expressionRegisters = self:processExpressionList(node.Expressions)
  local startRegister       = expressionRegisters[1] or 0
  local returnAmount        = #node.Expressions + 1
  local lastExpression      = node.Expressions[#node.Expressions]
  if self:isMultiretNode(lastExpression) then
    returnAmount = 0
  end

  -- OP_RETURN [A, B]    return R(A), ... ,R(A+B-2)
  self:emitInstruction("RETURN", startRegister, returnAmount, 0)
  self:deallocateRegisters(expressionRegisters) -- Deallocate return expression registers
end

function CodeGenerator:compileWhileLoopNode(node)
  local loopStart         = #self.currentProto.code
  local conditionRegister = self:processExpressionNode(node.Condition)
  -- OP_TEST [A, C]    if not (R(A) <=> C) then pc++
  self:emitInstruction("TEST", conditionRegister, 0, 0)
  -- OP_JMP [A, sBx]    pc+=sBx
  local jumpInstructionIndex = self:emitInstruction("JMP", 0, 0)
  self:deallocateRegister(conditionRegister)
  local oldBreakInstructions = self.breakInstructions
  self.breakInstructions = {}
  self:processCodeBlock(node.CodeBlock)
  -- OP_JMP [A, sBx]    pc+=sBx
  self:emitInstruction("JMP", 0, loopStart - #self.currentProto.code - 1)
  self:updateJumpInstruction(jumpInstructionIndex)
  self:updateJumpInstructions(self.breakInstructions)
  self.breakInstructions = oldBreakInstructions
end

function CodeGenerator:compileRepeatLoopNode(node)
  local loopStart = #self.currentProto.code
  self.breakInstructions = {}
  self:enterScope()
  self:processCodeBlockInCurrentScope(node.CodeBlock)
  local conditionRegister = self:processExpressionNode(node.Condition)
  -- OP_TEST [A, C]    if not (R(A) <=> C) then pc++
  self:emitInstruction("TEST", conditionRegister, 0, 0)
  -- OP_JMP [A, sBx]    pc+=sBx
  self:emitInstruction("JMP", 0, loopStart - #self.currentProto.code - 1)
  self:updateJumpInstructions(self.breakInstructions)
  self:deallocateRegister(conditionRegister)
  self:exitScope()
end

function CodeGenerator:compileDoBlockNode(node)
  self:processCodeBlock(node.CodeBlock)
end

function CodeGenerator:compileIfStatementNode(node)
  local branches      = node.Branches
  local elseCodeBlock = node.ElseCodeBlock
  local jumpToEndInstructions = {}
  for index, branch in ipairs(branches) do
    local condition = branch.Condition
    local codeBlock = branch.CodeBlock
    local conditionRegister = self:processExpressionNode(condition)
    -- OP_TEST [A, C]    if not (R(A) <=> C) then pc++
    self:emitInstruction("TEST", conditionRegister, 0, 0)
    -- OP_JMP [A, sBx]    pc+=sBx
    local conditionJumpInstructionIndex = self:emitInstruction("JMP", 0, 0)
    self:deallocateRegister(conditionRegister)
    self:processCodeBlock(codeBlock)
    if index < #branches or elseCodeBlock then
      -- OP_JMP [A, sBx]    pc+=sBx
      local jumpInstructionIndex = self:emitInstruction("JMP", 0, 0)
      table.insert(jumpToEndInstructions, jumpInstructionIndex)
    end
    self:updateJumpInstruction(conditionJumpInstructionIndex)
  end
  if elseCodeBlock then
    self:processCodeBlock(elseCodeBlock)
  end

  self:updateJumpInstructions(jumpToEndInstructions)
end

function CodeGenerator:compileVariableAssignmentNode(node)
  local expressionRegisters = self:processExpressionList(node.Expressions)

  for index, lvalue in ipairs(node.LValues) do
    local lvalueType = lvalue.TYPE
    if lvalueType == "Variable" then
      local variableType = lvalue.VariableType
      local variableName = lvalue.Name
      local expressionRegister = expressionRegisters[index]
      if not expressionRegister then error("Expected an expression for assignment") end

      if variableType == "Local" then
        local variableRegister = self:findVariableRegister(variableName)
        -- OP_MOVE [A, B]    R(A) := R(B)
        self:emitInstruction("MOVE", variableRegister, expressionRegister)
      elseif variableType == "Upvalue" then
        -- OP_SETUPVAL [A, B]    UpValue[B] := R(A)
        self:emitInstruction("SETUPVAL", expressionRegister, self:findOrCreateUpvalue(variableName))
      elseif variableType == "Global" then
        -- OP_SETGLOBAL [A, Bx]    Gbl[Kst(Bx)] := R(A)
        self:emitInstruction("SETGLOBAL", expressionRegister, self:findOrCreateConstant(variableName))
      end
    elseif lvalueType == "TableIndex" then
      local indexRegister           = self:processConstantOrExpression(lvalue.Index)
      local tableExpressionRegister = self:processConstantOrExpression(lvalue.Expression)
      local expressionRegister = expressionRegisters[index]
      if not expressionRegister then error("Expected an expression for assignment") end

      -- OP_SETTABLE [A, B, C]    R(A)[RK(B)] := RK(C)
      self:emitInstruction("SETTABLE", tableExpressionRegister, indexRegister, expressionRegister)
      self:deallocateIfRegister(tableExpressionRegister)
      self:deallocateIfRegister(indexRegister)
    else
      error("Unsupported lvalue type: " .. lvalueType)
    end
  end

  self:deallocateRegisters(expressionRegisters)
end

--// Code Generation //--

-- Returns either a positive integer, indicating a register index, or
-- a negative integer, indicating a constant index. Used for RK() operands.
function CodeGenerator:processConstantOrExpression(node, expressionRegister)
  local nodeType = node.TYPE

  if nodeType == "String" or nodeType == "Number" then
    local constantIndex = self:findOrCreateConstant(node.Value)

    -- Check if it can fit in 9-bit signed operand.
    if -constantIndex < 255 then
      -- Return a negative integer, indicating an index in constant table.
      return constantIndex
    end
  end

  return self:processExpressionNode(node, expressionRegister)
end

function CodeGenerator:processExpressionNode(node, expressionRegister)
  expressionRegister = expressionRegister or self:allocateRegister()

  local nodeType        = node.TYPE
  local handlerName     = COMPILER_EXPRESSION_HANDLERS[nodeType]
  local handlerFunction = self[handlerName]

  -- Hot path: The node is unlikely to be "ParenthesizedExpr".
  if handlerFunction then
    return handlerFunction(self, node, expressionRegister)

  -- Cold path: The node is either invalid, which will throw an error and
  --            stop the entire compiling process, or it's "ParenthesizedExpr".
  elseif nodeType == "ParenthesizedExpr" then
    -- Must compile the first part of the expression only on parenthesis
    return self:processExpressionNode(node.Expression, expressionRegister)
  end

  error("Unsupported expression node type: " .. tostring(nodeType))
end

function CodeGenerator:processStatementNode(node)
  local nodeType        = node.TYPE
  local handlerName     = COMPILER_STATEMENT_HANDLERS[nodeType]
  local handlerFunction = self[handlerName]

  -- Hot path: The node is unlikely to be "FunctionCall".
  if handlerFunction then
    -- The statement handler functions don't return anything,
    -- we use "return" just to exit out of this function.
    return handlerFunction(self, node)

  -- Cold path: The node is either invalid, which will throw an error and
  --            stop the entire compiling process, or it's "FunctionCall".
  elseif nodeType == "FunctionCall" then
    -- Instantly deallocate the register
    self:deallocateRegister(self:compileFunctionCallNode(node))
    return
  end

  error("Unsupported statement node type: " .. tostring(nodeType))
end

function CodeGenerator:processExpressionList(expressionList)
  local registers = {}
  for _, node in ipairs(expressionList) do
    local expressionRegisters = { self:processExpressionNode(node) }
    for _, register in ipairs(expressionRegisters) do
      table.insert(registers, register)
    end
  end

  return registers
end

function CodeGenerator:processCodeBlockInCurrentScope(list)
  for _, node in ipairs(list) do
    self:processStatementNode(node)
  end
end

function CodeGenerator:processCodeBlock(list)
  self:enterScope()
  for _, node in ipairs(list) do
    self:processStatementNode(node)
  end
  self:exitScope()
end

function CodeGenerator:processFunctionCodeBlock(list, parameters)
  self:enterScope(true) -- Enter with function scope
  for _, parameter in ipairs(parameters) do
    local parameterRegister = self:allocateRegister()
    self:registerVariable(parameter, parameterRegister)
  end

  for _, node in ipairs(list) do
    self:processStatementNode(node)
  end

  self:exitScope()
end

function CodeGenerator:processFunction(node, expressionRegister, name)
  local codeBlock    = node.CodeBlock
  local parameters   = node.Parameters
  local isVarArg     = node.IsVarArg
  local oldProto     = self.currentProto
  local proto        = self:newProto()
  proto.numParams    = #parameters
  proto.isVarArg     = isVarArg
  proto.functionName = (name and "@" .. name) or "@anonymous"

  self:processFunctionCodeBlock(codeBlock, parameters)

  -- OP_RETURN [A, B]    return R(A), ... ,R(A+B-2)
  self:emitInstruction("RETURN", 0, 1) -- Default return statement
  self.currentProto = oldProto
  table.insert(self.currentProto.protos, proto)
  -- R(A) := closure(KPROTO[Bx], R(A), ... ,R(A+n))
  self:emitInstruction("CLOSURE", expressionRegister, #self.currentProto.protos - 1)

  for _, upvalueName in ipairs(proto.upvalues) do
    local upvalueType = self:getVariableType(upvalueName)
    if upvalueType == "Local" then
      -- OP_MOVE [A, B]    R(A) := R(B)
      self:emitInstruction("MOVE", 0, self:findVariableRegister(upvalueName))
    elseif upvalueType == "Upvalue" then
      -- OP_GETUPVAL [A, B]    R(A) := UpValue[B]
      self:emitInstruction("GETUPVAL", 0, self:findOrCreateUpvalue(upvalueName))
    else
      error("Unsupported upvalue type: " .. upvalueType)
    end
  end

  -- Remove internal temporary fields
  proto.constantLookup = nil
  proto.upvalueLookup  = nil

  return proto
end

--// Main Code Generation //--
function CodeGenerator:generate()
  local proto = self:newProto()
  proto.isVarArg = true
  self:processCodeBlock(self.ast)
  -- OP_RETURN [A, B]    return R(A), ... ,R(A+B-2)
  self:emitInstruction("RETURN", 0, 1) -- Default return statement

  -- Remove internal temporary fields
  proto.constantLookup = nil
  proto.upvalueLookup  = nil

  return proto
end

--[[
  ============================================================================
                                    (•_•)?
                              COMPILER CONSTANTS
  ============================================================================
--]]

-- Lua 5.1 Format Specification Constants
local LUA_SIGNATURE        = "\27Lua"          -- Standard magic number identifying Lua bytecode
local LUA_VERSION_BYTE     = string.char(0x51) -- 0x51 signifies Lua version 5.1, 0x52 for 5.2, etc.
local LUA_FORMAT_VERSION   = string.char(0)    -- 0 = official format version
local LUA_ENDIANNESS       = string.char(1)    -- 1 = little-endian, 0 = big-endian
local LUA_SIZE_INT         = string.char(4)    -- sizeof(int) = 4 bytes
local LUA_SIZE_SIZE_T      = string.char(8)    -- sizeof(size_t) = 8 bytes (for string lengths)
local LUA_SIZE_INSTRUCTION = string.char(4)    -- sizeof(Instruction) = 4 bytes
local LUA_SIZE_LUA_NUMBER  = string.char(8)    -- sizeof(lua_Number) = 8 bytes (double)
local LUA_INTEGRAL_FLAG    = string.char(0)    -- 0 = lua_Number is floating-point
local LUA_COMMON_HEADER    = LUA_SIGNATURE .. LUA_VERSION_BYTE .. LUA_FORMAT_VERSION .. LUA_ENDIANNESS ..
                              LUA_SIZE_INT .. LUA_SIZE_SIZE_T .. LUA_SIZE_INSTRUCTION ..
                              LUA_SIZE_LUA_NUMBER .. LUA_INTEGRAL_FLAG

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
--           'sBx' is like Bx but represents a *signed* offset, primarily used
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
local COMPILER_OPCODE_LOOKUP = {
  ["MOVE"]     = {0, MODE_iABC, OpArgR, OpArgN},  ["LOADK"]     = {1, MODE_iABx, OpArgK, OpArgN},  ["LOADBOOL"] = {2, MODE_iABC, OpArgU, OpArgU},  ["LOADNIL"]   = {3, MODE_iABC, OpArgR, OpArgN},
  ["GETUPVAL"] = {4, MODE_iABC, OpArgU, OpArgN},  ["GETGLOBAL"] = {5, MODE_iABx, OpArgK, OpArgN},  ["GETTABLE"] = {6, MODE_iABC, OpArgR, OpArgK},  ["SETGLOBAL"] = {7, MODE_iABx, OpArgK, OpArgN},
  ["SETUPVAL"] = {8, MODE_iABC, OpArgU, OpArgN},  ["SETTABLE"]  = {9, MODE_iABC, OpArgK, OpArgK},  ["NEWTABLE"] = {10, MODE_iABC, OpArgU, OpArgU}, ["SELF"]      = {11, MODE_iABC, OpArgR, OpArgK},
  ["ADD"]      = {12, MODE_iABC, OpArgK, OpArgK}, ["SUB"]       = {13, MODE_iABC, OpArgK, OpArgK}, ["MUL"]      = {14, MODE_iABC, OpArgK, OpArgK}, ["DIV"]       = {15, MODE_iABC, OpArgK, OpArgK},
  ["MOD"]      = {16, MODE_iABC, OpArgK, OpArgK}, ["POW"]       = {17, MODE_iABC, OpArgK, OpArgK}, ["UNM"]      = {18, MODE_iABC, OpArgR, OpArgN}, ["NOT"]       = {19, MODE_iABC, OpArgR, OpArgN},
  ["LEN"]      = {20, MODE_iABC, OpArgR, OpArgN}, ["CONCAT"]    = {21, MODE_iABC, OpArgR, OpArgR}, ["JMP"]      = {22, MODE_iAsBx, OpArgR, OpArgN},["EQ"]        = {23, MODE_iABC, OpArgK, OpArgK},
  ["LT"]       = {24, MODE_iABC, OpArgK, OpArgK}, ["LE"]        = {25, MODE_iABC, OpArgK, OpArgK}, ["TEST"]     = {26, MODE_iABC, OpArgR, OpArgU}, ["TESTSET"]   = {27, MODE_iABC, OpArgR, OpArgU},
  ["CALL"]     = {28, MODE_iABC, OpArgU, OpArgU}, ["TAILCALL"]  = {29, MODE_iABC, OpArgU, OpArgU}, ["RETURN"]   = {30, MODE_iABC, OpArgU, OpArgN}, ["FORLOOP"]   = {31, MODE_iAsBx, OpArgR, OpArgN},
  ["FORPREP"]  = {32, MODE_iAsBx, OpArgR, OpArgN},["TFORLOOP"]  = {33, MODE_iABC, OpArgN, OpArgU}, ["SETLIST"]  = {34, MODE_iABC, OpArgU, OpArgU}, ["CLOSE"]     = {35, MODE_iABC, OpArgN, OpArgN},
  ["CLOSURE"]  = {36, MODE_iABx, OpArgU, OpArgN}, ["VARARG"]    = {37, MODE_iABC, OpArgU, OpArgN}
}

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

--* Compiler *--
local Compiler = {}
Compiler.__index = Compiler

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

-- TODO: Change names/purpose.
function Compiler:toUnsigned(value)
  value = value or 0
  return math.max(value, -value - 1)
end

function Compiler:toUnsigned2(value)
  value = value or 0
  if value < 0 then
    return 255 + -value
  end
  return value
end

function Compiler:makeOneByte(value)
  return string.char(value % 256)
end

function Compiler:makeFourBytes(value)
  local b1 = value % 256; value = math.floor(value / 256)
  local b2 = value % 256; value = math.floor(value / 256)
  local b3 = value % 256; value = math.floor(value / 256)
  local b4 = value % 256
  return string.char(b1, b2, b3, b4)
end

function Compiler:makeEightBytes(value)
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
  local opcodeTable     = COMPILER_OPCODE_LOOKUP[instruction[1]]
  if not opcodeTable or not instructionName then
    error("Compiler: Unsupported instruction '" .. tostring(instructionName) .. "'")
  end

  local opcode, opmode, arg1, arg2 = opcodeTable[1], opcodeTable[2], opcodeTable[3], opcodeTable[4]
  local a = instruction[2]

  -- Check for overflow first
  if a < 0 or a > 255 then
    error("Compiler: Operand A overflow in instruction '" .. tostring(instructionName) .. "'")
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

  a = self:lshift(self:toUnsigned(a), 6)
  if opmode == MODE_iABC then
    local b, c = instruction[3], instruction[4]

    -- Check for overflows first
    if b < -256 or b > 255 then
      error("Compiler: Operand B overflow in instruction '" .. tostring(instructionName) .. "'")
    end
    if c < -256 or c > 255 then
      error("Compiler: Operand C overflow in instruction '" .. tostring(instructionName) .. "'")
    end

    -- Handle RK() operands (register or constant)
    if arg1 == OpArgK then b = self:lshift(self:toUnsigned2(b), 23)
    else                   b = self:lshift(b, 23) end
    if arg2 == OpArgK then c = self:lshift(self:toUnsigned2(c), 14)
    else                   c = self:lshift(c, 14) end

    -- [ Op(6) | A(8) | C(9) | B(9) ]
    return self:makeFourBytes(opcode + a + b + c)
  elseif opmode == MODE_iABx then
    local b = self:toUnsigned(instruction[3])
    local bx = self:lshift(b, 14)

    -- [ Op(6) | A(8) |    Bx(18)    ]
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

    -- [ Op(6) | A(8) |    sBx(18)    ]
    return self:makeFourBytes(opcode + a + sbx)
  end

  error("Compiler: Unsupported instruction format for '" .. tostring(instructionName) .. "'")
end

--[[
  Serializes the constant section of a function prototype.
  This includes basic constants (nil, bool, num, str) and nested prototypes.

  Format:
    Constant Count (4 bytes)
      Constant Data ((sizeof(Constant) * Number of Constants) bytes)
    Prototype Count (4 bytes)
      Proto Data ((sizeof(Proto) * Number of Protos) bytes)
--]]
function Compiler:makeConstantSection(proto)
  local constantSection = { self:makeFourBytes(#proto.constants) } -- Number of constants
  for _, constant in ipairs(proto.constants) do
    constantSection[#constantSection + 1] = self:makeConstant(constant)
  end
  constantSection[#constantSection + 1] = self:makeFourBytes(#proto.protos) -- Number of protos
  for _, childProto in ipairs(proto.protos) do
    constantSection[#constantSection + 1] = self:makeFunction(childProto)
  end
  return table.concat(constantSection)
end

--[[
  Serializes the code section of a function prototype.

  Format:
  | Instruction Count (4 bytes) | Instruction 1 (4 bytes) | Instruction 2 (4 bytes) | ... |
--]]
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
  if not proto.code or not proto.constants then
    error("Compiler: Invalid function prototype")
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
    self:makeOneByte((proto.isVarArg and VARARG_ISVARARG) or VARARG_NONE),
    self:makeOneByte(proto.maxStackSize), -- Max stack size
    self:makeCodeSection(proto),          -- Code section
    self:makeConstantSection(proto),      -- Constant section

    -- Debug-only info (not implemented)
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
  return LUA_COMMON_HEADER .. self:makeFunction(self.mainProto)
end

-- Now I'm just exporting everything...
return {
  Tokenizer     = Tokenizer,
  Parser        = Parser,
  CodeGenerator = CodeGenerator,
  Compiler      = Compiler,

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