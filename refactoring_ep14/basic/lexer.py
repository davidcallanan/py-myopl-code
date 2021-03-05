from basic.token import (
    Token, TokenType, KEYWORDS, DIGITS, LETTERS, SKIP_LETTERS, NEW_LINES,
    COMMENT_SYMBOL, SYMBOL_TO_TOKENS
)
from basic.position import Position
from basic.errors import IllegalCharError, ExpectedCharError


LETTERS_DIGITS = LETTERS + DIGITS

class Lexer:
  def __init__(self, fn, text):
    self.fn = fn
    self.text = text
    self.pos = Position(-1, 0, -1, fn, text)
    self.current_char = None
    self.advance()
  
  def advance(self):
    self.pos.advance(self.current_char)
    self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None

  def make_tokens(self):
    tokens = []
    

    while self.current_char != None:
        if self.current_char in SKIP_LETTERS:
            self.advance()
        elif self.current_char == COMMENT_SYMBOL:
            self.skip_comment()
        elif self.current_char in DIGITS:
            tokens.append(self.make_number())
        elif self.current_char in LETTERS:
            tokens.append(self.make_identifier())
        elif self.current_char in SYMBOL_TO_TOKENS:
            tokens.append(Token(SYMBOL_TO_TOKENS[self.current_char], pos_start=self.pos))
            self.advance()
        elif self.current_char == '"':
            tokens.append(self.make_string())
        elif self.current_char == '-':
            tokens.append(self.make_minus_or_arrow())
        elif self.current_char == '=':
            tokens.append(self.make_equals())
        elif self.current_char == '<':
            tokens.append(self.make_less_than())
        elif self.current_char == '>':
            tokens.append(self.make_greater_than())
        elif self.current_char == '!':
            token, error = self.make_not_equals()
            if error: return [], error
            tokens.append(token)
        else:
            pos_start = self.pos.copy()
            char = self.current_char
            self.advance()
            return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")

    tokens.append(Token(TokenType.TT_EOF, pos_start=self.pos))
    return tokens, None

  def make_number(self):
    num_str = ''
    dot_count = 0
    pos_start = self.pos.copy()

    while self.current_char != None and self.current_char in DIGITS + '.':
      if self.current_char == '.':
        if dot_count == 1: break
        dot_count += 1
      num_str += self.current_char
      self.advance()

    if dot_count == 0:
      return Token(TokenType.TT_INT, int(num_str), pos_start, self.pos)
    else:
      return Token(TokenType.TT_FLOAT, float(num_str), pos_start, self.pos)

  def make_string(self):
    string = ''
    pos_start = self.pos.copy()
    escape_character = False
    self.advance()

    escape_characters = {
      'n': '\n',
      't': '\t'
    }

    while self.current_char != None and (self.current_char != '"' or escape_character):
      if escape_character:
        string += escape_characters.get(self.current_char, self.current_char)
      else:
        if self.current_char == '\\':
          escape_character = True
        else:
          string += self.current_char
      self.advance()
      escape_character = False
    
    self.advance()
    return Token(TokenType.TT_STRING, string, pos_start, self.pos)

  def make_identifier(self):
    keyword = ''
    pos_start = self.pos.copy()

    while self.current_char != None and self.current_char in LETTERS_DIGITS + '_':
      keyword += self.current_char
      self.advance()

    tok_type = TokenType.TT_KEYWORD if keyword in KEYWORDS else TokenType.TT_IDENTIFIER
    if tok_type == TokenType.TT_KEYWORD:
        keyword = KEYWORDS[keyword]
    return Token(tok_type, keyword, pos_start, self.pos)

  def make_minus_or_arrow(self):
    tok_type = TokenType.TT_MINUS
    pos_start = self.pos.copy()
    self.advance()

    if self.current_char == '>':
      self.advance()
      tok_type = TokenType.TT_ARROW

    return Token(tok_type, pos_start=pos_start, pos_end=self.pos)

  def make_not_equals(self):
    pos_start = self.pos.copy()
    self.advance()

    if self.current_char == '=':
      self.advance()
      return Token(TokenType.TT_NE, pos_start=pos_start, pos_end=self.pos), None

    self.advance()
    return None, ExpectedCharError(pos_start, self.pos, "'=' (after '!')")
  
  def make_equals(self):
    tok_type = TokenType.TT_EQ
    pos_start = self.pos.copy()
    self.advance()

    if self.current_char == '=':
      self.advance()
      tok_type = TokenType.TT_EE

    return Token(tok_type, pos_start=pos_start, pos_end=self.pos)

  def make_less_than(self):
    tok_type = TokenType.TT_LT
    pos_start = self.pos.copy()
    self.advance()

    if self.current_char == '=':
      self.advance()
      tok_type = TokenType.TT_LTE

    return Token(tok_type, pos_start=pos_start, pos_end=self.pos)

  def make_greater_than(self):
    tok_type = TokenType.TT_GT
    pos_start = self.pos.copy()
    self.advance()

    if self.current_char == '=':
      self.advance()
      tok_type = TokenType.TT_GTE

    return Token(tok_type, pos_start=pos_start, pos_end=self.pos)

  def skip_comment(self):
    self.advance()

    while self.current_char != '\n':
      self.advance()

    self.advance()