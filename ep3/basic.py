#######################################
# IMPORTS
#######################################

from string_with_arrows import *

#######################################
# CONSTANTS
#######################################

DIGITS = '0123456789'

#######################################
# POSITION
#######################################

class Position:
    def __init__(self, idx=0, ln=0, col=0):
        self.idx = idx
        self.ln = ln
        self.col = col

    def advance(self, current_char=None):
        self.idx += 1
        self.col += 1
        if current_char == '\n':
            self.col = 0
            self.ln += 1

    def copy(self):
        return Position(self.idx, self.ln, self.col)

#######################################
# TOKENS
#######################################

TT_INT      = 'INT'
TT_FLOAT    = 'FLOAT'
TT_PLUS     = 'PLUS'
TT_MINUS    = 'MINUS'
TT_MUL      = 'MUL'
TT_DIV      = 'DIV'
TT_LPAREN   = 'LPAREN'
TT_RPAREN   = 'RPAREN'
TT_EOF      = 'EOF'

class Token:
    def __init__(self, type_, value=None, pos_start=None, pos_end=None):
        self.type = type_
        self.value = value
        
        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()
        
        if pos_end:
            self.pos_end = pos_end.copy()

    def __repr__(self):
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'

#######################################
# LEXER
#######################################

class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = Position(-1, 0, -1)
        self.current_char = None
        self.advance()

    def advance(self):
        self.pos.advance(self.current_char)
        self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None

    def make_tokens(self):
        tokens = []

        while self.current_char != None:
            if self.current_char in ' \t':
                self.advance()
            elif self.current_char in DIGITS:
                tokens.append(self.make_number())
            elif self.current_char == '+':
                tokens.append(Token(TT_PLUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(Token(TT_MINUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '*':
                tokens.append(Token(TT_MUL, pos_start=self.pos))
                self.advance()
            elif self.current_char == '/':
                tokens.append(Token(TT_DIV, pos_start=self.pos))
                self.advance()
            elif self.current_char == '(':
                tokens.append(Token(TT_LPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Token(TT_RPAREN, pos_start=self.pos))
                self.advance()
            else:
                print(f'Illegal character: {self.current_char}')
                print(f'Ln {self.pos.ln + 1}, Col {self.pos.col + 1}')
                return []

        tokens.append(Token(TT_EOF, pos_start=self.pos))
        return tokens

    def make_number(self):
        num_str = ''
        dot_count = 0
        pos_start = self.pos.copy()

        while self.current_char != None and self.current_char in DIGITS + '.':
            if self.current_char == '.':
                if dot_count == 1: break
                dot_count += 1
                num_str += '.'
            else:
                num_str += self.current_char
            self.advance()

        if dot_count == 0:
            return Token(TT_INT, int(num_str), pos_start, self.pos)
        else:
            return Token(TT_FLOAT, float(num_str), pos_start, self.pos)

#######################################
# NODES
#######################################

class NumberNode:
    def __init__(self, tok):
        self.tok = tok

        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end
    
    def __repr__(self):
        return f'{self.tok}'

class BinOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node

        self.pos_start = self.left_node.pos_start
        self.pos_end = self.right_node.pos_end

    def __repr__(self):
        return f'({self.left_node}, {self.op_tok}, {self.right_node})'

class UnaryOpNode:
    def __init__(self, op_tok, node):
        self.op_tok = op_tok
        self.node = node

        self.pos_start = self.op_tok.pos_start
        self.pos_end = self.node.pos_end

    def __repr__(self):
        return f'({self.op_tok}, {self.node})'

#######################################
# PARSE RESULT
#######################################

class ParseResult:
    def __init__(self):
        self.to_reverse = 0
        self.error = None

    def register(self, res=None):
        self.to_reverse += res.to_reverse if res else 1
        if res and res.error: self.error = res.error
        return res

    def success(self, res):
        self.parse_error = None
        self.is_success = True
        self.node = res.node if isinstance(res, ParseResult) else res
        return self

    def failure(self, error):
        self.is_success = False
        self.node = None
        self.error = error
        return self

#######################################
# PARSE ERROR
#######################################

class ParseError:
    def __init__(self, pos_start, pos_end, details=''):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.details = details

    def string(self, text):
        err  = f'Invalid Syntax: {self.details}\n'
        err += f'Ln {self.pos_start.ln + 1}, Col {self.pos_start.col + 1}'
        err += f' -> Ln {self.pos_end.ln + 1}, Col {self.pos_end.col + 1}\n'
        err += '\n' + string_with_arrows(text, self.pos_start, self.pos_end)

        return err

#######################################
# PARSER
#######################################

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.tok_idx = -1
        self.advance()

    def advance(self):
        self.tok_idx += 1
        if self.tok_idx < len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]
        
    def reverse(self, count):
        self.tok_index -= count

    def parse(self):
        res = self.expr()
        if res.is_success and self.current_tok.type != TT_EOF:
            return res.failure(ParseError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '+', '-', '*' or '/'"
            ))
        return res

    ###################################

    def factor(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in (TT_PLUS, TT_MINUS):
            res.register(self.advance())
            factor = self.factor()
            if not factor.is_success: return res.failure(factor.error)
            return res.success(UnaryOpNode(tok, factor.node))
        
        elif tok.type in (TT_INT, TT_FLOAT):
            res.register(self.advance())
            return res.success(NumberNode(tok))
        
        elif tok.type == TT_LPAREN:
            res.register(self.advance())
            expr = res.register(self.expr())
            if not expr.is_success: return res.failure(factor.error)
            if self.current_tok.type == TT_RPAREN:
                res.register(self.advance())
                return res.success(expr.node)
        
        return res.failure(ParseError(
            tok.pos_start, tok.pos_end,
            "Expected int, float, '+', '-' or '('"
        ))

    def term(self):
        return self.bin_op(self.factor, (TT_MUL, TT_DIV))

    def expr(self):
        return self.bin_op(self.term, (TT_PLUS, TT_MINUS))

    def bin_op(self, func, ops):
        res = ParseResult()
        left = func()
        if not left.is_success: return res.failure(left.error)
        node = left.node

        while self.current_tok.type in ops:
            op_tok = self.current_tok
            res.register(self.advance())
            right = func()
            if not right.is_success: return res.failure(right.error)
            node = BinOpNode(node, op_tok, right.node)

        return res.success(node)

#######################################
# RUNTIME RESULT
#######################################

class RTResult:
    def __init__(self, value=None, error=None):
        self.value = value
        self.error = error

#######################################
# RUNTIME ERROR
#######################################

class RTError:
    def __init__(self, pos_start, pos_end, details=''):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.details = details

    def string(self, text):
        err  = f'Runtime Error: {self.details}\n'
        err += f'Ln {self.pos_start.ln + 1}, Col {self.pos_start.col + 1}'
        err += f' -> Ln {self.pos_end.ln + 1}, Col {self.pos_end.col + 1}\n'
        err += '\n' + string_with_arrows(text, self.pos_start, self.pos_end)

        return err

#######################################
# VALUES
#######################################

class Number:
    def __init__(self, value, pos_start, pos_end):
        self.value = value

        self.pos_start = pos_start
        self.pos_end = pos_end

    def added_to(self, other):
        if isinstance(other, Number):
            return RTResult(Number(self.value + other.value,
                self.pos_start, other.pos_end))

    def subbed_by(self, other):
        if isinstance(other, Number):
            return RTResult(Number(self.value - other.value,
                self.pos_start, other.pos_end))

    def multed_by(self, other):
        if isinstance(other, Number):
            return RTResult(Number(self.value * other.value,
                self.pos_start, other.pos_end))

    def dived_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return RTResult(error=RTError(
                    other.pos_start, other.pos_end,
                    'Division by zero'
                ))
            return RTResult(Number(self.value / other.value,
                self.pos_start, other.pos_end))

    def negated(self):
        return RTResult(Number(-self.value, self.pos_start, self.pos_end))
        
#######################################
# INTERPRETER
#######################################

class Interpreter:
    def visit(self, node):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit_method)
        return method(node)

    def no_visit_method(self, node):
        raise Exception(f'No visit_{type(node).__name__} method')

    ###################################

    def visit_NumberNode(self, node):
        return RTResult(Number(node.tok.value, node.pos_start, node.pos_end))

    def visit_BinOpNode(self, node):
        left = self.visit(node.left_node)
        if left.error: return left
        right = self.visit(node.right_node)
        if right.error: return right
        
        if node.op_tok.type == TT_PLUS:
            return left.value.added_to(right.value)
        if node.op_tok.type == TT_MINUS:
            return left.value.subbed_by(right.value)
        if node.op_tok.type == TT_MUL:
            return left.value.multed_by(right.value)
        if node.op_tok.type == TT_DIV:
            return left.value.dived_by(right.value)

    def visit_UnaryOpNode(self, node):
        number = self.visit(node.node)
        if number.error: return number

        if node.op_tok.type == TT_MINUS:
            result = number.value.negated()
            result.value.pos_start = node.op_tok.pos_start
            return result
        
        return number

#######################################
# MAIN
#######################################

def main():
    while True:
        # Get input
        text = input('basic > ')

        # Generate tokens
        lexer = Lexer(text)
        tokens = lexer.make_tokens()
        if len(tokens) == 0: continue

        # Generate AST
        parser = Parser(tokens)
        ast = parser.parse()
        if not ast.is_success:
            print(ast.error.string(text))
            continue

        # Run code
        interpreter = Interpreter()
        result = interpreter.visit(ast.node)
        if result.error:
            print(result.error.string(text))
            continue
        
        # Output result
        if isinstance(result.value, Number):
            print(result.value.value)

if __name__ == '__main__':
    main()
