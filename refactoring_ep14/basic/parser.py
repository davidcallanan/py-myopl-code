from basic.errors import InvalidSyntaxError
from basic.nodes import *
from basic.token import TokenType, Keywords

#######################################
# PARSE RESULT
#######################################


class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
        self.last_registered_advance_count = 0
        self.advance_count = 0
        self.to_reverse_count = 0

    def register_advancement(self):
        self.last_registered_advance_count = 1
        self.advance_count += 1

    def register(self, res):
        self.last_registered_advance_count = res.advance_count
        self.advance_count += res.advance_count
        if res.error: self.error = res.error
        return res.node

    def try_register(self, res):
        if res.error:
            self.to_reverse_count = res.advance_count
            return None
        return self.register(res)

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        if not self.error or self.last_registered_advance_count == 0:
            self.error = error
        return self


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
        self.update_current_tok()
        return self.current_tok

    def reverse(self, amount=1):
        self.tok_idx -= amount
        self.update_current_tok()
        return self.current_tok

    def update_current_tok(self):
        if self.tok_idx >= 0 and self.tok_idx < len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]

    def parse(self):
        res = self.statements()
        if not res.error and self.current_tok.type != TokenType.TT_EOF:
            return res.failure(
                InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Token cannot appear after previous tokens"))
        return res

    ###################################

    def statements(self):
        res = ParseResult()
        statements = []
        pos_start = self.current_tok.pos_start.copy()

        while self.current_tok.type == TokenType.TT_NEWLINE:
            res.register_advancement()
            self.advance()

        statement = res.register(self.statement())
        if res.error: return res
        statements.append(statement)

        more_statements = True

        while True:
            newline_count = 0
            while self.current_tok.type == TokenType.TT_NEWLINE:
                res.register_advancement()
                self.advance()
                newline_count += 1
            if newline_count == 0:
                more_statements = False

            if self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_END):
                break

            if self.current_tok.type == TokenType.TT_EOF:
                break

            if not more_statements: break
            statement = res.try_register(self.statement())
            if not statement:
                self.reverse(res.to_reverse_count)
                more_statements = False
                continue
            statements.append(statement)

        return res.success(
            ListNode(statements, pos_start, self.current_tok.pos_end.copy()))

    def statement(self):
        res = ParseResult()
        pos_start = self.current_tok.pos_start.copy()

        if self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_RETURN):
            res.register_advancement()
            self.advance()

            expr = res.try_register(self.expr())
            if not expr:
                self.reverse(res.to_reverse_count)
            return res.success(
                ReturnNode(expr, pos_start, self.current_tok.pos_start.copy()))

        if self.current_tok.matches(TokenType.TT_KEYWORD,
                                    Keywords.KW_CONTINUE):
            res.register_advancement()
            self.advance()
            return res.success(
                ContinueNode(pos_start, self.current_tok.pos_start.copy()))

        if self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_BREAK):
            res.register_advancement()
            self.advance()
            return res.success(
                BreakNode(pos_start, self.current_tok.pos_start.copy()))

        expr = res.register(self.expr())
        if res.error:
            expected_list = [
                Keywords.KW_RETURN, Keywords.KW_CONTINUE, Keywords.KW_BREAK,
                Keywords.KW_VAR, Keywords.KW_IF, Keywords.KW_FOR,
                Keywords.KW_WHILE, Keywords.KW_FUN, TokenType.TT_INT,
                TokenType.TT_FLOAT, TokenType.TT_IDENTIFIER, '+', '-', '(',
                '[', Keywords.KW_NOT
            ]
            return res.failure(
                InvalidSyntaxError(self.current_tok.pos_start,
                                   self.current_tok.pos_end,
                                   f"Expected {','.join(map(str, expected_list))}"))
        return res.success(expr)

    def expr(self):
        res = ParseResult()

        if self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_VAR):
            res.register_advancement()
            self.advance()

            if self.current_tok.type != TokenType.TT_IDENTIFIER:
                return res.failure(
                    InvalidSyntaxError(self.current_tok.pos_start,
                                       self.current_tok.pos_end,
                                       f"Expected {TokenType.TT_IDENTIFIER}"))

            var_name = self.current_tok
            res.register_advancement()
            self.advance()

            if self.current_tok.type != TokenType.TT_EQ:
                return res.failure(
                    InvalidSyntaxError(self.current_tok.pos_start,
                                       self.current_tok.pos_end,
                                       "Expected '='"))

            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            return res.success(VarAssignNode(var_name, expr))

        node = res.register(
            self.bin_op(self.comp_expr, ((TokenType.TT_KEYWORD, Keywords.KW_AND),
                                         (TokenType.TT_KEYWORD, Keywords.KW_OR))))

        if res.error:
            expected_list = [
                Keywords.KW_VAR, Keywords.KW_IF, Keywords.KW_FOR, Keywords.KW_WHILE,
                Keywords.KW_FUN, TokenType.TT_INT, TokenType.TT_FLOAT, TokenType.TT_IDENTIFIER,
                '+', '-', '(', '[', Keywords.KW_NOT
            ]
            return res.failure(
                InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected {','.join(map(str, expected_list))}"
                ))

        return res.success(node)

    def comp_expr(self):
        res = ParseResult()

        if self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_NOT):
            op_tok = self.current_tok
            res.register_advancement()
            self.advance()

            node = res.register(self.comp_expr())
            if res.error: return res
            return res.success(UnaryOpNode(op_tok, node))

        node = res.register(
            self.bin_op(self.arith_expr,
                        (TokenType.TT_EE, TokenType.TT_NE, TokenType.TT_LT,
                         TokenType.TT_GT, TokenType.TT_LTE, TokenType.TT_GTE)))

        if res.error:
            expected_list = [
                TokenType.TT_INT, TokenType.TT_FLOAT, TokenType.TT_IDENTIFIER,
                '+', '-', '(', '[', Keywords.KW_IF, Keywords.KW_FOR, Keywords.KW_WHILE,
                Keywords.KW_FUN, Keywords.KW_NOT
            ]
            return res.failure(
                InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected {','.join(map(str, expected_list))}"
                ))

        return res.success(node)

    def arith_expr(self):
        return self.bin_op(self.term, (TokenType.TT_PLUS, TokenType.TT_MINUS))

    def term(self):
        return self.bin_op(self.factor, (TokenType.TT_MUL, TokenType.TT_DIV))

    def factor(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in (TokenType.TT_PLUS, TokenType.TT_MINUS):
            res.register_advancement()
            self.advance()
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(tok, factor))

        return self.power()

    def power(self):
        return self.bin_op(self.call, (TokenType.TT_POW, ), self.factor)

    def call(self):
        res = ParseResult()
        atom = res.register(self.atom())
        if res.error: return res

        if self.current_tok.type == TokenType.TT_LPAREN:
            res.register_advancement()
            self.advance()
            arg_nodes = []

            if self.current_tok.type == TokenType.TT_RPAREN:
                res.register_advancement()
                self.advance()
            else:
                arg_nodes.append(res.register(self.expr()))
                if res.error:
                    expected_list = [
                        ')', Keywords.KW_VAR, Keywords.KW_IF, Keywords.KW_FOR,
                        Keywords.KW_WHILE, Keywords.KW_FUN, TokenType.TT_INT,
                        TokenType.TT_FLOAT, TokenType.TT_IDENTIFIER,
                        '+', '-', '(', '[', Keywords.KW_NOT
                    ]
                    return res.failure(
                        InvalidSyntaxError(
                            self.current_tok.pos_start,
                            self.current_tok.pos_end,
                            f"Expected {','.join(map(str, expected_list))}"
                        ))

                while self.current_tok.type == TokenType.TT_COMMA:
                    res.register_advancement()
                    self.advance()

                    arg_nodes.append(res.register(self.expr()))
                    if res.error: return res

                if self.current_tok.type != TokenType.TT_RPAREN:
                    expected_list = [
                        ',', ')'
                    ]
                    return res.failure(
                        InvalidSyntaxError(self.current_tok.pos_start,
                                           self.current_tok.pos_end,
                                           f"Expected {expected_list}"))

                res.register_advancement()
                self.advance()
                
            return res.success(CallNode(atom, arg_nodes))
        return res.success(atom)

    def atom(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in (TokenType.TT_INT, TokenType.TT_FLOAT):
            res.register_advancement()
            self.advance()
            return res.success(NumberNode(tok))

        elif tok.type == TokenType.TT_STRING:
            res.register_advancement()
            self.advance()
            return res.success(StringNode(tok))

        elif tok.type == TokenType.TT_IDENTIFIER:
            res.register_advancement()
            self.advance()
            return res.success(VarAccessNode(tok))

        elif tok.type == TokenType.TT_LPAREN:
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            if self.current_tok.type == TokenType.TT_RPAREN:
                res.register_advancement()
                self.advance()
                return res.success(expr)
            else:
                return res.failure(
                    InvalidSyntaxError(self.current_tok.pos_start,
                                       self.current_tok.pos_end,
                                       "Expected ')'"))

        elif tok.type == TokenType.TT_LSQUARE:
            list_expr = res.register(self.list_expr())
            if res.error: return res
            return res.success(list_expr)

        elif tok.matches(TokenType.TT_KEYWORD, Keywords.KW_IF):
            if_expr = res.register(self.if_expr())
            if res.error: return res
            return res.success(if_expr)

        elif tok.matches(TokenType.TT_KEYWORD, Keywords.KW_FOR):
            for_expr = res.register(self.for_expr())
            if res.error: return res
            return res.success(for_expr)

        elif tok.matches(TokenType.TT_KEYWORD, Keywords.KW_WHILE):
            while_expr = res.register(self.while_expr())
            if res.error: return res
            return res.success(while_expr)

        elif tok.matches(TokenType.TT_KEYWORD, Keywords.KW_FUN):
            func_def = res.register(self.func_def())
            if res.error: return res
            return res.success(func_def)

        expected_list = [
            TokenType.TT_INT, TokenType.TT_FLOAT,
            TokenType.TT_IDENTIFIER, '+', '-', '(', '[',
            Keywords.KW_IF, Keywords.KW_FOR, Keywords.KW_WHILE,
            Keywords.KW_FUN
        ]
        return res.failure(
            InvalidSyntaxError(
                tok.pos_start, tok.pos_end,
                f"Expected {expected_list}"
            ))

    def list_expr(self):
        res = ParseResult()
        element_nodes = []
        pos_start = self.current_tok.pos_start.copy()

        if self.current_tok.type != TokenType.TT_LSQUARE:
            return res.failure(
                InvalidSyntaxError(self.current_tok.pos_start,
                                   self.current_tok.pos_end, f"Expected '['"))

        res.register_advancement()
        self.advance()

        if self.current_tok.type == TokenType.TT_RSQUARE:
            res.register_advancement()
            self.advance()
        else:
            element_nodes.append(res.register(self.expr()))
            if res.error:
                return res.failure(
                    InvalidSyntaxError(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        "Expected ']', 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
                    ))

            while self.current_tok.type == TokenType.TT_COMMA:
                res.register_advancement()
                self.advance()

                element_nodes.append(res.register(self.expr()))
                if res.error: return res

            if self.current_tok.type != TokenType.TT_RSQUARE:
                return res.failure(
                    InvalidSyntaxError(self.current_tok.pos_start,
                                       self.current_tok.pos_end,
                                       f"Expected ',' or ']'"))

            res.register_advancement()
            self.advance()

        return res.success(
            ListNode(element_nodes, pos_start,
                     self.current_tok.pos_end.copy()))

    def if_expr(self):
        res = ParseResult()
        all_cases = res.register(self.if_expr_cases(Keywords.KW_IF))
        if res.error: return res
        cases, else_case = all_cases
        return res.success(IfNode(cases, else_case))

    def if_expr_b(self):
        return self.if_expr_cases(Keywords.KW_ELIF)

    def if_expr_c(self):
        res = ParseResult()
        else_case = None

        if self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_ELSE):
            res.register_advancement()
            self.advance()

            if self.current_tok.type == TokenType.TT_NEWLINE:
                res.register_advancement()
                self.advance()

                statements = res.register(self.statements())
                if res.error: return res
                else_case = (statements, True)

                if self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_END):
                    res.register_advancement()
                    self.advance()
                else:
                    return res.failure(
                        InvalidSyntaxError(self.current_tok.pos_start,
                                           self.current_tok.pos_end,
                                           "Expected 'END'"))
            else:
                expr = res.register(self.statement())
                if res.error: return res
                else_case = (expr, False)

        return res.success(else_case)

    def if_expr_b_or_c(self):
        res = ParseResult()
        cases, else_case = [], None

        if self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_ELIF):
            all_cases = res.register(self.if_expr_b())
            if res.error: return res
            cases, else_case = all_cases
        else:
            else_case = res.register(self.if_expr_c())
            if res.error: return res

        return res.success((cases, else_case))

    def if_expr_cases(self, case_keyword):
        res = ParseResult()
        cases = []
        else_case = None

        if not self.current_tok.matches(TokenType.TT_KEYWORD, case_keyword):
            return res.failure(
                InvalidSyntaxError(self.current_tok.pos_start,
                                   self.current_tok.pos_end,
                                   f"Expected '{case_keyword}'"))

        res.register_advancement()
        self.advance()

        condition = res.register(self.expr())
        if res.error: return res

        if not self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_THEN):
            return res.failure(
                InvalidSyntaxError(self.current_tok.pos_start,
                                   self.current_tok.pos_end,
                                   f"Expected 'THEN'"))

        res.register_advancement()
        self.advance()

        if self.current_tok.type == TokenType.TT_NEWLINE:
            res.register_advancement()
            self.advance()

            statements = res.register(self.statements())
            if res.error: return res
            cases.append((condition, statements, True))

            if self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_END):
                res.register_advancement()
                self.advance()
            else:
                all_cases = res.register(self.if_expr_b_or_c())
                if res.error: return res
                new_cases, else_case = all_cases
                cases.extend(new_cases)
        else:
            expr = res.register(self.statement())
            if res.error: return res
            cases.append((condition, expr, False))

            all_cases = res.register(self.if_expr_b_or_c())
            if res.error: return res
            new_cases, else_case = all_cases
            cases.extend(new_cases)

        return res.success((cases, else_case))

    def for_expr(self):
        res = ParseResult()

        if not self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_FOR):
            return res.failure(
                InvalidSyntaxError(self.current_tok.pos_start,
                                   self.current_tok.pos_end,
                                   f"Expected 'FOR'"))

        res.register_advancement()
        self.advance()

        if self.current_tok.type != TokenType.TT_IDENTIFIER:
            return res.failure(
                InvalidSyntaxError(self.current_tok.pos_start,
                                   self.current_tok.pos_end,
                                   f"Expected identifier"))

        var_name = self.current_tok
        res.register_advancement()
        self.advance()

        if self.current_tok.type != TokenType.TT_EQ:
            return res.failure(
                InvalidSyntaxError(self.current_tok.pos_start,
                                   self.current_tok.pos_end, f"Expected '='"))

        res.register_advancement()
        self.advance()

        start_value = res.register(self.expr())
        if res.error: return res

        if not self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_TO):
            return res.failure(
                InvalidSyntaxError(self.current_tok.pos_start,
                                   self.current_tok.pos_end, f"Expected 'TO'"))

        res.register_advancement()
        self.advance()

        end_value = res.register(self.expr())
        if res.error: return res

        if self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_STEP):
            res.register_advancement()
            self.advance()

            step_value = res.register(self.expr())
            if res.error: return res
        else:
            step_value = None

        if not self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_THEN):
            return res.failure(
                InvalidSyntaxError(self.current_tok.pos_start,
                                   self.current_tok.pos_end,
                                   f"Expected 'THEN'"))

        res.register_advancement()
        self.advance()

        if self.current_tok.type == TokenType.TT_NEWLINE:
            res.register_advancement()
            self.advance()

            body = res.register(self.statements())
            if res.error: return res

            if not self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_END):
                return res.failure(
                    InvalidSyntaxError(self.current_tok.pos_start,
                                       self.current_tok.pos_end,
                                       f"Expected 'END'"))

            res.register_advancement()
            self.advance()

            return res.success(
                ForNode(var_name, start_value, end_value, step_value, body,
                        True))

        body = res.register(self.statement())
        if res.error: return res

        return res.success(
            ForNode(var_name, start_value, end_value, step_value, body, False))

    def while_expr(self):
        res = ParseResult()

        if not self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_WHILE):
            return res.failure(
                InvalidSyntaxError(self.current_tok.pos_start,
                                   self.current_tok.pos_end,
                                   f"Expected 'WHILE'"))

        res.register_advancement()
        self.advance()

        condition = res.register(self.expr())
        if res.error: return res

        if not self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_THEN):
            return res.failure(
                InvalidSyntaxError(self.current_tok.pos_start,
                                   self.current_tok.pos_end,
                                   f"Expected 'THEN'"))

        res.register_advancement()
        self.advance()

        if self.current_tok.type == TokenType.TT_NEWLINE:
            res.register_advancement()
            self.advance()

            body = res.register(self.statements())
            if res.error: return res

            if not self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_END):
                return res.failure(
                    InvalidSyntaxError(self.current_tok.pos_start,
                                       self.current_tok.pos_end,
                                       f"Expected 'END'"))

            res.register_advancement()
            self.advance()

            return res.success(WhileNode(condition, body, True))

        body = res.register(self.statement())
        if res.error: return res

        return res.success(WhileNode(condition, body, False))

    def func_def(self):
        res = ParseResult()

        if not self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_FUN):
            return res.failure(
                InvalidSyntaxError(self.current_tok.pos_start,
                                   self.current_tok.pos_end,
                                   f"Expected 'FUN'"))

        res.register_advancement()
        self.advance()

        if self.current_tok.type == TokenType.TT_IDENTIFIER:
            var_name_tok = self.current_tok
            res.register_advancement()
            self.advance()
            if self.current_tok.type != TokenType.TT_LPAREN:
                return res.failure(
                    InvalidSyntaxError(self.current_tok.pos_start,
                                       self.current_tok.pos_end,
                                       f"Expected '('"))
        else:
            var_name_tok = None
            if self.current_tok.type != TokenType.TT_LPAREN:
                return res.failure(
                    InvalidSyntaxError(self.current_tok.pos_start,
                                       self.current_tok.pos_end,
                                       f"Expected identifier or '('"))

        res.register_advancement()
        self.advance()
        arg_name_toks = []

        if self.current_tok.type == TokenType.TT_IDENTIFIER:
            arg_name_toks.append(self.current_tok)
            res.register_advancement()
            self.advance()

            while self.current_tok.type == TokenType.TT_COMMA:
                res.register_advancement()
                self.advance()

                if self.current_tok.type != TokenType.TT_IDENTIFIER:
                    return res.failure(
                        InvalidSyntaxError(self.current_tok.pos_start,
                                           self.current_tok.pos_end,
                                           f"Expected identifier"))

                arg_name_toks.append(self.current_tok)
                res.register_advancement()
                self.advance()

            if self.current_tok.type != TokenType.TT_RPAREN:
                return res.failure(
                    InvalidSyntaxError(self.current_tok.pos_start,
                                       self.current_tok.pos_end,
                                       f"Expected ',' or ')'"))
        else:
            if self.current_tok.type != TokenType.TT_RPAREN:
                return res.failure(
                    InvalidSyntaxError(self.current_tok.pos_start,
                                       self.current_tok.pos_end,
                                       f"Expected identifier or ')'"))

        res.register_advancement()
        self.advance()

        if self.current_tok.type == TokenType.TT_ARROW:
            res.register_advancement()
            self.advance()

            body = res.register(self.expr())
            if res.error: return res

            return res.success(
                FuncDefNode(var_name_tok, arg_name_toks, body, True))

        if self.current_tok.type != TokenType.TT_NEWLINE:
            return res.failure(
                InvalidSyntaxError(self.current_tok.pos_start,
                                   self.current_tok.pos_end,
                                   f"Expected '->' or NEWLINE"))

        res.register_advancement()
        self.advance()

        body = res.register(self.statements())
        if res.error: return res

        if not self.current_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_END):
            return res.failure(
                InvalidSyntaxError(self.current_tok.pos_start,
                                   self.current_tok.pos_end,
                                   f"Expected 'END'"))

        res.register_advancement()
        self.advance()

        return res.success(
            FuncDefNode(var_name_tok, arg_name_toks, body, False))

    ###################################

    def bin_op(self, func_a, ops, func_b=None):
        if func_b == None:
            func_b = func_a

        res = ParseResult()
        left = res.register(func_a())
        if res.error: return res

        while self.current_tok.type in ops or (self.current_tok.type,
                                               self.current_tok.value) in ops:
            op_tok = self.current_tok
            res.register_advancement()
            self.advance()
            right = res.register(func_b())
            if res.error: return res
            left = BinOpNode(left, op_tok, right)

        return res.success(left)
