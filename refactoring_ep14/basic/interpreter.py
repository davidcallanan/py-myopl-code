from basic.runtime import RTResult
from basic.errors import RTError
from basic.token import TokenType, Keywords
from basic.types.number import Number
from basic.types.string import String
from basic.types.list import List

#######################################
# INTERPRETER
#######################################


class Interpreter:
    @classmethod
    def visit(cls, node, context):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(cls, method_name, cls.no_visit_method)
        return method(node, context)

    @classmethod
    def no_visit_method(cls, node, context):
        raise Exception(f'No visit_{type(node).__name__} method defined')

    ###################################

    @classmethod
    def visit_NumberNode(cls, node, context):
        return RTResult().success(
            Number(node.tok.value).set_context(context).set_pos(
                node.pos_start, node.pos_end))

    @classmethod
    def visit_StringNode(cls, node, context):
        return RTResult().success(
            String(node.tok.value).set_context(context).set_pos(
                node.pos_start, node.pos_end))

    @classmethod
    def visit_ListNode(cls, node, context):
        res = RTResult()
        elements = []

        for element_node in node.element_nodes:
            elements.append(res.register(cls.visit(element_node, context)))
            if res.should_return(): return res

        return res.success(
            List(elements).set_context(context).set_pos(
                node.pos_start, node.pos_end))

    @classmethod
    def visit_VarAccessNode(cls, node, context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = context.symbol_table.get(var_name)

        if not value:
            return res.failure(
                RTError(node.pos_start, node.pos_end,
                        f"'{var_name}' is not defined", context))

        value = value.copy().set_pos(node.pos_start,
                                     node.pos_end).set_context(context)
        return res.success(value)

    @classmethod
    def visit_VarAssignNode(cls, node, context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = res.register(cls.visit(node.value_node, context))
        if res.should_return(): return res

        context.symbol_table.set(var_name, value)
        return res.success(value)

    @classmethod
    def visit_BinOpNode(cls, node, context):
        res = RTResult()
        left = res.register(cls.visit(node.left_node, context))
        if res.should_return(): return res
        right = res.register(cls.visit(node.right_node, context))
        if res.should_return(): return res

        if node.op_tok.type == TokenType.TT_PLUS:
            result, error = left.added_to(right)
        elif node.op_tok.type == TokenType.TT_MINUS:
            result, error = left.subbed_by(right)
        elif node.op_tok.type == TokenType.TT_MUL:
            result, error = left.multed_by(right)
        elif node.op_tok.type == TokenType.TT_DIV:
            result, error = left.dived_by(right)
        elif node.op_tok.type == TokenType.TT_POW:
            result, error = left.powed_by(right)
        elif node.op_tok.type == TokenType.TT_EE:
            result, error = left.get_comparison_eq(right)
        elif node.op_tok.type == TokenType.TT_NE:
            result, error = left.get_comparison_ne(right)
        elif node.op_tok.type == TokenType.TT_LT:
            result, error = left.get_comparison_lt(right)
        elif node.op_tok.type == TokenType.TT_GT:
            result, error = left.get_comparison_gt(right)
        elif node.op_tok.type == TokenType.TT_LTE:
            result, error = left.get_comparison_lte(right)
        elif node.op_tok.type == TokenType.TT_GTE:
            result, error = left.get_comparison_gte(right)
        elif node.op_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_AND):
            result, error = left.anded_by(right)
        elif node.op_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_OR):
            result, error = left.ored_by(right)

        if error:
            return res.failure(error)
        else:
            return res.success(result.set_pos(node.pos_start, node.pos_end))

    @classmethod
    def visit_UnaryOpNode(cls, node, context):
        res = RTResult()
        number = res.register(cls.visit(node.node, context))
        if res.should_return(): return res

        error = None

        if node.op_tok.type == TokenType.TT_MINUS:
            number, error = number.multed_by(Number(-1))
        elif node.op_tok.matches(TokenType.TT_KEYWORD, Keywords.KW_NOT):
            number, error = number.notted()

        if error:
            return res.failure(error)
        else:
            return res.success(number.set_pos(node.pos_start, node.pos_end))

    @classmethod
    def visit_IfNode(cls, node, context):
        res = RTResult()

        for condition, expr, should_return_null in node.cases:
            condition_value = res.register(cls.visit(condition, context))
            if res.should_return(): return res

            if condition_value.is_true():
                expr_value = res.register(cls.visit(expr, context))
                if res.should_return(): return res
                return res.success(
                    Number.null if should_return_null else expr_value)

        if node.else_case:
            expr, should_return_null = node.else_case
            expr_value = res.register(cls.visit(expr, context))
            if res.should_return(): return res
            return res.success(
                Number.null if should_return_null else expr_value)

        return res.success(Number.null)

    @classmethod
    def visit_ForNode(cls, node, context):
        res = RTResult()
        elements = []

        start_value = res.register(cls.visit(node.start_value_node, context))
        if res.should_return(): return res

        end_value = res.register(cls.visit(node.end_value_node, context))
        if res.should_return(): return res

        if node.step_value_node:
            step_value = res.register(cls.visit(node.step_value_node,
                                                 context))
            if res.should_return(): return res
        else:
            step_value = Number(1)

        i = start_value.value

        if step_value.value >= 0:
            condition = lambda: i < end_value.value
        else:
            condition = lambda: i > end_value.value

        while condition():
            context.symbol_table.set(node.var_name_tok.value, Number(i))
            i += step_value.value

            value = res.register(cls.visit(node.body_node, context))
            if res.should_return(
            ) and res.loop_should_continue == False and res.loop_should_break == False:
                return res

            if res.loop_should_continue:
                continue

            if res.loop_should_break:
                break

            elements.append(value)

        return res.success(
            Number.null if node.should_return_null else List(elements).
            set_context(context).set_pos(node.pos_start, node.pos_end))

    @classmethod
    def visit_WhileNode(cls, node, context):
        res = RTResult()
        elements = []

        while True:
            condition = res.register(cls.visit(node.condition_node, context))
            if res.should_return(): return res

            if not condition.is_true():
                break

            value = res.register(cls.visit(node.body_node, context))
            if res.should_return(
            ) and res.loop_should_continue == False and res.loop_should_break == False:
                return res

            if res.loop_should_continue:
                continue

            if res.loop_should_break:
                break

            elements.append(value)

        return res.success(
            Number.null if node.should_return_null else List(elements).
            set_context(context).set_pos(node.pos_start, node.pos_end))

    @classmethod
    def visit_FuncDefNode(cls, node, context):
        from basic.types.function import Function
        res = RTResult()

        func_name = node.var_name_tok.value if node.var_name_tok else None
        body_node = node.body_node
        arg_names = [arg_name.value for arg_name in node.arg_name_toks]
        func_value = Function(
            func_name, body_node, arg_names,
            node.should_auto_return).set_context(context).set_pos(
                node.pos_start, node.pos_end)

        if node.var_name_tok:
            context.symbol_table.set(func_name, func_value)

        return res.success(func_value)

    @classmethod
    def visit_CallNode(cls, node, context):
        res = RTResult()
        args = []

        value_to_call = res.register(cls.visit(node.node_to_call, context))
        if res.should_return(): return res
        value_to_call = value_to_call.copy().set_pos(node.pos_start,
                                                     node.pos_end)

        for arg_node in node.arg_nodes:
            args.append(res.register(cls.visit(arg_node, context)))
            if res.should_return(): return res

        return_value = res.register(value_to_call.execute(args))
        if res.should_return(): return res
        return_value = return_value.copy().set_pos(
            node.pos_start, node.pos_end).set_context(context)
        return res.success(return_value)

    @classmethod
    def visit_ReturnNode(cls, node, context):
        res = RTResult()

        if node.node_to_return:
            value = res.register(cls.visit(node.node_to_return, context))
            if res.should_return(): return res
        else:
            value = Number.null

        return res.success_return(value)

    @classmethod
    def visit_ContinueNode(cls, node, context):
        return RTResult().success_continue()

    @classmethod
    def visit_BreakNode(cls, node, context):
        return RTResult().success_break()
