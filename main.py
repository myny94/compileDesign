import ply.lex
from ply import yacc
import sys
from tree_print import treeprint
from semantics_common import visit_tree, SymbolData, SemData

reserved = {
    'define': 'DEFINE',
    'begin': 'BEGIN',
    'end': 'END',
    'each': 'EACH',
    'select': 'SELECT'
 }

tokens = ['WHITESPACE', 'LARROW', 'RARROW', 'LPAREN', 'RPAREN', 'LSQUARE', 'RSQUARE', 'COMMA',
          'DOT', 'PIPE', 'DOUBLEPLUS', 'DOUBLEMULT', 'DOUBLEDOT', 'COLON', 'EQ', 'NOTEQ', 'LT', 'LTEQ', 'GT',
          'GTEQ', 'PLUS', 'MINUS', 'MULT', 'DIV', 'MOD','NUMBER_LITERAL', 'STRING_LITERAL', 'varIDENT',
          'constIDENT', 'tupleIDENT', 'funcIDENT'] + list(reserved.values())

def t_ID(t):
    r'define|begin|end|each|select'
    #r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')    # Check for reserved words
    return t

t_ignore = ' \t'
t_LARROW = r'\<\-'
t_RARROW = r'\-\>'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LSQUARE = r'\['
t_RSQUARE = r'\]'
t_COMMA = r'\,'
t_DOT = r'\.'
t_PIPE = r'\|'
t_DOUBLEPLUS = r'\+{2}'
t_DOUBLEMULT = r'\*{2}'
t_DOUBLEDOT = r'\.{2}'
t_COLON = r'\:'

t_EQ = r'\='
t_NOTEQ = r'\!\='
t_LT = r'\<'
t_LTEQ = r'\<='
t_GT = r'\>'
t_GTEQ = r'\>='
t_PLUS = r'\+'
t_MINUS = r'-'
t_MULT = r'\*'
t_DIV = r'/'
t_MOD = r'%'

def t_NUMBER_LITERAL(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_STRING_LITERAL(t):
    r'"[^"]*"'
    t.value = t.value.strip('"')
    return t

t_varIDENT = r'[a-z][a-zA-Z0-9_]+'
t_constIDENT = r'[A-Z]+'
t_tupleIDENT = r'\<[a-z]+\>'
t_funcIDENT = r'[A-Z][a-z0-9_]+'

def t_error(t):
    raise Exception("Illegal character '{}' at line {}".format(
        t.value, t.lexer.lineno))

def t_newline(t):
    r'\n+'
    #t.type = "newline"
    t.lexer.lineno += len(t.value)
    pass

def t_WHITESPACE(t):
    r'\s+'
    pass

def t_COMMENT(t):
    r'\{.*\}'
    pass

lexer = ply.lex.lex()

class Tree:
    def __init__(self, nodetype):
        self.nodetype = nodetype

def p_program(p):
    '''program : functions_or_variables return_value DOT'''
    p[0] = Tree("program")
    p[0].children_funcs_vars = p[1]
    p[0].child_return_value = p[2]

def p_function_or_variable_definition(p):
    '''function_or_variable_definition : variable_definition
                                       | function_definition'''
    p[0] = p[1]

def p_functions_or_variables1(p):
    '''functions_or_variables : '''
    
def p_functions_or_variables2(p):
    '''functions_or_variables : function_or_variable_definition'''
    p[0] = [p[1]]

def p_functions_or_variables3(p):
    '''functions_or_variables : functions_or_variables function_or_variable_definition'''
    p[0] = p[1]
    p[0].append(p[2])

def p_function_definition(p): # function without parameters
    '''function_definition : DEFINE funcIDENT LSQUARE RSQUARE BEGIN function_body return_value DOT END DOT
                           | DEFINE funcIDENT LSQUARE RSQUARE BEGIN return_value DOT END DOT'''
    p[0] = Tree('function_definition')
    p[0].child_formals = None
    p[0].value = p[2]
    if len(p)>10:
        p[0].children_vars = p[6]
        p[0].child_retval = p[7]
    else:
        p[0].child_retval = p[6]
    

def p_function_definition2(p): # function with parameters
    '''function_definition : DEFINE funcIDENT LSQUARE formals RSQUARE BEGIN return_value DOT END DOT
                           | DEFINE funcIDENT LSQUARE formals RSQUARE BEGIN function_body return_value DOT END DOT'''
    p[0] = Tree('function_definition')
    p[0].value = p[2]
    if len(p) == 11:
        p[0].child_formals = p[4]
        p[0].child_retval = p[7]
    else:
        p[0].child_formals = p[4]
        p[0].children_vars = p[7]
        p[0].child_retval = p[8]
        
def p_function_body1(p):
    '''function_body : variable_definition'''
    p[0] = [p[1]]
    
def p_function_body2(p):
    '''function_body : function_body variable_definition'''
    p[0] = p[1]
    p[0].append(p[2])

def p_formals1(p):
    '''formals : varIDENT'''
    p[0]= Tree('parameter')
    p[0].children_parameter = [Tree('parameter')]
    p[0].children_parameter[0].value = p[1]

def p_formals2(p):
    '''formals : formals COMMA varIDENT'''
    p[0] = p[1]
    p[0].children_parameter.append(Tree('parameter'))
    p[0].children_parameter[-1].value = p[3]

def p_return_value(p):
    '''return_value : EQ simple_expression
                    | NOTEQ pipe_expression'''
    p[0] = Tree('simple_return_value')
    p[0].child_retval = p[2]

def p_variable_definition(p):
    '''variable_definition : var_definition
                           | constant_definition
                           | tuple_definition
                           | tuple_definition2'''
    p[0] = p[1]

def p_var_definition(p):
    '''var_definition : varIDENT LARROW simple_expression DOT'''
    p[0] = Tree('variable_definition')
    p[0].value = p[1]
    p[0].child_expr = p[3]

def p_constant_definition(p):
    '''constant_definition : constIDENT LARROW constant_expression DOT'''  
    p[0] = Tree('constant_definition')
    p[0].value = p[1]
    p[0].child_expr = p[3]

def p_tuple_definition(p):
    '''tuple_definition : tupleIDENT LARROW tuple_expression DOT'''
    p[0] = Tree('tuple_definition')
    p[0].value = p[1]
    p[0].child_expr = p[3]

def p_tuple_definition2(p):
    '''tuple_definition2 : pipe_expression RARROW tupleIDENT DOT'''
    p[0] = Tree('tuple_definition')
    p[0].value = p[3]
    p[0].child_expr  = p[1]

def p_constant_expression1(p):
    '''constant_expression : constIDENT'''
    p[0] = Tree('constIDENT')
    p[0].value = p[1]

def p_constant_expression2(p):
    '''constant_expression : NUMBER_LITERAL'''
    p[0] = Tree('NUMBER_LITERAL')
    p[0].value = p[1]

def p_pipe_expression1(p):
    '''pipe_expression : tuple_expression'''
    p[0] = p[1]

def p_pipe_expression2(p):
    '''pipe_expression : pipe_expression PIPE pipe_operation'''
    p[0] = Tree('pipe_expression')
    p[0].child_expr = p[1]
    p[0].child_op = p[3]

def p_pipe_operation1(p):
    '''pipe_operation : funcIDENT'''
    p[0] = Tree('funcIDENT')
    p[0].value = p[1]

def p_pipe_operation2(p):
    '''pipe_operation : MULT'''
    p[0] = Tree('MULT')
    p[0].value = p[1]

def p_pipe_operation3(p):
    '''pipe_operation : PLUS'''
    p[0] = Tree('PLUS')
    p[0].value = p[1]

def p_pipe_operation4(p):
    '''pipe_operation : each_statement'''
    p[0] = p[1]

def p_each_statement(p):
    '''each_statement : EACH COLON funcIDENT'''
    p[0] = Tree('each_statement')
    p[0].value = p[3]

def p_tuple_expression(p):
    '''tuple_expression : tuple_atom'''
    p[0] = p[1]

def p_tuple_expression2(p):
    '''tuple_expression : tuple_expression tuple_operation tuple_atom'''
    p[0] = Tree('tuple_expression')
    p[0].child_expr1 = p[1]
    p[0].child_op = p[2]
    p[0].child_expr2 = p[3]

def p_tuple_operation(p):
    '''tuple_operation : DOUBLEPLUS'''
    p[0] = Tree('tuple_operation')
    p[0].value = p[1]

def p_tuple_atom1(p):
    '''tuple_atom : LSQUARE constant_expression DOUBLEMULT constant_expression RSQUARE
                  | LSQUARE constant_expression DOUBLEDOT  constant_expression RSQUARE
                  | LSQUARE arguments RSQUARE'''
    p[0] = Tree('tuple_atom')
    if len(p) == 4:
        p[0].child_argument = p[2]
    else:
        p[0].children_expr = [p[2]]
        p[0].children_expr.append(p[4])

def p_tuple_atom2(p):
    '''tuple_atom : tupleIDENT'''
    p[0] = Tree('tupleIDENT')
    p[0].value = p[1]

def p_tuple_atom3(p):
    '''tuple_atom : function_call'''
    p[0] = p[1]

def p_function_call(p):
    '''function_call : funcIDENT LSQUARE RSQUARE
                     | funcIDENT LSQUARE arguments RSQUARE'''
    p[0] = Tree('function_call')
    p[0].value = p[1]
    if len(p) > 4:
        p[0].child_op = p[3]
    else:
        p[0].child_op = None

def p_arguments1(p):
    '''arguments : simple_expression'''
    p[0] = Tree('arguments')
    p[0].children_expr = [p[1]]

def p_arguments2(p):
    '''arguments : arguments COMMA simple_expression'''
    p[0] = p[1]
    p[0].children_expr.append(p[3])

def p_atom(p):
    '''atom : function_call
            | LPAREN simple_expression RPAREN
            | SELECT COLON constant_expression LSQUARE tuple_expression RSQUARE'''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4:
        p[0] = p[2]
    else:
        p[0] = Tree('atom')
        p[0].child_expr1 = p[3]
        p[0].child_expr2 = p[5]

def p_atom2(p):
    '''atom : NUMBER_LITERAL'''
    p[0] = Tree('NUMBER_LITERAL')
    p[0].value = p[1]

def p_atom3(p):
    '''atom : STRING_LITERAL'''
    p[0] = Tree('STRING_LITERAL')
    p[0].value = p[1]

def p_atom4(p):
    '''atom : varIDENT'''
    p[0] = Tree('varIDENT')
    p[0].value = p[1]

def p_atom5(p):
    '''atom : constIDENT'''
    p[0] = Tree('constIDENT')
    p[0].value = p[1]

def p_factor(p):
    '''factor : atom
              | MINUS atom'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[2]

def p_term1(p):
    '''term : factor'''
    p[0] = p[1]

def p_term2(p):
    '''term : term MULT factor
            | term DIV factor'''
    p[0] = Tree('factor')  
    p[0].child_expr1 = p[1]
    p[0].child_expr2 = p[3]
  
def p_simple_expression1(p):
    '''simple_expression : term'''
    p[0] = p[1]

def p_simple_expression2(p):
    '''simple_expression : simple_expression MINUS term
                         | simple_expression PLUS term'''
    p[0] = Tree('simple_expression')
    p[0].child_expr1 = p[1]
    p[0].child_expr2 = p[3]

def p_error(p):
    print( 'syntax error @', p )
    raise SystemExit

parser = yacc.yacc()

def before_fun(node, semdata):
    nodetype = node.nodetype
    if nodetype == "constant_definition":
        if node.value in semdata.variable_def:
            return f'{node.value} is already defined!'
        else:
            semdata.variable_def.append(node.value)
    
    elif nodetype == "tuple_definition":
        if node.value in semdata.variable_def:
            return f'{node.value} is already defined!'
        else:
            semdata.variable_def.append(node.value)

    elif nodetype == "variable_definition":
        if node.value in semdata.variable_def:
            return f'{node.value} is already defined!'
        else:
            semdata.variable_def.append(node.value)
    
    elif nodetype == "function_definition":
        if node.child_formals == None: # no parameters
            semdata.parameters[node.value] = []

        else:
            parameters = [x.value for x in node.child_formals.children_parameter]
            semdata.parameters[node.value] = parameters
            semdata.funcs[node.value] = parameters

        if node.value in semdata.variable_def:
            return f'{node.value} is already defined!'
        else:
            semdata.variable_def.append(node.value)
            
    elif nodetype == "constIDENT":
        if node.value in semdata.variable_def:
            pass
        else:
            return f'{node.value} is not defined!'

    elif nodetype == "tupleIDENT":
        if node.value in semdata.variable_def:
            pass
        else:
            return f'{node.value} is not defined!'

    elif nodetype == "varIDENT":
        if node.value in semdata.variable_def or node.value in sum([semdata.parameters[x] for x in semdata.parameters.keys()],[]):
            pass
        else:
            return f'{node.value} is not defined!'
    
    elif nodetype == "each_statement":
        if node.value in semdata.variable_def:
            pass
        else:
            return f'{node.value} is not defined!'

    elif nodetype == "function_call":
        if node.value not in semdata.funcs:
            return f'Function {node.value} is not defined!'

        if node.child_op == None: # no parameters
            if semdata.funcs[node.value] == 0:
                pass
            else:
                return f'The number of parameters of the function {node.value} should be 0'
        else:
            parameters = [x.value for x in node.child_op.children_expr]
            if len(parameters) == len(semdata.funcs[node.value]):
                pass
            else:
                return f'The number of parameters of the function {node.value} should be {len(semdata.funcs[node.value])}'

def after_fun(node, semdata):
    if node.nodetype == "function_definition":
        semdata.parameters = {}

def semantic_checks(tree):
    semdata = SemData()
    semdata.variable_def = []
    semdata.parameters = {}
    semdata.funcs = {}
    visit_tree(tree, before_fun, after_fun, semdata)
    return semdata

if __name__ == '__main__':
    import argparse, codecs
    arg_parser = argparse.ArgumentParser()
    group = arg_parser.add_mutually_exclusive_group()
    group.add_argument('--who', action='store_true', help='who wrote this' )
    group.add_argument('-f', '--file', help='filename to process')
    ns = arg_parser.parse_args()
    if ns.who == True:
        print('259196 Nayeong Song')
    elif ns.file is None:
        arg_parser.print_help()
    else:
        data = codecs.open( ns.file, encoding='utf-8').read()
        result = parser.parse(data, lexer=lexer, debug=False)
        print('tree')
        treeprint(result)
        semantic_checks(result)
        if result is None:
            print( 'syntax OK' )