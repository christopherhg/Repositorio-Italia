from strings_with_arrows import * #Esta es una librería que sirve para poner una flecha donde está el error en el mensaje.

#######################################
# Posición
#######################################

class Posicion: #Esta clase determina la posición de nuestro lexema ayuda a la clase error a saber donde está el fallo.
    def __init__(self, idx, ln, col, fn, ftxt): #ESta es la lista de atributos que tiene una posición.
        self.idx = idx
        self.ln = ln
        self.col = col
        self.fn = fn
        self.ftxt = ftxt

    def advance(self, current_char=None): #Este metodo permite al analizador moverse por el texto que hemos puesto en el shell.
        self.idx += 1
        self.col += 1

        if current_char == '\n': #Esta condición es para frenarlo.
            self.ln += 1
            self.col = 0

        return self

    def copy(self):
        return Posicion(self.idx, self.ln, self.col, self.fn, self.ftxt)

#######################################
# Tokens
#######################################

#Estos son los tipos de tokens que puede haber.

TT_Int		= 'Int'
TT_Real    = 'Real'
TT_Suma     = 'Suma'
TT_Resta    = 'Resta'
TT_Mult      = 'Mult'
TT_Div      = 'Div'
TT_LParen  = 'LParen'
TT_RParen   = 'RParen'
TT_Fin			= 'Fin' #Toquen reservado para el fin del arhivo

class Token: #En esta clase se segrega el tipo de input que tiene Lexemas
    def __init__(self, type_, value=None, pos_start=None, pos_end=None): #En esta linea se le da un tipo y un valor, igual nos aseguramos de darle una posición de inicio y fin
        self.type = type_
        self.value = value
        
        if pos_start: #Se asegura que haya una posición de arranque 
         self.pos_start = pos_start.copy()
         self.pos_end = pos_start.copy()
         self.pos_end.advance()

        if pos_end: #Se asegura que haya una posición de arranque 
         self.pos_end = pos_end
    
    def __repr__(self): #Este es un metodo de representación, sirve para que cuando se imprima el codigo. 
        if self.value: return f'{self.type}:{self.value}' #Si el token tiene un valor, va a imprimir el tipo y el valor.
        return f'{self.type}' #Sino solo imprime el typo.

#######################################
# Constantes
#######################################

Digitos = '0123456789' #Rango de numeros posibles.


#######################################
# Analizador.
#######################################

class AnalizadorLexico:
    def __init__(self, fn, Lexemas):
        self.fn = fn
        self.Lexemas = Lexemas  #Aquí es donde la variable del shell viene 
        self.pos = Posicion(-1, 0, -1, fn, Lexemas) #Esto y la siguiente le dice al codigo donde iniciar el analisis. 
        self.current_char = None 
        self.advance() # Aqui llamas al la sig función.
    
    def advance(self): #Esta función hace la función de mover de una parte del texto a la siguiente
        self.pos.advance(self.current_char)
        self.current_char = self.Lexemas[self.pos.idx] if self.pos.idx < len(self.Lexemas) else None #En esta linea le dices al codigo que pare si el texto se acaba

    def make_tokens(self):
        tokens = []

        while self.current_char != None: #Este es el while que hace la parte del analisis, los else son las condiciones. Para optimizar se podría intentar remplazar con una función switch
            if self.current_char in ' \t': #Esta linea es para que ignore los espacios
                self.advance() #Esta linea llama a la función de avance.
            elif self.current_char in Digitos: #Esta condicional lleva a la función que separa enteros de reales.
                tokens.append(self.SeparadorNumerico()) #los pasa a la función de aquí
            elif self.current_char == '+': #Esta y las de abajo comparan mediante == si el caracter debe ser del tipo toquen que se ve.
                tokens.append(Token(TT_Suma, pos_start=self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(Token(TT_Resta, pos_start=self.pos))
                self.advance()
            elif self.current_char == '*':
                tokens.append(Token(TT_Mult, pos_start=self.pos))
                self.advance()
            elif self.current_char == '/':
                tokens.append(Token(TT_Div, pos_start=self.pos))
                self.advance()
            elif self.current_char == '(':
                tokens.append(Token(TT_LParen, pos_start=self.pos))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Token(TT_RParen, pos_start=self.pos))
                self.advance()
            else: #Este pedazo se usa si el analizador no encontró nada, para decir que hay un error de caracter.
                pos_start = self.pos.copy()
                char = self.current_char
                self.advance()
                return [], CaracterErroneo(pos_start, self.pos, "'" + char + "'")

        tokens.append(Token(TT_Fin, pos_start=self.pos))
        return tokens, None

    def SeparadorNumerico(self): #Esta funcíon separa los enteros de los reales.
        num_str = '' #Esta es una variable vacia, aquí vamos a poner el numero para volverlo string.
        ContadorPuntos = 0
        pos_start = self.pos.copy()

        while self.current_char != None and self.current_char in Digitos + '.': #"Mientras el caracter actual no sea None y esté en Digitos..."
            if self.current_char == '.':
                if ContadorPuntos == 1: break #Esta linea es para que el codigo compruebe si el numero tiene 2 o más "." porque eso no existe.
                ContadorPuntos += 1 #Aumenta el contador de puntos
                num_str += '.' #Le pone un ". al string"
            else:
                num_str += self.current_char #Mete el caracter actual, nuestro numero, a la variable vacia.
            self.advance()

        if ContadorPuntos == 0: #Si la cantidad de . en el numero es 0 asigna entero, sino es flotante.
            return Token(TT_Int, int(num_str))
        else: 
            return Token(TT_Real, float(num_str))
        
#######################################
# Nodos (Estos son para las ramas del arbol sintactico)
#######################################

class NodoNumerico:
	def __init__(self, tok):
		self.tok = tok

	def __repr__(self):
		return f'{self.tok}' #Devuelve el token en un string para impresión.

class NodoBin: #Estos nodos son para la suma,resta, mult y div.
	def __init__(self, left_node, op_tok, right_node):
		self.left_node = left_node
		self.op_tok = op_tok
		self.right_node = right_node

	def __repr__(self):
		return f'({self.left_node}, {self.op_tok}, {self.right_node})'

class NodoNegativo: #Este nodo trabaja con el numero negativo.
	def __init__(self, op_tok, node):
		self.op_tok = op_tok
		self.node = node

	def __repr__(self):
		return f'({self.op_tok}, {self.node})'


#######################################
# Arbol sintactico
#######################################

class Parser:
	def __init__(self, tokens):
		self.tokens = tokens
		self.tok_idx = -1
		self.advance()

	def advance(self, ):
		self.tok_idx += 1
		if self.tok_idx < len(self.tokens):
			self.current_tok = self.tokens[self.tok_idx]
		return self.current_tok

	def parse(self):
		res = self.expr()
		if not res.error and self.current_tok.type != TT_Fin:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Falta un signo '+', '-', '*' o '/'"
			))
		return res

	#Factores, terminos y expresiones

	def factor(self): #Un factor es el lexema más basico, en este caso un numero, flotante o caracter.
		res = ParseResult()
		tok = self.current_tok

		if tok.type in (TT_Suma, TT_Resta): #Checa si el token es suma o resta.
			res.register(self.advance())
			factor = res.register(self.factor())
			if res.error: return res
			return res.success(NodoNegativo(tok, factor))
		
		elif tok.type in (TT_Int, TT_Real): #Checa si el numero es entero o real
			res.register(self.advance())
			return res.success(NodoNumerico(tok))

		elif tok.type == TT_LParen: #Checa si el token es parentesis.
			res.register(self.advance())
			expr = res.register(self.expr())
			if res.error: return res
			if self.current_tok.type == TT_RParen: #Esta linea se asegura de que el parentesis esté cerrado, sino brinca el error del else.
				res.register(self.advance())
				return res.success(expr)
			else:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Falta una ')'"
				))

		return res.failure(InvalidSyntaxError(
			tok.pos_start, tok.pos_end,
			"Falta un Int o Real"
		))

	def term(self): # Un termino es la unión de 2 factores. 
		return self.bin_op(self.factor, (TT_Mult, TT_Div)) #Esta linea llama a la operación binaria y requiere factores y los simbolos de multiplicar y dividir.

	def expr(self): #Una expresión es la suma de varios terminos.
		return self.bin_op(self.term, (TT_Suma, TT_Resta))

	###################################

	def bin_op(self, func, ops): #Esta es la operación binaria.
		res = ParseResult() #Variable para el resultado
		left = res.register(func()) #Variable para el numero de la izquierda
		if res.error: return res 

		while self.current_tok.type in ops: #Mientras el token actual sea del tipo que aparezca en operadores...
			op_tok = self.current_tok
			res.register(self.advance())
			right = res.register(func())
			if res.error: return res
			left = NodoBin(left, op_tok, right) #Llama al metodo en nodos.

		return res.success(left)


#######################################
# Flechas. Esta clase usa la librería para poner las flechas en el error.
#######################################

class ParseResult:
	def __init__(self):
		self.error = None
		self.node = None

	def register(self, res):
		if isinstance(res, ParseResult):
			if res.error: self.error = res.error #Checa si el nodo tiene algun error.
			return res.node

		return res

	def success(self, node): #Si el resultado es correcto, 
		self.node = node
		return self

	def failure(self, error):
		self.error = error
		return self

        
#######################################
# Errores.
#######################################

class Error: #Esta clase es por si no encuentra nada en el analizador.
    def __init__(self, pos_start, pos_end, error_name, details): 
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details
    
    def as_string(self): #Esta función imprime el error.
        resultado  = f'{self.error_name}: {self.details}\n'
        resultado += f'File {self.pos_start.fn}, line {self.pos_start.ln + 1}'
        resultado += '\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
        return resultado

class CaracterErroneo(Error): #Est clase es para los caracteres que no deben existir como los reales con 2 "."
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Error de carater', details)

class InvalidSyntaxError(Error):
		def __init__(self, pos_start, pos_end, details=''):
				super().__init__(pos_start, pos_end, 'Syntaxis no valida', details)

#######################################
# Testeo.
#######################################

def Testeo(fn, Lexemas):
    lexer = AnalizadorLexico(fn, Lexemas) #Esta parte activa el analizador lexico
    tokens, error = lexer.make_tokens()
    if error: return None, error #Se asegura que el error no llegue al arbol
    
    #Activa el arbol sintactico.
    parser = Parser(tokens)
    ast = parser.parse()

    return ast.node, ast.error