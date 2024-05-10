import Lexico #Esta linea importa el archivo lexico, donde está toda la logica.

while True: #Esto es un metodo while que se mantiene corriendo perpetuamente para capturar los inputs del teclado.
    Lexemas = input('Ingrese Lexemas: ') #El metodo input de python se usa para capturar el teclado y almacena lo que pongas en la variable Lexemas. Como python es un lenguaje no tipado, la variable puede almacenar todo tipo de dato pues se autodefinen.
    resultado, error = Lexico.Testeo('<stdin>', Lexemas) #Esta linea llama al metodo de testeo del archivo lexico para que todo funcione.

    if error: print(error.as_string())
    else: print(resultado) #Metodo para imprimir en consola. (Debe cambiarse luego).