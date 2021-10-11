# intento de generar una lista infinita de x's en un lenguaje
# con evaluacion ansiosa (eager)
def repetidos(x):
    return [x] + repetidos(x)

# importen la funcion en un interprete de Python e intenten hacer
#  x = repetidos(1)
#  y = repetidos(1)[0]

# Que sucedio?
# Que diferencia notan respecto a Haskell?