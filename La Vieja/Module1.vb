'Programa que permite jugar La Vieja a través de la Consola
'Autor: Gianfranco Gasbarri V-26.654.860        Fecha: 05/10/18
Module Module1

    'Subprograma que permite escribir el tablero actual en la Consola para su visualización
    Sub imprimirTablero(ByVal matrizJugadas(,) As Char)

        Console.WriteLine(" {0} | {1} | {2} ", matrizJugadas(0, 0), matrizJugadas(0, 1), matrizJugadas(0, 2))
        Console.WriteLine("___________")
        Console.WriteLine(" {0} | {1} | {2} ", matrizJugadas(1, 0), matrizJugadas(1, 1), matrizJugadas(1, 2))
        Console.WriteLine("___________")
        Console.WriteLine(" {0} | {1} | {2} ", matrizJugadas(2, 0), matrizJugadas(2, 1), matrizJugadas(2, 2))

    End Sub

    'Función que retorna True si dicho símbolo ganó en la fila especificada, sino False
    Function ganoFila(ByVal matrizJugadas(,) As Char, ByVal fila As Integer, ByVal symbol As Char) As Boolean

        'Se inicializa el retorno
        ganoFila = True

        For columna = 0 To matrizJugadas.GetUpperBound(1)

            If matrizJugadas(fila, columna) <> symbol Then

                ganoFila = False

            End If

        Next

    End Function


    'Función que retorna True si dicho símbolo ganó en la columna especificada, sino False
    Function ganoColumna(ByVal matrizJugadas(,) As Char, ByVal columna As Integer, ByVal symbol As Char) As Boolean

        'Se inicializa el retorno
        ganoColumna = True

        For fila = 0 To matrizJugadas.GetUpperBound(0)

            If matrizJugadas(fila, columna) <> symbol Then

                ganoColumna = False

            End If

        Next

    End Function

    'Función que retorna True si dicho símbolo ganó en alguna de las diagonales, sino False
    Function ganoDiagonal(ByVal matrizJugadas(,) As Char, ByVal symbol As Char) As Boolean

        'Declaramos e inicializamos variables bandera para cada diagonal
        Dim ganoPrimeraDiagonal, ganoSegundaDiagonal As Boolean

        ganoPrimeraDiagonal = True
        ganoSegundaDiagonal = True

        For i = 0 To matrizJugadas.GetUpperBound(0)

            If matrizJugadas(i, i) <> symbol Then

                ganoPrimeraDiagonal = False

            End If

            If matrizJugadas(i, matrizJugadas.GetUpperBound(0) - i) <> symbol Then

                ganoSegundaDiagonal = False

            End If

        Next

        ganoDiagonal = ganoPrimeraDiagonal Or ganoSegundaDiagonal

    End Function

    'Funcion que devuelve 0 si nadie ha ganado, 1 si gano el primer jugador o 2 si gano el segundo jugador
    Function calcularGanador(ByVal matrizJugadas(,) As Char, ByVal numeroJugada As Integer) As Integer

        'Declaramos variables
        Dim valorFila, valorColumna As Char

        calcularGanador = 0

        For num = 0 To matrizJugadas.GetUpperBound(0)

            'Verificar fila

            valorFila = matrizJugadas(num, 0)

            If valorFila = "X" Then

                If ganoFila(matrizJugadas, num, valorFila) Then

                    calcularGanador = 1

                End If

            ElseIf valorFila = "O" Then

                If ganoFila(matrizJugadas, num, valorFila) Then

                    calcularGanador = 2

                End If

            End If

            'Verificar columna

            valorColumna = matrizJugadas(0, num)

            If valorColumna = "X" Then

                If ganoColumna(matrizJugadas, num, valorColumna) Then

                    calcularGanador = 1

                End If

            ElseIf valorColumna = "O" Then

                If ganoColumna(matrizJugadas, num, valorColumna) Then

                    calcularGanador = 2

                End If

            End If

        Next

        'Verificar diagonales

        If ganoDiagonal(matrizJugadas, "X") Then

            calcularGanador = 1

        ElseIf ganoDiagonal(matrizJugadas, "X") Then

            calcularGanador = 2

        End If

        If calcularGanador = 0 And numeroJugada = 9 Then

            'Ganó la Vieja
            calcularGanador = 4

        End If

    End Function

    'Función que devuelve True si es posible una jugada en la posición especificada, sino False
    Function puedeInsertar(ByVal matrizJugadas(,) As Char, ByVal linearPos As Integer) As Boolean

        'Declaración de variables
        Dim posicionValida As Boolean
        Dim fila, columna As Integer

        'Se obtienen los parámetros
        fila = (linearPos - 1) \ 3
        columna = (linearPos - 1) Mod 3

        posicionValida = ((linearPos >= 1) And (linearPos <= 9))

        puedeInsertar = ((posicionValida) And (matrizJugadas(fila, columna) = " "))

    End Function

    'Subprograma que inserta una jugada en la posición especificada
    Sub insertarJugada(ByRef matrizJugadas(,) As Char, ByVal linearPos As Integer, ByVal symbol As Char)

        'Declaración de variables
        Dim fila, columna As Integer

        'Se obtienen los parámetros
        fila = (linearPos - 1) \ 3
        columna = (linearPos - 1) Mod 3

        matrizJugadas(fila, columna) = symbol

    End Sub


    Sub Main()

        'Entradas
        Dim nombre1, nombre2 As String
        Dim linearPos As Integer

        'Proceso
        Dim matrizJugadas(2, 2), symbol As Char
        Dim turnoActual As Integer 'Jugador 1 o Jugador 2
        Dim numeroJugada As Integer
        Dim ganador As Integer '0, 1 (primer jugador), 2 (segundo jugador) o 3 (La Vieja)
        Dim seguirJugando As Boolean

        'Salidas
        Dim partidasGanadas1, partidasGanadas2 As Integer


        'Escritura de instrucciones
        Console.WriteLine()
        Console.WriteLine("¡Bienvenido a La Vieja 2.0!")
        Console.WriteLine()
        Console.WriteLine("Las reglas son sencillas, cada jugador (X y O) debe tratar de lograr tres marcas en línea sobre el tablero 3x3.")
        Console.Write("Para jugar, se debe ingresar la posición en que se quiere jugar como un número del 1 al 9 ")
        Console.WriteLine("donde 1 es la casilla más superior izquierda, y 9 es la casilla más inferior derecha")
        Console.WriteLine("¡Mucha suerte! Y recuerda que es sólo un juego.")
        Console.WriteLine()

        'Nombres de jugadores
        Console.Write("Nombre 1er jugador (X): ")
        nombre1 = Console.ReadLine()

        Console.Write("Nombre 2do jugador (O): ")
        nombre2 = Console.ReadLine()

        Console.WriteLine()



        seguirJugando = True

        'Ciclo principal
        'Iteración = Cada juego
        While seguirJugando

            'Variables globales del juego
            turnoActual = 1
            ganador = 0
            numeroJugada = 0

            'Iniciar la matriz de jugadas como vacia
            For i = 0 To 2
                For j = 0 To 2
                    matrizJugadas(i, j) = " "
                Next
            Next

            imprimirTablero(matrizJugadas)
            Console.WriteLine()

            'Iteración = Cada jugada
            While ganador = 0

                Console.Write("Le toca a ")

                If turnoActual = 1 Then

                    Console.WriteLine(nombre1)

                ElseIf turnoActual = 2 Then

                    Console.WriteLine(nombre2)

                End If

                'Pedir posición de nueva jugada
                Console.WriteLine()
                Console.WriteLine("Ingrese posición (1-9): ")
                linearPos = Console.ReadLine()
                Console.WriteLine()

                If puedeInsertar(matrizJugadas, linearPos) Then

                    If turnoActual = 1 Then
                        symbol = "X"
                    Else
                        symbol = "O"
                    End If

                    insertarJugada(matrizJugadas, linearPos, symbol)

                    imprimirTablero(matrizJugadas)
                    Console.WriteLine()

                    turnoActual = (turnoActual Mod 2) + 1 'Convierte 1 a 2 y 2 a 1
                    numeroJugada += 1

                    'Actualizar el valor del ganador
                    ganador = calcularGanador(matrizJugadas, numeroJugada)

                Else

                    Console.WriteLine("Posición inválida, es del 1 al 9 y no debe estar ocupada.")
                    Console.WriteLine()

                End If

            End While

            'Imprimir ganador

            Console.Write("El ganador ha sido ")

            If ganador = 1 Then

                partidasGanadas1 += 1

                Console.WriteLine(nombre1)

                Console.WriteLine("¡Felicidades!")

            ElseIf ganador = 2 Then

                partidasGanadas2 += 1

                Console.WriteLine(nombre2)

                Console.WriteLine("¡Felicidades!")

            Else

                Console.WriteLine("La Vieja")

                Console.WriteLine("¡Qué lástima!")

            End If

            Console.WriteLine()


            'Imprimir partidas ganadas por jugador

            Console.WriteLine("Partidas ganadas: ")

            Console.WriteLine(nombre1 & " = " & partidasGanadas1)

            Console.WriteLine(nombre2 & " = " & partidasGanadas2)

            Console.WriteLine()
            Console.WriteLine()

            'Actualizar valor de seguirJugando

            Console.WriteLine("¿Seguir jugando?")
            Console.Write("(1 = Sí, 2 = No): ")
            seguirJugando = (Console.ReadLine() = 1)

            Console.WriteLine()

        End While

        'Despedida del juego
        Console.WriteLine("¡Gracias por jugar!")
        Console.WriteLine()
        Console.WriteLine("Presiona una tecla para salir...")
        Console.ReadKey()

    End Sub

End Module
