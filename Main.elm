module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, h2, p, text)
import Html.Attributes exposing (style)


-- =============================================================================
-- EJERCICIO 1: Función Potencia
-- =============================================================================

{-| Función que eleva un número 'a' a la potencia 'b'
Ejemplos:
- power 2 3 = 8
- power 5 0 = 1
- power 10 2 = 100
-}
power : Int -> Int -> Int
power a b =
    if b < 0 then
        0  -- Para simplificar, potencias negativas retornan 0
    else if b == 0 then
        1  -- Caso base: cualquier número^0 = 1
    else
        a * power a (b - 1)  -- Caso recursivo: a^b = a * a^(b-1)


-- =============================================================================
-- EJERCICIO 2: Factorial
-- =============================================================================

{-| Función que calcula el factorial de un número
Ejemplos:
- factorial 5 = 120
- factorial 0 = 1
- factorial 1 = 1
-}
factorial : Int -> Int
factorial n =
    if n < 0 then
        0  -- Para simplificar, factorial de negativos es 0
    else if n <= 1 then
        1  -- Caso base: 0! = 1, 1! = 1
    else
        n * factorial (n - 1)  -- Caso recursivo: n! = n * (n-1)!


-- =============================================================================
-- EJERCICIO 3: Fibonacci
-- =============================================================================

{-| Versión exponencial (ingenua) de Fibonacci
Ejemplos:
- fibonacciExponential 0 = 0
- fibonacciExponential 1 = 1
- fibonacciExponential 10 = 55
-}
fibonacciExponential : Int -> Int
fibonacciExponential n =
    if n < 0 then
        0  -- Para negativos retornamos 0
    else if n == 0 then
        0  -- Caso base: F(0) = 0
    else if n == 1 then
        1  -- Caso base: F(1) = 1
    else
        fibonacciExponential (n - 1) + fibonacciExponential (n - 2)


{-| Versión lineal (optimizada) de Fibonacci usando acumuladores
Ejemplos:
- fibonacciLinear 0 = 0
- fibonacciLinear 1 = 1
- fibonacciLinear 10 = 55
-}
fibonacciLinear : Int -> Int
fibonacciLinear n =
    if n < 0 then
        0
    else
        fibonacciHelper n 0 1


{-| Función auxiliar para Fibonacci lineal con acumuladores
-}
fibonacciHelper : Int -> Int -> Int -> Int
fibonacciHelper n prev curr =
    if n == 0 then
        prev
    else if n == 1 then
        curr
    else
        fibonacciHelper (n - 1) curr (prev + curr)


-- =============================================================================
-- EJERCICIO 4: Triángulo de Pascal
-- =============================================================================

{-| Función que calcula el valor en la posición (x, y) del triángulo de Pascal
Ejemplos:
- pascalTriangle 0 0 = 1
- pascalTriangle 2 4 = 6
- pascalTriangle 1 3 = 3
-}
pascalTriangle : Int -> Int -> Int
pascalTriangle x y =
    if x < 0 || y < 0 || x > y then
        0  -- Posiciones inválidas
    else if x == 0 || x == y then
        1  -- Los bordes del triángulo siempre valen 1
    else
        pascalTriangle (x - 1) (y - 1) + pascalTriangle x (y - 1)


-- =============================================================================
-- EJERCICIO 5: Máximo Común Divisor (MCD)
-- =============================================================================

{-| Algoritmo euclidiano para calcular el máximo común divisor
Ejemplos:
- gcd 48 18 = 6
- gcd 17 13 = 1
- gcd 100 25 = 25
-}
gcd : Int -> Int -> Int
gcd a b =
    let
        absA = abs a
        absB = abs b
    in
    if absB == 0 then
        absA  -- Caso base: cuando b = 0, el resultado es |a|
    else
        gcd absB (absA |> modBy absB)  -- Algoritmo euclidiano: gcd(a, b) = gcd(b, a mod b)


-- =============================================================================
-- EJERCICIO 6: Contar Dígitos
-- =============================================================================

{-| Función que cuenta la cantidad de dígitos en un número
Ejemplos:
- countDigits 12345 = 5
- countDigits 7 = 1
- countDigits -456 = 3
-}
countDigits : Int -> Int
countDigits n =
    let
        absN = abs n
    in
    if absN < 10 then
        1  -- Caso base: números menores a 10 tienen 1 dígito
    else
        1 + countDigits (absN // 10)  -- Caso recursivo: dividir por 10 y sumar 1


-- =============================================================================
-- EJERCICIO 7: Suma de Dígitos
-- =============================================================================

{-| Función que suma todos los dígitos de un número
Ejemplos:
- sumDigits 123 = 6 (1+2+3)
- sumDigits 999 = 27 (9+9+9)
- sumDigits -456 = 15 (4+5+6)
-}
sumDigits : Int -> Int
sumDigits n =
    let
        absN = abs n
    in
    if absN < 10 then
        absN  -- Caso base: un solo dígito
    else
        (absN |> modBy 10) + sumDigits (absN // 10)  -- Último dígito + suma del resto


-- =============================================================================
-- EJERCICIO 8: Verificar Palíndromo
-- =============================================================================

{-| Función que invierte un número
Ejemplos:
- reverseNumber 123 = 321
- reverseNumber 7 = 7
-}
reverseNumber : Int -> Int
reverseNumber n =
    reverseHelper (abs n) 0


{-| Función auxiliar para invertir un número usando acumulador
-}
reverseHelper : Int -> Int -> Int
reverseHelper n acc =
    if n == 0 then
        acc
    else
        reverseHelper (n // 10) (acc * 10 + (n |> modBy 10))


{-| Función que determina si un número es palíndromo
Ejemplos:
- isPalindrome 12321 = True
- isPalindrome 12345 = False
- isPalindrome 7 = True
-}
isPalindrome : Int -> Bool
isPalindrome n =
    let
        absN = abs n
    in
    absN == reverseNumber absN


-- =============================================================================
-- EJERCICIO 9: Paréntesis Balanceados
-- =============================================================================

{-| Función que determina si los paréntesis en un texto están balanceados
Ejemplos:
- isBalanced "()" = True
- isBalanced "((()))()" = True
- isBalanced "(()())" = True
- isBalanced "(()(())" = False
- isBalanced ")(" = False
-}
isBalanced : String -> Bool
isBalanced str =
    isBalancedHelper (String.toList str) 0


{-| Función auxiliar para verificar balance de paréntesis con contador
-}
isBalancedHelper : List Char -> Int -> Bool
isBalancedHelper chars counter =
    case chars of
        [] ->
            counter == 0  -- Al final, el contador debe ser exactamente 0
        
        char :: rest ->
            if char == '(' then
                isBalancedHelper rest (counter + 1)  -- Incrementar por cada '('
            else if char == ')' then
                if counter <= 0 then
                    False  -- Si el contador se vuelve negativo, están desbalanceados
                else
                    isBalancedHelper rest (counter - 1)  -- Decrementar por cada ')'
            else
                isBalancedHelper rest counter  -- Ignorar otros caracteres


-- =============================================================================
-- FUNCIÓN MAIN PARA TESTING
-- =============================================================================

main : Html msg
main =
    div [ style "padding" "20px", style "font-family" "Arial" ]
        [ h1 [] [ text "Ejercicios de Funciones Recursivas en Elm" ]
        
        -- Ejercicio 1: Potencia
        , h2 [] [ text "Ejercicio 1: Función Potencia" ]
        , p [] [ text ("power 2 3 = " ++ String.fromInt (power 2 3)) ]
        , p [] [ text ("power 5 0 = " ++ String.fromInt (power 5 0)) ]
        , p [] [ text ("power 10 2 = " ++ String.fromInt (power 10 2)) ]
        
        -- Ejercicio 2: Factorial
        , h2 [] [ text "Ejercicio 2: Factorial" ]
        , p [] [ text ("factorial 5 = " ++ String.fromInt (factorial 5)) ]
        , p [] [ text ("factorial 0 = " ++ String.fromInt (factorial 0)) ]
        , p [] [ text ("factorial 1 = " ++ String.fromInt (factorial 1)) ]
        
        -- Ejercicio 3: Fibonacci
        , h2 [] [ text "Ejercicio 3: Fibonacci" ]
        , p [] [ text ("fibonacciExponential 10 = " ++ String.fromInt (fibonacciExponential 10)) ]
        , p [] [ text ("fibonacciLinear 10 = " ++ String.fromInt (fibonacciLinear 10)) ]
        
        -- Ejercicio 4: Triángulo de Pascal
        , h2 [] [ text "Ejercicio 4: Triángulo de Pascal" ]
        , p [] [ text ("pascalTriangle 0 0 = " ++ String.fromInt (pascalTriangle 0 0)) ]
        , p [] [ text ("pascalTriangle 2 4 = " ++ String.fromInt (pascalTriangle 2 4)) ]
        , p [] [ text ("pascalTriangle 1 3 = " ++ String.fromInt (pascalTriangle 1 3)) ]
        
        -- Ejercicio 5: MCD
        , h2 [] [ text "Ejercicio 5: Máximo Común Divisor" ]
        , p [] [ text ("gcd 48 18 = " ++ String.fromInt (gcd 48 18)) ]
        , p [] [ text ("gcd 17 13 = " ++ String.fromInt (gcd 17 13)) ]
        , p [] [ text ("gcd 100 25 = " ++ String.fromInt (gcd 100 25)) ]
        
        -- Ejercicio 6: Contar Dígitos
        , h2 [] [ text "Ejercicio 6: Contar Dígitos" ]
        , p [] [ text ("countDigits 12345 = " ++ String.fromInt (countDigits 12345)) ]
        , p [] [ text ("countDigits 7 = " ++ String.fromInt (countDigits 7)) ]
        , p [] [ text ("countDigits -456 = " ++ String.fromInt (countDigits -456)) ]
        
        -- Ejercicio 7: Suma de Dígitos
        , h2 [] [ text "Ejercicio 7: Suma de Dígitos" ]
        , p [] [ text ("sumDigits 123 = " ++ String.fromInt (sumDigits 123)) ]
        , p [] [ text ("sumDigits 999 = " ++ String.fromInt (sumDigits 999)) ]
        , p [] [ text ("sumDigits -456 = " ++ String.fromInt (sumDigits -456)) ]
        
        -- Ejercicio 8: Palíndromo
        , h2 [] [ text "Ejercicio 8: Verificar Palíndromo" ]
        , p [] [ text ("reverseNumber 123 = " ++ String.fromInt (reverseNumber 123)) ]
        , p [] [ text ("isPalindrome 12321 = " ++ boolToString (isPalindrome 12321)) ]
        , p [] [ text ("isPalindrome 12345 = " ++ boolToString (isPalindrome 12345)) ]
        , p [] [ text ("isPalindrome 7 = " ++ boolToString (isPalindrome 7)) ]
        
        -- Ejercicio 9: Paréntesis Balanceados
        , h2 [] [ text "Ejercicio 9: Paréntesis Balanceados" ]
        , p [] [ text ("isBalanced \"()\" = " ++ boolToString (isBalanced "()")) ]
        , p [] [ text ("isBalanced \"((()))(\" = " ++ boolToString (isBalanced "((()))(")) ]
        , p [] [ text ("isBalanced \"(()())\" = " ++ boolToString (isBalanced "(()())")) ]
        , p [] [ text ("isBalanced \"(()(())\" = " ++ boolToString (isBalanced "(()(())")) ]
        , p [] [ text ("isBalanced \")(\" = " ++ boolToString (isBalanced ")(")) ]
        ]


{-| Función auxiliar para convertir Bool a String
-}
boolToString : Bool -> String
boolToString bool =
    if bool then
        "True"
    else
        "False"