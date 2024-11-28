# Лабораторная работа 3

---

  * Студент: `Бушмелев Константин Алексеевич`
  * Группа: `P3318`
  * ИСУ: `367950`

---
## Требования к разработанному ПО

### Описание алгоритма

1. **Линейная интерполяция**: Использует две последние точки для построения линейной функции.
2. **Квадратичная интерполяция**: Использует три последние точки для построения квадратичной функции.
3. **Интерполяция Лагранжа**: Использует шесть последних точек для построения полинома Лагранжа.

### Требования

- **Haskell**: Программа написана на языке Haskell.
- **Stack**: Для сборки и запуска проекта используется Stack.
- **Ввод данных**: Пользователь вводит точки в формате `X Y` через пробел.
- **Вывод данных**: Программа выводит результаты интерполяции на стандартный вывод.

## Ключевые элементы реализации

### `Lib.hs`

```haskell
linearInterpolation :: [(Double, Double)] -> Double -> [(Double, Double)]
linearInterpolation dots step = 
    let 
        (x2, y2) = last dots
        (x1, y1) = last $ init dots
        k = (y2 - y1) / (x2 - x1)
        b = y1 - k * x1
        generatePoints x
            | x > x2 = []
            | otherwise = (roundTo 2 x, roundTo 2 (k * x + b)) : generatePoints (x + step)
    in
        if length dots >= 2
        then generatePoints x1
        else []

quadraticInterpolation :: [(Double, Double)] -> Double -> [(Double, Double)]
quadraticInterpolation dots step =
    let 
        (x3, y3) = last dots
        (x2, y2) = last $ init dots
        (x1, y1) = last $ init $ init dots
        a = ((y3 - y1) / (x3 - x1) - (y2 - y1) / (x2 - x1)) / (x3 - x2)
        b = (y2 - y1) / (x2 - x1) - a * (x1 + x2)
        c = y1 - a * x1 * x1 - b * x1
        generatePoints x
            | x > x3 = []
            | otherwise = (roundTo 2 x, roundTo 2 (a * x * x + b * x + c)) : generatePoints (x + step)
    in 
        if length dots >= 3
        then generatePoints x1
        else []

lagrangeInterpolation :: [(Double, Double)] -> Double -> [(Double, Double)]
lagrangeInterpolation dots step =
    let 
        (x6, y6) = last dots
        (x5, y5) = last $ init dots
        (x4, y4) = last $ init $ init dots
        (x3, y3) = last $ init $ init $ init dots
        (x2, y2) = last $ init $ init $ init $ init dots
        (x1, y1) = last $ init $ init $ init $ init $ init dots
        l1 x = (x - x2) * (x - x3) * (x - x4) * (x - x5) * (x - x6) / ((x1 - x2) * (x1 - x3) * (x1 - x4) * (x1 - x5) * (x1 - x6))
        l2 x = (x - x1) * (x - x3) * (x - x4) * (x - x5) * (x - x6) / ((x2 - x1) * (x2 - x3) * (x2 - x4) * (x2 - x5) * (x2 - x6))
        l3 x = (x - x1) * (x - x2) * (x - x4) * (x - x5) * (x - x6) / ((x3 - x1) * (x3 - x2) * (x3 - x4) * (x3 - x5) * (x3 - x6))
        l4 x = (x - x1) * (x - x2) * (x - x3) * (x - x5) * (x - x6) / ((x4 - x1) * (x4 - x2) * (x4 - x3) * (x4 - x5) * (x4 - x6))
        l5 x = (x - x1) * (x - x2) * (x - x3) * (x - x4) * (x - x6) / ((x5 - x1) * (x5 - x2) * (x5 - x3) * (x5 - x4) * (x5 - x6))
        l6 x = (x - x1) * (x - x2) * (x - x3) * (x - x4) * (x - x5) / ((x6 - x1) * (x6 - x2) * (x6 - x3) * (x6 - x4) * (x6 - x5))
        generatePoints x
            | x > x6 = []
            | otherwise = (roundTo 2 x, roundTo 2 (y1 * l1 x + y2 * l2 x + y3 * l3 x + y4 * l4 x + y5 * l5 x + y6 * l6 x)) : generatePoints (x + step)

    in
        if length dots >= 6
        then generatePoints x1
        else []
```
### `IO.hs`
```haskell
inputDot :: IO [(Double, Double)]
inputDot = do
    putStrLn "Ввод точки (X Y через пробел):"
    str1 <- getLine
    if null str1
        then return []
        else do
            let [x, y] = map read $ words str1 :: [Double]
            return [(x, y)]
```
## Ввод/вывод программы
### Ввод
 - Пользователь вводит точки в формате X Y через пробел.
### Вывод
```
Ввод точки (X Y через пробел):
1 1
Ввод точки (X Y через пробел):
2 4 
Линейная, Квадратичная и Лагранж интерполяция с шагом 1.1:
Линейная: [(1.0,1.0)],
Квадратичная: [],
Лагранж: []
Ввод точки (X Y через пробел):
3 27
Линейная, Квадратичная и Лагранж интерполяция с шагом 1.1:
Линейная: [(2.0,4.0)],
Квадратичная: [(1.0,1.0),(2.1,5.4)],
Лагранж: []
Ввод точки (X Y через пробел):
4 64
Линейная, Квадратичная и Лагранж интерполяция с шагом 1.1:
Линейная: [(3.0,27.0)],
Квадратичная: [(2.0,4.0),(3.1,30.07)],
Лагранж: []
Ввод точки (X Y через пробел):
5 125
Линейная, Квадратичная и Лагранж интерполяция с шагом 1.1:
Линейная: [(4.0,64.0)],
Квадратичная: [(3.0,27.0),(4.1,69.02)],
Лагранж: []
Ввод точки (X Y через пробел):
6 200
Линейная, Квадратичная и Лагранж интерполяция с шагом 1.1:
Линейная: [(5.0,125.0)],
Квадратичная: [(4.0,64.0),(5.1,131.87)],
Лагранж: [(1.0,1.0),(2.1,5.79),(3.2,32.96),(4.3,79.53),(5.4,155.37)]
```
