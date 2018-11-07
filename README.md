# Machine
## Метрические алгоритмы
Метрические алгоритмы классифицируют объекты в зависимости от их сходства, а для оценки сходства объектов используют функцию расстояния, называемую *метрикой*. Чем меньше расстояние, тем больше объекты похожи друг на друга.
### Алгоритм KNN
Пусть дана обучающая выборка объектов - Xl, классифицируемый объект - z и метрика(Евклидово расстояние).
Алгоритм состоит из трёх основных пунктов:
1. Вычисление расстояния от объекта z до каждого из объектов обучающей выборки Xl.
2. Отбор k объектов обучающей выборки, расстояние до которых минимально.
3. Определение класса объекта z, исходя из того, какой класс встречается наиболее часто среди ближайших k соседей.
# Функция расстояния:
```diff
euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}
```


