# Machine
## Метрические алгоритмы
Метрические алгоритмы классифицируют объекты в зависимости от их сходства, а для оценки сходства объектов используют функцию расстояния, называемую *метрикой*. Чем меньше расстояние, тем больше объекты похожи друг на друга.
### Алгоритм KNN
Пусть дана обучающая выборка объектов - Xl, классифицируемый объект - z и метрика(Евклидово расстояние).
Алгоритм состоит из трёх основных пунктов:
1. Вычисление расстояния от объекта z до каждого из объектов обучающей выборки Xl.
2. Отбор k объектов обучающей выборки, расстояние до которых минимально.
3. Определение класса объекта z, исходя из того, какой класс встречается наиболее часто среди ближайших k соседей.
#### Функция расстояния:
```R
euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}
```
#### Функция KNN:
```R
kNN <- function(xl, z, k)
{
  ## Сортируем выборку согласно классифицируемого объекта
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  ## Получаем классы первых k соседей
  classes <- orderedXl[1:k, n + 1]
  ## Составляем таблицу встречаемости каждого класса
  counts <- table(classes)
  ## Находим класс, который доминирует среди первых k соседей
  class <- names(which.max(counts))
  return (class)
}
```
#### Карта классификации:
![](https://github.com/boername/Machine/blob/master/pict/pict_KNN2.PNG)

### Алгоритм 1NN
Алгоритм 1NN является частным случаем KNN. При его использовании аналогичным образом измеряется расстояние от классифицируемого объекта  z до всех элементов обучающей выборки Xl. Далее происходит сортировка этих расстояний. В качестве ответа(класса) выводится тот класс, элемент которого явлется самым ближайшим к классифицируемому.   
#### Функция 1NN:
```R
nn <- function(z, xl)
{
  #print(xl) вывести выборку
  #определяю размерность выборки
  l <- nrow(xl)
  n <- ncol(xl)-1
  
  distances <- c() #вектор расстояний
  for (i in 1:l)
  {
    distances <- c(distances, euclideanDistance(xl[i, 1:n], z))
  }
  #print(distances) вывод расстояний
  #order(distances) упорядочивание расстояний
  xl[order(distances)[1], n+1]
}
```
#### Карта классификации:
![](https://github.com/boername/Machine/blob/master/pict/pict_1NN2.PNG)

### Алгоритм KwNN
Метод KwNN по своей сути является модернизированным алгоритмом KNN, он заключается в следующем:
1. Вычисление расстояния от объекта z до каждого из объектов обучающей выборки Xl.
2. Отбор k объектов обучающей выборки, расстояние до которых минимально.
3. Присвоение веса каждому i-ому соседу(из k ближайших) классифицируемого объекта, с помощью весовой функции.
4. Суммирование весов соседей(веса одного класса суммируются)
5. Присвоение того класса классифицируемому объекту, который набрал наибольший суммарный вес на шаге 4.

#### Функция KwNN:
```R
kwNN <- function(xl, z, k, q)
{
  ## Сортируем выборку согласно классифицируемого объекта
  orderedXl <- sortObjectsByDist(xl, z)
  #3 столбец orderedXl содержит класс
  v1 <- c('setosa', 'versicolor', 'virginica') #создаём вектор с именами классов
  v2 <- c(0,0,0) #создаём пустой вектор
  
  for(i in 1:k){ 
    orderedXl[i, 4] = q^i #для первых k соседей каждой точки присваивается вес(в 4 столбец)(каждому соседу)
  }
  #4 столбец orderedXl содержит вес
  
  classes <- orderedXl[1:k, 3:4]
  #classes содержит класс и вес первых k соседей каждой точки

  v2[1]=sum(classes[classes$Species=='setosa', 2])
  #суммируем величины 2го столбца, где вес для сетосы, аналогично другие
  v2[2]=sum(classes[classes$Species=='versicolor', 2])
  v2[3]=sum(classes[classes$Species=='virginica', 2])
  
  gen <- cbind(v1,v2) #объединим векторы v1 и v2
  class <- v1[which.max(v2)]
  #which.max возвращает порядковый номер элемента объекта с максимальным значением
  #таким образом в class через индекс попадает имя класса(где вес максимален)
  return (class) 
}
```
#### Карта классификации:
![](https://github.com/boername/Machine/blob/master/pict/pict_KwNN.PNG)

## Байесовские алгоритмы
Введём понятия **вероятностей**:

**Априорная вероятность** – это вероятность, присвоенная событию при отсутствии знания, поддерживающего его наступление.  

**Апостериорная вероятность** – это условная вероятность события при некотором условии, рассматриваемая в противоположность его априорной вероятности.  
**Теорема Байеса:**  
<img src="https://camo.githubusercontent.com/81fb749849c2393a95d502f20ec079e4f02ae4ab/68747470733a2f2f6c617465782e636f6465636f67732e636f6d2f6769662e6c617465783f7028797c78293d5c667261637b7028782c79297d7b702878297d2673706163653b3d2673706163653b5c667261637b7028787c7929702879297d7b702878297d" width="270">  
**p(y|x)** - _апостериорная вероятности_(вероятность того, что объект x принадлежит классу y)  
**p(x|y)** - _функция правдободобия_  
**p(y)** - _априорная вероятность_ (вероятность появления класса)  

Любой **Байесовский классификатор** основан на принципе максимума _апостериорной вероятности_.  
Если плотности распределения каждого из классов известны, то искомый алгоритм можно выписать в явном виде(он является оптимальным).  
На практике плотности распределения классов обычно не известны. Их приходится (восстанавливать) по обучающей выборке.  

Для классифицируемого объекта вычисляются _функции правдоподобия_ каждого из классов, по ним вычисляются _апостериорные вероятности_ классов. Объект относится к тому классу, для которого _апостериорная вероятность_ максимальна.  

Алгоритм **a(x)** называется **оптимальным байесовским решающим правилом**, где   
**P(y)** - _априорные вероятности_ классов,   
**p(x|y)** - _функции правдоподобия_,  


