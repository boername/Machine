# Machine  

- [Метрические](#Метрические-алгоритмы)  
  - [Алгоритм 1NN](#Алгоритм-1NN)  
- [Байесовские алгоритмы](#Байесовские-алгоритмы)  
  - [Наивный байесовский классификатор](#Наивный-байесовский-классификатор)  
- [Линейные алгоритмы](#Линейные-алгоритмы)  
  
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

<img src="https://github.com/boername/Machine/blob/master/pict/bayes1.gif" width="270"> 

Алгоритм **a(x)** называется **оптимальным байесовским решающим правилом**, где   
**P(y)** - _априорные вероятности_ классов,   
**p(x|y)** - _функции правдоподобия_,  
<img src="https://github.com/boername/Machine/blob/master/pict/lambday.gif"> - _величина потери_ алгоритмом **а** при неправильной классификации объекта класса **y**  

_иными словами_:   
По известному вектору признаков - **x**, мы определяем класс **y**, к которому принадлежит объект из **a(x)** по формуле:   
**a(x) = argmax P(y|x)**, для которого при условии **x**, вероятность класса **y** - наиболее высока.  

### Наивный байесовский классификатор  
**Наивный байесовский алгоритм** – это алгоритм классификации, основанный на _теореме Байеса_ с предположением, что все признаки есть независимые случайные величины. **НБА** предполагает, что наличие какого-либо признака в классе не связано с наличием какого-либо другого признака. В его работе стоит задача найти оценки **n**-одномерных плотностей(**j-го** признака для класса **y**.) и подставить их в оптимальное байесовское решающее правило.

Ниже представлен код программы **Наивного нормального байесовского классификатора:**
```R
naiveb = function(x, Py, Mo, covar, m, n) {
  kolvo <- matrix(c('setosa','versicolor', 'virginica', 0, 0, 0), nrow = 3, ncol = 2)
  schet = rep(0, m)
  for (i in 1:m) {
    schet[i] = Py[i]
    for (j in 1:n){
      N=1/sqrt(2*pi)/covar[i,j]*exp(-1/2*(x[j]-Mo[i,j])^2/covar[i,j]^2) #вычисление плотностей
      schet[i] = schet[i] * N #ищем для каждого класса
    }
    kolvo[i,2]=schet[i]
  }
  class <- kolvo[,1][which.max(kolvo[,2])]
}
```
#### Карта классификации:
![](https://github.com/boername/Machine/blob/master/pict/naiveb.PNG)

## Линейные алгоритмы
Пусть ![](http://latex.codecogs.com/svg.latex?X%20%3D%20%5Cmathbb%7BR%7D%5En)
и ![](http://latex.codecogs.com/svg.latex?Y%20%3D%20%5C%7B-1%3B&plus;1%5C%7D).  
Тогда алгоритм ![](http://latex.codecogs.com/svg.latex?a%28x%2Cw%29%3D%20%5Ctext%7Bsign%7Df%28x%2Cw%29%3D%5Ctext%7Bsign%7D%20%28%5Clangle%20w%2Cx%20%5Crangle-w_0%29%2Cw%20%5Cin%20%5Cmathbb%7BR%7D%5En) - это __линейный алгоритм классификации__.  
Если _f_>0, то алгоритм _a_ относит _x_ к классу +1, иначе к классу -1,  
где ![](http://latex.codecogs.com/svg.latex?%5Clangle%20w%2Cx%20%5Crangle%3D0) - этой __уравнение разделяющей поверхности__.  

Тогда другими словами, если x находится по одну сторону гиперплоскости с её направляющим вектором w, объект x относится к классу +1, в противном случае - к классу -1.  

Величина ![](http://latex.codecogs.com/svg.latex?M_i%28w%29%3Dy_i%5Clangle%20x_i%2Cw%20%5Crangle) есть __отступ__ объекта относительно алгоритма классификации.  
Если ![](http://latex.codecogs.com/svg.latex?M_i%28w%29%3C0), это значит, что алгоритм совершает на объекте ![](http://latex.codecogs.com/svg.latex?x_i) ошибку.  



