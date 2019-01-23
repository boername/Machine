## Квадратичная функция потерь
lossQuad <- function(x)
{
  return ((x-1)^2)
}

## Логистическая функция потерь
lossLog <- function(x)
{
  return (log2(1 + exp(-x)))
}
## Сигмоидная функция для весов ЛогРег
sigmoidFunction <- function(z)
{
  return (1 / (1 + exp(-z)))
}

## Стохастический градиент для ADALINE
sg.ADALINE <- function(xl, eta = 1, lambda = 1/6)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- c(1/2, 1/2, 1/2)
  iterCount <- 0
  ## Начальная оценка Q
  Q <- 0
  for (i in 1:l)
  {
    ## Вычисление скалярного произведения <w,x>
    wx <- sum(w * xl[i, 1:n])
    ## Вычисление отступа
    margin <- wx * xl[i, n + 1]
    Q <- Q + lossQuad(margin)
  }
  repeat
  {
    
    margins <- array(dim = l)
    
    for (i in 1:l)
    {
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      margins[i] <- crossprod(w, xi) * yi
    }
    ## Выбор объектов с ошибкой
    errorIndexes <- which(margins <= 0)
    if (length(errorIndexes) > 0)
    {
      # Выбор случайного индекса из ошибок
      i <- sample(errorIndexes, 1)
      iterCount <- iterCount + 1
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      ## Вычисление скалярного произведения <w,xi>
      wx <- sum(w * xi)
      margin <- wx * yi
      ## Выбрать объект x_i из Xl, вычислить ошибку и сделать шаг градиентного спуска:
      ex <- lossQuad(margin)
      eta <- 1 / sqrt(sum(xi * xi))
      w <- w - eta * (wx - yi) * xi ## Шаг градиентного спуска
      ## Оценка нового Q
      Qprev <- Q
      Q <- (1 - lambda) * Q + lambda * ex
    }
    # Выход, если выборки полностью разделены
    else
    {
      break
    }
  }
  return (w)
}


## Стохастический градиент для логистической регрессии
sg.LogRegression <- function(xl)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- c(1/2, 1/2, 1/2)
  iterCount <- 0
  lambda <- 1/l
  ## Начальная оценка Q
  Q <- 0
  for (i in 1:l)
  {
    ## Вычисление скалярного произведения <w,x>
    wx <- sum(w * xl[i, 1:n])
    
    ## Вычисление отступа
    margin <- wx * xl[i, n + 1]
    Q <- Q + sigmoidFunction(margin)
  }
  repeat
  {
    # Выбор случайного индекса из ошибок
    i <- sample(1:l, 1)
    iterCount <- iterCount + 1
    # i <- sample(1:l, 1)
    xi <- xl[i, 1:n]
    yi <- xl[i, n + 1]
    ## Вычисление скалярного произведения <w,xi>
    wx <- sum(w * xi)
    ## Градиентный спуск
    margin <- wx * yi
    ex <- sigmoidFunction(margin)
    eta <- 0.3
    w <- w + eta * xi * yi * sigmoidFunction(-wx * yi)
    ## Оценка нового Q
    Qprev <- Q
    Q <- (1 - lambda) * Q + lambda * ex
    if (abs(Qprev - Q) / abs(max(Qprev, Q)) < 1e-5)
      break
  }
  return (w)
}
