import numsca.Matrix
import utils.Utils
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable 

case class LinearCache(A: Matrix,W: Matrix, b:Matrix)
case class ActivationCache(A: Matrix, linear_cache: LinearCache)


object NN{ 
  def initialize_parameters(n_x: Int,n_h: Int,n_y: Int): Map[String,Matrix] = 
    var W1 = Matrix.randomMatrix(n_h,n_x) * 0.01
    var b1 = Matrix(n_h, 1)
    var W2 = Matrix.randomMatrix(n_y,n_h) * 0.01
    var b2 = Matrix(n_y,1)
    
    var parameters = Map("W1" -> W1,"W2"->W2,"b1"->b1,"b2"->b2)
    parameters

  def initialize_parameters_deep(layer_dims: Int*): mutable.Map[String,Matrix] = 
    var parameters = mutable.Map[String, Matrix]()
    for (l <- 1 until layer_dims.length) do 
      var W = Matrix.randomMatrix(layer_dims(l),layer_dims(l-1)) * 0.01
      var b = Matrix(layer_dims(l),1)
      
      parameters(s"W$l") = W 
      parameters(s"b$l") = b 
    parameters



  def linear_forward(A: Matrix, W: Matrix, b: Matrix): (Matrix, LinearCache) = 
    val Z = (W matmul A) + b 
    val cache = LinearCache(A,W,b)
    (Z,cache) 

  def linear_activation_forward(A_prev: Matrix, W: Matrix, b: Matrix, activation: String): (Matrix,ActivationCache) = 
    var unpack = linear_forward(A_prev,W,b)
    var Z = unpack._1
    var linear_cache = unpack._2 

    val A = activation match  
      case "sigmoid" => Utils.sigmoid(Z)
      case _=> Utils.relu(Z)

    val cache = ActivationCache(A,linear_cache)
    (A,cache)


  def L_model_forward(X: Matrix, parameters: mutable.Map[String,Matrix]): (Matrix, ArrayBuffer[ActivationCache]) = 
    val caches: ArrayBuffer[ActivationCache] = ArrayBuffer()
    val L = math.floor(parameters.size/2).toInt 
    var A = X 

    for l <- 1 until L do 
      var A_prev = A 
      
      var unpack = linear_activation_forward(A_prev:Matrix, parameters(f"W$l"),parameters(f"b$l"),activation="relu")
      A = unpack._1     
      caches += unpack._2
    
    var unpack = linear_activation_forward(A,parameters(f"W$L"),parameters(f"b$L"),"sigmoid")
    val AL = unpack._1
    caches += unpack._2

    (AL,caches)


  def compute_cost(AL: Matrix, Y: Matrix): Matrix = 
    var m = Y.shape(1)
    
    var cost = (Y matmul (AL.log.T)) + (Y.subtractTo(1.0) matmul (AL.subtractTo(1.0).log.T))
    cost = cost * (-1.0/m)
    cost
    //cost = (-1/m) * (np.dot(Y, np.log(AL).T) + np.dot((1-Y), np.log(1-AL).T))/
    


  def linear_backward(dZ: Matrix, linear_cache: LinearCache): (Matrix,Matrix,Matrix) =
    val A_prev = linear_cache.A 
    val W = linear_cache.W 
    val b = linear_cache.b  
    val m = A_prev.shape(1) 
    
    val dW = (dZ matmul A_prev.T) * (1.0/m) 
    val db = dZ.sum(1) * (1.0/m)  
    val dA_prev = W.T matmul dZ

    (dA_prev,dW,db)

  def linear_activation_backward(dA: Matrix, cache: ActivationCache, activation: String): (Matrix,Matrix,Matrix) = 
    val A = cache.A
    val linear_cache = cache.linear_cache
    
    if activation == "sigmoid" then
      var dZ = Utils.sigmoid_backward(dA,A)
      linear_backward(dZ, linear_cache)
      
    else
      var dZ = Utils.relu_backward(dA,A)
    
      linear_backward(dZ, linear_cache)
   
  def L_model_backward(AL: Matrix, Y: Matrix, caches: ArrayBuffer[ActivationCache]): mutable.Map[String,Matrix] =
    val grads: mutable.Map[String, Matrix] = mutable.Map.empty[String, Matrix] 
    
    val L = caches.length 
    val m = AL.shape(1)
    
    val dAL = ((Y / AL) - (Y.subtractTo(1) / AL.subtractTo(1))) * (-1.0)  
    
    var current_cache = caches(L-1)
    var (dAl,dWl,dbl)= linear_activation_backward(dAL, current_cache,"sigmoid")
    grads(f"dA${L-1}") = dAl 
    grads(f"dW${L}") = dWl 
    grads(f"db${L}") = dbl 
    
    for (l <- (0 until (L-1)).reverse) do
      current_cache = caches(l)
      var (dAl,dWl,dbl) = linear_activation_backward(grads(f"dA${l+1}"), current_cache,"relu")
      grads(f"dA${l}") = dAl 
      grads(f"dW${l+1}") = dWl 
      grads(f"db${l+1}") = dbl 
    grads




  def update_parameters(parameters: mutable.Map[String,Matrix], grads: mutable.Map[String,Matrix], learning_rate: Double): mutable.Map[String,Matrix] =
    val L = math.floor(parameters.size/2).toInt 
    
    for l <- 0 until L do 
      parameters(f"W${l+1}") = parameters(f"W${l+1}") - grads(f"dW${l+1}") * learning_rate
      parameters(f"b${l+1}") = parameters(f"b${l+1}") - grads(f"db${l+1}") * learning_rate
    parameters
}

val m = 1
val inputX = 3

var params = NN.initialize_parameters_deep(inputX,4,1)
var X = Matrix.randomMatrix(inputX,m)
var Y = Matrix(1,m)  


def run = 
  for iteration <- 1 to 100 do 
    val temp = NN.L_model_forward(X,params)
    var AL = temp._1
    var caches =  temp._2
    var grads = NN.L_model_backward(AL,Y,caches)
    params = NN.update_parameters(params,grads,0.01)
    if iteration % 100 == 0 then 
      println(iteration)
  
  var temp = NN.L_model_forward(X,params) 
  println(f"Final Guess is ${temp._1}")
