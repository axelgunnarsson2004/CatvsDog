import NN.*
import numsca.*
@main 
def main = 
  var train_x_cat_path ="archive/training_set/training_set/cats"

  var dataloader = DataLoader("cat_temp","dog_temp")

  var Xdata = dataloader.processImages()

  var paramsdata = NN.initialize_parameters_deep(12288,14,11,1) 
  print("init param")
  
  var temp1 = Matrix(1,2000) 
  var temp2 = Matrix(1,2000)
  var newis= temp1.elements(0)
  // Matrix.fromArray(Array(Xdata.T.elements(3111))).T.plot("temp.jpg")

  var Ydata = Matrix.fromArray(Array(Array.concat(temp1.elements(0),temp2.elements(0))))


  var ramsdata= NN.fit(paramsdata, Xdata, Ydata)



