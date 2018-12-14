package pplAssignment

object F2016A7PS0095P{
    //Start Coding from here
	//CONVOLUTION
	//PART1
	def dotProduct(matrix_1:List[List[Double]],matrix_2:List[List[Double]]):Double={
		if(matrix_1==Nil||matrix_2==Nil) 0
		else if (matrix_1.head==Nil||matrix_2.head==Nil) dotProduct(matrix_1.tail,matrix_2.tail)
		else (matrix_1.head.head*matrix_2.head.head)+dotProduct(matrix_1.head.tail::matrix_1.tail,matrix_2.head.tail::matrix_2.tail)
		}         
                                 
	//PART2                 
	def convolute(Image:List[List[Double]],Kernel:List[List[Double]],imageSize:List[Int],kernelSize:List[Int]):List[List[Double]]={
	
		val it_row=imageSize.head-kernelSize.head+1
		val it_col=imageSize.tail.head-kernelSize.tail.head+1
		def droprow(mat:List[List[Double]]):List[List[Double]]={
			if(mat==Nil)
			Nil
			else
			mat.tail
		}                                 
		def dropcol(mat:List[List[Double]]):List[List[Double]]={
			if(mat==Nil)
			Nil
			else
			mat.head.tail::dropcol(mat.tail)
		}                        
		def extractkelems(l:List[Double],Kcol:Int):List[Double]= { 
			if(Kcol==0||l==Nil)
		 	Nil 
		 	else
		 	l.head::extractkelems(l.tail,Kcol-1)
		 }                                
		 def extractkcols(m:List[List[Double]],Kcol:Int):List[List[Double]]={
			if(m==Nil)
			Nil
			else
			extractkelems(m.head,Kcol)::extractkcols(m.tail,Kcol)
	 	}                                        
		def conv_hor(mat:List[List[Double]],kern:List[List[Double]],it_col:Int,kernelSize:List[Int]):List[Double]={
			if(it_col==0)
			Nil
			else
			dotProduct(kern,extractkcols(mat,kernelSize.tail.head))::conv_hor(dropcol(mat),kern,it_col-1,kernelSize)
		}                    
		def extractkrows(Image:List[List[Double]],Krow:Int):List[List[Double]]={
			if(Krow==0||Image==Nil)
		 	Nil //change
		 	else
		 	Image.head::extractkrows(Image.tail,Krow-1)
 		}                                 
 		def conv_ver(Image:List[List[Double]],Kernel:List[List[Double]],kernelSize:List[Int],it_row:Int,it_col:Int):List[List[Double]]={
			if(it_row==0)
			Nil
			else
			conv_hor(extractkrows(Image,kernelSize.head),Kernel,it_col,kernelSize)::conv_ver(droprow    			(Image),Kernel,kernelSize,it_row-1,it_col)
			}     
		conv_ver(Image,Kernel,kernelSize,it_row,it_col)
	}                                              
      


	//ACTIVATION

	def activationLayer(f:Double=>Double,m:List[List[Double]]):List[List[Double]]={
		def bylist(f:Double=>Double,lis:List[Double]):List[Double]= lis match{
			case Nil=>Nil
			case (h::t)=>List(f(h)):::bylist(f,t)
		}
		m match{
		case Nil=>Nil
		case (x::xs)=>bylist(f,x)::(activationLayer(f,xs))
		}
	}   
	
	
		
	//POOLING
	//PART1
		
	def singlePooling(poolingFunc:List[Double]=>Double,Image:List[List[Double]],K:Int):List[Double]={
		def extractk(l:List[Double],K:Int):List[List[Double]]={
			if((K==0)&&(l!=Nil))
		 	List(Nil,l) //change
		 	else if(l==Nil)
		 	List(Nil,Nil)
		 	else
		 	List(List(l.head):::extractk(l.tail,K-1).head,extractk(l.tail,K-1).tail.head)
 		 }
		def passtoextractk(m:List[List[Double]],K:Int):List[List[List[Double]]]={
			m match{
			case Nil=>Nil
			case (x::xs)=>
			if(passtoextractk(xs,K)!=Nil)
			List(extractk(x,K).head::passtoextractk(xs,K).head,extractk(x,K).tail.head::passtoextractk(xs,K).tail.head)
			else
			List(List(extractk(x,K).head),List(extractk(x,K).tail.head))
			}
 		}
		def matrixtolist(mat:List[List[Double]]):List[Double]={
			mat match{
			case Nil=>Nil
			case(Nil::xss)=>matrixtolist(xss)
			case((x::xs)::xss)=>List(x):::matrixtolist(xs::xss)
			}
		}
		if(Image.head==List())
	 	Nil
		else
	 	poolingFunc(matrixtolist(passtoextractk(Image,K).head))::singlePooling(poolingFunc,passtoextractk(Image,K).tail.head,K)
	 }                         
          
  
  //PART2
  	def poolingLayer(poolingFunc:List[Double]=>Double,Image:List[List[Double]],K:Int):List[List[Double]]={
		def extractkrows(mat:List[List[Double]],K:Int):List[List[List[Double]]]={
	    		if(K==0&&mat!=Nil)
	    		List(Nil,mat)
	    		else if(mat==Nil)
	    		List(Nil,Nil)
	    		else
	    		List(mat.head::extractkrows(mat.tail,K-1).head,extractkrows(mat.tail,K-1).tail.head)
    		}
		if(Image==Nil)
	    	Nil
	    	else
	    	singlePooling(poolingFunc,extractkrows(Image,K).head,K)::poolingLayer(poolingFunc,extractkrows(Image,K).tail.head,K)
    	}  



     
  //MIXED LAYER

  	def mixedLayer(Image:List[List[Double]],Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int],activationFunc:Double => Double,poolingFunc:List[Double]=>Double,K:Int):List[List[Double]]={
  		val mat1=convolute(Image,Kernel,imageSize,kernelSize)
		val mat2=activationLayer(activationFunc,mat1)
		poolingLayer(poolingFunc,mat2,K)
  	}

 
  

  //NORMALISATION

  	def maximum(Image:List[List[Double]],max:Double):Double=Image match{
		case Nil=>max
		case((x::xs)::xss)=>
		if(x>=max)
		maximum(xs::xss,x)
		else
		maximum(xs::xss,max)
		case(Nil::xss)=>
		maximum(xss,max)
	}                                        
	def minimum(Image:List[List[Double]],min:Double):Double= Image match{
		case Nil=>min
		case((x::xs)::xss)=>
		if(x<=min)
		minimum(xs::xss,x)
		else
		minimum(xs::xss,min)
		case(Nil::xss)=>
		minimum(xss,min)
	}                                        
	def normalise(Image:List[List[Double]]):List[List[Int]]= {
  		val max=maximum(Image,Image.head.head)
	  	val min=minimum(Image,Image.head.head)
	  	def normaliseinner(Image:List[List[Double]],max:Double,min:Double):List[List[Int]]={
			def normline(lis:List[Double],max:Double,min:Double):List[Int] = lis match{
   			case Nil=>Nil
	   		case (h::t)=>List(((h-min)*255/(max-min)).round.toInt):::normline(t,max,min)
			}
	   		Image match{
				case Nil=>Nil
				case(x::xs)=>
				normline(x,max,min)::normaliseinner(xs,max,min)
		  	}
  		}
  		normaliseinner(Image,max,min)
  	}//end of normalize                                  
    

    
    //ASSEMBLY
	def assembly(Image:List[List[Double]],imageSize:List[Int],w1:Double,w2:Double,b:Double,Kernel1:List[List[Double]],kernelSize1:List[Int],
    Kernel2:List[List[Double]],kernelSize2:List[Int],Kernel3:List[List[Double]],kernelSize3:List[Int],Size: Int):List[List[Int]]={
  
  		def reLU(x:Double):Double={
  		if(x>0)x else 0
		}

		def leaky_reLU(x:Double):Double={
	  	if(x>0)x else 0.5*x 
	  	}

	  	def max_pooling(l:List[Double]):Double={
	  		def maxfirst(l2:List[Double],max:Double):Double=l2 match{
	  			case Nil=>max
	  			case h::t=>if(h>max) maxfirst(t,h) else maxfirst(t,max)
			}
	  		maxfirst(l,l.head)
	  	}
	  
	  	def avg_pooling(l:List[Double]):Double={
	  		def avg(lis:List[Double],sum:Double,count:Double):List[Double]=lis match{
		  		case Nil=>List(sum,count)
		  		case (x::xs)=>avg(xs,sum+x,count+1)
	  		}
	  		val av=avg(l,0,0)
			av.head/av.tail.head
			}
			
		def matrix_addition(m1:List[List[Double]],m2:List[List[Double]]):List[List[Double]]={
			def matrix_row_addition(lis1:List[Double],lis2:List[Double]):List[Double]={
				if(lis1==Nil) Nil
				else (lis1.head+lis2.head)::matrix_row_addition(lis1.tail,lis2.tail)
			}
			if(m1==Nil) Nil
			else matrix_row_addition(m1.head,m2.head)::matrix_addition(m1.tail,m2.tail)
		}
  
		def multiply_weight(m1:List[List[Double]],wt:Double):List[List[Double]]={
			def mulbyline(lis:List[Double],wt:Double):List[Double]= lis match{
			case Nil=>Nil
			case x::xs=>List(x*wt):::mulbyline(xs,wt)
			}
			m1 match{
			case Nil=>Nil
			case x::xs=>mulbyline(x,wt)::multiply_weight(xs,wt)
			}
		}
	  
	  	def add_bias(m1:List[List[Double]],b:Double):List[List[Double]]={
				def addbyline(lis:List[Double],wt:Double):List[Double]= lis match{
				case Nil=>Nil
				case x::xs=>List(x+b):::addbyline(xs,b)
				}
				m1 match{
				case Nil=>Nil
				case x::xs=>addbyline(x,b)::add_bias(xs,b)
				}
			}
	  
		def findrow(mat:List[List[Double]],row:Int):Int={
			if(mat==Nil)
			row
			else
			findrow(mat.tail,row+1)
		}
		def findcol(mat:List[Double],col:Int):Int={
		
		if(mat==Nil)
		col
		else
		findcol(mat.tail,col+1)
		
		} 

		  val tempout1=mixedLayer(Image,Kernel1,imageSize,kernelSize1,reLU,avg_pooling,Size)
		  val tempout2=mixedLayer(Image,Kernel2,imageSize,kernelSize2,reLU,avg_pooling,Size)
		  val tempout3=add_bias(matrix_addition(multiply_weight(tempout1,w1),multiply_weight(tempout2,w2)),b)
		  val size=List(findrow(tempout3,0),findcol(tempout3.head,0))
		  val tempout4=mixedLayer(tempout3,Kernel3,size,kernelSize3,leaky_reLU,max_pooling,Size)
		  val output=normalise(tempout4)
		  output
    }//end of assembly
}
